{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Structural and Delaunay rejection against explicit mesh corruptions.
module Moonlight.Planar.ValidationSpec
  ( tests
  ) where

import Control.Monad ( unless, when )
import Data.Word ( Word32 )
import Moonlight.Planar.Dcel ( faceDirectedEdges, numDirectedEdges, numVertices )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( innerFaces )
import Moonlight.Planar.Internal.HandleDefs ( DirectedEdgeId(DirectedEdgeId), FaceId(unFaceId) )
import Moonlight.Planar.Internal.Paged ( Paged, fromVector, toVector )
import Moonlight.Planar.Internal.Validation ( validateTopologyClosure )
import Moonlight.Planar.Internal.Types (PlanarIncidenceError (..))
import Moonlight.Planar.MeshFixtures ( requirePointBuild )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Types (ConstraintMode(Unconstrained), InvariantViolation(LocallyIllegalDelaunayEdge,
  CoordinatePlaneLengthMismatch, InnerFaceNotCounterClockwise, IncidenceViolation), BuildResult(buildTriangulation), Triangulation)
import Moonlight.Planar.Validation ( validateDelaunay, validateTopology )
import Support ( assertEqual, assertValid )
import qualified Data.IntSet as IntSet
import Moonlight.Planar.Internal.Representation qualified as Internal
import qualified Data.Vector.Unboxed as U


tests :: IO ()
tests =
  sequence_
    [ testValidationRejectsCorruptedMeshes
    , testRejectsMismatchedTwinEndpoints
    ]

type NativeMesh = Triangulation 'Unconstrained (Point) () () ()

-- Both face cycles, all representatives and Euler hold. Only the paired
-- endpoints disagree: XOR involution alone is not the twin law.
testRejectsMismatchedTwinEndpoints :: IO ()
testRejectsMismatchedTwinEndpoints = do
  built <- requirePointBuild "mismatched twin fixture" [Point 0 0, Point 1 0, Point 2 1]
  let mesh :: NativeMesh
      mesh = (buildTriangulation built)
        { Internal.triHalfTopology = fromVector 0 (U.fromList
            [0,2,4,1, 0,3,5,0, 1,4,0,1, 2,5,1,0, 2,0,2,1, 1,1,3,0])
        , Internal.triVertexOut = fromVector 0 (U.fromList [0,2,4])
        , Internal.triFaceEdge = fromVector 0 (U.fromList [1,0])
        }
  assertEqual "mismatched twin endpoints rejected"
    [IncidenceViolation (IncidenceEdgeEndpointMismatch (DirectedEdgeId 0))]
    (validateTopology mesh)
  unless (any isEndpointMismatch (validateTopologyClosure (IntSet.singleton 1) IntSet.empty mesh)) $
    fail "local closure missed the shared endpoint law"
  let inconsistentFace :: NativeMesh
      inconsistentFace = mesh
        { Internal.triHalfTopology = fromVector 0 (U.fromList
            [0,2,4,1, 1,5,3,0, 1,4,0,0, 2,1,5,0, 2,0,2,1, 0,3,1,0]) }
  assertEqual "face continuity rejected despite triangular next cycles"
    [IncidenceViolation (IncidenceEdgeLinkMismatch (DirectedEdgeId 0))]
    (validateTopology inconsistentFace)
 where
  isEndpointMismatch (IncidenceViolation IncidenceEdgeEndpointMismatch {}) = True
  isEndpointMismatch _ = False

-- | A named corruption of a mesh that was valid one line earlier, paired with
-- the violation it must provoke. Validation that has never been made to fail is
-- evidence only that it ran.
data Corruption = Corruption
  { corruptionName :: String
  , corruptMesh :: NativeMesh -> NativeMesh
  , provokes :: InvariantViolation -> Bool
  }

-- Sheared so that no four sites are cocircular: on a square grid the diagonal
-- of every cell is a free choice, and a fixture that admits two answers cannot
-- witness a wrong one.
corruptionFixture :: [Point]
corruptionFixture =
  [ Point (fromIntegral column + 0.25 * fromIntegral row) (1.3 * fromIntegral row)
  | column <- [0 .. 3 :: Int]
  , row <- [0 .. 3 :: Int]
  ]

rewritePlane :: (U.Unbox a, Num a) => (U.Vector a -> U.Vector a) -> Paged a -> Paged a
rewritePlane edit = fromVector 0 . edit . toVector

slot :: U.Unbox a => Int -> a -> U.Vector a -> U.Vector a
slot at value = (U.// [(at, value)])

-- Past the end of every plane in the fixture, and far from the sentinels the
-- packed representation reserves for absence.
beyond :: NativeMesh -> Word32
beyond mesh = fromIntegral (numDirectedEdges mesh + numVertices mesh + 64)

onTopology :: (NativeMesh -> U.Vector Word32 -> U.Vector Word32) -> NativeMesh -> NativeMesh
onTopology edit mesh =
  mesh {Internal.triHalfTopology = rewritePlane (edit mesh) (Internal.triHalfTopology mesh)}

-- The half-edge plane has stride four — origin, next, previous, face — so the
-- first four entries below are one surgery distinguished only by which field
-- the out-of-range index lands on.
structuralCorruptions :: [Corruption]
structuralCorruptions =
  [ Corruption
      "edge origin names an absent vertex"
      (onTopology (slot 0 . beyond))
      (\case IncidenceViolation IncidenceVertexOriginInvalid {} -> True; _ -> False)
  , Corruption
      "edge next names an absent edge"
      (onTopology (slot 1 . beyond))
      (\case IncidenceViolation IncidenceEdgeLinksInvalid {} -> True; _ -> False)
  , Corruption
      "edge previous names an absent edge"
      (onTopology (slot 2 . beyond))
      (\case IncidenceViolation IncidenceEdgeLinksInvalid {} -> True; _ -> False)
  , Corruption
      "edge face names an absent face"
      (onTopology (slot 3 . beyond))
      (\case IncidenceViolation IncidenceEdgeFaceInvalid {} -> True; _ -> False)
  , Corruption
      "vertex outgoing names an absent edge"
      ( \mesh ->
          mesh
            { Internal.triVertexOut =
                rewritePlane (slot 0 (beyond mesh)) (Internal.triVertexOut mesh)
            }
      )
      (\case IncidenceViolation IncidenceVertexRootInvalid {} -> True; _ -> False)
  , Corruption
      "a coordinate plane is one short"
      (\mesh -> mesh {Internal.triPointX = rewritePlane U.init (Internal.triPointX mesh)})
      (\case CoordinatePlaneLengthMismatch {} -> True; _ -> False)
  , Corruption
      "every inner face wound clockwise"
      (\mesh -> mesh {Internal.triPointY = rewritePlane (U.map negate) (Internal.triPointY mesh)})
      (\case InnerFaceNotCounterClockwise {} -> True; _ -> False)
  ]

-- Structural corruption is caught before geometry is read, so the empty-circle
-- law needs a mesh that stays well formed and merely stops being Delaunay.
delaunayCorruptions :: [Corruption]
delaunayCorruptions =
  [ Corruption
      "one site dragged through its neighbours' circumcircles"
      (\mesh -> mesh {Internal.triPointX = rewritePlane (slot 5 40) (Internal.triPointX mesh)})
      (\case LocallyIllegalDelaunayEdge {} -> True; _ -> False)
  ]

-- A surgery that changed nothing would report the oracle as unarmed when in
-- truth it was never asked anything, so the mutant must differ before its
-- rejection means a thing.
assertRejects
  :: String -> (NativeMesh -> [InvariantViolation]) -> NativeMesh -> Corruption -> IO ()
assertRejects oracle check pristine corruption = do
  let mutant = corruptMesh corruption pristine
      label = oracle <> " / " <> corruptionName corruption
      reported = check mutant
  when (mutant == pristine) $ fail (label <> ": the surgery changed nothing")
  unless (any (provokes corruption) reported) $
    fail (label <> ": admitted, reporting " <> show reported)

testValidationRejectsCorruptedMeshes :: IO ()
testValidationRejectsCorruptedMeshes = do
  built <- requirePointBuild "corruption fixture" corruptionFixture
  let pristine = buildTriangulation built
  assertValid "corruption fixture" pristine
  mapM_ (assertRejects "topology" validateTopology pristine) structuralCorruptions
  mapM_ (assertRejects "delaunay" validateDelaunay pristine) delaunayCorruptions
  case innerFaces pristine of
    [] -> fail "local validation fixture has no inner face"
    witnessFace : _ ->
      case faceDirectedEdges pristine witnessFace of
        [] -> fail "local validation fixture has no face edge"
        DirectedEdgeId rawEdge : _ -> do
          let admitted = IntSet.singleton (fromIntegral (unFaceId witnessFace))
              corrupted
                :: NativeMesh
              corrupted =
                pristine
                  { Internal.triHalfTopology =
                      rewritePlane
                        (slot (4 * fromIntegral rawEdge + 1) rawEdge)
                        (Internal.triHalfTopology pristine)
                  }
          assertEqual
            "selected validation accepts valid closure"
            []
            (validateTopologyClosure admitted IntSet.empty pristine)
          unless (not (null (validateTopology corrupted))) $
            fail "global validation missed the locally corrupt face closure"
          unless (not (null (validateTopologyClosure admitted IntSet.empty corrupted))) $
            fail "selected validation missed a locally corrupt face closure"
