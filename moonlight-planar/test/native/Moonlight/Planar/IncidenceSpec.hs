-- | Shared native packed observations and non-cellular exact face closure.
module Moonlight.Planar.IncidenceSpec (tests) where

import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Moonlight.Planar.BulkLoad (delaunay)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Types (buildTriangulation, unitElementDefaults)
import qualified Moonlight.Planar.Dcel as Dcel
import Moonlight.Planar.Exact (ExactPoint)
import Moonlight.Planar.Internal.CellSet
  ( CellSelectionError (..)
  , closeExactCellSetWith
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , exactCellSetIsFaceClosure
  , exactCellSetVertexCount
  )
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Internal.Incidence
import Moonlight.Planar.Internal.PackedIndex (noIndex)
import Moonlight.Planar.Internal.Paged (Paged, fromVector)
import Moonlight.Planar.Internal.Representation (Triangulation (..))
import Support (assertEqual, integerPoint, requireRight)

tests :: IO ()
tests = sequence_
  [ testNativeProjection
  , testExactFaceComponents
  , testPackedRefusals
  , putStrLn "incidence: ok"
  ]

testNativeProjection :: IO ()
testNativeProjection = traverse_ checkNative
  [ []
  , [Point 1 1]
  , [Point 0 0, Point 1 0, Point 2 0]
  , [Point 0 0, Point 3 0, Point 3 3, Point 0 3, Point 1 1]
 ]
 where
  checkNative :: [Point] -> IO ()
  checkNative points = do
    result <- requireRight "native incidence fixture" (delaunay unitElementDefaults (V.fromList points))
    let mesh = buildTriangulation result
        incidence = nativePlanarIncidence mesh
        isolated = if Dcel.numVertices mesh == 1
          then IntMap.singleton 0 (IntSet.singleton 0)
          else IntMap.empty
    checked <- requireRight "native planes satisfy exact packed admission"
      (admitPlanarIncidence (triVertexOut mesh) (triHalfTopology mesh) (triFaceEdge mesh) IntMap.empty isolated)
    assertEqual "native projection and checked same-plane publication agree" incidence checked
    assertEqual "native topology counts"
      (Dcel.numVertices mesh, Dcel.numDirectedEdges mesh, Dcel.numUndirectedEdges mesh, Dcel.numFaces mesh)
      (incidenceVertexCount incidence, incidenceDirectedEdgeCount incidence, incidenceEdgeCount incidence, incidenceFaceCount incidence)
    assertEqual "native non-cellular support is empty" [] (incidenceNonCellularFaces incidence)
    traverse_
      (\index ->
         let edge = DirectedEdgeId (fromIntegral index)
          in assertEqual "native packed dart observations"
               (Dcel.origin mesh edge, Dcel.destination mesh edge, Dcel.next mesh edge, Dcel.previous mesh edge, Dcel.incidentFace mesh edge)
               (incidenceOrigin incidence edge, incidenceDestination incidence edge, incidenceNext incidence edge, incidencePrevious incidence edge, incidenceIncidentFace incidence edge))
      [0 .. Dcel.numDirectedEdges mesh - 1]
    traverse_
      (\index ->
         let vertex = VertexId (fromIntegral index)
          in assertEqual "native vertex star observations"
               (Dcel.vertexOutgoingEdges mesh vertex) (incidenceVertexOutgoingEdges incidence vertex))
      [0 .. Dcel.numVertices mesh - 1]
    traverse_
      (\index ->
         let face = FaceId (fromIntegral index)
          in assertEqual "native face cycle observations"
               (Dcel.faceDirectedEdges mesh face) (concat (faceBoundaryComponents incidence face)))
      [0 .. Dcel.numFaces mesh - 1]

testExactFaceComponents :: IO ()
testExactFaceComponents = do
  incidence <- requireRight "annulus with isolated puncture" fixtureIncidence
  assertEqual "both oriented annulus boundary components"
    (fmap (fmap DirectedEdgeId) [[0, 2, 4, 6], [9, 15, 13, 11]])
    (faceBoundaryComponents incidence (FaceId 1))
  assertEqual "puncture belongs to annular face" [VertexId 8]
    (faceIsolatedVertices incidence (FaceId 1))
  assertEqual "sparse non-cellular support" [FaceId 1] (incidenceNonCellularFaces incidence)
  assertEqual "one hole and one puncture have two boundary components"
    [BoundaryCycleRoot (DirectedEdgeId 9), IsolatedBoundaryVertex (VertexId 8)]
    (faceInnerBoundaryComponents incidence (FaceId 1))
  assertEqual "open annulus minus one point has Euler contribution minus one" (-1)
    (faceEulerContribution incidence (FaceId 1))
  assertEqual "disk face remains a cell" 1 (faceEulerContribution incidence (FaceId 2))
  closure <- requireRight "exact annular closure"
    (closeExactCellSetWith fixturePoint incidence [] [] [FaceId 1])
  assertEqual "closure includes all cycles and isolated boundary vertices" (9, 8, 1)
    (exactCellSetVertexCount closure, exactCellSetEdgeCount closure, exactCellSetFaceCount closure)
  assertEqual "arrangement closure is recognized without a native mesh" True
    (exactCellSetIsFaceClosure closure)
  singleton <- requireRight "explicit isolated vertex selection"
    (closeExactCellSetWith fixturePoint incidence [VertexId 8] [] [])
  assertEqual "an explicit vertex is not a face closure" False (exactCellSetIsFaceClosure singleton)

testPackedRefusals :: IO ()
testPackedRefusals = sequence_
  [ expectRefusal "paired-edge layout" isLayout
      (admitPlanarIncidence fixtureVertices (wordsPlane [0]) fixtureFaces fixtureComponents fixtureIsolated)
  , expectRefusal "duplicate CCB roots" isBoundaryCoverage
      (admitPlanarIncidence fixtureVertices fixtureTopology fixtureFaces
        (IntMap.singleton 1 [DirectedEdgeId 9, DirectedEdgeId 9]) fixtureIsolated)
  , expectRefusal "unowned isolated point" isIsolatedCoverage
      (admitPlanarIncidence fixtureVertices fixtureTopology fixtureFaces fixtureComponents IntMap.empty)
  , expectRefusal "multiply owned isolated point" isIsolatedCoverage
      (admitPlanarIncidence fixtureVertices fixtureTopology fixtureFaces fixtureComponents
        (IntMap.insert 0 (IntSet.singleton 8) fixtureIsolated))
  , expectRefusal "nonexistent isolated point" isIsolatedCoverage
      (admitPlanarIncidence fixtureVertices fixtureTopology fixtureFaces fixtureComponents
        (IntMap.singleton 1 (IntSet.fromList [8, 99])))
  , expectRefusal "missing bounded outer root" isBoundedRoot
      (admitPlanarIncidence fixtureVertices fixtureTopology (wordsPlane [1, noIndex, 8])
        fixtureComponents fixtureIsolated)
  ]
 where
  expectRefusal :: String -> (PlanarIncidenceError -> Bool) -> Either PlanarIncidenceError PlanarIncidence -> IO ()
  expectRefusal label matches outcome = case outcome of
    Left obstruction | matches obstruction -> pure ()
    other -> fail (label <> " lost its typed incidence refusal: " <> show other)
  isLayout, isBoundaryCoverage, isIsolatedCoverage, isBoundedRoot :: PlanarIncidenceError -> Bool
  isLayout (IncidencePackedLayoutInvalid _ _ _) = True
  isLayout _ = False
  isBoundaryCoverage (IncidenceBoundaryCoverageInvalid _ _) = True
  isBoundaryCoverage _ = False
  isIsolatedCoverage (IncidenceIsolatedOwnershipInvalid _ _) = True
  isIsolatedCoverage _ = False
  isBoundedRoot (IncidenceBoundedFaceWithoutBoundary _) = True
  isBoundedRoot _ = False

fixtureIncidence :: Either PlanarIncidenceError PlanarIncidence
fixtureIncidence = admitPlanarIncidence fixtureVertices fixtureTopology fixtureFaces fixtureComponents fixtureIsolated

fixtureVertices, fixtureTopology, fixtureFaces :: Paged Word32
fixtureVertices = wordsPlane [0, 2, 4, 6, 8, 10, 12, 14, noIndex]
fixtureFaces = wordsPlane [1, 0, 8]
fixtureTopology = wordsPlane (concat
  [ [0, 2, 6, 1], [1, 7, 3, 0]
  , [1, 4, 0, 1], [2, 1, 5, 0]
  , [2, 6, 2, 1], [3, 3, 7, 0]
  , [3, 0, 4, 1], [0, 5, 1, 0]
  , [4, 10, 14, 2], [5, 15, 11, 1]
  , [5, 12, 8, 2], [6, 9, 13, 1]
  , [6, 14, 10, 2], [7, 11, 15, 1]
  , [7, 8, 12, 2], [4, 13, 9, 1]
  ])

fixtureComponents :: IntMap.IntMap [DirectedEdgeId]
fixtureComponents = IntMap.singleton 1 [DirectedEdgeId 9]

fixtureIsolated :: IntMap.IntMap IntSet.IntSet
fixtureIsolated = IntMap.singleton 1 (IntSet.singleton 8)

wordsPlane :: [Word32] -> Paged Word32
wordsPlane = fromVector noIndex . U.fromList

fixturePoint :: VertexId -> Either CellSelectionError ExactPoint
fixturePoint vertex = case vertexIdIndex vertex of
  0 -> Right (integerPoint 0 0)
  1 -> Right (integerPoint 8 0)
  2 -> Right (integerPoint 8 8)
  3 -> Right (integerPoint 0 8)
  4 -> Right (integerPoint 2 2)
  5 -> Right (integerPoint 6 2)
  6 -> Right (integerPoint 6 6)
  7 -> Right (integerPoint 2 6)
  8 -> Right (integerPoint 1 1)
  _ -> Left (CellVertexOutOfRange vertex 9)
