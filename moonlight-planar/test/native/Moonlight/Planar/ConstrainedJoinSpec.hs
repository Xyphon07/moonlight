{-# LANGUAGE DataKinds #-}

-- | Annotated constrained union and asymmetric extension laws.
module Moonlight.Planar.ConstrainedJoinSpec
  ( tests
  ) where

import Control.DeepSeq ( force )
import Control.Exception ( evaluate )
import Moonlight.Planar.Cdt ( constrainedDelaunay, constraintSegments, extendConstrainedWith,
  unionConstrained, unionConstrainedWith, CanonicalSegment, CdtError(ConstraintIntersection),
  ConstrainedExtensionResult(constrainedExtensionBuildStats, constrainedExtensionConstraintBatch),
  ConstrainedUnionError(ConstraintUnionConstructionFailed),
  ConstraintBatchResult(constraintBatchStats, constraintBatchOutcomes,
  constraintBatchTriangulation), ConstraintBatchStats(constraintBatchRequests,
  constraintBatchAccepted, constraintBatchRejected) )
import Moonlight.Planar.Dcel ( mapVertices, vertexData, vertexPoint )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( vertices )
import Moonlight.Planar.Internal.HandleDefs ( VertexId )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Types (unitElementDefaults, ConstraintMode(Constrained), BuildResult(buildTriangulation), Triangulation)
import Moonlight.Planar.BuildStats (BuildMetric (InputPoints, UniquePoints, ExistingPoints), buildStat)
import Support ( assertEqual, assertValid, requireRight )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testAnnotatedConstrainedUnion
    , testAsymmetricConstrainedExtension
    , testLargeAsymmetricConstrainedExtension
    ]

testAnnotatedConstrainedUnion :: IO ()
testAnnotatedConstrainedUnion = do
  let leftPoints :: V.Vector (Point)
      leftPoints = V.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2]
      rightPoints :: V.Vector (Point)
      rightPoints = V.fromList [Point 2 0, Point 4 0, Point 4 2, Point 2 2]
      thirdPoints :: V.Vector (Point)
      thirdPoints = V.fromList [Point 4 0, Point 6 0, Point 6 2, Point 4 2]
  leftBuild <-
    requireRight "annotated constrained union left" $
      constrainedDelaunay
        unitElementDefaults
        leftPoints
        (V.singleton (0, 2))
  rightBuild <-
    requireRight "annotated constrained union right" $
      constrainedDelaunay
        unitElementDefaults
        rightPoints
        (V.singleton (0, 2))
  thirdBuild <-
    requireRight "annotated constrained union third" $
      constrainedDelaunay
        unitElementDefaults
        thirdPoints
        (V.singleton (0, 2))
  let left =
        mapVertices
          (const (Set.singleton "left"))
          (buildTriangulation leftBuild)
      right =
        mapVertices
          (const (Set.singleton "right"))
          (buildTriangulation rightBuild)
      third =
        mapVertices
          (const (Set.singleton "third"))
          (buildTriangulation thirdBuild)
  joined <-
    requireRight
      "annotated constrained union"
      (unionConstrainedWith Set.union left right)
  let expectedAnnotations =
        Map.fromList
          [ (Point 0 0, Set.singleton "left")
          , (Point 0 2, Set.singleton "left")
          , (Point 2 0, Set.fromList ["left", "right"])
          , (Point 2 2, Set.fromList ["left", "right"])
          , (Point 4 0, Set.singleton "right")
          , (Point 4 2, Set.singleton "right")
          ]
      actualAnnotations =
        Map.fromList
          [ (vertexPoint joined vertex, vertexData joined vertex)
          | vertex <- vertices joined
          ]
      expectedConstraints =
        Set.union
          (Set.fromList (V.toList (constraintSegments left)))
          (Set.fromList (V.toList (constraintSegments right)))
  assertEqual
    "annotated constrained union preserves and combines site payloads"
    expectedAnnotations
    actualAnnotations
  assertEqual
    "annotated constrained union preserves both constraint sections"
    expectedConstraints
    (Set.fromList (V.toList (constraintSegments joined)))
  assertValid "annotated constrained union" joined
  commuted <-
    requireRight
      "annotated constrained union commuted"
      (unionConstrainedWith Set.union right left)
  assertEqual
    "annotated constrained union is commutative under a commutative payload combiner"
    joined
    commuted
  leftAssociated <-
    requireRight
      "annotated constrained union left-associated"
      (unionConstrainedWith Set.union joined third)
  rightPair <-
    requireRight
      "annotated constrained union right pair"
      (unionConstrainedWith Set.union right third)
  rightAssociated <-
    requireRight
      "annotated constrained union right-associated"
      (unionConstrainedWith Set.union left rightPair)
  assertEqual
    "annotated constrained union is associative wherever both descents are admitted"
    leftAssociated
    rightAssociated
  unitJoined <-
    requireRight
      "unit constrained union specialization"
      ( unionConstrained
          (mapVertices (const ()) left)
          (mapVertices (const ()) right)
      )
  assertEqual
    "unit constrained union specializes annotated constrained union"
    unitJoined
    (mapVertices (const ()) joined)

-- | Sequential extension retains the base authority as the transaction root.
-- The semantic result agrees with canonical union on sites and constraint
-- sections, but its receipt proves that only the incoming constraint section
-- was interpreted.
testAsymmetricConstrainedExtension :: IO ()
testAsymmetricConstrainedExtension = do
  let basePoints :: V.Vector (Point)
      basePoints = V.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2]
      extensionPoints :: V.Vector (Point)
      extensionPoints = V.fromList [Point 2 0, Point 4 0, Point 4 2, Point 2 2]
  baseBuild <-
    requireRight
      "asymmetric extension base"
      (constrainedDelaunay unitElementDefaults basePoints (V.singleton (0, 2)))
  extensionBuild <-
    requireRight
      "asymmetric extension incoming"
      (constrainedDelaunay unitElementDefaults extensionPoints (V.singleton (0, 2)))
  let base = mapVertices (const (Set.singleton "base")) (buildTriangulation baseBuild)
      extension = mapVertices (const (Set.singleton "extension")) (buildTriangulation extensionBuild)
      expectedAnnotations =
        Map.fromList
          [ (Point 0 0, Set.singleton "base")
          , (Point 0 2, Set.singleton "base")
          , (Point 2 0, Set.fromList ["base", "extension"])
          , (Point 2 2, Set.fromList ["base", "extension"])
          , (Point 4 0, Set.singleton "extension")
          , (Point 4 2, Set.singleton "extension")
          ]
      expectedConstraints =
        Set.union
          (Set.fromList (V.toList (constraintSegments base)))
          (Set.fromList (V.toList (constraintSegments extension)))
      baseAnnotations =
        Map.fromList
          [ (vertexPoint base vertex, vertexData base vertex)
          | vertex <- vertices base
          ]
      extensionAnnotations =
        Map.fromList
          [ (vertexPoint extension vertex, vertexData extension vertex)
          | vertex <- vertices extension
          ]
  baseAnnotationsBefore <- evaluate (force baseAnnotations)
  baseConstraintsBefore <- evaluate (force (constraintSegments base))
  extensionAnnotationsBefore <- evaluate (force extensionAnnotations)
  extensionConstraintsBefore <- evaluate (force (constraintSegments extension))
  extendedResult <-
    requireRight
      "asymmetric constrained extension"
      (extendConstrainedWith Set.union base extension)
  let constraintBatch = constrainedExtensionConstraintBatch extendedResult
      extended = constraintBatchTriangulation constraintBatch
      receipt = constraintBatchStats constraintBatch
      buildReceipt = constrainedExtensionBuildStats extendedResult
      actualAnnotations =
        Map.fromList
          [ (vertexPoint extended vertex, vertexData extended vertex)
          | vertex <- vertices extended
          ]
  assertEqual
    "asymmetric extension preserves base and combines coincident site payloads"
    expectedAnnotations
    actualAnnotations
  assertEqual
    "asymmetric extension preserves resident and incoming constraint sections"
    expectedConstraints
    (Set.fromList (V.toList (constraintSegments extended)))
  assertEqual
    "asymmetric extension does not mutate the frozen base predecessor sites"
    baseAnnotationsBefore
    ( Map.fromList
        [ (vertexPoint base vertex, vertexData base vertex)
        | vertex <- vertices base
        ]
    )
  assertEqual
    "asymmetric extension does not mutate the frozen base predecessor constraints"
    baseConstraintsBefore
    (constraintSegments base)
  assertEqual
    "asymmetric extension does not mutate the frozen incoming predecessor sites"
    extensionAnnotationsBefore
    ( Map.fromList
        [ (vertexPoint extension vertex, vertexData extension vertex)
        | vertex <- vertices extension
        ]
    )
  assertEqual
    "asymmetric extension does not mutate the frozen incoming predecessor constraints"
    extensionConstraintsBefore
    (constraintSegments extension)
  assertEqual
    "asymmetric extension recovers only incoming constraints"
    (V.length (constraintSegments extension))
    (constraintBatchRequests receipt)
  assertEqual
    "asymmetric extension returns one outcome for each incoming constraint"
    (V.length (constraintSegments extension))
    (V.length (constraintBatchOutcomes constraintBatch))
  assertEqual
    "asymmetric extension admits every incoming constraint"
    (V.length (constraintSegments extension))
    (constraintBatchAccepted receipt)
  assertEqual
    "asymmetric extension reports no incoming constraint rejection"
    0
    (constraintBatchRejected receipt)
  assertEqual
    "asymmetric extension charges every incoming site once"
    (V.length extensionPoints)
    ((buildStat InputPoints) buildReceipt)
  assertEqual
    "asymmetric extension distinguishes occupied incoming sites"
    2
    ((buildStat ExistingPoints) buildReceipt)
  assertEqual
    "asymmetric extension distinguishes newly materialized incoming sites"
    2
    ((buildStat UniquePoints) buildReceipt)
  assertValid "asymmetric constrained extension" extended

  crossingBuild <-
    requireRight
      "asymmetric extension crossing incoming section"
      (constrainedDelaunay unitElementDefaults basePoints (V.singleton (1, 3)))
  case
      extendConstrainedWith
        Set.union
        base
        (mapVertices (const (Set.singleton "crossing")) (buildTriangulation crossingBuild)) of
    Left (ConstraintUnionConstructionFailed (ConstraintIntersection _)) -> pure ()
    other -> fail ("asymmetric extension did not return the corridor intersection witness: " <> show other)

testLargeAsymmetricConstrainedExtension :: IO ()
testLargeAsymmetricConstrainedExtension = do
  let width = 40 :: Int
      height = 28 :: Int
      basePoints :: V.Vector (Point)
      basePoints =
        V.fromList
          ( [ Point 0 0
            , Point (fromIntegral (width + 1)) 0
            , Point (fromIntegral (width + 1)) (fromIntegral (height + 1))
            , Point 0 (fromIntegral (height + 1))
            ]
              <> [ Point
                     (fromIntegral (column + 1) + fromIntegral ((column * 17 + row * 31) `mod` 13) * 1.0e-3)
                     (fromIntegral (row + 1) + fromIntegral ((column * 23 + row * 19) `mod` 17) * 1.0e-3)
                 | row <- [0 .. height - 1]
                 , column <- [0 .. width - 1]
                 ]
          )
      baseConstraints = V.fromList [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2)]
      extensionPoints :: V.Vector (Point)
      extensionPoints = V.fromList [Point 48 8, Point 53.2 8.4, Point 50.4 15.8]
      extensionConstraints = V.fromList [(0, 1), (1, 2), (2, 0)]
      conflictPoints :: V.Vector (Point)
      conflictPoints = V.fromList [Point 8 22, Point 30 5, Point 26 7]
  baseBuild <-
    requireRight
      "large asymmetric extension base"
      (constrainedDelaunay unitElementDefaults basePoints baseConstraints)
  extensionBuild <-
    requireRight
      "large asymmetric extension incoming"
      (constrainedDelaunay unitElementDefaults extensionPoints extensionConstraints)
  conflictBuild <-
    requireRight
      "large asymmetric extension conflicting incoming"
      (constrainedDelaunay unitElementDefaults conflictPoints (V.singleton (0, 1)))
  let base = mapVertices (const (Set.singleton "base")) (buildTriangulation baseBuild)
      extension = mapVertices (const (Set.singleton "extension")) (buildTriangulation extensionBuild)
      conflicting = mapVertices (const (Set.singleton "conflict")) (buildTriangulation conflictBuild)
      predecessor
        :: Triangulation 'Constrained (Set.Set String) () () ()
        -> ( Triangulation 'Constrained (Set.Set String) () () ()
           , Map.Map VertexId (Point, Set.Set String)
           , Set.Set (CanonicalSegment)
           )
      predecessor triangulation =
        ( triangulation
        , Map.fromList
            [ (vertex, (vertexPoint triangulation vertex, vertexData triangulation vertex))
            | vertex <- vertices triangulation
            ]
        , Set.fromList (V.toList (constraintSegments triangulation))
        )
      assertPredecessor
        :: String
        -> ( Triangulation 'Constrained (Set.Set String) () () ()
           , Map.Map VertexId (Point, Set.Set String)
           , Set.Set (CanonicalSegment)
           )
        -> Triangulation 'Constrained (Set.Set String) () () ()
        -> IO ()
      assertPredecessor label snapshot triangulation =
        assertEqual label snapshot (predecessor triangulation)
  baseBefore <- evaluate (force (predecessor base))
  extensionBefore <- evaluate (force (predecessor extension))
  conflictingBefore <- evaluate (force (predecessor conflicting))
  let (_, baseVertexSnapshotBefore, baseConstraintSnapshotBefore) = baseBefore
      (_, _, extensionConstraintSnapshotBefore) = extensionBefore
  extensionResult <-
    requireRight
      "large asymmetric constrained extension"
      (extendConstrainedWith Set.union base extension)
  let constraintBatch = constrainedExtensionConstraintBatch extensionResult
      extended = constraintBatchTriangulation constraintBatch
      extendedConstraints = Set.fromList (V.toList (constraintSegments extended))
      extendedVertexSnapshot =
        Map.fromList
          [ (vertex, (vertexPoint extended vertex, vertexData extended vertex))
          | vertex <- vertices extended
          ]
  assertEqual
    "large asymmetric extension retains every base vertex handle, coordinate, and payload"
    baseVertexSnapshotBefore
    (Map.restrictKeys extendedVertexSnapshot (Map.keysSet baseVertexSnapshotBefore))
  assertEqual
    "large asymmetric extension retains its complete base constraint source section"
    baseConstraintSnapshotBefore
    (Set.intersection baseConstraintSnapshotBefore extendedConstraints)
  assertEqual
    "large asymmetric extension retains its complete incoming constraint source section"
    extensionConstraintSnapshotBefore
    (Set.intersection extensionConstraintSnapshotBefore extendedConstraints)
  assertEqual
    "large asymmetric extension preserves exactly both source constraint sections"
    (Set.union baseConstraintSnapshotBefore extensionConstraintSnapshotBefore)
    extendedConstraints
  assertEqual
    "large asymmetric extension leaves the frozen base predecessor physically unchanged"
    baseBefore
    (predecessor base)
  assertPredecessor
    "large asymmetric extension leaves the frozen incoming predecessor physically unchanged"
    extensionBefore
    extension
  assertEqual
    "large asymmetric extension charges only the incoming sites"
    (V.length extensionPoints)
    ((buildStat InputPoints) (constrainedExtensionBuildStats extensionResult))
  assertEqual
    "large asymmetric extension replays only the incoming constraints"
    (V.length extensionConstraints)
    (constraintBatchRequests (constraintBatchStats constraintBatch))
  assertValid "large asymmetric constrained extension" extended
  case extendConstrainedWith Set.union base conflicting of
    Left (ConstraintUnionConstructionFailed (ConstraintIntersection _)) -> pure ()
    outcome ->
      fail
        ( "large asymmetric extension did not refuse the crossing incoming corridor: "
            <> show outcome
        )
  assertEqual
    "large asymmetric extension refusal leaves the frozen base predecessor physically unchanged"
    baseBefore
    (predecessor base)
  assertPredecessor
    "large asymmetric extension refusal leaves the conflicting predecessor physically unchanged"
    conflictingBefore
    conflicting

-- | A separated constrained seam is a restriction-preserving operation, not
-- the canonical site-set union wearing a cheaper costume. Both closed source
-- sections survive face-for-face; only the corridor contributes new faces.
