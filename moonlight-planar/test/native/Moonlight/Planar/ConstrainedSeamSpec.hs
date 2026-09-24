{-# LANGUAGE DataKinds #-}

-- | Separated seams, protected interfaces, and subsequent refinement laws.
module Moonlight.Planar.ConstrainedSeamSpec
  ( tests
  ) where

import Control.Monad ( forM_, unless, when )
import Moonlight.Planar.Cdt ( constrainedDelaunay, boundedRegionFaces, constraintSegments,
  joinSeparatedConstrained, CanonicalSegment(..), CdtError(CdtBuildError),
  ConstrainedSeamFaceEvidence(constrainedSeamSourceFace, constrainedSeamTargetFace,
  constrainedSeamFaceFirstPoint, constrainedSeamFaceSecondPoint, constrainedSeamFaceThirdPoint),
  ConstrainedSeamResult(constrainedSeamRightFaceEvidence, constrainedSeamLeftFaceCount,
  constrainedSeamLeftConstraintCount, constrainedSeamConstraintStats,
  constrainedSeamCachedFrontierPointReads, constrainedSeamPublicationStats,
  constrainedSeamJoinFaces, constrainedSeamResultTriangulation), ConstrainedSeamSide(SeamIncoming,
  SeamResident), ConstrainedUnionError(ConstraintUnionConstructionFailed,
  ConstraintUnionNotSeparated), ConstraintBatchStats(constraintBatchRequests) )
import Moonlight.Planar.Dcel ( numConstraints, numInnerFaces, numVertices, vertexData, vertexPoint )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( vertices, innerFaces )
import Moonlight.Planar.Internal.Paged ( PublicationStats(..) )
import Moonlight.Planar.Internal.Types ( RefinementParameters(..) )
import Moonlight.Planar.Math ( squaredDistanceWide )
import Moonlight.Planar.MeshFixtures ( faceKeyOf )
import Moonlight.Planar.Refinement ( refineWithinDomain )
import Moonlight.Planar.RefinementAssertions ( assertClosureCountsAgree )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Types (defaultRefinementParameters, unitElementDefaults, BuildError(SeamSourceEdgeRequiresFlip, RefinementSeamBridgeBudgetExceeded), ConstraintMode(Constrained), BuildResult(buildTriangulation), RefinementDomainResult(refinementDomainResult), RefinementResult(refinementComplete,
  refinedTriangulation), Triangulation)
import Support ( assertEqual, assertValid, requireRight )
import Moonlight.Planar.Internal.Representation qualified as Internal
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testSeparatedConstrainedSeam
    , testAxisSeparatedConstrainedSeams
    , testSeparatedConstrainedSeamRefusesPinnedFrontierRewrite
    ]

testSeparatedConstrainedSeam :: IO ()
testSeparatedConstrainedSeam = do
  let leftPoints :: V.Vector (Point)
      leftPoints =
        V.fromList
          [ Point (-4) (-1)
          , Point (-2) (-1)
          , Point (-2) 1
          , Point (-4) 1
          ]
      rightPoints :: V.Vector (Point)
      rightPoints =
        V.fromList
          [ Point 2 (-1)
          , Point 4 (-1)
          , Point 4 1
          , Point 2 1
          ]
      closedContour = V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)]
  leftBuild <-
    requireRight
      "separated constrained seam left"
      (constrainedDelaunay unitElementDefaults leftPoints closedContour)
  rightBuild <-
    requireRight
      "separated constrained seam right"
      (constrainedDelaunay unitElementDefaults rightPoints closedContour)
  let left =
        Internal.geometryOnlyPublication
          (buildTriangulation leftBuild)
      right =
        Internal.geometryOnlyPublication
          (buildTriangulation rightBuild)
      leftFaceKeys = Set.fromList (fmap (faceKeyOf left) (innerFaces left))
      rightFaceKeys = Set.fromList (fmap (faceKeyOf right) (innerFaces right))
      expectedConstraints =
        Set.union
          (Set.fromList (V.toList (constraintSegments left)))
          (Set.fromList (V.toList (constraintSegments right)))
  seam <-
    requireRight
      "source-preserving separated constrained seam"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters left right)
  let joined = constrainedSeamResultTriangulation seam
      joinedFaceKeys = Set.fromList (fmap (faceKeyOf joined) (innerFaces joined))
      actualAnnotations =
        Map.fromList
          [ (vertexPoint joined vertex, vertexData joined vertex)
          | vertex <- vertices joined
          ]
      expectedAnnotations =
        Map.union
          ( Map.fromList
              [ (vertexPoint left vertex, vertexData left vertex)
              | vertex <- vertices left
              ]
          )
          ( Map.fromList
              [ (vertexPoint right vertex, vertexData right vertex)
              | vertex <- vertices right
              ]
          )
      actualConstraints = Set.fromList (V.toList (constraintSegments joined))
      certifiedPerimeter =
        fmap
          (\segmentValue -> (segmentStart segmentValue, segmentEnd segmentValue))
          (Set.toAscList (Set.difference actualConstraints expectedConstraints))
  assertValid "source-preserving separated constrained seam" joined
  assertEqual
    "separated seam preserves all source annotations"
    expectedAnnotations
    actualAnnotations
  assertEqual
    "separated seam preserves both source constraint sections"
    True
    (expectedConstraints `Set.isSubsetOf` actualConstraints)
  assertEqual
    "separated seam certifies exactly its two synthetic perimeter bridges"
    [ (Point (-2) (-1), Point 2 (-1))
    , (Point (-2) 1, Point 2 1)
    ]
    certifiedPerimeter
  unless (leftFaceKeys `Set.isSubsetOf` joinedFaceKeys) $
    fail "separated seam removed or retriangulated a left source face"
  unless (rightFaceKeys `Set.isSubsetOf` joinedFaceKeys) $
    fail "separated seam removed or retriangulated a right source face"
  assertEqual
    "separated seam records the stable left face witness"
    (numInnerFaces left)
    (constrainedSeamLeftFaceCount seam)
  assertEqual
    "separated seam records the stable left constraint witness"
    (numConstraints left)
    (constrainedSeamLeftConstraintCount seam)
  assertEqual
    "separated seam has one right face witness per source face"
    (numInnerFaces right)
    (V.length (constrainedSeamRightFaceEvidence seam))
  forM_ (V.toList (constrainedSeamRightFaceEvidence seam)) $ \evidence ->
    let sourceFaceKey = faceKeyOf right (constrainedSeamSourceFace evidence)
        targetFaceKey = faceKeyOf joined (constrainedSeamTargetFace evidence)
        evidenceFaceKey =
          [ constrainedSeamFaceFirstPoint evidence
          , constrainedSeamFaceSecondPoint evidence
          , constrainedSeamFaceThirdPoint evidence
          ]
     in do
          assertEqual
            "right face witness names the exact target triangle"
            sourceFaceKey
            targetFaceKey
          assertEqual
            "right face witness carries the exact target triangle"
            targetFaceKey
            evidenceFaceKey
  when (V.null (constrainedSeamJoinFaces seam)) $
    fail "separated seam did not identify any new corridor face"
  assertEqual
    "copied source constraints require no corridor recovery"
    0
    (constraintBatchRequests (constrainedSeamConstraintStats seam))
  let seamPublication = constrainedSeamPublicationStats seam
  assertEqual
    "separated seam does not enumerate resident unboxed base pages"
    0
    (publicationUnboxedBasePageEnumerations seamPublication)
  assertEqual
    "separated seam does not freeze resident unboxed base pages"
    0
    (publicationUnboxedBasePageFreezes seamPublication)
  assertEqual
    "separated seam does not enumerate resident boxed base pages"
    0
    (publicationBoxedBasePageEnumerations seamPublication)
  unless
    ( publicationUnboxedDirtyBasePages seamPublication
        + publicationUnboxedDirtyAppendedPages seamPublication
        > 0
    ) $
    fail "separated seam publication did not record appended unboxed writes"
  unless (constrainedSeamCachedFrontierPointReads seam > 0) $
    fail "separated seam did not record cached frontier point reads"

  let boundedBridgeParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 6
          , refineMaxEdgeLength = Just 1
          }
  boundedSeam <-
    requireRight
      "edge-bounded separated constrained seam"
      ( joinSeparatedConstrained
          (\_ _ -> True)
          boundedBridgeParameters
          left
          right
      )
  let boundedJoined = constrainedSeamResultTriangulation boundedSeam
      boundedSyntheticSegments =
        Set.difference
          (Set.fromList (V.toList (constraintSegments boundedJoined)))
          expectedConstraints
  assertValid "edge-bounded separated constrained seam" boundedJoined
  assertEqual
    "edge-bounded seam charges its exact bridge Steiner budget"
    6
    (numVertices boundedJoined - numVertices left - numVertices right)
  assertEqual
    "edge-bounded seam publishes both four-edge bridge paths"
    8
    (Set.size boundedSyntheticSegments)
  unless (all ((<= 1) . canonicalSegmentSquaredLength) boundedSyntheticSegments) $
    fail "edge-bounded seam published an oversized synthetic bridge child"
  assertEqual
    "edge-bounded seam cache agrees with its explicit frontier observation"
    (Internal.prepareSeamFrontierIndex boundedJoined)
    (Internal.triSeamFrontier boundedJoined)
  case
      joinSeparatedConstrained
        (\_ _ -> True)
        boundedBridgeParameters{refineMaxAdditionalVertices = Just 5}
        left
        right of
    Left
      ( ConstraintUnionConstructionFailed
          (CdtBuildError (RefinementSeamBridgeBudgetExceeded 6 5))
        ) -> pure ()
    outcome ->
      fail
        ( "edge-bounded seam did not return its exact budget obstruction: "
            <> either show (const "success") outcome
        )

  reversed <- requireRight "reversed source-preserving separated seam" (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters right left)
  assertValid "reversed source-preserving separated seam" (constrainedSeamResultTriangulation reversed)

  let localParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 2
          , refineMaxArea = Just 0.5
          , refineKeepConstraintEdges = True
          }
      allJoinedFaces = Set.fromList (innerFaces joined)
  locallyRefined <-
    requireRight
      "seam refinement preserves cached hull frontier"
      (refineWithinDomain (const ()) localParameters allJoinedFaces joined)
  assertClosureCountsAgree "seam refinement" allJoinedFaces locallyRefined
  let refinedJoined = refinedTriangulation (refinementDomainResult locallyRefined)
  unless (refinementComplete (refinementDomainResult locallyRefined)) $
    fail "seam refinement fixture did not drain its local worklist"
  assertEqual
    "local refinement retains the exact cached hull frontier"
    (Internal.triSeamFrontier joined)
    (Internal.triSeamFrontier refinedJoined)

  thirdBuild <-
    requireRight
      "repeated separated constrained seam source"
      (constrainedDelaunay unitElementDefaults (V.fromList
        [ Point 8 (-1)
        , Point 10 (-1)
        , Point 10 1
        , Point 8 1
        ]) closedContour)
  let third =
        Internal.geometryOnlyPublication
          (buildTriangulation thirdBuild)
  repeated <-
    requireRight
      "repeated separated seam after local refinement"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters refinedJoined third)
  assertValid
    "repeated separated seam after local refinement"
    (constrainedSeamResultTriangulation repeated)
  case joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters left left of
    Left ConstraintUnionNotSeparated -> pure ()
    other -> fail ("non-separated constrained seam was not refused: " <> show other)

-- North/south additions use the orientation-preserving Y chart rather than
-- rotating or copying the resident mesh.  The sequence then alternates axes,
-- so a stale one-axis frontier cache cannot accidentally make the third join
-- succeed.
testAxisSeparatedConstrainedSeams :: IO ()
testAxisSeparatedConstrainedSeams = do
  let closedContour :: Int -> V.Vector (Int, Int)
      closedContour pointCount =
        V.fromList
          [ (index, (index + 1) `mod` pointCount)
          | index <- [0 .. pointCount - 1]
          ]
      buildPublished
        :: String
        -> V.Vector Point
        -> IO (Triangulation 'Constrained () () () ())
      buildPublished label points = do
        built <-
          requireRight
            label
            (constrainedDelaunay
               unitElementDefaults
               points
               (closedContour (V.length points)))
        pure (Internal.geometryOnlyPublication (buildTriangulation built))
      centralPoints =
        V.fromList
          [ Point (-1) (-1)
          , Point 1 (-1)
          , Point 1 1
          , Point (-1) 1
          ]
      northPoints =
        V.fromList
          [ Point (-1) 3
          , Point 1 3
          , Point 1 5
          , Point (-1) 5
          ]
      southPoints =
        V.fromList
          [ Point (-1) (-5)
          , Point 1 (-5)
          , Point 1 (-3)
          , Point (-1) (-3)
          ]
      eastPoints =
        V.fromList
          [ Point 3 (-1)
          , Point 5 (-1)
          , Point 5 1
          , Point 3 1
          ]
      overlapPoints =
        V.fromList
          [ Point 0 0
          , Point 2 0
          , Point 2 2
          , Point 0 2
          ]
      irregularNorthPoints =
        V.fromList
          [ Point (-2) 7
          , Point 0 7
          , Point 2 7
          , Point 2.5 9
          , Point 0 11
          , Point (-2.5) 9
          ]
  central <- buildPublished "axis seam central source" centralPoints
  north <- buildPublished "axis seam north source" northPoints
  south <- buildPublished "axis seam south source" southPoints
  east <- buildPublished "axis seam east source" eastPoints
  overlap <- buildPublished "axis seam overlapping source" overlapPoints

  let northParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 2
          , refineMaxEdgeLength = Just 1
          }
  northJoined <-
    requireRight
      "edge-bounded north source-preserving separated seam"
      (joinSeparatedConstrained (\_ _ -> True) northParameters central north)
  let northTopology = constrainedSeamResultTriangulation northJoined
      northPublication = constrainedSeamPublicationStats northJoined
      northSyntheticSegments =
        Set.difference
          (Set.fromList (V.toList (constraintSegments northTopology)))
          ( Set.union
              (Set.fromList (V.toList (constraintSegments central)))
              (Set.fromList (V.toList (constraintSegments north)))
          )
  assertValid
    "north source-preserving separated seam"
    northTopology
  assertEqual
    "north seam charges two bridge Steiner vertices"
    2
    (numVertices northTopology - numVertices central - numVertices north)
  assertEqual
    "north seam certifies two bounded two-edge bridge paths"
    4
    (Set.size northSyntheticSegments)
  unless (all ((<= 1) . canonicalSegmentSquaredLength) northSyntheticSegments) $
    fail "north seam published an oversized synthetic bridge child"
  assertEqual
    "north seam cache agrees with an explicit frontier observation"
    (Internal.prepareSeamFrontierIndex northTopology)
    (Internal.triSeamFrontier northTopology)
  assertEqual
    "north seam does not enumerate resident unboxed pages"
    0
    (publicationUnboxedBasePageEnumerations northPublication)
  assertEqual
    "north seam does not freeze resident unboxed pages"
    0
    (publicationUnboxedBasePageFreezes northPublication)
  assertEqual
    "north seam does not enumerate resident boxed pages"
    0
    (publicationBoxedBasePageEnumerations northPublication)
  unless
    ( publicationUnboxedDirtyBasePages northPublication
        + publicationUnboxedDirtyAppendedPages northPublication
        > 0
    ) $
    fail "north seam did not record appended unboxed writes"

  reversed <-
    requireRight
      "reversed south/north source-preserving separated seam"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters south central)
  assertValid
    "reversed south/north source-preserving separated seam"
    (constrainedSeamResultTriangulation reversed)

  mixed <-
    requireRight
      "mixed-axis east extension after north seam"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters
         (constrainedSeamResultTriangulation northJoined)
         east)
  assertValid
    "mixed-axis east extension after north seam"
    (constrainedSeamResultTriangulation mixed)
  assertEqual
    "mixed-axis seam retains prior perimeter and certifies two new bridges"
    ( numConstraints (constrainedSeamResultTriangulation northJoined)
        + numConstraints east
        + 2
    )
    (numConstraints (constrainedSeamResultTriangulation mixed))

  repeated <-
    requireRight
      "repeated mixed-axis south extension"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters
         (constrainedSeamResultTriangulation mixed)
         south)
  assertValid
    "repeated mixed-axis south extension"
    (constrainedSeamResultTriangulation repeated)
  assertEqual
    "repeated mixed-axis seam cache agrees with an explicit frontier observation"
    (Internal.prepareSeamFrontierIndex (constrainedSeamResultTriangulation repeated))
    (Internal.triSeamFrontier (constrainedSeamResultTriangulation repeated))

  irregularNorth <-
    buildPublished
      "axis seam irregular north source"
      irregularNorthPoints
  irregularJoined <-
    requireRight
      "irregular collinear north extension"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters
         (constrainedSeamResultTriangulation repeated)
         irregularNorth)
  assertValid
    "irregular collinear north extension"
    (constrainedSeamResultTriangulation irregularJoined)
  assertEqual
    "irregular collinear north cache agrees with an explicit frontier observation"
    (Internal.prepareSeamFrontierIndex (constrainedSeamResultTriangulation irregularJoined))
    (Internal.triSeamFrontier (constrainedSeamResultTriangulation irregularJoined))

  case joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters central overlap of
    Left ConstraintUnionNotSeparated -> pure ()
    outcome ->
      fail
        ( "overlapping sources were not refused by either-axis admission: "
            <> either show (const "success") outcome
        )

-- A separated source frontier may become an interior diagonal when the seam
-- closes. If that diagonal is unconstrained and locally illegal, preserving
-- the source and publishing a Delaunay join are incompatible obligations. The
-- constrained twin is lawful: its contour explicitly authorizes that fixed
-- diagonal to remain.
testSeparatedConstrainedSeamRefusesPinnedFrontierRewrite :: IO ()
testSeparatedConstrainedSeamRefusesPinnedFrontierRewrite = do
  let leftPoints =
        V.fromList
          [ Point (-2) 0
          , Point 0 0
          , Point (-1) 10
          , Point (-3) 5
          ]
      rightPoints =
        V.fromList
          [ Point 1 5
          , Point 2 4
          , Point 3 5
          , Point 2 6
          ]
      closedQuadrilateral = V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)]
      buildSource
        :: String
        -> V.Vector (Int, Int)
        -> V.Vector Point
        -> IO (BuildResult 'Constrained Point () () ())
      buildSource label constraints points =
        requireRight label (constrainedDelaunay unitElementDefaults points constraints)
      publish
        :: BuildResult 'Constrained Point () () ()
        -> Triangulation 'Constrained () () () ()
      publish = Internal.geometryOnlyPublication . buildTriangulation
  unconstrainedLeft <- publish <$> buildSource "incompatible seam left" V.empty leftPoints
  unconstrainedRight <- publish <$> buildSource "incompatible seam right" V.empty rightPoints
  case joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters unconstrainedLeft unconstrainedRight of
    Left
      ( ConstraintUnionConstructionFailed
          (CdtBuildError (SeamSourceEdgeRequiresFlip _))
        ) -> pure ()
    outcome ->
      fail
        ( "separated seam did not refuse an unconstrained pinned source rewrite: "
            <> either show (const "success") outcome
        )
  selectiveJoin <-
    requireRight
      "selective seam rewrites only unselected exterior faces"
      (joinSeparatedConstrained (\_ _ -> False) defaultRefinementParameters unconstrainedLeft unconstrainedRight)
  let selectivelyJoined = constrainedSeamResultTriangulation selectiveJoin
      sourceFaceKeys =
        Set.union
          (Set.fromList (fmap (faceKeyOf unconstrainedLeft) (innerFaces unconstrainedLeft)))
          (Set.fromList (fmap (faceKeyOf unconstrainedRight) (innerFaces unconstrainedRight)))
      selectiveFaceKeys =
        Set.fromList (fmap (faceKeyOf selectivelyJoined) (innerFaces selectivelyJoined))
  assertValid "selective unconstrained seam" selectivelyJoined
  when (sourceFaceKeys `Set.isSubsetOf` selectiveFaceKeys) $
    fail "selective seam did not rewrite the incompatible exterior source face"
  when (V.null (constrainedSeamJoinFaces selectiveJoin)) $
    fail "selective seam did not publish the exact rewritten J component"
  unless (V.null (constrainedSeamRightFaceEvidence selectiveJoin)) $
    fail "selective seam fabricated transport evidence for unselected incoming faces"
  constrainedLeft <-
    publish
      <$> buildSource
        "compatible constrained seam left"
        closedQuadrilateral
        leftPoints
  constrainedRight <-
    publish
      <$> buildSource
        "compatible constrained seam right"
        closedQuadrilateral
        rightPoints
  let protectedLeftFaces = Set.fromList (boundedRegionFaces constrainedLeft)
      protectedRightFaces = Set.fromList (boundedRegionFaces constrainedRight)
      preserveConstrainedSource side faceHandle =
        case side of
          SeamResident -> Set.member faceHandle protectedLeftFaces
          SeamIncoming -> Set.member faceHandle protectedRightFaces
      protectedFaceKeys =
        Set.union
          (Set.fromList (fmap (faceKeyOf constrainedLeft) (Set.toList protectedLeftFaces)))
          (Set.fromList (fmap (faceKeyOf constrainedRight) (Set.toList protectedRightFaces)))
  constrainedJoin <-
    requireRight
      "compatible selected constrained seam"
      (joinSeparatedConstrained preserveConstrainedSource defaultRefinementParameters constrainedLeft constrainedRight)
  let constrainedJoined = constrainedSeamResultTriangulation constrainedJoin
      joinedFaceKeys =
        Set.fromList (fmap (faceKeyOf constrainedJoined) (innerFaces constrainedJoined))
      evidencedIncomingFaces =
        Set.fromList
          ( fmap
              constrainedSeamSourceFace
              (V.toList (constrainedSeamRightFaceEvidence constrainedJoin))
          )
  assertValid
    "compatible selected constrained seam"
    constrainedJoined
  unless (protectedFaceKeys `Set.isSubsetOf` joinedFaceKeys) $
    fail "selected constrained source face did not survive exactly"
  assertEqual
    "selected seam evidences exactly the protected incoming faces"
    protectedRightFaces
    evidencedIncomingFaces

-- An obtuse unconstrained source has a hull edge inside the opposite vertex's
-- diametral circle.  That edge is queued by the refinement seed even though
-- it is not a source constraint.  The checked local interpreter must refuse
-- the encroachment split at its authoritative resolver, retain the cached
-- frontier, and remain joinable afterwards.
canonicalSegmentSquaredLength :: CanonicalSegment -> Double
canonicalSegmentSquaredLength segmentValue =
  squaredDistanceWide
    (segmentStart segmentValue)
    (segmentEnd segmentValue)
