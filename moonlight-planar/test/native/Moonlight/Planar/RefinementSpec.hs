{-# LANGUAGE BangPatterns #-}

-- | Refinement fixpoints, local-domain obligations, and mesh-quality laws.
module Moonlight.Planar.RefinementSpec
  ( tests
  ) where

import Control.Monad ( unless, when )
import Data.Foldable ( traverse_ )
import Data.List ( sort )
import Moonlight.Planar.Cdt ( constrainedDelaunay, outerRegionFaces, constraintSegments,
  joinSeparatedConstrained, ConstrainedSeamResult(constrainedSeamResultTriangulation) )
import Moonlight.Planar.Dcel ( faceDirectedEdges, faceVertices, incidentFace, isConstraintEdge,
  numConstraints, numFaces, numInnerFaces, numVertices, outerFace, undirectedEndpoints, vertexPoint
  )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( undirectedEdges, innerFaces )
import Moonlight.Planar.Internal.Canonical ( canonicalize )
import Moonlight.Planar.Internal.HandleDefs ( FaceId(FaceId), asUndirected, directedPair )
import Moonlight.Planar.Internal.Paged ( PublicationStats(..) )
import Moonlight.Planar.Internal.Types ( RefinementParameters(..) )
import Moonlight.Planar.Math ( squaredDistanceWide )
import Moonlight.Planar.Refinement ( refine, refineWithinDomain )
import Moonlight.Planar.RefinementAssertions ( assertClosureCountsAgree )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Types (defaultRefinementParameters, unitElementDefaults, BuildError(RefinementDomainWouldCrossInterface, RefinementDomainRequiresFiniteVertexBudget,
  RefinementDomainRequiresConvexHullPreservation, RefinementDomainRequiresConstraintPreservation,
  RefinementDomainForbidsOuterFaceExclusion, RefinementOversizedEdge), ClosureStats(closureVertices,
  closureFaces, closureDirectedEdges), BuildResult(buildTriangulation), RefinementDomainResult(refinementDomainReceipt, refinementDomainResult), RefinementReceipt(refinementVisitedProtectedFaces, refinementFinalInterfaceIncidence,
  refinementClosureStats, refinementPublicationStats, refinementCreatedFaces,
  refinementInterfaceBoundaryReads, refinementAttemptedBoundaryCrossings,
  refinementFinalPermittedFaces), RefinementResult(refinementAddedVertices, refinementExcludedFaces,
  refinementComplete, refinedTriangulation), Triangulation)
import Support ( assertEqual, assertValid, requireRight )
import Moonlight.Planar.Internal.Predicates qualified as Admitted
import qualified Moonlight.Planar.Dcel as Dcel
import Moonlight.Planar.Internal.Representation qualified as Internal
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testRefinementCompletionIsAFixpoint
    , testLocalRefinementRejectsEncroachedHullSplit
    , testConstrainedRefinement
    , testCheckedLocalRefinement
    , testMaximumEdgeLengthQuality
    , testLocalRefinementRejectsTrueInterfaceCrossing
    , testRepeatedBoundaryAdjacentRefinement
    ]

-- | What @refinementComplete@ claims is that the quality worklist drained. The
-- assertable content of that claim is a fixpoint: a second pass under the same
-- parameters can admit nothing. The complementary run is budget-starved, where
-- the run must stop short and spend exactly what it was given.
testRefinementCompletionIsAFixpoint :: IO ()
testRefinementCompletionIsAFixpoint = do
  -- Barrier parity needs constraints to bound a domain: on an unconstrained
  -- mesh every face sits at depth zero, so excluding outer faces excludes all
  -- of them and the worklist drains having refined nothing.
  bounded <-
    requireRight "refinement fixpoint domain" $
      constrainedDelaunay
        unitElementDefaults
        (V.fromList [Point 0 0, Point 8 0, Point 8 8, Point 0 8])
        (V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)])
  let source = buildTriangulation bounded
      parameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 500
          , refineMaxArea = Just 3
          , refineExcludeOuterFaces = True
          }
  drained <- requireRight "drained refinement" (refine id parameters source)
  unless (refinementComplete drained) $
    fail "the refinement budget was too small to drain the worklist"
  -- Without this the fixpoint below is vacuous: a run that refined nothing
  -- trivially admits nothing on a second pass.
  unless (refinementAddedVertices drained > 0) $
    fail "the drained run inserted no Steiner points, so the fixpoint proves nothing"
  assertValid "drained refinement" (refinedTriangulation drained)
  again <- requireRight "second refinement pass" (refine id parameters (refinedTriangulation drained))
  assertEqual
    "a drained worklist admits nothing on a second pass"
    0
    (refinementAddedVertices again)
  starved <-
    requireRight
      "starved refinement"
      (refine id parameters {refineMaxAdditionalVertices = Just 3} source)
  when (refinementComplete starved) $
    fail "a three-vertex budget reported a drained worklist"
  assertEqual "a starved run spends exactly its budget" 3 (refinementAddedVertices starved)

testLocalRefinementRejectsEncroachedHullSplit :: IO ()
testLocalRefinementRejectsEncroachedHullSplit = do
  built <-
    requireRight
      "encroached hull source"
      ( constrainedDelaunay
          unitElementDefaults
          (V.fromList [Point 0 0, Point 2 0, Point 0.1 0.1])
          V.empty
      )
  let source =
        Internal.geometryOnlyPublication
          (buildTriangulation built)
      permitted = Set.fromList (innerFaces source)
      parameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 1
          , refineMaxArea = Just 0.001
          , refineKeepConstraintEdges = True
          }
      hullEdges =
        [ edge
        | edge <- undirectedEdges source
        , let (forward, backward) = directedPair edge
        , incidentFace source forward == outerFace || incidentFace source backward == outerFace
        ]
      encroachedHull edge =
        let (fromVertex, toVertex) = Dcel.undirectedEndpoints source edge
            fromPoint = vertexPoint source fromVertex
            toPoint = vertexPoint source toVertex
            oppositeFaceEdge =
              case directedPair edge of
                (forward, backward)
                  | incidentFace source forward == outerFace -> backward
                  | otherwise -> forward
            oppositePoint =
              let innerFace = incidentFace source oppositeFaceEdge
               in if innerFace == outerFace
                    then Nothing
                    else
                      case Dcel.faceVertices source innerFace of
                        verticesInFace ->
                          case filter (/= fromVertex) (filter (/= toVertex) verticesInFace) of
                            [vertex] -> Just (vertexPoint source vertex)
                            _ -> Nothing
         in maybe False (Admitted.inDiametralCircle fromPoint toPoint) oppositePoint
  unless (any encroachedHull hullEdges) $
    fail "encroached hull fixture did not produce an encroached hull pair"
  refined <-
    requireRight
      "encroached hull local refinement"
      (refineWithinDomain (const ()) parameters permitted source)
  assertClosureCountsAgree "encroached hull local refinement" permitted refined
  let refinementResult = refinementDomainResult refined
      result = refinedTriangulation refinementResult
  unless (refinementComplete refinementResult) $
    fail "encroached hull local refinement exhausted without a fixpoint"
  assertEqual "encroached hull split count" (numVertices source) (numVertices result)
  assertEqual
    "encroached hull refinement retains cached frontier"
    (Internal.triSeamFrontier source)
    (Internal.triSeamFrontier result)

  secondBuild <-
    requireRight
      "encroached hull join source"
      ( constrainedDelaunay
          unitElementDefaults
          (V.fromList [Point 4 0, Point 6 0, Point 6 1, Point 4 1])
          V.empty
      )
  let second =
        Internal.geometryOnlyPublication
          (buildTriangulation secondBuild)
  joined <-
    requireRight
      "encroached hull repeated join"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters result second)
  assertValid
    "encroached hull repeated join"
    (constrainedSeamResultTriangulation joined)

testConstrainedRefinement :: IO ()
testConstrainedRefinement = do
  cdtBuild <- requireRight "bounded domain" $ constrainedDelaunay
    unitElementDefaults
    (V.fromList [Point 0 0, Point 8 0, Point 8 8, Point 0 8, Point 4 2, Point 4 6])
    (V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)])
  let cdt = buildTriangulation cdtBuild
      parameters = defaultRefinementParameters
        { refineMaxAdditionalVertices = Just 80
        , refineMaxArea = Just 3
        , refineMaxRadiusEdgeRatio = Just 1.4
        , refineExcludeOuterFaces = True
        , refineKeepConstraintEdges = False
        }
  refined <- requireRight "constrained refinement" (refine id parameters cdt)
  let result = refinedTriangulation refined
  canonicalResult <- requireRight "canonical constrained refinement" (canonicalize result)
  unless (refinementAddedVertices refined > 0) $ fail "constrained refinement inserted no Steiner points"
  assertValid "constrained refinement" result
  unless (numConstraints result >= numConstraints cdt) $
    fail "constraint splitting lost the constrained boundary"
  assertEqual
    "canonical publication preserves constraint segments"
    (constraintSegments result)
    (constraintSegments canonicalResult)

  -- Refinement maintains the outer-region classification incrementally, from
  -- the touched patch alone. That is a claim about what an insertion cannot
  -- reach, so it is gated against an independent flood over the finished mesh
  -- rather than trusted. The domain here is deliberately narrower than its
  -- convex hull, so the excluded set is non-empty and the two can disagree.
  notchBuild <- requireRight "notched domain" $ constrainedDelaunay
    unitElementDefaults
    (V.fromList [Point 0 0, Point 8 0, Point 8 8, Point 0 8, Point 13 4, Point 4 4])
    (V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)])
  let notch = buildTriangulation notchBuild
      notchMaximumEdgeLength = 6.2
      notchParameters = defaultRefinementParameters
        { refineMaxAdditionalVertices = Just 120
        , refineMaxArea = Just 1.5
        , refineMaxEdgeLength = Just notchMaximumEdgeLength
        , refineExcludeOuterFaces = True
        , refineKeepConstraintEdges = False
        }
  notchRefined <- requireRight "notched refinement" (refine id notchParameters notch)
  let notchResult = refinedTriangulation notchRefined
      maintained = sort (V.toList (refinementExcludedFaces notchRefined))
      independent = sort (outerRegionFaces notchResult)
      excludedEdgeLengthsSquared =
        [ squaredDistanceWide
            (vertexPoint notchResult fromVertex)
            (vertexPoint notchResult toVertex)
        | face <- maintained
        , directed <- faceDirectedEdges notchResult face
        , let (fromVertex, toVertex) =
                undirectedEndpoints notchResult (asUndirected directed)
        ]
  unless (refinementAddedVertices notchRefined > 0) $ fail "notched refinement inserted no Steiner points"
  unless (refinementComplete notchRefined) $ fail "notched maximum-edge refinement did not complete"
  when (null independent) $ fail "notched domain produced no outer region: the gate is vacuous"
  unless
    (any (> notchMaximumEdgeLength * notchMaximumEdgeLength) excludedEdgeLengthsSquared)
    (fail "notched fixture has no excluded maximum-edge violation")
  assertEqual "incremental exclusion agrees with an independent flood" independent maintained
  assertValid "notched refinement" notchResult

  annulusBuild <- requireRight "annular domain" $ constrainedDelaunay
    unitElementDefaults
    ( V.fromList
        [ Point 0 0
        , Point 12 0
        , Point 12 12
        , Point 0 12
        , Point 4 4
        , Point 8 4
        , Point 8 8
        , Point 4 8
        ]
    )
    ( V.fromList
        [ (0, 1)
        , (1, 2)
        , (2, 3)
        , (3, 0)
        , (4, 5)
        , (5, 6)
        , (6, 7)
        , (7, 4)
        ]
    )
  let annulus = buildTriangulation annulusBuild
      annulusParameters :: Int -> Maybe Double -> RefinementParameters
      annulusParameters budget maximumArea =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just budget
          , refineMaxArea = maximumArea
          , refineExcludeOuterFaces = True
          , refineKeepConstraintEdges = False
          }
  annulusUnchanged <-
    requireRight
      "budget-zero annular refinement"
      (refine id (annulusParameters 0 Nothing) annulus)
  let initialAnnulusOutside = sort (outerRegionFaces annulus)
      budgetZeroOutside =
        sort (V.toList (refinementExcludedFaces annulusUnchanged))
  assertEqual "annulus has one two-crossing hole" 2 (length initialAnnulusOutside)
  assertEqual
    "budget-zero refinement uses the authoritative annulus classification"
    initialAnnulusOutside
    budgetZeroOutside

  annulusRefined <-
    requireRight
      "positive-budget annular refinement"
      (refine id (annulusParameters 40 (Just 4)) annulus)
  let refinedAnnulus = refinedTriangulation annulusRefined
      maintainedAnnulus =
        sort (V.toList (refinementExcludedFaces annulusRefined))
      independentAnnulus = sort (outerRegionFaces refinedAnnulus)
  unless (refinementAddedVertices annulusRefined > 0) $
    fail "annular refinement inserted no Steiner points"
  assertEqual
    "incremental annulus exclusion agrees with authoritative barrier depth"
    independentAnnulus
    maintainedAnnulus
  assertValid "annular constrained refinement" refinedAnnulus

  -- A tiny constraint can share a mesh with coordinates of vastly different
  -- magnitude. Encroachment is discovered by a local cavity walk, so there is
  -- no broad phase for the range to overflow; the pin is that the scaled
  -- circumcenter and diametral predicates still produce exactly one vertex.
  wideGridBuild <- requireRight "wide-grid constrained domain" $ constrainedDelaunay
    unitElementDefaults
    ( V.fromList
        [ Point 0 0
        , Point 2.0e-43 0
        , Point 1.0e60 0
        , Point 0 1.0e60
        ] :: V.Vector (Point)
    )
    (V.singleton (0, 1))
  wideGridRefined <-
    requireRight
      "wide-grid constrained refinement"
      ( refine
          id
          defaultRefinementParameters
            { refineMaxAdditionalVertices = Just 1
            , refineMaxArea = Just 1.0e119
            , refineKeepConstraintEdges = False
            }
          (buildTriangulation wideGridBuild)
      )
  assertEqual "wide-grid refinement count" 1 (refinementAddedVertices wideGridRefined)
  assertValid "wide-grid constrained refinement" (refinedTriangulation wideGridRefined)

-- | A local domain is a closed section, not a hopeful initial queue. Its
-- interface is exact, its protected faces survive point-for-point, and the
-- receipt contains no visit to the protected side.
testCheckedLocalRefinement :: IO ()
testCheckedLocalRefinement = do
  built <-
    requireRight
      "checked local refinement source"
      ( constrainedDelaunay
          unitElementDefaults
          ( V.fromList
              [ Point 0 0
              , Point 4.1 0
              , Point 8 0.2
              , Point 0.1 4
              , Point 4 4.2
              , Point 8.1 4
              ]
          )
          (V.singleton (2, 5))
      )
  let source = buildTriangulation built
      permitted =
        Set.fromList
          [ face
          | face <- innerFaces source
          , faceCentroidX source face < 4.05
          ]
      interface =
        Set.fromList
          [ edge
          | edge <- undirectedEdges source
          , let (forward, backward) = directedPair edge
                forwardFace = incidentFace source forward
                backwardFace = incidentFace source backward
          , forwardFace /= outerFace
          , backwardFace /= outerFace
          , Set.member forwardFace permitted /= Set.member backwardFace permitted
          ]
      protected = filter (`Set.notMember` permitted) (innerFaces source)
      protectedSignatures = fmap (\face -> (face, sort (fmap (vertexPoint source) (faceVertices source face)))) protected
      calmParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 3
          , refineMaxRadiusEdgeRatio = Nothing
          , refineKeepConstraintEdges = True
          }
      crossingParameters = calmParameters{refineMaxArea = Just 1}
  unless (not (Set.null permitted) && not (null protected) && not (Set.null interface)) $
    fail "checked local refinement fixture did not form a nontrivial cover"
  case refineWithinDomain id calmParameters{refineMaxAdditionalVertices = Nothing} permitted source of
    Left RefinementDomainRequiresFiniteVertexBudget -> pure ()
    outcome -> fail ("checked local refinement accepted an unbounded local budget: " <> either show (const "success") outcome)
  refined <-
    requireRight
      "checked local refinement"
      (refineWithinDomain id calmParameters permitted source)
  assertClosureCountsAgree "checked local refinement" permitted refined
  let localResult = refinementDomainResult refined
      target = refinedTriangulation localResult
      targetProtectedSignatures = fmap (\face -> (face, sort (fmap (vertexPoint target) (faceVertices target face)))) protected
      receipt = refinementDomainReceipt refined
      finalPermitted = Set.fromList (V.toList (refinementFinalPermittedFaces receipt))
      expectedFinalInterfaceIncidence =
        V.fromList
          [ if Set.member sourceForwardFace permitted
              then (edge, sourceBackwardFace, targetForwardFace)
              else (edge, sourceForwardFace, targetBackwardFace)
          | edge <- Set.toAscList interface
          , let (forward, backward) = directedPair edge
                sourceForwardFace = incidentFace source forward
                sourceBackwardFace = incidentFace source backward
                targetForwardFace = incidentFace target forward
                targetBackwardFace = incidentFace target backward
          ]
      actualFinalInterfaceIncidence = refinementFinalInterfaceIncidence receipt
  assertEqual "checked local protected face restriction" protectedSignatures targetProtectedSignatures
  assertEqual
    "checked local final permitted lineage"
    permitted
    finalPermitted
  assertEqual
    "checked local final interface incidence"
    expectedFinalInterfaceIncidence
    actualFinalInterfaceIncidence
  assertEqual
    "checked local final interface edge set"
    interface
    (Set.fromList [edge | (edge, _, _) <- V.toList actualFinalInterfaceIncidence])
  traverse_
    (\(edge, protectedFace, finalPermittedFace) ->
       unless
         ( Set.notMember protectedFace permitted
             && Set.member finalPermittedFace finalPermitted
         )
         ( fail
             ( "checked local final interface incidence misclassified "
                 <> show (edge, protectedFace, finalPermittedFace)
             )
         )
    )
    actualFinalInterfaceIncidence
  assertEqual "checked local protected visit receipt" V.empty (refinementVisitedProtectedFaces receipt)
  assertEqual "checked local boundary crossing receipt" 0 (refinementAttemptedBoundaryCrossings receipt)
  let closureStats = refinementClosureStats receipt
  unless
    ( closureFaces closureStats > 0
        && closureDirectedEdges closureStats > 0
        && closureVertices closureStats > 0
    ) $
    fail "checked local refinement did not record its selected closure"
  assertEqual
    "checked local refinement does not enumerate resident unboxed base pages"
    0
    (publicationUnboxedBasePageEnumerations (refinementPublicationStats receipt))
  assertEqual
    "checked local protected constraint restriction"
    (constraintSegments source)
    (constraintSegments target)
  assertValid "checked local refinement" target
  case refineWithinDomain id crossingParameters permitted source of
    Left (RefinementDomainWouldCrossInterface _ _) -> pure ()
    Left obstruction -> fail ("checked local refinement returned the wrong crossing obstruction: " <> show obstruction)
    Right _ -> fail "checked local refinement silently crossed its immutable interface"
  case refineWithinDomain id calmParameters{refinePreserveConvexHull = False} permitted source of
    Left RefinementDomainRequiresConvexHullPreservation -> pure ()
    outcome -> fail ("checked local refinement accepted hull mutation: " <> either show (const "success") outcome)
  case refineWithinDomain id calmParameters{refineKeepConstraintEdges = False} permitted source of
    Left RefinementDomainRequiresConstraintPreservation -> pure ()
    outcome -> fail ("checked local refinement accepted constraint mutation: " <> either show (const "success") outcome)
  case refineWithinDomain id calmParameters{refineExcludeOuterFaces = True} permitted source of
    Left RefinementDomainForbidsOuterFaceExclusion -> pure ()
    outcome -> fail ("checked local refinement accepted outer-face exclusion: " <> either show (const "success") outcome)
  wholeRefined <-
    requireRight
      "checked whole-section refinement"
      ( refineWithinDomain
          id
          crossingParameters
          (Set.fromList (innerFaces source))
          source
      )
  let wholeResult = refinementDomainResult wholeRefined
      wholeReceipt = refinementDomainReceipt wholeRefined
  unless (refinementAddedVertices wholeResult > 0) $
    fail "checked whole-section refinement did not improve its admitted section"
  unless (not (V.null (refinementCreatedFaces wholeReceipt))) $
    fail "checked whole-section refinement omitted semantically rewritten face slots"
  unless
    ( V.any
        (\(FaceId raw) -> toInteger raw < toInteger (numFaces source))
        (refinementCreatedFaces wholeReceipt)
    ) $
    fail "checked whole-section refinement omitted recycled face slots"
  assertEqual
    "checked whole-section final permitted lineage"
    (Set.fromList (innerFaces (refinedTriangulation wholeResult)))
    (Set.fromList (V.toList (refinementFinalPermittedFaces wholeReceipt)))
  assertValid "checked whole-section refinement" (refinedTriangulation wholeResult)
 where
  faceCentroidX
    :: Triangulation mode vertex directed undirected face
    -> FaceId
    -> Double
  faceCentroidX triangulation face =
    case fmap (vertexPoint triangulation) (faceVertices triangulation face) of
      [] -> 0
      points ->
        sum [x | Point x _ <- points] / fromIntegral (length points)

-- | A maximum edge bound is a local geometric law, not a rendering preference:
-- a long, thin face is split along its longest edge even when its area is below
-- the minimum-area short-circuit. A budget-limited pass remains resumable;
-- once the longest edge is frozen at the local interface, a completed pass
-- returns the exact typed quality obstruction instead of crossing it.
testMaximumEdgeLengthQuality :: IO ()
testMaximumEdgeLengthQuality = do
  built <-
    requireRight
      "maximum-edge long-thin source"
      ( constrainedDelaunay
          unitElementDefaults
          ( V.fromList
              [ Point 0 0
              , Point 10 0
              , Point 10 1
              , Point 0 1
              ]
          )
          (V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)])
      )
  let source = buildTriangulation built
      allPermitted = Set.fromList (innerFaces source)
      parameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 1
          , refineMinArea = Just 6
          , refineMaxEdgeLength = Just 10.01
          , refineMaxRadiusEdgeRatio = Nothing
          , refineKeepConstraintEdges = True
          }
  partial <-
    requireRight
      "maximum-edge budget-limited refinement"
      (refineWithinDomain id parameters allPermitted source)
  let partialResult = refinementDomainResult partial
      partialTarget = refinedTriangulation partialResult
      partialReceipt = refinementDomainReceipt partial
      resumedPermitted = Set.fromList (V.toList (refinementFinalPermittedFaces partialReceipt))
  unless (refinementAddedVertices partialResult > 0) $
    fail "maximum-edge refinement did not split the long edge before the area floor"
  when (refinementComplete partialResult) $
    fail "maximum-edge budget-limited refinement reported completion"
  assertValid "maximum-edge budget-limited refinement" partialTarget
  resumed <-
    requireRight
      "maximum-edge resumed refinement"
      ( refineWithinDomain
          id
          parameters{refineMaxAdditionalVertices = Just 8}
          resumedPermitted
          partialTarget
      )
  let resumedResult = refinementDomainResult resumed
      resumedReceipt = refinementDomainReceipt resumed
  unless (refinementComplete resumedResult) $
    fail "maximum-edge resumed refinement did not drain its local worklist"
  assertMaximumEdgeLength
    "maximum-edge resumed refinement"
    10.01
    (refinedTriangulation resumedResult)
    (V.toList (refinementFinalPermittedFaces resumedReceipt))

  case
      refine
        id
        parameters
          { refineMaxAdditionalVertices = Just 8
          , refineMaxEdgeLength = Just 9
          }
        source of
    Left (RefinementOversizedEdge _ edge actual bound)
      | isConstraintEdge source edge && actual > bound -> pure ()
    Left obstruction ->
      fail ("maximum-edge global audit returned the wrong obstruction: " <> show obstruction)
    Right _ -> fail "maximum-edge global refinement accepted an oversized kept constraint"

  case innerFaces source of
    firstFace : _ -> do
      let permitted = Set.singleton firstFace
          interface =
            Set.fromList
              [ edge
              | edge <- undirectedEdges source
              , let (forward, backward) = directedPair edge
                    forwardFace = incidentFace source forward
                    backwardFace = incidentFace source backward
              , forwardFace /= outerFace
              , backwardFace /= outerFace
              , Set.member forwardFace permitted /= Set.member backwardFace permitted
              ]
      let blocked =
            refineWithinDomain
              id
              parameters{refineMaxAdditionalVertices = Just 8}
              permitted
              source
      case blocked of
        Left (RefinementOversizedEdge face edge actual bound)
          | Set.member face permitted
              && Set.member edge interface
              && actual > bound -> pure ()
        Left obstruction ->
          fail ("maximum-edge interface returned the wrong obstruction: " <> show obstruction)
        Right _ -> fail "maximum-edge interface was silently crossed or accepted"
    [] -> fail "maximum-edge source has no inner face"

-- | A circumcenter can be inside the admitted face while its legalization
-- cavity also meets the immutable interface. That is a true crossing demand,
-- not the boundary-limited locator read discharged by the local interpreter.
testLocalRefinementRejectsTrueInterfaceCrossing :: IO ()
testLocalRefinementRejectsTrueInterfaceCrossing = do
  built <-
    requireRight
      "deterministic true interface crossing source"
      ( constrainedDelaunay
          unitElementDefaults
          ( V.fromList
              [ Point 0 0
              , Point 2 0
              , Point 1 (sqrt 3)
              , Point (-1) (sqrt 3)
              ]
          )
          V.empty
      )
  let source = buildTriangulation built
      equilateralFacePoints = sort [Point 0 0, Point 2 0, Point 1 (sqrt 3)]
      permittedFace =
        List.find
          ( \face ->
              sort (fmap (vertexPoint source) (faceVertices source face))
                == equilateralFacePoints
          )
          (innerFaces source)
  assertEqual "deterministic crossing source has two inner faces" 2 (numInnerFaces source)
  case permittedFace of
    Nothing -> fail "deterministic crossing source omitted its equilateral face"
    Just face -> do
      let permitted = Set.singleton face
          interface =
            Set.fromList
              [ edge
              | edge <- undirectedEdges source
              , let (forward, backward) = directedPair edge
                    forwardFace = incidentFace source forward
                    backwardFace = incidentFace source backward
              , forwardFace /= outerFace
              , backwardFace /= outerFace
              , Set.member forwardFace permitted /= Set.member backwardFace permitted
              ]
          parameters =
            defaultRefinementParameters
              { refineMaxAdditionalVertices = Just 1
              , refineMaxArea = Just 0.5
              , refineMaxRadiusEdgeRatio = Nothing
              , refineKeepConstraintEdges = True
              }
      assertEqual
        "deterministic crossing source has one interface edge"
        1
        (Set.size interface)
      case refineWithinDomain id parameters permitted source of
        Left (RefinementDomainWouldCrossInterface _ _) -> pure ()
        Left obstruction ->
          fail
            ( "deterministic true interface crossing returned the wrong obstruction: "
                <> show obstruction
            )
        Right _ ->
          fail "deterministic true interface crossing was silently accepted"

testRepeatedBoundaryAdjacentRefinement :: IO ()
testRepeatedBoundaryAdjacentRefinement = do
  built <-
    requireRight
      "repeated boundary-adjacent constrained source"
      ( constrainedDelaunay
          unitElementDefaults
          ( V.fromList
              [ Point 0 0
              , Point 4 0
              , Point 8 0
              , Point 12 0
              , Point 0 4
              , Point 4 4
              , Point 8 4
              , Point 12 4
              ]
          )
          (V.singleton (2, 6))
      )
  let source = buildTriangulation built
      sourcePermitted = leftSection source
      fullParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 20
          , refineMaxArea = Just 1
          , refineMaxRadiusEdgeRatio = Nothing
          , refineKeepConstraintEdges = True
          }
  fullRefinement <-
    requireRight
      "full boundary-adjacent refinement"
      (refineWithinDomain id fullParameters sourcePermitted source)
  let fullResult = refinementDomainResult fullRefinement
      fullTarget = refinedTriangulation fullResult
      fullReceipt = refinementDomainReceipt fullRefinement
  unless (refinementAddedVertices fullResult > 0) $
    fail "full boundary-adjacent refinement did not refine its admitted section"
  unless (refinementInterfaceBoundaryReads fullReceipt > 0) $
    fail "boundary-adjacent refinement did not report its immutable interface read"
  assertEqual
    "boundary-adjacent crossing attempts"
    0
    (refinementAttemptedBoundaryCrossings fullReceipt)
  assertEqual
    "full boundary-adjacent constraint restriction"
    (constraintSegments source)
    (constraintSegments fullTarget)
  assertEqual
    "completed boundary-adjacent refinement is idempotent"
    True
    (refinementComplete fullResult)

  let progressParameters = fullParameters{refineMaxAdditionalVertices = Just 1}
  progressFirstRefinement <-
    requireRight
      "first bounded boundary-adjacent refinement"
      (refineWithinDomain id progressParameters sourcePermitted source)
  let progressFirstResult = refinementDomainResult progressFirstRefinement
      progressFirstTarget = refinedTriangulation progressFirstResult
      progressFirstReceipt = refinementDomainReceipt progressFirstRefinement
      progressPermitted =
        Set.fromList (V.toList (refinementFinalPermittedFaces progressFirstReceipt))
  unless (refinementAddedVertices progressFirstResult > 0) $
    fail "first bounded boundary-adjacent refinement did not refine its admitted section"
  when (refinementComplete progressFirstResult) $
    fail "first bounded boundary-adjacent refinement unexpectedly reached a fixpoint"
  progressSecondRefinement <-
    requireRight
      "repeated boundary-adjacent refinement"
      ( refineWithinDomain
          id
          progressParameters
          progressPermitted
          progressFirstTarget
      )
  let progressSecondResult = refinementDomainResult progressSecondRefinement
      progressSecondTarget = refinedTriangulation progressSecondResult
      progressSecondReceipt = refinementDomainReceipt progressSecondRefinement
  unless (refinementAddedVertices progressSecondResult > 0) $
    fail "repeated boundary-adjacent refinement did not retain its dynamic join-face support"
  assertEqual
    "repeated boundary-adjacent protected visits"
    V.empty
    (refinementVisitedProtectedFaces progressSecondReceipt)
  assertEqual
    "repeated boundary-adjacent constraint restriction"
    (constraintSegments progressFirstTarget)
    (constraintSegments progressSecondTarget)
  assertValid "repeated boundary-adjacent refinement" progressSecondTarget
 where
  leftSection
    :: Triangulation mode vertex directed undirected face
    -> Set.Set FaceId
  leftSection triangulation =
    Set.fromList
      [ face
      | face <- innerFaces triangulation
      , faceCentroidX triangulation face < 8
      ]
  faceCentroidX
    :: Triangulation mode vertex directed undirected face
    -> FaceId
    -> Double
  faceCentroidX triangulation face =
    case fmap (vertexPoint triangulation) (faceVertices triangulation face) of
      [] -> 0
      points ->
        sum [x | Point x _ <- points] / fromIntegral (length points)

assertMaximumEdgeLength
  :: String
  -> Double
  -> Triangulation mode vertex directed undirected face
  -> [FaceId]
  -> IO ()
assertMaximumEdgeLength label maximumLength triangulation faces =
  unless (null violations) $
    fail (label <> " oversized edges: " <> show (take 1 violations))
 where
  !maximumSquaredLength = maximumLength * maximumLength
  violations =
    [ (face, edge, squaredLength)
    | face <- faces
    , directed <- faceDirectedEdges triangulation face
    , let edge = asUndirected directed
          (fromVertex, toVertex) = undirectedEndpoints triangulation edge
          squaredLength =
            squaredDistanceWide
              (vertexPoint triangulation fromVertex)
              (vertexPoint triangulation toVertex)
    , squaredLength > maximumSquaredLength
    ]
