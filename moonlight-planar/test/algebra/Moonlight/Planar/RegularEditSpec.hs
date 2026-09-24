-- | Regular insertion, removal, and reweighting against reconstruction.
module Moonlight.Planar.RegularEditSpec
  ( tests
  ) where

import Data.Foldable ( traverse_ )
import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Planar.Convex ( ConvexPolygon )
import Moonlight.Planar.PowerDiagram ( insertRegularSite, removeRegularSite, reweightRegularSites,
  powerSitePosition, regularEdgeLabels, regularFaceLabels, boundedPowerDiagramFromRegular,
  emptyRegularTriangulation, regularEdges, regularFaces, regularNeighbours, regularSite,
  regularSiteCount, regularSiteDisposition, regularSites, regularTriangulation,
  regularTriangulationReceipt, PowerSite(..), RegularEditError(RegularEditUnknownSites,
  RegularEditSiteConflict), RegularEditResult(regularEditTransitions, regularEditChangedSites,
  regularEditTriangulation), RegularSiteDisposition(RegularSiteVisible,
  RegularSiteCoincidentDominatedBy, RegularSiteCoincidentEquivalentTo, RegularSiteHidden),
  RegularSiteTransition(RegularSiteDisappeared, RegularSiteAppeared, RegularSiteTransitioned),
  RegularTriangulation )
import Moonlight.Planar.PowerFixtures ( admittedSite, admittedWeight, squareDomain )
import Moonlight.Planar.RegularAlpha ( regularAlphaBirths, regularAlphaComplexAtBirth,
  regularAlphaFiltration )
import Moonlight.Planar.Point (Point(..))
import Support ( assertEqual, requireRight )
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Vector as Vector


tests :: IO ()
tests =
  sequence_
    [ testHiddenInsertionAndExposure
    , testRegularInsertionAndRemovalTransitions
    , testRegularReweightTransitions
    , testTopologyPreservingEdits
    , testRegularEditDifferentialLaws
    , testRegularEditObstructionsAndIdempotence
    ]

testHiddenInsertionAndExposure :: IO ()
testHiddenInsertionAndExposure = do
  zero <- admittedWeight 0
  hiddenWeight <- admittedWeight (-2)
  first <- admittedSite "first" (Point 0 0) zero
  second <- admittedSite "second" (Point 2 0) zero
  third <- admittedSite "third" (Point 0 2) zero
  hidden <- admittedSite "hidden" (Point 0.5 0.5) hiddenWeight
  (initial, _) <-
    requireRight
      "regular hidden-insertion base"
      (regularTriangulation (first :| [second, third]))
  inserted <-
    requireRight "insert hidden regular site" (insertRegularSite hidden initial)
  assertEqual
    "hidden insertion is successful resident publication"
    (Just hidden)
    (regularSite "hidden" (regularEditTriangulation inserted))
  assertEqual
    "hidden insertion disposition"
    [RegularSiteAppeared "hidden" RegularSiteHidden]
    (Vector.toList (regularEditTransitions inserted))
  removed <-
    requireRight
      "remove face-defining site"
      (removeRegularSite "first" (regularEditTriangulation inserted))
  assertEqual
    "removal re-exposes a hidden resident"
    (Just RegularSiteVisible)
    (regularSiteDisposition "hidden" (regularEditTriangulation removed))
  assertEqual
    "hidden exposure receipt"
    [ RegularSiteDisappeared "first" RegularSiteVisible
    , RegularSiteTransitioned "hidden" RegularSiteHidden RegularSiteVisible
    ]
    (Vector.toList (regularEditTransitions removed))

testRegularInsertionAndRemovalTransitions :: IO ()
testRegularInsertionAndRemovalTransitions = do
  zero <- admittedWeight 0
  dominantWeight <- admittedWeight 1
  resident <- admittedSite "a-resident" (Point 0 0) zero
  dominant <- admittedSite "z-dominant" (Point 0 0) dominantWeight
  (initial, _) <-
    requireRight "regular insertion base" (regularTriangulation (resident :| []))
  inserted <-
    requireRight "dominant regular insertion" (insertRegularSite dominant initial)
  assertEqual
    "insert changed-site support"
    (Set.singleton "z-dominant")
    (regularEditChangedSites inserted)
  assertEqual
    "dominant insertion transitions"
    [ RegularSiteTransitioned
        "a-resident"
        RegularSiteVisible
        (RegularSiteCoincidentDominatedBy "z-dominant")
    , RegularSiteAppeared "z-dominant" RegularSiteVisible
    ]
    (Vector.toList (regularEditTransitions inserted))
  removed <-
    requireRight
      "dominant regular removal"
      (removeRegularSite "z-dominant" (regularEditTriangulation inserted))
  assertEqual
    "remove changed-site support"
    (Set.singleton "z-dominant")
    (regularEditChangedSites removed)
  assertEqual
    "dominant removal re-exposes resident"
    [ RegularSiteTransitioned
        "a-resident"
        (RegularSiteCoincidentDominatedBy "z-dominant")
        RegularSiteVisible
    , RegularSiteDisappeared "z-dominant" RegularSiteVisible
    ]
    (Vector.toList (regularEditTransitions removed))
  assertEqual
    "insert then remove returns the canonical site value"
    initial
    (regularEditTriangulation removed)

testRegularReweightTransitions :: IO ()
testRegularReweightTransitions = do
  dominantWeight <- admittedWeight 1
  subordinateWeight <- admittedWeight 0
  promotedWeight <- admittedWeight 2
  first <- admittedSite "a" (Point 0 0) dominantWeight
  second <- admittedSite "b" (Point 0 0) subordinateWeight
  (initial, _) <-
    requireRight "regular reweight base" (regularTriangulation (first :| [second]))
  reweighted <-
    requireRight
      "regular batch reweight"
      (reweightRegularSites (Map.singleton "b" promotedWeight) initial)
  assertEqual
    "reweight changed-site support"
    (Set.singleton "b")
    (regularEditChangedSites reweighted)
  assertEqual
    "reweight transitions preserve identities"
    [ RegularSiteTransitioned
        "a"
        RegularSiteVisible
        (RegularSiteCoincidentDominatedBy "b")
    , RegularSiteTransitioned
        "b"
        (RegularSiteCoincidentDominatedBy "a")
        RegularSiteVisible
    ]
    (Vector.toList (regularEditTransitions reweighted))
  let revised = regularEditTriangulation reweighted
  assertEqual
    "reweight preserves the site position"
    (Just (Point 0 0))
    (powerSitePosition <$> regularSite "b" revised)
  assertEqual
    "reweight replaces only the requested weight"
    (Just promotedWeight)
    (powerSiteWeight <$> regularSite "b" revised)

testTopologyPreservingEdits :: IO ()
testTopologyPreservingEdits = do
  domain <- squareDomain
  zero <- admittedWeight 0
  one <- admittedWeight 1
  hiddenWeight <- admittedWeight (-2)
  lowerHiddenWeight <- admittedWeight (-3)

  representative <- admittedSite "a" (Point 0 0) one
  subordinate <- admittedSite "b" (Point 0 0) zero
  (coincidentBase, _) <-
    requireRight "coincident edit base" (regularTriangulation (representative :| []))
  inserted <-
    requireRight
      "canonical coincident insertion"
      (insertRegularSite subordinate coincidentBase)
  assertEqual
    "coincident subordinate insertion transition"
    [RegularSiteAppeared "b" (RegularSiteCoincidentDominatedBy "a")]
    (Vector.toList (regularEditTransitions inserted))
  assertRegularReconstructs domain "canonical coincident insertion" (regularEditTriangulation inserted)

  reweighted <-
    requireRight
      "topology-preserving coincident reweight"
      (reweightRegularSites (Map.singleton "b" one) (regularEditTriangulation inserted))
  assertEqual
    "coincident subordinate reweight transition"
    [ RegularSiteTransitioned
        "b"
        (RegularSiteCoincidentDominatedBy "a")
        (RegularSiteCoincidentEquivalentTo "a")
    ]
    (Vector.toList (regularEditTransitions reweighted))
  assertRegularReconstructs domain "coincident reweight" (regularEditTriangulation reweighted)

  removedSubordinate <-
    requireRight
      "topology-preserving coincident removal"
      (removeRegularSite "b" (regularEditTriangulation reweighted))
  assertEqual
    "coincident subordinate removal transition"
    [RegularSiteDisappeared "b" (RegularSiteCoincidentEquivalentTo "a")]
    (Vector.toList (regularEditTransitions removedSubordinate))
  assertRegularReconstructs domain "coincident removal" (regularEditTriangulation removedSubordinate)

  first <- admittedSite "first" (Point 0 0) zero
  second <- admittedSite "second" (Point 2 0) zero
  third <- admittedSite "third" (Point 0 2) zero
  hidden <- admittedSite "hidden" (Point 0.5 0.5) hiddenWeight
  secondHidden <- admittedSite "hidden-second" (Point 0.75 0.5) hiddenWeight
  (hiddenBase, _) <-
    requireRight
      "hidden edit base"
      (regularTriangulation (first :| [second, third, hidden, secondHidden]))
  lowered <-
    requireRight
      "topology-preserving hidden reweight"
      ( reweightRegularSites
          (Map.fromList [("hidden", lowerHiddenWeight), ("hidden-second", lowerHiddenWeight)])
          hiddenBase
      )
  assertEqual
    "hidden batch changed-site support"
    (Set.fromList ["hidden", "hidden-second"])
    (regularEditChangedSites lowered)
  assertEqual "hidden downward reweight has no visibility transition" [] (Vector.toList (regularEditTransitions lowered))
  assertRegularReconstructs domain "hidden downward reweight" (regularEditTriangulation lowered)

  removedHidden <-
    requireRight
      "topology-preserving hidden removal"
      (removeRegularSite "hidden" (regularEditTriangulation lowered))
  assertEqual
    "hidden removal transition"
    [RegularSiteDisappeared "hidden" RegularSiteHidden]
    (Vector.toList (regularEditTransitions removedHidden))
  assertRegularReconstructs domain "hidden removal" (regularEditTriangulation removedHidden)

assertRegularReconstructs
  :: (Ord label, Show label)
  => ConvexPolygon
  -> String
  -> RegularTriangulation label
  -> IO ()
assertRegularReconstructs domain name edited =
  case NonEmpty.nonEmpty (regularSites edited) of
    Nothing -> assertEqual (name <> " remains empty") 0 (regularSiteCount edited)
    Just sites -> do
      (rebuilt, _) <-
        requireRight (name <> " reconstruction") (regularTriangulation sites)
      let labels = fmap powerSiteLabel (NonEmpty.toList sites)
      assertEqual
        (name <> " receipt")
        (regularTriangulationReceipt rebuilt)
        (regularTriangulationReceipt edited)
      assertEqual
        (name <> " face labels")
        (fmap regularFaceLabels (regularFaces rebuilt))
        (fmap regularFaceLabels (regularFaces edited))
      assertEqual (name <> " faces") (regularFaces rebuilt) (regularFaces edited)
      assertEqual
        (name <> " edge labels")
        (fmap regularEdgeLabels (regularEdges rebuilt))
        (fmap regularEdgeLabels (regularEdges edited))
      assertEqual (name <> " edges") (regularEdges rebuilt) (regularEdges edited)
      traverse_
        (\label -> do
           assertEqual
             (name <> " disposition " <> show label)
             (regularSiteDisposition label rebuilt)
             (regularSiteDisposition label edited)
           assertEqual
             (name <> " neighbours " <> show label)
             (regularNeighbours label rebuilt)
             (regularNeighbours label edited))
        labels
      rebuiltDiagram <-
        requireRight (name <> " rebuilt clipping") (boundedPowerDiagramFromRegular domain rebuilt)
      editedDiagram <-
        requireRight (name <> " edited clipping") (boundedPowerDiagramFromRegular domain edited)
      assertEqual (name <> " bounded cells") rebuiltDiagram editedDiagram

testRegularEditDifferentialLaws :: IO ()
testRegularEditDifferentialLaws = do
  domain <- squareDomain
  sites <- traverse prepareDifferentialSite [0 .. 47]
  nonEmptySites <-
    maybe (fail "differential regular fixture is empty") pure (NonEmpty.nonEmpty sites)
  baseSites <-
    maybe (fail "differential insertion fixture is empty") pure
      (NonEmpty.nonEmpty (NonEmpty.init nonEmptySites))
  (base, _) <- requireRight "differential insertion base" (regularTriangulation baseSites)
  inserted <-
    requireRight
      "differential local insertion"
      (insertRegularSite (NonEmpty.last nonEmptySites) base)
  assertRegularAlphaClosed
    "differential weighted alpha"
    (regularEditTriangulation inserted)
  assertRegularReconstructs
    domain
    "differential insertion"
    (regularEditTriangulation inserted)
  removed <-
    requireRight
      "differential local removal"
      (removeRegularSite "s23" (regularEditTriangulation inserted))
  assertRegularReconstructs
    domain
    "differential removal"
    (regularEditTriangulation removed)
  raised <- admittedWeight 2
  reweighted <-
    requireRight
      "differential local reweight"
      (reweightRegularSites (Map.singleton "s17" raised) (regularEditTriangulation removed))
  assertRegularReconstructs
    domain
    "differential reweight"
    (regularEditTriangulation reweighted)

assertRegularAlphaClosed
  :: (Ord label, Show label)
  => String
  -> RegularTriangulation label
  -> IO ()
assertRegularAlphaClosed name regular = do
  filtration <- requireRight name (regularAlphaFiltration regular)
  traverse_
    (requireRight (name <> " sublevel") . (`regularAlphaComplexAtBirth` filtration))
    (Map.elems (regularAlphaBirths filtration))

prepareDifferentialSite :: Int -> IO (PowerSite String)
prepareDifferentialSite label = do
  let coordinateX = fromIntegral ((label * 37 + 11) `mod` 97) / 10
      coordinateY = fromIntegral ((label * 61 + 7) `mod` 89) / 10
      weightValue = fromIntegral ((label * 17) `mod` 13 - 6) / 64
  weight <- admittedWeight weightValue
  admittedSite ("s" <> show label) (Point coordinateX coordinateY) weight

testRegularEditObstructionsAndIdempotence :: IO ()
testRegularEditObstructionsAndIdempotence = do
  zero <- admittedWeight 0
  one <- admittedWeight 1
  site <- admittedSite "site" (Point 0 0) zero
  conflicting <- admittedSite "site" (Point 0 0) one
  inserted <-
    requireRight
      "insert into empty regular topology"
      (insertRegularSite site emptyRegularTriangulation)
  repeated <-
    requireRight
      "repeat identical regular insertion"
      (insertRegularSite site (regularEditTriangulation inserted))
  assertEqual
    "identical insertion changes no site"
    Set.empty
    (regularEditChangedSites repeated)
  assertEqual "identical insertion has no transitions" [] (Vector.toList (regularEditTransitions repeated))
  case insertRegularSite conflicting (regularEditTriangulation inserted) of
    Left (RegularEditSiteConflict "site" resident requested) -> do
      assertEqual "conflict retains resident" site resident
      assertEqual "conflict retains requested site" conflicting requested
    other -> fail ("expected regular edit site conflict, got " <> show other)
  case reweightRegularSites (Map.singleton "missing" zero) (regularEditTriangulation inserted) of
    Left (RegularEditUnknownSites ("missing" :| [])) -> pure ()
    other -> fail ("expected unknown reweight site, got " <> show other)
  absent <-
    requireRight
      "remove absent regular site"
      (removeRegularSite "missing" (regularEditTriangulation inserted))
  assertEqual
    "absent removal changes no site"
    Set.empty
    (regularEditChangedSites absent)
  assertEqual "absent removal has no transitions" [] (Vector.toList (regularEditTransitions absent))
  removed <-
    requireRight
      "remove final regular site"
      (removeRegularSite "site" (regularEditTriangulation inserted))
  assertEqual "final removal returns empty" 0 (regularSiteCount (regularEditTriangulation removed))
  assertEqual
    "final removal transition"
    [RegularSiteDisappeared "site" RegularSiteVisible]
    (Vector.toList (regularEditTransitions removed))
