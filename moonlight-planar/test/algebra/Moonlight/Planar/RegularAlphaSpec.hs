-- | Signed weighted-alpha births, closed sublevels, and common weight shifts.
module Moonlight.Planar.RegularAlphaSpec
  ( tests
  ) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Planar.PowerDiagram ( reweightRegularSites, regularTriangulation,
  RegularEditResult(regularEditTransitions, regularEditTriangulation) )
import Moonlight.Planar.PowerFixtures ( admittedSite, admittedWeight )
import Moonlight.Planar.RegularAlpha ( powerAlphaBirthExact, regularAlphaBirths,
  regularAlphaComplex, regularAlphaComplexAtBirth, regularAlphaFiltration )
import Moonlight.Planar.Simplex ( planarComplexCells, planarEdge, planarFace, planarVertex )
import Moonlight.Planar.Point (Point(..))
import Support ( assertEqual, requireRight, requireJust )
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Vector as Vector


tests :: IO ()
tests =
  sequence_
    [ testRegularAlphaBirths
    , testRegularAlphaCommonShift
    ]

testRegularAlphaBirths :: IO ()
testRegularAlphaBirths = do
  positive <- admittedWeight 3
  singleton <- admittedSite "only" (Point 0 0) positive
  (singleRegular, _) <-
    requireRight "singleton regular topology" (regularTriangulation (singleton :| []))
  singleAlpha <- requireRight "singleton weighted alpha" (regularAlphaFiltration singleRegular)
  assertEqual
    "positive weight gives signed vertex birth"
    (Just (-3))
    (fmap powerAlphaBirthExact (Map.lookup (planarVertex "only") (regularAlphaBirths singleAlpha)))

  zero <- admittedWeight 0
  firstSite <- admittedSite "first" (Point 0 0) zero
  secondSite <- admittedSite "second" (Point 2 0) zero
  thirdSite <- admittedSite "third" (Point 0 2) zero
  dominated <- admittedSite "dominated" (Point 0 0) =<< admittedWeight (-1)
  (regular, _) <-
    requireRight
      "triangle regular topology for weighted alpha"
      (regularTriangulation (firstSite :| [secondSite, thirdSite, dominated]))
  filtration <- requireRight "triangle weighted alpha" (regularAlphaFiltration regular)
  firstSecond <- requireRight "first-second simplex" (planarEdge "first" "second")
  firstThird <- requireRight "first-third simplex" (planarEdge "first" "third")
  secondThird <- requireRight "second-third simplex" (planarEdge "second" "third")
  face <- requireRight "triangle simplex" (planarFace "first" "second" "third")
  let birthAt simplex = fmap powerAlphaBirthExact (Map.lookup simplex (regularAlphaBirths filtration))
  assertEqual "first leg birth" (Just 1) (birthAt firstSecond)
  assertEqual "second leg birth" (Just 1) (birthAt firstThird)
  assertEqual "hypotenuse birth" (Just 2) (birthAt secondThird)
  assertEqual "face birth" (Just 2) (birthAt face)
  assertEqual
    "coincident subordinate has no alpha simplex"
    False
    (Set.member (planarVertex "dominated") (planarComplexCells (regularAlphaComplex filtration)))
  threshold <- requireJust "edge threshold" (Map.lookup firstSecond (regularAlphaBirths filtration))
  _ <- requireRight "weighted alpha sublevel is closed" (regularAlphaComplexAtBirth threshold filtration)
  pure ()

testRegularAlphaCommonShift :: IO ()
testRegularAlphaCommonShift = do
  zero <- admittedWeight 0
  shifted <- admittedWeight 3
  let labelledPoints =
        ("south-west", Point 0 0)
          :| [ ("south-east", Point 4 0)
             , ("north-east", Point 3 3)
             , ("north-west", Point 0 4)
             ]
  baseSites <- traverse (\(label, point) -> admittedSite label point zero) labelledPoints
  shiftedSites <- traverse (\(label, point) -> admittedSite label point shifted) labelledPoints
  (baseRegular, _) <- requireRight "base regular alpha topology" (regularTriangulation baseSites)
  (shiftedRegular, _) <- requireRight "shifted regular alpha topology" (regularTriangulation shiftedSites)
  shiftedEdit <-
    requireRight
      "common regular weight shift"
      ( reweightRegularSites
          (Map.fromList [(label, shifted) | (label, _) <- NonEmpty.toList labelledPoints])
          baseRegular
      )
  assertEqual
    "common shift reuses the regular topology"
    shiftedRegular
    (regularEditTriangulation shiftedEdit)
  assertEqual "common shift changes no disposition" [] (Vector.toList (regularEditTransitions shiftedEdit))
  baseAlpha <- requireRight "base regular alpha" (regularAlphaFiltration baseRegular)
  shiftedAlpha <- requireRight "shifted regular alpha" (regularAlphaFiltration shiftedRegular)
  assertEqual
    "common weight shift preserves the complex"
    (regularAlphaComplex baseAlpha)
    (regularAlphaComplex shiftedAlpha)
  assertEqual
    "common weight shift subtracts from every birth"
    (Map.map ((\birth -> birth - 3) . powerAlphaBirthExact) (regularAlphaBirths baseAlpha))
    (Map.map powerAlphaBirthExact (regularAlphaBirths shiftedAlpha))
