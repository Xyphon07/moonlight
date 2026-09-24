-- | Independent endpoint-reconstruction oracle for the retained-line clipper.
--
-- The oracle is deliberately test-only. Production has one clipping authority:
-- 'exactClipRetainedPolygon'.
module Moonlight.Planar.ExactClipRetentionSpec (tests) where

import Control.Monad (foldM, unless)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList)
import Moonlight.Planar.Convex
  ( convexPolygon, convexPolygonPoints, convexPolygonFromRetained
  , retainConvexPolygon, retainTranslatedConvexPolygon )
import Moonlight.Planar.Exact
  ( ExactAffineLine
  , ExactClipDisposition (..)
  , ExactClipReceipt (..)
  , ExactClosedHalfPlane
  , ExactHalfPlaneError (..)
  , ExactIntersectionError
  , ExactPoint
  , ExactRetainedPolygon
  , ExactVector (..)
  , SegmentRelation (..)
  , classifyExactPoint
  , exactAffineLine
  , exactAffineLineCoefficients
  , exactAffineLineIntersection
  , exactClipRetainedPolygon
  , exactClosedHalfPlane
  , exactClosedHalfPlaneFromDirectedEdge
  , exactClosedHalfPlaneLine
  , exactOrient2d
  , exactPoint
  , exactPointCoordinates
  , exactRetainedPolygon
  , exactRetainedPolygonPoints
  , translateExactPoint
  , emptyExactClipReceipt
  )
import Moonlight.Planar.Internal.BoundaryCycle
  ( cyclePairsNonEmpty
  , cyclicTriples
  )
import Moonlight.Planar.Internal.ExactRational
  ( ExactRational
  , exactRational
  , exactRationalBitWidth
  , exactRationalDenominatorBitWidth
  )
import Support (assertEqual, integerPoint, requireRight)

tests :: IO ()
tests = do
  testTrustedConvexRetention
  testSignedReceiptCombination
  testAffineIntersectionCrossProducts
  testSelfIntersectingAllLeftCycleRefused
  testRetainedClosingLineSurvivesSecondClip
  testAngularHalfPlanePermutation
  testClosedDimensionalDispositions
  initialPolygon <-
    requireRight
      "retained-line fixture polygon"
      ( exactRetainedPolygon
          ( integerPoint (-1000) (-1000)
              :| [ integerPoint 1000 (-1000)
                 , integerPoint 1000 1000
                 , integerPoint (-1000) 1000
                 ]
          )
      )
  halfPlanes <- traverse fixtureHalfPlane fixtureCoefficients
  (retainedDisposition, retainedReceipt) <-
    requireRight
      "retained-line fixture clipping"
      (exactClipRetainedPolygon initialPolygon halfPlanes)
  retainedPoints <-
    case retainedDisposition of
      ExactClipFullDimensional polygon -> pure (exactRetainedPolygonPoints polygon)
      other -> fail ("retained-line fixture lost full dimension: " <> show other)
  (endpointPoints, endpointReceipt) <-
    requireRight
      "endpoint-reconstruction oracle"
      (endpointClipSequence (exactRetainedPolygonPoints initialPolygon) halfPlanes)

  assertExactPointSet "final exact polygon" retainedPoints endpointPoints
  assertEqual
    "final reduced denominator width is representation invariant"
    (exactClipFinalDenominatorBits retainedReceipt)
    (endpointFinalDenominatorBits endpointReceipt)
  assertEqual "measured retained coefficient width" 21 (exactClipMaximumAffineCoefficientBits retainedReceipt)
  assertEqual "measured endpoint coefficient width" 32 (endpointPeakAffineCoefficientBits endpointReceipt)
  unless
    ( endpointPeakAffineCoefficientBits endpointReceipt
        > exactClipMaximumAffineCoefficientBits retainedReceipt
    )
    ( fail
        "endpoint reconstruction did not exhibit strictly larger affine coefficients"
    )

  -- Equality is checked at every local section, not merely after global gluing.
  -- This is the reason no honest coordinate-width counterexample can exist.
  traverse_
    (assertPrefixAgreement initialPolygon)
    (List.inits halfPlanes)
  putStrLn "exact retained-line clipping: ok (source coefficients 21 bits; endpoint oracle 32 bits)"

testTrustedConvexRetention :: IO ()
testTrustedConvexRetention = do
  oneThird <- requireRight "translation x" (exactRational 1 3)
  twoFifths <- requireRight "translation y" (exactRational (-2) 5)
  let shift = ExactVector oneThird twoFifths
      pointAt :: Int -> ExactPoint
      pointAt index = exactPoint (fromIntegral index) (fromIntegral (index * index))
      fixtures =
        [ pointAt 0 :| fmap pointAt [1 .. count - 1]
        | count <- [3, 4, 8, 32]
        ]
      check points = do
        polygon <- requireRight "trusted convex fixture" (convexPolygon points)
        checked <- requireRight "checked retained oracle" (exactRetainedPolygon (convexPolygonPoints polygon))
        checkedShift <- requireRight "checked translated oracle"
          (exactRetainedPolygon (fmap (`translateExactPoint` shift) (convexPolygonPoints polygon)))
        assertEqual "trusted retention preserves complete lines and one-vertex rotation"
          checked (retainConvexPolygon polygon)
        assertEqual "trusted translated retention preserves complete lines and rotation"
          checkedShift (retainTranslatedConvexPolygon shift polygon)
        assertEqual "convex publication retracts retention"
          polygon (convexPolygonFromRetained (retainConvexPolygon polygon))
  traverse_ check fixtures

-- Signed public receipts form a Semigroup, not a Monoid under zero/max.
testSignedReceiptCombination :: IO ()
testSignedReceiptCombination = do
  let left = ExactClipReceipt 1 2 3 4 (-9) (-8) (-7) (-6) (-5)
      middle = ExactClipReceipt 2 3 4 5 (-4) (-3) (-2) (-1) (-9)
      right = ExactClipReceipt 3 4 5 6 (-7) (-6) (-5) (-4) (-3)
  assertEqual "signed clip receipt associativity"
    ((left <> middle) <> right) (left <> (middle <> right))
  assertEqual "empty clip work is explicitly zero"
    (ExactClipReceipt 0 0 0 0 0 0 0 0 0) emptyExactClipReceipt
  unless (left <> emptyExactClipReceipt /= left) $
    fail "negative-width witness no longer refutes the old Monoid identity"

testAffineIntersectionCrossProducts :: IO ()
testAffineIntersectionCrossProducts = do
  oneHalf <- rationalCoefficient 1 2
  oneThird <- rationalCoefficient 1 3
  twoFifths <- rationalCoefficient 2 5
  negativeOneSeventh <- rationalCoefficient (-1) 7
  negativeOne <- rationalCoefficient (-1) 1
  oneEleventh <- rationalCoefficient 1 11
  firstLine <-
    requireRight
      "first rational intersection line"
      (exactAffineLine oneHalf oneThird negativeOne)
  secondLine <-
    requireRight
      "second rational intersection line"
      (exactAffineLine twoFifths negativeOneSeventh oneEleventh)
  intersection <-
    requireRight
      "homogeneous integer affine intersection"
      (exactAffineLineIntersection firstLine secondLine)
  expectedX <- requireRight "expected rational x" (exactRational 260 473)
  expectedY <- requireRight "expected rational y" (exactRational 1029 473)
  assertEqual
    "homogeneous integer intersection agrees with rational Cramer's rule"
    (exactPoint expectedX expectedY)
    intersection
  assertEqual
    "intersection remains on the first source line"
    EQ
    (classifyExactPoint (exactClosedHalfPlane firstLine) intersection)
  assertEqual
    "intersection remains on the second source line"
    EQ
    (classifyExactPoint (exactClosedHalfPlane secondLine) intersection)
 where
  rationalCoefficient :: Integer -> Integer -> IO ExactRational
  rationalCoefficient numerator denominator =
    requireRight
      "rational intersection coefficient"
      (exactRational numerator denominator)

testAngularHalfPlanePermutation :: IO ()
testAngularHalfPlanePermutation = do
  square <- retainedSquare
  halfPlanes <-
    traverse
      fixtureHalfPlane
      [ (1, 0, -2)
      , (2, 0, -4)
      , (-1, 0, 8)
      , (0, 1, -1)
      , (0, -1, 9)
      ]
  (expected, expectedReceipt) <-
    requireRight
      "angular half-plane canonical result"
      (exactClipRetainedPolygon square halfPlanes)
  case expected of
    ExactClipFullDimensional polygon ->
      assertExactPointSet
        "parallel descent retains the strongest boundaries"
        ( integerPoint 2 1
            :| [integerPoint 8 1, integerPoint 8 9, integerPoint 2 9]
        )
        (exactRetainedPolygonPoints polygon)
    other -> fail ("expected a full-dimensional canonical section, got " <> show other)
  traverse_
    (\permutation -> do
       (actual, receipt) <-
         requireRight
           "permuted angular half-plane intersection"
           (exactClipRetainedPolygon square permutation)
       assertEqual "angular half-plane permutation" expected actual
       assertEqual
         "submitted half-plane count"
         (length halfPlanes)
         (exactClipSubmittedHalfPlanes receipt))
    (List.permutations halfPlanes)
  unless (exactClipBoundaryCompatibilityChecks expectedReceipt > 0) $
    fail "angular descent reported no boundary compatibility work"

testClosedDimensionalDispositions :: IO ()
testClosedDimensionalDispositions = do
  square <- retainedSquare
  xAtOne <- traverse fixtureHalfPlane [(1, 0, -1), (-1, 0, 1)]
  (segmentDisposition, _) <-
    requireRight
      "closed segment half-plane intersection"
      (exactClipRetainedPolygon square xAtOne)
  case segmentDisposition of
    ExactClipLowerDimensional points ->
      assertEqual
        "opposing closed half-planes retain their shared segment"
        (List.sort [integerPoint 1 0, integerPoint 1 10])
        (List.sort (NonEmpty.toList points))
    other -> fail ("expected a closed segment, got " <> show other)

  yAtThree <- traverse fixtureHalfPlane [(0, 1, -3), (0, -1, 3)]
  (pointDisposition, _) <-
    requireRight
      "closed point half-plane intersection"
      (exactClipRetainedPolygon square (xAtOne <> yAtThree))
  case pointDisposition of
    ExactClipLowerDimensional points ->
      assertEqual
        "two zero-width closed sections retain their shared point"
        [integerPoint 1 3]
        (List.sort (NonEmpty.toList points))
    other -> fail ("expected a closed point, got " <> show other)

  incompatible <- traverse fixtureHalfPlane [(1, 0, -6), (-1, 0, 5)]
  (emptyDisposition, _) <-
    requireRight
      "incompatible closed half-plane intersection"
      (exactClipRetainedPolygon square incompatible)
  assertEqual "separated opposing boundaries are empty" ExactClipEmpty emptyDisposition

retainedSquare :: IO ExactRetainedPolygon
retainedSquare =
  requireRight
    "retained square"
    ( exactRetainedPolygon
        ( integerPoint 0 0
            :| [integerPoint 10 0, integerPoint 10 10, integerPoint 0 10]
        )
    )

testSelfIntersectingAllLeftCycleRefused :: IO ()
testSelfIntersectingAllLeftCycleRefused = do
  let points =
        integerPoint 0 0
          :| [ integerPoint 5 3
             , integerPoint (-1) 3
             , integerPoint 4 0
             , integerPoint 2 5
             ]
      turns =
        fmap
          (\(previous, current, next) -> exactOrient2d previous current next)
          (cyclicTriples (NonEmpty.toList points))
  assertEqual "self-intersection fixture has only local left turns" (replicate 5 GT) turns
  case exactRetainedPolygon points of
    Left (ExactRetainedPolygonSelfRelation 0 2 SegmentsProperlyCross) -> pure ()
    other ->
      fail
        ( "self-intersecting all-left retained polygon: expected edge 0/2 crossing, got "
            <> show other
        )

-- The first clip emits its first point twice. The closing occurrence carries
-- the source line of the retained bottom edge; preserving the opening record
-- instead corrupts that edge into the square's stale right boundary and makes
-- the second clip spuriously report parallel lines.
testRetainedClosingLineSurvivesSecondClip :: IO ()
testRetainedClosingLineSurvivesSecondClip = do
  square <-
    requireRight
      "retained square"
      ( exactRetainedPolygon
          ( integerPoint 0 0
              :| [ integerPoint 10 0
                 , integerPoint 10 10
                 , integerPoint 0 10
                 ]
          )
      )
  diagonalLine <- requireRight "diagonal clipping line" (exactAffineLine (-1) (-1) 10)
  verticalLine <- requireRight "vertical clipping line" (exactAffineLine (-1) 0 5)
  let diagonalHalfPlane = exactClosedHalfPlane diagonalLine
      verticalHalfPlane = exactClosedHalfPlane verticalLine
      retainedHalfPlanes = diagonalHalfPlane :| [verticalHalfPlane]
  (diagonalDisposition, _) <-
    requireRight
      "retained square diagonal clip"
      (exactClipRetainedPolygon square [diagonalHalfPlane])
  diagonalPolygon <-
    requireFullDimensionalClip
      "retained square diagonal clip"
      diagonalDisposition
  assertExactPointSet
    "first clip retains the expected triangle"
    (integerPoint 0 0 :| [integerPoint 10 0, integerPoint 0 10])
    (exactRetainedPolygonPoints diagonalPolygon)
  (sequentialDisposition, _) <-
    requireRight
      "second clip over the retained first result"
      (exactClipRetainedPolygon diagonalPolygon [verticalHalfPlane])
  sequentialPolygon <-
    requireFullDimensionalClip
      "second clip over the retained first result"
      sequentialDisposition
  (combinedDisposition, combinedReceipt) <-
    requireRight
      "combined retained two-clip descent"
      (exactClipRetainedPolygon square (NonEmpty.toList retainedHalfPlanes))
  combinedPolygon <-
    requireFullDimensionalClip
      "combined retained two-clip descent"
      combinedDisposition
  let expectedFinalPoints =
        integerPoint 0 0
          :| [integerPoint 5 0, integerPoint 5 5, integerPoint 0 10]
  assertExactPointSet
    "second clip intersects the retained bottom and diagonal source lines"
    expectedFinalPoints
    (exactRetainedPolygonPoints sequentialPolygon)
  assertEqual
    "sequential and combined clipping preserve the same retained polygon"
    sequentialPolygon
    combinedPolygon
  assertEqual
    "combined clipping records both submitted half-planes"
    2
    (exactClipSubmittedHalfPlanes combinedReceipt)
  assertRetainedVerticesInside
    retainedHalfPlanes
    (exactRetainedPolygonPoints combinedPolygon)

requireFullDimensionalClip
  :: String
  -> ExactClipDisposition
  -> IO ExactRetainedPolygon
requireFullDimensionalClip _ (ExactClipFullDimensional polygon) = pure polygon
requireFullDimensionalClip label disposition =
  fail (label <> ": expected a full-dimensional polygon, got " <> show disposition)

assertExactPointSet
  :: String
  -> NonEmpty ExactPoint
  -> NonEmpty ExactPoint
  -> IO ()
assertExactPointSet label expected actual =
  assertEqual
    label
    (List.sort (NonEmpty.toList expected))
    (List.sort (NonEmpty.toList actual))

assertRetainedVerticesInside
  :: NonEmpty ExactClosedHalfPlane
  -> NonEmpty ExactPoint
  -> IO ()
assertRetainedVerticesInside halfPlanes points =
  traverse_
    (\halfPlane ->
       traverse_
         (\point ->
            unless (classifyExactPoint halfPlane point /= LT) $
              fail ("retained vertex violates clipping half-plane: " <> show point))
         points)
    halfPlanes

data EndpointOracleError
  = EndpointOracleHalfPlane !ExactHalfPlaneError
  | EndpointOracleIntersection !ExactIntersectionError
  | EndpointOracleLostFullDimension ![ExactPoint]
  deriving stock (Eq, Show)

data EndpointOracleReceipt = EndpointOracleReceipt
  { endpointPeakAffineCoefficientBits :: !Int
  , endpointPeakCoordinateBits :: !Int
  , endpointFinalDenominatorBits :: !Int
  }
  deriving stock (Eq, Show)

data EndpointEdgeClip = EndpointEdgeClip
  { endpointEdgePoints :: ![ExactPoint]
  , endpointEdgeReconstructedCoefficientBits :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

endpointClipSequence
  :: NonEmpty ExactPoint
  -> [ExactClosedHalfPlane]
  -> Either EndpointOracleError (NonEmpty ExactPoint, EndpointOracleReceipt)
endpointClipSequence initialPoints halfPlanes = do
  let initialReceipt =
        EndpointOracleReceipt
          { endpointPeakAffineCoefficientBits = 0
          , endpointPeakCoordinateBits = pointCycleBitWidth initialPoints
          , endpointFinalDenominatorBits = pointCycleDenominatorBitWidth initialPoints
          }
  (finalPoints, accumulatedReceipt) <-
    foldM endpointClipStep (initialPoints, initialReceipt) halfPlanes
  pure
    ( finalPoints
    , accumulatedReceipt
        { endpointFinalDenominatorBits =
            pointCycleDenominatorBitWidth finalPoints
        }
    )

endpointClipStep
  :: (NonEmpty ExactPoint, EndpointOracleReceipt)
  -> ExactClosedHalfPlane
  -> Either EndpointOracleError (NonEmpty ExactPoint, EndpointOracleReceipt)
endpointClipStep (points, receipt) halfPlane = do
  clippedEdges <-
    traverse (clipEndpointEdge halfPlane) (cyclePairsNonEmpty points)
  let clippedPoints = concatMap endpointEdgePoints clippedEdges
      stepCoefficientBits =
        maximumOrZero
          ( foldMap
              (maybeToList . endpointEdgeReconstructedCoefficientBits)
              clippedEdges
          )
  nextPoints <-
    maybe
      (Left (EndpointOracleLostFullDimension clippedPoints))
      Right
      (NonEmpty.nonEmpty clippedPoints)
  if NonEmpty.length nextPoints < 3
    then Left (EndpointOracleLostFullDimension clippedPoints)
    else
      Right
        ( nextPoints
        , receipt
            { endpointPeakAffineCoefficientBits =
                max
                  (endpointPeakAffineCoefficientBits receipt)
                  stepCoefficientBits
            , endpointPeakCoordinateBits =
                max
                  (endpointPeakCoordinateBits receipt)
                  (pointCycleBitWidth nextPoints)
            }
        )

clipEndpointEdge
  :: ExactClosedHalfPlane
  -> (ExactPoint, ExactPoint)
  -> Either EndpointOracleError EndpointEdgeClip
clipEndpointEdge halfPlane (from, to) =
  let fromInside = classifyExactPoint halfPlane from /= LT
      toInside = classifyExactPoint halfPlane to /= LT
      crossing = do
        reconstructedBoundary <-
          either
            (Left . EndpointOracleHalfPlane)
            (Right . exactClosedHalfPlaneLine)
            (exactClosedHalfPlaneFromDirectedEdge from to)
        point <-
          either
            (Left . EndpointOracleIntersection)
            Right
            ( exactAffineLineIntersection
                reconstructedBoundary
                (exactClosedHalfPlaneLine halfPlane)
            )
        pure (point, affineLineBitWidth reconstructedBoundary)
   in case (fromInside, toInside) of
    (True, True) -> Right (EndpointEdgeClip [to] Nothing)
    (True, False) ->
      (\(point, coefficientBits) ->
         EndpointEdgeClip [point] (Just coefficientBits))
        <$> crossing
    (False, True) ->
      (\(point, coefficientBits) ->
         EndpointEdgeClip [point, to] (Just coefficientBits))
        <$> crossing
    (False, False) -> Right (EndpointEdgeClip [] Nothing)

assertPrefixAgreement
  :: ExactRetainedPolygon
  -> [ExactClosedHalfPlane]
  -> IO ()
assertPrefixAgreement initialPolygon prefix = do
  (retainedDisposition, retainedReceipt) <-
    requireRight
      "retained-line prefix"
      (exactClipRetainedPolygon initialPolygon prefix)
  retainedPoints <-
    case retainedDisposition of
      ExactClipFullDimensional polygon -> pure (exactRetainedPolygonPoints polygon)
      other -> fail ("retained-line prefix lost full dimension: " <> show other)
  (endpointPoints, endpointReceipt) <-
    requireRight
      "endpoint prefix"
      (endpointClipSequence (exactRetainedPolygonPoints initialPolygon) prefix)
  assertExactPointSet "prefix exact polygon" retainedPoints endpointPoints
  assertEqual
    "prefix denominator width"
    (exactClipFinalDenominatorBits retainedReceipt)
    (endpointFinalDenominatorBits endpointReceipt)

fixtureHalfPlane
  :: (Integer, Integer, Integer)
  -> IO ExactClosedHalfPlane
fixtureHalfPlane (coefficientX, coefficientY, constant) =
  exactClosedHalfPlane
    <$> requireRight
      "fixture affine half-plane"
      ( exactAffineLine
          (fromInteger coefficientX)
          (fromInteger coefficientY)
          (fromInteger constant)
      )

fixtureCoefficients :: [(Integer, Integer, Integer)]
fixtureCoefficients =
  [ (1, 2, 1300)
  , (-3, 5, 1700)
  , (-7, -2, 1900)
  , (4, -9, 2200)
  , (11, 3, 1800)
  , (-5, 13, 2100)
  , (-17, -4, 2300)
  , (6, -19, 2400)
  , (23, 7, 2500)
  , (-8, 29, 2600)
  ]

affineLineBitWidth :: ExactAffineLine -> Int
affineLineBitWidth line =
  let (coefficientX, coefficientY, constant) = exactAffineLineCoefficients line
   in maximumOrZero (fmap exactRationalBitWidth [coefficientX, coefficientY, constant])

pointCycleBitWidth :: NonEmpty ExactPoint -> Int
pointCycleBitWidth =
  maximumOrZero
    . concatMap
      (\point ->
         let (x, y) = exactPointCoordinates point
          in fmap exactRationalBitWidth [x, y])

pointCycleDenominatorBitWidth :: NonEmpty ExactPoint -> Int
pointCycleDenominatorBitWidth =
  maximumOrZero
    . concatMap
      (\point ->
         let (x, y) = exactPointCoordinates point
          in fmap exactRationalDenominatorBitWidth [x, y])

maximumOrZero :: Foldable collection => collection Int -> Int
maximumOrZero = List.foldl' max 0
