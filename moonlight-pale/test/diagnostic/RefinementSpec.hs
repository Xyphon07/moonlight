module RefinementSpec
  ( tests,
  )
where

import Moonlight.Pale.Diagnostic.Local.Replay
  ( Nanoseconds,
    NonNegativeCount,
    RateNonFiniteValue (..),
    ReplayDiagnosticsValidationError (..),
    diffNonNegativeCount,
    mkNanoseconds,
    mkNonNegativeCount,
    mkRate,
    nanosecondsFromNatural,
    nonNegativeCountFromNatural,
    rateFromCounts,
    rateValue,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "pale.diagnostic.refinement"
    [ testCase "mkNonNegativeCount rejects negative counts and accepts valid counts" $ do
        assertEqual
          "negative count rejection"
          (Left (NegativeCount (-1)))
          (mkNonNegativeCount (-1))
        assertEqual
          "valid count acceptance"
          (Right validCount)
          (mkNonNegativeCount 3),
      testCase "mkNanoseconds rejects negative durations and accepts valid durations" $ do
        assertEqual
          "negative nanoseconds rejection"
          (Left (NegativeNanoseconds (-8)))
          (mkNanoseconds (-8))
        assertEqual
          "valid nanoseconds acceptance"
          (Right validNanoseconds)
          (mkNanoseconds 13),
      testCase "mkRate rejects invalid rates and accepts valid rates" $ do
        assertEqual
          "infinite rate rejection"
          (Left (NonFiniteRate RateInfinite))
          (mkRate infiniteRateInput)
        assertEqual
          "out of bounds rate rejection"
          (Left (RateOutOfBounds 1.25))
          (mkRate 1.25)
        assertEqual
          "valid rate acceptance"
          (Right 0.5)
          (rateValue <$> mkRate 0.5),
      testCase "rateFromCounts rejects invalid ratios and accepts valid ratios" $ do
        assertEqual
          "zero denominator rejection"
          (Left RateDenominatorZero)
          (rateFromCounts validCount zeroCount)
        assertEqual
          "numerator greater than denominator rejection"
          (Left (RateNumeratorExceedsDenominator validCount smallerCount))
          (rateFromCounts validCount smallerCount)
        assertEqual
          "valid ratio acceptance"
          (Right (1 / 3))
          (rateValue <$> rateFromCounts smallerCount validCount),
      testCase "diffNonNegativeCount rejects underflow without wrapping" $
        assertEqual
          "underflowing count difference"
          (Left (CountDifferenceUnderflow smallerCount validCount))
          (diffNonNegativeCount smallerCount validCount)
    ]

zeroCount :: NonNegativeCount
zeroCount =
  nonNegativeCountFromNatural 0

smallerCount :: NonNegativeCount
smallerCount =
  nonNegativeCountFromNatural 1

validCount :: NonNegativeCount
validCount =
  nonNegativeCountFromNatural 3

validNanoseconds :: Nanoseconds
validNanoseconds =
  nanosecondsFromNatural 13

infiniteRateInput :: Double
infiniteRateInput =
  1 / 0
