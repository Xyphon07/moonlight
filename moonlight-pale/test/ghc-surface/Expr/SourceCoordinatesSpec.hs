module Expr.SourceCoordinatesSpec (tests) where

import Moonlight.Pale.Ghc.Expr
  ( SourceEndConvention (..),
    SourceRangeFailure (..),
    SourceRegion (..),
    sourceCharRangeEnd,
    sourceCharRangeRegion,
    sourceCharRangeStart,
    sourceCharRangeText,
    sourceRegionCharRangeWith,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "profile-rewrite.coordinates"
    [ testCase "tab-aware half-open coordinates retain Unicode scalar offsets" $ do
        resolved <- requireRight (sourceRegionCharRangeWith 8 SourceEndHalfOpen canonicalSource (SourceRegion 1 1 1 13))
        assertEqual "start" 0 (sourceCharRangeStart resolved)
        assertEqual "end" 6 (sourceCharRangeEnd resolved)
        assertEqual "slice" (Right "α\tbeta") (sourceCharRangeText canonicalSource resolved),
      testCase "tab-aware inclusive coordinates consume the final source character" $ do
        resolved <- requireRight (sourceRegionCharRangeWith 8 SourceEndInclusive canonicalSource (SourceRegion 1 9 1 12))
        assertEqual "start" 2 (sourceCharRangeStart resolved)
        assertEqual "end" 6 (sourceCharRangeEnd resolved)
        assertEqual "slice" (Right "beta") (sourceCharRangeText canonicalSource resolved),
      testCase "coordinates inside a tab expansion are noninvertible" $
        assertEqual
          "inside tab"
          (Left (SourceRangePositionInsideTab 1 3))
          (sourceRegionCharRangeWith 8 SourceEndHalfOpen canonicalSource (SourceRegion 1 3 1 9)),
      testCase "cross-line half-open coordinates preserve the newline" $ do
        let expectedRegion = SourceRegion 1 9 2 2
        resolved <- requireRight (sourceRegionCharRangeWith 8 SourceEndHalfOpen canonicalSource expectedRegion)
        assertEqual "slice" (Right "beta\nz") (sourceCharRangeText canonicalSource resolved)
        assertEqual "inverse region" (Right expectedRegion) (sourceCharRangeRegion canonicalSource resolved),
      testCase "tab-aware range inversion returns visual columns" $ do
        let expectedRegion = SourceRegion 1 9 1 13
        resolved <- requireRight (sourceRegionCharRangeWith 8 SourceEndHalfOpen canonicalSource expectedRegion)
        assertEqual "visual-column inverse" (Right expectedRegion) (sourceCharRangeRegion canonicalSource resolved),
      testCase "CRLF source is refused" $
        assertEqual
          "CRLF"
          (Left SourceRangeCarriageReturnUnsupported)
          (sourceRegionCharRangeWith 8 SourceEndHalfOpen "x\r\ny\n" (SourceRegion 1 1 1 2)),
      testCase "bare carriage returns are refused" $
        assertEqual
          "bare CR"
          (Left SourceRangeCarriageReturnUnsupported)
          (sourceRegionCharRangeWith 8 SourceEndHalfOpen "x\ry" (SourceRegion 1 1 1 2)),
      testCase "invalid tab stops are refused before conversion" $
        assertEqual
          "tab stop"
          (Left (SourceRangeInvalidTabStop 0))
          (sourceRegionCharRangeWith 0 SourceEndHalfOpen canonicalSource (SourceRegion 1 1 1 2)),
      testCase "inclusive coordinates cannot name the boundary after a line" $
        assertEqual
          "inclusive boundary"
          (Left (SourceRangePositionOutsideSource 1 13))
          (sourceRegionCharRangeWith 8 SourceEndInclusive canonicalSource (SourceRegion 1 9 1 13))
    ]

canonicalSource :: String
canonicalSource = "α\tbeta\nz\n"

requireRight :: Show failure => Either failure value -> IO value
requireRight = either (assertFailure . show) pure
