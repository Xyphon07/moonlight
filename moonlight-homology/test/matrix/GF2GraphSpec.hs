module GF2GraphSpec
  ( tests,
  )
where

import Data.Set qualified as Set
import Moonlight.Homology
  ( GraphBoundaryGF2Failure (..),
    graphBoundaryRankDefectGF2,
    prepareGraphBoundaryGF2,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "GF2 graph boundary"
    [ testCase "rejects missing endpoints" testRejectsMissingEndpoint,
      testCase "tree rank defect is zero" testTreeRankDefect,
      testCase "simple cycle rank defect is one" testSimpleCycleRankDefect,
      testCase "self-loop rank defect is one" testSelfLoopRankDefect,
      testCase "parallel edges rank defect is one" testParallelEdgeRankDefect
    ]

testRejectsMissingEndpoint :: Assertion
testRejectsMissingEndpoint =
  case prepareGraphBoundaryGF2 (Set.singleton (1 :: Int)) [(1, 2)] of
    Left (GraphBoundaryGF2EndpointMissing edgeIndex missingCell edgeValue) ->
      assertEqual "missing endpoint" (0, 2, (1, 2)) (edgeIndex, missingCell, edgeValue)
    Left failureValue ->
      assertFailure ("unexpected graph boundary failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected graph boundary construction to reject missing endpoint"

testTreeRankDefect :: Assertion
testTreeRankDefect =
  assertGraphDefect "tree defect" 0 (Set.fromList [1 :: Int, 2]) [(1, 2)]

testSimpleCycleRankDefect :: Assertion
testSimpleCycleRankDefect =
  assertGraphDefect "triangle defect" 1 (Set.fromList [1 :: Int, 2, 3]) [(1, 2), (2, 3), (3, 1)]

testSelfLoopRankDefect :: Assertion
testSelfLoopRankDefect =
  assertGraphDefect "self-loop defect" 1 (Set.singleton (1 :: Int)) [(1, 1)]

testParallelEdgeRankDefect :: Assertion
testParallelEdgeRankDefect =
  assertGraphDefect "parallel-edge defect" 1 (Set.fromList [1 :: Int, 2]) [(1, 2), (1, 2)]

assertGraphDefect :: String -> Int -> Set.Set Int -> [(Int, Int)] -> Assertion
assertGraphDefect label expectedDefect vertices edges =
  case prepareGraphBoundaryGF2 vertices edges of
    Left failureValue ->
      assertFailure ("graph boundary construction failed: " <> show failureValue)
    Right boundaryValue ->
      assertEqual label expectedDefect (graphBoundaryRankDefectGF2 boundaryValue)
