
module DynamicSpec
  ( tests,
  )
where

import Moonlight.LinAlg
  ( DynMatrix,
    dynMatrixFromRows,
    dynMatrixShape,
    dynMatrixToList,
    dynMatrixToRows,
    fromDynMatrix,
    fromListMatrix,
    mkDynMatrix,
    toDynMatrix,
    toListMatrix,
    withDynMatrix,
  )
import Helpers (extractRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Dynamic"
    [ testCase "toDynMatrix preserves shape and payload" testToDyn,
      testCase "fromDynMatrix reifies static dimensions" testFromDyn,
      testCase "fromDynMatrix rejects equal-cardinality shape changes" testFromDynShapeMismatch,
      testCase "dynamic nested rows preserve row-major shape" testDynamicRows,
      testCase "dynamic zero-column rows retain row count" testDynamicZeroColumns,
      testCase "withDynMatrix introduces existential static dimensions" testWithDyn
    ]

testToDyn :: Assertion
testToDyn =
  let result = do
        matrixValue <- fromListMatrix @2 @3 @Double [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
        pure (toDynMatrix matrixValue)
   in extractRight result (\dynValue -> do
        assertEqual "dynamic shape" (2, 3) (dynMatrixShape dynValue)
        assertEqual "dynamic payload" [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] (dynMatrixToList dynValue)
      )

testFromDyn :: Assertion
testFromDyn =
  let result = do
        dynValue <- mkDynMatrix 2 2 ([1, 2, 3, 4] :: [Integer])
        fromDynMatrix @2 @2 dynValue
   in extractRight result (\matrixValue -> assertEqual "static payload" [1, 2, 3, 4] (toListMatrix matrixValue))

testFromDynShapeMismatch :: Assertion
testFromDynShapeMismatch =
  case
    do
      dynValue <- mkDynMatrix 1 4 ([1, 2, 3, 4] :: [Integer])
      fromDynMatrix @2 @2 dynValue
    of
    Left err ->
      assertEqual
        "shape failure"
        "InvariantViolation \"dynamic matrix shape does not match static dimensions: expected (2,2) but received (1,4)\""
        (show err)
    Right _ ->
      assertFailure "fromDynMatrix must not reinterpret a 1x4 matrix as 2x2"

testDynamicRows :: Assertion
testDynamicRows =
  let result = do
        matrixValue <- dynMatrixFromRows [[1 :: Integer, 2], [3, 4]]
        rows <- dynMatrixToRows matrixValue
        pure (dynMatrixShape matrixValue, dynMatrixToList matrixValue, rows)
   in extractRight result $ \(shapeValue, payload, rows) -> do
        assertEqual "shape" (2, 2) shapeValue
        assertEqual "payload" [1, 2, 3, 4] payload
        assertEqual "rows" [[1, 2], [3, 4]] rows

testDynamicZeroColumns :: Assertion
testDynamicZeroColumns =
  let result = do
        matrixValue <- dynMatrixFromRows [[], [], [] :: [Integer]]
        rows <- dynMatrixToRows matrixValue
        pure (dynMatrixShape matrixValue, rows)
   in extractRight result $ \(shapeValue, rows) -> do
        assertEqual "shape" (3, 0) shapeValue
        assertEqual "rows" [[], [], []] rows

testWithDyn :: Assertion
testWithDyn =
  let result = do
        dynValue :: DynMatrix Double <- mkDynMatrix 1 3 [7.0, 8.0, 9.0]
        withDynMatrix dynValue (\matrixValue -> toListMatrix matrixValue)
   in extractRight result (\values -> assertEqual "existential reification" [7.0, 8.0, 9.0] values)
