module BlockSpec
  ( tests,
  )
where

import Data.Ratio ((%))
import Moonlight.LinAlg
  ( BlockMatrixFailure (..),
    GF2 (..),
    invertGF2Block,
    invertRationalBlock,
    invertUnimodularIntegerBlock,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "block inverse"
    [ testCase "rational 2x2 inverse satisfies both sides" testRationalInverse,
      testCase "GF2 invertible block succeeds" testGF2Inverse,
      testCase "integer unimodular block succeeds" testIntegerUnimodularInverse,
      testCase "integer non-unimodular block is rejected" testIntegerNonUnimodularRejected
    ]

testRationalInverse :: IO ()
testRationalInverse =
  assertEqual
    "rational inverse"
    (Right [[(-2) :: Rational, 1], [3 % 2, (-1) % 2]])
    (invertRationalBlock [[1, 2], [3, 4]])

testGF2Inverse :: IO ()
testGF2Inverse =
  assertEqual
    "GF2 inverse"
    (Right [[GF2One, GF2One], [GF2Zero, GF2One]])
    (invertGF2Block [[GF2One, GF2One], [GF2Zero, GF2One]])

testIntegerUnimodularInverse :: IO ()
testIntegerUnimodularInverse =
  assertEqual
    "integer unimodular inverse"
    (Right [[1 :: Integer, -1], [0, 1]])
    (invertUnimodularIntegerBlock [[1, 1], [0, 1]])

testIntegerNonUnimodularRejected :: IO ()
testIntegerNonUnimodularRejected =
  assertEqual
    "integer non-unimodular rejection"
    (Left (BlockMatrixNonUnimodular [[1 % 2]]))
    (invertUnimodularIntegerBlock [[2 :: Integer]])

