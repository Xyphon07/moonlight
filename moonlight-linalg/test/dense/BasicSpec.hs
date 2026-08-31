
module BasicSpec
  ( tests,
  )
where

import Moonlight.LinAlg
  ( Matrix,
    add,
    fromListMatrix,
    gf2One,
    gf2Zero,
    mapMatrix,
    mult,
    toListMatrix,
    transpose,
  )
import Moonlight.Core (MoonlightError)
import Helpers (extractRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Basic"
    [ testCase "add on Double matrices" testAdd,
      testCase "multiply on Double matrices" testMultiply,
      testCase "transpose on Double matrices" testTranspose,
      testCase "mapMatrix changes scalar type through a direct function" testMapMatrix,
      testCase "multiply on GF2 matrices uses algebraic semantics" testGF2Multiply
    ]

testAdd :: Assertion
testAdd =
  let result = do
        left <- fromListMatrix @2 @2 @Double [1.0, 2.0, 3.0, 4.0]
        right <- fromListMatrix @2 @2 @Double [4.0, 3.0, 2.0, 1.0]
        add left right
   in extractRight result (\value -> assertEqual "matrix sum" [5.0, 5.0, 5.0, 5.0] (toListMatrix value))

testMultiply :: Assertion
testMultiply =
  let result = do
        left <- fromListMatrix @2 @2 @Double [1.0, 2.0, 3.0, 4.0]
        right <- fromListMatrix @2 @2 @Double [2.0, 0.0, 1.0, 2.0]
        mult left right
   in extractRight result (\value -> assertEqual "matrix product" [4.0, 4.0, 10.0, 8.0] (toListMatrix value))

testTranspose :: Assertion
testTranspose =
  let result = do
        matrixValue <- fromListMatrix @2 @3 @Double [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
        transpose matrixValue
   in extractRight result (\value -> assertEqual "matrix transpose" [1.0, 4.0, 2.0, 5.0, 3.0, 6.0] (toListMatrix value))

testMapMatrix :: Assertion
testMapMatrix =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [1.2, 2.8, 3.4, 4.9]
        mapMatrix round matrixValue :: Either MoonlightError (Matrix 2 2 Integer)
   in extractRight result (\value -> assertEqual "mapped matrix" [1, 3, 3, 5] (toListMatrix value))

testGF2Multiply :: Assertion
testGF2Multiply =
  let result = do
        left <- fromListMatrix @2 @2 [gf2One, gf2One, gf2Zero, gf2One]
        right <- fromListMatrix @2 @2 [gf2One, gf2Zero, gf2One, gf2One]
        mult left right
   in extractRight result (\value -> assertEqual "GF2 product" [gf2Zero, gf2One, gf2One, gf2One] (toListMatrix value))
