module TypeLevelSpec (tests) where

import GHC.TypeNats (natVal)
import Numeric.Natural (Natural)
import Moonlight.Core (SNat(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

snatToNatural :: SNat n -> Natural
snatToNatural s@SNat = natVal s

tests :: TestTree
tests = testGroup "TypeLevel"
  [ testCase "SNat @0 witnesses 0" $
      snatToNatural (SNat @0) @?= 0
  , testCase "SNat @42 witnesses 42" $
      snatToNatural (SNat @42) @?= 42
  , testCase "SNat @1000000 witnesses 1000000" $
      snatToNatural (SNat @1000000) @?= 1000000
  ]
