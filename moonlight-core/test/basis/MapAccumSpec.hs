module MapAccumSpec (tests) where

import Data.Map.Strict qualified as Map
import Moonlight.Core (accumByKey, buildTripleIndex, indexMap)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "MapAccum"
    [ testCase "accumByKey preserves input order for noncommutative semigroups" $
        accumByKey fst (pure . snd) [('a', "first"), ('a', "second"), ('b', "only")]
          @?= Map.fromList [('a', ["first", "second"]), ('b', ["only"])],
      testCase "indexMap records the last index for duplicate keys" $
        indexMap ["alpha", "beta", "alpha", "gamma", "beta"]
          @?= Map.fromList [("alpha", 2), ("beta", 4), ("gamma", 3)],
      testCase "buildTripleIndex preserves per-key input order" $
        let values =
              [ ('a', "left", 1 :: Int),
                ('a', "right", 2),
                ('b', "left", 1),
                ('a', "left", 3)
              ]
         in buildTripleIndex
              (\(firstKey, _secondKey, _thirdKey) -> firstKey)
              (\(_firstKey, secondKey, _thirdKey) -> secondKey)
              (\(_firstKey, _secondKey, thirdKey) -> thirdKey)
              values
              @?= ( Map.fromList [('a', [('a', "left", 1), ('a', "right", 2), ('a', "left", 3)]), ('b', [('b', "left", 1)])],
                    Map.fromList [("left", [('a', "left", 1), ('b', "left", 1), ('a', "left", 3)]), ("right", [('a', "right", 2)])],
                    Map.fromList [(1, [('a', "left", 1), ('b', "left", 1)]), (2, [('a', "right", 2)]), (3, [('a', "left", 3)])]
                  )
    ]
