module DedupSpec
  ( tests,
  )
where

import Moonlight.Core.Dedup (firstDuplicate, firstValueWithDuplicate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "stable duplicate detection"
    [ testCase "accepts a list with no repeated value" $
        assertEqual "no repeated value" Nothing (firstDuplicate [9 :: Int, 3, 7]),
      testCase "reports an immediate repetition" $
        assertEqual "immediate repetition" (Just (9 :: Int)) (firstDuplicate [9, 9, 3]),
      testCase "reports the first repetition in encounter order rather than the least repeated value" $
        assertEqual "first encountered repetition" (Just (9 :: Int)) (firstDuplicate [9, 3, 9, 3]),
      testCase "distinguishes the earliest repeated occurrence from the earliest value that repeats" $ do
        assertEqual "second occurrence order" (Just (3 :: Int)) (firstDuplicate [9, 3, 3, 9])
        assertEqual "first occurrence order" (Just (9 :: Int)) (firstValueWithDuplicate [9, 3, 3, 9])
    ]
