module ReplaySpec
  ( replayTests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch
import PatchSupport
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

replayTests :: TestTree
replayTests =
  testGroup
    "replay"
    [ testCase "fused replay uses the latest patch representative across aligned support" $
        let firstPatchKey =
              RepresentativeKey 1 "first-patch"
            secondPatchKey =
              RepresentativeKey 1 "second-patch"
            stateKey =
              RepresentativeKey 1 "state"
            patches =
              [ singletonPatch firstPatchKey (Just "old") (Just "middle"),
                singletonPatch secondPatchKey (Just "middle") (Just "new")
              ]
         in fmap (fmap (representativeKeyName . fst) . Map.toAscList) (replay patches (Map.singleton stateKey "old"))
              @?= Right ["second-patch"],
      testCase "aligned replay retains first-before and latest-after endpoints" alignedReplayCase,
      testCase "fused replay boundary mismatch reports the next patch representative" $
        let firstPatchKey =
              RepresentativeKey 1 "first-patch"
            secondPatchKey =
              RepresentativeKey 1 "second-patch"
            stateKey =
              RepresentativeKey 1 "state"
            patches =
              [ singletonPatch firstPatchKey (Just "old") (Just "middle"),
                singletonPatch secondPatchKey (Just "wrong") (Just "new")
              ]
         in case replay patches (Map.singleton stateKey "old") of
              Left replayError -> do
                replayIndex replayError @?= 1
                representativeKeyName (mismatchKey (replayApply replayError)) @?= "second-patch"
                expectedBefore (replayApply replayError) @?= Just "wrong"
                actualBefore (replayApply replayError) @?= Just "middle"
              Right _ ->
                assertFailure "expected replay boundary mismatch",
      testCase "divergent singleton replay finalizes ascending disjoint keys" $
        let initialState =
              Map.fromAscList [(0 :: Int, 0 :: Int), (1, 10), (2, 20), (3, 30)]
            patchForKey :: Int -> Patch Int Int
            patchForKey key =
              singletonPatch key (Just (key * 10)) (Just (key * 10 + 1))
            expectedState =
              Map.fromAscList [(0, 1), (1, 11), (2, 21), (3, 31)]
         in replay (fmap patchForKey [0 .. 3]) initialState
              @?= Right expectedState,
      testCase "divergent singleton replay batches repeated keys" $
        let initialState =
              Map.fromAscList [(0 :: Int, 0 :: Int), (1, 0)]
            patches =
              [ singletonPatch 0 (Just 0) (Just 1),
                singletonPatch 1 (Just 0) (Just 1),
                singletonPatch 0 (Just 1) (Just 2),
                singletonPatch 1 (Just 1) (Just 2)
              ]
         in replay patches initialState
              @?= Right (Map.fromAscList [(0, 2), (1, 2)]),
      testCase "divergent singleton replay keeps latest patch representatives" $
        let firstPatchKey =
              RepresentativeKey 1 "first-patch"
            latestPatchKey =
              RepresentativeKey 1 "latest-patch"
            supportPatchKey =
              RepresentativeKey 2 "support-patch"
            stateKey =
              RepresentativeKey 1 "state"
            supportStateKey =
              RepresentativeKey 2 "support-state"
            patches =
              [ singletonPatch firstPatchKey (Just "old") (Just "middle"),
                singletonPatch supportPatchKey (Just "zero") (Just "one"),
                singletonPatch latestPatchKey (Just "middle") (Just "new")
              ]
            initialState =
              Map.fromAscList
                [ (stateKey, "old"),
                  (supportStateKey, "zero")
                ]
         in fmap (fmap (representativeKeyName . fst) . Map.toAscList) (replay patches initialState)
              @?= Right ["latest-patch", "support-patch"],
      testCase "divergent singleton replay reports the stale patch index" $
        let stalePatchKey =
              RepresentativeKey 1 "stale-patch"
            stateKey =
              RepresentativeKey 1 "state"
            patches =
              [ singletonPatch (RepresentativeKey 1 "first-patch") (Just "old") (Just "middle"),
                singletonPatch (RepresentativeKey 2 "support-patch") (Just "zero") (Just "one"),
                singletonPatch stalePatchKey (Just "wrong") (Just "new")
              ]
            initialState =
              Map.fromAscList
                [ (stateKey, "old"),
                  (RepresentativeKey 2 "support-state", "zero")
                ]
         in case replay patches initialState of
              Left replayError -> do
                replayIndex replayError @?= 2
                representativeKeyName (mismatchKey (replayApply replayError)) @?= "stale-patch"
                expectedBefore (replayApply replayError) @?= Just "wrong"
                actualBefore (replayApply replayError) @?= Just "middle"
              Right _ ->
                assertFailure "expected stale replay failure",
      testCase "divergent singleton replay checks assert-absent before insert" $
        let patches =
              [ singletonPatch (0 :: Int) Nothing Nothing,
                singletonPatch 1 Nothing Nothing,
                singletonPatch 0 Nothing (Just "inserted")
              ]
         in replay patches Map.empty
              @?= Right (Map.singleton 0 "inserted"),
      testCase "divergent singleton replay falls back at the current patch" $
        let initialState =
              Map.fromAscList [(0 :: Int, 0 :: Int), (1, 0)]
            patches =
              [ singletonPatch 0 (Just 0) (Just 1),
                singletonPatch 1 (Just 0) (Just 1),
                fromAscList
                  [ (0, replace 1 2),
                    (1, replace 1 2)
                  ]
              ]
         in replay patches initialState
              @?= Right (Map.fromAscList [(0, 2), (1, 2)])
    ]

alignedReplayCase :: IO ()
alignedReplayCase = do
  let supportKeys :: [Int]
      supportKeys = [0 .. 127]
      stepCount :: Int
      stepCount = 96
      initialState =
        Map.fromAscList
          [ (key, 0)
            | key <- supportKeys
          ]
      patchAt :: Int -> Patch Int Int
      patchAt step =
        fromAscList
          [ (key, replace step (step + 1))
            | key <- supportKeys
          ]
      patches = fmap patchAt [0 .. stepCount - 1]
      expectedState =
        Map.fromAscList
          [ (key, stepCount)
            | key <- supportKeys
          ]
  replay patches initialState @?= Right expectedState
