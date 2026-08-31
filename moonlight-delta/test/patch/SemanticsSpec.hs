module SemanticsSpec
  ( semanticTests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch
import Moonlight.Delta.Patch qualified as Patch
import PatchSupport
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

semanticTests :: TestTree
semanticTests =
  testGroup
    "semantics"
    [ testCase "composition rejects incompatible cell boundary" $
        compose
          (singletonPatch (1 :: Int) (Just "x") (Just "b"))
          (singletonPatch (1 :: Int) Nothing (Just "a"))
          @?=
            Left
              ComposeBoundaryMismatch
                { boundaryKey = 1,
                  olderAfter = Just "a",
                  newerBefore = Just "x"
                },
      testCase "normalization preserves assertion-only cells" $
        let assertion =
              singletonPatch (1 :: Int) (Just "a") (Just "a")
         in do
              Patch.null (normalize assertion) @?= False
              apply
                (normalize assertion)
                (Map.singleton 1 "b")
                @?=
                  Left
                    ApplyBeforeMismatch
                      { mismatchKey = 1,
                        expectedBefore = Just "a",
                        actualBefore = Just "b"
                      },
      testCase "composition preserves round-trip source assertion" $
        let older =
              singletonPatch (1 :: Int) (Just "a") (Just "b")
            newer =
              singletonPatch 1 (Just "b") (Just "a")
            assertion =
              singletonPatch 1 (Just "a") (Just "a")
         in do
              compose newer older @?= Right assertion
              apply assertion (Map.singleton 1 "c")
                @?=
                  Left
                    ApplyBeforeMismatch
                      { mismatchKey = 1,
                        expectedBefore = Just "a",
                        actualBefore = Just "c"
                      },
      testCase "applied cell recording keeps endpoints only" $
        let applied =
              recordApplied
                1
                (cellFromEndpoints (Just "a") (Just "b"))
                empty
                >>= recordApplied
                  (1 :: Int)
                  (cellFromEndpoints (Just "b") (Just "c"))
         in applied @?= Right (singletonPatch 1 (Just "a") (Just "c")),
      testCase "applied round trip remains a checked assertion" $
        let applied =
              recordApplied
                1
                (cellFromEndpoints (Just "a") (Just "b"))
                empty
                >>= recordApplied
                  (1 :: Int)
                  (cellFromEndpoints (Just "b") (Just "a"))
            assertion =
              singletonPatch 1 (Just "a") (Just "a")
         in do
              applied @?= Right assertion
              case applied of
                Left err ->
                  assertFailure (show err)
                Right patch ->
                  apply patch (Map.singleton 1 "stale")
                    @?=
                      Left
                        ApplyBeforeMismatch
                          { mismatchKey = 1,
                            expectedBefore = Just "a",
                            actualBefore = Just "stale"
                          },
      testCase "applied cell recording rejects discontinuous producer boundary" $
        let applied =
              recordApplied
                1
                (cellFromEndpoints (Just "a") (Just "b"))
                empty
                >>= recordApplied
                  (1 :: Int)
                  (cellFromEndpoints (Just "c") (Just "d"))
         in applied
              @?=
                Left
                  ComposeBoundaryMismatch
                    { boundaryKey = 1,
                      olderAfter = Just "b",
                      newerBefore = Just "c"
                    },
      testCase "recordMany interprets repeated keys temporally" $
        recordMany
          [ (1 :: Int, replace "a" "b"),
            (1, replace "b" "c")
          ]
          @?= Right (singleton 1 (replace "a" "c")),
      testCase "fromAscList falls back to canonical construction for nonascending input" $
        fromAscList
          [ (2 :: Int, insert "b"),
            (1, insert "a"),
            (2, replace "b" "c")
          ]
          @?= fromList
            [ (2, insert "b"),
              (1, insert "a"),
              (2, replace "b" "c")
            ],
      testCase "absence assertion is a restricted identity" $ do
        let assertion = singleton (1 :: Int) (assertAbsent :: CellPatch String)
        Patch.null assertion @?= False
        apply assertion Map.empty @?= Right (Map.empty :: Map.Map Int String)
        apply assertion (Map.singleton 1 "present")
          @?=
            Left
              ApplyBeforeMismatch
                { mismatchKey = 1,
                  expectedBefore = Nothing,
                  actualBefore = Just "present"
                },
      testCase "apply mismatch reports the patch key representative" $
        let patchKey =
              RepresentativeKey 1 "patch"
            stateKey =
              RepresentativeKey 1 "state"
            patch =
              singletonPatch patchKey (Just "expected") (Just "new")
         in case apply patch (Map.singleton stateKey "actual") of
              Left mismatch -> do
                representativeKeyName (mismatchKey mismatch) @?= "patch"
                expectedBefore mismatch @?= Just "expected"
                actualBefore mismatch @?= Just "actual"
              Right _ ->
                assertFailure "expected representative-key mismatch",
      testCase "apply update uses the patch key representative" $
        let patchKey =
              RepresentativeKey 1 "patch"
            stateKey =
              RepresentativeKey 1 "state"
            patch =
              singletonPatch patchKey (Just "old") (Just "new")
         in fmap (fmap (representativeKeyName . fst) . Map.toAscList) (apply patch (Map.singleton stateKey "old"))
              @?= Right ["patch"],
      testCase "dense apply uses patch representatives for every matched key" $
        let rowKeys :: [Int]
            rowKeys = [0 .. 127]
            state =
              Map.fromAscList
                [ (RepresentativeKey key "state", key)
                  | key <- rowKeys
                ]
            patch =
              fromAscList
                [ (RepresentativeKey key "patch", replace key (key + 1))
                  | key <- rowKeys
                ]
            labels :: Map.Map RepresentativeKey Int -> [String]
            labels =
              fmap (representativeKeyName . fst) . Map.toAscList
         in fmap labels (apply patch state)
              @?= Right (replicate 128 "patch"),
      testCase "composition is independent of page layout" differentPageLayoutCase,
      testCase "mask-first validation preserves least-key error ordering" maskFirstLeastMismatchCase,
      testCase "disjoint composition preserves logical rows across an underfull seam" disjointSeamCase,
      testCase "endpoint-native fold agrees with logical row materialization" endpointNativeFoldCase
    ]

maskFirstLeastMismatchCase :: IO ()
maskFirstLeastMismatchCase = do
  let older :: Patch Int Int
      older =
        fromAscList
          [ (0, replace 0 10),
            (1, insert 20)
          ]
      newer :: Patch Int Int
      newer =
        fromAscList
          [ (0, replace 11 12),
            (1, assertAbsent)
          ]
  compose newer older
    @?= Left
      ComposeBoundaryMismatch
        { boundaryKey = 0,
          olderAfter = Just 10,
          newerBefore = Just 11
        }

disjointSeamCase :: IO ()
disjointSeamCase = do
  let olderRows :: [(Int, CellPatch Int)]
      olderRows =
        [ (key, replace key (key + 1))
          | key <- [0 .. 30]
        ]
      newerRows :: [(Int, CellPatch Int)]
      newerRows =
        [ (key, replace key (key + 1))
          | key <- [100 .. 130]
        ]
      older = fromAscList olderRows
      newer = fromAscList newerRows
      expected = fromAscList (olderRows <> newerRows)
  compose newer older @?= Right expected

endpointNativeFoldCase :: IO ()
endpointNativeFoldCase = do
  let patch :: Patch Int Int
      patch =
        fromAscList
          [ (0, assertAbsent),
            (1, insert 10),
            (2, delete 20),
            (3, replace 30 31)
          ]
      folded =
        foldWithKey'
          (\ !total key -> total + key)
          (\ !total key after -> total + key + after)
          (\ !total key before -> total + key + before)
          (\ !total key before after -> total + key + before + after)
          0
          patch
      materialized =
        foldl'
          ( \ !total (key, cell) ->
              total
                + key
                + maybe 0 id (cellBefore cell)
                + maybe 0 id (cellAfter cell)
          )
          0
          (toAscList patch)
  folded @?= materialized

differentPageLayoutCase :: IO ()
differentPageLayoutCase = do
  let rowKeys :: [Int]
      rowKeys = [0 .. 256]
      olderRows =
        [ (key, replace key (key + 1))
          | key <- rowKeys
        ]
      newerRows =
        [ (key, replace (key + 1) (key + 2))
          | key <- rowKeys
        ]
      expectedRows =
        [ (key, replace key (key + 2))
          | key <- rowKeys
        ]
      initialState = Map.fromAscList [(key, key) | key <- rowKeys]
      finalState = Map.fromAscList [(key, key + 2) | key <- rowKeys]
      olderCanonical = fromAscList olderRows
      newerCanonical = fromAscList newerRows
      expected = fromAscList expectedRows
  case recordMany (reverse olderRows) of
    Left err ->
      assertFailure ("unexpected recordMany rejection: " <> show err)
    Right olderRecorded -> do
      olderRecorded @?= olderCanonical
      compare olderRecorded olderCanonical @?= EQ
      compose newerCanonical olderRecorded @?= Right expected
      replay [olderRecorded, newerCanonical] initialState @?= Right finalState
