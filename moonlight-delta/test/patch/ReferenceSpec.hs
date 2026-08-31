module ReferenceSpec
  ( referenceTests,
  )
where

import Data.Map.Internal.Debug qualified as MapDebug
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch
import PatchSupport
import PatchReference qualified
import Test.QuickCheck
  ( Property,
    forAll,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck (testProperty)

referenceTests :: TestTree
referenceTests =
  testGroup
    "reference"
    [ testProperty "compose matches reference" $ forAll patchPairGen patchComposeMatchesReference,
      testProperty "compose emits strictly ascending patch entries" $
        forAll patchPairGen patchComposeEmitsStrictlyAscendingEntries,
      testProperty "apply matches reference" $ forAll patchApplyCaseGen patchApplyMatchesReference,
      testProperty "apply emits a valid map" $ forAll patchApplyCaseGen patchApplyEmitsValidMap,
      testProperty "fused replay matches sequential reference" $ forAll patchReplayCaseGen patchReplayMatchesReference,
      testProperty "fused replay emits a valid map" $ forAll patchReplayCaseGen patchReplayEmitsValidMap,
      testProperty "diff applies from source to target" $ forAll patchStatePairGen patchDiffApplies,
      testProperty "invert reverses canonical endpoints" $ forAll patchDeltaGen patchInvertReversesCanonicalEndpoints,
      testProperty "invert is involutive" $ forAll patchDeltaGen patchInvertInvolutive,
      testProperty "authoritative map construction round-trips the logical view" $
        forAll patchDeltaGen patchMapRoundTrip,
      testGroup
        "small-form threshold sweep"
        [ testProperty "apply matches reference across the small/paged threshold" $
            forAll patchStraddlingApplyCaseGen patchApplyMatchesReference,
          testProperty "compose matches reference across the small/paged threshold" $
            forAll patchStraddlingPairGen patchComposeMatchesReference,
          testProperty "fused replay matches reference across the small/paged threshold" $
            forAll patchStraddlingReplayCaseGen patchReplayMatchesReference,
          testProperty "map construction round-trips across the small/paged threshold" $
            forAll patchStraddlingGen patchMapRoundTrip,
          testProperty "invert is involutive across the small/paged threshold" $
            forAll patchStraddlingGen patchInvertInvolutive
        ],
      testCase "all finite two-key compositions match the reference" $
        finiteCompositionMismatches @?= [],
      testCase "all finite two-key applications match the reference" $
        finiteApplicationMismatches @?= [],
      testCase "all finite two-step replays match the reference" $
        finiteReplayMismatches @?= []
    ]

patchComposeMatchesReference :: (Patch Int String, Patch Int String) -> Property
patchComposeMatchesReference (newer, older) =
  compose newer older
    === PatchReference.compose newer older

patchComposeEmitsStrictlyAscendingEntries :: (Patch Int String, Patch Int String) -> Property
patchComposeEmitsStrictlyAscendingEntries (newer, older) =
  case compose newer older of
    Left _err ->
      True === True
    Right patch ->
      patchDeltaEntriesStrictlyAscending patch === True

patchApplyMatchesReference :: (Map.Map Int String, Patch Int String) -> Property
patchApplyMatchesReference (state, patch) =
  apply patch state
    === PatchReference.apply patch state

patchApplyEmitsValidMap :: (Map.Map Int String, Patch Int String) -> Property
patchApplyEmitsValidMap (state, patch) =
  case apply patch state of
    Left _err ->
      True === True
    Right result ->
      MapDebug.valid result === True

patchReplayMatchesReference :: (Map.Map Int String, [Patch Int String]) -> Property
patchReplayMatchesReference (state, patches) =
  replay patches state
    === PatchReference.replay patches state

patchReplayEmitsValidMap :: (Map.Map Int String, [Patch Int String]) -> Property
patchReplayEmitsValidMap (state, patches) =
  case replay patches state of
    Left _err ->
      True === True
    Right result ->
      MapDebug.valid result === True

patchDiffApplies :: (Map.Map Int String, Map.Map Int String) -> Property
patchDiffApplies (before, after) =
  apply (diff before after) before === Right after

patchInvertReversesCanonicalEndpoints :: Patch Int String -> Property
patchInvertReversesCanonicalEndpoints patch =
  apply (invert patch) (patchCanonicalAfter patch) === Right (patchCanonicalBefore patch)

patchInvertInvolutive :: Patch Int String -> Property
patchInvertInvolutive patch =
  invert (invert patch) === patch

patchMapRoundTrip :: Patch Int String -> Property
patchMapRoundTrip patch =
  fromAscList (toAscList patch) === patch
finiteCompositionMismatches :: [((Patch Int Bool, Patch Int Bool), Either (ComposeError Int Bool) (Patch Int Bool), Either (ComposeError Int Bool) (Patch Int Bool))]
finiteCompositionMismatches =
  take
    1
    [ ((newer, older), actual, expected)
      | newer <- finitePatches,
        older <- finitePatches,
        let actual = compose newer older,
        let expected = PatchReference.compose newer older,
        actual /= expected
    ]

finiteApplicationMismatches :: [((Map.Map Int Bool, Patch Int Bool), Either (ApplyError Int Bool) (Map.Map Int Bool), Either (ApplyError Int Bool) (Map.Map Int Bool))]
finiteApplicationMismatches =
  take
    1
    [ ((state, patch), actual, expected)
      | state <- finiteStates,
        patch <- finitePatches,
        let actual = apply patch state,
        let expected = PatchReference.apply patch state,
        actual /= expected
    ]

finiteReplayMismatches :: [((Map.Map Int Bool, Patch Int Bool, Patch Int Bool), Either (ReplayError Int Bool) (Map.Map Int Bool), Either (ReplayError Int Bool) (Map.Map Int Bool))]
finiteReplayMismatches =
  take
    1
    [ ((state, first, second), actual, expected)
      | state <- finiteStates,
        first <- finitePatches,
        second <- finitePatches,
        let patches = [first, second],
        let actual = replay patches state,
        let expected = PatchReference.replay patches state,
        actual /= expected
    ]

finitePatches :: [Patch Int Bool]
finitePatches =
  [ fromAscList [(key, cell) | (key, Just cell) <- zip [0, 1] choices]
    | choices <- sequence (replicate 2 (Nothing : fmap Just finiteCells))
  ]

finiteCells :: [CellPatch Bool]
finiteCells =
  [ cellFromEndpoints before after
    | before <- [Nothing, Just False, Just True],
      after <- [Nothing, Just False, Just True]
  ]

finiteStates :: [Map.Map Int Bool]
finiteStates =
  [ Map.fromAscList [(key, value) | (key, Just value) <- zip [0, 1] choices]
    | choices <- sequence (replicate 2 [Nothing, Just False, Just True])
  ]
