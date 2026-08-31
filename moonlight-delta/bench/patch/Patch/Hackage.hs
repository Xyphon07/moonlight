{-# LANGUAGE BangPatterns #-}

module Patch.Hackage
  ( prepareHackagePatchComposeFixture,
    prepareHackagePatchApplyFixture,
    prepareHackagePatchReplayFixture,
    hackagePatchComposeConstruct,
    hackagePatchApplyConstruct,
    hackageReplaySequentially,
    hackagePatchMapWeight,
    toHackagePatchMap,
    hackagePatchMapCompose,
    hackagePatchMapApply,
  )
where

import Control.Exception
  ( throwIO,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import BenchSupport
  ( BenchmarkFixtureFailure (..),
    assertBenchmarkAgreement,
    forceBenchmarkFixture,
    maybeIntWeight,
  )
import Patch.Types
import Moonlight.Delta.Patch
  ( Patch,
  )
import Moonlight.Delta.Patch qualified as Patch

prepareHackagePatchComposeFixture :: PatchComposeFixture -> IO HackagePatchComposeFixture
prepareHackagePatchComposeFixture fixture =
  case Patch.compose (pcfNewer fixture) (pcfOlder fixture) of
    Left err ->
      throwIO (BenchmarkFixtureFailure "patch compose hackage projection" (show err))
    Right expectedComposed -> do
      let !hackageFixture =
            HackagePatchComposeFixture
              { hpcfNewer = toHackagePatchMap (pcfNewer fixture),
                hpcfOlder = toHackagePatchMap (pcfOlder fixture)
              }
      assertBenchmarkAgreement
        "patch compose hackage projection"
        (toHackagePatchMap expectedComposed)
        (hackagePatchComposeConstruct hackageFixture)
      forceBenchmarkFixture hackageFixture

prepareHackagePatchApplyFixture :: PatchApplyFixture -> IO HackagePatchApplyFixture
prepareHackagePatchApplyFixture fixture =
  case Patch.apply (pafPatch fixture) (pafState fixture) of
    Left err ->
      throwIO (BenchmarkFixtureFailure "patch apply hackage projection" (show err))
    Right expectedState -> do
      let !hackageFixture =
            HackagePatchApplyFixture
              { hpafState = pafState fixture,
                hpafPatch = toHackagePatchMap (pafPatch fixture)
              }
      assertBenchmarkAgreement
        "patch apply hackage projection"
        expectedState
        (hackagePatchApplyConstruct hackageFixture)
      forceBenchmarkFixture hackageFixture

prepareHackagePatchReplayFixture :: PatchReplayFixture -> IO HackagePatchReplayFixture
prepareHackagePatchReplayFixture fixture =
  case Patch.replay (prfPatches fixture) (prfInitialState fixture) of
    Left err ->
      throwIO (BenchmarkFixtureFailure "patch replay hackage projection" (show err))
    Right expectedState -> do
      let !hackageFixture =
            HackagePatchReplayFixture
              { hprfInitialState = prfInitialState fixture,
                hprfPatches = fmap toHackagePatchMap (prfPatches fixture)
              }
      assertBenchmarkAgreement
        "patch replay hackage projection"
        expectedState
        (hackageReplaySequentially (hprfInitialState hackageFixture) (hprfPatches hackageFixture))
      forceBenchmarkFixture hackageFixture

{-# NOINLINE hackagePatchComposeConstruct #-}
hackagePatchComposeConstruct ::
  HackagePatchComposeFixture ->
  HackagePatchMap
hackagePatchComposeConstruct fixture =
  hackagePatchMapCompose (hpcfNewer fixture) (hpcfOlder fixture)

{-# NOINLINE hackagePatchApplyConstruct #-}
hackagePatchApplyConstruct ::
  HackagePatchApplyFixture ->
  Map Int Int
hackagePatchApplyConstruct fixture =
  hackagePatchMapApply (hpafPatch fixture) (hpafState fixture)

hackageReplaySequentially ::
  Map Int Int ->
  [HackagePatchMap] ->
  Map Int Int
hackageReplaySequentially =
  foldl' (\ !state patchValue -> hackagePatchMapApply patchValue state)

hackagePatchMapWeight :: HackagePatchMap -> Int
hackagePatchMapWeight patchMap =
  Map.foldlWithKey' weighEntry 0 (unHackagePatchMap patchMap)
  where
    weighEntry :: Int -> Int -> Maybe Int -> Int
    weighEntry !total key maybeValue =
      total + key + maybeIntWeight maybeValue

toHackagePatchMap :: Patch Int Int -> HackagePatchMap
toHackagePatchMap =
  HackagePatchMap
    . Map.fromDistinctAscList
    . Patch.foldWithKey
      (\_key rest -> rest)
      (\key after rest -> (key, Just after) : rest)
      (\key _before rest -> (key, Nothing) : rest)
      (\key _before after rest -> (key, Just after) : rest)
      []

hackagePatchMapCompose :: HackagePatchMap -> HackagePatchMap -> HackagePatchMap
hackagePatchMapCompose (HackagePatchMap newer) (HackagePatchMap older) =
  HackagePatchMap (Map.union newer older)

hackagePatchMapApply :: HackagePatchMap -> Map Int Int -> Map Int Int
hackagePatchMapApply (HackagePatchMap patchMap) old =
  let !insertions = Map.mapMaybe id patchMap
      !deletions = Map.mapMaybe hackageDeletion patchMap
   in insertions `Map.union` (old `Map.difference` deletions)

hackageDeletion :: Maybe Int -> Maybe ()
hackageDeletion maybeValue =
  case maybeValue of
    Nothing ->
      Just ()
    Just _ ->
      Nothing
