{-# LANGUAGE BangPatterns #-}

module Patch.Types
  ( PatchComposeFixture (..),
    PatchApplyFixture (..),
    PatchReplayFixture (..),
    HackagePatchMap (..),
    HackagePatchComposeFixture (..),
    HackagePatchApplyFixture (..),
    HackagePatchReplayFixture (..),
    PatchDiffFixture (..),
    PatchInvertFixture (..),
    PatchSupportFixture (..),
    PatchProducerFixture (..),
    PreparedPatch (..),
    PreparedHackagePatchMap (..),
    AllocationCase (..),
    AllocationResult (..),
    PatchComposeImplementation,
    PatchApplyImplementation,
    rnfPatch,
    rnfPatchList,
    rnfCellList,
    rnfHackagePatchMap,
    rnfHackagePatchMapList,
    checkedMapMergeLabel,
    moonlightPagedPatchLabel,
    hackagePatchMapLabel,
    moonlightSplitApplyLabel,
    checkedSequentialMapMergeLabel,
    moonlightSequentialApplyLabel,
    moonlightUncheckedSequentialPatchLabel,
    hackageSequentialPatchMapLabel,
    moonlightFusedReplayLabel,
  )
where

import Control.DeepSeq
  ( NFData (rnf),
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Delta.Patch
  ( ApplyError,
    CellPatch,
    ComposeError,
    Patch,
  )
import Moonlight.Delta.Patch qualified as Patch

data PatchComposeFixture = PatchComposeFixture
  { pcfNewer :: !(Patch Int Int),
    pcfOlder :: !(Patch Int Int)
  }

data PatchApplyFixture = PatchApplyFixture
  { pafState :: !(Map Int Int),
    pafPatch :: !(Patch Int Int)
  }

data PatchReplayFixture = PatchReplayFixture
  { prfInitialState :: !(Map Int Int),
    prfPatches :: ![Patch Int Int]
  }

-- Benchmark-private projection of Hackage patch-0.0.8.4 Data.Patch.Map:
-- PatchMap k v = Map k (Maybe v), compose is left-biased union, and apply is
-- insertions union (old difference deletions). It cannot represent
-- AssertAbsent or before-value validation, so these rows are unchecked baseline
-- comparisons, not semantic replacements for Patch.
newtype HackagePatchMap = HackagePatchMap
  { unHackagePatchMap :: Map Int (Maybe Int)
  }
  deriving stock (Eq, Show)

data HackagePatchComposeFixture = HackagePatchComposeFixture
  { hpcfNewer :: !HackagePatchMap,
    hpcfOlder :: !HackagePatchMap
  }

data HackagePatchApplyFixture = HackagePatchApplyFixture
  { hpafState :: !(Map Int Int),
    hpafPatch :: !HackagePatchMap
  }

data HackagePatchReplayFixture = HackagePatchReplayFixture
  { hprfInitialState :: !(Map Int Int),
    hprfPatches :: ![HackagePatchMap]
  }

data PatchDiffFixture = PatchDiffFixture
  { pdfBeforeState :: !(Map Int Int),
    pdfAfterState :: !(Map Int Int)
  }

newtype PatchInvertFixture = PatchInvertFixture
  { pifPatch :: Patch Int Int
  }

newtype PatchSupportFixture = PatchSupportFixture
  { psfPatch :: Patch Int Int
  }

newtype PatchProducerFixture = PatchProducerFixture
  { ppfCells :: [(Int, CellPatch Int)]
  }

newtype PreparedPatch = PreparedPatch
  { preparedPatch :: Patch Int Int
  }

newtype PreparedHackagePatchMap = PreparedHackagePatchMap
  { preparedHackagePatchMap :: HackagePatchMap
  }

instance NFData PreparedPatch where
  rnf prepared =
    rnfPatch (preparedPatch prepared)

instance NFData PreparedHackagePatchMap where
  rnf prepared =
    rnfHackagePatchMap (preparedHackagePatchMap prepared)

data AllocationCase = AllocationCase
  { allocationCaseName :: !String,
    allocationCaseAction :: !(IO (Int -> IO Int))
  }

data AllocationResult = AllocationResult
  { allocationResultName :: !String,
    allocationResultGrossBytes :: !Word64,
    allocationResultBaselineBytes :: !Word64,
    allocationResultNetBytes :: !Integer,
    allocationResultRepetitions :: !Int,
    allocationResultChecksum :: !Int
  }

instance NFData PatchComposeFixture where
  rnf fixture =
    rnfPatch (pcfNewer fixture)
      `seq` rnfPatch (pcfOlder fixture)

instance NFData PatchApplyFixture where
  rnf fixture =
    rnf (pafState fixture) `seq` rnfPatch (pafPatch fixture)

instance NFData PatchReplayFixture where
  rnf fixture =
    rnf (prfInitialState fixture)
      `seq` rnfPatchList (prfPatches fixture)

instance NFData HackagePatchComposeFixture where
  rnf fixture =
    rnfHackagePatchMap (hpcfNewer fixture)
      `seq` rnfHackagePatchMap (hpcfOlder fixture)

instance NFData HackagePatchApplyFixture where
  rnf fixture =
    rnf (hpafState fixture)
      `seq` rnfHackagePatchMap (hpafPatch fixture)

instance NFData HackagePatchReplayFixture where
  rnf fixture =
    rnf (hprfInitialState fixture)
      `seq` rnfHackagePatchMapList (hprfPatches fixture)

instance NFData PatchDiffFixture where
  rnf fixture =
    rnf (pdfBeforeState fixture) `seq` rnf (pdfAfterState fixture)

instance NFData PatchInvertFixture where
  rnf fixture =
    rnfPatch (pifPatch fixture)

instance NFData PatchSupportFixture where
  rnf fixture =
    rnfPatch (psfPatch fixture)

instance NFData PatchProducerFixture where
  rnf fixture =
    rnfCellList (ppfCells fixture)

rnfPatch :: Patch Int Int -> ()
rnfPatch =
  Patch.foldWithKey
    (\key rest -> rnf key `seq` rest)
    (\key after rest -> rnf key `seq` rnf after `seq` rest)
    (\key before rest -> rnf key `seq` rnf before `seq` rest)
    (\key before after rest -> rnf key `seq` rnf before `seq` rnf after `seq` rest)
    ()

rnfPatchList :: [Patch Int Int] -> ()
rnfPatchList =
  foldr (\patchValue rest -> rnfPatch patchValue `seq` rest) ()

rnfCellList :: [(Int, CellPatch Int)] -> ()
rnfCellList =
  foldr rnfCell ()
  where
    rnfCell :: (Int, CellPatch Int) -> () -> ()
    rnfCell (key, patchValue) rest =
      rnf key
        `seq` rnf (Patch.cellBefore patchValue)
        `seq` rnf (Patch.cellAfter patchValue)
        `seq` rest

rnfHackagePatchMap :: HackagePatchMap -> ()
rnfHackagePatchMap patchMap =
  rnf (unHackagePatchMap patchMap)

rnfHackagePatchMapList :: [HackagePatchMap] -> ()
rnfHackagePatchMapList =
  foldr (\patchMap rest -> rnfHackagePatchMap patchMap `seq` rest) ()

type PatchComposeImplementation =
  Patch Int Int ->
  Patch Int Int ->
  Either (ComposeError Int Int) (Patch Int Int)

type PatchApplyImplementation =
  Patch Int Int ->
  Map Int Int ->
  Either (ApplyError Int Int) (Map Int Int)

checkedMapMergeLabel :: String
checkedMapMergeLabel =
  "reference: checked Map.mergeA"

moonlightPagedPatchLabel :: String
moonlightPagedPatchLabel =
  "moonlight: paged Patch"

hackagePatchMapLabel :: String
hackagePatchMapLabel =
  "hackage-source: unchecked Data.Patch.Map PatchMap"

moonlightSplitApplyLabel :: String
moonlightSplitApplyLabel =
  "moonlight: split-link Patch.apply"

checkedSequentialMapMergeLabel :: String
checkedSequentialMapMergeLabel =
  "reference: sequential checked Map.mergeA"

moonlightSequentialApplyLabel :: String
moonlightSequentialApplyLabel =
  "moonlight: sequential Patch.apply"

moonlightUncheckedSequentialPatchLabel :: String
moonlightUncheckedSequentialPatchLabel =
  "moonlight: unchecked sequential Patch fold"

hackageSequentialPatchMapLabel :: String
hackageSequentialPatchMapLabel =
  "hackage-source: unchecked sequential Data.Patch.Map"

moonlightFusedReplayLabel :: String
moonlightFusedReplayLabel =
  "moonlight: fused Patch.replay"
