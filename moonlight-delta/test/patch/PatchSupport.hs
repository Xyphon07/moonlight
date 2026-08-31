{-# LANGUAGE DerivingStrategies #-}

module PatchSupport where

import Data.Map.Strict qualified as Map
import PatchLaws
  ( PatchChain (..),
    PatchStaleCase (..),
  )
import Moonlight.Delta.Patch
import Test.QuickCheck
  ( Gen,
    chooseInt,
    elements,
    frequency,
    listOf,
    sublistOf,
    vectorOf,
  )

data RepresentativeKey = RepresentativeKey
  { representativeKeyOrder :: !Int,
    representativeKeyName :: !String
  }
  deriving stock (Show)

instance Eq RepresentativeKey where
  left == right =
    representativeKeyOrder left == representativeKeyOrder right

instance Ord RepresentativeKey where
  compare left right =
    compare (representativeKeyOrder left) (representativeKeyOrder right)

data PatchFacadeChain = PatchFacadeChain
  { pfcState0 :: !(Map.Map Int Int),
    pfcState1 :: !(Map.Map Int Int),
    pfcState2 :: !(Map.Map Int Int),
    pfcState3 :: !(Map.Map Int Int)
  }
  deriving stock (Eq, Show)
patchDeltaEntriesStrictlyAscending :: Patch Int String -> Bool
patchDeltaEntriesStrictlyAscending =
  keysStrictlyAscending . fmap fst . patchToAscList

patchCanonicalBefore :: Patch key value -> Map.Map key value
patchCanonicalBefore =
  mapMaybeWithKey (const cellBefore)

patchCanonicalAfter :: Patch key value -> Map.Map key value
patchCanonicalAfter =
  mapMaybeWithKey (const cellAfter)

singletonPatch :: key -> Maybe value -> Maybe value -> Patch key value
singletonPatch key before after =
  singleton key (cellFromEndpoints before after)

patchFromList :: (PatchKey key, PatchValue value) => [(key, CellPatch value)] -> Patch key value
patchFromList =
  fromList

patchToAscList :: Patch key value -> [(key, CellPatch value)]
patchToAscList =
  toAscList

keysStrictlyAscending :: [Int] -> Bool
keysStrictlyAscending keysList =
  case keysList of
    [] ->
      True
    [_key] ->
      True
    left : right : rest ->
      left < right && keysStrictlyAscending (right : rest)

patchCellValues :: [Maybe String]
patchCellValues =
  [ Nothing,
    Just "a",
    Just "b",
    Just "c"
  ]

patchCellValueGen :: Gen (Maybe String)
patchCellValueGen =
  elements patchCellValues

patchEntryGen :: Gen (Int, CellPatch String)
patchEntryGen =
  (,)
    <$> chooseInt (0, 16)
    <*> (cellFromEndpoints <$> patchCellValueGen <*> patchCellValueGen)

patchDeltaGen :: Gen (Patch Int String)
patchDeltaGen =
  patchFromList <$> listOf patchEntryGen

patchPairGen :: Gen (Patch Int String, Patch Int String)
patchPairGen =
  (,) <$> patchDeltaGen <*> patchDeltaGen

patchStateEntryGen :: Gen (Int, String)
patchStateEntryGen =
  (,) <$> chooseInt (0, 16) <*> elements ["a", "b", "c"]

patchStateGen :: Gen (Map.Map Int String)
patchStateGen =
  Map.fromList <$> listOf patchStateEntryGen

patchApplyCaseGen :: Gen (Map.Map Int String, Patch Int String)
patchApplyCaseGen =
  (,) <$> patchStateGen <*> patchDeltaGen

patchReplayCaseGen :: Gen (Map.Map Int String, [Patch Int String])
patchReplayCaseGen =
  (,) <$> patchStateGen <*> listOf patchDeltaGen

patchThresholdStraddlingSizes :: [Int]
patchThresholdStraddlingSizes =
  [0, 1, 8, 15, 16, 17, 32]

patchStraddlingGen :: Gen (Patch Int String)
patchStraddlingGen = do
  targetSize <- elements patchThresholdStraddlingSizes
  cells <- vectorOf targetSize (snd <$> patchEntryGen)
  pure (patchFromList (zip [0 ..] cells))

patchStraddlingApplyCaseGen :: Gen (Map.Map Int String, Patch Int String)
patchStraddlingApplyCaseGen =
  (,) <$> patchStateGen <*> patchStraddlingGen

patchStraddlingPairGen :: Gen (Patch Int String, Patch Int String)
patchStraddlingPairGen =
  (,) <$> patchStraddlingGen <*> patchStraddlingGen

patchStraddlingReplayCaseGen :: Gen (Map.Map Int String, [Patch Int String])
patchStraddlingReplayCaseGen =
  (,) <$> patchStateGen <*> listOf patchStraddlingGen

patchStatePairGen :: Gen (Map.Map Int String, Map.Map Int String)
patchStatePairGen =
  (,) <$> patchStateGen <*> patchStateGen

patchFacadeChainGen :: Gen PatchFacadeChain
patchFacadeChainGen =
  frequency
    [ (1, elements (patchFacadeChain <$> patchFacadeKeyFamilies)),
      (9, patchFacadeRandomChainGen)
    ]

patchFacadeRandomChainGen :: Gen PatchFacadeChain
patchFacadeRandomChainGen =
  PatchFacadeChain
    <$> patchFacadeRandomStateGen
    <*> patchFacadeRandomStateGen
    <*> patchFacadeRandomStateGen
    <*> patchFacadeRandomStateGen

patchFacadeKeyUniverse :: [Int]
patchFacadeKeyUniverse =
  [0 .. 8]

patchFacadeRandomStateGen :: Gen (Map.Map Int Int)
patchFacadeRandomStateGen =
  sublistOf patchFacadeKeyUniverse
    >>= fmap Map.fromList . traverse patchFacadeRandomStateEntryGen

patchFacadeRandomStateEntryGen :: Int -> Gen (Int, Int)
patchFacadeRandomStateEntryGen key =
  (,) key <$> chooseInt (0, 3)

patchAppliedEditListGen :: Gen [(Int, CellPatch Int)]
patchAppliedEditListGen =
  patchFacadeChainEdits <$> patchFacadeChainGen

patchFacadeKeyFamilies :: [([Int], [Int], [Int], [Int])]
patchFacadeKeyFamilies =
  [ ([0 .. 8], [0 .. 8], [0 .. 8], [0 .. 8]),
    ([0, 2, 4, 6, 8], [1, 3, 5, 7], [0, 1, 2, 6, 7, 8], [0 .. 8]),
    ([0 .. 4], [2 .. 6], [4 .. 8], [1, 3, 5, 7]),
    ([0, 1], [3, 4], [6, 7], [2, 5, 8]),
    ([0, 4, 8], [0, 1, 4, 7], [2, 4, 6, 8], [1, 2, 3, 5, 8])
  ]

patchFacadeChain :: ([Int], [Int], [Int], [Int]) -> PatchFacadeChain
patchFacadeChain (keys0, keys1, keys2, keys3) =
  PatchFacadeChain
    { pfcState0 = patchFacadeState 0 keys0,
      pfcState1 = patchFacadeState 1 keys1,
      pfcState2 = patchFacadeState 2 keys2,
      pfcState3 = patchFacadeState 3 keys3
    }

patchFacadeState :: Int -> [Int] -> Map.Map Int Int
patchFacadeState stage keys =
  Map.fromAscList
    [ (key, stage * 16 + key)
      | key <- keys
    ]

patchFacadeChainEdits :: PatchFacadeChain -> [(Int, CellPatch Int)]
patchFacadeChainEdits chain =
  toAscList (diff (pfcState0 chain) (pfcState1 chain))
    <> toAscList (diff (pfcState1 chain) (pfcState2 chain))
    <> toAscList (diff (pfcState2 chain) (pfcState3 chain))

patchChainGen :: Gen (PatchChain Int String)
patchChainGen =
  PatchChain
    <$> chooseInt (0, 16)
    <*> patchCellValueGen
    <*> patchCellValueGen
    <*> patchCellValueGen

patchStaleCaseGen :: Gen (PatchStaleCase Int String)
patchStaleCaseGen = do
  key <- chooseInt (0, 16)
  expected <- patchCellValueGen
  actual <- elements (filter (/= expected) patchCellValues)
  replacement <- patchCellValueGen
  pure
    PatchStaleCase
      { pscKey = key,
        pscExpectedValue = expected,
        pscActualValue = actual,
        pscReplacementValue = replacement
      }
