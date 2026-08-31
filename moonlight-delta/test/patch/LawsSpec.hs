{-# LANGUAGE DerivingStrategies #-}

module LawsSpec
  ( lawTests,
  )
where

import Data.Foldable qualified as Foldable
import Data.Set qualified as Set
import Moonlight.Core (IsLawName (..), constructorLawName)
import PatchLaws
  ( patchLaws,
  )
import Moonlight.Delta.Patch
import Moonlight.Delta.Patch qualified as Patch
import PatchSupport
import LawManifest
  ( lawManifestCase,
    lawProperty,
  )
import Test.QuickCheck
  ( Property,
    counterexample,
    forAll,
    (===),
  )
import Test.Tasty (TestTree, testGroup)

data PatchFacadeLaw
  = PatchFacadeComposeIdentity
  | PatchFacadeComposeAssociativityMultiKey
  | PatchFacadeApplyComposeHomomorphismMultiKey
  | PatchFacadeSupportToAscList
  | PatchFacadeLookupToAscListCoherence
  | PatchFacadeRecordManyCoherence
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName PatchFacadeLaw where
  lawNameText =
    constructorLawName . show

lawTests :: TestTree
lawTests =
  testGroup
    "laws"
    [ patchLaws "single-cell patch" patchDeltaGen patchChainGen patchStaleCaseGen,
      lawManifestCase "patch facade" ([minBound .. maxBound] :: [PatchFacadeLaw]),
      lawProperty PatchFacadeComposeIdentity $
        forAll patchDeltaGen patchComposeIdentity,
      lawProperty PatchFacadeComposeAssociativityMultiKey $
        forAll patchFacadeChainGen patchComposeAssociativityMultiKey,
      lawProperty PatchFacadeApplyComposeHomomorphismMultiKey $
        forAll patchFacadeChainGen patchApplyComposeHomomorphismMultiKey,
      lawProperty PatchFacadeSupportToAscList $
        forAll patchDeltaGen patchSupportToAscList,
      lawProperty PatchFacadeLookupToAscListCoherence $
        forAll patchDeltaGen patchLookupToAscListCoherence,
      lawProperty PatchFacadeRecordManyCoherence $
        forAll patchAppliedEditListGen patchRecordManyCoherence
    ]

patchComposeIdentity :: Patch Int String -> Property
patchComposeIdentity patch =
  (compose emptyPatch patch, compose patch emptyPatch)
    === (Right patch, Right patch)
  where
    emptyPatch :: Patch Int String
    emptyPatch =
      empty

patchComposeAssociativityMultiKey :: PatchFacadeChain -> Property
patchComposeAssociativityMultiKey chain =
  (compose p3 =<< compose p2 p1)
    === (flip compose p1 =<< compose p3 p2)
  where
    p1 =
      diff (pfcState0 chain) (pfcState1 chain)
    p2 =
      diff (pfcState1 chain) (pfcState2 chain)
    p3 =
      diff (pfcState2 chain) (pfcState3 chain)

patchApplyComposeHomomorphismMultiKey :: PatchFacadeChain -> Property
patchApplyComposeHomomorphismMultiKey chain =
  case compose p2 p1 of
    Left err ->
      counterexample ("compatible multi-key patch chain refused composition: " <> show err) False
    Right composed ->
      apply composed (pfcState0 chain) === Right (pfcState2 chain)
  where
    p1 =
      diff (pfcState0 chain) (pfcState1 chain)
    p2 =
      diff (pfcState1 chain) (pfcState2 chain)

patchSupportToAscList :: Patch Int String -> Property
patchSupportToAscList patch =
  support patch === Set.fromList (fst <$> toAscList patch)

patchLookupToAscListCoherence :: Patch Int String -> Property
patchLookupToAscListCoherence patch =
  (presentLookups, absentLookups)
    === (expectedPresentLookups, expectedAbsentLookups)
  where
    rows =
      toAscList patch
    presentLookups =
      [(key, Patch.lookup key patch) | (key, _cell) <- rows]
    expectedPresentLookups =
      [(key, Just cell) | (key, cell) <- rows]
    presentKeys =
      Set.fromList (fst <$> rows)
    absentKeys =
      take 3 [key | key <- [-1 .. 17], Set.notMember key presentKeys]
    absentLookups =
      [(key, Patch.lookup key patch) | key <- absentKeys]
    expectedAbsentLookups =
      [(key, Nothing) | key <- absentKeys]

patchRecordManyCoherence :: [(Int, CellPatch Int)] -> Property
patchRecordManyCoherence edits =
  recordMany edits === recordAppliedFold edits

recordAppliedFold :: [(Int, CellPatch Int)] -> Either (ComposeError Int Int) (Patch Int Int)
recordAppliedFold =
  Foldable.foldl' appendApplied (Right empty)
  where
    appendApplied ::
      Either (ComposeError Int Int) (Patch Int Int) ->
      (Int, CellPatch Int) ->
      Either (ComposeError Int Int) (Patch Int Int)
    appendApplied applied (key, cell) =
      applied >>= recordApplied key cell
