{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EpochSupport.Generators where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Delta.Epoch
import EpochSupport.Types
import Test.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    elements,
    forAll,
    listOf,
    sublistOf,
    suchThatMap,
  )

contextProjectionDeltaIntGen :: Gen (ContextProjectionDelta IntSet)
contextProjectionDeltaIntGen =
  ContextProjectionDelta <$> intSetGen <*> intSetGen

contextProjectionCarrierIntGen :: Gen (ContextProjectionDelta Int)
contextProjectionCarrierIntGen =
  ContextProjectionDelta <$> chooseInt (-16, 16) <*> chooseInt (-16, 16)

contextProjectionDeltaGenericGen :: Gen (ContextProjectionDelta GenericSet)
contextProjectionDeltaGenericGen =
  ContextProjectionDelta <$> genericSetGen <*> genericSetGen

projectionIntProperty :: (ContextProjectionDelta IntSet -> Property) -> Property
projectionIntProperty law =
  forAll contextProjectionDeltaIntGen law

projectionGenericProperty :: (ContextProjectionDelta GenericSet -> Property) -> Property
projectionGenericProperty law =
  forAll contextProjectionDeltaGenericGen law

projectionIntPairProperty ::
  ((ContextProjectionDelta IntSet, ContextProjectionDelta IntSet) -> Property) ->
  Property
projectionIntPairProperty law =
  forAll ((,) <$> contextProjectionDeltaIntGen <*> contextProjectionDeltaIntGen) law

projectionGenericPairProperty ::
  ((ContextProjectionDelta GenericSet, ContextProjectionDelta GenericSet) -> Property) ->
  Property
projectionGenericPairProperty law =
  forAll ((,) <$> contextProjectionDeltaGenericGen <*> contextProjectionDeltaGenericGen) law

projectionIntTripleProperty ::
  ((ContextProjectionDelta IntSet, ContextProjectionDelta IntSet, ContextProjectionDelta IntSet) -> Property) ->
  Property
projectionIntTripleProperty law =
  forAll ((,,) <$> contextProjectionDeltaIntGen <*> contextProjectionDeltaIntGen <*> contextProjectionDeltaIntGen) law

projectionGenericTripleProperty ::
  ((ContextProjectionDelta GenericSet, ContextProjectionDelta GenericSet, ContextProjectionDelta GenericSet) -> Property) ->
  Property
projectionGenericTripleProperty law =
  forAll ((,,) <$> contextProjectionDeltaGenericGen <*> contextProjectionDeltaGenericGen <*> contextProjectionDeltaGenericGen) law

viewIntGen :: Gen (ContextView IntSet Int)
viewIntGen =
  viewAt <$> epochVersionSmallGen <*> intSetGen <*> chooseInt (-16, 16)

viewGenericGen :: Gen (ContextView GenericSet Int)
viewGenericGen =
  viewAt <$> epochVersionSmallGen <*> genericSetGen <*> chooseInt (-16, 16)

viewIntProperty :: (ContextView IntSet Int -> Property) -> Property
viewIntProperty law =
  forAll viewIntGen law

viewGenericProperty :: (ContextView GenericSet Int -> Property) -> Property
viewGenericProperty law =
  forAll viewGenericGen law

epochVersionPairIntViewGen :: Gen (Version, ContextView IntSet Int)
epochVersionPairIntViewGen =
  (,) <$> epochVersionSmallGen <*> viewIntGen

epochVersionPairGenericViewGen :: Gen (Version, ContextView GenericSet Int)
epochVersionPairGenericViewGen =
  (,) <$> epochVersionSmallGen <*> viewGenericGen

epochDeltaIntGen :: Gen (EpochDelta (IntMap Int) IntSet)
epochDeltaIntGen =
  edcDelta <$> epochDeltaIntCaseGen

epochDeltaIntCaseGen :: Gen (EpochDeltaCase (IntMap Int) IntSet)
epochDeltaIntCaseGen =
  suchThatMap epochInputIntGen mintEpochCase

epochDeltaGenericCaseGen :: Gen (EpochDeltaCase GenericMap GenericSet)
epochDeltaGenericCaseGen =
  suchThatMap epochInputGenericGen mintEpochCase

epochDeltaIntCaseProperty :: (EpochDeltaCase (IntMap Int) IntSet -> Property) -> Property
epochDeltaIntCaseProperty law =
  forAll epochDeltaIntCaseGen law

epochDeltaGenericCaseProperty :: (EpochDeltaCase GenericMap GenericSet -> Property) -> Property
epochDeltaGenericCaseProperty law =
  forAll epochDeltaGenericCaseGen law

epochPairIntGen :: Gen (EpochPairCase (IntMap Int) IntSet)
epochPairIntGen =
  compatiblePairGen intSetGen transportProposalIntGen intSubsetOf

stableEpochPairIntGen :: Gen (EpochPairCase (IntMap Int) IntSet)
stableEpochPairIntGen =
  stablePairGen intSetGen transportProposalIntGen intSubsetOf

stableEpochChainIntGen :: Gen (EpochChainCase (IntMap Int) IntSet)
stableEpochChainIntGen =
  stableChainGen intSetGen transportProposalIntGen intSubsetOf

stableEpochPairGenericGen :: Gen (EpochPairCase GenericMap GenericSet)
stableEpochPairGenericGen =
  stablePairGen genericSetGen transportProposalGenericGen genericSubsetOf

stableEpochChainGenericGen :: Gen (EpochChainCase GenericMap GenericSet)
stableEpochChainGenericGen =
  stableChainGen genericSetGen transportProposalGenericGen genericSubsetOf

epochInputIntGen :: Gen (EpochInput (IntMap Int) IntSet)
epochInputIntGen = do
  sourceVersionKey <- chooseInt (0, 32)
  targetVersionKey <- chooseInt (sourceVersionKey + 1, sourceVersionKey + 4)
  sourceKeySet <- intSetGen
  targetKeySet <- intSetGen
  (transport, retired) <- transportProposalIntGen sourceKeySet targetKeySet
  changed <- intSubsetOf sourceKeySet
  pure
    EpochInput
      { eiSource = Endpoint (versionFromKey (fromIntegral sourceVersionKey)) sourceKeySet,
        eiTarget = Endpoint (versionFromKey (fromIntegral targetVersionKey)) targetKeySet,
        eiTransport = transport,
        eiRetired = retired,
        eiChanged = changed
      }

epochInputGenericGen :: Gen (EpochInput GenericMap GenericSet)
epochInputGenericGen = do
  sourceVersionKey <- chooseInt (0, 32)
  targetVersionKey <- chooseInt (sourceVersionKey + 1, sourceVersionKey + 4)
  sourceKeySet <- genericSetGen
  targetKeySet <- genericSetGen
  (transport, retired) <- transportProposalGenericGen sourceKeySet targetKeySet
  changed <- genericSubsetOf sourceKeySet
  pure
    EpochInput
      { eiSource = Endpoint (versionFromKey (fromIntegral sourceVersionKey)) sourceKeySet,
        eiTarget = Endpoint (versionFromKey (fromIntegral targetVersionKey)) targetKeySet,
        eiTransport = transport,
        eiRetired = retired,
        eiChanged = changed
      }

mintEpochCase ::
  EpochKeyed keyMap observed =>
  EpochInput keyMap observed ->
  Maybe (EpochDeltaCase keyMap observed)
mintEpochCase input =
  case epochDelta (eiSource input) (eiTarget input) (eiTransport input) (eiRetired input) (eiChanged input) of
    Left _err ->
      Nothing
    Right deltaValue ->
      Just
        EpochDeltaCase
          { edcInput = input,
            edcDelta = deltaValue
          }

compatiblePairGen ::
  (EpochKeyed keyMap observed, Show observed) =>
  Gen observed ->
  (observed -> observed -> Gen (keyMap, observed)) ->
  (observed -> Gen observed) ->
  Gen (EpochPairCase keyMap observed)
compatiblePairGen observedGen rekeyGen subsetGen = do
  sourceKeySet <- observedGen
  middleKeys <- observedGen
  targetKeySet <- observedGen
  first <- deltaBetweenGen (versionFromKey 0) (versionFromKey 1) sourceKeySet middleKeys rekeyGen subsetGen
  second <- deltaBetweenGen (versionFromKey 1) (versionFromKey 2) middleKeys targetKeySet rekeyGen subsetGen
  pure (EpochPairCase first second)

stablePairGen ::
  (EpochKeyed keyMap observed, Show observed) =>
  Gen observed ->
  (observed -> observed -> Gen (keyMap, observed)) ->
  (observed -> Gen observed) ->
  Gen (EpochPairCase keyMap observed)
stablePairGen observedGen rekeyGen subsetGen = do
  keysValue <- observedGen
  first <- deltaBetweenGen (versionFromKey 0) (versionFromKey 1) keysValue keysValue rekeyGen subsetGen
  second <- deltaBetweenGen (versionFromKey 1) (versionFromKey 2) keysValue keysValue rekeyGen subsetGen
  pure (EpochPairCase first second)

stableChainGen ::
  (EpochKeyed keyMap observed, Show observed) =>
  Gen observed ->
  (observed -> observed -> Gen (keyMap, observed)) ->
  (observed -> Gen observed) ->
  Gen (EpochChainCase keyMap observed)
stableChainGen observedGen rekeyGen subsetGen = do
  keysValue <- observedGen
  first <- deltaBetweenGen (versionFromKey 0) (versionFromKey 1) keysValue keysValue rekeyGen subsetGen
  second <- deltaBetweenGen (versionFromKey 1) (versionFromKey 2) keysValue keysValue rekeyGen subsetGen
  third <- deltaBetweenGen (versionFromKey 2) (versionFromKey 3) keysValue keysValue rekeyGen subsetGen
  pure (EpochChainCase first second third)

deltaBetweenGen ::
  (EpochKeyed keyMap observed, Show observed) =>
  Version ->
  Version ->
  observed ->
  observed ->
  (observed -> observed -> Gen (keyMap, observed)) ->
  (observed -> Gen observed) ->
  Gen (EpochDelta keyMap observed)
deltaBetweenGen sourceVersionValue targetVersionValue sourceKeySet targetKeySet transportGen subsetGen =
  suchThatMap inputGen (fmap edcDelta . mintEpochCase)
  where
    inputGen = do
      (transport, retired) <- transportGen sourceKeySet targetKeySet
      changed <- subsetGen sourceKeySet
      pure
        EpochInput
          { eiSource = Endpoint sourceVersionValue sourceKeySet,
            eiTarget = Endpoint targetVersionValue targetKeySet,
            eiTransport = transport,
            eiRetired = retired,
            eiChanged = changed
          }

intSetGen :: Gen IntSet
intSetGen =
  IntSet.fromList <$> listOf (chooseInt (0, 16))

genericSetGen :: Gen GenericSet
genericSetGen =
  Set.fromList <$> listOf genericKeyGen

genericKeyGen :: Gen GenericKey
genericKeyGen =
  GenericKey <$> chooseInt (0, 16)

epochVersionSmallGen :: Gen Version
epochVersionSmallGen =
  versionFromKey . fromIntegral <$> chooseInt (0, 16)

intSubsetOf :: IntSet -> Gen IntSet
intSubsetOf keysValue =
  IntSet.fromList <$> sublistOf (IntSet.toAscList keysValue)

genericSubsetOf :: GenericSet -> Gen GenericSet
genericSubsetOf keysValue =
  Set.fromList <$> sublistOf (Set.toAscList keysValue)

transportProposalIntGen :: IntSet -> IntSet -> Gen (IntMap Int, IntSet)
transportProposalIntGen sourceKeySet targetKeySet =
  case IntSet.toAscList targetKeySet of
    [] ->
      pure (IntMap.empty, sourceKeySet)
    targetList -> do
      retired <- IntSet.fromList <$> sublistOf (IntSet.toAscList sourceKeySet)
      let surviving = IntSet.difference sourceKeySet retired
      chosenEntries <- traverse (\sourceKey -> (sourceKey,) <$> elements targetList) (IntSet.toAscList surviving)
      pure (IntMap.fromList chosenEntries, retired)

transportProposalGenericGen :: GenericSet -> GenericSet -> Gen (GenericMap, GenericSet)
transportProposalGenericGen sourceKeySet targetKeySet =
  case Set.toAscList targetKeySet of
    [] ->
      pure (Map.empty, sourceKeySet)
    targetList -> do
      retired <- Set.fromList <$> sublistOf (Set.toAscList sourceKeySet)
      let surviving = Set.difference sourceKeySet retired
      chosenEntries <- traverse (\sourceKey -> (sourceKey,) <$> elements targetList) (Set.toAscList surviving)
      pure (Map.fromList chosenEntries, retired)
