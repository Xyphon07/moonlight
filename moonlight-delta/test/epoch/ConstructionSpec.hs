{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ConstructionSpec
  ( constructionTests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import EpochSupport.Generators
import EpochSupport.Types
import LawManifest (lawManifestCase, lawProperty)
import Moonlight.Core (IsLawName (..), OrdMap (..), OrdSet (..), SetKey, constructorLawName)
import Moonlight.Delta.Epoch
import Moonlight.Delta.Normalize (deltaNull)
import Test.QuickCheck (Property, (===), (.&&.))
import Test.Tasty (TestTree, testGroup)

data EpochDeltaConstructionLaw
  = EpochDeltaRequiresVersionAdvance
  | EpochDeltaRejectsTransportDomainEscape
  | EpochDeltaRejectsTransportImageEscape
  | EpochDeltaRejectsRetirementEscape
  | EpochDeltaRejectsTransportForRetiredSource
  | EpochDeltaRejectsUnmappedSurvivor
  | EpochDeltaRejectsChangedOutsideSource
  | EpochDeltaTransportIdentityFree
  | EpochDeltaAllowsManyToOneTransport
  | EpochDeltaFreshKeysDerivation
  | EpochDeltaDirtyTargetDerivation
  | EpochDeltaIdentityIsNull
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName EpochDeltaConstructionLaw where
  lawNameText = constructorLawName . show

constructionTests :: TestTree
constructionTests =
  testGroup
    "construction"
    [ lawManifestCase "epoch delta construction" ([minBound .. maxBound] :: [EpochDeltaConstructionLaw]),
      lawProperty EpochDeltaRequiresVersionAdvance sameVersionRejected,
      lawProperty EpochDeltaRejectsTransportDomainEscape transportDomainEscapeRejected,
      lawProperty EpochDeltaRejectsTransportImageEscape transportImageEscapeRejected,
      lawProperty EpochDeltaRejectsRetirementEscape retirementEscapeRejected,
      lawProperty EpochDeltaRejectsTransportForRetiredSource transportForRetiredSourceRejected,
      lawProperty EpochDeltaRejectsUnmappedSurvivor unmappedSurvivorRejected,
      lawProperty EpochDeltaRejectsChangedOutsideSource changedOutsideSourceRejected,
      lawProperty EpochDeltaTransportIdentityFree transportIdentityFree,
      lawProperty EpochDeltaAllowsManyToOneTransport manyToOneTransport,
      lawProperty EpochDeltaFreshKeysDerivation $
        epochDeltaIntCaseProperty freshKeysDerivation
          .&&. epochDeltaGenericCaseProperty freshKeysDerivation,
      lawProperty EpochDeltaDirtyTargetDerivation $
        epochDeltaIntCaseProperty dirtyTargetDerivation
          .&&. epochDeltaGenericCaseProperty dirtyTargetDerivation,
      lawProperty EpochDeltaIdentityIsNull identityIsNull
    ]

sameVersionRejected :: Property
sameVersionRejected =
  epochDelta endpoint endpoint IntMap.empty IntSet.empty IntSet.empty
    === Left (VersionDidNotAdvance (versionFromKey 1) (versionFromKey 1))
  where
    endpoint = Endpoint (versionFromKey 1) IntSet.empty

transportDomainEscapeRejected :: Property
transportDomainEscapeRejected =
  epochDelta source target (IntMap.singleton 0 1) IntSet.empty IntSet.empty
    === Left (TransportDomainEscapesSource 0)
  where
    source = Endpoint (versionFromKey 1) IntSet.empty
    target = Endpoint (versionFromKey 2) (IntSet.singleton 1)

transportImageEscapeRejected :: Property
transportImageEscapeRejected =
  epochDelta source target (IntMap.singleton 0 1) IntSet.empty IntSet.empty
    === Left (TransportImageEscapesTarget 0 1)
  where
    source = Endpoint (versionFromKey 1) (IntSet.singleton 0)
    target = Endpoint (versionFromKey 2) IntSet.empty

retirementEscapeRejected :: Property
retirementEscapeRejected =
  epochDelta source target IntMap.empty (IntSet.singleton 1) IntSet.empty
    === Left (RetiredKeyOutsideSource 1)
  where
    source = Endpoint (versionFromKey 1) (IntSet.singleton 0)
    target = Endpoint (versionFromKey 2) (IntSet.singleton 0)

transportForRetiredSourceRejected :: Property
transportForRetiredSourceRejected =
  epochDelta source target (IntMap.singleton 0 1) (IntSet.singleton 0) IntSet.empty
    === Left (TransportDefinedForRetiredSource 0)
  where
    source = Endpoint (versionFromKey 1) (IntSet.singleton 0)
    target = Endpoint (versionFromKey 2) (IntSet.singleton 1)

unmappedSurvivorRejected :: Property
unmappedSurvivorRejected =
  epochDelta source target IntMap.empty IntSet.empty IntSet.empty
    === Left (SurvivingKeyOutsideTarget 0)
  where
    source = Endpoint (versionFromKey 1) (IntSet.singleton 0)
    target = Endpoint (versionFromKey 2) IntSet.empty

changedOutsideSourceRejected :: Property
changedOutsideSourceRejected =
  epochDelta source target IntMap.empty IntSet.empty (IntSet.singleton 1)
    === Left (ChangedKeyOutsideSource 1)
  where
    source = Endpoint (versionFromKey 1) (IntSet.singleton 0)
    target = Endpoint (versionFromKey 2) (IntSet.singleton 0)

transportIdentityFree :: Property
transportIdentityFree =
  fmap transportOverrides (epochDelta source target (IntMap.fromList [(0, 0), (1, 2)]) IntSet.empty IntSet.empty)
    === Right (IntMap.singleton 1 2)
  where
    source = Endpoint (versionFromKey 1) (IntSet.fromList [0, 1])
    target = Endpoint (versionFromKey 2) (IntSet.fromList [0, 2])

manyToOneTransport :: Property
manyToOneTransport =
  fmap (\deltaValue -> transportKeys deltaValue querySourceKeys) (epochDelta source target transport IntSet.empty IntSet.empty)
    === Right (Transport transport IntSet.empty IntSet.empty)
  where
    querySourceKeys = IntSet.fromList [0, 1]
    source = Endpoint (versionFromKey 1) querySourceKeys
    target = Endpoint (versionFromKey 2) (IntSet.singleton 2)
    transport = IntMap.fromList [(0, 2), (1, 2)]

freshKeysDerivation ::
  (EpochKeyed keyMap observed, Eq observed, Show observed, Show keyMap, Show (SetKey observed)) =>
  EpochDeltaCase keyMap observed ->
  Property
freshKeysDerivation epochCase =
  freshKeys deltaValue
    === differenceSet (targetKeys deltaValue) transportedTargetKeys
  where
    deltaValue = edcDelta epochCase
    transportedTargetKeys =
      fromListSet (fmap snd (toAscListMap (transportedKeys (transportKeys deltaValue (sourceKeys deltaValue)))))

dirtyTargetDerivation ::
  (EpochKeyed keyMap observed, Eq observed, Show observed, Show keyMap, Show (SetKey observed)) =>
  EpochDeltaCase keyMap observed ->
  Property
dirtyTargetDerivation epochCase =
  changedKeysAcrossEpoch deltaValue
    === unionSet transportedChanged (freshKeys deltaValue)
  where
    input = edcInput epochCase
    deltaValue = edcDelta epochCase
    transportedChanged =
      fromListSet (fmap snd (toAscListMap (transportedKeys (transportKeys deltaValue (eiChanged input)))))

identityIsNull :: Property
identityIsNull =
  deltaNull (identityDelta endpoint :: EpochDelta (Map.Map GenericKey GenericKey) (Set.Set GenericKey)) === True
  where
    endpoint = Endpoint (versionFromKey 3) (Set.singleton (GenericKey 0))
