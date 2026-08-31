{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ComposeSpec
  ( composeTests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import EpochSupport.Generators
import EpochSupport.Reference
import EpochSupport.Types
import LawManifest (lawManifestCase, lawProperty)
import Moonlight.Core (IsLawName (..), SetKey, constructorLawName)
import Moonlight.Delta.Epoch
import Test.QuickCheck (Property, counterexample, forAll, (===), (.&&.))
import Test.Tasty (TestTree, testGroup)

data EpochComposeLaw
  = EpochComposeBoundaryMismatchRejected
  | EpochComposeIdentityLeft
  | EpochComposeIdentityRight
  | EpochComposeAssociative
  | EpochComposeAgreesWithSequentialReference
  | EpochComposeDirtyTargetSequential
  | EpochComposeRetireRecreateDirty
  | EpochComposeRenameIntoRetirement
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName EpochComposeLaw where
  lawNameText = constructorLawName . show

composeTests :: TestTree
composeTests =
  testGroup
    "compose"
    [ lawManifestCase "epoch compose" ([minBound .. maxBound] :: [EpochComposeLaw]),
      lawProperty EpochComposeBoundaryMismatchRejected boundaryMismatchRejected,
      lawProperty EpochComposeIdentityLeft $
        epochDeltaIntCaseProperty composeIdentityLeft
          .&&. epochDeltaGenericCaseProperty composeIdentityLeft,
      lawProperty EpochComposeIdentityRight $
        epochDeltaIntCaseProperty composeIdentityRight
          .&&. epochDeltaGenericCaseProperty composeIdentityRight,
      lawProperty EpochComposeAssociative $
        forAll stableEpochChainIntGen composeAssociative
          .&&. forAll stableEpochChainGenericGen composeAssociative,
      lawProperty EpochComposeAgreesWithSequentialReference $
        forAll epochPairIntGen composeAgreesWithReference,
      lawProperty EpochComposeDirtyTargetSequential $
        forAll epochPairIntGen composeDirtyTargetSequential,
      lawProperty EpochComposeRetireRecreateDirty retireRecreateDirty,
      lawProperty EpochComposeRenameIntoRetirement renameIntoRetirement
    ]

boundaryMismatchRejected :: Property
boundaryMismatchRejected =
  ( composeDelta newerVersion older,
    composeDelta newerUniverse older
  )
    === ( Left (ComposeVersionMismatch (versionFromKey 1) (versionFromKey 2)),
          Left ComposeUniverseMismatch
        )
  where
    older = identityDelta (Endpoint (versionFromKey 1) (IntSet.singleton 0)) :: EpochDelta (IntMap.IntMap Int) IntSet.IntSet
    newerVersion = identityDelta (Endpoint (versionFromKey 2) (IntSet.singleton 0))
    newerUniverse = identityDelta (Endpoint (versionFromKey 1) (IntSet.singleton 1))

composeIdentityLeft ::
  (EpochKeyed keyMap observed, Eq observed, Eq keyMap, Show observed, Show keyMap, Show (SetKey observed)) =>
  EpochDeltaCase keyMap observed ->
  Property
composeIdentityLeft epochCase =
  composeDelta deltaValue (identityDelta (sourceEndpointOf deltaValue)) === Right deltaValue
  where
    deltaValue = edcDelta epochCase

composeIdentityRight ::
  (EpochKeyed keyMap observed, Eq observed, Eq keyMap, Show observed, Show keyMap, Show (SetKey observed)) =>
  EpochDeltaCase keyMap observed ->
  Property
composeIdentityRight epochCase =
  composeDelta (identityDelta (targetEndpointOf deltaValue)) deltaValue === Right deltaValue
  where
    deltaValue = edcDelta epochCase

composeAssociative ::
  (EpochKeyed keyMap observed, Eq observed, Eq keyMap, Show observed, Show keyMap, Show (SetKey observed)) =>
  EpochChainCase keyMap observed ->
  Property
composeAssociative chain =
  leftAssociated === rightAssociated
  where
    older = eccFirst chain
    middle = eccSecond chain
    newer = eccThird chain
    leftAssociated =
      composeDelta middle older >>= composeDelta newer
    rightAssociated =
      composeDelta newer middle >>= (\newerMiddle -> composeDelta newerMiddle older)

composeAgreesWithReference :: EpochPairCase (IntMap.IntMap Int) IntSet.IntSet -> Property
composeAgreesWithReference pairCase =
  fmap referenceFromDelta (composeDelta newer older)
    === referenceCompose (referenceFromDelta newer) (referenceFromDelta older)
  where
    older = epcFirst pairCase
    newer = epcSecond pairCase

composeDirtyTargetSequential :: EpochPairCase (IntMap.IntMap Int) IntSet.IntSet -> Property
composeDirtyTargetSequential pairCase =
  case (composeDelta newer older, referenceCompose (referenceFromDelta newer) (referenceFromDelta older)) of
    (Right composed, Right referenceComposed) ->
      changedKeysAcrossEpoch composed === referenceChangedKeys referenceComposed
    (Left err, _) ->
      counterexample ("boundary-compatible composition failed: " <> show err) False
    (_, Left err) ->
      counterexample ("boundary-compatible reference failed: " <> show err) False
  where
    older = epcFirst pairCase
    newer = epcSecond pairCase

retireRecreateDirty :: Property
retireRecreateDirty =
  case (epochDelta source middle IntMap.empty (IntSet.singleton 0) IntSet.empty, epochDelta middle target IntMap.empty IntSet.empty IntSet.empty) of
    (Right older, Right newer) ->
      fmap changedKeysAcrossEpoch (composeDelta newer older)
        === Right (IntSet.singleton 0)
    fixtures ->
      counterexample ("valid retire/recreate fixtures rejected: " <> show fixtures) False
  where
    source = Endpoint (versionFromKey 0) (IntSet.singleton 0)
    middle = Endpoint (versionFromKey 1) IntSet.empty
    target = Endpoint (versionFromKey 2) (IntSet.singleton 0)

renameIntoRetirement :: Property
renameIntoRetirement =
  case (epochDelta source middle (IntMap.singleton 0 1) IntSet.empty IntSet.empty, epochDelta middle target IntMap.empty (IntSet.singleton 1) IntSet.empty) of
    (Right older, Right newer) ->
      case composeDelta newer older of
        Left err ->
          counterexample ("rename into retirement refused composition: " <> show err) False
        Right composed ->
          (retiredKeys composed, transportKeys composed (IntSet.singleton 0))
            === ( IntSet.singleton 0,
                  Transport IntMap.empty (IntSet.singleton 0) IntSet.empty
                )
    fixtures ->
      counterexample ("valid rename/retire fixtures rejected: " <> show fixtures) False
  where
    source = Endpoint (versionFromKey 0) (IntSet.singleton 0)
    middle = Endpoint (versionFromKey 1) (IntSet.singleton 1)
    target = Endpoint (versionFromKey 2) IntSet.empty
