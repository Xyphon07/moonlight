{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module TransportSpec
  ( transportTests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import EpochSupport.Expected (viewProjection)
import EpochSupport.Generators
import EpochSupport.Reference
import EpochSupport.Types
import LawManifest (lawManifestCase, lawProperty)
import Moonlight.Core (IsLawName (..), OrdMap (..), OrdSet (..), SetKey, constructorLawName)
import Moonlight.Delta.Epoch
import Test.QuickCheck (Gen, Property, counterexample, forAll, (===), (.&&.))
import Test.Tasty (TestTree, testGroup)

data TransportLaw
  = TransportPartitionsQuery
  | TransportTargetsBelongToEndpoint
  | TransportAgreesWithSequentialReference
  | EpochViewTransportSourceVersionGuard
  | EpochViewTransportDropsRetiredKeys
  | EpochViewTransportTargetsValid
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName TransportLaw where
  lawNameText = constructorLawName . show

transportTests :: TestTree
transportTests =
  testGroup
    "transport"
    [ lawManifestCase "epoch transport" ([minBound .. maxBound] :: [TransportLaw]),
      lawProperty TransportPartitionsQuery $
        epochDeltaIntCaseProperty (transportPartitionsQuery intSetGen)
          .&&. epochDeltaGenericCaseProperty (transportPartitionsQuery genericSetGen),
      lawProperty TransportTargetsBelongToEndpoint $
        epochDeltaIntCaseProperty (transportTargetsBelongToEndpoint intSetGen)
          .&&. epochDeltaGenericCaseProperty (transportTargetsBelongToEndpoint genericSetGen),
      lawProperty TransportAgreesWithSequentialReference $
        forAll epochDeltaIntCaseGen transportAgreesWithReference,
      lawProperty EpochViewTransportSourceVersionGuard viewTransportSourceVersionGuard,
      lawProperty EpochViewTransportDropsRetiredKeys viewTransportDropsRetiredKeys,
      lawProperty EpochViewTransportTargetsValid $
        epochDeltaIntCaseProperty (viewTransportTargetsValid intSubsetOf)
          .&&. epochDeltaGenericCaseProperty (viewTransportTargetsValid genericSubsetOf)
    ]

transportPartitionsQuery ::
  (EpochKeyed keyMap observed, Eq observed, Show observed, Show keyMap, Show (SetKey observed)) =>
  Gen observed ->
  EpochDeltaCase keyMap observed ->
  Property
transportPartitionsQuery queryGen epochCase =
  forAll queryGen $ \queryKeys ->
    let result = transportKeys deltaValue queryKeys
        transportedDomain = fromListSet (fmap fst (toAscListMap (transportedKeys result)))
        retired = transportRetiredKeys result
        unknown = transportUnknownKeys result
     in counterexample ("transport result: " <> show result) $
          ( unionsSet [transportedDomain, retired, unknown],
            intersectionSet transportedDomain retired,
            intersectionSet transportedDomain unknown,
            intersectionSet retired unknown
          )
            === (queryKeys, emptySet, emptySet, emptySet)
  where
    deltaValue = edcDelta epochCase

transportTargetsBelongToEndpoint ::
  (EpochKeyed keyMap observed, Eq observed, Show observed, Show keyMap, Show (SetKey observed)) =>
  Gen observed ->
  EpochDeltaCase keyMap observed ->
  Property
transportTargetsBelongToEndpoint queryGen epochCase =
  forAll queryGen $ \queryKeys ->
    let result = transportKeys deltaValue queryKeys
        targetImages = fromListSet (fmap snd (toAscListMap (transportedKeys result)))
     in intersectionSet targetImages (targetKeys deltaValue) === targetImages
  where
    deltaValue = edcDelta epochCase

transportAgreesWithReference :: EpochDeltaCase (IntMap.IntMap Int) IntSet.IntSet -> Property
transportAgreesWithReference epochCase =
  forAll intSetGen $ \queryKeys ->
    transportKeys deltaValue queryKeys
      === referenceTransportKeys (referenceFromInput (edcInput epochCase)) queryKeys
  where
    deltaValue = edcDelta epochCase

viewTransportSourceVersionGuard :: Property
viewTransportSourceVersionGuard =
  case epochDelta source target IntMap.empty IntSet.empty IntSet.empty of
    Left err ->
      counterexample ("valid view fixture rejected: " <> show err) False
    Right deltaValue ->
      transportView deltaValue (viewAt (versionFromKey 0) (IntSet.singleton 1) ())
        === Left (ViewSourceVersionMismatch (versionFromKey 1) (versionFromKey 0))
  where
    source = Endpoint (versionFromKey 1) (IntSet.singleton 1)
    target = Endpoint (versionFromKey 2) (IntSet.singleton 1)

viewTransportDropsRetiredKeys :: Property
viewTransportDropsRetiredKeys =
  case epochDelta source target IntMap.empty (IntSet.singleton 1) IntSet.empty of
    Left err ->
      counterexample ("valid retirement fixture rejected: " <> show err) False
    Right deltaValue ->
      fmap viewProjection (transportView deltaValue (viewAt (versionFromKey 1) (IntSet.singleton 1) ()))
        === Right (versionFromKey 2, IntSet.empty, ())
  where
    source = Endpoint (versionFromKey 1) (IntSet.singleton 1)
    target = Endpoint (versionFromKey 2) IntSet.empty

viewTransportTargetsValid ::
  (EpochKeyed keyMap observed, Eq observed, Show observed, Show keyMap, Show (SetKey observed)) =>
  (observed -> Gen observed) ->
  EpochDeltaCase keyMap observed ->
  Property
viewTransportTargetsValid subsetGen epochCase =
  forAll (subsetGen (sourceKeys deltaValue)) $ \observedKeys ->
    case transportView deltaValue (viewAt (sourceVersion deltaValue) observedKeys ()) of
      Left err ->
        counterexample ("source-contained view transport failed: " <> show err) False
      Right transportedView ->
        intersectionSet (cvObservedKeys transportedView) (targetKeys deltaValue)
          === cvObservedKeys transportedView
  where
    deltaValue = edcDelta epochCase
