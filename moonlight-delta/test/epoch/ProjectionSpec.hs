{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ProjectionSpec
  ( projectionTests,
  )
where

import Data.IntSet (IntSet)
import Moonlight.Core
  ( IsLawName (..),
    OrdSet (..),
    constructorLawName,
  )
import Moonlight.Delta.Epoch
import EpochSupport.Generators
import EpochSupport.Mapping
import EpochSupport.Types
import Moonlight.Delta.Normalize
import Moonlight.Delta.Support
import LawManifest
  ( lawManifestCase,
    lawProperty,
  )
import Test.QuickCheck
  ( Property,
    (===),
    (.&&.),
  )
import Test.Tasty (TestTree, testGroup)

data EpochProjectionLaw
  = EpochProjectionUnionAssociative
  | EpochProjectionUnionCommutative
  | EpochProjectionUnionIdempotent
  | EpochProjectionEmptyUnit
  | EpochProjectionMapIdentity
  | EpochProjectionMapComposition
  | EpochProjectionMapSemilatticeHomomorphism
  | EpochProjectionNullIffComponentsEmpty
  | EpochProjectionNormalizeCanonicalCarrier
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName EpochProjectionLaw where
  lawNameText =
    constructorLawName . show

projectionTests :: TestTree
projectionTests =
  testGroup
    "projection"
    [ lawManifestCase "epoch projection" ([minBound .. maxBound] :: [EpochProjectionLaw]),
      lawProperty EpochProjectionUnionAssociative $
        projectionIntTripleProperty projectionUnionAssociative
          .&&. projectionGenericTripleProperty projectionUnionAssociative,
      lawProperty EpochProjectionUnionCommutative $
        projectionIntPairProperty projectionUnionCommutative
          .&&. projectionGenericPairProperty projectionUnionCommutative,
      lawProperty EpochProjectionUnionIdempotent $
        projectionIntProperty projectionUnionIdempotent
          .&&. projectionGenericProperty projectionUnionIdempotent,
      lawProperty EpochProjectionEmptyUnit $
        projectionIntProperty projectionEmptyUnit
          .&&. projectionGenericProperty projectionEmptyUnit,
      lawProperty EpochProjectionMapIdentity $
        projectionIntProperty projectionMapIdentityInt
          .&&. projectionGenericProperty projectionMapIdentityGeneric,
      lawProperty EpochProjectionMapComposition $
        projectionIntProperty projectionMapCompositionInt
          .&&. projectionGenericProperty projectionMapCompositionGeneric,
      lawProperty EpochProjectionMapSemilatticeHomomorphism $
        projectionIntPairProperty projectionMapSemilatticeHomomorphismInt
          .&&. projectionGenericPairProperty projectionMapSemilatticeHomomorphismGeneric,
      lawProperty EpochProjectionNullIffComponentsEmpty $
        projectionIntProperty projectionNullIffComponentsEmpty
          .&&. projectionGenericProperty projectionNullIffComponentsEmpty,
      lawProperty EpochProjectionNormalizeCanonicalCarrier $
        projectionIntProperty projectionNormalizeCanonicalCarrier
          .&&. projectionGenericProperty projectionNormalizeCanonicalCarrier
    ]
projectionUnionAssociative ::
  (Eq observed, Show observed, OrdSet observed) =>
  (ContextProjectionDelta observed, ContextProjectionDelta observed, ContextProjectionDelta observed) ->
  Property
projectionUnionAssociative (left, middle, right) =
  left <> (middle <> right) === (left <> middle) <> right

projectionUnionCommutative ::
  (Eq observed, Show observed, OrdSet observed) =>
  (ContextProjectionDelta observed, ContextProjectionDelta observed) ->
  Property
projectionUnionCommutative (left, right) =
  left <> right === right <> left

projectionUnionIdempotent ::
  (Eq observed, Show observed, OrdSet observed) =>
  ContextProjectionDelta observed ->
  Property
projectionUnionIdempotent deltaValue =
  deltaValue <> deltaValue === deltaValue

projectionEmptyUnit ::
  (Eq observed, Show observed, OrdSet observed) =>
  ContextProjectionDelta observed ->
  Property
projectionEmptyUnit deltaValue =
  (emptyContextProjectionDelta <> deltaValue, deltaValue <> emptyContextProjectionDelta)
    === (deltaValue, deltaValue)

projectionMapIdentityInt :: ContextProjectionDelta IntSet -> Property
projectionMapIdentityInt deltaValue =
  mapProjectionInt identityInt deltaValue === deltaValue

projectionMapIdentityGeneric :: ContextProjectionDelta GenericSet -> Property
projectionMapIdentityGeneric deltaValue =
  mapProjectionGeneric identityGenericKey deltaValue === deltaValue

projectionMapCompositionInt :: ContextProjectionDelta IntSet -> Property
projectionMapCompositionInt deltaValue =
  mapProjectionInt (incrementInt . doubleInt) deltaValue
    === mapProjectionInt incrementInt (mapProjectionInt doubleInt deltaValue)

projectionMapCompositionGeneric :: ContextProjectionDelta GenericSet -> Property
projectionMapCompositionGeneric deltaValue =
  mapProjectionGeneric (genericIncrement . genericDouble) deltaValue
    === mapProjectionGeneric genericIncrement (mapProjectionGeneric genericDouble deltaValue)

projectionMapSemilatticeHomomorphismInt ::
  (ContextProjectionDelta IntSet, ContextProjectionDelta IntSet) ->
  Property
projectionMapSemilatticeHomomorphismInt (left, right) =
  mapProjectionInt incrementInt (left <> right)
    === mapProjectionInt incrementInt left <> mapProjectionInt incrementInt right

projectionMapSemilatticeHomomorphismGeneric ::
  (ContextProjectionDelta GenericSet, ContextProjectionDelta GenericSet) ->
  Property
projectionMapSemilatticeHomomorphismGeneric (left, right) =
  mapProjectionGeneric genericIncrement (left <> right)
    === mapProjectionGeneric genericIncrement left <> mapProjectionGeneric genericIncrement right

projectionNullIffComponentsEmpty ::
  (Eq observed, Show observed, OrdSet observed) =>
  ContextProjectionDelta observed ->
  Property
projectionNullIffComponentsEmpty deltaValue =
  nullContextProjectionDelta deltaValue
    === (nullSet (dirtyBaseKeys deltaValue) && nullSet (dirtyResultKeys deltaValue))

projectionNormalizeCanonicalCarrier ::
  ( Eq observed,
    Show observed,
    OrdSet observed,
    Eq (ContextProjectionDelta observed),
    Show (ContextProjectionDelta observed)
  ) =>
  ContextProjectionDelta observed ->
  Property
projectionNormalizeCanonicalCarrier deltaValue =
  ( normalizeContextProjectionDelta deltaValue,
    normalizeDelta deltaValue,
    deltaSupport (normalizeDelta deltaValue),
    deltaNull (normalizeDelta deltaValue)
  )
    === ( deltaValue,
          deltaValue,
          deltaSupport deltaValue,
          deltaNull deltaValue
        )
