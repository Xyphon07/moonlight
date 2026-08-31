{-# LANGUAGE DerivingStrategies #-}

module ViewSpec
  ( viewTests,
  )
where

import Data.IntSet (IntSet)
import Moonlight.Core
  ( IsLawName (..),
    constructorLawName,
  )
import Moonlight.Delta.Epoch
import EpochSupport.Generators
import EpochSupport.Mapping
import EpochSupport.Types
import LawManifest
  ( lawManifestCase,
    lawProperty,
  )
import Test.QuickCheck
  ( Property,
    forAll,
    (===),
    (.&&.),
  )
import Test.Tasty (TestTree, testGroup)

data EpochViewLaw
  = EpochViewMapKeysIdentity
  | EpochViewMapKeysComposition
  | EpochViewCurrentIffVersionEqual
  | EpochViewStaleComplement
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName EpochViewLaw where
  lawNameText =
    constructorLawName . show

viewTests :: TestTree
viewTests =
  testGroup
    "view"
    [ lawManifestCase "epoch view" ([minBound .. maxBound] :: [EpochViewLaw]),
      lawProperty EpochViewMapKeysIdentity $
        viewIntProperty viewMapKeysIdentityInt
          .&&. viewGenericProperty viewMapKeysIdentityGeneric,
      lawProperty EpochViewMapKeysComposition $
        viewIntProperty viewMapKeysCompositionInt
          .&&. viewGenericProperty viewMapKeysCompositionGeneric,
      lawProperty EpochViewCurrentIffVersionEqual $
        forAll epochVersionPairIntViewGen viewCurrentIffVersionEqual
          .&&. forAll epochVersionPairGenericViewGen viewCurrentIffVersionEqual,
      lawProperty EpochViewStaleComplement $
        forAll epochVersionPairIntViewGen viewStaleComplement
          .&&. forAll epochVersionPairGenericViewGen viewStaleComplement
    ]
viewMapKeysIdentityInt :: ContextView IntSet Int -> Property
viewMapKeysIdentityInt contextView =
  mapViewInt identityInt contextView === contextView

viewMapKeysIdentityGeneric :: ContextView GenericSet Int -> Property
viewMapKeysIdentityGeneric contextView =
  mapViewGeneric identityGenericKey contextView === contextView

viewMapKeysCompositionInt :: ContextView IntSet Int -> Property
viewMapKeysCompositionInt contextView =
  mapViewInt (incrementInt . doubleInt) contextView
    === mapViewInt incrementInt (mapViewInt doubleInt contextView)

viewMapKeysCompositionGeneric :: ContextView GenericSet Int -> Property
viewMapKeysCompositionGeneric contextView =
  mapViewGeneric (genericIncrement . genericDouble) contextView
    === mapViewGeneric genericIncrement (mapViewGeneric genericDouble contextView)

viewCurrentIffVersionEqual ::
  (Version, ContextView observed Int) ->
  Property
viewCurrentIffVersionEqual (epochVersion, contextView) =
  contextViewIsCurrent epochVersion contextView
    === (epochVersion == cvVersion contextView)

viewStaleComplement ::
  (Version, ContextView observed Int) ->
  Property
viewStaleComplement (epochVersion, contextView) =
  contextViewIsStale epochVersion contextView
    === not (contextViewIsCurrent epochVersion contextView)
