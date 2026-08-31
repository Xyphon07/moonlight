{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PatchLaws
  ( PatchChain (..),
    PatchStaleCase (..),
    patchLaws,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import DeltaLaws (deltaNormalizeLaws)
import LawManifest
  ( lawManifestCase,
    lawProperty,
  )
import Moonlight.Core (IsLawName (..), constructorLawName)
import Moonlight.Delta.Patch
import Test.QuickCheck
  ( Gen,
    Property,
    counterexample,
    forAll,
    (===),
  )
import Test.Tasty (TestTree, testGroup)

data PatchChain key value = PatchChain
  { pcKey :: !key,
    pcOldValue :: !(Maybe value),
    pcMiddleValue :: !(Maybe value),
    pcNewValue :: !(Maybe value)
  }
  deriving stock (Eq, Show)

data PatchStaleCase key value = PatchStaleCase
  { pscKey :: !key,
    pscExpectedValue :: !(Maybe value),
    pscActualValue :: !(Maybe value),
    pscReplacementValue :: !(Maybe value)
  }
  deriving stock (Eq, Show)

data PatchLaw
  = PatchCompatibleCompositionStitchesBoundary
  | PatchCompositionApplicationSequential
  | PatchStaleStateRejected
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName PatchLaw where
  lawNameText =
    constructorLawName . show

patchLaws ::
  forall key value.
  (PatchKey key, PatchValue value, Show key, Show value) =>
  String ->
  Gen (Patch key value) ->
  Gen (PatchChain key value) ->
  Gen (PatchStaleCase key value) ->
  TestTree
patchLaws label deltaGen chainGen staleGen =
  testGroup
    label
    [ lawManifestCase label ([minBound .. maxBound] :: [PatchLaw]),
      deltaNormalizeLaws "normalize" deltaGen,
      lawProperty PatchCompatibleCompositionStitchesBoundary $ forAll chainGen patchComposition,
      lawProperty PatchCompositionApplicationSequential $ forAll chainGen patchApplicationComposition,
      lawProperty PatchStaleStateRejected $ forAll staleGen patchStaleRejection
    ]
  where
    patchComposition :: PatchChain key value -> Property
    patchComposition chain =
      compose (patchChainNewer chain) (patchChainOlder chain)
        === Right (patchChainComposed chain)

    patchApplicationComposition :: PatchChain key value -> Property
    patchApplicationComposition chain =
      case compose (patchChainNewer chain) (patchChainOlder chain) of
        Right composed ->
          apply composed (patchChainInitialState chain)
            === ( apply (patchChainOlder chain) (patchChainInitialState chain)
                    >>= apply (patchChainNewer chain)
                )
        Left err ->
          counterexample ("compatible patch chain refused composition: " <> show err) False

    patchStaleRejection :: PatchStaleCase key value -> Property
    patchStaleRejection staleCase =
      apply
        (singleton (pscKey staleCase) (cellFromEndpoints (pscExpectedValue staleCase) (pscReplacementValue staleCase)))
        (patchState (pscKey staleCase) (pscActualValue staleCase))
        === Left
          ApplyBeforeMismatch
            { mismatchKey = pscKey staleCase,
              expectedBefore = pscExpectedValue staleCase,
              actualBefore = pscActualValue staleCase
            }

patchChainOlder :: PatchChain key value -> Patch key value
patchChainOlder chain =
  singleton (pcKey chain) (cellFromEndpoints (pcOldValue chain) (pcMiddleValue chain))

patchChainNewer :: PatchChain key value -> Patch key value
patchChainNewer chain =
  singleton (pcKey chain) (cellFromEndpoints (pcMiddleValue chain) (pcNewValue chain))

patchChainComposed :: PatchChain key value -> Patch key value
patchChainComposed chain =
  singleton (pcKey chain) (cellFromEndpoints (pcOldValue chain) (pcNewValue chain))

patchChainInitialState :: Ord key => PatchChain key value -> Map key value
patchChainInitialState chain =
  patchState (pcKey chain) (pcOldValue chain)

patchState :: Ord key => key -> Maybe value -> Map key value
patchState key =
  maybe Map.empty (Map.singleton key)
