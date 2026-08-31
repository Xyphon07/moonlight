{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FiniteSpec (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Moonlight.Core
  ( FiniteUniverse (..),
    boundedEnumUniverse,
    finiteUniverseList,
    finiteUniverseSet,
  )
import Moonlight.Core (IsLawName (..), constructorLawName)
import LawProperty (lawProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===))

data FiniteBit
  = FiniteOff
  | FiniteOn
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance FiniteUniverse FiniteBit where
  finiteUniverse =
    boundedEnumUniverse

data FiniteTriad
  = FiniteAlpha
  | FiniteBeta
  | FiniteGamma
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance FiniteUniverse FiniteTriad where
  finiteUniverse =
    FiniteAlpha :| [FiniteBeta, FiniteGamma]

data FiniteLaw
  = FiniteUniverseListExhaustive
  | FiniteUniverseListDuplicateFree
  | FiniteUniverseSetCoherent
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName FiniteLaw where
  lawNameText =
    constructorLawName . show

tests :: TestTree
tests =
  testGroup
    "Finite"
    [ testGroup
        "FiniteBit"
        [ lawProperty FiniteUniverseListExhaustive (propFiniteUniverseListExhaustive @FiniteBit),
          lawProperty FiniteUniverseListDuplicateFree (propFiniteUniverseListDuplicateFree @FiniteBit),
          lawProperty FiniteUniverseSetCoherent (propFiniteUniverseSetCoherent @FiniteBit)
        ],
      testGroup
        "FiniteTriad"
        [ lawProperty FiniteUniverseListExhaustive (propFiniteUniverseListExhaustive @FiniteTriad),
          lawProperty FiniteUniverseListDuplicateFree (propFiniteUniverseListDuplicateFree @FiniteTriad),
          lawProperty FiniteUniverseSetCoherent (propFiniteUniverseSetCoherent @FiniteTriad)
        ]
    ]

propFiniteUniverseListExhaustive ::
  forall value.
  (Bounded value, Enum value, Eq value, FiniteUniverse value, Show value) =>
  Property
propFiniteUniverseListExhaustive =
  finiteUniverseList @value === enumFromTo minBound maxBound

propFiniteUniverseListDuplicateFree ::
  forall value.
  (FiniteUniverse value, Ord value) =>
  Property
propFiniteUniverseListDuplicateFree =
  length (finiteUniverseList @value) === Set.size (finiteUniverseSet @value)

propFiniteUniverseSetCoherent ::
  forall value.
  (FiniteUniverse value, Ord value, Show value) =>
  Property
propFiniteUniverseSetCoherent =
  finiteUniverseSet @value === Set.fromList (finiteUniverseList @value)
