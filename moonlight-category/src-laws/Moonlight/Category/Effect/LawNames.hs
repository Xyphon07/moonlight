{-# LANGUAGE DerivingStrategies #-}

-- | The 'LawName' registry: one constructor per law exercised by the suite.
-- Renderings derive from constructors, so a rename breaks the build.
module Moonlight.Category.Effect.LawNames
  ( LawName (..),
    lawName,
  )
where

import Data.Kind (Type)
import Moonlight.Core (IsLawName (..), constructorLawNameWithOverrides)

type LawName :: Type
data LawName
  = FinCatWellFormed
  | SiteCoverageClosure
  | SiteCategoryIdentity
  | SiteCategoryAssociativity
  | SiteLayerPolicyConformance
  | SiteFreePathWitness
  | SiteQuotientCoherence
  | SiteQuotientIdentity
  | SiteQuotientComposition
  | PathThinCodomainIdentity
  | PathThinCodomainComposition
  | PathQuotientUniqueness
  | PathQuotientFaithful
  | PathQuotientInterpreterCoherence
  | CategoryLeftId
  | CategoryRightId
  | CategoryAssoc
  | GaloisAdjoint
  | GaloisDeflation
  | GaloisInflation
  | GaloisRetraction
  | OrdinalGaloisMonotone
  | ProductProj1
  | ProductProj2
  | CoproductInj1
  | CoproductInj2
  | PullbackCommutes
  | PushoutCommutes
  | AdhesiveWitnessMonicSound
  | PushoutComplementSquareCommutes
  | PushoutComplementUniversal
  | PBPOPullbackSquareCommutes
  | PBPOPushoutSquareCommutes
  | PBPOComplementUniversal
  | EqualizerCommutes
  | CoequalizerCommutes
  | HigherHorizontalBoundary
  | HigherVerticalBoundary
  | HigherInterchange
  deriving stock (Eq, Ord, Show)

instance IsLawName LawName where
  lawNameText = lawName

lawName :: LawName -> String
lawName =
  constructorLawNameWithOverrides [("FinCatWellFormed", "fincat_well_formed"), ("ProductProj1", "limits_product_proj1"), ("ProductProj2", "limits_product_proj2"), ("CoproductInj1", "limits_coproduct_inj1"), ("CoproductInj2", "limits_coproduct_inj2"), ("PullbackCommutes", "limits_pullback_commutes"), ("PushoutCommutes", "limits_pushout_commutes"), ("EqualizerCommutes", "limits_equalizer_commutes"), ("CoequalizerCommutes", "limits_coequalizer_commutes")] . show
