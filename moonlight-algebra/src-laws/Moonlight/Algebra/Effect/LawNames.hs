{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Algebra.Effect.LawNames
  ( LawName (..),
    lawName,
    CommonLawName (..),
    IsLawName (..),
    constructorLawNameWithOverrides,
  )
where

import Data.Kind (Type)
import Moonlight.Core (CommonLawName (..), IsLawName (..), constructorLawNameWithOverrides)

type LawName :: Type
data LawName
  = MonoidAssoc
  | MonoidLeftId
  | MonoidRightId
  | GroupInvLeft
  | GroupInvRight
  | AbelianComm
  | RingAddAssoc
  | RingMulComm
  | CommonLaw CommonLawName
  | ModuleDistribScalarAdd
  | ModuleDistribVectorAdd
  | HeytingImpliesSelfTop
  | HeytingMeetImplication
  | HeytingConsequentMeetImplication
  | HeytingImplicationDistributesMeet
  | HeytingNegDefault
  | HeytingEquivalenceDefault
  | GcdDividesLeft
  | GcdDividesRight
  | ExtGcdBezout
  | ModInverseCorrect
  | ModInverseAbsentNonunit
  | CrtSound
  | UnitInverseOne
  | UnitInverseCorrect
  | UnitInverseAbsent
  | FreeMonoidAssoc
  | FreeMonoidLeftId
  | FreeMonoidRightId
  | PolynomialCanonicalizationIdempotent
  | ZnGeneratorNormalized
  | PolynomialGeneratorCanonical
  | FreeAbelianGeneratorCanonical
  | SparseVecGeneratorCanonical
  | PowerSetGeneratorCanonical
  | LaneVectorSubtraction
  | OrientationGroupInvLeft
  | OrientationGroupInvRight
  | OrientationAbelianComm
  deriving stock (Eq, Ord, Show)

lawName :: LawName -> String
lawName lawNameValue =
  case lawNameValue of
    CommonLaw commonLawName -> lawNameText commonLawName
    specificLawName -> constructorLawNameWithOverrides [("PowerSetGeneratorCanonical", "powerset_generator_canonical")] (show specificLawName)

instance IsLawName LawName where
  lawNameText = lawName
