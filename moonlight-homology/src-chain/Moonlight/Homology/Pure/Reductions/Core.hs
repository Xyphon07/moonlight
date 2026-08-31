module Moonlight.Homology.Pure.Reductions.Core
  ( ChainMap (..),
    ChainHomotopy (..),
    Reduction (..),
    ReductionWitness (..),
    ReductionViolation (..),
    ReductionLawContext (..),
    ReductionValidation,
    ReductionChecks (..),
    mkReductionWitness,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Moonlight.Core (Validation (..))
import Moonlight.Homology.Pure.Failure (HomologyFailure)

type ChainMap :: Type -> Type -> Type -> Type
newtype ChainMap source target r = ChainMap
  { runChainMap :: source -> [(r, target)]
  }

type ChainHomotopy :: Type -> Type -> Type
newtype ChainHomotopy basis r = ChainHomotopy
  { runChainHomotopy :: basis -> [(r, basis)]
  }

type Reduction :: Type -> Type -> Type -> Type -> Type -> Type
data Reduction large small r largeBasis smallBasis = Reduction
  { projection :: ChainMap largeBasis smallBasis r,
    inclusion :: ChainMap smallBasis largeBasis r,
    homotopy :: ChainHomotopy largeBasis r
  }

type ReductionViolation :: Type
data ReductionViolation
  = ProjectionInclusionIdentityViolation HomologyFailure
  | InclusionProjectionHomotopyViolation HomologyFailure
  | ProjectionChainMapViolation HomologyFailure
  | InclusionChainMapViolation HomologyFailure
  deriving stock (Eq, Show)

type ReductionLawContext :: Type -> Type -> Type -> Type
data ReductionLawContext largeBasis smallBasis r = ReductionLawContext
  { sampledLargeBasis :: [largeBasis],
    sampledSmallBasis :: [smallBasis],
    largeBoundary :: largeBasis -> [(r, largeBasis)],
    smallBoundary :: smallBasis -> [(r, smallBasis)]
  }

type ReductionValidation :: Type -> Type
type ReductionValidation a = Validation (NonEmpty ReductionViolation) a

type ReductionChecks :: Type -> Type -> Type -> Type
data ReductionChecks largeBasis smallBasis r = ReductionChecks
  { checkProjectionInclusionIdentity ::
      ChainMap largeBasis smallBasis r ->
      ChainMap smallBasis largeBasis r ->
      Either ReductionViolation (),
    checkInclusionProjectionHomotopy ::
      ChainMap largeBasis smallBasis r ->
      ChainMap smallBasis largeBasis r ->
      ChainHomotopy largeBasis r ->
      Either ReductionViolation (),
    checkProjectionChainMap ::
      ChainMap largeBasis smallBasis r ->
      Either ReductionViolation (),
    checkInclusionChainMap ::
      ChainMap smallBasis largeBasis r ->
      Either ReductionViolation ()
  }

type ReductionWitness :: Type -> Type -> Type -> Type -> Type -> Type
newtype ReductionWitness large small r largeBasis smallBasis = ReductionWitness
  { checkedReduction :: Reduction large small r largeBasis smallBasis
  }

mkReductionWitness ::
  Reduction large small r largeBasis smallBasis ->
  ReductionChecks largeBasis smallBasis r ->
  ReductionValidation (ReductionWitness large small r largeBasis smallBasis)
mkReductionWitness reduction checks =
  (\_ _ _ _ -> ReductionWitness reduction)
    <$> validate (checkProjectionInclusionIdentity checks projectionMap inclusionMap)
    <*> validate (checkInclusionProjectionHomotopy checks projectionMap inclusionMap homotopyMap)
    <*> validate (checkProjectionChainMap checks projectionMap)
    <*> validate (checkInclusionChainMap checks inclusionMap)
  where
    projectionMap = projection reduction
    inclusionMap = inclusion reduction
    homotopyMap = homotopy reduction
    validate :: Either ReductionViolation () -> ReductionValidation ()
    validate checkResult =
      case checkResult of
        Left violation -> Invalid (violation :| [])
        Right () -> Valid ()
