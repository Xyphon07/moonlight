module Moonlight.Homology.Pure.Effective
  ( EffectiveHomology,
    sourceComplex,
    reducedComplex,
    reductionWitness,
    finiteBoundary,
    mkEffectiveHomology,
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Boundary.Finite (FiniteChainComplex)
import Moonlight.Homology.Pure.Reductions
  ( Reduction,
    ReductionChecks,
    ReductionValidation,
    ReductionWitness,
    mkReductionWitness,
  )

type EffectiveHomology :: Type -> Type -> Type -> Type -> Type -> Type
data EffectiveHomology large small r largeBasis smallBasis = EffectiveHomology
  { sourceComplex :: large,
    reducedComplex :: small,
    reductionWitness :: ReductionWitness large small r largeBasis smallBasis,
    finiteBoundary :: FiniteChainComplex r
  }

mkEffectiveHomology ::
  large ->
  small ->
  Reduction large small r largeBasis smallBasis ->
  ReductionChecks largeBasis smallBasis r ->
  FiniteChainComplex r ->
  ReductionValidation (EffectiveHomology large small r largeBasis smallBasis)
mkEffectiveHomology source reduced reduction checks finite =
  (\witness -> EffectiveHomology source reduced witness finite) <$> mkReductionWitness reduction checks
