module Moonlight.Category.Pure.Simplicial.Set.Internal
  ( GeneratedSSet (..),
    TruncatedNormalizedSSet (..),
    trustedGeneratedSSet,
    trustedGeneratedSSetWithWitness,
    trustedTruncatedNormalizedSSet,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import GHC.TypeNats (KnownNat, type (+))
import Moonlight.Category.Pure.Simplicial.TypeLevel (Dimension (..), Fin, withReifiedFinOffset)
import Numeric.Natural (Natural)

type GeneratedSSet :: Type -> Type
data GeneratedSSet simplex = GeneratedSSet
  { generationBound :: Natural,
    generatedSimplicesByDimension :: Natural -> [simplex],
    generatedFaceMap ::
      forall n.
      KnownNat n =>
      Dimension (n + 1) ->
      Fin (n + 2) ->
      simplex ->
      Maybe simplex,
    generatedDegeneracyMap ::
      forall n.
      KnownNat n =>
      Dimension n ->
      Fin (n + 1) ->
      simplex ->
      Maybe simplex,
    generatedDegenerateWitness :: Natural -> simplex -> Bool
  }

type TruncatedNormalizedSSet :: Type -> Type
data TruncatedNormalizedSSet simplex = TruncatedNormalizedSSet
  { truncationBound :: Natural,
    nondegenerateSimplicesByDimension :: Map Natural [simplex],
    faceMap ::
      forall n.
      KnownNat n =>
      Dimension (n + 1) ->
      Fin (n + 2) ->
      simplex ->
      Maybe simplex,
    degeneracyMap ::
      forall n.
      KnownNat n =>
      Dimension n ->
      Fin (n + 1) ->
      simplex ->
      Maybe simplex
  }

applyDegeneracyFunctionAtDimension ::
  (forall n. KnownNat n => Dimension n -> Fin (n + 1) -> simplex -> Maybe simplex) ->
  Natural ->
  Natural ->
  simplex ->
  Maybe simplex
applyDegeneracyFunctionAtDimension degeneracyFunction simplexDimension degeneracyIndex simplexValue
  | degeneracyIndex > simplexDimension = Nothing
  | otherwise = withReifiedFinOffset @1 simplexDimension degeneracyIndex $ \dimensionWitness finiteIndex ->
      degeneracyFunction dimensionWitness finiteIndex simplexValue

derivedDegenerateWitness ::
  Eq simplex =>
  Natural ->
  (Natural -> [simplex]) ->
  (forall n. KnownNat n => Dimension n -> Fin (n + 1) -> simplex -> Maybe simplex) ->
  Natural ->
  simplex ->
  Bool
derivedDegenerateWitness upperBound simplicesFunction degeneracyFunction simplexDimension simplexValue
  | simplexDimension == 0 = False
  | simplexDimension > upperBound = False
  | otherwise =
      let sourceDimension = simplexDimension - 1
       in any
            ( \sourceSimplex ->
                any
                  ( \degeneracyIndex ->
                      applyDegeneracyFunctionAtDimension degeneracyFunction sourceDimension degeneracyIndex sourceSimplex
                        == Just simplexValue
                  )
                  [0 .. sourceDimension]
            )
            (simplicesFunction sourceDimension)

trustedGeneratedSSet ::
  Eq simplex =>
  Natural ->
  (Natural -> [simplex]) ->
  (forall n. KnownNat n => Dimension (n + 1) -> Fin (n + 2) -> simplex -> Maybe simplex) ->
  (forall n. KnownNat n => Dimension n -> Fin (n + 1) -> simplex -> Maybe simplex) ->
  GeneratedSSet simplex
trustedGeneratedSSet upperBound simplicesFunction faceFunction degeneracyFunction =
  trustedGeneratedSSetWithWitness
    upperBound
    simplicesFunction
    faceFunction
    degeneracyFunction
    (derivedDegenerateWitness upperBound simplicesFunction degeneracyFunction)

trustedGeneratedSSetWithWitness ::
  Natural ->
  (Natural -> [simplex]) ->
  (forall n. KnownNat n => Dimension (n + 1) -> Fin (n + 2) -> simplex -> Maybe simplex) ->
  (forall n. KnownNat n => Dimension n -> Fin (n + 1) -> simplex -> Maybe simplex) ->
  (Natural -> simplex -> Bool) ->
  GeneratedSSet simplex
trustedGeneratedSSetWithWitness upperBound simplicesFunction faceFunction degeneracyFunction degenerateWitness =
  GeneratedSSet
    { generationBound = upperBound,
      generatedSimplicesByDimension =
        \dimensionValue ->
          if dimensionValue <= upperBound
            then simplicesFunction dimensionValue
            else [],
      generatedFaceMap = faceFunction,
      generatedDegeneracyMap = degeneracyFunction,
      generatedDegenerateWitness = degenerateWitness
    }

trustedTruncatedNormalizedSSet ::
  Natural ->
  Map Natural [simplex] ->
  (forall n. KnownNat n => Dimension (n + 1) -> Fin (n + 2) -> simplex -> Maybe simplex) ->
  (forall n. KnownNat n => Dimension n -> Fin (n + 1) -> simplex -> Maybe simplex) ->
  TruncatedNormalizedSSet simplex
trustedTruncatedNormalizedSSet upperBound levelMap faceFunction degeneracyFunction =
  TruncatedNormalizedSSet
    { truncationBound = upperBound,
      nondegenerateSimplicesByDimension = levelMap,
      faceMap = faceFunction,
      degeneracyMap = degeneracyFunction
    }
