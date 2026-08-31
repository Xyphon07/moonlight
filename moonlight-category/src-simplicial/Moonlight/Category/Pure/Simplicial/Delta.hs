-- | Runtime-dimensional morphisms in the simplex category Δ.
module Moonlight.Category.Pure.Simplicial.Delta
  ( DeltaOb (..),
    Coface (..),
    Codegeneracy (..),
    DeltaMorphism,
    deltaDomainDimension,
    deltaCodomainDimension,
    deltaMapValues,
    mkDeltaMorphism,
    deltaIdentity,
    composeDeltaMorphism,
    cofaceMorphism,
    codegeneracyMorphism,
    DeltaNormalForm,
    normalDomainDimension,
    normalCodomainDimension,
    normalSurjection,
    normalInjection,
    normalizeDeltaMorphism,
    denormalizeDeltaNormalForm,
    deltaMorphismEqual,
    allDeltaMorphisms,
    deltaToSomeMonotone,
    deltaFromSomeMonotone,
    surjectionDegeneracyIndices,
    injectionMissingIndices,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import GHC.TypeNats (KnownNat, Nat, natVal, type (+))
import Moonlight.Core (safeIndexNatural)
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Simplicial.Delta.Types (DeltaMorphism (..))
import Moonlight.Category.Pure.Simplicial.Ordinal
  ( SomeMonotone (..),
    SomeNormalizedMonotone (..),
    composeSomeMonotone,
    mkSomeMonotone,
    monotoneCodomainDimension,
    monotoneDomainDimension,
    monotoneValues,
    normalizeSomeMonotone,
    normalizedInjectionValues,
    normalizedSurjectionValues,
    someMonotoneEqualByNormalForm,
  )
import Moonlight.Category.Pure.Simplicial.TypeLevel (Fin, finValue)

type DeltaOb :: Nat -> Type
data DeltaOb (n :: Nat) = DeltaOb

type Coface :: Nat -> Type
data Coface (n :: Nat) where
  CofaceMap :: KnownNat n => Fin (n + 2) -> Coface n

type Codegeneracy :: Nat -> Type
data Codegeneracy (n :: Nat) where
  CodegeneracyMap :: KnownNat n => Fin (n + 1) -> Codegeneracy n

mkDeltaMorphism :: Natural -> Natural -> [Natural] -> Maybe DeltaMorphism
mkDeltaMorphism domainDimension codomainDimension mapValues =
  (\_ -> DeltaMorphism domainDimension codomainDimension mapValues)
    <$> mkSomeMonotone domainDimension codomainDimension mapValues

deltaIdentity :: Natural -> DeltaMorphism
deltaIdentity nValue =
  DeltaMorphism
    { deltaDomainDimension = nValue,
      deltaCodomainDimension = nValue,
      deltaMapValues = [0 .. nValue]
    }

composeDeltaMorphism :: DeltaMorphism -> DeltaMorphism -> Maybe DeltaMorphism
composeDeltaMorphism outer inner =
  do
    outerMonotone <- deltaToSomeMonotone outer
    innerMonotone <- deltaToSomeMonotone inner
    composed <- composeSomeMonotone outerMonotone innerMonotone
    pure (deltaFromSomeMonotone composed)

cofaceMorphism :: forall n. Coface n -> DeltaMorphism
cofaceMorphism (CofaceMap skippedIndex) =
  let domainDimension = natVal (Proxy @n)
      codomainDimension = domainDimension + 1
      skippedValue = finValue skippedIndex
      mappedValues =
        [0 .. domainDimension]
          & map (\domainValue -> if domainValue < skippedValue then domainValue else domainValue + 1)
   in DeltaMorphism
        { deltaDomainDimension = domainDimension,
          deltaCodomainDimension = codomainDimension,
          deltaMapValues = mappedValues
        }

codegeneracyMorphism :: forall n. Codegeneracy n -> DeltaMorphism
codegeneracyMorphism (CodegeneracyMap repeatedIndex) =
  let codomainDimension = natVal (Proxy @n)
      domainDimension = codomainDimension + 1
      repeatedValue = finValue repeatedIndex
      mappedValues =
        [0 .. domainDimension]
          & map
            ( \domainValue ->
                if domainValue <= repeatedValue
                  then domainValue
                  else domainValue - 1
            )
   in DeltaMorphism
        { deltaDomainDimension = domainDimension,
          deltaCodomainDimension = codomainDimension,
          deltaMapValues = mappedValues
        }

type DeltaNormalForm :: Type
data DeltaNormalForm = DeltaNormalForm
  { normalDomainDimension :: Natural,
    normalCodomainDimension :: Natural,
    normalSurjection :: [Natural],
    normalInjection :: [Natural]
  }
  deriving stock (Eq, Show)

normalizeDeltaMorphism :: DeltaMorphism -> Maybe DeltaNormalForm
normalizeDeltaMorphism morphism =
  case deltaToSomeMonotone morphism >>= normalizeSomeMonotone of
    Just (SomeNormalizedMonotone _ _ normalized) ->
      Just
        DeltaNormalForm
        { normalDomainDimension = deltaDomainDimension morphism,
          normalCodomainDimension = deltaCodomainDimension morphism,
          normalSurjection = normalizedSurjectionValues normalized,
          normalInjection = normalizedInjectionValues normalized
        }
    Nothing -> Nothing

denormalizeDeltaNormalForm :: DeltaNormalForm -> Maybe DeltaMorphism
denormalizeDeltaNormalForm normalForm = do
  mappedValues <- traverse (`safeIndexNatural` normalInjection normalForm) (normalSurjection normalForm)
  mkDeltaMorphism
    (normalDomainDimension normalForm)
    (normalCodomainDimension normalForm)
    mappedValues

deltaMorphismEqual :: DeltaMorphism -> DeltaMorphism -> Bool
deltaMorphismEqual left right =
  case (deltaToSomeMonotone left, deltaToSomeMonotone right) of
    (Just leftMonotone, Just rightMonotone) -> someMonotoneEqualByNormalForm leftMonotone rightMonotone
    _ -> False

nondecreasingRows :: Natural -> Natural -> Natural -> [[Natural]]
nondecreasingRows lowerBound upperBound rowLength =
  if rowLength == 0
    then [[]]
    else
      [lowerBound .. upperBound]
        & concatMap
          ( \headValue ->
              nondecreasingRows headValue upperBound (rowLength - 1)
                & map (headValue :)
          )

allDeltaMorphisms :: Natural -> Natural -> [DeltaMorphism]
allDeltaMorphisms domainDimension codomainDimension =
  nondecreasingRows 0 codomainDimension (domainDimension + 1)
    & mapMaybe (mkDeltaMorphism domainDimension codomainDimension)

deltaToSomeMonotone :: DeltaMorphism -> Maybe SomeMonotone
deltaToSomeMonotone morphism =
  mkSomeMonotone
    (deltaDomainDimension morphism)
    (deltaCodomainDimension morphism)
    (deltaMapValues morphism)

deltaFromSomeMonotone :: SomeMonotone -> DeltaMorphism
deltaFromSomeMonotone (SomeMonotone _ _ monotone) =
  DeltaMorphism
    { deltaDomainDimension = monotoneDomainDimension monotone,
      deltaCodomainDimension = monotoneCodomainDimension monotone,
      deltaMapValues = monotoneValues monotone
    }

surjectionDegeneracyIndices :: [Natural] -> [Natural]
surjectionDegeneracyIndices surjectionRanks =
  NonEmpty.group surjectionRanks
    & zip [0 ..]
    & foldMap
      ( \(runIndex, rankRun) ->
          rankRun
            & NonEmpty.tail
            & fmap (const runIndex)
      )

injectionMissingIndices :: Natural -> [Natural] -> [Natural]
injectionMissingIndices codomainDimension injectionValues =
  let injectionImage = Set.fromList injectionValues
   in [0 .. codomainDimension]
        & filter (`Set.notMember` injectionImage)
