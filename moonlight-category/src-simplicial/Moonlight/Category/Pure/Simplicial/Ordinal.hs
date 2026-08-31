
-- | Monotone maps between finite ordinals with runtime-validated construction.
module Moonlight.Category.Pure.Simplicial.Ordinal
  ( Monotone,
    SomeMonotone (..),
    mkMonotone,
    mkSomeMonotone,
    monotoneValues,
    monotoneDomainDimension,
    monotoneCodomainDimension,
    monotoneIdentity,
    applyMonotoneAt,
    composeMonotone,
    composeSomeMonotone,
    MonotoneInjection,
    mkMonotoneInjection,
    monotoneInjectionValues,
    MonotoneSurjection,
    mkMonotoneSurjection,
    monotoneSurjectionValues,
    NormalizedMonotone,
    SomeNormalizedMonotone (..),
    normalizeMonotone,
    normalizeSomeMonotone,
    denormalizeNormalizedMonotone,
    denormalizeSomeNormalizedMonotone,
    normalizedSurjectionValues,
    normalizedInjectionValues,
    monotoneEqualByNormalForm,
    someMonotoneEqualByNormalForm,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Moonlight.Core (safeIndexNatural)
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Simplicial.TypeLevel (Dimension (..))

type Monotone :: Nat -> Nat -> Type
data Monotone (m :: Nat) (n :: Nat) where
  Monotone :: (KnownNat m, KnownNat n) => [Natural] -> Monotone m n

type SomeMonotone :: Type
data SomeMonotone where
  SomeMonotone :: (KnownNat m, KnownNat n) => Dimension m -> Dimension n -> Monotone m n -> SomeMonotone

instance Eq (Monotone m n) where
  left == right = monotoneValues left == monotoneValues right

instance Show (Monotone m n) where
  show monotone =
    "Monotone(domain="
      <> show (monotoneDomainDimension monotone)
      <> ", codomain="
      <> show (monotoneCodomainDimension monotone)
      <> ", values="
      <> show (monotoneValues monotone)
      <> ")"

monotoneValues :: Monotone m n -> [Natural]
monotoneValues (Monotone values) = values

monotoneDomainDimension :: forall m n. Monotone m n -> Natural
monotoneDomainDimension (Monotone _) = natVal (Proxy @m)

monotoneCodomainDimension :: forall m n. Monotone m n -> Natural
monotoneCodomainDimension (Monotone _) = natVal (Proxy @n)

hasExpectedLength :: Natural -> [a] -> Bool
hasExpectedLength domainDimension values =
  fromIntegral (length values) == domainDimension + 1

nondecreasing :: [Natural] -> Bool
nondecreasing [] = False
nondecreasing values = and (zipWith (<=) values (drop 1 values))

mkMonotone :: forall m n. (KnownNat m, KnownNat n) => [Natural] -> Maybe (Monotone m n)
mkMonotone values =
  let domainDimension = natVal (Proxy @m)
      codomainDimension = natVal (Proxy @n)
   in if hasExpectedLength domainDimension values
        && all (<= codomainDimension) values
        && nondecreasing values
        then Just (Monotone values)
        else Nothing

mkSomeMonotone :: Natural -> Natural -> [Natural] -> Maybe SomeMonotone
mkSomeMonotone domainDimension codomainDimension values =
  case (someNatVal domainDimension, someNatVal codomainDimension) of
    (SomeNat (_ :: Proxy m), SomeNat (_ :: Proxy n)) ->
      SomeMonotone (Dimension @m) (Dimension @n) <$> mkMonotone @m @n values

monotoneIdentity :: forall n. KnownNat n => Monotone n n
monotoneIdentity =
  let dimensionValue = natVal (Proxy @n)
   in Monotone [0 .. dimensionValue]

applyMonotoneAt :: Monotone m n -> Natural -> Maybe Natural
applyMonotoneAt monotone indexValue =
  safeIndexNatural indexValue (monotoneValues monotone)

composeMonotone :: Monotone n p -> Monotone m n -> Maybe (Monotone m p)
composeMonotone (Monotone outerValues) (Monotone innerValues) =
  traverse (`safeIndexNatural` outerValues) innerValues
    >>= mkMonotone

composeSomeMonotone :: SomeMonotone -> SomeMonotone -> Maybe SomeMonotone
composeSomeMonotone (SomeMonotone _ _ outer) (SomeMonotone _ _ inner) =
  if monotoneDomainDimension outer == monotoneCodomainDimension inner
    then do
      composedValues <- traverse (`safeIndexNatural` monotoneValues outer) (monotoneValues inner)
      mkSomeMonotone
        (monotoneDomainDimension inner)
        (monotoneCodomainDimension outer)
        composedValues
    else Nothing

type MonotoneInjection :: Nat -> Nat -> Type
newtype MonotoneInjection (k :: Nat) (n :: Nat) = MonotoneInjection
  { unMonotoneInjection :: Monotone k n
  }

instance Show (MonotoneInjection k n) where
  show injection = "MonotoneInjection(" <> show (monotoneInjectionValues injection) <> ")"

type MonotoneSurjection :: Nat -> Nat -> Type
newtype MonotoneSurjection (m :: Nat) (k :: Nat) = MonotoneSurjection
  { unMonotoneSurjection :: Monotone m k
  }

instance Show (MonotoneSurjection m k) where
  show surjection = "MonotoneSurjection(" <> show (monotoneSurjectionValues surjection) <> ")"

strictlyIncreasing :: [Natural] -> Bool
strictlyIncreasing [] = False
strictlyIncreasing values = and (zipWith (<) values (drop 1 values))

mkMonotoneInjection :: Monotone k n -> Maybe (MonotoneInjection k n)
mkMonotoneInjection monotone =
  if strictlyIncreasing (monotoneValues monotone)
    then Just (MonotoneInjection monotone)
    else Nothing

monotoneInjectionValues :: MonotoneInjection k n -> [Natural]
monotoneInjectionValues = monotoneValues . unMonotoneInjection

coversAllCodomain :: Natural -> [Natural] -> Bool
coversAllCodomain codomainDimension values =
  fmap NonEmpty.head (NonEmpty.group values) == [0 .. codomainDimension]

mkMonotoneSurjection :: Monotone m k -> Maybe (MonotoneSurjection m k)
mkMonotoneSurjection monotone =
  if coversAllCodomain (monotoneCodomainDimension monotone) (monotoneValues monotone)
    then Just (MonotoneSurjection monotone)
    else Nothing

monotoneSurjectionValues :: MonotoneSurjection m k -> [Natural]
monotoneSurjectionValues = monotoneValues . unMonotoneSurjection

type NormalizedMonotone :: Nat -> Nat -> Type
data NormalizedMonotone (m :: Nat) (n :: Nat) where
  NormalizedMonotone :: KnownNat k => Dimension k -> MonotoneSurjection m k -> MonotoneInjection k n -> NormalizedMonotone m n

type SomeNormalizedMonotone :: Type
data SomeNormalizedMonotone where
  SomeNormalizedMonotone :: (KnownNat m, KnownNat n) => Dimension m -> Dimension n -> NormalizedMonotone m n -> SomeNormalizedMonotone

instance Show (NormalizedMonotone m n) where
  show normalized =
    "NormalizedMonotone(surjection="
      <> show (normalizedSurjectionValues normalized)
      <> ", injection="
      <> show (normalizedInjectionValues normalized)
      <> ")"

normalizedSurjectionValues :: NormalizedMonotone m n -> [Natural]
normalizedSurjectionValues (NormalizedMonotone _ surjection _) =
  monotoneSurjectionValues surjection

normalizedInjectionValues :: NormalizedMonotone m n -> [Natural]
normalizedInjectionValues (NormalizedMonotone _ _ injection) =
  monotoneInjectionValues injection

normalizeMonotone :: forall m n. Monotone m n -> Maybe (NormalizedMonotone m n)
normalizeMonotone (Monotone values) =
  let rankedValueGroups = zip [0 ..] (NonEmpty.group values)
      imageValues = fmap (NonEmpty.head . snd) rankedValueGroups
      surjectionValues =
        rankedValueGroups
          & foldMap (\(imageRank, valueGroup) -> fmap (const imageRank) (NonEmpty.toList valueGroup))
      middleDimension = fromIntegral (length imageValues) - 1
   in case someNatVal middleDimension of
        SomeNat (_ :: Proxy k) -> do
          surjection <- mkMonotone @m @k surjectionValues >>= mkMonotoneSurjection
          injection <- mkMonotone @k @n imageValues >>= mkMonotoneInjection
          pure (NormalizedMonotone (Dimension @k) surjection injection)

normalizeSomeMonotone :: SomeMonotone -> Maybe SomeNormalizedMonotone
normalizeSomeMonotone (SomeMonotone domainDimension codomainDimension monotone) =
  SomeNormalizedMonotone domainDimension codomainDimension <$> normalizeMonotone monotone

denormalizeNormalizedMonotone :: NormalizedMonotone m n -> Maybe (Monotone m n)
denormalizeNormalizedMonotone (NormalizedMonotone _ surjection injection) =
  composeMonotone (unMonotoneInjection injection) (unMonotoneSurjection surjection)

denormalizeSomeNormalizedMonotone :: SomeNormalizedMonotone -> Maybe SomeMonotone
denormalizeSomeNormalizedMonotone (SomeNormalizedMonotone domainDimension codomainDimension normalized) =
  SomeMonotone domainDimension codomainDimension <$> denormalizeNormalizedMonotone normalized

monotoneEqualByNormalForm :: Monotone m n -> Monotone m n -> Bool
monotoneEqualByNormalForm left right =
  fromMaybe False $ do
    leftNormalized <- normalizeMonotone left
    rightNormalized <- normalizeMonotone right
    pure $ normalizedSurjectionValues leftNormalized == normalizedSurjectionValues rightNormalized
      && normalizedInjectionValues leftNormalized == normalizedInjectionValues rightNormalized

someMonotoneEqualByNormalForm :: SomeMonotone -> SomeMonotone -> Bool
someMonotoneEqualByNormalForm left right
  | monotoneDomainDimensionFromSome left /= monotoneDomainDimensionFromSome right = False
  | monotoneCodomainDimensionFromSome left /= monotoneCodomainDimensionFromSome right = False
  | otherwise = fromMaybe False $ do
      SomeNormalizedMonotone _ _ leftNormalized <- normalizeSomeMonotone left
      SomeNormalizedMonotone _ _ rightNormalized <- normalizeSomeMonotone right
      pure $ normalizedSurjectionValues leftNormalized == normalizedSurjectionValues rightNormalized
        && normalizedInjectionValues leftNormalized == normalizedInjectionValues rightNormalized

monotoneDomainDimensionFromSome :: SomeMonotone -> Natural
monotoneDomainDimensionFromSome (SomeMonotone _ _ monotone) =
  monotoneDomainDimension monotone

monotoneCodomainDimensionFromSome :: SomeMonotone -> Natural
monotoneCodomainDimensionFromSome (SomeMonotone _ _ monotone) =
  monotoneCodomainDimension monotone
