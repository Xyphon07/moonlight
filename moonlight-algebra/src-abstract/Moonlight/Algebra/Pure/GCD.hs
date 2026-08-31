
-- | GCD, extended GCD, modular inverse and CRT over Euclidean domains.
--
-- Laws: 'extGcd' yields a Bézout identity @a*x + b*y = gcd a b@; 'modInverse'
-- inverts a unit modulo the given modulus when one exists.
module Moonlight.Algebra.Pure.GCD
  ( NonZeroModulus,
    withNonZeroModulus,
    withNonZeroModulusValue,
    CanonicalResidue,
    CrtMod,
    mkCanonicalResidue,
    withCanonicalResidue,
    gcd,
    extGcd,
    modInverse,
    crt,
  )
where

import Prelude hiding (gcd)
import Data.Kind (Type)
import Moonlight.Algebra.Pure.Ring
  ( CanonicalEuclideanDomain (..),
    EuclideanDomain (..),
    GCDDomain (..),
    IntegralDomain (..),
    mkNonZeroDivisor,
  )
import Moonlight.Algebra.Unsafe.GCDWitness
  ( CanonicalResidue (..),
    NonZeroModulus,
    canonicalResidueModulus,
    canonicalResidueValue,
    mkNonZeroInternal,
    nonZeroValue,
    retagNonZero,
  )
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MultiplicativeMonoid (..),
  )

withNonZeroModulus ::
  IntegralDomain a =>
  a ->
  (forall modulus. NonZeroModulus modulus a -> r) ->
  Maybe r
withNonZeroModulus value use =
  use <$> mkNonZeroInternal isZero value

withNonZeroModulusValue :: NonZeroModulus modulus a -> (a -> r) -> r
withNonZeroModulusValue modulus use = use (nonZeroValue modulus)

mkCanonicalResidue :: CanonicalEuclideanDomain a => NonZeroModulus modulus a -> a -> CanonicalResidue modulus a
mkCanonicalResidue modulus value =
  CanonicalResidue modulus (normalize modulus value)

withCanonicalResidue ::
  CanonicalResidue modulus a ->
  (NonZeroModulus modulus a -> a -> r) ->
  r
withCanonicalResidue residue use =
  use (canonicalResidueModulus residue) (canonicalResidueValue residue)

gcd :: GCDDomain a => a -> a -> a
gcd = gcdDomain

extGcd :: GCDDomain a => a -> a -> (a, a, a)
extGcd = extendedGcdDomain

modInverse :: CanonicalEuclideanDomain a => a -> NonZeroModulus modulus a -> Maybe a
modInverse value modulus = do
  let (gcdValue, inverseCandidate, _) = extGcd value (nonZeroValue modulus)
  gcdInv <- unitInverse gcdValue
  Just (normalize modulus (mul inverseCandidate gcdInv))

type CrtMod :: Type -> Type -> Type
data CrtMod left right

crt ::
  forall left right a.
  CanonicalEuclideanDomain a =>
  CanonicalResidue left a ->
  CanonicalResidue right a ->
  Maybe (CanonicalResidue (CrtMod left right) a)
crt left right = do
  divisorRefined <- mkNonZeroDivisor divisor
  let (diffQuotient, diffRemainder) =
        divideWithRemainder (sub rightResidue leftResidue) divisorRefined
      (reducedRightModulusValue, reducedRightRemainder) =
        divideWithRemainder rightModulusValue divisorRefined
  if diffRemainder /= zero || reducedRightRemainder /= zero
    then Nothing
    else do
      reducedRightModulus <- (mkNonZeroInternal isZero reducedRightModulusValue :: Maybe (NonZeroModulus right a))
      let offset = normalize reducedRightModulus (mul diffQuotient leftCoefficient)
          candidate = add leftResidue (mul leftModulusValue offset)
          combinedModulusValue = mul leftModulusValue reducedRightModulusValue
      combinedModulus <- (mkNonZeroInternal isZero combinedModulusValue :: Maybe (NonZeroModulus (CrtMod left right) a))
      pure (mkCanonicalResidue combinedModulus candidate)
  where
    leftResidue = canonicalResidueValue left
    rightResidue = canonicalResidueValue right
    leftModulus = canonicalResidueModulus left
    rightModulus = canonicalResidueModulus right
    leftModulusValue = nonZeroValue leftModulus
    rightModulusValue = nonZeroValue rightModulus
    (divisor, leftCoefficient, _) = extGcd leftModulusValue rightModulusValue

normalize :: CanonicalEuclideanDomain a => NonZeroModulus modulus a -> a -> a
normalize modulus value =
  canonicalRemainder value (retagNonZero modulus)
