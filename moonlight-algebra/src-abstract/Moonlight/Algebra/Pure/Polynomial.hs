{-# LANGUAGE TypeApplications #-}

-- | Univariate polynomials over a coefficient ring; a 'FreeModule' on monomial
-- degrees.
--
-- Laws: addition forms an additive group with the zero polynomial as identity and
-- scaling acts coefficient-wise (the module laws).
module Moonlight.Algebra.Pure.Polynomial
  ( Polynomial,
    fromCoefficients,
    toCoefficients,
    normalizePolynomial,
    evaluatePolynomial,
    monomial,
  )
where

import Data.Kind (Type)
import Data.List (genericReplicate)
import Numeric.Natural (Natural)
import Moonlight.Algebra.Pure.Module
  ( FreeModule (..),
    BilinearSpace (..),
    Module (..),
    VectorSpace,
  )
import Moonlight.Algebra.Pure.Ring
  ( CommutativeRing,
    Semiring,
  )
import Moonlight.Algebra.Pure.SparseVec (SparseVec)
import qualified Moonlight.Algebra.Pure.SparseVec as SparseVec
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    Field,
    MultiplicativeMonoid (..),
    Ring,
  )

type Polynomial :: Type -> Type
newtype Polynomial r = Polynomial (SparseVec r Natural)
  deriving stock (Eq)

instance (Show r, AdditiveMonoid r) => Show (Polynomial r) where
  show polynomialValue =
    "Polynomial " <> show (toCoefficients polynomialValue)

fromCoefficients :: (Eq r, AdditiveMonoid r) => [r] -> Polynomial r
fromCoefficients =
  fromSparseVec . SparseVec.fromEntries . zip [0 ..]

toCoefficients :: AdditiveMonoid r => Polynomial r -> [r]
toCoefficients = denseCoefficientsFromTerms . SparseVec.toEntries . asSparseVec

normalizePolynomial :: Polynomial r -> Polynomial r
normalizePolynomial = id

evaluatePolynomial :: Ring r => r -> Polynomial r -> r
evaluatePolynomial value =
  foldr
    (\termValue accumulator -> add termValue (mul value accumulator))
    zero
    . toCoefficients

monomial :: (Eq r, AdditiveMonoid r) => Natural -> r -> Polynomial r
monomial degreeValue coefficientValue =
  fromSparseVec (SparseVec.fromEntries [(degreeValue, coefficientValue)])

asSparseVec :: Polynomial r -> SparseVec r Natural
asSparseVec (Polynomial sparsePolynomial) =
  sparsePolynomial

fromSparseVec :: SparseVec r Natural -> Polynomial r
fromSparseVec =
  Polynomial

denseCoefficientsFromTerms :: AdditiveMonoid r => [(Natural, r)] -> [r]
denseCoefficientsFromTerms = go 0
  where
    go :: (Integral degree, AdditiveMonoid coeff) => degree -> [(degree, coeff)] -> [coeff]
    go _ [] = []
    go expectedDegree remainingTerms@((degreeValue, termValue) : restTerms)
      | expectedDegree < degreeValue =
          genericReplicate (degreeValue - expectedDegree) zero
            <> go degreeValue remainingTerms
      | otherwise = termValue : go (expectedDegree + 1) restTerms

multiplyPolynomials :: (Eq r, Semiring r) => Polynomial r -> Polynomial r -> Polynomial r
multiplyPolynomials leftPolynomial rightPolynomial =
  fromSparseVec
    ( SparseVec.fromEntries
        [ (leftDegree + rightDegree, mul leftCoefficient rightCoefficient)
          | (leftDegree, leftCoefficient) <- SparseVec.toEntries (asSparseVec leftPolynomial),
            (rightDegree, rightCoefficient) <- SparseVec.toEntries (asSparseVec rightPolynomial)
        ]
    )

instance (Eq r, AdditiveMonoid r) => AdditiveMonoid (Polynomial r) where
  zero = fromSparseVec zero
  add left right =
    fromSparseVec (add (asSparseVec left) (asSparseVec right))

instance (Eq r, AdditiveGroup r) => AdditiveGroup (Polynomial r) where
  neg =
    fromSparseVec . neg . asSparseVec
  sub left right = add left (neg right)

instance (Eq r, Semiring r) => MultiplicativeMonoid (Polynomial r) where
  one = monomial 0 one
  mul = multiplyPolynomials

instance (Eq r, Ring r) => Ring (Polynomial r)

instance (Eq r, Semiring r) => Semiring (Polynomial r)

instance (Eq r, CommutativeRing r) => CommutativeRing (Polynomial r)

instance (Eq r, Ring r) => Module r (Polynomial r) where
  scale scalar =
    fromSparseVec . scale scalar . asSparseVec

instance (Eq r, Ring r) => FreeModule r (Polynomial r) where
  type Basis r (Polynomial r) = Natural
  support = support @r . asSparseVec
  coefficient degreeValue = SparseVec.lookupEntry degreeValue . asSparseVec
  generator degreeValue = monomial degreeValue one

instance (Eq k, Field k) => VectorSpace k (Polynomial k)

instance (Eq k, Field k) => BilinearSpace k (Polynomial k) where
  bilinearForm leftPolynomial rightPolynomial =
    let rightSparse = asSparseVec rightPolynomial
     in foldr
          add
          zero
          [ mul leftCoefficient (SparseVec.lookupEntry degreeValue rightSparse)
            | (degreeValue, leftCoefficient) <- SparseVec.toEntries (asSparseVec leftPolynomial)
          ]
