{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wmissing-local-signatures #-}

module AlgebraicSpec
  ( tests,
  )
where

import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..), MultiplicativeMonoid (..), Ring, Semiring)
import Moonlight.Pale.Test.Laws.Algebraic
  ( groupLeftInverse,
    monoidAssociativity,
    ringDistributivityLeft,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

newtype Mod5 = Mod5 Int
  deriving stock (Eq, Show)

instance AdditiveMonoid Mod5 where
  zero :: Mod5
  zero = Mod5 0

  add :: Mod5 -> Mod5 -> Mod5
  add (Mod5 x) (Mod5 y) = normalizeMod5 (x + y)

instance AdditiveGroup Mod5 where
  neg :: Mod5 -> Mod5
  neg (Mod5 x) = normalizeMod5 (negate x)

instance MultiplicativeMonoid Mod5 where
  one :: Mod5
  one = Mod5 1

  mul :: Mod5 -> Mod5 -> Mod5
  mul (Mod5 x) (Mod5 y) = normalizeMod5 (x * y)

instance Semiring Mod5

instance Ring Mod5

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Laws.Algebraic"
    [ testCase "modular addition satisfies monoid associativity" $
        assertBool "expected Z/5Z addition to be associative" $
          allTernary (monoidAssociativity add) mod5Carrier,
      testCase "modular addition satisfies group left inverse" $
        assertBool "expected Z/5Z addition to satisfy left inverse" $
          allUnary (groupLeftInverse add neg zero) mod5Carrier,
      testCase "modular arithmetic satisfies left distributivity" $
        assertBool "expected Z/5Z multiplication to distribute over addition" $
          allTernary ringDistributivityLeft mod5Carrier,
      testCase "associativity rejects a non-associative operation" $
        assertBool "expected subtraction modulo five to fail associativity" $
          not (allTernary (monoidAssociativity nonAssociativeOperation) mod5Carrier)
    ]

mod5Carrier :: [Mod5]
mod5Carrier = fmap Mod5 [0, 1, 2, 3, 4]

normalizeMod5 :: Int -> Mod5
normalizeMod5 value = Mod5 (value `mod` 5)

nonAssociativeOperation :: Mod5 -> Mod5 -> Mod5
nonAssociativeOperation (Mod5 x) (Mod5 y) = normalizeMod5 (x - y)

allUnary :: (a -> Bool) -> [a] -> Bool
allUnary predicate values = all predicate values

allTernary :: (a -> a -> a -> Bool) -> [a] -> Bool
allTernary predicate values = all (applyTernary predicate) ((,,) <$> values <*> values <*> values)

applyTernary :: (a -> a -> a -> Bool) -> (a, a, a) -> Bool
applyTernary predicate (x, y, z) = predicate x y z
