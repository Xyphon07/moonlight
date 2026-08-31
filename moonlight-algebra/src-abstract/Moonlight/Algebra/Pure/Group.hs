{-# LANGUAGE GHC2024 #-}

-- | Lawful group refinements over the standard 'Semigroup' and 'Monoid'
-- classes, plus operation-selecting wrappers for carriers with more than one
-- legitimate monoidal structure.
--
-- A raw carrier such as 'Integer' admits both additive and multiplicative
-- monoids. Haskell instances are global, so 'Additive' and 'Multiplicative'
-- make the selected operation explicit instead of pretending the carrier has
-- one canonical monoid.
module Moonlight.Algebra.Pure.Group
  ( Additive (..),
    Multiplicative (..),
    Semigroup (..),
    Monoid (..),
    Group (..),
    AbelianGroup,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MultiplicativeMonoid (..),
  )
import Prelude
  ( Eq,
    Monoid (..),
    Num,
    Ord,
    Semigroup (..),
    Show,
  )

type Additive :: Type -> Type
newtype Additive a = Additive {getAdditive :: a}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

type Multiplicative :: Type -> Type
newtype Multiplicative a = Multiplicative {getMultiplicative :: a}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

type Group :: Type -> Constraint
class Monoid group => Group group where
  groupInverse :: group -> group

  groupDifference :: group -> group -> group
  groupDifference left right =
    left <> groupInverse right

type AbelianGroup :: Type -> Constraint
class Group group => AbelianGroup group

instance AdditiveMonoid a => Semigroup (Additive a) where
  Additive left <> Additive right =
    Additive (add left right)

instance AdditiveMonoid a => Monoid (Additive a) where
  mempty =
    Additive zero

instance AdditiveGroup a => Group (Additive a) where
  groupInverse (Additive value) =
    Additive (neg value)

instance AdditiveGroup a => AbelianGroup (Additive a)

instance MultiplicativeMonoid a => Semigroup (Multiplicative a) where
  Multiplicative left <> Multiplicative right =
    Multiplicative (mul left right)

instance MultiplicativeMonoid a => Monoid (Multiplicative a) where
  mempty =
    Multiplicative one
