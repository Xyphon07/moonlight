-- | The two-element sign/orientation type 'Orientation', with the magma
-- structure that composes signs.
--
-- Laws: forms the two-element group ℤ/2 — @Positive@ is the identity and
-- @Negative@ is its own inverse.
module Moonlight.Algebra.Pure.Orientation
  ( Orientation (..),
    flipOrientation,
  )
where

import Data.Kind (Type)
import Moonlight.Algebra.Pure.Group
  ( AbelianGroup,
    Group (..),
  )
import Prelude
  ( Eq,
    Monoid (..),
    Ord,
    Read,
    Semigroup (..),
    Show,
    id,
  )

type Orientation :: Type
data Orientation
  = Positive
  | Negative
  deriving stock (Eq, Ord, Show, Read)

flipOrientation :: Orientation -> Orientation
flipOrientation Positive = Negative
flipOrientation Negative = Positive

instance Semigroup Orientation where
  Positive <> other = other
  Negative <> Positive = Negative
  Negative <> Negative = Positive

instance Monoid Orientation where
  mempty = Positive

instance Group Orientation where
  groupInverse = id

instance AbelianGroup Orientation
