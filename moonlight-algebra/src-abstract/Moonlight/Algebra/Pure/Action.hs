-- | Monoid actions on a carrier ('Action'), with the group-acting refinement
-- 'InvertibleAction'.
--
-- Laws: @act mempty = id@ and @act (x <> y) = act x . act y@; an invertible
-- action additionally has @act (groupInverse m)@ inverse to @act m@.
module Moonlight.Algebra.Pure.Action
  ( Action (..),
    InvertibleAction,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Algebra.Pure.Group (Group)
import Prelude (Monoid)

type Action :: Type -> Type -> Constraint
class Monoid m => Action m s where
  act :: m -> s -> s

type InvertibleAction :: Type -> Type -> Constraint
class (Group m, Action m s) => InvertibleAction m s
