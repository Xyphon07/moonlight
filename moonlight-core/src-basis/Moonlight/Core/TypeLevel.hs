{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoStarIsType #-}

module Moonlight.Core.TypeLevel
  ( type (+),
    type (*),
    type (<=),
    SNat (..),
  )
where

import Data.Kind (Type)
import GHC.TypeNats (KnownNat, Nat, type (*), type (+), type (<=))

type SNat :: Nat -> Type
data SNat (n :: Nat) where
  SNat :: (KnownNat n) => SNat n
