{-# LANGUAGE FunctionalDependencies #-}

-- | The 'IsoNorm' class: a representation isomorphism @wrap@ ↔ @rep@, with
-- 'isoNormalize' canonicalising through it.
module Moonlight.Core.IsoNorm
  ( IsoNorm (..),
    isoNormalize,
  )
where

import Data.Kind (Constraint, Type)
import Prelude ((.))

type IsoNorm :: Type -> Type -> Constraint
class IsoNorm wrap rep | wrap -> rep where
  isoFrom :: rep -> wrap
  isoTo :: wrap -> rep

isoNormalize :: IsoNorm wrap rep => wrap -> wrap
isoNormalize = isoFrom . isoTo
