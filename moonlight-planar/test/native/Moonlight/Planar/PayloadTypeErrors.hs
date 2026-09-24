{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Only the deliberate ill-typed clients defer errors, never the test driver.
module Moonlight.Planar.PayloadTypeErrors (forgeRequiredFill, coerceRequiredFill) where

import Data.Coerce (coerce)
import Moonlight.Planar.Internal.BoxedPaged
  ( BoxedPaged, BoxedFill (..), FillRequirement (..), emptyBoxedPaged )

forgeRequiredFill :: BoxedPaged 'RequiredFill Int
forgeRequiredFill = emptyBoxedPaged NoFill

coerceRequiredFill :: BoxedPaged 'OptionalFill Int -> BoxedPaged 'RequiredFill Int
coerceRequiredFill = coerce
