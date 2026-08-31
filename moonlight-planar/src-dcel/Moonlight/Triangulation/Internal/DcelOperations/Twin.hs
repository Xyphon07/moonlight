{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The pairing involution on half-edge indices.
module Moonlight.Triangulation.Internal.DcelOperations.Twin
  ( reverseIndex
  ) where

import Data.Bits (xor)

reverseIndex :: Int -> Int
reverseIndex edge = edge `xor` 1
{-# INLINE reverseIndex #-}
