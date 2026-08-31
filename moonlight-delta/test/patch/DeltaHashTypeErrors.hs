{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module DeltaHashTypeErrors
  ( crossStrategyDigestComparison,
  )
where

import Moonlight.Delta.Patch
  ( MerkleDeltaHashDigest,
    MultisetDeltaHashDigest,
  )
import Prelude

-- Identity and lane re-indexing fixtures no longer exist: the public facade
-- exposes neither raw-lane construction/projection nor a 'Read' path. Their
-- unrepresentability is stronger than a deferred-error witness.
crossStrategyDigestComparison :: MerkleDeltaHashDigest -> MultisetDeltaHashDigest -> Bool
crossStrategyDigestComparison merkleDigest multisetDigest =
  merkleDigest == multisetDigest
