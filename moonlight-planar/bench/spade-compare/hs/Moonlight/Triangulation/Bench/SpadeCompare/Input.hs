-- | Cross-language benchmark input programs. Both Haskell interpreters import
-- this owner directly; neither keeps a private copy of the LCG stream.
module Moonlight.Triangulation.Bench.SpadeCompare.Input
  ( randomPoints
  ) where

import Data.Word (Word64)
import Moonlight.Triangulation.Types (Point (..))

-- | The Rust generator, bit for bit.
randomPoints :: Word64 -> Int -> [Point]
randomPoints seed count = take count (go seed)
 where
  go :: Word64 -> [Point]
  go state =
    let state1 = state * 6364136223846793005 + 1442695040888963407
        state2 = state1 * 6364136223846793005 + 1442695040888963407
        unit :: Word64 -> Double
        unit value = fromIntegral (value `div` 2048) / 9007199254740992
     in Point (2 * unit state1 - 1) (2 * unit state2 - 1) : go state2
