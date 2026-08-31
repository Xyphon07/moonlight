-- | Admissibility of a requested vertex count against the packed index limit.
module Moonlight.Triangulation.Internal.Capacity
  ( ensureCapacity
  ) where

import Moonlight.Triangulation.Internal.PackedIndex (indexLimit)
import Moonlight.Triangulation.Internal.Types (BuildError (..))

ensureCapacity :: Int -> Either BuildError ()
ensureCapacity count
  | count < 0 || count > maximumVertexCapacity = Left (CapacityExceeded count)
  | otherwise = Right ()

-- Mutable allocation reserves @8n + 16@ directed-edge slots. Every topology
-- handle is packed below 'indexLimit', so admissibility is stated against that
-- actual representation bound rather than the machine 'Int' bound.
maximumVertexCapacity :: Int
maximumVertexCapacity = (indexLimit - 16) `quot` 8
