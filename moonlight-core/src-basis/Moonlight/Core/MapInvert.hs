-- | Transpose a set-valued map: every @(k, v)@ membership pair of @Map k (Set v)@ reappears as @(v, k)@.
module Moonlight.Core.MapInvert
  ( invertMapOfSets,
  )
where

import Control.Monad ((<=<))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude (Ord, fmap, (.))

invertMapOfSets ::
  (Ord k, Ord v) =>
  Map k (Set v) ->
  Map v (Set k)
invertMapOfSets =
  Map.fromListWith Set.union
    . (invertEntry <=< Map.toList)
  where
    invertEntry :: (sourceKey, Set targetValue) -> [(targetValue, Set sourceKey)]
    invertEntry (k, vs) =
      fmap (, Set.singleton k) (Set.toList vs)
