-- | Stateful traversal combinators: map-accumulate, monadic fold and monadic
-- unfold.
module Moonlight.Core.Scan
  ( scanMap,
    scanFoldM,
    unfoldM,
  )
where

import Control.Monad (Monad, foldM)
import Data.Foldable (Foldable)
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import Data.Traversable (Traversable, mapAccumL)
import Prelude (Applicative (pure))

scanMap :: Traversable container => (state -> input -> (state, output)) -> state -> container input -> (state, container output)
scanMap = mapAccumL

scanFoldM :: (Monad effect, Foldable container) => (state -> input -> effect state) -> state -> container input -> effect state
scanFoldM = foldM

unfoldM :: Monad effect => (seed -> effect (Maybe (output, seed))) -> seed -> effect [output]
unfoldM step seed =
  do
    nextStep <- step seed
    case nextStep of
      Nothing ->
        pure []
      Just (output, nextSeed) ->
        (output :) <$> unfoldM step nextSeed
