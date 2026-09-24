{-# LANGUAGE DataKinds #-}

-- | Bounded monadic folding, refusable transaction sequencing, and the handle
-- and error coercions the constrained layer shares.
module Moonlight.Planar.Internal.Cdt.Combinators
  ( foldWhileM
  , bindMutable
  , asConstraintStep
  ) where

import Moonlight.Planar.Internal.Cdt.Types (CdtError (..))
import Moonlight.Planar.Internal.Types (BuildError)

-- | Monadic left fold whose continuation is supplied by a lazy right fold.
-- The state predicate decides descent before the next effect is constructed,
-- so graph walks stop at their authoritative local answer without mutable loop
-- control or traversing the unused safety suffix.
foldWhileM
  :: (Foldable container, Monad monad)
  => (state -> Bool)
  -> (state -> item -> monad state)
  -> state
  -> container item
  -> monad state
foldWhileM shouldContinue step initial items =
  foldr
    (\item continuation state ->
      if shouldContinue state
        then step state item >>= continuation
        else pure state
    )
    pure
    items
    initial
{-# INLINE foldWhileM #-}

-- | Sequence two refusable transaction steps. Refusal short-circuits, so a
-- transaction that abandons never reaches its publication; writing the bind
-- once is what keeps the constraint verbs from nesting their case analysis
-- five deep.
bindMutable :: Monad monad => monad (Either failure a) -> (a -> monad (Either failure b)) -> monad (Either failure b)
bindMutable step continue = do
  outcome <- step
  case outcome of
    Left failure -> pure (Left failure)
    Right value -> continue value
{-# INLINE bindMutable #-}

-- | Relabel a step whose refusal is a build failure, so it composes with the
-- constraint layer's own.
asConstraintStep :: Functor f => f (Either BuildError a) -> f (Either (CdtError) a)
asConstraintStep = fmap (either (Left . CdtBuildError) Right)
{-# INLINE asConstraintStep #-}
