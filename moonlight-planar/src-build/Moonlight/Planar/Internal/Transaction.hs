{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- | The hidden publication boundary shared by persistent build-side edits.
--
-- The rank-two action can observe one mutable section, but neither the section
-- nor any site witness can escape it.  This is deliberately below the public
-- Session surface: a caller may compose public verbs, while an owner that has
-- just derived private evidence can interpret it without making that evidence
-- forgeable.
module Moonlight.Planar.Internal.Transaction
  ( runTransaction
  , runTransactionWithPublication
  , runUnmeasuredTransaction
  ) where

import Control.Monad.ST (ST, runST)
import Moonlight.Planar.Dcel (numVertices)
import Moonlight.Planar.Internal.Capacity (ensureCapacity)
import Moonlight.Planar.Internal.Mutable
  ( MutableDcel
  , freezeTriangulation
  , freezeTriangulationWithStats
  , halfEdgeCapacity
  , thawTriangulation
  , thawTriangulationDense
  )
import Moonlight.Planar.Internal.OperationState
  ( OperationState
  , finalizeBuildStats
  , newOperationState
  )
import Moonlight.Planar.Internal.Paged
  ( PublicationStats
  , TransactionShape (..)
  , emptyPublicationStats
  )
import Moonlight.Planar.Internal.Representation (Triangulation)
import Moonlight.Planar.Internal.Types (BuildError)
import Moonlight.Planar.BuildStats (BuildStats)

-- | Reserve, thaw, interpret, and publish one transaction.
--
-- The physical section is selected by the operation that knows its edit
-- volume.  Refusal short-circuits before freezing, so a partially rewritten
-- mutable mesh cannot escape as a published triangulation.
runTransaction
  :: (BuildError -> failure)
  -> TransactionShape
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either failure result))
  -> Either failure (result, Triangulation mode vertex directed undirected face, BuildStats)
runTransaction = runTransactionWithReceipt finalizeBuildStats
{-# INLINE runTransaction #-}

-- | The measured publication boundary.  The storage owner returns its page
-- counters alongside the immutable mesh; callers must not infer locality from
-- a final page count or from a benchmark timer.
runTransactionWithPublication
  :: (BuildError -> failure)
  -> TransactionShape
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either failure result))
  -> Either failure (result, Triangulation mode vertex directed undirected face, BuildStats, PublicationStats)
runTransactionWithPublication = runTransactionWithPublicationReceipt finalizeBuildStats
{-# INLINE runTransactionWithPublication #-}

-- | Publish a transaction whose caller observes no instrumentation. Avoiding
-- the statistics fold matters for singleton constraint verbs: their public
-- result has no statistics field, so reading every counter would be dead work.
runUnmeasuredTransaction
  :: (BuildError -> failure)
  -> TransactionShape
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either failure result))
  -> Either failure (result, Triangulation mode vertex directed undirected face)
runUnmeasuredTransaction mapBuildFailure shape triangulation additional action =
  fmap
    (\(result, frozen, ()) -> (result, frozen))
    ( runTransactionWithReceipt
        (const (pure ()))
        mapBuildFailure
        shape
        triangulation
        additional
        action
    )
{-# INLINE runUnmeasuredTransaction #-}

runTransactionWithReceipt
  :: (forall s. OperationState s -> ST s receipt)
  -> (BuildError -> failure)
  -> TransactionShape
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either failure result))
  -> Either failure (result, Triangulation mode vertex directed undirected face, receipt)
runTransactionWithReceipt freezeReceipt mapBuildFailure shape triangulation additional action = do
  fmap
    (\(value, frozen, receipt, _publicationStats) -> (value, frozen, receipt))
    (runTransactionCore freezeWithoutPublicationStats freezeReceipt mapBuildFailure shape triangulation additional action)
{-# INLINE runTransactionWithReceipt #-}

runTransactionWithPublicationReceipt
  :: (forall s. OperationState s -> ST s receipt)
  -> (BuildError -> failure)
  -> TransactionShape
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either failure result))
  -> Either failure (result, Triangulation mode vertex directed undirected face, receipt, PublicationStats)
runTransactionWithPublicationReceipt freezeReceipt mapBuildFailure shape triangulation additional action = do
  runTransactionCore freezeTriangulationWithStats freezeReceipt mapBuildFailure shape triangulation additional action
{-# INLINE runTransactionWithPublicationReceipt #-}

runTransactionCore
  :: (forall s. MutableDcel s vertex directed undirected face -> ST s (Either BuildError (Triangulation mode vertex directed undirected face, PublicationStats)))
  -> (forall s. OperationState s -> ST s receipt)
  -> (BuildError -> failure)
  -> TransactionShape
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. MutableDcel s vertex directed undirected face -> OperationState s -> ST s (Either failure result))
  -> Either failure (result, Triangulation mode vertex directed undirected face, receipt, PublicationStats)
runTransactionCore freezePublication freezeReceipt mapBuildFailure shape triangulation additional action = do
  let !currentCapacity = numVertices triangulation
      !requestedAdditional = max 0 additional
      !capacity
        | requestedAdditional > maxBound - currentCapacity = maxBound
        | otherwise = currentCapacity + requestedAdditional
  case ensureCapacity capacity of
    Left failure -> Left (mapBuildFailure failure)
    Right () -> pure ()
  runST $ do
    mutable <-
      case shape of
        DenseTransaction -> thawTriangulationDense capacity triangulation
        LocalTransaction -> thawTriangulation capacity triangulation
    operation <- newOperationState (halfEdgeCapacity mutable)
    outcome <- action mutable operation
    case outcome of
      Left refusal -> pure (Left refusal)
      Right value -> do
        frozenOutcome <- freezePublication mutable
        case frozenOutcome of
          Left obstruction -> pure (Left (mapBuildFailure obstruction))
          Right (frozen, publicationStats) -> do
            receipt <- freezeReceipt operation
            pure (Right (value, frozen, receipt, publicationStats))
{-# INLINE runTransactionCore #-}

freezeWithoutPublicationStats
  :: MutableDcel s vertex directed undirected face
  -> ST s (Either BuildError (Triangulation mode vertex directed undirected face, PublicationStats))
freezeWithoutPublicationStats mutable =
  fmap (fmap (\frozen -> (frozen, emptyPublicationStats))) (freezeTriangulation mutable)
