{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | The batch interpreter: many constraint requests in order inside one sealed
-- dense transaction, with rejection carried as a value.
module Moonlight.Planar.Internal.Cdt.Batch
  ( recoverConstraints
  , recoverConstraintBatch
  , finalizeConstraintBatch
  , initialConstraintBatchStats
  , interpretConstraintRequest
  , interpretConstraintRequests
  ) where

import Control.Monad.ST (ST)
import Data.Either (isRight)
import qualified Data.Vector as V
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Internal.Cdt.Combinators (foldWhileM)
import Moonlight.Planar.Internal.Cdt.Query (validateEndpoints)
import Moonlight.Planar.Internal.Cdt.Recovery (applyMutableConstraint)
import Moonlight.Planar.Internal.Cdt.Types
import Moonlight.Planar.Internal.Growable
  ( GrowableWord32
  , newGrowableWord32
  )
import Moonlight.Planar.Internal.Mutable
import Moonlight.Planar.Internal.OperationState
  ( OperationState )
import Moonlight.Planar.Internal.Paged (TransactionShape (DenseTransaction))
import Moonlight.Planar.Internal.Representation
import Moonlight.Planar.Internal.Transaction (runTransaction)
import Moonlight.Planar.Internal.Types (ConstraintMode (..))

-- | Interpret constraint requests in order inside one sealed mutable DCEL
-- transaction. Rejections are values because an earlier accepted request can
-- lawfully obstruct a later request; structural recovery failures remain typed
-- errors and prevent a partially rewritten mesh from escaping. The public batch
-- interpreter amortizes dense materialization; known singleton repair sites
-- reuse this algebra under the sparse persistent-page interpretation.
recoverConstraints
  :: Triangulation 'Constrained vertex directed undirected face
  -> V.Vector (VertexId, VertexId)
  -> Either (CdtError) (ConstraintBatchResult vertex directed undirected face)
recoverConstraints triangulation requests
  | V.null requests =
      Right
        ConstraintBatchResult
          { constraintBatchTriangulation = triangulation
          , constraintBatchOutcomes = V.empty
          , constraintBatchStats = initialConstraintBatchStats 0
          }
  | otherwise =
      recoverConstraintBatch
        triangulation
        requests

recoverConstraintBatch
  :: Triangulation 'Constrained vertex directed undirected face
  -> V.Vector (VertexId, VertexId)
  -> Either (CdtError) (ConstraintBatchResult vertex directed undirected face)
recoverConstraintBatch triangulation requests = do
  V.mapM_ (uncurry (validateEndpoints triangulation)) requests
  (completed, frozen, _) <-
    runTransaction
      CdtBuildError
      DenseTransaction
      triangulation
      0
      (interpretConstraintRequests requests)
  pure (finalizeConstraintBatch frozen completed)
{-# INLINE recoverConstraintBatch #-}

-- | Interpret a complete constraint request section against an already-open
-- transaction. Both ordinary recovery and asymmetric constrained extension
-- use this one interpreter; callers decide only which request section is
-- resident before it begins.
interpretConstraintRequests
  :: V.Vector (VertexId, VertexId)
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> ST s (Either (CdtError) ConstraintBatchAccumulator)
interpretConstraintRequests requests mutable operation = do
  programWords <- newGrowableWord32 256
  let initialAccumulator =
        ConstraintBatchAccumulator
          { accumulatedConstraintOutcomes = []
          , accumulatedConstraintStats =
              initialConstraintBatchStats (V.length requests)
          }
  foldWhileM
    isRight
    (interpretConstraintRequest programWords mutable operation)
    (Right initialAccumulator)
    requests
{-# INLINE interpretConstraintRequests #-}

-- | Materialize the ordinary recovery receipt only after the enclosing
-- transaction has frozen. The mutable accumulator cannot escape as a partial
-- constrained mesh.
finalizeConstraintBatch
  :: Triangulation 'Constrained vertex directed undirected face
  -> ConstraintBatchAccumulator
  -> ConstraintBatchResult vertex directed undirected face
finalizeConstraintBatch frozen completed =
  ConstraintBatchResult
    { constraintBatchTriangulation = frozen
    , constraintBatchOutcomes = V.fromList (reverse (accumulatedConstraintOutcomes completed))
    , constraintBatchStats = accumulatedConstraintStats completed
    }
{-# INLINE finalizeConstraintBatch #-}

initialConstraintBatchStats :: Int -> ConstraintBatchStats
initialConstraintBatchStats requestCount =
  ConstraintBatchStats
    { constraintBatchRequests = requestCount
    , constraintBatchAccepted = 0
    , constraintBatchRejected = 0
    , constraintBatchCorridors = 0
    , constraintBatchReusedFaces = 0
    , constraintBatchCrossedEdges = 0
    }

interpretConstraintRequest
  :: GrowableWord32 s
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Either (CdtError) ConstraintBatchAccumulator
  -> (VertexId, VertexId)
  -> ST s (Either (CdtError) ConstraintBatchAccumulator)
interpretConstraintRequest _ _ _ rejected@(Left _) _ = pure rejected
interpretConstraintRequest programWords mutable operation (Right accumulator) (from, to) = do
  applied <- applyMutableConstraint programWords mutable operation from to
  pure $ case applied of
    Left obstruction -> Left obstruction
    Right (MutableConstraintRejected blocking) ->
      let previousStats = accumulatedConstraintStats accumulator
       in Right
            accumulator
              { accumulatedConstraintOutcomes =
                  ConstraintRejected blocking : accumulatedConstraintOutcomes accumulator
              , accumulatedConstraintStats =
                  previousStats
                    { constraintBatchRejected = constraintBatchRejected previousStats + 1
                    }
              }
    Right (MutableConstraintAccepted request) ->
      let previousStats = accumulatedConstraintStats accumulator
       in Right
            accumulator
              { accumulatedConstraintOutcomes =
                  ConstraintAccepted
                    (V.fromList (reverse (accumulatedRequestPath request)))
                    (accumulatedRequestAddedEdges request)
                    : accumulatedConstraintOutcomes accumulator
              , accumulatedConstraintStats =
                  previousStats
                    { constraintBatchAccepted = constraintBatchAccepted previousStats + 1
                    , constraintBatchCorridors =
                        constraintBatchCorridors previousStats + accumulatedRequestCorridors request
                    , constraintBatchReusedFaces =
                        constraintBatchReusedFaces previousStats + accumulatedRequestReusedFaces request
                    , constraintBatchCrossedEdges =
                        constraintBatchCrossedEdges previousStats + accumulatedRequestCrossedEdges request
                    }
              }
