{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Constrained bulk loading: an unconstrained load promoted to the
-- constrained layer, then driven through the batch corridor interpreter.
module Moonlight.Planar.Internal.Cdt.Build
  ( fromDelaunay
  , constrainedDelaunay
  , constrainedDelaunayMaximal
  ) where

import Data.Bifunctor (first)
import qualified Data.Vector as V
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, sizeofPrimArray)
import Data.Word (Word32)
import Moonlight.Planar.BulkLoad (delaunay)
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Internal.Cdt.Batch (recoverConstraints)
import Moonlight.Planar.Internal.Cdt.Types
import Moonlight.Planar.Internal.Representation
import Moonlight.Planar.Internal.Types
import Moonlight.Planar.Point (HasPosition (..))

-- | Promote an unconstrained mesh into the constrained layer with no marked edges.
fromDelaunay
  :: Triangulation 'Unconstrained vertex directed undirected face
  -> Triangulation 'Constrained vertex directed undirected face
fromDelaunay = promoteConstrained

-- | Build a constrained triangulation, refusing the complete request when any
-- input constraint cannot be admitted.
constrainedDelaunay
  :: HasPosition vertex
  => ElementDefaults directed undirected face
  -> V.Vector vertex
  -> V.Vector (Int, Int)
  -> Either (CdtError) (BuildResult 'Constrained vertex directed undirected face)
constrainedDelaunay defaults inputVertices constraints = do
  result <- constrainedDelaunayMaximal defaults inputVertices constraints
  if V.null (cdtRejectedConstraints result)
    then Right (cdtAcceptedBuild result)
    else Left (ConstraintInputConflicts (cdtRejectedConstraints result))

-- | Stable constrained bulk loading. Duplicate input vertices are rerouted to
-- the first surviving handle. Every proper constraint conflict is returned in
-- input order; accepted constraints remain in the result.
constrainedDelaunayMaximal
  :: HasPosition vertex
  => ElementDefaults directed undirected face
  -> V.Vector vertex
  -> V.Vector (Int, Int)
  -> Either (CdtError) (CdtBuildResult vertex directed undirected face)
constrainedDelaunayMaximal defaults inputVertices constraints = do
  built <- first CdtBuildError (delaunay defaults inputVertices)
  let !mapping = buildInputVertices built
      !initial = fromDelaunay (buildTriangulation built)
  requests <- V.mapM (mapConstraintRequest mapping) constraints
  batch <- recoverConstraints initial requests
  let rejected =
        V.mapMaybe
          (\(request, outcome) ->
            case outcome of
              ConstraintAccepted _ _ -> Nothing
              ConstraintRejected _ -> Just request
          )
          (V.zip constraints (constraintBatchOutcomes batch))
  pure
    CdtBuildResult
      { cdtAcceptedBuild =
          BuildResult
            { buildTriangulation = constraintBatchTriangulation batch
            , buildInputVertices = mapping
            , buildStats = buildStats built
            }
      , cdtRejectedConstraints = rejected
      }
 where
  mapConstraintRequest
    :: PrimArray Word32
    -> (Int, Int)
    -> Either (CdtError) (VertexId, VertexId)
  mapConstraintRequest mapping (fromIndex, toIndex) =
    (,)
      <$> mapConstraintEndpoint mapping fromIndex
      <*> mapConstraintEndpoint mapping toIndex

  mapConstraintEndpoint
    :: PrimArray Word32
    -> Int
    -> Either (CdtError) VertexId
  mapConstraintEndpoint mapping endpointIndex
    | endpointIndex >= 0 && endpointIndex < sizeofPrimArray mapping =
        Right (VertexId (indexPrimArray mapping endpointIndex))
    | otherwise =
        Left (ConstraintEndpointIndexOutOfRange endpointIndex (sizeofPrimArray mapping))
