{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Siting of constraint endpoints: resolving a point to an existing handle,
-- and materializing one inside an open transaction when it has none.
module Moonlight.Triangulation.Internal.Cdt.Site
  ( lookupExistingConstraintEndpoint
  , placeConstraintEndpoint
  ) where

import Control.Monad.ST (ST)
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Insertion (insertVertexAtPoint)
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  )
import Moonlight.Triangulation.Internal.PointIndex (lookupPointIndex)
import Moonlight.Triangulation.Internal.Probe (Probe (..))
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Types
import Moonlight.Triangulation.Math

lookupExistingConstraintEndpoint
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> Maybe VertexId
lookupExistingConstraintEndpoint triangulation point =
  VertexId . fromIntegral
    <$> lookupPointIndex
      (triPointX triangulation)
      (triPointY triangulation)
      (triPointIndex triangulation)
      point

-- | Materialize one point in the open transaction. A payload standing at an
-- occupied position keeps that handle and overwrites the payload, which is what
-- the persistent insertion verb settled on.
--
-- The coordinates are checked here rather than by the callers, because a split
-- point is computed rather than supplied: @lineIntersection@ refuses only a
-- zero denominator, and a denominator merely close to zero answers a coordinate
-- no arena should hold.
placeConstraintEndpoint
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Maybe Int
  -> Point
  -> vertex
  -> ST s (Either BuildError VertexId)
placeConstraintEndpoint mutable operation hint point payload =
  case validatePoint Nothing point of
    Left failure -> pure (Left failure)
    Right _ -> do
      addCounter operation CounterInputPoints 1
      outcome <- insertVertexAtPoint @'ProbeOff mutable operation hint point payload
      case outcome of
        Left failure -> pure (Left failure)
        Right (vertex, disposition) -> do
          case disposition of
            AlreadyPresent -> do
              writeVertexData mutable vertex payload
              addCounter operation CounterExistingPoints 1
              addCounter operation CounterDuplicatePoints 1
            Inserted -> addCounter operation CounterUniquePoints 1
          pure (Right (VertexId (fromIntegral vertex)))
