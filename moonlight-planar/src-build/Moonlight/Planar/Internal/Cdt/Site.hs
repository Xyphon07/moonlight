{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Siting of constraint endpoints: resolving a point to an existing handle,
-- and materializing one inside an open transaction when it has none.
module Moonlight.Planar.Internal.Cdt.Site
  ( lookupExistingConstraintEndpoint
  , placeConstraintEndpoint
  ) where

import Control.Monad.ST (ST)
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Insertion (insertVertexAtPoint)
import Moonlight.Planar.Internal.Mutable
import Moonlight.Planar.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  )
import Moonlight.Planar.Internal.PointIndex (lookupPointIndex)
import Moonlight.Planar.Internal.Probe (Probe (..))
import Moonlight.Planar.Internal.Representation
import Moonlight.Planar.Internal.Types
import Moonlight.Planar.BuildStats (BuildMetric (InputPoints, UniquePoints, ExistingPoints, DuplicatePoints))
import Moonlight.Planar.Point (Point (..))

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
      addCounter operation (Reported InputPoints) 1
      outcome <- insertVertexAtPoint @'ProbeOff mutable operation hint point payload
      case outcome of
        Left failure -> pure (Left failure)
        Right (vertex, disposition) -> do
          case disposition of
            AlreadyPresent -> do
              writeVertexData mutable vertex payload
              addCounter operation (Reported ExistingPoints) 1
              addCounter operation (Reported DuplicatePoints) 1
            Inserted -> addCounter operation (Reported UniquePoints) 1
          pure (Right (VertexId (fromIntegral vertex)))
