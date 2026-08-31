{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.Triangulation.Insertion
  ( insertExistingVertex
  , insertExistingVertexWithHint
  , insertPointCombining
  , insertAdmittedPointCombining
  , insertVertexAtPoint
  , insertExistingVertexAtLocation
  ) where

import Control.Monad.ST (ST)
import Moonlight.Triangulation.Internal.HandleDefs (VertexId (..))
import Moonlight.Triangulation.Internal.DcelOperations.Chain
  ( extendLine
  , lineToArea
  , setupFirstVertex
  , setupSecondVertex
  , splitLineEdge
  )
import Moonlight.Triangulation.Internal.DcelOperations.Hull (insertOutsideHull)
import Moonlight.Triangulation.Internal.DcelOperations.Subdivide
  ( insertIntoFace
  , insertOnEdge
  )
import Moonlight.Triangulation.Internal.Location
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  )
import Moonlight.Triangulation.Internal.Probe (KnownProbe, Probe (..))
import Moonlight.Triangulation.Internal.Predicates
import Moonlight.Triangulation.Math (validatePoint)
import Moonlight.Triangulation.Types

-- | Admit a raw site, then delegate to the shared admitted-site interpreter.
-- Geometry decides whether a site is new; callers choose only the annotation
-- law for an occupied site. Keeping raw admission and the common location,
-- placement, and counters in this one owner prevents sessions and constrained
-- extension from drifting into two insertion semantics merely because they own
-- different enclosing transactions.
insertPointCombining
  :: (vertex -> vertex -> vertex)
  -> Maybe Int
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Point
  -> vertex
  -> ST s (Either BuildError (Int, InsertionDisposition))
insertPointCombining combine seed mutable operation point payload =
  case validatePoint Nothing point of
    Left failure -> pure (Left failure)
    Right admitted ->
      insertAdmittedPointCombining combine seed mutable operation admitted payload
{-# INLINE insertPointCombining #-}

-- | Insert an already admitted site. The caller has retained the canonical
-- coordinate section, so this interpreter only locates and glues the payload;
-- it must not reopen coordinate admission.
insertAdmittedPointCombining
  :: (vertex -> vertex -> vertex)
  -> Maybe Int
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> QueryPoint
  -> vertex
  -> ST s (Either BuildError (Int, InsertionDisposition))
insertAdmittedPointCombining combine seed mutable operation queryPoint payload = do
  addCounter operation CounterInputPoints 1
  outcome <-
    insertVertexAtPoint
      @'ProbeOff
      mutable
      operation
      seed
      (queryPointValue queryPoint)
      payload
  case outcome of
    Left failure -> pure (Left failure)
    Right resolved@(vertex, disposition) -> do
      case disposition of
        AlreadyPresent -> do
          resident <- vertexDataAt mutable vertex
          writeVertexData mutable vertex (combine resident payload)
          addCounter operation CounterExistingPoints 1
          addCounter operation CounterDuplicatePoints 1
        Inserted -> addCounter operation CounterUniquePoints 1
      pure (Right resolved)
{-# INLINE insertAdmittedPointCombining #-}

insertExistingVertex
  :: forall p s vertex directed undirected face
   . KnownProbe p
  => MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> ST s (Either BuildError ())
insertExistingVertex mutable operation = insertExistingVertexWithHint @p mutable operation Nothing

-- | Insert a materialized vertex while beginning point location from a face
-- already known to be geometrically adjacent to the request. The hint changes
-- only the amount of walking; the located site remains authoritative.
insertExistingVertexWithHint
  :: forall p s vertex directed undirected face
   . KnownProbe p
  => MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Maybe Int
  -> Int
  -> ST s (Either BuildError ())
insertExistingVertexWithHint mutable operation hint vertex = do
  query <- pointAt mutable vertex
  located <- locateMutable mutable operation hint query
  case located of
    Left obstruction -> pure (Left obstruction)
    Right site -> insertExistingVertexAtLocation @p mutable operation vertex site

-- | Locate before materializing a point, so duplicate detection remains a
-- topological fact rather than a resident coordinate index.
insertVertexAtPoint
  :: forall p s vertex directed undirected face
   . KnownProbe p
  => MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Maybe Int
  -> Point
  -> vertex
  -> ST s (Either BuildError (Int, InsertionDisposition))
insertVertexAtPoint mutable operation hint point vertexData = do
  located <- locateMutable mutable operation hint point
  case located of
    Left obstruction -> pure (Left obstruction)
    Right (MutableOnVertex existing) -> pure (Right (existing, AlreadyPresent))
    Right site -> do
      capacity <- ensurePointCapacity mutable 1
      case capacity of
        Left obstruction -> pure (Left obstruction)
        Right () -> do
          vertex <- appendVertex mutable point vertexData
          inserted <- insertExistingVertexAtLocation @p mutable operation vertex site
          pure ((vertex, Inserted) <$ inserted)

-- | Interpret a point-location result without locating the same point again.
-- Callers may hold this witness only while no topology mutation intervenes.
--
-- The site decides which counts are consulted, and the vertex's own point is
-- read only by the two strata that compare against it — the degenerate line and
-- the failure report. The area strata already stand on a located site and would
-- otherwise rebuild a point the locate stage was handed.
insertExistingVertexAtLocation
  :: forall p s vertex directed undirected face
   . KnownProbe p
  => MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> MutableLocation
  -> ST s (Either BuildError ())
insertExistingVertexAtLocation mutable operation vertex located =
  case located of
    MutableOnVertex existing ->
      pure
        ( Left
            ( FreshInsertionMatchedExistingVertex
                (VertexId (fromIntegral vertex))
                (VertexId (fromIntegral existing))
            )
        )
    MutableEmpty -> do
      connected <- connectedCount mutable
      if connected == 0
        then setupFirstVertex mutable vertex >> pure (Right ())
        else locationFailed
    MutableOnEdge edge -> do
      faces <- faceCount mutable
      if faces <= 1
        then splitLineEdge mutable operation edge vertex
        else insertOnEdge @p mutable operation edge vertex
    MutableInFace face -> do
      faces <- faceCount mutable
      if faces <= 1
        then locationFailed
        else insertIntoFace @p mutable operation face vertex
    MutableOutsideHull edge -> do
      connected <- connectedCount mutable
      if connected == 1
        then setupSecondVertex mutable vertex
        else do
          faces <- faceCount mutable
          if faces <= 1
            then extendDegenerateLine edge
            else insertOutsideHull @p mutable operation edge vertex
 where
  extendDegenerateLine edge = do
    from <- edgeOriginPoint mutable edge
    to <- edgeOriginPoint mutable (edge `xorInt` 1)
    query <- pointAt mutable vertex
    if orient2d from to query == EQ
      then do
        endpoint <- readOrigin mutable edge
        extendLine mutable operation endpoint vertex
      else lineToArea @p mutable operation vertex

  locationFailed = do
    query <- pointAt mutable vertex
    pure (Left (PointLocationFailed query))

xorInt :: Int -> Int -> Int
xorInt value 1 = if even value then value + 1 else value - 1
xorInt value _ = value
{-# INLINE xorInt #-}
