{-# LANGUAGE BangPatterns #-}

module Moonlight.Triangulation.Internal.Location
  ( MutableLocation (..)
  , locateMutable
  , locateMutableWithin
  , locateLineMutable
  , visibleOuterEdge
  ) where

import Control.Monad.ST (ST)
import Data.Bits ((.&.))
import qualified Data.IntSet as IntSet
import Data.STRef (readSTRef, writeSTRef)
import Moonlight.Triangulation.Internal.DcelOperations.Twin (reverseIndex)
import Moonlight.Triangulation.Internal.FaceProbe
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  , maxCounter
  )
import Moonlight.Triangulation.Internal.HandleDefs
  ( FaceId (..)
  , UndirectedEdgeId (..)
  )
import Moonlight.Triangulation.Internal.Predicates
import Moonlight.Triangulation.Math
  ( canonicalPoint
  , squaredDistanceWide
  )
import Moonlight.Triangulation.Types

data MutableLocation
  = MutableEmpty
  | MutableOnVertex {-# UNPACK #-} !Int
  | MutableOnEdge {-# UNPACK #-} !Int
  | MutableInFace {-# UNPACK #-} !Int
  | MutableOutsideHull {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

locateMutable
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Maybe Int
  -> Point
  -> ST s (Either BuildError MutableLocation)
locateMutable mutable operation hint rawQuery = do
  locateMutableWithBarrier Nothing mutable operation hint rawQuery

-- | Locate from a checked local face while refusing to walk across the exact
-- interface section. The ordinary locator remains unchanged for unrestricted
-- refinement; the local interpreter supplies only its compact barrier pairs.
locateMutableWithin
  :: IntSet.IntSet
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Maybe Int
  -> Point
  -> ST s (Either BuildError MutableLocation)
locateMutableWithin interfacePairs =
  locateMutableWithBarrier (Just interfacePairs)

locateMutableWithBarrier
  :: Maybe IntSet.IntSet
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Maybe Int
  -> Point
  -> ST s (Either BuildError MutableLocation)
locateMutableWithBarrier barrier mutable operation hint rawQuery = do
  connected <- connectedCount mutable
  faces <- faceCount mutable
  let !query = canonicalPoint rawQuery
  case connected of
    0 -> pure (Right MutableEmpty)
    _ | faces <= 1 -> Right <$> locateLineMutable mutable query
    -- A hint is the caller vouching for adjacency, so the exact walk starts
    -- there directly; only an unhinted locate buys the vertex descent.
    _ | Just face <- hint, face > 0 && face < faces -> walkWithBarrier barrier mutable operation query face
    _ -> do
      start <- chooseStartFace mutable hint
      anchor <- readFaceEdge mutable start
      if anchor < 0
        then walkWithBarrier barrier mutable operation query start
        else do
          origin <- readOrigin mutable anchor
          nearest <- descendToNearest mutable operation query origin
          face <- incidentInnerFace mutable nearest start
          walkWithBarrier barrier mutable operation query face

-- | Greedy first-improvement descent through vertex neighbours: hop to the
-- first neighbour strictly closer to the query until none improves. Distance
-- is a heuristic only — exact containment belongs to 'walkWithBarrier' — so the widened
-- comparison can lengthen the path but never move the located answer.
--
-- Strictly decreasing distance bounds the hops and nothing else: no vertex is
-- entered twice, but the star rotation terminates only if @reverse . previous@
-- closes an orbit, which is a property of the links rather than of the
-- geometry. Disjoint stars over distinct entered vertices spend at most one
-- step per outgoing half-edge, so a budget past 'directedEdgeCount' cannot be
-- exhausted while the links are well formed. Exhausting it means they are not,
-- and yields the vertex in hand rather than an error — a truncated descent can
-- only lengthen 'walkWithBarrier', never move what it finds.
descendToNearest
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Point
  -> Int
  -> ST s Int
descendToNearest mutable operation query start = do
  halfEdges <- directedEdgeCount mutable
  point <- pointAt mutable start
  settle (halfEdges + 1) start (squaredDistanceWide query point)
 where
  settle !budget !vertex !best = do
    first <- readVertexOut mutable vertex
    if first < 0
      then pure vertex
      else rotate budget vertex best first first
  rotate !budget !vertex !best !first !edge
    | budget <= 0 = pure vertex
    | otherwise = do
        neighbour <- readOrigin mutable (reverseIndex edge)
        point <- pointAt mutable neighbour
        let !candidate = squaredDistanceWide query point
        if candidate < best
          then do
            addCounter operation CounterLocationWalkSteps 1
            settle (budget - 1) neighbour candidate
          else do
            previousEdge <- readPrevious mutable edge
            let !outgoing = reverseIndex previousEdge
            if outgoing == first then pure vertex else rotate (budget - 1) vertex best first outgoing

-- | An inner face incident to the vertex, so the exact walk starts adjacent
-- to where the descent settled; the caller's face stands in when the star
-- offers none.
incidentInnerFace
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> ST s Int
incidentInnerFace mutable vertex fallback = do
  first <- readVertexOut mutable vertex
  if first < 0 then pure fallback else rotate first first
 where
  rotate !first !edge = do
    face <- readFace mutable edge
    if face > 0
      then pure face
      else do
        previousEdge <- readPrevious mutable edge
        let !outgoing = reverseIndex previousEdge
        if outgoing == first then pure fallback else rotate first outgoing

locateLineMutable :: MutableDcel s vertex directed undirected face -> Point -> ST s MutableLocation
locateLineMutable mutable rawQuery = do
  let !query = canonicalPoint rawQuery
  vertices <- pointCount mutable
  exactVertex <- findVertex query vertices 0
  case exactVertex of
    Just vertex -> pure (MutableOnVertex vertex)
    Nothing -> do
      halfEdges <- directedEdgeCount mutable
      if halfEdges == 0
        then pure (MutableOutsideHull 0)
        else do
          onEdge <- findSegment query halfEdges 0
          case onEdge of
            Just edge -> pure (MutableOnEdge edge)
            Nothing -> do
              firstConnected <- findConnected vertices 0
              firstOut <- readVertexOut mutable firstConnected
              let edge = if firstOut < 0 then 0 else firstOut
              from <- edgeOriginPoint mutable edge
              to <- edgeOriginPoint mutable (reverseIndex edge)
              case orient2d from to query of
                GT -> pure (MutableOutsideHull (orientOuter edge))
                LT -> pure (MutableOutsideHull (orientOuter (reverseIndex edge)))
                EQ -> MutableOutsideHull <$> nearestTerminalEdge query halfEdges edge
 where
  findVertex !query !limit !index
    | index >= limit = pure Nothing
    | otherwise = do
        connected <- isConnected mutable index
        if not connected
          then findVertex query limit (index + 1)
          else do
            point <- pointAt mutable index
            if point == query then pure (Just index) else findVertex query limit (index + 1)

  findSegment !query !limit !edge
    | edge >= limit = pure Nothing
    | otherwise = do
        let normalized = edge .&. complementOne
        from <- edgeOriginPoint mutable normalized
        to <- edgeOriginPoint mutable (reverseIndex normalized)
        if onClosedSegment from to query
          then pure (Just normalized)
          else findSegment query limit (normalized + 2)

  findConnected !limit !index
    | index >= limit = pure 0
    | otherwise = do
        connected <- isConnected mutable index
        if connected then pure index else findConnected limit (index + 1)

  orientOuter :: Int -> Int
  orientOuter edge = edge

  -- A degenerate chain's outer cycle doubles back exactly at its two terminal
  -- vertices, so @next e == reverse e@ characterises the edge entering a
  -- terminal. Collinear extension must attach to the terminal nearest the
  -- query, which for three or more vertices is not an endpoint of any single
  -- arbitrary edge.
  nearestTerminalEdge !query !limit !fallback = go 0 Nothing
   where
    go !edge !best
      | edge >= limit = pure (maybe fallback fst best)
      | otherwise = do
          edgeNext <- readNext mutable edge
          if edgeNext /= reverseIndex edge
            then go (edge + 1) best
            else do
              let !terminal = reverseIndex edge
              point <- edgeOriginPoint mutable terminal
              let !candidate = squaredDistanceWide query point
              case best of
                Just (_, closest) | closest <= candidate -> go (edge + 1) best
                _ -> go (edge + 1) (Just (terminal, candidate))

  complementOne = -2

walkWithBarrier
  :: Maybe IntSet.IntSet
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Point
  -> Int
  -> ST s (Either BuildError MutableLocation)
walkWithBarrier barrier mutable operation query initialFace = do
  faces <- faceCount mutable
  halfEdges <- directedEdgeCount mutable
  let !budget = max 8 (faces + halfEdges `quot` 2 + 4)
  go budget 0 initialFace
 where
  go !remaining !steps !face
    | remaining <= 0 = do
        addCounter operation CounterLocationFallbacks 1
        maxCounter operation CounterLocationMaxWalk steps
        pure (Left (LocationWalkExhausted query steps))
    | face <= 0 = Right . MutableOutsideHull <$> visibleOuterEdge mutable query
    | otherwise = do
        addCounter operation CounterLocationWalkSteps 1
        let !nextSteps = steps + 1
        probe <- probeFace mutable face query
        case probe of
          MutableInFace found -> do
            writeSTRef (mdLastFace mutable) found
            maxCounter operation CounterLocationMaxWalk nextSteps
            pure (Right probe)
          MutableOnVertex _ -> do
            writeSTRef (mdLastFace mutable) face
            maxCounter operation CounterLocationMaxWalk nextSteps
            pure (Right probe)
          MutableOnEdge _ -> do
            writeSTRef (mdLastFace mutable) face
            maxCounter operation CounterLocationMaxWalk nextSteps
            pure (Right probe)
          MutableOutsideHull edge -> do
            adjacent <- readFace mutable edge
            if adjacent > 0
                && maybe False (IntSet.member (edge `quot` 2)) barrier
              then
                pure
                  ( Left
                      ( RefinementDomainWouldCrossInterface
                          (UndirectedEdgeId (fromIntegral (edge `quot` 2)))
                          (FaceId (fromIntegral face))
                      )
                  )
              else
                if adjacent == 0
                  then do
                    maxCounter operation CounterLocationMaxWalk nextSteps
                    pure (Right (MutableOutsideHull edge))
                  else go (remaining - 1) nextSteps adjacent
          MutableEmpty -> pure (Left (PointLocationFailed query))

probeFace :: MutableDcel s vertex directed undirected face -> Int -> Point -> ST s MutableLocation
-- 'faceEdges' walks the @next@ chain, so the cycle already states each
-- boundary's destination: @destination e0 = origin e1@. Reading the twin's
-- origin instead asks the store for what the face has already said, at twice
-- the endpoint loads and twice the coordinate loads per probe. Three origins
-- and three points settle all three boundaries. Vertex and edge hits still
-- return on the first boundary that reports one, and a later crossing still
-- displaces an earlier one.
probeFace mutable face query = do
  (e0, e1, e2) <- faceEdges mutable face
  a <- readOrigin mutable e0
  b <- readOrigin mutable e1
  c <- readOrigin mutable e2
  pa <- pointAt mutable a
  pb <- pointAt mutable b
  pc <- pointAt mutable c
  let !first = probeBoundary reverseIndex query e0 a pa b pb
  case first of
    BoundaryOnVertex vertex -> pure (MutableOnVertex vertex)
    BoundaryOnEdge boundary -> pure (MutableOnEdge boundary)
    _ -> do
      let !second = probeBoundary reverseIndex query e1 b pb c pc
      case second of
        BoundaryOnVertex vertex -> pure (MutableOnVertex vertex)
        BoundaryOnEdge boundary -> pure (MutableOnEdge boundary)
        _ -> do
          let !third = probeBoundary reverseIndex query e2 c pc a pa
          case third of
            BoundaryOnVertex vertex -> pure (MutableOnVertex vertex)
            BoundaryOnEdge boundary -> pure (MutableOnEdge boundary)
            _ -> pure $ case keep third (keep second (keep first Nothing)) of
              Nothing -> MutableInFace face
              Just boundary -> MutableOutsideHull boundary
 where
  keep :: BoundaryProbe boundary probeVertex -> Maybe boundary -> Maybe boundary
  keep (BoundaryCrossing boundary) _ = Just boundary
  keep _ held = held

chooseStartFace :: MutableDcel s vertex directed undirected face -> Maybe Int -> ST s Int
chooseStartFace mutable hint = do
  faces <- faceCount mutable
  cached <- readSTRef (mdLastFace mutable)
  let candidate = case hint of
        Just face | face > 0 && face < faces -> face
        _ | cached > 0 && cached < faces -> cached
        _ -> 1
  pure candidate

visibleOuterEdge :: MutableDcel s vertex directed undirected face -> Point -> ST s Int
visibleOuterEdge mutable query = do
  start <- readFaceEdge mutable 0
  halfEdges <- directedEdgeCount mutable
  if start < 0 || halfEdges == 0
    then pure 0
    else go (halfEdges + 1) start start Nothing
 where
  go !remaining !start !edge !fallback
    | remaining <= 0 = pure (maybe start id fallback)
    | otherwise = do
        from <- edgeOriginPoint mutable edge
        to <- edgeOriginPoint mutable (reverseIndex edge)
        let !side = orient2d from to query
            !fallback' = if side /= LT then Just edge else fallback
        if side == GT
          then pure edge
          else do
            edgeNext <- readNext mutable edge
            if edgeNext == start then pure (maybe edge id fallback') else go (remaining - 1) start edgeNext fallback'
