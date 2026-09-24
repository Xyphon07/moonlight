{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Local direction tracing inside the thawed mesh: where a directed line
-- leaves the vertex or edge it currently stands on.
module Moonlight.Planar.Internal.Cdt.Corridor.Trace
  ( MutableVertexOut (..)
  , MutableVertexTrace (..)
  , MutableEdgeOut (..)
  , nextMutableIntersection
  , traceMutableDirectionOutOfVertex
  , advanceMutableVertexTrace
  , traceMutableDirectionOutOfEdge
  , mutableEdgeIntersectsNonCollinear
  , mutableVertexTraceIsSearching
  ) where

import Control.Monad.ST (ST)
import Data.Bits (xor)
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.IntersectionIterator
import Moonlight.Planar.Internal.Cdt.Combinators (foldWhileM)
import Moonlight.Planar.Internal.Mutable
import Moonlight.Planar.Internal.Predicates
  ( onClosedSegment
  , orient2d
  , orient2dCoordinates
  )
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Math (projectionFactor)

data MutableVertexOut
  = MutableVertexOutHull
  | MutableVertexOutOverlap {-# UNPACK #-} !Int
  | MutableVertexOutEdge {-# UNPACK #-} !Int

data MutableVertexTrace
  = MutableVertexTraceSearching {-# UNPACK #-} !Int !Ordering
  | MutableVertexTraceComplete !MutableVertexOut

data MutableEdgeOut
  = MutableEdgeOutHull
  | MutableEdgeOutVertex {-# UNPACK #-} !Int
  | MutableEdgeOutEdge {-# UNPACK #-} !Int
  | MutableEdgeOutNone

nextMutableIntersection
  :: MutableDcel s vertex directed undirected face
  -> Point
  -> Point
  -> Intersection
  -> ST s (Maybe Intersection)
nextMutableIntersection mutable lineFrom lineTo current =
  case current of
    EdgeIntersection directed -> do
      edgeOut <- traceMutableDirectionOutOfEdge mutable (directedEdgeIdIndex directed) lineFrom lineTo
      pure $ case edgeOut of
        MutableEdgeOutHull -> Nothing
        MutableEdgeOutVertex vertex ->
          Just (VertexIntersection (VertexId (fromIntegral vertex)))
        MutableEdgeOutEdge edge ->
          Just (EdgeIntersection (DirectedEdgeId (fromIntegral edge)))
        MutableEdgeOutNone -> Nothing
    VertexIntersection vertex -> do
      currentPoint <- pointAt mutable (vertexIdIndex vertex)
      if currentPoint == lineTo
        then pure Nothing
        else do
          vertexOut <- traceMutableDirectionOutOfVertex mutable (vertexIdIndex vertex) lineTo
          case vertexOut of
            MutableVertexOutHull -> pure Nothing
            MutableVertexOutOverlap edge ->
              pure (Just (EdgeOverlap (DirectedEdgeId (fromIntegral edge))))
            MutableVertexOutEdge edge -> do
              edgeFrom <- edgeOriginPoint mutable edge
              edgeTo <- edgeOriginPoint mutable (edge `xor` 1)
              pure
                ( if orient2d edgeFrom edgeTo lineTo == LT
                    then Nothing
                    else Just (EdgeIntersection (DirectedEdgeId (fromIntegral edge)))
                )
    EdgeOverlap directed
      | lineFrom == lineTo -> pure Nothing
      | otherwise -> do
          destination <- readOrigin mutable (directedEdgeIdIndex directed `xor` 1)
          destinationPoint <- pointAt mutable destination
          pure
            ( if onClosedSegment lineFrom lineTo destinationPoint
                then Just (VertexIntersection (VertexId (fromIntegral destination)))
                else Nothing
            )

traceMutableDirectionOutOfVertex
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> Point
  -> ST s MutableVertexOut
traceMutableDirectionOutOfVertex mutable vertex target = do
  start <- readVertexOut mutable vertex
  if start < 0
    then pure MutableVertexOutHull
    else do
      currentPoint <- pointAt mutable vertex
      startTarget <- edgeOriginPoint mutable (start `xor` 1)
      halfEdges <- directedEdgeCount mutable
      let startSide = orient2d currentPoint startTarget target
          rotateCounterClockwise = startSide == GT
      traced <-
        foldWhileM
          mutableVertexTraceIsSearching
          (advanceMutableVertexTrace mutable currentPoint target rotateCounterClockwise)
          (MutableVertexTraceSearching start startSide)
          [0 .. halfEdges]
      pure $ case traced of
        MutableVertexTraceComplete result -> result
        MutableVertexTraceSearching _ _ -> MutableVertexOutHull

advanceMutableVertexTrace
  :: MutableDcel s vertex directed undirected face
  -> Point
  -> Point
  -> Bool
  -> MutableVertexTrace
  -> Int
  -> ST s MutableVertexTrace
advanceMutableVertexTrace _ _ _ _ complete@(MutableVertexTraceComplete _) _ = pure complete
advanceMutableVertexTrace mutable currentPoint target rotateCounterClockwise (MutableVertexTraceSearching current currentSide) _ = do
  currentTarget <- edgeOriginPoint mutable (current `xor` 1)
  if currentSide == EQ && projectionFactor currentPoint currentTarget target >= 0
    then pure (MutableVertexTraceComplete (MutableVertexOutOverlap current))
    else do
      following <-
        if rotateCounterClockwise
          then (`xor` 1) <$> readPrevious mutable current
          else readNext mutable (current `xor` 1)
      followingTarget <- edgeOriginPoint mutable (following `xor` 1)
      let followingSide = orient2d currentPoint followingTarget target
      if followingSide == EQ && projectionFactor currentPoint followingTarget target >= 0
        then pure (MutableVertexTraceComplete (MutableVertexOutOverlap following))
        else do
          faceBetween <-
            readFace mutable (if rotateCounterClockwise then current else following)
          if faceBetween == 0
            then pure (MutableVertexTraceComplete MutableVertexOutHull)
            else
              if rotateCounterClockwise == (followingSide == LT)
                then do
                  segment <-
                    if rotateCounterClockwise
                      then readNext mutable current
                      else readPrevious mutable (current `xor` 1)
                  pure
                    ( MutableVertexTraceComplete
                        (MutableVertexOutEdge (segment `xor` 1))
                    )
                else pure (MutableVertexTraceSearching following followingSide)

traceMutableDirectionOutOfEdge
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> Point
  -> Point
  -> ST s MutableEdgeOut
traceMutableDirectionOutOfEdge mutable edge lineFrom lineTo = do
  incident <- readFace mutable edge
  if incident == 0
    then pure MutableEdgeOutHull
    else do
      edgePrevious <- readPrevious mutable edge
      edgeNext <- readNext mutable edge
      edgeOrigin <- edgeOriginPoint mutable edge
      oppositeVertex <- edgeOriginPoint mutable edgePrevious
      let !originSide = orient2d lineFrom lineTo edgeOrigin
          !oppositeSide = orient2d lineFrom lineTo oppositeVertex
      if originSide == EQ || oppositeSide == EQ
        then classifyDegenerate edgePrevious edgeNext
        else
          if originSide == oppositeSide
            then do
              nextOrigin <- edgeOriginPoint mutable edgeNext
              let !outgoing = edgeNext `xor` 1
                  !targetSide = orient2d oppositeVertex nextOrigin lineTo
              pure
                ( if targetSide == LT
                    then MutableEdgeOutNone
                    else MutableEdgeOutEdge outgoing
                )
            else do
              let !outgoing = edgePrevious `xor` 1
                  !targetSide = orient2d edgeOrigin oppositeVertex lineTo
              pure
                ( if targetSide == LT
                    then MutableEdgeOutNone
                    else MutableEdgeOutEdge outgoing
                )
 where
  classifyDegenerate edgePrevious edgeNext = do
    previousIntersects <- mutableEdgeIntersectsNonCollinear mutable lineFrom lineTo edgePrevious
    nextIntersects <- mutableEdgeIntersectsNonCollinear mutable lineFrom lineTo edgeNext
    case (previousIntersects, nextIntersects) of
      (True, False) -> pure (MutableEdgeOutEdge (edgePrevious `xor` 1))
      (False, True) -> pure (MutableEdgeOutEdge (edgeNext `xor` 1))
      (True, True) -> MutableEdgeOutVertex <$> readOrigin mutable edgePrevious
      (False, False) -> pure MutableEdgeOutNone

mutableEdgeIntersectsNonCollinear
  :: MutableDcel s vertex directed undirected face
  -> Point
  -> Point
  -> Int
  -> ST s Bool
mutableEdgeIntersectsNonCollinear
  mutable
  (Point lineFromX lineFromY)
  (Point lineToX lineToY)
  edge = do
    edgeFromVertex <- readOrigin mutable edge
    edgeToVertex <- readOrigin mutable (edge `xor` 1)
    Point edgeFromX edgeFromY <- pointAt mutable edgeFromVertex
    Point edgeToX edgeToY <- pointAt mutable edgeToVertex
    let !lineFromSide =
          orient2dCoordinates
            edgeFromX edgeFromY edgeToX edgeToY lineFromX lineFromY
        !lineToSide =
          orient2dCoordinates
            edgeFromX edgeFromY edgeToX edgeToY lineToX lineToY
        !edgeFromSide =
          orient2dCoordinates
            lineFromX lineFromY lineToX lineToY edgeFromX edgeFromY
        !edgeToSide =
          orient2dCoordinates
            lineFromX lineFromY lineToX lineToY edgeToX edgeToY
    pure
      ( lineFromSide /= lineToSide
          && edgeFromSide /= edgeToSide
      )

mutableVertexTraceIsSearching :: MutableVertexTrace -> Bool
mutableVertexTraceIsSearching (MutableVertexTraceSearching _ _) = True
mutableVertexTraceIsSearching _ = False
