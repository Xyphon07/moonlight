{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O3 -fllvm -optlo-O3 -optlc-O3 #-}

-- | Growth outside the hull: visible ranges, turn closure, and convexity repair.
module Moonlight.Triangulation.Internal.DcelOperations.Hull
  ( ReservedSweepCells
  , SweepCellCursor
  , SweepInsertion (..)
  , reserveSweepCells
  , initialSweepCellCursor
  , commitReservedSweepConnections
  , insertOutsideHull
  , insertOutsideHullAtEdge
  , closeOuterTurn
  , closeOuterTurnReserved
  , fixHullConvexity
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.STRef (writeSTRef)
import Moonlight.Triangulation.Internal.HandleDefs (DirectedEdgeId (..), FaceId (..))
import Moonlight.Triangulation.Internal.DcelOperations.CandidateArena
  ( seedGenericPairInArena
  )
import Moonlight.Triangulation.Internal.DcelOperations.Legalize
  ( legalizeScratch
  , legalizeDenseStarEdgeInArena
  )
import Moonlight.Triangulation.Internal.DcelOperations.Normalize
  ( LegalizationDrain (..)
  , drainDenseUnconstrainedGenericLegalization
  )
import Moonlight.Triangulation.Internal.DcelOperations.Twin (reverseIndex)
import Moonlight.Triangulation.Internal.Mutable
  ( DenseMutableDcel
  , MutableDcel (..)
  , addEdge
  , addEdgeBlock
  , addFaceBlock
  , directedEdgeCount
  , denseCommitFreshConnections
  , denseInitializeUnconstrainedEdgeBlock
  , denseLinkEdges
  , denseMarkFreshConnected
  , denseMutableOwner
  , denseReadFace
  , denseReadFaceEdge
  , denseReadNext
  , denseReadOrigin
  , denseReadPointX
  , denseReadPointY
  , denseReadPrevious
  , denseSetCycle3
  , denseWriteFace
  , denseWriteFaceEdge
  , denseWriteNext
  , denseWriteOrigin
  , denseWritePrevious
  , denseWriteVertexOut
  , ensureCellCapacity
  , faceCount
  , linkEdges
  , markConnected
  , pointAt
  , readFace
  , readNext
  , readOrigin
  , readPointX
  , readPointY
  , readPrevious
  , setCycle3
  , writeFace
  , writeFaceEdge
  , writeNext
  , writeOrigin
  , writePrevious
  , writeVertexOut
  )
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (..)
  , LegalizationArena
  , OperationState
  , addCounter
  , readScratch
  , writeScratch
  )
import Moonlight.Triangulation.Internal.Probe (KnownProbe)
import Moonlight.Triangulation.Types (BuildError (..), Point (..))
import Moonlight.Triangulation.Internal.Predicates (orient2dCoordinates)

-- | Proof that the remaining circle-sweep program fits the particular mutable
-- arena carried here. The constructor is private: only 'reserveSweepCells' can
-- pair a DCEL with the one-time capacity check. Carrying the DCEL inside the
-- witness prevents a proof for one mutable mesh from being applied to another
-- mesh that happens to share the same @ST@ region.
data ReservedSweepCells s vertex directed undirected face = ReservedSweepCells
  !(DenseMutableDcel s vertex directed undirected face)
  {-# UNPACK #-} !SweepCellCursor

-- | The uncommitted directed-edge and face cardinalities of one reserved
-- sweep section. Its constructor is private: only the reservation can mint the
-- initial cursor, and only reserved topology rewrites can advance it.
data SweepCellCursor = SweepCellCursor
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int

-- | The outer edges glued by one direct sweep insertion and the normalization
-- work discharged while its star still existed. The sweep accumulates these
-- strict metrics and charges the operation counters once, instead of mutating
-- diagnostic cells once per point.
data SweepInsertion s
  = SweepInsertionFailure !BuildError
  | SweepInsertion
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      !(LegalizationArena s)
      {-# UNPACK #-} !SweepCellCursor

-- | Discharge the monotone circle-sweep allocation budget once. Every
-- remaining point can contribute at most three undirected edges and two faces
-- to a planar triangulation. The seed already occupies its own cells, so this
-- bound is stated only over the points the sweep has not connected yet.
reserveSweepCells
  :: DenseMutableDcel s vertex directed undirected face
  -> Int
  -> ST s (Either BuildError (ReservedSweepCells s vertex directed undirected face))
reserveSweepCells dense remainingPoints = do
  let !mutable = denseMutableOwner dense
  halfEdges <- directedEdgeCount mutable
  faces <- faceCount mutable
  capacity <- ensureCellCapacity mutable (3 * remainingPoints) (2 * remainingPoints)
  pure (ReservedSweepCells dense (SweepCellCursor halfEdges faces) <$ capacity)

initialSweepCellCursor
  :: ReservedSweepCells s vertex directed undirected face
  -> SweepCellCursor
initialSweepCellCursor (ReservedSweepCells _ cursor) = cursor
{-# INLINE initialSweepCellCursor #-}

-- | Commit the cardinality of the fresh connectivity sections materialized by
-- direct sweep insertions. Their per-vertex bits and outgoing edges are already
-- present; this is the one global descent step before any skipped point enters
-- the ordinary insertion interpreter.
commitReservedSweepConnections
  :: ReservedSweepCells s vertex directed undirected face
  -> SweepCellCursor
  -> Int
  -> ST s ()
commitReservedSweepConnections (ReservedSweepCells dense _) (SweepCellCursor halfEdges faces) inserted = do
  denseCommitFreshConnections dense inserted
  let !MutableDcel{mdHalfCount, mdFaceCount, mdLastFace} = denseMutableOwner dense
  writeSTRef mdHalfCount halfEdges
  writeSTRef mdFaceCount faces
  when (inserted > 0 && faces > 1) (writeSTRef mdLastFace (faces - 1))
{-# INLINE commitReservedSweepConnections #-}

insertOutsideHull :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s (Either BuildError ())
insertOutsideHull mutable operation start vertex = do
  query <- pointAt mutable vertex
  visibleStart <- visibleOuter mutable start query
  if not visibleStart
    then pure (Left (HullStartNotVisible (DirectedEdgeId (fromIntegral start))))
    else do
      halfEdges <- directedEdgeCount mutable
      left <- expandPrevious halfEdges start start query
      right <- expandNext halfEdges start left query
      collected <- collectOuterChain mutable operation left right
      case collected of
        Left obstruction -> pure (Left obstruction)
        Right chainCount -> do
          inserted <- insertOutsideHullCollected @p mutable operation vertex chainCount
          case inserted of
            Left obstruction -> pure (Left obstruction)
            Right _ -> do
              -- The regular insertion path owns its own hull-insertion count; the sweep
              -- path through 'insertOutsideHullBetween' counts its own instead.
              addCounter operation CounterHullInsertions 1
              pure (Right ())
 where
  expandPrevious !bound !stopAt !current !query
    | bound <= 0 = pure current
    | otherwise = do
        candidate <- readPrevious mutable current
        if candidate == stopAt
          then pure current
          else do
            visible <- visibleOuter mutable candidate query
            if visible then expandPrevious (bound - 1) stopAt candidate query else pure current

  expandNext !bound !current !left !query
    | bound <= 0 = pure current
    | otherwise = do
        candidate <- readNext mutable current
        if candidate == left
          then pure current
          else do
            visible <- visibleOuter mutable candidate query
            if visible then expandNext (bound - 1) candidate left query else pure current

-- | Replace one selected outer edge @a->b@ by @a->v, v->b@ and materialize
-- the covered triangle. This is the circle sweep's actual local section: the
-- ordinary outside-hull path continues to own arbitrary visible ranges, while
-- the sweep no longer collects a singleton range into shared scratch merely to
-- rediscover its first and last edge.
insertOutsideHullAtEdge
  :: ReservedSweepCells s vertex directed undirected face
  -> SweepCellCursor
  -> LegalizationArena s
  -> Int
  -> Int
  -> Int
  -> Int
  -> ST s (SweepInsertion s)
insertOutsideHullAtEdge (ReservedSweepCells dense _) (SweepCellCursor nextHalf nextFace) arena outerEdge from to vertex = do
  incident <- denseReadFace dense outerEdge
  if incident /= 0
    then
      pure
        ( SweepInsertionFailure
            ( OuterRangeContainsInnerEdge
                (DirectedEdgeId (fromIntegral outerEdge))
                (FaceId (fromIntegral incident))
            )
        )
    else do
      oldPrevious <- denseReadPrevious dense outerEdge
      oldNext <- denseReadNext dense outerEdge
      denseInitializeUnconstrainedEdgeBlock dense nextHalf 2
      let !edgeBase = nextHalf
          !face = nextFace
          !firstOuterSpoke = edgeBase
          !firstInnerSpoke = edgeBase + 1
          !secondInnerSpoke = edgeBase + 2
          !lastOuterSpoke = edgeBase + 3
      denseWriteOrigin dense firstOuterSpoke from
      denseWriteOrigin dense firstInnerSpoke vertex
      denseWriteOrigin dense secondInnerSpoke to
      denseWriteOrigin dense lastOuterSpoke vertex
      denseSetCycle3 dense face outerEdge secondInnerSpoke firstInnerSpoke
      denseWriteFace dense firstOuterSpoke 0
      denseWriteFace dense lastOuterSpoke 0
      denseLinkEdges dense oldPrevious firstOuterSpoke
      denseLinkEdges dense firstOuterSpoke lastOuterSpoke
      denseLinkEdges dense lastOuterSpoke oldNext
      denseWriteFaceEdge dense 0 firstOuterSpoke
      denseWriteVertexOut dense from outerEdge
      denseWriteVertexOut dense to secondInnerSpoke
      denseMarkFreshConnected dense vertex lastOuterSpoke
      LegalizationDrain flips maxDepth () finalArena <-
        legalizeDenseStarEdgeInArena dense arena vertex outerEdge
      pure
        ( SweepInsertion
            firstOuterSpoke
            lastOuterSpoke
            flips
            maxDepth
            finalArena
            (SweepCellCursor (nextHalf + 4) (nextFace + 1))
        )
{-# INLINE insertOutsideHullAtEdge #-}

collectOuterChain
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> ST s (Either BuildError Int)
collectOuterChain mutable operation left right = do
  halfEdges <- directedEdgeCount mutable
  go (halfEdges + 1) left 0
 where
  go !remaining !current !count
    | remaining <= 0 =
        pure
          ( Left
              ( OuterRangeDidNotTerminate
                  (DirectedEdgeId (fromIntegral left))
                  (DirectedEdgeId (fromIntegral right))
                  count
              )
          )
    | otherwise = do
        incident <- readFace mutable current
        if incident /= 0
          then
            pure
              ( Left
                  ( OuterRangeContainsInnerEdge
                      (DirectedEdgeId (fromIntegral current))
                      (FaceId (fromIntegral incident))
                  )
              )
          else do
            writeScratch operation count current
            if current == right
              then pure (Right (count + 1))
              else readNext mutable current >>= \following -> go (remaining - 1) following (count + 1)

insertOutsideHullCollected
  :: forall p s vertex directed undirected face
   . KnownProbe p
  => MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> ST s (Either BuildError (Int, Int))
insertOutsideHullCollected mutable@MutableDcel{mdLastFace} operation vertex chainCount = do
  capacity <- ensureCellCapacity mutable (chainCount + 1) chainCount
  case capacity of
    Left obstruction -> pure (Left obstruction)
    Right () -> Right <$> insertOutsideHullWithCapacity
 where
  insertOutsideHullWithCapacity = do
    left <- readScratch operation 0
    right <- readScratch operation (chainCount - 1)
    oldPrevious <- readPrevious mutable left
    oldNext <- readNext mutable right
    edgeBase <- addEdgeBlock mutable (chainCount + 1)
    faceBase <- addFaceBlock mutable chainCount
    forM_ [0 .. chainCount] $ \index -> do
      chainVertex <-
        if index == 0
          then readScratch operation 0 >>= readOrigin mutable
          else readScratch operation (index - 1) >>= readOrigin mutable . reverseIndex
      let !forward = edgeBase + 2 * index
          !backward = forward + 1
      writeOrigin mutable forward chainVertex
      writeOrigin mutable backward vertex
    forM_ [0 .. chainCount - 1] $ \index -> do
      outerEdge <- readScratch operation index
      let !face = faceBase + index
          !nextSpoke = edgeBase + 2 * (index + 1)
          !previousSpoke = edgeBase + 2 * index + 1
      setCycle3 mutable face outerEdge nextSpoke previousSpoke
    let !firstOuterSpoke = edgeBase
        !lastOuterSpoke = edgeBase + 2 * chainCount + 1
    writeFace mutable firstOuterSpoke 0
    writeFace mutable lastOuterSpoke 0
    linkEdges mutable oldPrevious firstOuterSpoke
    linkEdges mutable firstOuterSpoke lastOuterSpoke
    linkEdges mutable lastOuterSpoke oldNext
    writeFaceEdge mutable 0 firstOuterSpoke
    forM_ [0 .. chainCount - 1] $ \index -> do
      chainEdge <- readScratch operation index
      chainVertex <- readOrigin mutable chainEdge
      writeVertexOut mutable chainVertex chainEdge
    lastChain <- readScratch operation (chainCount - 1)
    lastVertex <- readOrigin mutable (reverseIndex lastChain)
    writeVertexOut mutable lastVertex (edgeBase + 2 * chainCount)
    markConnected mutable vertex lastOuterSpoke
    writeSTRef mdLastFace faceBase
    legalizeScratch @p mutable operation vertex chainCount
    pure (firstOuterSpoke, lastOuterSpoke)

-- | Replace two consecutive outer edges @a->b, b->c@ by @a->c@ and
-- materialize the triangle they bound. The returned edge is the new outer
-- diagonal. This is the sole turn-closing primitive used both by deferred
-- circle sweep and the final Graham repair. Topology mutation only: the
-- caller owns the legalization epoch, and seeds the two closed edges itself
-- once the replacement's links are in place.
closeOuterTurn
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> ST s (Either BuildError Int)
closeOuterTurn mutable first = do
  capacity <- ensureCellCapacity mutable 1 1
  case capacity of
    Left obstruction -> pure (Left obstruction)
    Right () -> Right <$> closeOuterTurnWithCapacity mutable first
{-# INLINE closeOuterTurn #-}

-- | The circle-sweep form of 'closeOuterTurn'. Its capacity obstruction was
-- discharged by 'reserveSweepCells'; topology mutation is otherwise identical
-- to the checked public-internal operation used by joins.
closeOuterTurnReserved
  :: ReservedSweepCells s vertex directed undirected face
  -> SweepCellCursor
  -> Int
  -> ST s (Int, SweepCellCursor)
closeOuterTurnReserved
  (ReservedSweepCells dense _)
  (SweepCellCursor nextHalf nextFace)
  first = do
    denseInitializeUnconstrainedEdgeBlock dense nextHalf 1
    replacement <- closeOuterTurnDenseAt dense nextHalf nextFace first
    pure (replacement, SweepCellCursor (nextHalf + 2) (nextFace + 1))
{-# INLINE closeOuterTurnReserved #-}

-- | Materialize one turn closure at cells already owned by the caller's
-- allocation section. Ordinary edits obtain those cells from the checked
-- allocator; the reserved sweep obtains them from its immutable cursor.
closeOuterTurnDenseAt
  :: DenseMutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> Int
  -> ST s Int
closeOuterTurnDenseAt dense edgeBase newFace first = do
  second <- denseReadNext dense first
  oldPrevious <- denseReadPrevious dense first
  oldNext <- denseReadNext dense second
  from <- denseReadOrigin dense first
  to <- denseReadOrigin dense (reverseIndex second)
  let !outer = edgeBase
      !inner = edgeBase + 1
  denseWriteOrigin dense outer from
  denseWriteOrigin dense inner to
  denseWriteFace dense outer 0
  -- @first -> second@ is the authoritative outer adjacency that licensed this
  -- closure. Preserve those two already-correct cells and write only the six
  -- changed links plus the new face section.
  denseWriteNext dense second inner
  denseWritePrevious dense inner second
  denseWriteNext dense inner first
  denseWritePrevious dense first inner
  denseWriteFace dense first newFace
  denseWriteFace dense second newFace
  denseWriteFace dense inner newFace
  denseWriteFaceEdge dense newFace first
  denseLinkEdges dense oldPrevious outer
  denseLinkEdges dense outer oldNext
  denseWriteFaceEdge dense 0 outer
  denseWriteVertexOut dense from outer
  denseWriteVertexOut dense to inner
  pure outer
{-# INLINE closeOuterTurnDenseAt #-}

closeOuterTurnWithCapacity
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> ST s Int
closeOuterTurnWithCapacity mutable first = do
  second <- readNext mutable first
  oldPrevious <- readPrevious mutable first
  oldNext <- readNext mutable second
  from <- readOrigin mutable first
  to <- readOrigin mutable (reverseIndex second)
  (outer, inner) <- addEdge mutable from to
  newFace <- addFaceBlock mutable 1
  writeFace mutable outer 0
  writeNext mutable second inner
  writePrevious mutable inner second
  writeNext mutable inner first
  writePrevious mutable first inner
  writeFace mutable first newFace
  writeFace mutable second newFace
  writeFace mutable inner newFace
  writeFaceEdge mutable newFace first
  linkEdges mutable oldPrevious outer
  linkEdges mutable outer oldNext
  writeFaceEdge mutable 0 outer
  writeVertexOut mutable from outer
  writeVertexOut mutable to inner
  pure outer
{-# INLINE closeOuterTurnWithCapacity #-}

-- | Close every remaining left turn of the star-shaped sweep hull in one
-- Graham-style pass. Each closure strictly decreases the outer-edge count, so
-- the pass is linear in the visited hull plus the local Delaunay legalization
-- work it causes. Turn closures are seeded into one shared epoch as they
-- happen and drained once at the end: closure never deletes an edge and never
-- touches the outer cycle's legality, so which turns close does not depend on
-- when the interior is repaired. The pass counts its own closures and returns
-- the drain's tallies; nothing is reported behind its back.
fixHullConvexity
  :: ReservedSweepCells s vertex directed undirected face
  -> SweepCellCursor
  -> OperationState s
  -> LegalizationArena s
  -> ST s (Either BuildError (Int, Int, Int, LegalizationArena s, SweepCellCursor))
fixHullConvexity reserved@(ReservedSweepCells dense _) initialCursor operation initialArena = do
  start <- denseReadFaceEdge dense 0
  if start < 0
    then pure (Right (0, 0, 0, initialArena, initialCursor))
    else do
      walked <- walk start start 0 0 0 0 initialArena initialCursor
      case walked of
        Left obstruction -> pure (Left obstruction)
        Right (top, closures, seededArena, finalCursor) -> do
          LegalizationDrain flips maxDepth () finalArena <-
            drainDenseUnconstrainedGenericLegalization dense seededArena top
          pure (Right (closures, flips, maxDepth, finalArena, finalCursor))
 where
  walk !start !current !stackSize !steps !top !closures !arena cursor@(SweepCellCursor halfEdges _) = do
    if steps > halfEdges + 2
      then
        pure
          ( Left
              ( OuterCycleDidNotTerminate
                  (DirectedEdgeId (fromIntegral start))
                  (DirectedEdgeId (fromIntegral current))
                  steps
              )
          )
      else do
        following <- denseReadNext dense current
        writeScratch operation stackSize current
        reduction <- reduce (stackSize + 1) top closures arena cursor
        case reduction of
          Left obstruction -> pure (Left obstruction)
          Right (reduced, nextTop, nextClosures, nextArena, nextCursor) -> do
            finished <-
              if reduced < 2
                then pure False
                else (== following) <$> readScratch operation 1
            if finished
              then pure (Right (nextTop, nextClosures, nextArena, nextCursor))
              else walk start following reduced (steps + 1) nextTop nextClosures nextArena nextCursor

  reduce !count !top !closures !arena !cursor
    | count < 2 = pure (Right (count, top, closures, arena, cursor))
    | otherwise = do
        first <- readScratch operation (count - 2)
        second <- readScratch operation (count - 1)
        fromVertex <- denseReadOrigin dense first
        middleVertex <- denseReadOrigin dense (reverseIndex first)
        targetVertex <- denseReadOrigin dense (reverseIndex second)
        fromX <- denseReadPointX dense fromVertex
        fromY <- denseReadPointY dense fromVertex
        middleX <- denseReadPointX dense middleVertex
        middleY <- denseReadPointY dense middleVertex
        targetX <- denseReadPointX dense targetVertex
        targetY <- denseReadPointY dense targetVertex
        if orient2dCoordinates fromX fromY middleX middleY targetX targetY == GT
          then do
            (replacement, nextCursor) <- closeOuterTurnReserved reserved cursor first
            writeScratch operation (count - 2) replacement
            (nextArena, nextTop) <- seedGenericPairInArena arena top first second
            reduce (count - 1) nextTop (closures + 1) nextArena nextCursor
          else pure (Right (count, top, closures, arena, cursor))

visibleOuter :: MutableDcel s vertex directed undirected face -> Int -> Point -> ST s Bool
visibleOuter mutable edge query = do
  fromVertex <- readOrigin mutable edge
  toVertex <- readOrigin mutable (reverseIndex edge)
  case query of
    Point queryX queryY -> do
      fromX <- readPointX mutable fromVertex
      fromY <- readPointY mutable fromVertex
      toX <- readPointX mutable toVertex
      toY <- readPointY mutable toVertex
      pure (orient2dCoordinates fromX fromY toX toY queryX queryY == GT)
{-# INLINE visibleOuter #-}
