{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Interpretation of a recovery program against the thawed mesh: conflict
-- strips are flipped away segment by segment and the constraints are set.
module Moonlight.Triangulation.Internal.Cdt.Recovery
  ( ConflictRecovery (..)
  , applyMutableConstraint
  , emptyConstraintRequest
  , recoverMutableRequest
  , recoverMutableProgramPiece
  , recordRecoveredSegment
  , resolveConflictStrip
  , mutableEdgeCrosses
  ) where

import Control.Monad.ST (ST)
import Data.Bits (xor)
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Internal.Cdt.Combinators
  ( foldWhileM
  )
import Moonlight.Triangulation.Internal.Cdt.Corridor
  ( constraintWorkspaceFor
  , existingProgramTag
  , readConstraintProgram
  , recoverProgramTag
  , scanMutableConstraint
  )
import Moonlight.Triangulation.Internal.Cdt.Query (findMutableEdge)
import Moonlight.Triangulation.Internal.Cdt.Types
import Moonlight.Triangulation.Internal.DcelOperations.FlipRewrite (flipEdge)
import Moonlight.Triangulation.Internal.DcelOperations.FlipRule (isFlippableEdge)
import Moonlight.Triangulation.Internal.DcelOperations.Legalize (legalizeEdges)
import Moonlight.Triangulation.Internal.Growable (GrowableWord32)
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  , readScratch
  , writeScratch
  )
import Moonlight.Triangulation.Internal.Types
import Moonlight.Triangulation.Internal.Predicates (orient2dCoordinates)

data ConflictRecovery = ConflictRecovery
  { recoveredConstraintEdge :: {-# UNPACK #-} !Int
  , recoveredConstraintFresh :: !Bool
  }

-- | Admit one request against the thawed mesh and recover it. Every constraint
-- verb in this module reaches the topology through here; the callers differ
-- only in what they do with a rejection and in when they publish.
applyMutableConstraint
  :: GrowableWord32 s
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> VertexId
  -> VertexId
  -> ST s (Either (CdtError) MutableConstraintOutcome)
applyMutableConstraint programWords mutable operation from to
  | from == to = pure (Right (MutableConstraintAccepted emptyConstraintRequest))
  | otherwise = do
      workspace <- constraintWorkspaceFor programWords mutable
      scanned <- scanMutableConstraint workspace mutable from to
      case scanned of
        Left failure -> pure (Left failure)
        Right (MutableConstraintScanBlocked blocking) ->
          pure (Right (MutableConstraintRejected (asUndirected blocking)))
        Right (MutableConstraintScanAdmitted program) ->
          fmap MutableConstraintAccepted
            <$> recoverMutableRequest workspace mutable operation program

emptyConstraintRequest :: ConstraintRequestAccumulator
emptyConstraintRequest =
  ConstraintRequestAccumulator
    { accumulatedRequestPath = []
    , accumulatedRequestAddedEdges = 0
    , accumulatedRequestCorridors = 0
    , accumulatedRequestReusedFaces = 0
    , accumulatedRequestCrossedEdges = 0
    }

recoverMutableRequest
  :: ConstraintWorkspace s
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> MutableConstraintProgram
  -> ST s (Either (CdtError) ConstraintRequestAccumulator)
recoverMutableRequest workspace mutable operation program = do
  interpreted <-
    foldWhileM
      isRight
      (recoverMutableProgramPiece workspace mutable operation (mutableProgramWordCount program))
      ( Right
          ConstraintProgramAccumulator
            { accumulatedProgramCursor = 0
            , accumulatedProgramRequest = emptyConstraintRequest
            }
      )
      [1 .. mutableProgramPieceCount program]
  pure $ case interpreted of
    Left obstruction -> Left obstruction
    Right completed
      | accumulatedProgramCursor completed == mutableProgramWordCount program ->
          Right (accumulatedProgramRequest completed)
      | otherwise ->
          Left
            ( ConstraintCorridorObstructed
                (CorridorProgramMalformed (accumulatedProgramCursor completed))
            )

recoverMutableProgramPiece
  :: ConstraintWorkspace s
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Either (CdtError) ConstraintProgramAccumulator
  -> Int
  -> ST s (Either (CdtError) ConstraintProgramAccumulator)
recoverMutableProgramPiece _ _ _ _ failed@(Left _) _ = pure failed
recoverMutableProgramPiece workspace mutable operation wordCount (Right accumulator) _ = do
  let !cursor = accumulatedProgramCursor accumulator
      malformed =
        Left
          ( ConstraintCorridorObstructed
              (CorridorProgramMalformed cursor)
          )
  if cursor < 0 || cursor >= wordCount
    then pure malformed
    else do
      tag <- readConstraintProgram workspace cursor
      case tag of
        _
          | tag == existingProgramTag && cursor + 1 < wordCount -> do
              edge <- readConstraintProgram workspace (cursor + 1)
              fresh <- setConstraint mutable edge
              pure
                ( Right
                    accumulator
                      { accumulatedProgramCursor = cursor + 2
                      , accumulatedProgramRequest =
                          recordRecoveredSegment
                            (accumulatedProgramRequest accumulator)
                            edge
                            fresh
                            0
                            0
                      }
                )
          | tag == recoverProgramTag && cursor + 3 < wordCount -> do
              rawFrom <- readConstraintProgram workspace (cursor + 1)
              rawTo <- readConstraintProgram workspace (cursor + 2)
              conflictCount <- readConstraintProgram workspace (cursor + 3)
              let !conflictStart = cursor + 4
                  !nextCursor = conflictStart + conflictCount
                  !from = VertexId (fromIntegral rawFrom)
                  !to = VertexId (fromIntegral rawTo)
              if conflictCount < 0 || nextCursor > wordCount
                then pure malformed
                else do
                  direct <- findMutableEdge mutable rawFrom rawTo
                  recovered <-
                    case direct of
                      Just edge -> do
                        fresh <- setConstraint mutable edge
                        pure
                          ( Right
                              ConflictRecovery
                                { recoveredConstraintEdge = edge
                                , recoveredConstraintFresh = fresh
                                }
                          )
                      Nothing
                        | conflictCount == 0 ->
                            pure
                              ( Left
                                  ( ConstraintCorridorObstructed
                                      (CorridorTargetMissing from to)
                                  )
                              )
                        | otherwise ->
                            resolveConflictStrip
                              workspace
                              mutable
                              operation
                              from
                              to
                              conflictStart
                              conflictCount
                  pure $ case recovered of
                    Left obstruction -> Left obstruction
                    Right recovery ->
                      Right
                        accumulator
                          { accumulatedProgramCursor = nextCursor
                          , accumulatedProgramRequest =
                              recordRecoveredSegment
                                (accumulatedProgramRequest accumulator)
                                (recoveredConstraintEdge recovery)
                                (recoveredConstraintFresh recovery)
                                conflictCount
                                (if conflictCount == 0 then 0 else conflictCount + 1)
                          }
          | otherwise -> pure malformed

recordRecoveredSegment
  :: ConstraintRequestAccumulator
  -> Int
  -> Bool
  -> Int
  -> Int
  -> ConstraintRequestAccumulator
recordRecoveredSegment accumulator edge fresh crossed reusedFaces =
  accumulator
    { accumulatedRequestPath = DirectedEdgeId (fromIntegral edge) : accumulatedRequestPath accumulator
    , accumulatedRequestAddedEdges =
        accumulatedRequestAddedEdges accumulator + if fresh then 1 else 0
    , accumulatedRequestCorridors =
        accumulatedRequestCorridors accumulator + if crossed == 0 then 0 else 1
    , accumulatedRequestReusedFaces =
        accumulatedRequestReusedFaces accumulator + reusedFaces
    , accumulatedRequestCrossedEdges =
        accumulatedRequestCrossedEdges accumulator + crossed
    }

resolveConflictStrip
  :: ConstraintWorkspace s
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> VertexId
  -> VertexId
  -> Int
  -> Int
  -> ST s (Either (CdtError) ConflictRecovery)
resolveConflictStrip workspace mutable operation from to conflictStart stripLength = do
  fromPoint <- pointAt mutable (vertexIdIndex from)
  toPoint <- pointAt mutable (vertexIdIndex to)
  traverse_
    (\index -> do
      edge <- readConstraintProgram workspace (conflictStart + index)
      writeScratch operation index edge
    )
    [0 .. stripLength - 1]
  recovered <-
    recover
      stripLength
      safetyBudget
      0
      0
      stripLength
      stripLength
      []
      fromPoint
      toPoint
  case recovered of
    Left failure -> pure (Left failure)
    Right (edge, flipped) -> do
      fresh <- setConstraint mutable edge
      legalizeEdges mutable operation flipped
      pure
        ( Right
            ConflictRecovery
              { recoveredConstraintEdge = edge
              , recoveredConstraintFresh = fresh
              }
        )
 where
  safetyBudget = max 64 (32 * (stripLength + 1) * (stripLength + 1))

  -- The corridor is an ordered section through the current triangulation.
  -- Until an original section is dequeued, no flip can have changed that
  -- edge's endpoints: flips only repurpose the identity being flipped, and
  -- requeued identities remain behind every unseen original in this FIFO.
  -- Consequently the first visit inherits the crossing proof established by
  -- the corridor walk. Only requeued edges require the predicate again.
  recover !capacity !remaining !stalled !headIndex !pendingCount !unseenOriginalCount !flipped !fromPoint !toPoint
    | remaining <= 0 =
        pure
          ( Left
              ( ConstraintRecoverySafetyBudgetExhausted
                  from
                  to
                  safetyBudget
                  (safetyBudget - remaining)
              )
          )
    | pendingCount <= 0 = do
        direct <- findMutableEdge mutable (vertexIdIndex from) (vertexIdIndex to)
        case direct of
          Just edge -> pure (Right (edge, flipped))
          Nothing ->
            pure
              (Left (ConstraintRecoveryStripExhausted from to stripLength))
    | otherwise = do
        edge <- readScratch operation headIndex
        let !nextHead = advance capacity headIndex
            !restCount = pendingCount - 1
            !nextUnseenOriginalCount = max 0 (unseenOriginalCount - 1)
        stillCrosses <-
          if unseenOriginalCount > 0
            then pure True
            else mutableEdgeCrosses mutable fromPoint toPoint edge
        if not stillCrosses
          then recover capacity (remaining - 1) 0 nextHead restCount nextUnseenOriginalCount flipped fromPoint toPoint
          else do
            flippable <- isFlippableEdge mutable edge
            if flippable
              then do
                rewritten <- flipEdge mutable edge
                case rewritten of
                  Left obstruction -> pure (Left (CdtBuildError obstruction))
                  Right () -> do
                    addCounter operation CounterEdgeFlips 1
                    crossesAfterFlip <- mutableEdgeCrosses mutable fromPoint toPoint edge
                    if crossesAfterFlip
                      then do
                        enqueue capacity nextHead restCount edge
                        recover
                          capacity
                          (remaining - 1)
                          0
                          nextHead
                          (restCount + 1)
                          nextUnseenOriginalCount
                          (edge : flipped)
                          fromPoint
                          toPoint
                      else recover capacity (remaining - 1) 0 nextHead restCount nextUnseenOriginalCount (edge : flipped) fromPoint toPoint
              else do
                enqueue capacity nextHead restCount edge
                let !nextCount = restCount + 1
                    !nextStalled = stalled + 1
                if nextStalled >= nextCount
                  then
                    pure
                      ( Left
                          ( ConstraintRecoveryStripUnflippable
                              from
                              to
                              (DirectedEdgeId (fromIntegral edge))
                              nextCount
                          )
                      )
                  else recover capacity (remaining - 1) nextStalled nextHead nextCount nextUnseenOriginalCount flipped fromPoint toPoint

  enqueue !capacity !headIndex !count !edge =
    writeScratch operation (wrap capacity (headIndex + count)) edge

  advance :: Int -> Int -> Int
  advance !capacity !index
    | index + 1 == capacity = 0
    | otherwise = index + 1

  wrap :: Int -> Int -> Int
  wrap !capacity !index
    | index >= capacity = index - capacity
    | otherwise = index

mutableEdgeCrosses
  :: MutableDcel s vertex directed undirected face
  -> Point
  -> Point
  -> Int
  -> ST s Bool
mutableEdgeCrosses
  mutable
  (Point lineFromX lineFromY)
  (Point lineToX lineToY)
  edge = do
    edgeFromVertex <- readOrigin mutable edge
    edgeToVertex <- readOrigin mutable (edge `xor` 1)
    Point edgeFromX edgeFromY <- pointAt mutable edgeFromVertex
    Point edgeToX edgeToY <- pointAt mutable edgeToVertex
    let !edgeFromSide =
          orient2dCoordinates
            lineFromX lineFromY lineToX lineToY edgeFromX edgeFromY
        !edgeToSide =
          orient2dCoordinates
            lineFromX lineFromY lineToX lineToY edgeToX edgeToY
        !lineFromSide =
          orient2dCoordinates
            edgeFromX edgeFromY edgeToX edgeToY lineFromX lineFromY
        !lineToSide =
          orient2dCoordinates
            edgeFromX edgeFromY edgeToX edgeToY lineToX lineToY
    pure
      ( opposite edgeFromSide edgeToSide
          && opposite lineFromSide lineToSide
      )
 where
  opposite LT GT = True
  opposite GT LT = True
  opposite _ _ = False
