{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | The corridor walk: one traversal of the requested segment that yields
-- either the first oriented blocking witness or a complete recovery program.
module Moonlight.Planar.Internal.Cdt.Corridor
  ( constraintWorkspaceFor
  , scanMutableConstraint
  , advanceMutablePlan
  , continueMutablePlan
  , beginMutableRecover
  , finishMutableRecover
  , settleMutableProgram
  , mutablePlanIsActive
  , writeConstraintProgram
  , readConstraintProgram
  , existingProgramTag
  , recoverProgramTag
  ) where

import Control.Monad.ST (ST)
import Data.Bits (xor)
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.IntersectionIterator (Intersection (..))
import Moonlight.Planar.Internal.Cdt.Combinators (foldWhileM)
import Moonlight.Planar.Internal.Cdt.Corridor.Trace (nextMutableIntersection)
import Moonlight.Planar.Internal.Cdt.Types
import Moonlight.Planar.Internal.Growable
  ( GrowableWord32
  , clearGrowable
  , readGrowable
  , writeGrowable
  )
import Moonlight.Planar.Internal.Mutable
import Moonlight.Planar.Point (Point)

-- | The walk budget bounds a corridor through the mesh as it stands, so it is
-- taken per request rather than per transaction: a verb that inserts before it
-- recovers has changed the mesh the budget describes. Recovery itself adds
-- neither vertex nor edge, so a batch reads the same number every time.
constraintWorkspaceFor
  :: GrowableWord32 s
  -> MutableDcel s vertex directed undirected face
  -> ST s (ConstraintWorkspace s)
constraintWorkspaceFor constraintProgramWords mutable = do
  halfEdges <- directedEdgeCount mutable
  vertices <- pointCount mutable
  pure
    ConstraintWorkspace
      { constraintProgramWords
      , constraintWalkBudget = 2 * halfEdges + vertices + 8
      }

writeConstraintProgram :: ConstraintWorkspace s -> Int -> Int -> ST s ()
writeConstraintProgram ConstraintWorkspace{constraintProgramWords} index value =
  writeGrowable constraintProgramWords index (fromIntegral value)
{-# INLINE writeConstraintProgram #-}

readConstraintProgram :: ConstraintWorkspace s -> Int -> ST s Int
readConstraintProgram ConstraintWorkspace{constraintProgramWords} index =
  fromIntegral <$> readGrowable constraintProgramWords index
{-# INLINE readConstraintProgram #-}

-- | Walk one corridor once, gluing the local observations into either the
-- first oriented blocking witness or a complete recovery program. The former
-- admission/planning pair traversed every admitted corridor twice and could
-- disagree if either walk evolved independently; this scanner is their single
-- semantic owner.
scanMutableConstraint
  :: ConstraintWorkspace s
  -> MutableDcel s vertex directed undirected face
  -> VertexId
  -> VertexId
  -> ST s (Either (CdtError) MutableConstraintScan)
scanMutableConstraint workspace@ConstraintWorkspace{constraintProgramWords, constraintWalkBudget} mutable from to = do
  clearGrowable constraintProgramWords
  lineFrom <- pointAt mutable (vertexIdIndex from)
  lineTo <- pointAt mutable (vertexIdIndex to)
  initialCursor <- beginMutableRecover workspace 0 from 0 False
  walked <-
    foldWhileM
      mutablePlanIsActive
      (advanceMutablePlan workspace mutable lineFrom lineTo to)
      (MutablePlanActive (VertexIntersection from) initialCursor)
      [1 .. constraintWalkBudget]
  pure $ case walked of
    MutablePlanComplete plan -> Right (MutableConstraintScanAdmitted plan)
    MutablePlanBlocked blocking -> Right (MutableConstraintScanBlocked blocking)
    MutablePlanFailed obstruction -> Left (ConstraintCorridorObstructed obstruction)
    MutablePlanActive _ _ ->
      Left
        ( ConstraintCorridorObstructed
            (CorridorWalkDidNotTerminate constraintWalkBudget)
        )

advanceMutablePlan
  :: ConstraintWorkspace s
  -> MutableDcel s vertex directed undirected face
  -> Point
  -> Point
  -> VertexId
  -> MutablePlanWalk
  -> Int
  -> ST s MutablePlanWalk
advanceMutablePlan _ _ _ _ _ complete@(MutablePlanComplete _) _ = pure complete
advanceMutablePlan _ _ _ _ _ blocked@(MutablePlanBlocked _) _ = pure blocked
advanceMutablePlan _ _ _ _ _ failed@(MutablePlanFailed _) _ = pure failed
advanceMutablePlan workspace mutable lineFrom lineTo target (MutablePlanActive event cursor) _ =
  case event of
    EdgeIntersection directed -> do
      constrained <- readConstraint mutable (directedEdgeIdIndex directed)
      if constrained
        then pure (MutablePlanBlocked directed)
        else do
          writeConstraintProgram workspace (mutableCursorWriteAt cursor) (directedEdgeIdIndex directed)
          continueMutablePlan
            workspace
            mutable
            lineFrom
            lineTo
            target
            event
            cursor
              { mutableCursorWriteAt = mutableCursorWriteAt cursor + 1
              , mutableCursorConflictCount = mutableCursorConflictCount cursor + 1
              , mutableCursorAfterOverlap = False
              }
    VertexIntersection vertex
      | mutableCursorAfterOverlap cursor ->
          continueMutablePlan
            workspace
            mutable
            lineFrom
            lineTo
            target
            event
            cursor{mutableCursorAfterOverlap = False}
      | vertex == mutableCursorAt cursor ->
          continueMutablePlan
            workspace
            mutable
            lineFrom
            lineTo
            target
            event
            cursor{mutableCursorAfterOverlap = False}
      | otherwise -> do
          finished <- finishMutableRecover workspace vertex cursor
          nextCursor <-
            beginMutableRecover
              workspace
              (mutableCursorWriteAt finished)
              vertex
              (mutableCursorPieceCount finished)
              False
          continueMutablePlan workspace mutable lineFrom lineTo target event nextCursor
    EdgeOverlap rawDirected -> do
      let rawEdge = directedEdgeIdIndex rawDirected
      rawOrigin <- readOrigin mutable rawEdge
      rawDestination <- readOrigin mutable (rawEdge `xor` 1)
      let current = vertexIdIndex (mutableCursorAt cursor)
          oriented
            | rawOrigin == current = Just rawEdge
            | rawDestination == current = Just (rawEdge `xor` 1)
            | otherwise = Nothing
      case oriented of
        Nothing ->
          pure
            ( MutablePlanFailed
                ( CorridorBoundaryMissing
                    (mutableCursorAt cursor)
                    (VertexId (fromIntegral rawOrigin))
                )
            )
        Just edge -> do
          prefix <-
            if mutableCursorConflictCount cursor == 0
              then pure cursor
              else finishMutableRecover workspace (mutableCursorAt cursor) cursor
          let !existingAt =
                if mutableCursorConflictCount cursor == 0
                  then mutableCursorHeader cursor
                  else mutableCursorWriteAt prefix
              !edgeDestination =
                VertexId
                  ( fromIntegral
                      (if edge == rawEdge then rawDestination else rawOrigin)
                  )
              !pieceCount = mutableCursorPieceCount prefix + 1
              !nextHeader = existingAt + 2
          writeConstraintProgram workspace existingAt existingProgramTag
          writeConstraintProgram workspace (existingAt + 1) edge
          nextCursor <-
            beginMutableRecover
              workspace
              nextHeader
              edgeDestination
              pieceCount
              True
          continueMutablePlan workspace mutable lineFrom lineTo target event nextCursor

continueMutablePlan
  :: ConstraintWorkspace s
  -> MutableDcel s vertex directed undirected face
  -> Point
  -> Point
  -> VertexId
  -> Intersection
  -> MutableProgramCursor
  -> ST s MutablePlanWalk
continueMutablePlan workspace mutable lineFrom lineTo target event cursor = do
  following <- nextMutableIntersection mutable lineFrom lineTo event
  case following of
    Just nextEvent -> pure (MutablePlanActive nextEvent cursor)
    Nothing -> MutablePlanComplete <$> settleMutableProgram workspace target cursor

beginMutableRecover
  :: ConstraintWorkspace s
  -> Int
  -> VertexId
  -> Int
  -> Bool
  -> ST s MutableProgramCursor
beginMutableRecover workspace header from pieceCount afterOverlap = do
  writeConstraintProgram workspace header recoverProgramTag
  writeConstraintProgram workspace (header + 1) (vertexIdIndex from)
  writeConstraintProgram workspace (header + 2) 0
  writeConstraintProgram workspace (header + 3) 0
  pure
    MutableProgramCursor
      { mutableCursorAt = from
      , mutableCursorHeader = header
      , mutableCursorWriteAt = header + 4
      , mutableCursorConflictCount = 0
      , mutableCursorPieceCount = pieceCount
      , mutableCursorAfterOverlap = afterOverlap
      }

finishMutableRecover
  :: ConstraintWorkspace s
  -> VertexId
  -> MutableProgramCursor
  -> ST s MutableProgramCursor
finishMutableRecover workspace to cursor = do
  writeConstraintProgram workspace (mutableCursorHeader cursor + 2) (vertexIdIndex to)
  writeConstraintProgram workspace (mutableCursorHeader cursor + 3) (mutableCursorConflictCount cursor)
  pure
    cursor
      { mutableCursorAt = to
      , mutableCursorPieceCount = mutableCursorPieceCount cursor + 1
      }

settleMutableProgram
  :: ConstraintWorkspace s
  -> VertexId
  -> MutableProgramCursor
  -> ST s MutableConstraintProgram
settleMutableProgram workspace target cursor
  | mutableCursorAt cursor == target =
      pure
        MutableConstraintProgram
          { mutableProgramWordCount = mutableCursorHeader cursor
          , mutableProgramPieceCount = mutableCursorPieceCount cursor
          }
  | otherwise = do
      finished <- finishMutableRecover workspace target cursor
      pure
        MutableConstraintProgram
          { mutableProgramWordCount = mutableCursorWriteAt finished
          , mutableProgramPieceCount = mutableCursorPieceCount finished
          }

existingProgramTag :: Int
existingProgramTag = 0

recoverProgramTag :: Int
recoverProgramTag = 1

mutablePlanIsActive :: MutablePlanWalk -> Bool
mutablePlanIsActive (MutablePlanActive _ _) = True
mutablePlanIsActive _ = False
