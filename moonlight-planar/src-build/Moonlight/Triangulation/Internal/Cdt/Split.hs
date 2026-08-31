{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The splitting program: corridors that divide every constraint they cross,
-- driven across as few sealed transactions as the vertex reservation allows.
module Moonlight.Triangulation.Internal.Cdt.Split
  ( addConstraintAndSplit
  , addConstraintsAndSplit
  ) where

import Control.Monad.ST (ST)
import Data.Bits (xor)
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Moonlight.Triangulation.Dcel as Dcel
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Internal.Cdt.Combinators
  ( asConstraintStep
  , bindMutable
  )
import Moonlight.Triangulation.Internal.Cdt.Corridor
  ( constraintWorkspaceFor
  , scanMutableConstraint
  )
import Moonlight.Triangulation.Internal.Cdt.Query
  ( findMutableEdge
  , getConflictingEdgesBetweenVertices
  , validateEndpoints
  )
import Moonlight.Triangulation.Internal.Cdt.Recovery
  ( applyMutableConstraint
  , recoverMutableRequest
  )
import Moonlight.Triangulation.Internal.Cdt.Segment (retireConstraintEdge)
import Moonlight.Triangulation.Internal.Cdt.Site (placeConstraintEndpoint)
import Moonlight.Triangulation.Internal.Cdt.Types
import Moonlight.Triangulation.Internal.Growable
  ( GrowableWord32
  , newGrowableWord32
  )
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState (OperationState)
import Moonlight.Triangulation.Internal.Paged
  ( TransactionShape (DenseTransaction, LocalTransaction)
  )
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Transaction (runUnmeasuredTransaction)
import Moonlight.Triangulation.Internal.Types
import Moonlight.Triangulation.Math

-- | The part of a splitting request that does not move: the payload maker, the
-- endpoints the failure report must name, the iteration budget, and the
-- transaction's vertex reservation, past which the corridor suspends rather
-- than writes.
data SplitRequest vertex = SplitRequest
  { splitRequestMakeVertex :: !(Point -> vertex)
  , splitRequestFrom :: !VertexId
  , splitRequestTo :: !VertexId
  , splitRequestBudget :: {-# UNPACK #-} !Int
  , splitRequestVertexBound :: {-# UNPACK #-} !Int
  }

-- | The part that does: how much budget is left, which vertex the unrecovered
-- remainder starts at, and the path and edge count accumulated behind it. The
-- path runs newest-first and is reversed once, at publication.
data SplitCursor = SplitCursor
  { splitCursorRemaining :: {-# UNPACK #-} !Int
  , splitCursorAt :: !VertexId
  , splitCursorPath :: ![DirectedEdgeId]
  , splitCursorAdded :: {-# UNPACK #-} !Int
  }

-- | How a corridor run ends inside its transaction: settled at the target, or
-- suspended mid-corridor because the next division would outgrow the vertex
-- reservation. A suspension is not a refusal; the driver publishes,
-- re-reserves against the published mesh, and resumes at the cursor.
data SplitStep
  = SplitSettled !SplitCursor
  | SplitSuspended !SplitCursor

-- | What one settled request leaves behind.
data SplitReceipt = SplitReceipt
  { splitReceiptPath :: !(V.Vector DirectedEdgeId)
  , splitReceiptAdded :: {-# UNPACK #-} !Int
  }

-- | Glue the requested segment, dividing every constraint it crosses at the
-- crossing point rather than refusing it. One transaction carries the whole
-- corridor: a singleton is a batch of one under the shared driver.
addConstraintAndSplit
  :: (Point -> vertex)
  -> Triangulation 'Constrained vertex directed undirected face
  -> VertexId
  -> VertexId
  -> Either (CdtError) (ConstraintResult vertex directed undirected face)
addConstraintAndSplit makeVertex triangulation from to = do
  (published, receipts) <-
    driveConstraintSplits LocalTransaction makeVertex triangulation (V.singleton (from, to))
  pure
    ( case receipts V.!? 0 of
        Just receipt ->
          ConstraintRecoveryResult
            { constraintRecoveryTriangulation = published
            , constraintRecoveryPathReceipt = splitReceiptPath receipt
            , constraintRecoveryAddedEdges = splitReceiptAdded receipt
            }
        Nothing -> ConstraintRecoveryResult published V.empty 0
    )

-- | Divide every requested segment inside as few transactions as the vertex
-- reservation allows: usually one, which is the referent semantics for a
-- splitting batch. Nothing is republished between corridors unless a corridor
-- crosses constraints created by an earlier request in the same batch --
-- growth no census against the base can see. In that one case the corridor
-- suspends, the chunk publishes, and the driver re-reserves against the
-- published mesh and resumes at the suspended cursor.
addConstraintsAndSplit
  :: (Point -> vertex)
  -> Triangulation 'Constrained vertex directed undirected face
  -> V.Vector (VertexId, VertexId)
  -> Either (CdtError) (ConstraintSplitBatchResult vertex directed undirected face)
addConstraintsAndSplit makeVertex triangulation requests = do
  (published, receipts) <- driveConstraintSplits DenseTransaction makeVertex triangulation requests
  pure
    ConstraintRecoveryResult
      { constraintRecoveryTriangulation = published
      , constraintRecoveryPathReceipt = V.map splitReceiptPath receipts
      , constraintRecoveryAddedEdges = V.sum (V.map splitReceiptAdded receipts)
      }

-- | The obstruction census for one request against a published mesh: how many
-- constrained crossings its corridor holds, and the first of them. The first
-- crossing is a valid local section of the next transaction before any rewrite
-- occurs, so a chunk's first request carries it across the thaw boundary
-- instead of immediately rediscovering it; every later request rescans,
-- because earlier corridors may have rewritten the topology the witness names.
splitCensus
  :: Triangulation 'Constrained vertex directed undirected face
  -> (VertexId, VertexId)
  -> (Int, Maybe DirectedEdgeId)
splitCensus triangulation (from, to)
  | from == to = (0, Nothing)
  | otherwise =
      List.foldl'
        countCrossing
        (0, Nothing)
        (getConflictingEdgesBetweenVertices triangulation from to)
 where
  countCrossing :: (Int, Maybe DirectedEdgeId) -> DirectedEdgeId -> (Int, Maybe DirectedEdgeId)
  countCrossing (!count, firstCrossing) crossing =
    ( count + 1
    , case firstCrossing of
        Just first -> Just first
        Nothing -> Just crossing
    )

-- | Interpret splitting requests in order across as few sealed transactions as
-- possible. Each chunk reserves against the exact obstruction census of every
-- remaining request, so a chunk always settles at least its first request:
-- that census ran against the very mesh the chunk thawed and is exact before
-- any in-session rewrite. Termination follows.
driveConstraintSplits
  :: forall vertex directed undirected face.
     TransactionShape
  -> (Point -> vertex)
  -> Triangulation 'Constrained vertex directed undirected face
  -> V.Vector (VertexId, VertexId)
  -> Either
      (CdtError)
      ( Triangulation 'Constrained vertex directed undirected face
      , V.Vector SplitReceipt
      )
driveConstraintSplits shape makeVertex base requests = do
  V.mapM_ (uncurry (validateEndpoints base)) requests
  if V.all (uncurry (==)) requests
    then Right (base, V.replicate (V.length requests) emptySplitReceipt)
    else advance base 0 [] Nothing
 where
  emptySplitReceipt = SplitReceipt V.empty 0

  advance
    :: Triangulation 'Constrained vertex directed undirected face
    -> Int
    -> [SplitReceipt]
    -> Maybe SplitCursor
    -> Either
        CdtError
        ( Triangulation 'Constrained vertex directed undirected face
        , V.Vector SplitReceipt
        )
  advance triangulation start settled resumed
    | start >= V.length requests =
        Right (triangulation, V.fromList (reverse settled))
    | otherwise = do
        let remaining = V.drop start requests
            firstRound = start == 0 && case resumed of
              Nothing -> True
              Just _ -> False
            -- The opening chunk reserves optimistically -- one crossing per
            -- request -- rather than walking every corridor for an exact
            -- census before any work begins. A request that outgrows the
            -- reservation suspends mid-corridor with its partial work kept,
            -- and the next round censuses exactly against the published mesh,
            -- so heavy batches pay at most one speculative chunk.
            censuses
              | firstRound = V.replicate (V.length remaining) (1, Nothing)
              | otherwise = V.map (splitCensus triangulation) remaining
            !reservedSites = V.sum (V.map fst censuses)
            !additionalCapacity = 2 * reservedSites + 8
            -- The walk budget bounds divisions per request; constraints born
            -- inside the chunk are covered by the reservation term.
            !budget =
              2 * (Dcel.numConstraints triangulation + 2 * reservedSites + V.length remaining)
                + Dcel.numUndirectedEdges triangulation
                + 8
        ((receipts, suspended), published) <-
          runUnmeasuredTransaction
            CdtBuildError
            shape
            triangulation
            additionalCapacity
            $ \mutable operation -> do
            programWords <- newGrowableWord32 32
            runSplitChunk
              makeVertex
              mutable
              operation
              programWords
              remaining
              censuses
              (pointCapacity mutable)
              budget
              resumed
        advance published (start + length receipts) (receipts ++ settled) suspended

-- | One sealed transaction over a prefix of the remaining requests. Receipts
-- run newest-first; a suspension carries no receipt, so the settled count is
-- exactly the receipt count.
runSplitChunk
  :: (Point -> vertex)
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> GrowableWord32 s
  -> V.Vector (VertexId, VertexId)
  -> V.Vector (Int, Maybe DirectedEdgeId)
  -> Int
  -> Int
  -> Maybe SplitCursor
  -> ST s (Either CdtError ([SplitReceipt], Maybe SplitCursor))
runSplitChunk makeVertex mutable operation programWords requests censuses capacity budget resumed =
  go 0 [] resumed
 where
  go offset receipts pending
    | offset >= V.length requests = pure (Right (receipts, Nothing))
    | otherwise =
        let (from, to) = requests V.! offset
        in case pending of
             Nothing
               | from == to ->
                   go (offset + 1) (SplitReceipt V.empty 0 : receipts) Nothing
             _ ->
               let witness = if offset == 0 then snd (censuses V.! 0) else Nothing
                   cursor = case pending of
                     Just resumedCursor -> resumedCursor
                     Nothing -> SplitCursor budget from [] 0
                   request =
                     SplitRequest
                       { splitRequestMakeVertex = makeVertex
                       , splitRequestFrom = from
                       , splitRequestTo = to
                       , splitRequestBudget = budget
                       , splitRequestVertexBound = capacity
                       }
               in splitConstraintCorridor request programWords mutable operation witness cursor
                    `bindMutable` \step ->
                      case step of
                        SplitSettled settledCursor ->
                          go (offset + 1) (receiptOf settledCursor : receipts) Nothing
                        SplitSuspended suspendedCursor ->
                          pure (Right (receipts, Just suspendedCursor))

  receiptOf cursor =
    SplitReceipt
      { splitReceiptPath = V.fromList (reverse (splitCursorPath cursor))
      , splitReceiptAdded = splitCursorAdded cursor
      }

splitConstraintCorridor
  :: SplitRequest vertex
  -> GrowableWord32 s
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Maybe DirectedEdgeId
  -> SplitCursor
  -> ST s (Either (CdtError) SplitStep)
splitConstraintCorridor request programWords mutable operation initialCrossing initialCursor =
  case initialCrossing of
    Just crossing -> divide initialCursor crossing
    Nothing -> descend initialCursor
 where
  !target = splitRequestTo request

  descend cursor
    | splitCursorRemaining cursor <= 0 =
        pure
          ( Left
              ( ConstraintSplitBudgetExhausted
                  (splitRequestFrom request)
                  target
                  (splitRequestBudget request)
              )
          )
    | otherwise = do
        workspace <- constraintWorkspaceFor programWords mutable
        scanMutableConstraint workspace mutable (splitCursorAt cursor) target
          `bindMutable` \scanned ->
            case scanned of
              MutableConstraintScanAdmitted program ->
                recoverMutableRequest workspace mutable operation program
                  `bindMutable` \final ->
                    pure
                      ( Right
                          ( SplitSettled
                              (advanceCursor cursor (splitCursorRemaining cursor) target final)
                          )
                      )
              MutableConstraintScanBlocked crossing -> divide cursor crossing

  divide cursor crossing = do
    occupied <- pointCount mutable
    if occupied + 2 > splitRequestVertexBound request
      then pure (Right (SplitSuspended cursor))
      else divideWithin cursor crossing

  divideWithin cursor crossing = do
    let !crossingEdge = directedEdgeIdIndex crossing
        !oldConstraint = asUndirected crossing
    segmentFrom <- pointAt mutable (vertexIdIndex (splitCursorAt cursor))
    segmentTo <- pointAt mutable (vertexIdIndex target)
    oldFrom <- readOrigin mutable crossingEdge
    oldTo <- readOrigin mutable (crossingEdge `xor` 1)
    edgeFrom <- pointAt mutable oldFrom
    edgeTo <- pointAt mutable oldTo
    -- The split point lies on the crossing edge, so both its incident faces
    -- already contain it and the exact walk settles on its first probe. Without
    -- this the locate is unhinted, and an unhinted locate descends vertex by
    -- vertex from the arena's first face -- a distance that grows with the mesh
    -- while the corridor it is splitting stays a fixed few faces wide. A hull
    -- edge carries the outer face on one side, which the locator would refuse.
    incidentFace <- readFace mutable crossingEdge
    twinFace <- readFace mutable (crossingEdge `xor` 1)
    let !splitHint = Just (if incidentFace > 0 then incidentFace else twinFace)
    let !oldEndpoints =
          ( VertexId (fromIntegral oldFrom)
          , VertexId (fromIntegral oldTo)
          )
    case lineIntersection crossing segmentFrom segmentTo edgeFrom edgeTo of
      Left indeterminate -> pure (Left indeterminate)
      Right splitPoint ->
        -- The split lands where the intersection says, not where a round trip
        -- through 'makeVertex' happens to put it.
        asConstraintStep
          ( placeConstraintEndpoint
              mutable
              operation
              splitHint
              splitPoint
              (splitRequestMakeVertex request splitPoint)
          )
          `bindMutable` \splitVertex ->
            repairIfRounded oldConstraint oldEndpoints splitVertex `bindMutable` \() ->
              applyMutableConstraint programWords mutable operation (splitCursorAt cursor) splitVertex
                `bindMutable` \prefix ->
                  case prefix of
                    MutableConstraintRejected blocking ->
                      pure (Left (ConstraintIntersection blocking))
                    MutableConstraintAccepted segment ->
                      descend
                        (advanceCursor cursor (splitCursorRemaining cursor - 1) splitVertex segment)

  repairIfRounded oldConstraint oldEndpoints splitVertex = do
    divided <- mutableEdgeWasSplit mutable oldConstraint oldEndpoints splitVertex
    if divided
      then pure (Right ())
      else repairRoundedSplit programWords mutable operation oldConstraint oldEndpoints splitVertex

  advanceCursor cursor remaining reached segment =
    cursor
      { splitCursorRemaining = remaining
      , splitCursorAt = reached
      , splitCursorPath = accumulatedRequestPath segment ++ splitCursorPath cursor
      , splitCursorAdded = splitCursorAdded cursor + accumulatedRequestAddedEdges segment
      }

-- | Whether the inserted vertex actually divided the constraint it was placed
-- on. It did if that constraint is gone — the split retired the identity — or
-- if both halves now carry one. Rounding can place the vertex somewhere that
-- leaves the old constraint standing and neither half glued.
mutableEdgeWasSplit
  :: MutableDcel s vertex directed undirected face
  -> UndirectedEdgeId
  -> (VertexId, VertexId)
  -> VertexId
  -> ST s Bool
mutableEdgeWasSplit mutable edge (from, to) splitVertex = do
  halfEdges <- directedEdgeCount mutable
  let !directed = directedEdgeIdIndex (normalizedDirected edge)
  stillConstrained <-
    if directed >= halfEdges
      then pure False
      else readConstraint mutable directed
  if not stillConstrained
    then pure True
    else do
      leading <- mutableConstraintBetween mutable from splitVertex
      if leading
        then mutableConstraintBetween mutable splitVertex to
        else pure False

mutableConstraintBetween
  :: MutableDcel s vertex directed undirected face
  -> VertexId
  -> VertexId
  -> ST s Bool
mutableConstraintBetween mutable from to = do
  found <- findMutableEdge mutable (vertexIdIndex from) (vertexIdIndex to)
  case found of
    Nothing -> pure False
    Just edge -> readConstraint mutable edge

-- | The split point rounded onto neither half of the constraint it was meant to
-- divide. Retire the stale identity and glue both halves against the vertex
-- that was actually sited. What this recovers is repair rather than the
-- requested segment, so it contributes nothing to the request's path.
repairRoundedSplit
  :: GrowableWord32 s
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> UndirectedEdgeId
  -> (VertexId, VertexId)
  -> VertexId
  -> ST s (Either (CdtError) ())
repairRoundedSplit programWords mutable operation oldEdge (from, to) splitVertex =
  retireConstraintEdge mutable operation oldEdge `bindMutable` \() ->
    glue from splitVertex `bindMutable` \() -> glue splitVertex to
 where
  glue start end =
    applyMutableConstraint programWords mutable operation start end `bindMutable` \recovered ->
      pure $ case recovered of
        MutableConstraintRejected blocking -> Left (ConstraintIntersection blocking)
        MutableConstraintAccepted _ -> Right ()

lineIntersection
  :: DirectedEdgeId
  -> Point
  -> Point
  -> Point
  -> Point
  -> Either (CdtError) (Point)
lineIntersection crossing (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy)
  | denominator == 0 =
      Left (ConstraintSplitIntersectionIndeterminate crossing)
  | otherwise =
      Right (canonicalPoint (Point (ax + t * rx) (ay + t * ry)))
 where
  rx = bx - ax
  ry = by - ay
  sx = dx - cx
  sy = dy - cy
  denominator = rx * sy - ry * sx
  t = ((cx - ax) * sy - (cy - ay) * sx) / denominator
