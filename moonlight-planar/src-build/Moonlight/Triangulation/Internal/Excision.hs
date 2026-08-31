{-# LANGUAGE BangPatterns #-}

-- | Excision of a vertex from a thawed mesh: the removal kernel, stated over
-- the mutable arena and publishing nothing.
module Moonlight.Triangulation.Internal.Excision
  ( removeMutable
  ) where

import Control.Monad (forM, when)
import Control.Monad.ST (ST)
import Data.Bits (xor)
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import Moonlight.Triangulation.Internal.DcelOperations.FlipRewrite (flipEdge)
import Moonlight.Triangulation.Internal.DcelOperations.Legalize
  ( legalizeCavityFanScratch
  , legalizeEdges
  )
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  , readScratch
  , writeScratch
  )
import Moonlight.Triangulation.Internal.Types
import Moonlight.Triangulation.Internal.Predicates (orient2d)

-- | The proved outgoing section of one ordinary removal. Its edges live in the
-- operation scratch arena in counter-clockwise order; the record carries only
-- the section's extent and the first outer-face incidence, if any.
data RemovalStar = RemovalStar {-# UNPACK #-} !Int !(Maybe Int)

removeMutable
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> ST s (Either BuildError (Point, vertex, Maybe (Int, Point)))
removeMutable mutable operation vertex = do
  faces <- faceCount mutable
  if faces <= 1
    then removeDegenerate mutable vertex
    else do
      collected <- collectRemovalStar mutable operation vertex
      case collected of
        Left obstruction -> pure (Left obstruction)
        Right (RemovalStar degree outerOutgoing) -> do
          case outerOutgoing of
            Nothing -> removeInterior mutable operation vertex degree
            Just hullEdge -> removeHull mutable operation vertex hullEdge

removeDegenerate
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> ST s (Either BuildError (Point, vertex, Maybe (Int, Point)))
removeDegenerate mutable vertex = do
  vertexCount <- pointCount mutable
  case vertexCount of
    0 ->
      pure
        (Left (RemovalEmptyTriangulation (VertexId (fromIntegral vertex))))
    1 -> do
      writeFaceEdge mutable 0 (-1)
      swapRemoveVertex mutable vertex
    2 -> do
      collected <- collectOutgoing mutable vertex
      case collected of
        Left obstruction -> pure (Left obstruction)
        Right outgoing ->
          case outgoing of
            [edge] -> do
              _ <- clearConstraint mutable edge
              removedEdge <- swapRemoveUndirectedEdge mutable (edge `quot` 2)
              case removedEdge of
                Left obstruction -> pure (Left obstruction)
                Right () -> do
                  let !other = if vertex == 0 then 1 else 0
                  writeVertexOut mutable other (-1)
                  writeFaceEdge mutable 0 (-1)
                  swapRemoveVertex mutable vertex
            _ ->
              pure
                ( Left
                    ( RemovalTwoPointDegreeMismatch
                        (VertexId (fromIntegral vertex))
                        (length outgoing)
                    )
                )
    _ -> removeCollinear mutable vertex

removeCollinear
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> ST s (Either BuildError (Point, vertex, Maybe (Int, Point)))
removeCollinear mutable vertex = do
  collected <- collectOutgoing mutable vertex
  case collected of
    Left obstruction -> pure (Left obstruction)
    Right outgoing ->
      case outgoing of
        [edge] -> do
          let !reversedEdge = edge `xor` 1
          target <- readOrigin mutable reversedEdge
          edgeNext <- readNext mutable edge
          writePrevious mutable edgeNext (edgeNext `xor` 1)
          writeNext mutable (edgeNext `xor` 1) edgeNext
          writeVertexOut mutable target edgeNext
          writeFaceEdge mutable 0 edgeNext
          _ <- clearConstraint mutable edge
          removedEdge <- swapRemoveUndirectedEdge mutable (edge `quot` 2)
          case removedEdge of
            Left obstruction -> pure (Left obstruction)
            Right () -> swapRemoveVertex mutable vertex
        [edge1, edge2] -> do
          let !t1 = edge1 `xor` 1
              !t1Reverse = edge1
              !t2 = edge2 `xor` 1
          constrained1 <- readConstraint mutable edge1
          constrained2 <- readConstraint mutable edge2
          edge2Next <- readNext mutable edge2
          edge2To <- readOrigin mutable t2
          t2Previous <- readPrevious mutable t2
          if edge2Next == t2
            then do
              writeNext mutable t1 t1Reverse
              writePrevious mutable t1Reverse t1
            else do
              writePrevious mutable edge2Next t1
              writeNext mutable t1 edge2Next
              writeNext mutable t2Previous t1Reverse
              writePrevious mutable t1Reverse t2Previous
          writeVertexOut mutable edge2To t1Reverse
          writeOrigin mutable t1Reverse edge2To
          -- The two segments meeting at the removed vertex are welded into
          -- one, and edge1's slot now spans both. It is neither of them.
          resetEdgeData mutable (edge1 `quot` 2)
          writeFaceEdge mutable 0 t1
          _ <- clearConstraint mutable edge1
          _ <- clearConstraint mutable edge2
          when (constrained1 || constrained2) $ do
            _ <- setConstraint mutable edge1
            pure ()
          removedVertex <- swapRemoveVertex mutable vertex
          case removedVertex of
            Left obstruction -> pure (Left obstruction)
            Right result -> do
              removedEdge <- swapRemoveUndirectedEdge mutable (edge2 `quot` 2)
              pure (result <$ removedEdge)
        _ ->
          pure
            ( Left
                ( RemovalCollinearDegreeMismatch
                    (VertexId (fromIntegral vertex))
                    (length outgoing)
                )
            )

removeInterior
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> ST s (Either BuildError (Point, vertex, Maybe (Int, Point)))
removeInterior mutable operation vertex degree = do
  traverse_ recordRing [0 .. degree - 1]
  capacity <- ensureCellCapacity mutable (max 0 (degree - 3)) (max 0 (degree - 2))
  case capacity of
    Left obstruction -> pure (Left obstruction)
    Right () -> do
      -- Taken before the fan appends anything, so it separates the fan's own edges
      -- from the cavity border exactly, the way spade's is_new_edge does.
      !cavityFloor <- (`quot` 2) <$> directedEdgeCount mutable
      remeshed <- remeshRingScratch mutable operation degree
      case remeshed of
        Left obstruction -> pure (Left obstruction)
        Right newEdgeCount -> do
          legalizeCavityFanScratch
            mutable
            operation
            cavityFloor
            (3 * degree)
            newEdgeCount
          edgesToRemove <-
            traverse
              (readScratch operation . (degree +))
              [0 .. degree - 1]
          facesToRemove <-
            traverse
              (readScratch operation . (2 * degree +))
              [0 .. degree - 1]
          cleaned <- cleanupEdgesAndFaces mutable edgesToRemove facesToRemove
          case cleaned of
            Left obstruction -> pure (Left obstruction)
            Right () -> swapRemoveVertex mutable vertex
 where
  recordRing index = do
    edge <- readScratch operation index
    following <- readNext mutable edge
    face <- readFace mutable edge
    writeScratch operation index following
    writeScratch operation (degree + index) (edge `quot` 2)
    writeScratch operation (2 * degree + index) face

-- | Fan the cavity a removal leaves. The border arrives in ring order, which is
-- the order the fan consumes it in.
remeshRingScratch
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> ST s (Either BuildError Int)
remeshRingScratch mutable operation degree
  | degree < 3 = pure (Left (RemovalBorderTooShort degree))
  | otherwise = do
      inner0 <- readScratch operation 0
      fanOrigin <- readOrigin mutable inner0
      build fanOrigin 1 inner0 0
 where
  build !fanOrigin !index !innerEdge !newEdgeCount
    | index == degree - 2 = do
      innerNext <- readScratch operation index
      innerPrevious <- readScratch operation (index + 1)
      newFace <- addFace mutable innerEdge
      writeFace mutable innerEdge newFace
      writeFace mutable innerPrevious newFace
      writeFace mutable innerNext newFace
      writeNext mutable innerEdge innerNext
      writePrevious mutable innerNext innerEdge
      writePrevious mutable innerEdge innerPrevious
      writeNext mutable innerPrevious innerEdge
      writePrevious mutable innerPrevious innerNext
      writeNext mutable innerNext innerPrevious
      previousOrigin <- readOrigin mutable innerPrevious
      nextOrigin <- readOrigin mutable innerNext
      writeVertexOut mutable previousOrigin innerPrevious
      writeVertexOut mutable nextOrigin innerNext
      writeVertexOut mutable fanOrigin innerEdge
      pure (Right newEdgeCount)
    | index < degree - 2 = do
      outerEdge <- readScratch operation index
      outerFrom <- readOrigin mutable outerEdge
      outerTo <- readOrigin mutable (outerEdge `xor` 1)
      (newEdge, newTwin) <- addEdge mutable outerTo fanOrigin
      newFace <- addFace mutable newEdge
      writeNext mutable newEdge innerEdge
      writePrevious mutable newEdge outerEdge
      writeFace mutable newEdge newFace
      writeNext mutable newTwin 0
      writePrevious mutable newTwin 0
      writeFace mutable newTwin 0
      writeFace mutable outerEdge newFace
      writeNext mutable outerEdge newEdge
      writePrevious mutable outerEdge innerEdge
      writePrevious mutable innerEdge newEdge
      writeNext mutable innerEdge outerEdge
      writeFace mutable innerEdge newFace
      writeFaceEdge mutable newFace newEdge
      writeVertexOut mutable outerFrom outerEdge
      writeScratch operation (3 * degree + newEdgeCount) newEdge
      build
        fanOrigin
        (index + 1)
        newTwin
        (newEdgeCount + 1)
    | otherwise =
      pure (Left (RemovalBorderArityMismatch (degree - index)))

removeHull :: MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s (Either BuildError (Point, vertex, Maybe (Int, Point)))
removeHull mutable operation vertex loopEnd = do
  loopStart <- counterClockwiseMutable mutable loopEnd
  loopEndNext <- readNext mutable loopEnd
  collected <- collectConvexStrip loopEnd loopStart [] []
  case collected of
    Left obstruction -> pure (Left obstruction)
    Right (!convexEdges, !edgesToValidate) -> do
      let !strip = convexEdges ++ [loopEndNext]
      (!edgesToRemove, !facesToRemove) <- disconnectStrip strip
      legalizeEdges mutable operation edgesToValidate
      cleaned <- cleanupEdgesAndFaces mutable edgesToRemove facesToRemove
      case cleaned of
        Left obstruction -> pure (Left obstruction)
        Right () -> swapRemoveVertex mutable vertex
 where
  collectConvexStrip !end !current !convexReversed !validate = do
    nextCurrent <- counterClockwiseMutable mutable current
    edge <- readNext mutable current
    repaired <- repairConvexity (edge : convexReversed) validate
    case repaired of
      Left obstruction -> pure (Left obstruction)
      Right (!repairedReversed, !validate') ->
        if nextCurrent == end
          then pure (Right (reverse repairedReversed, validate'))
          else collectConvexStrip end nextCurrent repairedReversed validate'

  repairConvexity !edgesReversed !validate =
    case edgesReversed of
      edge2 : edge1 : restReversed -> do
        target <- readOrigin mutable (edge2 `xor` 1)
        from <- edgeOriginPoint mutable edge1
        to <- edgeOriginPoint mutable (edge1 `xor` 1)
        targetPoint <- pointAt mutable target
        if orient2d from to targetPoint == GT
          then do
            previousEdge <- readPrevious mutable edge2
            let !toFlip = previousEdge `xor` 1
            rewritten <- flipEdge mutable toFlip
            case rewritten of
              Left obstruction -> pure (Left obstruction)
              Right () -> do
                addCounter operation CounterEdgeFlips 1
                repairConvexity (toFlip : restReversed) (toFlip : validate)
          else pure (Right (edgesReversed, validate))
      _ -> pure (Right (edgesReversed, validate))

  disconnectStrip strip = do
    removed <- forM strip $ \edge -> do
      previousSpoke <- readPrevious mutable edge
      face <- readFace mutable edge
      from <- readOrigin mutable edge
      ccw <- counterClockwiseMutable mutable edge
      predecessor <- readPrevious mutable ccw
      writeNext mutable predecessor edge
      writePrevious mutable edge predecessor
      writeFace mutable edge 0
      writeFaceEdge mutable 0 edge
      writeVertexOut mutable from edge
      pure (previousSpoke `quot` 2, face)
    pure (map fst removed, map snd removed)

cleanupEdgesAndFaces :: MutableDcel s vertex directed undirected face -> [Int] -> [Int] -> ST s (Either BuildError ())
cleanupEdgesAndFaces mutable rawEdges rawFaces = do
  let !edges = sortUniqueDesc rawEdges
      !faces = sortUniqueDesc (filter (> 0) rawFaces)
  removedEdges <- traverseUntilFailure (swapRemoveUndirectedEdge mutable) edges
  case removedEdges of
    Left obstruction -> pure (Left obstruction)
    Right () -> traverseUntilFailure (swapRemoveFace mutable) faces
 where
  traverseUntilFailure
    :: (Int -> ST s (Either BuildError ()))
    -> [Int]
    -> ST s (Either BuildError ())
  traverseUntilFailure action =
    foldr
      ( \item continuation -> do
          outcome <- action item
          case outcome of
            Left obstruction -> pure (Left obstruction)
            Right () -> continuation
      )
      (pure (Right ()))

-- | Descending, deduplicated. Both properties are load-bearing: swap-remove
-- must retire the high index first (a lower index shifts under it), and a
-- duplicated index would be retired twice.
--
-- A removal hands over its vertex's degree, which is small on ordinary meshes
-- and unbounded in the worst case, so the shape is chosen by size. Insertion
-- sort wins outright while the ring is short — measured 6.75 against
-- 8.24 KiB/removal for @IntSet@ on the n=10000 lane — and is quadratic, so a
-- high-degree ring goes to the ordered set that carries the asymptotics.
--
-- The lazy 'foldl' is deliberate: its accumulator is the output structure.
-- 'foldl'' forced each intermediate spine and measured 7.31 versus
-- 6.75 KiB/removal on the same lane.
sortUniqueDesc :: [Int] -> [Int]
sortUniqueDesc values
  | exceedsInsertionRing values = IntSet.toDescList (IntSet.fromList values)
  | otherwise = foldl insertUnique [] values
 where
  insertUnique :: [Int] -> Int -> [Int]
  insertUnique sorted value = go sorted
   where
    go [] = [value]
    go (first : rest) = case compare value first of
      GT -> value : first : rest
      EQ -> first : rest
      LT -> first : go rest

-- | Whether a ring is long enough to owe the ordered set its logarithm,
-- decided without measuring the whole list: the insertion path is chosen by
-- the prefix, never by a full traversal.
exceedsInsertionRing :: [Int] -> Bool
exceedsInsertionRing = not . null . drop insertionRingLimit

insertionRingLimit :: Int
insertionRingLimit = 32

collectRemovalStar
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> ST s (Either BuildError RemovalStar)
collectRemovalStar mutable operation vertex = do
  start <- readVertexOut mutable vertex
  if start < 0
    then pure (Right (RemovalStar 0 Nothing))
    else do
      halfEdges <- directedEdgeCount mutable
      let !budget = halfEdges + 1
          go !remaining !current !seen !degree !outerEdge
            | remaining <= 0 =
                pure
                  ( Left
                      ( RemovalOutgoingCycleDidNotTerminate
                          (VertexId (fromIntegral vertex))
                          (DirectedEdgeId (fromIntegral current))
                          budget
                      )
                  )
            | seen && current == start =
                pure (Right (RemovalStar degree outerEdge))
            | otherwise = do
                writeScratch operation degree current
                face <- readFace mutable current
                _ <- clearConstraint mutable current
                previousEdge <- readPrevious mutable current
                let !nextOuter =
                      case outerEdge of
                        Just edge -> Just edge
                        Nothing
                          | face == 0 -> Just current
                          | otherwise -> Nothing
                go
                  (remaining - 1)
                  (previousEdge `xor` 1)
                  True
                  (degree + 1)
                  nextOuter
      go budget start False 0 Nothing

collectOutgoing
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> ST s (Either BuildError [Int])
collectOutgoing mutable vertex = do
  start <- readVertexOut mutable vertex
  if start < 0
    then pure (Right [])
    else do
      halfEdges <- directedEdgeCount mutable
      let !budget = halfEdges + 1
          go !remaining !current !seen !result
            | remaining <= 0 =
                pure
                  ( Left
                      ( RemovalOutgoingCycleDidNotTerminate
                          (VertexId (fromIntegral vertex))
                          (DirectedEdgeId (fromIntegral current))
                          budget
                      )
                  )
            | seen && current == start = pure (Right (reverse result))
            | otherwise = do
                previousEdge <- readPrevious mutable current
                go (remaining - 1) (previousEdge `xor` 1) True (current : result)
      go budget start False []

counterClockwiseMutable :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
counterClockwiseMutable mutable edge = (`xor` 1) <$> readPrevious mutable edge
