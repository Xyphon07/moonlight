{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Moonlight.Triangulation.Internal.Mutable
  ( MutableDcel (..)
  , DenseMutableDcel
  , MutableTopology (..)
  , denseMutableDcel
  , denseMutableOwner
  , DcelCapacity
  , generalDcelCapacity
  , planarDcelCapacity
  , exactDcelCapacity
  , newMutableDcel
  , DefaultedVertexDcel
  , newMutableDcelWithVertexDefault
  , defaultedVertexDcel
  , defaultedDenseVertexDcel
  , thawTriangulation
  , thawTriangulationDense
  , freezeTriangulation
  , freezeTriangulationWithStats
  , pointCapacity
  , halfEdgeCapacity
  , pointCount
  , connectedCount
  , directedEdgeCount
  , faceCount
  , pointAt
  , lookupPointVertex
  , activatePointIndex
  , activateBatchPointIndex
  , discardBatchPointIndex
  , identityIndexActive
  , readPointX
  , readPointY
  , writePoint
  , vertexDataAt
  , writeVertexData
  , payloadsPristine
  , resetEdgeData
  , resetFaceData
  , edgeOriginPoint
  , appendVertex
  , appendVertexCoordinates
  , appendDefaultVertexCoordinates
  , NextVertexSlot
  , nextVertexSlot
  , nextVertexSlotIndex
  , appendVertexCoordinatesAtSlot
  , ensurePointCapacity
  , markConnected
  , isConnected
  , addEdge
  , addEdgeBlock
  , addFace
  , addFaceBlock
  , denseAddEdgeBlock
  , denseAddFaceBlock
  , denseInitializeUnconstrainedEdgeBlock
  , ensureCellCapacity
  , truncatePoints
  , truncateDirectedEdges
  , truncateFaces
  , swapRemoveUndirectedEdge
  , swapRemoveFace
  , swapRemoveVertex
  , linkEdges
  , setCycle3
  , faceEdges
  , readOrigin
  , writeOrigin
  , readNext
  , writeNext
  , readPrevious
  , writePrevious
  , readFace
  , writeFace
  , readVertexOut
  , writeVertexOut
  , readFaceEdge
  , writeFaceEdge
  , readConstraint
  , denseReadPointX
  , denseReadPointY
  , denseWriteFreshDefaultPoint
  , denseCommitFreshDefaultPoints
  , denseReadOrigin
  , denseWriteOrigin
  , denseReadNext
  , denseWriteNext
  , denseReadPrevious
  , denseWritePrevious
  , denseReadFace
  , denseWriteFace
  , denseWriteVertexOut
  , denseMarkFreshConnected
  , denseCommitFreshConnections
  , denseReadFaceEdge
  , denseWriteFaceEdge
  , denseReadConstraint
  , denseLinkEdges
  , denseSetCycle3
  , denseFaceEdges
  , setConstraint
  , clearConstraint
  ) where

import Control.Monad (foldM, forM_, unless, when)
import Data.Bits (xor)
import Control.Monad.ST (ST)
import qualified Data.IntSet as IntSet
import Data.STRef
  ( STRef
  , modifySTRef'
  , newSTRef
  , readSTRef
  , writeSTRef
  )
import Data.Word (Word8, Word32)
import Moonlight.Triangulation.Internal.HandleDefs
  ( DirectedEdgeId (..)
  , FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  )
import Moonlight.Triangulation.Internal.BoxedPaged
import Moonlight.Triangulation.Internal.PackedIndex (noIndex, packIndex, unpackIndex)
import Moonlight.Triangulation.Internal.Paged
import Moonlight.Triangulation.Internal.PointIndex
  ( MutablePointIndex
  , MutablePointIndexUpdate (..)
  , PointIndex
  , buildPointIndex
  , emptyPointIndex
  , insertPointIndex
  , lookupMutablePoint
  , newMutablePointIndex
  , pointIndexCandidates
  , relocateMutablePoint
  , relocatePointIndex
  , removeMutablePoint
  , removePointIndex
  , seedMutablePointIndex
  )
import Moonlight.Triangulation.Math (canonicalPoint)
import Moonlight.Triangulation.Internal.Representation (Triangulation (..))
import Moonlight.Triangulation.Internal.Types (BuildError (..), ElementDefaults (..), Point (..))
-- | Dormant carries the inherited index through mutations as unforced pure
-- updates; persistent Active is the same index forced and kept strict because
-- a singleton lookup already proved someone is asking. Batch Active is the
-- existing open-addressed owner scoped to one dense identity program. Missing
-- means publication owes a lazy whole-mesh rebuild to any eventual asker.
data MutablePointIndexState s
  = DormantPointIndex PointIndex
  | ActivePersistentPointIndex !PointIndex
  | ActiveBatchPointIndex !(MutablePointIndex s)
  | MissingPointIndex

data MutableDcel s vertex directed undirected face = MutableDcel
  { mdPointX :: !(MutablePaged s Double)
  , mdPointY :: !(MutablePaged s Double)
  , mdPointIndex :: !(STRef s (MutablePointIndexState s))
  , mdVertexOut :: !(MutablePaged s Word32)
  , mdVertexData :: !(MutableBoxedPaged s vertex)
  , mdNewConnected :: !(MutablePaged s Word8)
  , mdRecycledNew :: !(STRef s IntSet.IntSet)
  , mdHalfTopology :: !(MutablePaged s Word32)
  , mdDirectedData :: !(MutableBoxedPaged s directed)
  , mdUndirectedData :: !(MutableBoxedPaged s undirected)
  , mdConstraint :: !(MutablePaged s Word8)
  , mdFaceEdge :: !(MutablePaged s Word32)
  , mdFaceData :: !(MutableBoxedPaged s face)
  , mdPointCount :: !(STRef s Int)
  , mdConnectedCount :: !(STRef s Int)
  , mdHalfCount :: !(STRef s Int)
  , mdFaceCount :: !(STRef s Int)
  , mdConstraintCount :: !(STRef s Int)
  , mdConstraintEdges :: !(STRef s IntSet.IntSet)
  , mdLastFace :: !(STRef s Int)
  , mdInitialPointCount :: {-# UNPACK #-} !Int
  , mdPointCapacity :: {-# UNPACK #-} !Int
  , mdHalfCapacity :: {-# UNPACK #-} !Int
  , mdFaceCapacity :: {-# UNPACK #-} !Int
  , -- | No element payload plane held a materialized page at thaw. Nothing
    -- inside a transaction can write one: the three planes are written only
    -- through the persistent setters, which run outside one, and both the
    -- rewrite reset and the swap-compaction relocation below are no-ops while
    -- this holds. So it is constant for the transaction's whole life, and a
    -- rewrite decides in a predictable branch that it has no label to move.
    mdPayloadsPristine :: !Bool
  , mdElementDefaults :: !(ElementDefaults directed undirected face)
  }

-- | Physical bounds for one fresh mutable DCEL. Vertex, directed-edge and
-- face sections are stated independently because a known planar construction
-- and an arbitrary append program obey different allocation laws. Keeping the
-- law in the constructor input prevents every fresh caller from silently
-- inheriting the loosest reservation and then copying its dead tail at freeze.
data DcelCapacity = DcelCapacity
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int

-- | Conservative append-program capacity, preserving the historical slack
-- for operations whose intermediate cells may outlive their final topology.
generalDcelCapacity :: Int -> DcelCapacity
generalDcelCapacity maximumVertices =
  let !vertices = max 1 maximumVertices
   in DcelCapacity vertices (max 2 (8 * vertices + 16)) (max 1 (3 * vertices + 8))

-- | Tight fresh planar capacity. Circle sweep allocates monotonically and no
-- rewrite abandons a cell, so Euler's bounds plus seed slack are authoritative:
-- at most @6n@ directed edges and @2n@ faces.
--
-- The separated seam merge is the second lawful caller. It copies two planar
-- sources and then only adds: stitching creates seam cells, legalization
-- rewires without allocating, and nothing is abandoned. Its peak is therefore
-- its published result, a planar triangulation on the summed vertex count.
planarDcelCapacity :: Int -> DcelCapacity
planarDcelCapacity maximumVertices =
  let !vertices = max 1 maximumVertices
   in DcelCapacity vertices (max 2 (6 * vertices + 16)) (max 1 (2 * vertices + 8))

-- | Exact section bounds for reconstruction programs that already know the
-- published cardinalities they will materialize.
exactDcelCapacity :: Int -> Int -> Int -> DcelCapacity
exactDcelCapacity vertices directedEdges faces =
  DcelCapacity (max 1 vertices) (max 2 directedEdges) (max 1 faces)

-- | The contiguous physical section of one mutable DCEL. It is a refinement
-- of the canonical owner, not a second mesh: every plane below is the exact
-- flat vector already held by 'denseMutableOwner'. Circle sweep and dense
-- sessions discharge this section once, then interpret their hot local
-- rewrites without re-testing each 'MutablePaged' sum at every cell.
data DenseMutableDcel s vertex directed undirected face = DenseMutableDcel
  { dmdOwner :: !(MutableDcel s vertex directed undirected face)
  , dmdPointX :: !(FlatMutablePaged s Double)
  , dmdPointY :: !(FlatMutablePaged s Double)
  , dmdVertexOut :: !(FlatMutablePaged s Word32)
  , dmdNewConnected :: !(FlatMutablePaged s Word8)
  , dmdHalfTopology :: !(FlatMutablePaged s Word32)
  , dmdConstraint :: !(FlatMutablePaged s Word8)
  , dmdFaceEdge :: !(FlatMutablePaged s Word32)
  }

denseMutableDcel
  :: MutableDcel s vertex directed undirected face
  -> Maybe (DenseMutableDcel s vertex directed undirected face)
denseMutableDcel owner@MutableDcel{mdPointX, mdPointY, mdVertexOut, mdNewConnected, mdHalfTopology, mdConstraint, mdFaceEdge} =
  DenseMutableDcel owner
    <$> flatMutableSection mdPointX
    <*> flatMutableSection mdPointY
    <*> flatMutableSection mdVertexOut
    <*> flatMutableSection mdNewConnected
    <*> flatMutableSection mdHalfTopology
    <*> flatMutableSection mdConstraint
    <*> flatMutableSection mdFaceEdge
{-# INLINE denseMutableDcel #-}

denseMutableOwner
  :: DenseMutableDcel s vertex directed undirected face
  -> MutableDcel s vertex directed undirected face
denseMutableOwner = dmdOwner
{-# INLINE denseMutableOwner #-}

-- | The physical interpretation required by local topology rewrites. The
-- semantic owner remains 'MutableDcel'; this algebra merely preserves the
-- storage refinement a caller has already proved, so one normalization law
-- specializes to either paged or flat cells instead of growing a sibling
-- rewrite engine.
class MutableTopology mutable where
  topologyOwner
    :: mutable s vertex directed undirected face
    -> MutableDcel s vertex directed undirected face
  topologyReadPointX
    :: mutable s vertex directed undirected face -> Int -> ST s Double
  topologyReadPointY
    :: mutable s vertex directed undirected face -> Int -> ST s Double
  topologyReadOrigin
    :: mutable s vertex directed undirected face -> Int -> ST s Int
  topologyReadNext
    :: mutable s vertex directed undirected face -> Int -> ST s Int
  topologyReadPrevious
    :: mutable s vertex directed undirected face -> Int -> ST s Int
  topologyReadFace
    :: mutable s vertex directed undirected face -> Int -> ST s Int
  topologyReadConstraint
    :: mutable s vertex directed undirected face -> Int -> ST s Bool
  topologyWriteOrigin
    :: mutable s vertex directed undirected face -> Int -> Int -> ST s ()
  topologyWriteNext
    :: mutable s vertex directed undirected face -> Int -> Int -> ST s ()
  topologyWritePrevious
    :: mutable s vertex directed undirected face -> Int -> Int -> ST s ()
  topologyWriteFace
    :: mutable s vertex directed undirected face -> Int -> Int -> ST s ()
  topologyWriteFaceEdge
    :: mutable s vertex directed undirected face -> Int -> Int -> ST s ()
  topologyWriteVertexOut
    :: mutable s vertex directed undirected face -> Int -> Int -> ST s ()

instance MutableTopology MutableDcel where
  topologyOwner = id
  topologyReadPointX = readPointX
  topologyReadPointY = readPointY
  topologyReadOrigin = readOrigin
  topologyReadNext = readNext
  topologyReadPrevious = readPrevious
  topologyReadFace = readFace
  topologyReadConstraint = readConstraint
  topologyWriteOrigin = writeOrigin
  topologyWriteNext = writeNext
  topologyWritePrevious = writePrevious
  topologyWriteFace = writeFace
  topologyWriteFaceEdge = writeFaceEdge
  topologyWriteVertexOut = writeVertexOut
  {-# INLINE topologyOwner #-}
  {-# INLINE topologyReadPointX #-}
  {-# INLINE topologyReadPointY #-}
  {-# INLINE topologyReadOrigin #-}
  {-# INLINE topologyReadNext #-}
  {-# INLINE topologyReadPrevious #-}
  {-# INLINE topologyReadFace #-}
  {-# INLINE topologyReadConstraint #-}
  {-# INLINE topologyWriteOrigin #-}
  {-# INLINE topologyWriteNext #-}
  {-# INLINE topologyWritePrevious #-}
  {-# INLINE topologyWriteFace #-}
  {-# INLINE topologyWriteFaceEdge #-}
  {-# INLINE topologyWriteVertexOut #-}

instance MutableTopology DenseMutableDcel where
  topologyOwner = denseMutableOwner
  topologyReadPointX = denseReadPointX
  topologyReadPointY = denseReadPointY
  topologyReadOrigin = denseReadOrigin
  topologyReadNext = denseReadNext
  topologyReadPrevious = denseReadPrevious
  topologyReadFace = denseReadFace
  topologyReadConstraint = denseReadConstraint
  topologyWriteOrigin = denseWriteOrigin
  topologyWriteNext = denseWriteNext
  topologyWritePrevious = denseWritePrevious
  topologyWriteFace = denseWriteFace
  topologyWriteFaceEdge = denseWriteFaceEdge
  topologyWriteVertexOut = denseWriteVertexOut
  {-# INLINE topologyOwner #-}
  {-# INLINE topologyReadPointX #-}
  {-# INLINE topologyReadPointY #-}
  {-# INLINE topologyReadOrigin #-}
  {-# INLINE topologyReadNext #-}
  {-# INLINE topologyReadPrevious #-}
  {-# INLINE topologyReadFace #-}
  {-# INLINE topologyReadConstraint #-}
  {-# INLINE topologyWriteOrigin #-}
  {-# INLINE topologyWriteNext #-}
  {-# INLINE topologyWritePrevious #-}
  {-# INLINE topologyWriteFace #-}
  {-# INLINE topologyWriteFaceEdge #-}
  {-# INLINE topologyWriteVertexOut #-}

newMutableDcel :: ElementDefaults directed undirected face -> DcelCapacity -> ST s (MutableDcel s vertex directed undirected face)
newMutableDcel defaults capacity = newMutableDcelFrom DenseTransaction Nothing defaults capacity Nothing

-- | A fresh dense mutable DCEL whose vertex payload plane is uniformly filled.
-- The witness keeps the flat section tied to its canonical owner and permits
-- geometry-only ingress to extend the payload plane without materializing one
-- boxed value per site.
data DefaultedVertexDcel s vertex directed undirected face = DefaultedVertexDcel
  { defaultedVertexDcel :: !(MutableDcel s vertex directed undirected face)
  , defaultedDenseVertexDcel :: !(DenseMutableDcel s vertex directed undirected face)
  }

newMutableDcelWithVertexDefault
  :: vertex
  -> ElementDefaults directed undirected face
  -> DcelCapacity
  -> ST s (Either BuildError (DefaultedVertexDcel s vertex directed undirected face))
newMutableDcelWithVertexDefault vertexDefault defaults capacity = do
  mutable <-
    newMutableDcelFrom
      DenseTransaction
      (Just vertexDefault)
      defaults
      capacity
      Nothing
  pure $
    case denseMutableDcel mutable of
      Nothing -> Left CircleSweepRequiresDenseStorage
      Just dense -> Right (DefaultedVertexDcel mutable dense)

-- | Open a local-edit transaction: copy-on-write pages, publication
-- proportional to dirtied pages. The section for singleton persistent verbs.
thawTriangulation
  :: Int
  -> Triangulation mode vertex directed undirected face
  -> ST s (MutableDcel s vertex directed undirected face)
thawTriangulation maximumVertices triangulation =
  newMutableDcelFrom
    LocalTransaction
    Nothing
    (triElementDefaults triangulation)
    (generalDcelCapacity maximumVertices)
    (Just triangulation)

-- | Open a batch transaction: one dense copy up front, flat reads and writes
-- thereafter. The section for sessions and every other many-edit operation.
thawTriangulationDense
  :: Int
  -> Triangulation mode vertex directed undirected face
  -> ST s (MutableDcel s vertex directed undirected face)
thawTriangulationDense maximumVertices triangulation =
  newMutableDcelFrom
    DenseTransaction
    Nothing
    (triElementDefaults triangulation)
    (generalDcelCapacity maximumVertices)
    (Just triangulation)

newMutableDcelFrom
  :: TransactionShape
  -> Maybe vertex
  -> ElementDefaults directed undirected face
  -> DcelCapacity
  -> Maybe (Triangulation mode vertex directed undirected face)
  -> ST s (MutableDcel s vertex directed undirected face)
newMutableDcelFrom shape vertexDefault mdElementDefaults (DcelCapacity requestedVertices requestedHalfEdges requestedFaces) source = do
  let !existingVertices = maybe 0 (pagedLength . triPointX) source
      !existingHalfEdges = maybe 0 ((`quot` 4) . pagedLength . triHalfTopology) source
      !existingFaces = maybe 1 (pagedLength . triFaceEdge) source
      !vertexCapacity = max existingVertices requestedVertices
      !halfCapacity = max existingHalfEdges requestedHalfEdges
      !faceCapacity = max existingFaces requestedFaces
      vertexDataBase = maybe (emptyBoxedPaged vertexDefault) triVertexData source
      directedDataBase = maybe (emptyBoxedPaged (Just (defaultDirectedEdgeData mdElementDefaults))) triDirectedData source
      undirectedDataBase = maybe (emptyBoxedPaged (Just (defaultUndirectedEdgeData mdElementDefaults))) triUndirectedData source
      faceDataBase = maybe (emptyBoxedPaged (Just (defaultFaceData mdElementDefaults))) triFaceData source
      constraintBaseCount = maybe 0 triConstraintCount source
      constraintBaseEdges = maybe IntSet.empty triConstraintEdges source
      pointIndexBase = maybe MissingPointIndex (DormantPointIndex . triPointIndex) source
  mdPointX <- maybe (newLocalMutablePaged vertexCapacity) (thawPagedShaped shape vertexCapacity . triPointX) source
  mdPointY <- maybe (newLocalMutablePaged vertexCapacity) (thawPagedShaped shape vertexCapacity . triPointY) source
  mdVertexOut <- maybe (newLocalMutablePaged vertexCapacity) (thawPagedShaped shape vertexCapacity . triVertexOut) source
  mdVertexData <- thawBoxedPaged shape vertexDataBase
  mdNewConnected <- newLocalMutablePaged (vertexCapacity - existingVertices)
  mdRecycledNew <- newSTRef IntSet.empty
  mdHalfTopology <- maybe (newMutablePaged (4 * halfCapacity)) (thawPagedShaped shape (4 * halfCapacity) . triHalfTopology) source
  mdDirectedData <- thawBoxedPaged shape directedDataBase
  mdUndirectedData <- thawBoxedPaged shape undirectedDataBase
  mdConstraint <- maybe (newMutablePaged (halfCapacity `quot` 2)) (thawPagedShaped shape (halfCapacity `quot` 2) . triConstraint) source
  mdFaceEdge <-
    case source of
      Just triangulation -> thawPagedShaped shape faceCapacity (triFaceEdge triangulation)
      Nothing -> do
        freshFaceEdges <- newLocalMutablePaged faceCapacity
        writePaged freshFaceEdges 0 noIndex
        pure freshFaceEdges
  mdFaceData <- thawBoxedPaged shape faceDataBase
  mdPointCount <- newSTRef existingVertices
  mdPointIndex <- newSTRef pointIndexBase
  mdConnectedCount <- newSTRef existingVertices
  mdHalfCount <- newSTRef existingHalfEdges
  mdFaceCount <- newSTRef existingFaces
  mdConstraintCount <- newSTRef constraintBaseCount
  mdConstraintEdges <- newSTRef constraintBaseEdges
  mdLastFace <- newSTRef (if existingFaces > 1 then 1 else 0)
  let !mdPayloadsPristine =
        boxedThawPristine mdDirectedData
          && boxedThawPristine mdUndirectedData
          && boxedThawPristine mdFaceData
  pure
    MutableDcel
      { mdInitialPointCount = existingVertices
      , mdPointCapacity = vertexCapacity
      , mdHalfCapacity = halfCapacity
      , mdFaceCapacity = faceCapacity
      , ..
      }

freezeTriangulation :: MutableDcel s vertex directed undirected face -> ST s (Either BuildError (Triangulation mode vertex directed undirected face))
freezeTriangulation mutable =
  fmap (fmap fst) (freezeTriangulationCore (const (pure emptyPublicationStats)) mutable)

freezeTriangulationWithStats
  :: MutableDcel s vertex directed undirected face
  -> ST s (Either BuildError (Triangulation mode vertex directed undirected face, PublicationStats))
freezeTriangulationWithStats = freezeTriangulationCore readMutableDcelPublicationStats

freezeTriangulationCore
  :: (MutableDcel s vertex directed undirected face -> ST s PublicationStats)
  -> MutableDcel s vertex directed undirected face
  -> ST s (Either BuildError (Triangulation mode vertex directed undirected face, PublicationStats))
freezeTriangulationCore readPublicationStats mutable@MutableDcel
  { mdPointX
  , mdPointY
  , mdPointIndex
  , mdVertexOut
  , mdVertexData
  , mdHalfTopology
  , mdDirectedData
  , mdUndirectedData
  , mdConstraint
  , mdFaceEdge
  , mdFaceData
  , mdPointCount
  , mdHalfCount
  , mdFaceCount
  , mdConstraintCount
  , mdConstraintEdges
  , mdElementDefaults
  } = do
    vertices <- readSTRef mdPointCount
    halfEdges <- readSTRef mdHalfCount
    faces <- readSTRef mdFaceCount
    triConstraintCount <- readSTRef mdConstraintCount
    triConstraintEdges <- readSTRef mdConstraintEdges
    vertexDataOutcome <- freezeBoxedPaged vertices mdVertexData
    directedDataOutcome <- freezeBoxedPaged halfEdges mdDirectedData
    undirectedDataOutcome <- freezeBoxedPaged (halfEdges `quot` 2) mdUndirectedData
    faceDataOutcome <- freezeBoxedPaged faces mdFaceData
    case
        (,,,)
          <$> vertexDataOutcome
          <*> directedDataOutcome
          <*> undirectedDataOutcome
          <*> faceDataOutcome
      of
        Left obstruction -> pure (Left (PayloadStorageFailure obstruction))
        Right (triVertexData, triDirectedData, triUndirectedData, triFaceData) -> do
          triPointX <- freezePaged vertices mdPointX
          triPointY <- freezePaged vertices mdPointY
          pointIndexState <- readSTRef mdPointIndex
          let triPointIndex =
                case pointIndexState of
                  DormantPointIndex residentIndex -> residentIndex
                  ActivePersistentPointIndex residentIndex -> residentIndex
                  ActiveBatchPointIndex _ -> buildPointIndex triPointX triPointY
                  MissingPointIndex -> buildPointIndex triPointX triPointY
          triVertexOut <- freezePaged vertices mdVertexOut
          triHalfTopology <- freezePaged (4 * halfEdges) mdHalfTopology
          triConstraint <- freezePaged (halfEdges `quot` 2) mdConstraint
          triFaceEdge <- freezePaged faces mdFaceEdge
          publicationStats <- readPublicationStats mutable
          let triElementDefaults = mdElementDefaults
          pure (Right (Triangulation{triSeamFrontier = Nothing, ..}, publicationStats))

readMutableDcelPublicationStats
  :: MutableDcel s vertex directed undirected face
  -> ST s PublicationStats
readMutableDcelPublicationStats MutableDcel
  { mdPointX
  , mdPointY
  , mdVertexOut
  , mdHalfTopology
  , mdConstraint
  , mdFaceEdge
  , mdVertexData
  , mdDirectedData
  , mdUndirectedData
  , mdFaceData
  } =
  foldl'
    addPublicationStats
    emptyPublicationStats
    <$> sequence
      [ readMutablePagedPublicationStats mdPointX
      , readMutablePagedPublicationStats mdPointY
      , readMutablePagedPublicationStats mdVertexOut
      , readMutablePagedPublicationStats mdHalfTopology
      , readMutablePagedPublicationStats mdConstraint
      , readMutablePagedPublicationStats mdFaceEdge
      , readMutableBoxedPublicationStats mdVertexData
      , readMutableBoxedPublicationStats mdDirectedData
      , readMutableBoxedPublicationStats mdUndirectedData
      , readMutableBoxedPublicationStats mdFaceData
      ]

pointCapacity :: MutableDcel s vertex directed undirected face -> Int
pointCapacity = mdPointCapacity
{-# INLINE pointCapacity #-}

halfEdgeCapacity :: MutableDcel s vertex directed undirected face -> Int
halfEdgeCapacity = mdHalfCapacity
{-# INLINE halfEdgeCapacity #-}

pointCount :: MutableDcel s vertex directed undirected face -> ST s Int
pointCount = readSTRef . mdPointCount
{-# INLINE pointCount #-}

connectedCount :: MutableDcel s vertex directed undirected face -> ST s Int
connectedCount = readSTRef . mdConnectedCount
{-# INLINE connectedCount #-}

directedEdgeCount :: MutableDcel s vertex directed undirected face -> ST s Int
directedEdgeCount = readSTRef . mdHalfCount
{-# INLINE directedEdgeCount #-}

faceCount :: MutableDcel s vertex directed undirected face -> ST s Int
faceCount = readSTRef . mdFaceCount
{-# INLINE faceCount #-}

pointAt :: MutableDcel s vertex directed undirected face -> Int -> ST s (Point)
pointAt MutableDcel{mdPointX, mdPointY} index =
  Point <$> readPaged mdPointX index <*> readPaged mdPointY index
{-# INLINE pointAt #-}

-- | Resolve a canonical site through the derived handle index, confirming
-- every hash candidate against the authoritative coordinate planes. A mesh
-- created from scratch derives the index only if a caller actually asks; a
-- thawed published mesh inherits its structurally shared index.
lookupPointVertex
  :: MutableDcel s vertex directed undirected face
  -> Point
  -> ST s (Maybe Int)
lookupPointVertex mutable@MutableDcel{mdPointIndex} rawPoint = do
  indexState <- readSTRef mdPointIndex
  case canonicalPoint rawPoint of
    Point x y ->
      case indexState of
        ActiveBatchPointIndex table ->
          lookupMutablePoint table (readPointX mutable) (readPointY mutable) x y
        ActivePersistentPointIndex residentIndex ->
          resolvePersistent x y residentIndex
        DormantPointIndex residentIndex -> do
          writeSTRef mdPointIndex (ActivePersistentPointIndex residentIndex)
          resolvePersistent x y residentIndex
        MissingPointIndex -> do
          derived <- deriveMutablePointIndex mutable
          writeSTRef mdPointIndex (ActivePersistentPointIndex derived)
          resolvePersistent x y derived
 where
  resolvePersistent x y pointIndex =
    foldM
      (confirmCandidate mutable x y)
      Nothing
      (pointIndexCandidates x y pointIndex)

-- | Whether the transaction has already committed to incremental identity
-- transport. Answering does not force a dormant index's lazy rebuild, which
-- is the point: a per-question caller must not buy a whole-mesh build.
identityIndexActive :: MutableDcel s vertex directed undirected face -> ST s Bool
identityIndexActive MutableDcel{mdPointIndex} = do
  indexState <- readSTRef mdPointIndex
  pure $ case indexState of
    ActivePersistentPointIndex _ -> True
    ActiveBatchPointIndex _ -> True
    _ -> False
{-# INLINE identityIndexActive #-}

-- | Declare that a singleton handle-keyed rewrite must transport the resident
-- immutable identity section strictly. Dense point-keyed removal uses
-- 'activateBatchPointIndex' instead, so it neither forces nor incrementally
-- allocates the published 'PointIndex'.
activatePointIndex
  :: MutableDcel s vertex directed undirected face
  -> ST s ()
activatePointIndex mutable@MutableDcel{mdPointIndex} = do
  indexState <- readSTRef mdPointIndex
  case indexState of
    ActivePersistentPointIndex _ -> pure ()
    ActiveBatchPointIndex _ -> pure ()
    DormantPointIndex residentIndex ->
      writeSTRef mdPointIndex (ActivePersistentPointIndex residentIndex)
    MissingPointIndex -> do
      derived <- deriveMutablePointIndex mutable
      writeSTRef mdPointIndex (ActivePersistentPointIndex derived)

-- | Open an identity section for one dense removal program. The table derives
-- only from coordinate authority, and its extent is deliberately narrower
-- than the surrounding session: publication returns to the lazy immutable
-- derivation instead of retaining a second mutable identity owner.
activateBatchPointIndex
  :: MutableDcel s vertex directed undirected face
  -> ST s (Either BuildError ())
activateBatchPointIndex mutable@MutableDcel{mdPointIndex} = do
  vertices <- pointCount mutable
  table <- newMutablePointIndex vertices
  seeded <-
    seedMutablePointIndex
      table
      vertices
      (readPointX mutable)
      (readPointY mutable)
  case seeded of
    Left failure -> pure (Left failure)
    Right () -> do
      writeSTRef mdPointIndex (ActiveBatchPointIndex table)
      pure (Right ())
-- | Close the batch-local identity section after its removal program. Its
-- contents cannot escape @ST@; marking the cache missing makes freeze glue a
-- lazy immutable derivation from the final coordinate arenas.
discardBatchPointIndex
  :: MutableDcel s vertex directed undirected face
  -> ST s ()
discardBatchPointIndex MutableDcel{mdPointIndex} =
  modifySTRef'
    mdPointIndex
    (\indexState ->
       case indexState of
         ActiveBatchPointIndex _ -> MissingPointIndex
         retained -> retained
    )

deriveMutablePointIndex
  :: MutableDcel s vertex directed undirected face
  -> ST s PointIndex
deriveMutablePointIndex mutable = do
  vertices <- pointCount mutable
  foldM insertResident emptyPointIndex [0 .. vertices - 1]
 where
  insertResident pointIndex vertex = do
    x <- readPointX mutable vertex
    y <- readPointY mutable vertex
    pure (insertPointIndex x y vertex pointIndex)

confirmCandidate
  :: MutableDcel s vertex directed undirected face
  -> Double
  -> Double
  -> Maybe Int
  -> Int
  -> ST s (Maybe Int)
confirmCandidate _ _ _ resident@(Just _) _ = pure resident
confirmCandidate mutable x y Nothing candidate = do
  heldX <- readPointX mutable candidate
  heldY <- readPointY mutable candidate
  pure (if heldX == x && heldY == y then Just candidate else Nothing)
{-# INLINE confirmCandidate #-}

-- | Read one stored coordinate without building a t'Point'. Coordinates are
-- the authoritative state owned by 'mdPointX'/'mdPointY'; the t'Point'
-- constructor is the cold accessor's packaging, and the construction kernel
-- reads these arenas directly so a specialized sweep never boxes one.
readPointX :: MutableDcel s vertex directed undirected face -> Int -> ST s Double
readPointX MutableDcel{mdPointX} index = readPaged mdPointX index
{-# INLINE readPointX #-}

readPointY :: MutableDcel s vertex directed undirected face -> Int -> ST s Double
readPointY MutableDcel{mdPointY} index = readPaged mdPointY index
{-# INLINE readPointY #-}

writePoint :: MutableDcel s vertex directed undirected face -> Int -> Point -> ST s ()
writePoint MutableDcel{mdPointX, mdPointY} index rawPoint =
  case canonicalPoint rawPoint of
    Point x y -> do
      writePaged mdPointX index x
      writePaged mdPointY index y
{-# INLINE writePoint #-}

vertexDataAt :: MutableDcel s vertex directed undirected face -> Int -> ST s vertex
vertexDataAt MutableDcel{mdVertexData} = readBoxedPaged mdVertexData
{-# INLINE vertexDataAt #-}

writeVertexData :: MutableDcel s vertex directed undirected face -> Int -> vertex -> ST s ()
writeVertexData MutableDcel{mdVertexData} = writeBoxedPaged mdVertexData
{-# INLINE writeVertexData #-}

-- | Whether no element payload plane can be holding anything. Constant for the
-- transaction: see 'mdPayloadsPristine'. A rewrite site that performs several
-- resets together tests this once rather than paying the test inside each.
payloadsPristine :: MutableDcel s vertex directed undirected face -> Bool
payloadsPristine = mdPayloadsPristine
{-# INLINE payloadsPristine #-}

-- | Return one undirected edge and both its half-edges to the element
-- defaults. A payload labels the element occupying a slot, and an element is
-- its geometry: a rewrite that gives a slot new endpoints has put a different
-- edge there, and the label the old one carried does not describe it. Leaving
-- it would also make the payload plane depend on the flip order that reached
-- the normal form, while the topology does not.
--
-- The constraint flag is deliberately not reset with it. A flag states that a
-- segment of the input is present, and a segment that gets split is still
-- present as its two halves; a payload states what an element is.
resetEdgeData :: MutableDcel s vertex directed undirected face -> Int -> ST s ()
resetEdgeData MutableDcel{mdDirectedData, mdUndirectedData, mdPayloadsPristine, mdElementDefaults} pair =
  unless mdPayloadsPristine $ do
    resetBoxedRange mdDirectedData (defaultDirectedEdgeData mdElementDefaults) (2 * pair) 2
    resetBoxedRange mdUndirectedData (defaultUndirectedEdgeData mdElementDefaults) pair 1
{-# INLINE resetEdgeData #-}

-- | Return one face to the element default. See 'resetEdgeData'.
resetFaceData :: MutableDcel s vertex directed undirected face -> Int -> ST s ()
resetFaceData MutableDcel{mdFaceData, mdPayloadsPristine, mdElementDefaults} face =
  unless mdPayloadsPristine (resetBoxedRange mdFaceData (defaultFaceData mdElementDefaults) face 1)
{-# INLINE resetFaceData #-}

edgeOriginPoint :: MutableDcel s vertex directed undirected face -> Int -> ST s (Point)
edgeOriginPoint mutable edge = readOrigin mutable edge >>= pointAt mutable
{-# INLINE edgeOriginPoint #-}

appendVertex
  :: MutableDcel s vertex directed undirected face
  -> Point
  -> vertex
  -> ST s Int
appendVertex mutable rawPoint vertexData =
  case canonicalPoint rawPoint of
    Point x y -> appendVertexCoordinates mutable x y vertexData
{-# INLINE appendVertex #-}

-- | The next unmaterialized vertex record in one mutable DCEL. The constructor
-- stays private: a caller may transport the candidate to a derived identity
-- index, but cannot fabricate a different arena position for the append that
-- follows. The slot remains lawful only while no intervening append occurs.
newtype NextVertexSlot s = NextVertexSlot Int

nextVertexSlot
  :: MutableDcel s vertex directed undirected face
  -> ST s (NextVertexSlot s)
nextVertexSlot MutableDcel{mdPointCount} = NextVertexSlot <$> readSTRef mdPointCount
{-# INLINE nextVertexSlot #-}

nextVertexSlotIndex :: NextVertexSlot s -> Int
nextVertexSlotIndex (NextVertexSlot vertex) = vertex
{-# INLINE nextVertexSlotIndex #-}

-- | Append a vertex whose coordinates are already canonical, by components.
-- This is the one owner of the append record; 'appendVertex' is its
-- t'Point'-carrying form for callers holding a point. Raw capacity means the
-- appender initializes every field of the record it exposes.
appendVertexCoordinates
  :: MutableDcel s vertex directed undirected face
  -> Double
  -> Double
  -> vertex
  -> ST s Int
appendVertexCoordinates mutable x y vertexData = do
  slot <- nextVertexSlot mutable
  appendVertexCoordinatesAtSlot mutable slot x y vertexData
{-# INLINE appendVertexCoordinates #-}

-- | Append the uniform vertex payload carried by a defaulted-vertex witness.
-- No boxed page is written: extending the authoritative point count extends
-- the defaulted payload plane at freeze.
appendDefaultVertexCoordinates
  :: DefaultedVertexDcel s vertex directed undirected face
  -> Double
  -> Double
  -> ST s Int
appendDefaultVertexCoordinates DefaultedVertexDcel{defaultedVertexDcel = mutable} x y = do
  slot <- nextVertexSlot mutable
  initializeVertexCoordinatesAtSlot mutable slot x y
{-# INLINE appendDefaultVertexCoordinates #-}

-- | Materialize the exact fresh record named by a previously acquired slot.
-- Keeping the slot typed and adjacent to identity resolution lets bulk ingress
-- share one point-count read between the identity candidate and the append.
appendVertexCoordinatesAtSlot
  :: MutableDcel s vertex directed undirected face
  -> NextVertexSlot s
  -> Double
  -> Double
  -> vertex
  -> ST s Int
appendVertexCoordinatesAtSlot mutable slot@(NextVertexSlot vertex) x y vertexData = do
  writeVertexData mutable vertex vertexData
  initializeVertexCoordinatesAtSlot mutable slot x y
{-# INLINE appendVertexCoordinatesAtSlot #-}

initializeVertexCoordinatesAtSlot
  :: MutableDcel s vertex directed undirected face
  -> NextVertexSlot s
  -> Double
  -> Double
  -> ST s Int
initializeVertexCoordinatesAtSlot mutable@MutableDcel{mdPointCount, mdPointX, mdPointY, mdPointIndex, mdNewConnected, mdRecycledNew} (NextVertexSlot vertex) x y = do
  writePaged mdPointX vertex x
  writePaged mdPointY vertex y
  writeVertexOut mutable vertex (-1)
  if vertex >= mdInitialPointCount mutable
    then writePaged mdNewConnected (vertex - mdInitialPointCount mutable) 0
    else modifySTRef' mdRecycledNew (IntSet.insert vertex)
  pointIndexState <- readSTRef mdPointIndex
  case pointIndexState of
    ActivePersistentPointIndex pointIndex ->
      writeSTRef mdPointIndex (ActivePersistentPointIndex (insertPointIndex x y vertex pointIndex))
    -- The batch table is only lawful over the removal subprogram that opened
    -- it. An insertion before that scope is closed invalidates the derived
    -- cache rather than pretending an unregistered handle exists.
    ActiveBatchPointIndex _ -> writeSTRef mdPointIndex MissingPointIndex
    -- Transported lazily: the field holds a pure update thunk, so a batch that
    -- never asks an identity question pays one allocation per append, while a
    -- persistent chain that asks every publication forces a depth-one thunk
    -- instead of rebuilding the index over the whole mesh.
    DormantPointIndex pointIndex ->
      writeSTRef mdPointIndex (DormantPointIndex (insertPointIndex x y vertex pointIndex))
    -- A missing derived view stays missing without writing its cell once per
    -- bulk vertex. Freeze already descends from the coordinate authority when
    -- a future identity query demands the view.
    MissingPointIndex -> pure ()
  writeSTRef mdPointCount (vertex + 1)
  pure vertex
{-# INLINE initializeVertexCoordinatesAtSlot #-}

-- | Check the point arena before a local rewrite materializes vertices. The
-- caller performs this before the first write, so refusal needs no rollback.
ensurePointCapacity
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> ST s (Either BuildError ())
ensurePointCapacity MutableDcel{mdPointCount, mdPointCapacity} additional
  | additional < 0 = pure (Left (CapacityExceeded additional))
  | otherwise = do
      current <- readSTRef mdPointCount
      let !required = current + additional
      pure $
        if required > mdPointCapacity
          then Left (CapacityExceeded required)
          else Right ()
{-# INLINE ensurePointCapacity #-}

markConnected :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
markConnected mutable@MutableDcel{mdInitialPointCount, mdNewConnected, mdRecycledNew, mdConnectedCount} vertex outgoing = do
  if vertex >= mdInitialPointCount
    then do
      let !newVertex = vertex - mdInitialPointCount
      connected <- readPaged mdNewConnected newVertex
      unless (connected /= 0) $ do
        writePaged mdNewConnected newVertex 1
        modifySTRef' mdConnectedCount (+ 1)
    else do
      recycled <- readSTRef mdRecycledNew
      when (IntSet.member vertex recycled) $ do
        writeSTRef mdRecycledNew (IntSet.delete vertex recycled)
        modifySTRef' mdConnectedCount (+ 1)
  writeVertexOut mutable vertex outgoing
{-# INLINE markConnected #-}

isConnected :: MutableDcel s vertex directed undirected face -> Int -> ST s Bool
isConnected MutableDcel{mdInitialPointCount, mdNewConnected, mdRecycledNew} vertex
  | vertex < mdInitialPointCount = IntSet.notMember vertex <$> readSTRef mdRecycledNew
  | otherwise = (/= 0) <$> readPaged mdNewConnected (vertex - mdInitialPointCount)
{-# INLINE isConnected #-}

addEdge :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s (Int, Int)
addEdge mutable from to = do
  base <- addEdgeBlock mutable 1
  writeOrigin mutable base from
  writeOrigin mutable (base + 1) to
  pure (base, base + 1)
{-# INLINE addEdge #-}

-- Each appended pair owns the initialization of its exposed topology and
-- constraint records; reserved capacity remains untouched.
addEdgeBlock :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
addEdgeBlock mutable@MutableDcel{mdHalfCount, mdConstraint} pairs = do
  base <- readSTRef mdHalfCount
  let !required = base + 2 * pairs
      !firstUndirected = base `quot` 2
      !lastUndirected = firstUndirected + pairs - 1
  forM_ [4 * base .. 4 * required - 1] $ \slot ->
    writePaged (mdHalfTopology mutable) slot noIndex
  forM_ [firstUndirected .. lastUndirected] $ \edge -> writePaged mdConstraint edge 0
  writeSTRef mdHalfCount required
  pure base
{-# INLINE addEdgeBlock #-}

-- | Append initialized cells through the already-proved contiguous section.
-- The semantic allocator and its tail invariant are identical to
-- 'addEdgeBlock'; only the physical interpreter is selected once rather than
-- once per topology slot.
denseAddEdgeBlock :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Int
denseAddEdgeBlock dense@DenseMutableDcel{dmdOwner = MutableDcel{mdHalfCount}} pairs = do
  base <- readSTRef mdHalfCount
  let !required = base + 2 * pairs
  denseInitializeUnconstrainedEdgeBlock dense base pairs
  writeSTRef mdHalfCount required
  pure base
{-# INLINE denseAddEdgeBlock #-}

-- | Initialize the constraint section of a proved fresh directed-edge block
-- without advancing the global arena count. A reserved bulk program threads
-- its allocation cursor immutably and commits the count once after gluing;
-- the ordinary allocator above shares this exact record initializer.
denseInitializeUnconstrainedEdgeBlock
  :: DenseMutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> ST s ()
denseInitializeUnconstrainedEdgeBlock DenseMutableDcel{dmdConstraint} directedBase pairs =
  forM_ [directedBase `quot` 2 .. directedBase `quot` 2 + pairs - 1] $ \edge ->
    writeFlatMutable dmdConstraint edge 0
{-# INLINE denseInitializeUnconstrainedEdgeBlock #-}

addFace :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
addFace mutable anchor = do
  base <- addFaceBlock mutable 1
  writeFaceEdge mutable base anchor
  pure base
{-# INLINE addFace #-}

addFaceBlock :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
addFaceBlock mutable@MutableDcel{mdFaceCount} count = do
  base <- readSTRef mdFaceCount
  let !required = base + count
  mapM_ (\face -> writeFaceEdge mutable face (-1)) [base .. required - 1]
  writeSTRef mdFaceCount required
  pure base
{-# INLINE addFaceBlock #-}

denseAddFaceBlock :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Int
denseAddFaceBlock DenseMutableDcel{dmdOwner = MutableDcel{mdFaceCount}} count = do
  base <- readSTRef mdFaceCount
  let !required = base + count
  writeSTRef mdFaceCount required
  pure base
{-# INLINE denseAddFaceBlock #-}

-- | Check the local allocation section before any topology rewrite begins.
-- A refusal leaves every mutable plane untouched, so the enclosing transaction
-- can abandon publication without rollback machinery.
ensureCellCapacity
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> ST s (Either BuildError ())
ensureCellCapacity MutableDcel{mdHalfCount, mdHalfCapacity, mdFaceCount, mdFaceCapacity} additionalPairs additionalFaces
  | additionalPairs < 0 = pure (Left (HalfEdgeCapacityExceeded additionalPairs mdHalfCapacity))
  | additionalFaces < 0 = pure (Left (FaceCapacityExceeded additionalFaces mdFaceCapacity))
  | otherwise = do
      currentHalfEdges <- readSTRef mdHalfCount
      currentFaces <- readSTRef mdFaceCount
      let !requiredHalfEdges = currentHalfEdges + 2 * additionalPairs
          !requiredFaces = currentFaces + additionalFaces
      if requiredHalfEdges > mdHalfCapacity
        then pure (Left (HalfEdgeCapacityExceeded requiredHalfEdges mdHalfCapacity))
        else
          if requiredFaces > mdFaceCapacity
            then pure (Left (FaceCapacityExceeded requiredFaces mdFaceCapacity))
            else pure (Right ())
{-# INLINE ensureCellCapacity #-}

truncatePoints :: MutableDcel s vertex directed undirected face -> Int -> ST s ()
truncatePoints MutableDcel{mdPointCount, mdConnectedCount} count = do
  writeSTRef mdPointCount count
  connected <- readSTRef mdConnectedCount
  when (connected > count) (writeSTRef mdConnectedCount count)

truncateDirectedEdges :: MutableDcel s vertex directed undirected face -> Int -> ST s ()
truncateDirectedEdges MutableDcel{mdHalfCount} = writeSTRef mdHalfCount

truncateFaces :: MutableDcel s vertex directed undirected face -> Int -> ST s ()
truncateFaces MutableDcel{mdFaceCount, mdLastFace} count = do
  writeSTRef mdFaceCount count
  lastFace <- readSTRef mdLastFace
  when (lastFace >= count) (writeSTRef mdLastFace (if count > 1 then 1 else 0))

-- | Remove one undirected edge by moving the last pair into its slot. Only the
-- two neighboring links, one vertex representative, and one face
-- representative per moved half-edge can reference the old handles.
--
-- The slot the tail vacates is returned to the defaults here rather than when
-- the allocator hands it back out. That keeps one invariant — every slot at or
-- above the live count holds the fill — which the thaw establishes, this
-- preserves, and 'addEdgeBlock' may therefore assume without testing anything.
-- The cost lands on removal, which is where the element was retired, instead of
-- on every allocation a pure insertion makes.
swapRemoveUndirectedEdge :: MutableDcel s vertex directed undirected face -> Int -> ST s (Either BuildError ())
swapRemoveUndirectedEdge mutable@MutableDcel{mdConstraint, mdConstraintCount, mdConstraintEdges} pair = do
  halfEdges <- directedEdgeCount mutable
  let !pairs = halfEdges `quot` 2
      !lastPair = pairs - 1
  if pair < 0 || pair > lastPair
    then
      pure
        ( Left
            ( RemovalEdgeOutOfRange
                (UndirectedEdgeId (fromIntegral pair))
                pairs
            )
        )
    else do
      swapRemoveUndirectedEdgeInRange lastPair
      pure (Right ())
 where
  swapRemoveUndirectedEdgeInRange lastPair = do
    removedFlag <- readPaged mdConstraint pair
    lastFlag <- readPaged mdConstraint lastPair
    when (removedFlag /= 0) (modifySTRef' mdConstraintCount (subtract 1))
    when (pair /= lastPair) $ do
      let !oldBase = 2 * lastPair
          !newBase = 2 * pair
          remap !handle
            | handle == oldBase = newBase
            | handle == oldBase + 1 = newBase + 1
            | otherwise = handle
      -- Both records are taken before either is republished: the pair's two
      -- half-edges can name each other, so a read after the first write would
      -- see the new handle where the old one belongs.
      !forwardOrigin <- readOrigin mutable oldBase
      !forwardNext <- remap <$> readNext mutable oldBase
      !forwardPrevious <- remap <$> readPrevious mutable oldBase
      !forwardFace <- readFace mutable oldBase
      !backwardOrigin <- readOrigin mutable (oldBase + 1)
      !backwardNext <- remap <$> readNext mutable (oldBase + 1)
      !backwardPrevious <- remap <$> readPrevious mutable (oldBase + 1)
      !backwardFace <- readFace mutable (oldBase + 1)
      writePaged mdConstraint pair lastFlag
      unless (mdPayloadsPristine mutable) $ do
        readBoxedPaged (mdUndirectedData mutable) lastPair >>= writeBoxedPaged (mdUndirectedData mutable) pair
        readBoxedPaged (mdDirectedData mutable) oldBase >>= writeBoxedPaged (mdDirectedData mutable) newBase
        readBoxedPaged (mdDirectedData mutable) (oldBase + 1) >>= writeBoxedPaged (mdDirectedData mutable) (newBase + 1)
      writeOrigin mutable newBase forwardOrigin
      writeNext mutable newBase forwardNext
      writePrevious mutable newBase forwardPrevious
      writeFace mutable newBase forwardFace
      writeOrigin mutable (newBase + 1) backwardOrigin
      writeNext mutable (newBase + 1) backwardNext
      writePrevious mutable (newBase + 1) backwardPrevious
      writeFace mutable (newBase + 1) backwardFace
      writeNext mutable forwardPrevious newBase
      writePrevious mutable forwardNext newBase
      writeVertexOut mutable forwardOrigin newBase
      writeFaceEdge mutable forwardFace newBase
      writeNext mutable backwardPrevious (newBase + 1)
      writePrevious mutable backwardNext (newBase + 1)
      writeVertexOut mutable backwardOrigin (newBase + 1)
      writeFaceEdge mutable backwardFace (newBase + 1)
    modifySTRef'
      mdConstraintEdges
      (\edges ->
         let withoutRetired = IntSet.delete pair (IntSet.delete lastPair edges)
          in if pair /= lastPair && lastFlag /= 0
               then IntSet.insert pair withoutRetired
               else withoutRetired
      )
    resetEdgeData mutable lastPair
    truncateDirectedEdges mutable (2 * lastPair)

swapRemoveFace :: MutableDcel s vertex directed undirected face -> Int -> ST s (Either BuildError ())
swapRemoveFace mutable face = do
  faces <- faceCount mutable
  let !lastFace = faces - 1
  if face <= 0 || face > lastFace
    then
      pure
        ( Left
            (RemovalFaceOutOfRange (FaceId (fromIntegral face)) faces)
        )
    else do
      relocated <-
        if face == lastFace
          then pure (Right ())
          else relocateLastFace lastFace
      case relocated of
        Left obstruction -> pure (Left obstruction)
        Right () -> do
          resetFaceData mutable lastFace
          truncateFaces mutable lastFace
          pure (Right ())
 where
  relocateLastFace lastFace = do
    start <- readFaceEdge mutable lastFace
    writeFaceEdge mutable face start
    -- Swap-compaction moves the last face's index, not the face. When the
    -- locator's cached start is that face, following it here is the difference
    -- between a batch of removals resuming where the previous one settled and
    -- 'truncateFaces' finding the cached index out of range and resetting it to
    -- the first inner face. The cache is a start, never an answer.
    cachedFace <- readSTRef (mdLastFace mutable)
    when (cachedFace == lastFace) (writeSTRef (mdLastFace mutable) face)
    unless (mdPayloadsPristine mutable) $
      readBoxedPaged (mdFaceData mutable) lastFace >>= writeBoxedPaged (mdFaceData mutable) face
    halfEdges <- directedEdgeCount mutable
    let go !remaining !current !seen
          | remaining <= 0 =
              pure
                ( Left
                    ( RemovalFaceCycleDidNotTerminate
                        (FaceId (fromIntegral lastFace))
                        (DirectedEdgeId (fromIntegral current))
                        (halfEdges + 1)
                    )
                )
          | seen && current == start = pure (Right ())
          | otherwise = do
              writeFace mutable current face
              nextEdge <- readNext mutable current
              go (remaining - 1) nextEdge True
    go (halfEdges + 1) start False

-- | Retire a vertex by moving the arena's last into its slot. The relocation is
-- reported as the slot together with the position now standing in it: the two
-- are one fact, and a caller told only the slot has to consult the mesh to
-- learn what landed there.
swapRemoveVertex :: MutableDcel s vertex directed undirected face -> Int -> ST s (Either BuildError (Point, vertex, Maybe (Int, Point)))
swapRemoveVertex mutable@MutableDcel{mdConnectedCount, mdPointIndex, mdRecycledNew, mdNewConnected} vertex = do
  vertices <- pointCount mutable
  let !lastVertex = vertices - 1
  if vertex < 0 || vertex > lastVertex
    then
      pure
        ( Left
            (RemovalVertexOutOfRange (VertexId (fromIntegral vertex)) vertices)
        )
    else swapRemoveVertexInRange lastVertex
 where
  swapRemoveVertexInRange lastVertex = do
    removedPoint <- pointAt mutable vertex
    removedPayload <- vertexDataAt mutable vertex
    movedOutcome <-
      if vertex == lastVertex
        then pure (Right Nothing)
        else moveTailVertex lastVertex
    case movedOutcome of
      Left obstruction -> pure (Left obstruction)
      Right moved -> do
        indexState <- readSTRef mdPointIndex
        updatedIndexState <-
          updatePointIndexAfterSwap mutable indexState removedPoint vertex lastVertex moved
        writeSTRef mdPointIndex updatedIndexState
        -- The connectivity companions must agree with the aggregate assertion two
        -- lines down: after a swap removal every surviving vertex is connected. The
        -- retired slot's entry and the relocated occupant's old entry are both
        -- stale, and a relocated occupant landing in the appended region must read
        -- connected through the offset store, not through its predecessor's bit.
        modifySTRef' mdRecycledNew (IntSet.delete vertex . IntSet.delete lastVertex)
        case moved of
          Just _
            | vertex >= mdInitialPointCount mutable ->
                writePaged mdNewConnected (vertex - mdInitialPointCount mutable) 1
          _ -> pure ()
        truncatePoints mutable lastVertex
        writeSTRef mdConnectedCount lastVertex
        pure (Right (removedPoint, removedPayload, moved))

  moveTailVertex lastVertex = do
    movedPoint <- pointAt mutable lastVertex
    movedPayload <- vertexDataAt mutable lastVertex
    movedOut <- readVertexOut mutable lastVertex
    writePoint mutable vertex movedPoint
    writeVertexData mutable vertex movedPayload
    writeVertexOut mutable vertex movedOut
    relocated <-
      if movedOut < 0
        then pure (Right ())
        else relocateOutgoingCycle lastVertex movedOut
    pure (Just (vertex, movedPoint) <$ relocated)

  relocateOutgoingCycle lastVertex movedOut = do
    halfEdges <- directedEdgeCount mutable
    let go !remaining !current !seen
          | remaining <= 0 =
              pure
                ( Left
                    ( RemovalOutgoingCycleDidNotTerminate
                        (VertexId (fromIntegral lastVertex))
                        (DirectedEdgeId (fromIntegral current))
                        (halfEdges + 1)
                    )
                )
          | seen && current == movedOut = pure (Right ())
          | otherwise = do
              writeOrigin mutable current vertex
              previousEdge <- readPrevious mutable current
              go (remaining - 1) (previousEdge `xor` 1) True
    go (halfEdges + 1) movedOut False

-- | Transport the identity view across one vertex swap. Persistent sections
-- retain their existing pure update law. A batch table mutates in place, then
-- deliberately falls back to @MissingPointIndex@ if either local proof cannot
-- be completed; geometry remains authoritative and subsequent operations walk
-- rather than observe a stale cache.
updatePointIndexAfterSwap
  :: MutableDcel s vertex directed undirected face
  -> MutablePointIndexState s
  -> Point
  -> Int
  -> Int
  -> Maybe (Int, Point)
  -> ST s (MutablePointIndexState s)
updatePointIndexAfterSwap mutable indexState removedPoint vertex lastVertex moved =
  case indexState of
    DormantPointIndex pointIndex ->
      pure (DormantPointIndex (updatePersistentPointIndex pointIndex))
    ActivePersistentPointIndex pointIndex ->
      pure (ActivePersistentPointIndex (updatePersistentPointIndex pointIndex))
    ActiveBatchPointIndex table ->
      transportBatchPointIndex table
    MissingPointIndex -> pure MissingPointIndex
 where
  updatePersistentPointIndex pointIndex =
    case removedPoint of
      Point removedX removedY ->
        let withoutRemoved = removePointIndex removedX removedY vertex pointIndex
         in case moved of
              Nothing -> withoutRemoved
              Just (_, Point movedX movedY) ->
                relocatePointIndex movedX movedY lastVertex vertex withoutRemoved

  transportBatchPointIndex table =
    case removedPoint of
      Point removedX removedY -> do
        removed <-
          removeMutablePoint
            table
            (readPointX mutable)
            (readPointY mutable)
            removedX
            removedY
            vertex
        case (removed, moved) of
          (MutablePointIndexUpdated, Nothing) ->
            pure (ActiveBatchPointIndex table)
          (MutablePointIndexUpdated, Just (_, Point movedX movedY)) -> do
            relocated <- relocateMutablePoint table movedX movedY lastVertex vertex
            pure $
              case relocated of
                MutablePointIndexUpdated -> ActiveBatchPointIndex table
                MutablePointIndexInvalidated -> MissingPointIndex
          (MutablePointIndexInvalidated, _) -> pure MissingPointIndex

  -- 'swapRemoveVertex' calls us before truncation. The tail's coordinate cells
  -- still carry the moved point, so backward-shift repair can derive every
  -- occupant home from the same canonical storage that the table indexes.
linkEdges :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
linkEdges mutable left right = do
  writeNext mutable left right
  writePrevious mutable right left
{-# INLINE linkEdges #-}

setCycle3 :: MutableDcel s vertex directed undirected face -> Int -> Int -> Int -> Int -> ST s ()
setCycle3 mutable face e0 e1 e2 = do
  writeNext mutable e0 e1
  writeNext mutable e1 e2
  writeNext mutable e2 e0
  writePrevious mutable e0 e2
  writePrevious mutable e1 e0
  writePrevious mutable e2 e1
  writeFace mutable e0 face
  writeFace mutable e1 face
  writeFace mutable e2 face
  writeFaceEdge mutable face e0
{-# INLINE setCycle3 #-}

faceEdges :: MutableDcel s vertex directed undirected face -> Int -> ST s (Int, Int, Int)
faceEdges mutable face = do
  e0 <- readFaceEdge mutable face
  e1 <- readNext mutable e0
  e2 <- readNext mutable e1
  pure (e0, e1, e2)
{-# INLINE faceEdges #-}

denseLinkEdges :: DenseMutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
denseLinkEdges dense left right = do
  denseWriteNext dense left right
  denseWritePrevious dense right left
{-# INLINE denseLinkEdges #-}

denseSetCycle3 :: DenseMutableDcel s vertex directed undirected face -> Int -> Int -> Int -> Int -> ST s ()
denseSetCycle3 dense face e0 e1 e2 = do
  denseWriteNext dense e0 e1
  denseWriteNext dense e1 e2
  denseWriteNext dense e2 e0
  denseWritePrevious dense e0 e2
  denseWritePrevious dense e1 e0
  denseWritePrevious dense e2 e1
  denseWriteFace dense e0 face
  denseWriteFace dense e1 face
  denseWriteFace dense e2 face
  denseWriteFaceEdge dense face e0
{-# INLINE denseSetCycle3 #-}

denseFaceEdges :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s (Int, Int, Int)
denseFaceEdges dense face = do
  e0 <- denseReadFaceEdge dense face
  e1 <- denseReadNext dense e0
  e2 <- denseReadNext dense e1
  pure (e0, e1, e2)
{-# INLINE denseFaceEdges #-}

denseReadPointX :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Double
denseReadPointX DenseMutableDcel{dmdPointX} = readFlatMutable dmdPointX
denseReadPointY :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Double
denseReadPointY DenseMutableDcel{dmdPointY} = readFlatMutable dmdPointY
{-# INLINE denseReadPointX #-}
{-# INLINE denseReadPointY #-}

-- | Materialize one coordinate slot in a fresh dense point section. The
-- caller owns the capacity witness and supplies the final cardinality once;
-- the point index remains the fresh owner's missing derived view until
-- publication rebuilds it from these authoritative coordinate planes.
denseWriteFreshDefaultPoint
  :: DefaultedVertexDcel s vertex directed undirected face
  -> Int
  -> Double
  -> Double
  -> ST s ()
denseWriteFreshDefaultPoint
  DefaultedVertexDcel
    { defaultedDenseVertexDcel =
        dense@DenseMutableDcel
          { dmdOwner = MutableDcel{mdInitialPointCount}
          , dmdPointX
          , dmdPointY
          , dmdNewConnected
          }
    }
  vertex
  x
  y = do
  writeFlatMutable dmdPointX vertex x
  writeFlatMutable dmdPointY vertex y
  denseWriteVertexOut dense vertex (-1)
  writeFlatMutable dmdNewConnected (vertex - mdInitialPointCount) 0
{-# INLINE denseWriteFreshDefaultPoint #-}

-- | Glue a completed fresh point section into the mutable owner's logical
-- extent. Per-site writes remain local; the aggregate cardinality descends
-- once after the section is complete.
denseCommitFreshDefaultPoints
  :: DefaultedVertexDcel s vertex directed undirected face
  -> Int
  -> ST s ()
denseCommitFreshDefaultPoints
  DefaultedVertexDcel
    { defaultedDenseVertexDcel = DenseMutableDcel
      { dmdOwner = MutableDcel{mdPointCount}
      }
    }
  = writeSTRef mdPointCount
{-# INLINE denseCommitFreshDefaultPoints #-}

denseReadOrigin :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Int
denseReadOrigin DenseMutableDcel{dmdHalfTopology} index = unpackIndex <$> readFlatMutable dmdHalfTopology (4 * index)
denseReadNext :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Int
denseReadNext DenseMutableDcel{dmdHalfTopology} index = unpackIndex <$> readFlatMutable dmdHalfTopology (4 * index + 1)
denseReadPrevious :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Int
denseReadPrevious DenseMutableDcel{dmdHalfTopology} index = unpackIndex <$> readFlatMutable dmdHalfTopology (4 * index + 2)
denseReadFace :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Int
denseReadFace DenseMutableDcel{dmdHalfTopology} index = unpackIndex <$> readFlatMutable dmdHalfTopology (4 * index + 3)
{-# INLINE denseReadOrigin #-}
{-# INLINE denseReadNext #-}
{-# INLINE denseReadPrevious #-}
{-# INLINE denseReadFace #-}

denseWriteOrigin :: DenseMutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
denseWriteOrigin DenseMutableDcel{dmdHalfTopology} index value = writeFlatMutable dmdHalfTopology (4 * index) (packIndex value)
denseWriteNext :: DenseMutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
denseWriteNext DenseMutableDcel{dmdHalfTopology} index value = writeFlatMutable dmdHalfTopology (4 * index + 1) (packIndex value)
denseWritePrevious :: DenseMutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
denseWritePrevious DenseMutableDcel{dmdHalfTopology} index value = writeFlatMutable dmdHalfTopology (4 * index + 2) (packIndex value)
denseWriteFace :: DenseMutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
denseWriteFace DenseMutableDcel{dmdHalfTopology} index value = writeFlatMutable dmdHalfTopology (4 * index + 3) (packIndex value)
{-# INLINE denseWriteOrigin #-}
{-# INLINE denseWriteNext #-}
{-# INLINE denseWritePrevious #-}
{-# INLINE denseWriteFace #-}

denseWriteVertexOut :: DenseMutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
denseWriteVertexOut DenseMutableDcel{dmdVertexOut} index value =
  writeFlatMutable dmdVertexOut index (if value < 0 then noIndex else packIndex value)
{-# INLINE denseWriteVertexOut #-}

-- | Materialize the connectivity section for a vertex proven fresh by the
-- circle-sweep reservation. Arbitrary insertion retains 'markConnected'; this
-- refined write has no resident case to rediscover, and its aggregate count is
-- committed once after the sweep's local sections glue.
denseMarkFreshConnected
  :: DenseMutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> ST s ()
denseMarkFreshConnected dense@DenseMutableDcel{dmdOwner = MutableDcel{mdInitialPointCount}, dmdNewConnected} vertex outgoing = do
  writeFlatMutable dmdNewConnected (vertex - mdInitialPointCount) 1
  denseWriteVertexOut dense vertex outgoing
{-# INLINE denseMarkFreshConnected #-}

denseCommitFreshConnections
  :: DenseMutableDcel s vertex directed undirected face
  -> Int
  -> ST s ()
denseCommitFreshConnections DenseMutableDcel{dmdOwner = MutableDcel{mdConnectedCount}} count =
  modifySTRef' mdConnectedCount (+ count)
{-# INLINE denseCommitFreshConnections #-}

denseReadFaceEdge :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Int
denseReadFaceEdge DenseMutableDcel{dmdFaceEdge} index = do
  value <- readFlatMutable dmdFaceEdge index
  pure (if value == noIndex then -1 else unpackIndex value)
{-# INLINE denseReadFaceEdge #-}

denseWriteFaceEdge :: DenseMutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
denseWriteFaceEdge DenseMutableDcel{dmdFaceEdge} index value =
  writeFlatMutable dmdFaceEdge index (if value < 0 then noIndex else packIndex value)
{-# INLINE denseWriteFaceEdge #-}

denseReadConstraint :: DenseMutableDcel s vertex directed undirected face -> Int -> ST s Bool
denseReadConstraint DenseMutableDcel{dmdConstraint} directed =
  (/= 0) <$> readFlatMutable dmdConstraint (directed `quot` 2)
{-# INLINE denseReadConstraint #-}

readOrigin :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
readOrigin MutableDcel{mdHalfTopology} index = unpackIndex <$> readPaged mdHalfTopology (4 * index)
readNext :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
readNext MutableDcel{mdHalfTopology} index = unpackIndex <$> readPaged mdHalfTopology (4 * index + 1)
readPrevious :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
readPrevious MutableDcel{mdHalfTopology} index = unpackIndex <$> readPaged mdHalfTopology (4 * index + 2)
readFace :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
readFace MutableDcel{mdHalfTopology} index = unpackIndex <$> readPaged mdHalfTopology (4 * index + 3)
{-# INLINE readOrigin #-}
{-# INLINE readNext #-}
{-# INLINE readPrevious #-}
{-# INLINE readFace #-}

writeOrigin :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
writeOrigin MutableDcel{mdHalfTopology} index value = writePaged mdHalfTopology (4 * index) (packIndex value)
writeNext :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
writeNext MutableDcel{mdHalfTopology} index value = writePaged mdHalfTopology (4 * index + 1) (packIndex value)
writePrevious :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
writePrevious MutableDcel{mdHalfTopology} index value = writePaged mdHalfTopology (4 * index + 2) (packIndex value)
writeFace :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
writeFace MutableDcel{mdHalfTopology} index value = writePaged mdHalfTopology (4 * index + 3) (packIndex value)
{-# INLINE writeOrigin #-}
{-# INLINE writeNext #-}
{-# INLINE writePrevious #-}
{-# INLINE writeFace #-}

readVertexOut :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
readVertexOut MutableDcel{mdVertexOut} index = do
  value <- readPaged mdVertexOut index
  pure (if value == noIndex then -1 else unpackIndex value)
{-# INLINE readVertexOut #-}

writeVertexOut :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
writeVertexOut MutableDcel{mdVertexOut} index value =
  writePaged mdVertexOut index (if value < 0 then noIndex else packIndex value)
{-# INLINE writeVertexOut #-}

readFaceEdge :: MutableDcel s vertex directed undirected face -> Int -> ST s Int
readFaceEdge MutableDcel{mdFaceEdge} index = do
  value <- readPaged mdFaceEdge index
  pure (if value == noIndex then -1 else unpackIndex value)
{-# INLINE readFaceEdge #-}

writeFaceEdge :: MutableDcel s vertex directed undirected face -> Int -> Int -> ST s ()
writeFaceEdge MutableDcel{mdFaceEdge} index value =
  writePaged mdFaceEdge index (if value < 0 then noIndex else packIndex value)
{-# INLINE writeFaceEdge #-}

readConstraint :: MutableDcel s vertex directed undirected face -> Int -> ST s Bool
readConstraint MutableDcel{mdConstraint} directed = (/= 0) <$> readPaged mdConstraint (directed `quot` 2)
{-# INLINE readConstraint #-}

setConstraint :: MutableDcel s vertex directed undirected face -> Int -> ST s Bool
setConstraint MutableDcel{mdConstraint, mdConstraintCount, mdConstraintEdges} directed = do
  let !index = directed `quot` 2
  current <- readPaged mdConstraint index
  if current /= 0
    then pure False
    else do
      writePaged mdConstraint index 1
      modifySTRef' mdConstraintCount (+ 1)
      modifySTRef' mdConstraintEdges (IntSet.insert index)
      pure True
{-# INLINE setConstraint #-}

clearConstraint :: MutableDcel s vertex directed undirected face -> Int -> ST s Bool
clearConstraint MutableDcel{mdConstraint, mdConstraintCount, mdConstraintEdges} directed = do
  let !index = directed `quot` 2
  current <- readPaged mdConstraint index
  if current == 0
    then pure False
    else do
      writePaged mdConstraint index 0
      modifySTRef' mdConstraintCount (subtract 1)
      modifySTRef' mdConstraintEdges (IntSet.delete index)
      pure True
{-# INLINE clearConstraint #-}
