{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Canonical publication: the same triangulation, renumbered so that its
-- representation is a function of its geometry alone.
module Moonlight.Triangulation.Internal.Canonical
  ( canonicalize
  , canonicalizeKnownAscending
  ) where

import Control.Monad (when)
import Control.Monad.ST (runST)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MUV
import Moonlight.Triangulation.Dcel (numFaces, numUndirectedEdges, numVertices, vertexData)
import Moonlight.Triangulation.Internal.HandleDefs (VertexId (..))
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.Paged (Paged, pagedUnsafeIndex)
import Moonlight.Triangulation.Internal.Representation (Triangulation (..))
import Moonlight.Triangulation.Internal.Types (BuildError)

-- | Whether the caller has already established the authoritative vertex-order
-- section. The canonicalizer still owns every edge, directed-edge, face,
-- anchor, and constraint normalization; this witness affects only its vertex
-- rank entrance.
data VertexOrderAdmission
  = VertexOrderUnknown
  | VertexOrderKnownAscending

-- | Renumber a triangulation into its canonical representation.
--
-- Two triangulations of the same sites are the same triangulation — Delaunay
-- uniqueness says so, and the tie-break on an exactly cocircular quadrilateral
-- is keyed on coordinates rather than on identifiers so that it stays true.
-- What differs between two builds of one site set is only /numbering/: which
-- vertex got index 0, which half-edge got the even slot, where a face's cycle
-- was anchored. All of that records the schedule the value was constructed by,
-- and none of it is geometry.
--
-- This is what removes it. Every identifier is assigned from the geometry:
--
--     * vertices in lexicographic coordinate rank;
--     * undirected edges in lexicographic rank of their endpoint pair, each
--       taken low first, so the pair is an unordered pair by construction;
--     * of a pair's two half-edges, the even slot is the one leaving the
--       lower-ranked endpoint, which keeps twinning an @xor@ with one;
--     * inner faces in order of the least half-edge on their boundary, which
--       is a single index rather than a vertex tuple and so needs no special
--       case for a cycle that is not a triangle;
--     * every anchor — each vertex's outgoing edge, each face's edge — set to
--       the least admissible half-edge.
--
-- Construction therefore need not run in canonical order. Local insertion and
-- seam fusion may preserve their cheaper schedule-specific numbering; callers
-- invoke this operation only when they require the construction-independent
-- physical representative used to observe the finite-set laws.
--
-- Element payloads are @()@ because a renumbering is a bijection and could
-- carry them, but nothing that wants that exists; the writers a general form
-- would need were retired when their last caller went. Vertex payloads travel
-- with their vertices, and constraint flags with their edges, so a constrained
-- triangulation canonicalizes as readily as an unconstrained one.
canonicalize
  :: Triangulation mode vertex () () ()
  -> Either BuildError (Triangulation mode vertex () () ())
canonicalize = canonicalizeWithVertexOrder VertexOrderUnknown

-- | Canonicalize a triangulation whose vertices were appended in strict
-- lexicographic coordinate order by the admitted SiteSet bulk entrance.
-- This is an execution witness, not a second numbering implementation.
canonicalizeKnownAscending
  :: Triangulation mode vertex () () ()
  -> Either BuildError (Triangulation mode vertex () () ())
canonicalizeKnownAscending = canonicalizeWithVertexOrder VertexOrderKnownAscending

canonicalizeWithVertexOrder
  :: VertexOrderAdmission
  -> Triangulation mode vertex () () ()
  -> Either BuildError (Triangulation mode vertex () () ())
canonicalizeWithVertexOrder vertexOrderAdmission source = runST $ do
  mutable <-
    newMutableDcel
      (triElementDefaults source)
      (exactDcelCapacity vertexTotal directedTotal faceTotal)
  forRange 0 vertexTotal $ \canonical -> do
    let !old = vertexOrder `U.unsafeIndex` canonical
    _ <-
      appendVertexCoordinates
        mutable
        (coordinateX `pagedUnsafeIndex` old)
        (coordinateY `pagedUnsafeIndex` old)
        (vertexData source (VertexId (fromIntegral old)))
    pure ()
  _ <- addEdgeBlock mutable edgeTotal
  _ <- addFaceBlock mutable (faceTotal - 1)
  forRange 0 directedTotal $ \canonical -> do
    let !old = directedFromCanonical `U.unsafeIndex` canonical
    writeOrigin mutable canonical (vertexRank `U.unsafeIndex` originOf old)
    writeNext mutable canonical (directedToCanonical `U.unsafeIndex` topology (4 * old + 1))
    writePrevious mutable canonical (directedToCanonical `U.unsafeIndex` topology (4 * old + 2))
    writeFace mutable canonical (faceRank `U.unsafeIndex` topology (4 * old + 3))
  forRange 0 vertexTotal $ \canonical -> do
    let !least = leastOutgoing `U.unsafeIndex` canonical
    if least == absent
      then writeVertexOut mutable canonical (-1)
      else markConnected mutable canonical least
  forRange 0 faceTotal $ \canonical -> do
    let !least = leastOnFace `U.unsafeIndex` oldFaceOf canonical
    writeFaceEdge mutable canonical (if least == absent then -1 else least)
  forRange 0 edgeTotal $ \canonicalUndirected ->
    when (constraintFlag `pagedUnsafeIndex` (edgeOrder `U.unsafeIndex` canonicalUndirected) /= 0) $
      () <$ setConstraint mutable (2 * canonicalUndirected)
  freezeTriangulation mutable
 where
  !vertexTotal = numVertices source
  !edgeTotal = numUndirectedEdges source
  !faceTotal = numFaces source
  !directedTotal = 2 * edgeTotal

  -- Read the arenas rather than the handle accessors. Every one of these is
  -- indexed a few times per element, and the accessors would box a t'Point' or
  -- an identifier newtype at each of them.
  !coordinateX = triPointX source
  !coordinateY = triPointY source
  !topologyArena = triHalfTopology source
  !constraintFlag = triConstraint source
  topology slot = fromIntegral (topologyArena `pagedUnsafeIndex` slot) :: Int
  originOf directed = topology (4 * directed)

  -- Vertices, in lexicographic coordinate rank. Sorting a vector of keys with
  -- the type's own ordering rather than an index vector under a closure: the
  -- comparison then specializes instead of being an unknown call per step.
  --
  -- Sortedness is checked first, because a great many of the meshes handed to
  -- this function already have it and the check costs one linear scan against a
  -- sort's @n log n@. A seam merge is the reason: it copies two canonically
  -- numbered operands into one arena, lower abscissa first, and the sites of
  -- the result are then already in rank order by construction. Nothing about
  -- the schedule is assumed here — the coordinates are simply read and
  -- believed, so a mesh that arrives sorted for any other reason is served just
  -- as well.
  !alreadyRanked =
    case vertexOrderAdmission of
      VertexOrderUnknown -> coordinatesAscend vertexTotal coordinateX coordinateY
      VertexOrderKnownAscending -> True
  !vertexOrder
    | alreadyRanked = U.enumFromN 0 vertexTotal
    | otherwise =
        thirdColumn $
          sortedVector
            ( U.generate
                vertexTotal
                ( \index ->
                    ( coordinateX `pagedUnsafeIndex` index
                    , coordinateY `pagedUnsafeIndex` index
                    , index
                    )
                )
            )
  !vertexRank
    | alreadyRanked = vertexOrder
    | otherwise = invertPermutation vertexTotal vertexOrder

  -- Undirected edges, in lexicographic rank of their endpoint pair.
  --
  -- Both components of the key are vertex ranks, so they are already dense
  -- indices into a range this function knows: comparing them is a counting
  -- sort's job, not a comparison sort's. Two stable passes — the high endpoint
  -- first, then the low one — leave the pairs in lexicographic order, in time
  -- linear in the edges and the vertices rather than @E log E@.
  !edgeLow =
    U.generate edgeTotal $ \index ->
      min
        (vertexRank `U.unsafeIndex` originOf (2 * index))
        (vertexRank `U.unsafeIndex` originOf (2 * index + 1))
  !edgeHigh =
    U.generate edgeTotal $ \index ->
      max
        (vertexRank `U.unsafeIndex` originOf (2 * index))
        (vertexRank `U.unsafeIndex` originOf (2 * index + 1))
  !edgeOrder =
    countingSortOn vertexTotal edgeLow $
      countingSortOn vertexTotal edgeHigh (U.enumFromN 0 edgeTotal)

  -- The even half of each canonical pair leaves the lower-ranked endpoint.
  !directedFromCanonical =
    U.generate directedTotal $ \canonical ->
      let !oldEdge = edgeOrder `U.unsafeIndex` (canonical `quot` 2)
          !evenHalf = 2 * oldEdge
          !leavesLower =
            vertexRank `U.unsafeIndex` originOf evenHalf
              <= vertexRank `U.unsafeIndex` originOf (evenHalf + 1)
       in if even canonical == leavesLower then evenHalf else evenHalf + 1
  !directedToCanonical = invertPermutation directedTotal directedFromCanonical

  -- The least canonical half-edge on each old face and leaving each canonical
  -- vertex, in one pass. Anchors have to be a function of the geometry too, or
  -- two builds of one site set would publish the same cycles anchored in
  -- different places.
  (!leastOnFace, !leastOutgoing) = runST $ do
    faces <- MUV.replicate (max 1 faceTotal) absent
    vertices <- MUV.replicate (max 1 vertexTotal) absent
    forRange 0 directedTotal $ \canonical -> do
      let !old = directedFromCanonical `U.unsafeIndex` canonical
          !face = topology (4 * old + 3)
          !rank = vertexRank `U.unsafeIndex` originOf old
      onFace <- MUV.unsafeRead faces face
      when (canonical < onFace) (MUV.unsafeWrite faces face canonical)
      leaving <- MUV.unsafeRead vertices rank
      when (canonical < leaving) (MUV.unsafeWrite vertices rank canonical)
    (,) <$> U.unsafeFreeze faces <*> U.unsafeFreeze vertices

  -- Inner faces, in order of the least canonical half-edge on their boundary.
  -- The outer face keeps index zero, which the arena reserves for it anyway.
  --
  -- No two faces share a least half-edge, so this key is injective and the
  -- ordering can be read off by inverting it: mark each face at its own least
  -- half-edge, then scan the half-edges in order. That is one linear pass and
  -- no comparisons at all. A face the scan never reaches has no boundary — only
  -- reachable in a mesh with no edges — and follows in old index order so that
  -- the result stays a permutation whatever it is handed.
  !innerFaceOrder = U.create $ do
    owner <- MUV.replicate (max 1 directedTotal) absent
    forRange 1 faceTotal $ \face -> do
      let !least = leastOnFace `U.unsafeIndex` face
      when (least /= absent) (MUV.unsafeWrite owner least face)
    emitted <- MUV.replicate (max 1 faceTotal) False
    out <- MUV.new (max 0 (faceTotal - 1))
    let scan !slot !filled
          | slot >= directedTotal = pure filled
          | otherwise = do
              !face <- MUV.unsafeRead owner slot
              if face == absent
                then scan (slot + 1) filled
                else do
                  MUV.unsafeWrite out filled face
                  MUV.unsafeWrite emitted face True
                  scan (slot + 1) (filled + 1)
        sweep !face !filled
          | face >= faceTotal = pure ()
          | otherwise = do
              !done <- MUV.unsafeRead emitted face
              if done
                then sweep (face + 1) filled
                else do
                  MUV.unsafeWrite out filled face
                  sweep (face + 1) (filled + 1)
    scan 0 0 >>= sweep 1
    pure out
  !faceRank = U.create $ do
    ranks <- MUV.replicate (max 1 faceTotal) 0
    U.iforM_ innerFaceOrder $ \rank old -> MUV.unsafeWrite ranks old (rank + 1)
    pure ranks
  oldFaceOf canonical
    | canonical == 0 = 0
    | otherwise = innerFaceOrder `U.unsafeIndex` (canonical - 1)


-- | No half-edge reaches this face or vertex. A one-site mesh has such a
-- vertex and an edgeless outer face; the arena spells the same absence as a
-- packed sentinel, which is not a value an index may take.
absent :: Int
absent = maxBound

-- | @[from, to)@, without materializing the range as a list.
forRange :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
forRange from to action = go from
 where
  go !index
    | index >= to = pure ()
    | otherwise = action index >> go (index + 1)
{-# INLINE forRange #-}

sortedVector :: (U.Unbox key, Ord key) => U.Vector key -> U.Vector key
sortedVector = U.modify Intro.sort
{-# INLINE sortedVector #-}

-- | Stably reorder @items@ by a key that is already a dense index below
-- @range@, in time linear in both.
--
-- Applied least-significant key first, repeated application leaves the items in
-- lexicographic order of the whole key — which is what makes a two-component
-- ordering over vertex ranks cost @O(V + E)@ instead of @O(E log E)@.
countingSortOn :: Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
countingSortOn range keys items = U.create $ do
  counts <- MUV.replicate (range + 1) 0
  U.forM_ items $ \item ->
    MUV.unsafeModify counts (+ 1) (keys `U.unsafeIndex` item)
  let prefix !key !running
        | key > range = pure ()
        | otherwise = do
            !count <- MUV.unsafeRead counts key
            MUV.unsafeWrite counts key running
            prefix (key + 1) (running + count)
  prefix 0 0
  out <- MUV.new (max 1 (U.length items))
  U.forM_ items $ \item -> do
    let !key = keys `U.unsafeIndex` item
    !slot <- MUV.unsafeRead counts key
    MUV.unsafeWrite counts key (slot + 1)
    MUV.unsafeWrite out slot item
  pure (MUV.slice 0 (U.length items) out)

thirdColumn :: (U.Unbox a, U.Unbox b) => U.Vector (a, b, Int) -> U.Vector Int
thirdColumn = U.map (\(_, _, index) -> index)
{-# INLINE thirdColumn #-}

-- | Whether the stored sites are already in strict lexicographic order, in
-- which case ranking them is the identity and both permutations are free.
--
-- Strict rather than non-strict: a triangulation stores each site once, so
-- equal adjacent coordinates would mean a mesh this function has no ordering
-- for, and it is the sort's business to say so rather than this predicate's.
coordinatesAscend :: Int -> Paged Double -> Paged Double -> Bool
coordinatesAscend total x y = go 1
 where
  go !index
    | index >= total = True
    | otherwise =
        let !previousX = x `pagedUnsafeIndex` (index - 1)
            !currentX = x `pagedUnsafeIndex` index
         in case compare previousX currentX of
              LT -> go (index + 1)
              GT -> False
              EQ ->
                y `pagedUnsafeIndex` (index - 1) < y `pagedUnsafeIndex` index
                  && go (index + 1)

-- | @inverse ! (order ! i) == i@: the rank each element was given.
invertPermutation :: Int -> U.Vector Int -> U.Vector Int
invertPermutation count order = U.create $ do
  inverse <- MUV.replicate (max 1 count) 0
  U.iforM_ order $ \rank element -> MUV.unsafeWrite inverse element rank
  pure inverse
