{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | One owned editing transaction over a triangulation: thaw once, edit, publish once.
module Moonlight.Triangulation.Internal.Session
  ( RemovalOutcome (..)
  , Session
  , withSession
  , withLocalSession
  , insertVertex
  , insertVertexAt
  , insertVertexAtNear
  , insertVertexAtNearVertex
  , insertAdmittedVertexAtCombining
  , removeAt
  , removeAtNear
  , removeManyAt
  , removeManyAtNear
  , excise
  , refuse
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.ST (ST)
import Data.Bits (xor)
import qualified Data.Vector as V
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Insertion
  ( insertAdmittedPointCombining
  , insertPointCombining
  )
import Moonlight.Triangulation.Internal.Excision (removeMutable)
import Moonlight.Triangulation.Internal.Location (MutableLocation (..), locateMutable)
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState (OperationState)
import Moonlight.Triangulation.Internal.Paged (TransactionShape (..))
import Moonlight.Triangulation.Math (canonicalPoint)
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Transaction (runTransaction)
import Moonlight.Triangulation.Internal.Types
import GHC.Generics (Generic)

-- | What a single removal produced: the removed position and payload, plus the
-- slot and position changed by swap compaction when another vertex moved.
data RemovalOutcome vertex = RemovalOutcome
  { removalOutcomePoint :: !(Point)
  , removalOutcomeData :: !vertex
  , removalOutcomeSwap :: !(Maybe (VertexId, Point))
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | An edit sequence against one thawed mesh.
--
-- The two published entry points before this one — an insertion session and a
-- removal session — each handed the caller a function and could not compose,
-- so a caller who wanted to remove a hundred vertices and insert fifty thawed
-- twice and paid the O(n) publication a session exists to delete. They are the
-- same transaction; only the verb differed. This is that transaction, and the
-- verbs are its primitives.
--
-- The public session algebra and the private exact-site single-insertion
-- interpreter cross the same hidden transaction boundary. Sessions provide
-- composition; the private interpreter reaches that boundary only with
-- evidence from the immediately preceding frozen read. A fold of persistent
-- verbs and a session over the same verbs therefore differ only in which
-- intermediate meshes they publish.
--
-- Refusal short-circuits: the first 'BuildError' abandons the rest of the
-- sequence, and 'withSession' freezes nothing. A partly edited arena cannot
-- reach a caller as a published triangulation.
newtype Session s vertex directed undirected face a = Session
  { stepSession
      :: MutableDcel s vertex directed undirected face
      -> OperationState s
      -> ST s (Either BuildError a)
  }

instance Functor (Session s vertex directed undirected face) where
  fmap f (Session step) = Session $ \mesh operation -> fmap (fmap f) (step mesh operation)
  {-# INLINE fmap #-}

instance Applicative (Session s vertex directed undirected face) where
  pure a = Session $ \_ _ -> pure (Right a)
  {-# INLINE pure #-}
  Session left <*> Session right = Session $ \mesh operation -> do
    outcome <- left mesh operation
    case outcome of
      Left refusal -> pure (Left refusal)
      Right f -> fmap (fmap f) (right mesh operation)
  {-# INLINE (<*>) #-}

instance Monad (Session s vertex directed undirected face) where
  Session step >>= f = Session $ \mesh operation -> do
    outcome <- step mesh operation
    case outcome of
      Left refusal -> pure (Left refusal)
      Right a -> stepSession (f a) mesh operation
  {-# INLINE (>>=) #-}

-- | Abandon the transaction. Nothing is published.
refuse :: BuildError -> Session s vertex directed undirected face a
refuse failure = Session $ \_ _ -> pure (Left failure)
{-# INLINE refuse #-}

-- | Insert a vertex, answering the handle it was given. A point already
-- present keeps its handle and takes the new payload.
insertVertex
  :: HasPosition vertex
  => vertex
  -> Session s vertex directed undirected face VertexId
insertVertex payload = Session $ \mesh operation ->
  fmap
    (fmap (VertexId . fromIntegral . fst))
    (insertPointCombining (\_ replacement -> replacement) Nothing mesh operation (position payload) payload)

-- | Insert at a stated point, answering the handle and whether a site was
-- created. 'insertVertex' is this with the point read out of the payload; a
-- caller that computed the point — a constraint split, a Steiner refinement —
-- states it rather than round-tripping through 'HasPosition'.
insertVertexAt
  :: Point
  -> vertex
  -> Session s vertex directed undirected face (VertexId, InsertionDisposition)
insertVertexAt = insertVertexAtFrom (const (pure Nothing))

-- | 'insertVertexAt' with the walk seeded at a face the caller vouches for --
-- typically the face a locate on the just-published value settled on, which
-- is exact on the mesh this transaction thawed. The seed is a hint, not an
-- authority: an out-of-range face degrades to the unhinted descent, and the
-- walk corrects.
insertVertexAtNear
  :: FaceId
  -> Point
  -> vertex
  -> Session s vertex directed undirected face (VertexId, InsertionDisposition)
insertVertexAtNear seed =
  insertVertexAtFrom (const (pure (Just (faceIdIndex seed))))

-- | 'insertVertexAt' with the exact walk restricted first to an inner face in
-- the supplied vertex's star. This is the native boundary for hierarchy and
-- nearest-neighbour hints, which answer vertices rather than faces. The seed
-- remains non-authoritative: an invalid handle or a vertex with no incident
-- inner face degrades to the ordinary unhinted descent.
insertVertexAtNearVertex
  :: VertexId
  -> Point
  -> vertex
  -> Session s vertex directed undirected face (VertexId, InsertionDisposition)
insertVertexAtNearVertex seed = insertVertexAtFrom (`interiorFaceNearVertex` seed)

insertVertexAtFrom
  :: (MutableDcel s vertex directed undirected face -> ST s (Maybe Int))
  -> Point
  -> vertex
  -> Session s vertex directed undirected face (VertexId, InsertionDisposition)
insertVertexAtFrom seedFor point payload = Session $ \mesh operation -> do
  seed <- seedFor mesh
  fmap
    (fmap (\(vertex, disposition) -> (VertexId (fromIntegral vertex), disposition)))
    (insertPointCombining
       (\_ replacement -> replacement)
       seed
       mesh
       operation
       point
       payload
    )
{-# INLINE insertVertexAtFrom #-}

-- | Insert an admitted site while combining an occupied annotation. This is
-- package-internal descent from a boundary that already owns coordinate
-- admission; only location and annotation gluing remain here.
insertAdmittedVertexAtCombining
  :: (vertex -> vertex -> vertex)
  -> QueryPoint
  -> vertex
  -> Session s vertex directed undirected face (VertexId, InsertionDisposition)
insertAdmittedVertexAtCombining combine queryPoint payload = Session $ \mesh operation ->
  fmap
    (fmap (\(vertex, disposition) -> (VertexId (fromIntegral vertex), disposition)))
    (insertAdmittedPointCombining combine Nothing mesh operation queryPoint payload)

-- | Remove the vertex standing at a point, answering 'Nothing' when no vertex
-- stands there.
--
-- Keyed by position rather than by handle because removal swap-compacts the
-- arenas: every outstanding t'VertexId' may relocate, and over a sequence of
-- removals a handle-keyed verb would force the caller to thread every
-- relocation by hand. A position is invariant under compaction. The relocation
-- is still reported, in the 'RemovalOutcome', for callers holding handles.
-- The question is an identity question -- which vertex stands at this exact
-- position. A session that has committed to identity work ('removeManyAt',
-- 'excise') answers it through the identity index in O(1); one that has not
-- answers it with a single walk, because a published index is a lazy rebuild
-- and forcing a whole-mesh build to answer one question is the wrong trade.
removeAt
  :: Point
  -> Session s vertex directed undirected face (Maybe (RemovalOutcome vertex))
removeAt point = Session $ \mesh operation -> do
  indexed <- identityIndexActive mesh
  if indexed
    then removeIndexed mesh operation point
    else walkAndRemove mesh operation Nothing point

removeIndexed
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Point
  -> ST s (Either BuildError (Maybe (RemovalOutcome vertex)))
removeIndexed mesh operation point = do
  located <- lookupPointVertex mesh point
  case located of
    Just vertex -> fmap (fmap Just) (removeMutableOutcome mesh operation vertex)
    Nothing -> pure (Right Nothing)

-- | Remove the vertex standing at a point, starting from a caller-supplied
-- near vertex -- a locate hint from an external structure such as the
-- Delaunay hierarchy. The guess is a hint, not an authority: a slot renamed
-- by swap-compaction or out of range degrades to a walk hinted by the
-- guess's incident face, and the walk corrects.
removeAtNear
  :: VertexId
  -> Point
  -> Session s vertex directed undirected face (Maybe (RemovalOutcome vertex))
removeAtNear guessVertex point = Session $ \mesh operation -> do
  indexed <- identityIndexActive mesh
  let !guess = vertexIdIndex guessVertex
      !query = canonicalPoint point
  if indexed
    then removeIndexed mesh operation query
    else do
      vertices <- pointCount mesh
      if guess < 0 || guess >= vertices
        then walkAndRemove mesh operation Nothing query
        else do
          stored <- pointAt mesh guess
          if stored == query
            then fmap (fmap Just) (removeMutableOutcome mesh operation guess)
            else do
              hint <- interiorFaceAtVertex mesh guess
              walkAndRemove mesh operation hint query

interiorFaceNearVertex
  :: MutableDcel s vertex directed undirected face
  -> VertexId
  -> ST s (Maybe Int)
interiorFaceNearVertex mesh vertex@(VertexId rawVertex) = do
  vertices <- pointCount mesh
  if toInteger rawVertex >= toInteger vertices
    then pure Nothing
    else interiorFaceAtVertex mesh (vertexIdIndex vertex)

interiorFaceAtVertex
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> ST s (Maybe Int)
interiorFaceAtVertex mesh vertex = do
  outgoing <- readVertexOut mesh vertex
  if outgoing < 0
    then pure Nothing
    else do
      primary <- innerFaceOf outgoing
      maybe (innerFaceOf (outgoing `xor` 1)) (pure . Just) primary
 where
  innerFaceOf edge = do
    face <- readFace mesh edge
    pure (if face > 0 then Just face else Nothing)

walkAndRemove
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Maybe Int
  -> Point
  -> ST s (Either BuildError (Maybe (RemovalOutcome vertex)))
walkAndRemove mesh operation hint query = do
  located <- locateMutable mesh operation hint query
  case located of
    Left obstruction -> pure (Left obstruction)
    Right (MutableOnVertex vertex) ->
      fmap (fmap Just) (removeMutableOutcome mesh operation vertex)
    Right _ -> pure (Right Nothing)

-- | Remove the vertex standing at each point, answering per point in order.
-- One session-level decision buys the locate strategy: few removals walk the
-- mesh, each a ~O(sqrt n) descent, while many seed the existing mutable
-- open-addressed identity section once from coordinate authority and answer
-- each question by hash. The guard squares the crossover to avoid a root.
removeManyAt
  :: V.Vector (Point)
  -> Session s vertex directed undirected face (V.Vector (Maybe (RemovalOutcome vertex)))
removeManyAt points =
  withBatchIdentityForLoad (V.length points) (V.mapM removeAt points)

-- | Install the mutable identity section only around a dense removal program.
-- It is a local section over the coordinate arenas, not a new published cache:
-- on every result path it is discarded and freeze glues a lazy @PointIndex@
-- back from those arenas. Sparse loads retain the existing locate walk.
withBatchIdentityForLoad
  :: Int
  -> Session s vertex directed undirected face result
  -> Session s vertex directed undirected face result
withBatchIdentityForLoad count (Session action) = Session $ \mesh operation -> do
  vertices <- pointCount mesh
  if count * count > 10 * vertices
    then do
      activated <- activateBatchPointIndex mesh
      case activated of
        Left failure -> pure (Left failure)
        Right () -> do
          outcome <- action mesh operation
          discardBatchPointIndex mesh
          pure outcome
    else action mesh operation

identityCommitted :: Session s vertex directed undirected face Bool
identityCommitted = Session $ \mesh _ -> fmap Right (identityIndexActive mesh)
{-# INLINE identityCommitted #-}

-- | 'removeManyAt' with a near vertex per point where the caller has one — a
-- hierarchy sample, a previous answer. The batch commits to its load first,
-- so the identity-index crossover decides the regime once: a dense batch buys
-- its local mutable table and never examines a guess, a sparse one walks from
-- its guesses.
removeManyAtNear
  :: V.Vector (Maybe VertexId)
  -> V.Vector (Point)
  -> Session s vertex directed undirected face (V.Vector (Maybe (RemovalOutcome vertex)))
removeManyAtNear guesses points =
  withBatchIdentityForLoad (V.length points) $ do
    indexed <- identityCommitted
    if indexed
      then V.mapM removeAt points
      else
        V.zipWithM
          (\guess point -> maybe (removeAt point) (\vertex -> removeAtNear vertex point) guess)
          guesses
          points

-- | Remove a stated vertex. Sound only while the handle still denotes what the
-- caller means: the first removal in a sequence compacts the arenas, so a
-- handle taken before it may name a different vertex after. @removeAt@ is the
-- verb for a sequence; this is the verb for a handle the caller has just been
-- given and has not yet let a removal run underneath.
excise
  :: VertexId
  -> Session s vertex directed undirected face (RemovalOutcome vertex)
excise requested@(VertexId raw) = Session $ \mesh operation -> do
  vertices <- pointCount mesh
  if toInteger raw >= toInteger vertices
    then pure (Left (RemovalVertexOutOfRange requested vertices))
    else do
      activatePointIndex mesh
      removeMutableOutcome mesh operation (vertexIdIndex requested)

removeMutableOutcome
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> ST s (Either BuildError (RemovalOutcome vertex))
removeMutableOutcome mesh operation vertex =
  fmap (fmap removalOutcomeOf) (removeMutable mesh operation vertex)

removalOutcomeOf
  :: (Point, vertex, Maybe (Int, Point))
  -> RemovalOutcome vertex
removalOutcomeOf (point, payload, swapped) =
  RemovalOutcome
    { removalOutcomePoint = point
    , removalOutcomeData = payload
    , removalOutcomeSwap =
        (\(slot, standing) -> (VertexId (fromIntegral slot), standing)) <$> swapped
    }

-- | Run an edit sequence: thaw once, edit, freeze once, and publish the
-- counters the whole transaction charged.
--
-- The reservation is taken for the peak vertex count, so it is the insertion
-- count that sizes it; removals only shrink the mesh and a mixed sequence
-- cannot exceed the peak an insert-only sequence of the same count reaches.
--
-- The session cannot escape its callback: the state token is universally
-- quantified, so the mesh it addresses is dead by the time the frozen
-- triangulation is returned.
withSession
  :: forall mode vertex directed undirected face result
   . Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. Session s vertex directed undirected face result)
  -> Either BuildError (result, Triangulation mode vertex directed undirected face, BuildStats)
withSession triangulation additional session =
  runTransaction
    id
    DenseTransaction
    triangulation
    additional
    (\mesh operation -> stepSession session mesh operation)

-- | The local-edit transaction: copy-on-write pages, publication proportional
-- to what the edit dirtied. This is the section for singleton persistent
-- verbs, whose one edit cannot amortize a dense copy of the whole mesh.
withLocalSession
  :: forall mode vertex directed undirected face result
   . Triangulation mode vertex directed undirected face
  -> Int
  -> (forall s. Session s vertex directed undirected face result)
  -> Either BuildError (result, Triangulation mode vertex directed undirected face, BuildStats)
withLocalSession triangulation additional session =
  runTransaction
    id
    LocalTransaction
    triangulation
    additional
    (\mesh operation -> stepSession session mesh operation)
