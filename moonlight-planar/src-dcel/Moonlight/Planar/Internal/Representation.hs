{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The stored representation: the structure-of-arrays mesh, the payload
-- traversals that reach its four free parameters, and the records that carry a
-- built mesh beside its telemetry.
module Moonlight.Planar.Internal.Representation
  ( Triangulation (..)
  , SeamFrontierIndex (..)
  , CanonicalAdmission (..)
  , prepareSeamFrontierIndex
  , summarizeSeamChart
  , seamQuarterTurn
  , geometryOnlyPublication
  , promoteConstrained
  , authoringElementDefaults
  , PayloadTraversal
  , vertexPayloads
  , directedPayloads
  , undirectedPayloads
  , facePayloads
  , mapVertices
  , mapDirectedEdges
  , mapUndirectedEdges
  , mapFaces
  , imapUndirectedEdges
  , imapFaces
  , DelaunayTriangulation
  , ConstrainedDelaunayTriangulation
  , BuildResult (..)
  , InsertionResult (..)
  , RefinementReceipt (..)
  , RefinementDomainResult (..)
  , RefinementResult (..)
  ) where

import Control.DeepSeq (NFData (..))
import Data.Foldable (toList)
import qualified Data.IntSet as IntSet
import Data.List (unfoldr)
import qualified Data.List as List
import Data.Primitive.PrimArray (PrimArray)
import qualified Data.Sequence as Seq
import Data.Traversable (foldMapDefault)
import qualified Data.Vector as V
import Data.Word (Word8, Word32)
import Moonlight.Planar.Internal.HandleDefs
  ( FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  )
import Moonlight.Planar.Internal.PackedIndex (unpackIndex, unpackOptionalIndex)
import Moonlight.Planar.Internal.BoxedPaged
  ( BoxedPaged, BoxedFill (..), FillRequirement (..)
  , boxedDefaulted, requiredBoxedFill, boxedFromVector, boxedToVector )
import Moonlight.Planar.Internal.Paged
  ( Paged
  , PublicationStats
  , pagedLength
  , pagedUnsafeIndex
  )
import Moonlight.Planar.Internal.PointIndex (PointIndex)
import Moonlight.Planar.Internal.Types (ConstraintMode (..), ElementDefaults (..), InsertionDisposition, ClosureStats)
import Moonlight.Planar.BuildStats (BuildStats)
import GHC.Generics (Generic)

-- | Immutable finite DCEL. The coordinate pages own the geometry; vertex
-- payloads are free annotations carried alongside it.
-- 'Moonlight.Planar.Internal.Types.HasPosition' is how a point is read
-- out of a payload at the moment of
-- ingestion and is not consulted again, so a payload whose instance later
-- disagrees with where its vertex sits is not a corrupt triangulation — it is a
-- payload nobody asks about position. Half-edge topology is one interleaved
-- arena: edge @e@ owns slots @4e..4e+3@ holding origin, next, previous and
-- face, so a twin pair is one contiguous eight-word record.
-- Directed edges are adjacent twin pairs, so reversal is an XOR with one.
-- Face zero is the unique outer face. Constraint flags are stored once per
-- undirected edge and are zero for ordinary Delaunay triangulations.
-- All four payload components are therefore representational: coercing a
-- newtype through any of them is a coercion, not a rebuild.
type role Triangulation nominal representational representational representational representational

data Triangulation (mode :: ConstraintMode) vertex directed undirected face = Triangulation
  { triPointX :: !(Paged Double)
  , triPointY :: !(Paged Double)
  , -- | Derived position-hash buckets containing vertex handles only. This is
    -- deliberately lazy: geometry is authoritative, so a workload that never
    -- asks an identity question owes no cache construction.
    triPointIndex :: PointIndex
  , triVertexOut :: !(Paged Word32)
  , triVertexData :: !(BoxedPaged 'OptionalFill vertex)
  , triHalfTopology :: !(Paged Word32)
  , triDirectedData :: !(BoxedPaged 'RequiredFill directed)
  , triUndirectedData :: !(BoxedPaged 'RequiredFill undirected)
  , triFaceEdge :: !(Paged Word32)
  , triFaceData :: !(BoxedPaged 'RequiredFill face)
  , triConstraint :: !(Paged Word8)
  , triConstraintCount :: {-# UNPACK #-} !Int
  , -- | Derived exact membership for the sparse constrained edge section.
    -- The flag plane remains authoritative and serializable; this index is
    -- transported with edge rewrites so constraint-only queries need not scan
    -- every ordinary Delaunay edge.
    triConstraintEdges :: !IntSet.IntSet
  , -- | Derived outer-frontier handles for persistent separated joins. The
    -- geometry/topology planes remain authoritative; this cache is omitted
    -- from equality and serialization and is rebuilt only at an explicit
    -- geometry-only preparation boundary.
    triSeamFrontier :: !(Maybe SeamFrontierIndex)
  , -- | Whether the numbering is already the canonical representative.
    -- Minted only by canonical publication, forgotten by every thaw, and,
    -- like the caches, absent from equality and serialization.
    triCanonical :: !CanonicalAdmission
  }
  deriving stock (Show)

-- Geometry/topology compatibility is admitted by construction. Generic
-- reconstruction could splice coordinate planes without transporting topology.
instance
  (NFData vertex, NFData directed, NFData undirected, NFData face)
  => NFData (Triangulation mode vertex directed undirected face) where
  rnf triangulation =
    rnf (triPointX triangulation)
      `seq` rnf (triPointY triangulation)
      `seq` rnf (triPointIndex triangulation)
      `seq` rnf (triVertexOut triangulation)
      `seq` rnf (triVertexData triangulation)
      `seq` rnf (triHalfTopology triangulation)
      `seq` rnf (triDirectedData triangulation)
      `seq` rnf (triUndirectedData triangulation)
      `seq` rnf (triFaceEdge triangulation)
      `seq` rnf (triFaceData triangulation)
      `seq` rnf (triConstraint triangulation)
      `seq` rnf (triConstraintCount triangulation)
      `seq` rnf (triConstraintEdges triangulation)
      `seq` rnf (triSeamFrontier triangulation)
      `seq` rnf (triCanonical triangulation)
  {-# INLINE rnf #-}

-- | The witness that a value is its own canonical representative. Every
-- identifier of a 'CanonicalKnown' value is a function of its geometry, so
-- canonical publication of it is the identity; an 'CanonicalUnknown' value
-- may or may not be, and publication renumbers it to find out.
data CanonicalAdmission
  = CanonicalUnknown
  | CanonicalKnown
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Exact immutable outer cycle plus four deterministic extreme anchors per
-- chart used to seed separated tangents. The sequence is persistent: seam
-- joins compose retained source sections without copying their elements.
data SeamFrontierIndex = SeamFrontierIndex
  { seamFrontierEdges :: !(Seq.Seq Int)
  , seamFrontierXMinimum :: !Double
  , seamFrontierXMaximum :: !Double
  , seamFrontierLowerRightmost :: {-# UNPACK #-} !Int
  , seamFrontierLowerLeftmost :: {-# UNPACK #-} !Int
  , seamFrontierUpperRightmost :: {-# UNPACK #-} !Int
  , seamFrontierUpperLeftmost :: {-# UNPACK #-} !Int
  , -- | The same four extreme anchors in the orientation-preserving
    -- quarter-turn chart @(u,v) = (y,-x)@.  Keeping this alongside the
    -- existing x chart lets a later seam admit a north/south extension
    -- without walking the resident frontier again.
    seamFrontierYMinimum :: !Double
  , seamFrontierYMaximum :: !Double
  , seamFrontierYLowerRightmost :: {-# UNPACK #-} !Int
  , seamFrontierYLowerLeftmost :: {-# UNPACK #-} !Int
  , seamFrontierYUpperRightmost :: {-# UNPACK #-} !Int
  , seamFrontierYUpperLeftmost :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Derive the exact frontier once from an admitted DCEL. This is intentionally
-- an explicit preparation operation: ordinary mutable publication clears the
-- cache instead of silently paying an outer-cycle traversal.
prepareSeamFrontierIndex
  :: Triangulation mode vertex directed undirected face
  -> Maybe SeamFrontierIndex
prepareSeamFrontierIndex triangulation = do
  start <- frontierStart triangulation
  let !directedCount = numDirectedEdges triangulation
      !edges =
        unfoldr
          (frontierStep start directedCount)
          (start, True, directedCount + 1)
      terminal = List.foldl' (\_ edge -> Just edge) Nothing edges
      closesAtStart = do
        lastEdge <- terminal
        pure
          ( unpackIndex
              (pagedUnsafeIndex (triHalfTopology triangulation) (4 * lastEdge + 1))
              == start
          )
  if closesAtStart == Just True
    then summarizeFrontier triangulation (Seq.fromList edges)
    else Nothing
 where
  numDirectedEdges
    :: Triangulation mode vertex directed undirected face
    -> Int
  numDirectedEdges mesh = pagedLength (triHalfTopology mesh) `quot` 4

  frontierStart
    :: Triangulation mode vertex directed undirected face
    -> Maybe Int
  frontierStart mesh =
    unpackOptionalIndex (pagedUnsafeIndex (triFaceEdge mesh) 0)

  frontierStep
    :: Int
    -> Int
    -> (Int, Bool, Int)
    -> Maybe (Int, (Int, Bool, Int))
  frontierStep start directedCount (edge, first, remaining)
    | remaining <= 0 = Nothing
    | edge == start && not first = Nothing
    | edge < 0 || edge >= directedCount = Nothing
    | otherwise =
        let next =
              fromIntegral
                (pagedUnsafeIndex (triHalfTopology triangulation) (4 * edge + 1))
         in Just (edge, (next, False, remaining - 1))

summarizeFrontier
  :: Triangulation mode vertex directed undirected face
  -> Seq.Seq Int
  -> Maybe SeamFrontierIndex
summarizeFrontier triangulation edges =
  case fmap (frontierPoint triangulation) (toList edges) of
    [] -> Nothing
    points -> do
      (minimumX, maximumX, lowerRightmost, lowerLeftmost, upperRightmost, upperLeftmost) <-
        summarizeSeamChart id (zip [0 ..] points)
      (minimumY, maximumY, yLowerRightmost, yLowerLeftmost, yUpperRightmost, yUpperLeftmost) <-
        summarizeSeamChart seamQuarterTurn (zip [0 ..] points)
      pure
        SeamFrontierIndex
          { seamFrontierEdges = edges
          , seamFrontierXMinimum = minimumX
          , seamFrontierXMaximum = maximumX
          , seamFrontierLowerRightmost = lowerRightmost
          , seamFrontierLowerLeftmost = lowerLeftmost
          , seamFrontierUpperRightmost = upperRightmost
          , seamFrontierUpperLeftmost = upperLeftmost
          , seamFrontierYMinimum = minimumY
          , seamFrontierYMaximum = maximumY
          , seamFrontierYLowerRightmost = yLowerRightmost
          , seamFrontierYLowerLeftmost = yLowerLeftmost
          , seamFrontierYUpperRightmost = yUpperRightmost
          , seamFrontierYUpperLeftmost = yUpperLeftmost
          }
 where
  frontierPoint
    :: Triangulation mode vertex directed undirected face
    -> Int
    -> (Double, Double)
  frontierPoint mesh rawEdge =
    let rawVertex = pagedUnsafeIndex (triHalfTopology mesh) (4 * rawEdge)
        vertex = unpackIndex rawVertex
     in ( pagedUnsafeIndex (triPointX mesh) vertex
        , pagedUnsafeIndex (triPointY mesh) vertex
        )

-- | Summarize one chart from a bounded set of frontier points.  The index is
-- retained beside the transformed point so the seam can carry the witness
-- through a residual interval without re-reading the source frontier.
summarizeSeamChart
  :: ((Double, Double) -> (Double, Double))
  -> [(Int, (Double, Double))]
  -> Maybe (Double, Double, Int, Int, Int, Int)
summarizeSeamChart transform points =
  case fmap (\(index, point) -> (index, transform point)) points of
    [] -> Nothing
    first : remaining ->
      let (!minimumValue, !maximumValue) =
            List.foldl'
              (\(!minimumSoFar, !maximumSoFar) (_, (u, _)) ->
                 (min minimumSoFar u, max maximumSoFar u))
              (chartU (snd first), chartU (snd first))
              remaining
          choose preference =
            snd
              ( List.foldl'
                  (\(!bestPoint, !bestIndex) (index, candidate) ->
                     if preference bestPoint candidate
                       then (candidate, index)
                       else (bestPoint, bestIndex))
                  (snd first, fst first)
                  remaining
              )
       in Just
            ( minimumValue
            , maximumValue
            , choose preferRightmost
            , choose preferLeftmost
            , choose preferRightmostUpper
            , choose preferLeftmostUpper
            )
 where
  chartU :: (Double, Double) -> Double
  preferRightmost :: (Double, Double) -> (Double, Double) -> Bool
  preferLeftmost :: (Double, Double) -> (Double, Double) -> Bool
  preferRightmostUpper :: (Double, Double) -> (Double, Double) -> Bool
  preferLeftmostUpper :: (Double, Double) -> (Double, Double) -> Bool
  chartU (u, _) = u
  preferRightmost (bestU, bestV) (candidateU, candidateV) =
    candidateU > bestU || (candidateU == bestU && candidateV < bestV)
  preferLeftmost (bestU, bestV) (candidateU, candidateV) =
    candidateU < bestU || (candidateU == bestU && candidateV < bestV)
  preferRightmostUpper (bestU, bestV) (candidateU, candidateV) =
    candidateU > bestU || (candidateU == bestU && candidateV > bestV)
  preferLeftmostUpper (bestU, bestV) (candidateU, candidateV) =
    candidateU < bestU || (candidateU == bestU && candidateV > bestV)

-- | Orientation-preserving quarter turn used by the north/south seam chart.
seamQuarterTurn :: (Double, Double) -> (Double, Double)
seamQuarterTurn (x, y) = (y, -x)

-- | Forget every payload while preparing a geometry-only mesh for persistent
-- extension. Each payload plane is replaced directly with a defaulted,
-- zero-page unit store; no source payload is traversed or densely rebuilt.
-- Coordinates, topology, constraints, counts, and derived point identity
-- remain authoritative, while future elements inherit the unit defaults.
geometryOnlyPublication
  :: Triangulation mode vertex directed undirected face
  -> Triangulation mode () () () ()
geometryOnlyPublication triangulation =
  triangulation
    { triVertexData = boxedDefaulted () (pagedLength (triPointX triangulation))
    , triDirectedData = boxedDefaulted () (pagedLength (triHalfTopology triangulation) `quot` 4)
    , triUndirectedData = boxedDefaulted () (pagedLength (triHalfTopology triangulation) `quot` 8)
    , triFaceData = boxedDefaulted () (pagedLength (triFaceEdge triangulation))
    , triSeamFrontier =
        case triSeamFrontier triangulation of
          Just frontier -> Just frontier
          Nothing -> prepareSeamFrontierIndex triangulation
    }

-- The point index is a derived cache and therefore not an observable part of
-- the mesh value. Structural equality compares every semantic plane and
-- default while deliberately refusing to construct or compare that cache.
instance
  ( Eq vertex
  , Eq directed
  , Eq undirected
  , Eq face
  ) => Eq (Triangulation mode vertex directed undirected face) where
  left == right =
    triPointX left == triPointX right
      && triPointY left == triPointY right
      && triVertexOut left == triVertexOut right
      && triVertexData left == triVertexData right
      && triHalfTopology left == triHalfTopology right
      && triDirectedData left == triDirectedData right
      && triUndirectedData left == triUndirectedData right
      && triFaceEdge left == triFaceEdge right
      && triFaceData left == triFaceData right
      && triConstraint left == triConstraint right
      && triConstraintCount left == triConstraintCount right

promoteConstrained
  :: Triangulation 'Unconstrained vertex directed undirected face
  -> Triangulation 'Constrained vertex directed undirected face
promoteConstrained Triangulation{
  triPointX, triPointY, triPointIndex, triVertexOut, triVertexData, triHalfTopology,
  triDirectedData, triUndirectedData, triFaceEdge, triFaceData,
  triConstraint, triConstraintCount, triConstraintEdges, triSeamFrontier, triCanonical
  } =
  Triangulation{
    triPointX, triPointY, triPointIndex, triVertexOut, triVertexData, triHalfTopology,
    triDirectedData, triUndirectedData, triFaceEdge, triFaceData,
    triConstraint, triConstraintCount, triConstraintEdges, triSeamFrontier, triCanonical
    }

-- | Authoring view of the three required fills. No resident defaults record
-- competes with the payload stores or can diverge from them during an edit.
authoringElementDefaults
  :: Triangulation mode vertex directed undirected face
  -> ElementDefaults directed undirected face
authoringElementDefaults triangulation = ElementDefaults
  (requiredBoxedFill (triDirectedData triangulation))
  (requiredBoxedFill (triUndirectedData triangulation))
  (requiredBoxedFill (triFaceData triangulation))

-- | A traversal of every occurrence of one payload parameter, in the van
-- Laarhoven encoding: an effectful visit that may change the payload's type.
-- The 'Applicative' belongs to the caller, so one traversal per parameter
-- serves relabeling, collection and genuinely effectful annotation alike
-- instead of a separate function for each.
type PayloadTraversal source target payload payload' =
  forall f. Applicative f => (payload -> f payload') -> source -> f target

-- | Every stored vertex payload, in vertex order, then its optional fill.
--
-- Dense authored vertices have no fill. Geometry-only publication does: that
-- payload must be visited once and preserved just like the resident values.
vertexPayloads
  :: PayloadTraversal
      (Triangulation mode vertex directed undirected face)
      (Triangulation mode vertex' directed undirected face)
      vertex
      vertex'
vertexPayloads visit triangulation =
  (\payloads -> triangulation{triVertexData = payloads}) <$> traverse visit (triVertexData triangulation)

-- | Every stored directed-edge payload, then the default a later directed edge
-- will inherit.
--
-- The default is visited once and written to its sole owner, the store fill.
directedPayloads
  :: PayloadTraversal
      (Triangulation mode vertex directed undirected face)
      (Triangulation mode vertex directed' undirected face)
      directed
      directed'
directedPayloads visit triangulation =
  (\payloads -> triangulation{triDirectedData = payloads}) <$> traverse visit (triDirectedData triangulation)

-- | Every stored undirected-edge payload, then the default a later undirected
-- edge will inherit.
undirectedPayloads
  :: PayloadTraversal
      (Triangulation mode vertex directed undirected face)
      (Triangulation mode vertex directed undirected' face)
      undirected
      undirected'
undirectedPayloads visit triangulation =
  (\payloads -> triangulation{triUndirectedData = payloads}) <$> traverse visit (triUndirectedData triangulation)

-- | Every stored face payload, then the default a later face will inherit.
facePayloads
  :: PayloadTraversal
      (Triangulation mode vertex directed undirected face)
      (Triangulation mode vertex directed undirected face')
      face
      face'
facePayloads visit triangulation =
  (\payloads -> triangulation{triFaceData = payloads}) <$> traverse visit (triFaceData triangulation)

-- | Ranges over the face payload, which is the last parameter and so the only
-- one a class of this kind can reach. The other three payloads have exactly
-- the same structure under 'vertexPayloads', 'directedPayloads' and
-- 'undirectedPayloads'; they are simply not spellable as instances here.
--
-- 'mapFaces' rather than the traversal, because it leaves an unmaterialized
-- page unmaterialized. The two agree on everything a 'BoxedPaged' lets anyone
-- observe, which is what the coherence law asks and all it asks.
instance Functor (Triangulation mode vertex directed undirected) where
  fmap = mapFaces
  {-# INLINE fmap #-}

-- | Folds the stored face payloads and then the default, so 'length' is one
-- greater than the number of stored faces. A fold that skipped the default
-- would report a triangulation as holding a value it does hold.
instance Foldable (Triangulation mode vertex directed undirected) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Traversable (Triangulation mode vertex directed undirected) where
  traverse = facePayloads
  {-# INLINE traverse #-}

-- | Sparse payload mapping also maps the fill at its storage owner, so future
-- insertion and resident observations cannot disagree about the new default.
mapDirectedEdges
  :: (directed -> directed')
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed' undirected face
mapDirectedEdges f triangulation =
  triangulation
    { triDirectedData = fmap f (triDirectedData triangulation)
    }

-- | Map every undirected-edge annotation and its future-element default.
mapUndirectedEdges
  :: (undirected -> undirected')
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected' face
mapUndirectedEdges f triangulation =
  triangulation
    { triUndirectedData = fmap f (triUndirectedData triangulation)
    }

-- | Map every face annotation and its future-element default.
mapFaces
  :: (face -> face')
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected face'
mapFaces f triangulation =
  triangulation
    { triFaceData = fmap f (triFaceData triangulation)
    }

-- | The vertex component is free, like the other three. Geometry owns the
-- points, so a payload map cannot move one — the image type need not even have
-- a position to speak of. An optional geometry-publication fill maps with it.
mapVertices
  :: (vertex -> vertex')
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex' directed undirected face
mapVertices f triangulation =
  triangulation{triVertexData = fmap f (triVertexData triangulation)}

-- | Materialize every resident undirected-edge annotation in handle order
-- while installing the declared fallback for edges created by a later edit.
imapUndirectedEdges
  :: undirected'
  -> (UndirectedEdgeId -> undirected -> undirected')
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected' face
imapUndirectedEdges fallback relabel triangulation =
  triangulation
    { triUndirectedData =
        boxedFromVector (Fill fallback)
          (V.imap (\index -> relabel (UndirectedEdgeId (fromIntegral index))) payloads)
    }
 where
  payloads = boxedToVector (triUndirectedData triangulation)

-- | Materialize every resident face annotation in handle order while
-- installing the declared fallback for faces created by a later edit.
imapFaces
  :: face'
  -> (FaceId -> face -> face')
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected face'
imapFaces fallback relabel triangulation =
  triangulation
    { triFaceData =
        boxedFromVector (Fill fallback)
          (V.imap (\index -> relabel (FaceId (fromIntegral index))) payloads)
    }
 where
  payloads = boxedToVector (triFaceData triangulation)

-- | Geometry-only unconstrained Delaunay triangulation.
type DelaunayTriangulation vertex = Triangulation 'Unconstrained vertex () () ()

-- | Geometry-only constrained Delaunay triangulation.
type ConstrainedDelaunayTriangulation vertex = Triangulation 'Constrained vertex () () ()

-- | A constructed triangulation and the canonical handle chosen for each input.
--
-- The result is a value, not a history: derived 'Eq'/'Show' would observe
-- 'buildStats' through a facade that hides it, so neither instance exists.
data BuildResult mode vertex directed undirected face = BuildResult
  { -- | The immutable constructed mesh.
    buildTriangulation :: !(Triangulation mode vertex directed undirected face)
  , -- | Canonical vertex handle for each input position, including duplicates.
    buildInputVertices :: !(PrimArray Word32)
  , buildStats :: !BuildStats
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

-- | Published insertion result, selected vertex, disposition, and work receipt.
data InsertionResult mode vertex directed undirected face = InsertionResult
  { insertionTriangulation :: !(Triangulation mode vertex directed undirected face)
  , insertionVertex :: !VertexId
  , insertionDisposition :: !InsertionDisposition
  , insertionStats :: !BuildStats
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance
  (Eq vertex, Eq directed, Eq undirected, Eq face)
  => Eq (InsertionResult mode vertex directed undirected face)
deriving stock instance
  (Show vertex, Show directed, Show undirected, Show face)
  => Show (InsertionResult mode vertex directed undirected face)

-- | Exact support touched by one refinement publication. Checked local
-- refinement uses this as the positive receipt accompanying its typed
-- obstruction surface; unrestricted refinement deliberately avoids the
-- additional support scan.
data RefinementReceipt = RefinementReceipt
  { refinementVisitedJoinFaces :: !(V.Vector FaceId)
  , refinementVisitedProtectedFaces :: !(V.Vector FaceId)
  , refinementCreatedFaces :: !(V.Vector FaceId)
    -- ^ Face slots whose published triangle changed or was appended.
  , refinementFinalPermittedFaces :: !(V.Vector FaceId)
    -- ^ Exact final face lineage admitted by a checked local domain.
  , refinementFinalInterfaceIncidence :: !(V.Vector (UndirectedEdgeId, FaceId, FaceId))
  , refinementTouchedEdges :: !(V.Vector UndirectedEdgeId)
  , refinementRemovedEdges :: !(V.Vector UndirectedEdgeId)
  , refinementInterfaceBoundaryReads :: {-# UNPACK #-} !Int
  , refinementAttemptedBoundaryCrossings :: {-# UNPACK #-} !Int
  , refinementPublicationStats :: !PublicationStats
    -- ^ Page publication work measured by the transaction owner.
  , refinementClosureStats :: !ClosureStats
    -- ^ Sizes of the closure the transaction's domain spans (admitted faces
    -- with their collar, their edges, vertices and pairs), counted from the
    -- domain's membership; no validation descent produces them.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A locally refined section paired with the proof of what the checked
-- interpreter observed and rewrote. Ordinary refinement does not pay to
-- construct this proof.
data RefinementDomainResult mode vertex directed undirected face = RefinementDomainResult
  { refinementDomainResult :: !(RefinementResult mode vertex directed undirected face)
  , refinementDomainReceipt :: !RefinementReceipt
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

-- | Refined mesh together with the budget, exclusion, and support outcome.
data RefinementResult mode vertex directed undirected face = RefinementResult
  { -- | The immutable mesh after all admitted refinement steps.
    refinedTriangulation :: !(Triangulation mode vertex directed undirected face)
  , refinementStats :: !BuildStats
  , refinementAddedVertices :: {-# UNPACK #-} !Int
  -- | Whether the quality worklist drained. 'False' means the vertex budget
  -- stopped the run with work outstanding. A drained worklist can still leave
  -- faces the quality bounds condemn but no admissible Steiner point can fix;
  -- auditing the result is the caller's to ask for, not a cost every run pays.
  , refinementComplete :: !Bool
  , -- | Faces deliberately excluded by barrier-depth policy.
    refinementExcludedFaces :: !(V.Vector FaceId)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)
