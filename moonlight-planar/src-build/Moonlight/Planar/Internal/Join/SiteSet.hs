{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Exact transient site sections derived from the authoritative coordinate
-- planes. This is the sole owner of coordinate-set classification for joins,
-- set algebra, and constrained union. Payloads travel as annotations; they do
-- not participate in site identity.
module Moonlight.Planar.Internal.Join.SiteSet
  ( SiteSet
  , siteSetFromTriangulation
  , siteSupportFromTriangulation
  , siteSetSize
  , siteSetRelation
  , siteRelationFromTriangulations
  , siteSetUnionWith
  , siteSetIntersectionWith
  , siteSetDifference
  , siteSetSymmetricDifferenceFromTriangulations
  , siteSetAssocs
  , siteSetPoints
  , delaunayFromSiteSet
  , rebuildCanonicalSiteSet
  ) where

import Control.Monad.ST (ST, runST)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as MapMerge
import Data.Functor.Const (Const (..))
import Data.Maybe (isJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MUV
import Moonlight.Planar.Internal.BulkLoad (delaunayFromAdmittedDistinctAscending)
import Moonlight.Planar.Dcel (numVertices, vertexData, vertexPoint)
import Moonlight.Planar.Internal.HandleDefs (VertexId (..))
import Moonlight.Planar.Internal.Canonical (canonicalizeKnownAscending)
import Moonlight.Planar.Handles.Iterators.FixedIterators
  ( foldVertices'
  )
import Moonlight.Planar.Internal.Paged
  ( pagedUnsafeIndex
  , toVector
  )
import Moonlight.Planar.Internal.PointIndex
  ( MutablePointIndex
  , lookupMutablePoint
  , lookupPointIndex
  , newMutablePointIndex
  , seedMutablePointIndex
  )
import Moonlight.Planar.Internal.Representation
  ( BuildResult (buildTriangulation)
  , Triangulation (..)
  )
import Moonlight.Planar.Internal.Types (BuildError, ConstraintMode (Unconstrained), SiteRelation (..), unitElementDefaults)
import Moonlight.Planar.Point (Point (..), queryPointValue)
import Moonlight.Planar.Internal.Point (QueryPoint (..))

newtype SiteSet annotation = SiteSet (Map.Map QueryPoint annotation)

-- Strict sufficient statistics for the one-pass ordered-map descent used by
-- 'siteSetRelation'. Keeping the census strict prevents a relation query from
-- replacing an intermediate map allocation with a chain of monoidal thunks.
data SiteRelationCensus = SiteRelationCensus !Int !Int !Int

instance Semigroup SiteRelationCensus where
  SiteRelationCensus leftA rightA overlapA <> SiteRelationCensus leftB rightB overlapB =
    SiteRelationCensus
      (leftA + leftB)
      (rightA + rightB)
      (overlapA + overlapB)

instance Monoid SiteRelationCensus where
  mempty = SiteRelationCensus 0 0 0

siteSetFromTriangulation
  :: Triangulation mode annotation directed undirected face
  -> SiteSet annotation
siteSetFromTriangulation triangulation =
  SiteSet
    ( Map.fromList
        [ ( QueryPoint
              ( Point
                  (triPointX triangulation `pagedUnsafeIndex` vertex)
                  (triPointY triangulation `pagedUnsafeIndex` vertex)
              )
          , vertexData triangulation (VertexId (fromIntegral vertex))
          )
        | vertex <- [0 .. numVertices triangulation - 1]
        ]
    )

-- | Coordinate support without touching the boxed annotation plane. Order and
-- set identity are geometric observations; callers that discard annotations
-- should not pay a boxed-page read per vertex merely to manufacture ignored
-- map values.
siteSupportFromTriangulation
  :: Triangulation mode vertex directed undirected face
  -> SiteSet ()
siteSupportFromTriangulation triangulation =
  SiteSet
    ( Map.fromList
        [ ( QueryPoint
              ( Point
                  (triPointX triangulation `pagedUnsafeIndex` vertex)
                  (triPointY triangulation `pagedUnsafeIndex` vertex)
              )
          , ()
          )
        | vertex <- [0 .. numVertices triangulation - 1]
        ]
    )
{-# INLINE siteSupportFromTriangulation #-}

siteSetSize :: SiteSet annotation -> Int
siteSetSize (SiteSet sites) = Map.size sites
{-# INLINE siteSetSize #-}

siteSetRelation
  :: SiteSet leftAnnotation
  -> SiteSet rightAnnotation
  -> SiteRelation
siteSetRelation (SiteSet left) (SiteSet right) =
  siteRelationFromCardinalities
    (leftOnly + overlap)
    (rightOnly + overlap)
    overlap
 where
  SiteRelationCensus leftOnly rightOnly overlap =
    getConst
      ( MapMerge.mergeA
          (MapMerge.traverseMissing (\_ _ -> Const (SiteRelationCensus 1 0 0)))
          (MapMerge.traverseMissing (\_ _ -> Const (SiteRelationCensus 0 1 0)))
          (MapMerge.zipWithAMatched (\_ _ _ -> Const (SiteRelationCensus 0 0 1)))
          left
          right
      )
{-# INLINE siteSetRelation #-}

siteRelationFromTriangulations
  :: Triangulation leftMode leftAnnotation leftDirected leftUndirected leftFace
  -> Triangulation rightMode rightAnnotation rightDirected rightUndirected rightFace
  -> SiteRelation
siteRelationFromTriangulations left right =
  siteRelationFromCardinalities leftCount rightCount overlap
 where
  !leftCount = numVertices left
  !rightCount = numVertices right
  !overlap
    | leftCount <= rightCount = exactOverlapCount right left
    | otherwise = exactOverlapCount left right
{-# INLINE siteRelationFromTriangulations #-}

siteRelationFromCardinalities :: Int -> Int -> Int -> SiteRelation
siteRelationFromCardinalities leftCount rightCount overlap
  | leftCount == rightCount && overlap == leftCount = EqualSites
  | overlap == leftCount = LeftProperSubset
  | overlap == rightCount = RightProperSubset
  | overlap == 0 = DisjointSites
  | otherwise = PartialOverlap overlap
{-# INLINE siteRelationFromCardinalities #-}

exactOverlapCount
  :: Triangulation sourceMode sourceAnnotation sourceDirected sourceUndirected sourceFace
  -> Triangulation indexedMode indexedAnnotation indexedDirected indexedUndirected indexedFace
  -> Int
exactOverlapCount source indexed =
  either
    (const (persistentExactOverlapCount source indexed))
    id
    (transientExactOverlapCount source indexed)
{-# INLINE exactOverlapCount #-}

transientExactOverlapCount
  :: Triangulation sourceMode sourceAnnotation sourceDirected sourceUndirected sourceFace
  -> Triangulation indexedMode indexedAnnotation indexedDirected indexedUndirected indexedFace
  -> Either BuildError Int
transientExactOverlapCount source indexed = runST $ do
  pointIndex <- newMutablePointIndex (numVertices indexed)
  seeded <-
    seedMutablePointIndex
      pointIndex
      (numVertices indexed)
      (readCoordinateX indexed)
      (readCoordinateY indexed)
  case seeded of
    Left failure -> pure (Left failure)
    Right () ->
      fmap Right
        ( U.ifoldM'
            (\count vertex x -> do
               let y = triPointY source `pagedUnsafeIndex` vertex
               match <-
                 lookupMutablePoint
                   pointIndex
                   (readCoordinateX indexed)
                   (readCoordinateY indexed)
                   x
                   y
               pure (if isJust match then count + 1 else count)
            )
            0
            (toVector (triPointX source))
        )
{-# INLINE transientExactOverlapCount #-}

persistentExactOverlapCount
  :: Triangulation sourceMode sourceAnnotation sourceDirected sourceUndirected sourceFace
  -> Triangulation indexedMode indexedAnnotation indexedDirected indexedUndirected indexedFace
  -> Int
persistentExactOverlapCount source indexed =
  foldVertices'
    source
    (\count vertex ->
       if pointOccursIn indexed (vertexPoint source vertex)
         then count + 1
         else count
    )
    0
{-# INLINE persistentExactOverlapCount #-}

siteSetUnionWith
  :: (annotation -> annotation -> annotation)
  -> SiteSet annotation
  -> SiteSet annotation
  -> SiteSet annotation
siteSetUnionWith combine (SiteSet left) (SiteSet right) =
  SiteSet (Map.unionWith combine left right)
{-# INLINE siteSetUnionWith #-}

siteSetIntersectionWith
  :: (leftAnnotation -> rightAnnotation -> annotation)
  -> SiteSet leftAnnotation
  -> SiteSet rightAnnotation
  -> SiteSet annotation
siteSetIntersectionWith combine (SiteSet left) (SiteSet right) =
  SiteSet (Map.intersectionWith combine left right)
{-# INLINE siteSetIntersectionWith #-}

siteSetDifference
  :: SiteSet annotation
  -> SiteSet other
  -> SiteSet annotation
siteSetDifference (SiteSet left) (SiteSet right) = SiteSet (Map.difference left right)
{-# INLINE siteSetDifference #-}

siteSetSymmetricDifferenceFromTriangulations
  :: Triangulation leftMode annotation leftDirected leftUndirected leftFace
  -> Triangulation rightMode annotation rightDirected rightUndirected rightFace
  -> Either BuildError (SiteSet annotation)
siteSetSymmetricDifferenceFromTriangulations left right
  | numVertices left >= numVertices right = indexedSymmetricDifference left right
  | otherwise = indexedSymmetricDifference right left
{-# INLINE siteSetSymmetricDifferenceFromTriangulations #-}

indexedSymmetricDifference
  :: forall sourceMode annotation sourceDirected sourceUndirected sourceFace
            indexedMode indexedDirected indexedUndirected indexedFace
  . Triangulation sourceMode annotation sourceDirected sourceUndirected sourceFace
  -> Triangulation indexedMode annotation indexedDirected indexedUndirected indexedFace
  -> Either BuildError (SiteSet annotation)
indexedSymmetricDifference source indexed =
  fmap (SiteSet . Map.fromList) (runST collectExclusiveAssociations)
 where
  collectExclusiveAssociations
    :: forall state. ST state (Either BuildError [(QueryPoint, annotation)])
  collectExclusiveAssociations = do
    pointIndex <- newMutablePointIndex (numVertices indexed)
    seeded <-
      seedMutablePointIndex
        pointIndex
        (numVertices indexed)
        (readCoordinateX indexed)
        (readCoordinateY indexed)
    case seeded of
      Left failure -> pure (Left failure)
      Right () -> do
        matchedIndexedVertices <- MUV.replicate (numVertices indexed) False
        sourceExclusive <-
          U.ifoldM'
            (collectSourceExclusive pointIndex matchedIndexedVertices)
            []
            (toVector (triPointX source))
        indexedExclusive <-
          U.ifoldM'
            (collectIndexedExclusive matchedIndexedVertices)
            []
            (toVector (triPointX indexed))
        pure (Right (sourceExclusive <> indexedExclusive))
   where
    collectSourceExclusive
      :: MutablePointIndex state
      -> MUV.MVector state Bool
      -> [(QueryPoint, annotation)]
      -> Int
      -> Double
      -> ST state [(QueryPoint, annotation)]
    collectSourceExclusive pointIndex matchedIndexedVertices associations rawVertex x = do
      let vertex = VertexId (fromIntegral rawVertex)
          y = triPointY source `pagedUnsafeIndex` rawVertex
          point = QueryPoint (Point x y)
      match <-
        lookupMutablePoint
          pointIndex
          (readCoordinateX indexed)
          (readCoordinateY indexed)
          x
          y
      case match of
        Nothing -> pure ((point, vertexData source vertex) : associations)
        Just indexedVertex -> do
          MUV.unsafeWrite matchedIndexedVertices indexedVertex True
          pure associations

    collectIndexedExclusive
      :: MUV.MVector state Bool
      -> [(QueryPoint, annotation)]
      -> Int
      -> Double
      -> ST state [(QueryPoint, annotation)]
    collectIndexedExclusive matchedIndexedVertices associations rawVertex x = do
      let vertex = VertexId (fromIntegral rawVertex)
          point = QueryPoint (Point x (triPointY indexed `pagedUnsafeIndex` rawVertex))
      matched <- MUV.unsafeRead matchedIndexedVertices rawVertex
      pure
        ( if matched
            then associations
            else (point, vertexData indexed vertex) : associations
        )
{-# INLINE indexedSymmetricDifference #-}

readCoordinateX
  :: Triangulation mode annotation directed undirected face
  -> Int
  -> ST state Double
readCoordinateX triangulation vertex =
  pure (triPointX triangulation `pagedUnsafeIndex` vertex)
{-# INLINE readCoordinateX #-}

readCoordinateY
  :: Triangulation mode annotation directed undirected face
  -> Int
  -> ST state Double
readCoordinateY triangulation vertex =
  pure (triPointY triangulation `pagedUnsafeIndex` vertex)
{-# INLINE readCoordinateY #-}

pointOccursIn
  :: Triangulation mode annotation directed undirected face
  -> Point
  -> Bool
pointOccursIn triangulation = isJust . lookupPointIn triangulation
{-# INLINE pointOccursIn #-}

lookupPointIn
  :: Triangulation mode annotation directed undirected face
  -> Point
  -> Maybe Int
lookupPointIn triangulation =
  lookupPointIndex
    (triPointX triangulation)
    (triPointY triangulation)
    (triPointIndex triangulation)
{-# INLINE lookupPointIn #-}

siteSetAssocs :: SiteSet annotation -> [(QueryPoint, annotation)]
siteSetAssocs (SiteSet sites) = Map.toAscList sites
{-# INLINE siteSetAssocs #-}

siteSetPoints :: SiteSet annotation -> V.Vector (Point)
siteSetPoints (SiteSet sites) = V.fromList (fmap queryPointValue (Map.keys sites))
{-# INLINE siteSetPoints #-}

-- | Publish an admitted exact site section through the one bulk-construction
-- engine. Its Map order is the trusted strict lexicographic vertex order; the
-- circle sweep still establishes the algorithm's radial execution order.
delaunayFromSiteSet
  :: SiteSet annotation
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
delaunayFromSiteSet sites =
  buildTriangulation
    <$> delaunayFromAdmittedDistinctAscending
      unitElementDefaults
      (V.fromList (siteSetAssocs sites))
{-# INLINE delaunayFromSiteSet #-}

-- | Canonical finite-set publication. The preceding SiteSet build preserves
-- lexicographic vertex order, so Canonical need not rediscover that fact.
rebuildCanonicalSiteSet
  :: SiteSet annotation
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
rebuildCanonicalSiteSet sites =
  delaunayFromSiteSet sites >>= canonicalizeKnownAscending
{-# INLINE rebuildCanonicalSiteSet #-}
