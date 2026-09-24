{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Finite-set operations on unconstrained meshes. Every
-- constructing operation returns the finite-arena obstruction instead of
-- laundering it through a partial class instance.
module Moonlight.Planar.SetAlgebra
  ( siteRelation
  , union
  , unions
  , intersection
  , intersectionWith
  , difference
  , symmetricDifference
  ) where

import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import qualified Data.Vector as V
import Moonlight.Planar.BulkLoad (empty)
import Moonlight.Planar.Dcel (numVertices, vertexData, vertexPoint, vertexPoints)
import Moonlight.Planar.Handles.Iterators.FixedIterators (vertices)
import Moonlight.Planar.Internal.Join (joinBalanced, joinNormalForm)
import Moonlight.Planar.Internal.Join.SiteSet
  ( rebuildCanonicalSiteSet
  , siteSetDifference
  , siteSetFromTriangulation
  , siteSetIntersectionWith
  , siteSetPoints
  , siteRelationFromTriangulations
  , siteSetSymmetricDifferenceFromTriangulations
  , siteSupportFromTriangulation
  )
import Moonlight.Planar.Internal.Representation (Triangulation)
import Moonlight.Planar.Internal.Types (BuildError (PointLocationFailed), ConstraintMode (Unconstrained), InsertionDisposition (..), SiteRelation (..), unitElementDefaults)
import Moonlight.Planar.BuildStats (BuildStats)
import Moonlight.Planar.Point (Point)
import Moonlight.Algebra.Pure.Join (JoinSemilattice)
import Moonlight.Planar.Internal.Session
  ( Session
  , insertVertexAt
  , refuse
  , removeAtNear
  , removeManyAt
  , withLocalSession
  )

-- | Exact relation between two triangulations' coordinate supports. Vertex
-- annotations and topology-element payloads are observations over the support;
-- none participates in this classification.
siteRelation
  :: Triangulation leftMode leftAnnotation leftDirected leftUndirected leftFace
  -> Triangulation rightMode rightAnnotation rightDirected rightUndirected rightFace
  -> SiteRelation
siteRelation left right =
  siteRelationFromTriangulations left right
{-# INLINE siteRelation #-}

-- | A valid Delaunay representative of both site sets. Use
-- 'Moonlight.Planar.Canonical.canonicalize'
-- when construction-independent dense numbering is required.
union
  :: (Eq annotation, JoinSemilattice annotation)
  => Triangulation 'Unconstrained annotation () () ()
  -> Triangulation 'Unconstrained annotation () () ()
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
union = joinNormalForm

-- | @union@ over a list, folded as a balanced tournament.
unions
  :: (Eq annotation, JoinSemilattice annotation)
  => [Triangulation 'Unconstrained annotation () () ()]
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
unions = joinBalanced

-- | The sites both meshes hold.
intersection
  :: Triangulation 'Unconstrained () () () ()
  -> Triangulation 'Unconstrained () () () ()
  -> Either BuildError (Triangulation 'Unconstrained () () () ())
intersection left right
  | numVertices left == 0 = Right left
  | numVertices right == 0 = Right right
  | otherwise =
      case siteRelationFromTriangulations left right of
        EqualSites -> Right left
        LeftProperSubset -> Right left
        RightProperSubset -> Right right
        DisjointSites -> Right (empty unitElementDefaults)
        PartialOverlap overlap -> intersectPartialOverlap overlap
 where
  leftSites = siteSupportFromTriangulation left
  rightSites = siteSupportFromTriangulation right
  rebuildIntersection = intersectionWith (\_ _ -> ()) left right
  intersectPartialOverlap overlap
    | leftRemoved <= rightRemoved
    , removalDeltaIsEligible leftRemoved overlap =
        removeExpectedFrom left (siteSetPoints (siteSetDifference leftSites rightSites))
    | rightRemoved < leftRemoved
    , removalDeltaIsEligible rightRemoved overlap =
        removeExpectedFrom right (siteSetPoints (siteSetDifference rightSites leftSites))
    | otherwise = rebuildIntersection
   where
    leftRemoved = numVertices left - overlap
    rightRemoved = numVertices right - overlap
{-# INLINE intersection #-}

-- | The sites both meshes hold, with the result annotation computed from the
-- left and right annotations at that exact coordinate. The combiner is called
-- only for shared sites, in left-then-right order; geometry remains the sole
-- authority for membership. @intersectionWith const left mask@ is the
-- annotation-preserving restriction of @left@ to @mask@'s support.
intersectionWith
  :: (leftAnnotation -> rightAnnotation -> annotation)
  -> Triangulation 'Unconstrained leftAnnotation () () ()
  -> Triangulation 'Unconstrained rightAnnotation () () ()
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
intersectionWith combine left right =
  rebuildCanonicalSiteSet
    ( siteSetIntersectionWith
        combine
        (siteSetFromTriangulation left)
        (siteSetFromTriangulation right)
    )
{-# INLINE intersectionWith #-}

-- | The left's sites, less the right's.
difference
  :: Triangulation 'Unconstrained leftAnnotation () () ()
  -> Triangulation 'Unconstrained rightAnnotation () () ()
  -> Either BuildError (Triangulation 'Unconstrained leftAnnotation () () ())
difference left right
  | numVertices left == 0 || numVertices right == 0 = Right left
  | numVertices right < numVertices left
  , removalDeltaIsEligible (numVertices right) (numVertices left - numVertices right) =
      removeAvailableFrom left (vertexPoints right)
  | otherwise =
      rebuildCanonicalSiteSet
        ( siteSetDifference
            (siteSetFromTriangulation left)
            (siteSupportFromTriangulation right)
        )
{-# INLINE difference #-}

-- | The sites exactly one mesh holds.
symmetricDifference
  :: Triangulation 'Unconstrained annotation () () ()
  -> Triangulation 'Unconstrained annotation () () ()
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
symmetricDifference left right
  | numVertices left == 0 = Right right
  | numVertices right == 0 = Right left
  | numVertices right < numVertices left
  , toggleDeltaIsEligible (numVertices right) (numVertices left - numVertices right) =
      toggleIncoming left right
  | numVertices left < numVertices right
  , toggleDeltaIsEligible (numVertices left) (numVertices right - numVertices left) =
      toggleIncoming right left
  | otherwise =
      siteSetSymmetricDifferenceFromTriangulations left right
        >>= rebuildCanonicalSiteSet
{-# INLINE symmetricDifference #-}

removalDeltaIsEligible :: Int -> Int -> Bool
removalDeltaIsEligible removed survivors = removed <= survivors `quot` 128
{-# INLINE removalDeltaIsEligible #-}

toggleDeltaIsEligible :: Int -> Int -> Bool
toggleDeltaIsEligible incoming residentRemainder = incoming <= residentRemainder `quot` 128
{-# INLINE toggleDeltaIsEligible #-}

removeAvailableFrom
  :: forall annotation. Triangulation 'Unconstrained annotation () () ()
  -> V.Vector (Point)
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
removeAvailableFrom triangulation points =
  fmap published
    (withLocalSession triangulation 0 (V.any isJust <$> removeManyAt points))
 where
  published
    :: (Bool, Triangulation 'Unconstrained annotation () () (), BuildStats)
    -> Triangulation 'Unconstrained annotation () () ()
  published (removed, revised, _) = if removed then revised else triangulation
{-# INLINE removeAvailableFrom #-}

removeExpectedFrom
  :: Triangulation 'Unconstrained annotation () () ()
  -> V.Vector (Point)
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
removeExpectedFrom triangulation points =
  fmap (\(_, revised, _) -> revised)
    (withLocalSession triangulation 0 (removeExpectedPoints points))
{-# INLINE removeExpectedFrom #-}

removeExpectedPoints
  :: V.Vector (Point)
  -> Session state annotation () () () ()
removeExpectedPoints points = do
  outcomes <- removeManyAt points
  V.zipWithM_
    (\point outcome -> maybe (refuse (PointLocationFailed point)) (const (pure ())) outcome)
    points
    outcomes
{-# INLINE removeExpectedPoints #-}

toggleIncoming
  :: Triangulation 'Unconstrained annotation () () ()
  -> Triangulation 'Unconstrained annotation () () ()
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
toggleIncoming base incoming =
  fmap (\(_, revised, _) -> revised)
    (withLocalSession base (numVertices incoming) (toggleIncomingVertices incoming))
{-# INLINE toggleIncoming #-}

toggleIncomingVertices
  :: Triangulation 'Unconstrained annotation () () ()
  -> Session state annotation () () () ()
toggleIncomingVertices incoming =
  traverse_
    (\vertex -> do
      let point = vertexPoint incoming vertex
          annotation = vertexData incoming vertex
      (fresh, disposition) <- insertVertexAt point annotation
      case disposition of
        Inserted -> pure ()
        AlreadyPresent -> do
          outcome <- removeAtNear fresh point
          maybe (refuse (PointLocationFailed point)) (const (pure ())) outcome
    )
    (vertices incoming)
{-# INLINE toggleIncomingVertices #-}
