{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The join of two triangulations: a Delaunay representative of the union of
-- their sites.
module Moonlight.Planar.Internal.Join
  ( joinNormalForm
  , joinBalanced
  ) where

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Algebra.Pure.Join (JoinSemilattice (join))
import Moonlight.Planar.BulkLoad (empty)
import Moonlight.Planar.Internal.Join.Plan
  ( PairPlan (..)
  , planPair
  )
import Moonlight.Planar.Internal.Join.Seam (executeSeam)
import Moonlight.Planar.Internal.Join.SiteSet
  ( SiteSet
  , rebuildCanonicalSiteSet
  , siteSetAssocs
  , siteSetSize
  )
import Moonlight.Planar.Internal.Session
  ( insertAdmittedVertexAtCombining
  , withLocalSession
  )
import Moonlight.Planar.Internal.Tournament
  ( interpretTournament
  , planTournament
  )
import Moonlight.Planar.Types

-- | A Delaunay representative of the union of two site sets.
--
-- Skewed operands descend through the existing local copy-on-write session, so
-- the larger operand's vertex handles and untouched pages survive. Comparable
-- operands may merge along a separating seam or rebuild from their combined
-- site set. Every schedule returns valid topology;
-- 'Moonlight.Planar.Canonical.canonicalize' is the
-- separate physical observation when construction-independent numbering is
-- required.
--
-- Empty and structurally identical operands return an existing value verbatim.
-- Algebraic agreement between all schedules is stated by equal canonical
-- observations; structural equality continues to describe exact resident
-- representation.
joinNormalForm
  :: (Eq annotation, JoinSemilattice annotation)
  => Triangulation 'Unconstrained annotation () () ()
  -> Triangulation 'Unconstrained annotation () () ()
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
joinNormalForm left right = executePairPlan (planPair left right) left right

executePairPlan
  :: JoinSemilattice annotation
  => PairPlan annotation
  -> Triangulation 'Unconstrained annotation () () ()
  -> Triangulation 'Unconstrained annotation () () ()
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
executePairPlan pairPlan left right =
  case pairPlan of
    ReturnLeftOperand -> Right left
    ReturnRightOperand -> Right right
    InsertLeftIntoRight sites -> insertSites sites right
    InsertRightIntoLeft sites -> insertSites sites left
    MergeSeparated seamPlan -> executeSeam seamPlan left right
    RebuildCanonicalUnion sites -> rebuildCanonicalSiteSet sites

insertSites
  :: JoinSemilattice annotation
  => SiteSet annotation
  -> Triangulation 'Unconstrained annotation () () ()
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
insertSites sites base = do
  ((), inserted, _) <-
    withLocalSession
      base
      (siteSetSize sites)
      ( traverse_
          (\(point, annotation) ->
             void (insertAdmittedVertexAtCombining join point annotation)
          )
          (siteSetAssocs sites)
      )
  pure inserted

-- | Combine by a balanced tournament rather than by a fold.
--
-- Associativity and commutativity make every bracketing canonically equivalent,
-- so this is a cost choice and not a semantic one. A left fold republishes an
-- accumulator that grows by one shard per step and so rebuilds @Θ(nk)@ sites
-- over @k@ shards; halving the list rebuilds @Θ(n log k)@.
--
-- The specialization below is load-bearing rather than decorative. Without it
-- this function is the only one on the path that stays polymorphic, and every
-- join in the tournament pays for a dictionary while a caller's own fold at a
-- known element type does not. That alone cost a factor of two and hid the
-- advantage this function exists for.
--
-- The tournament retains the binary operation's shortcut and annotation-gluing
-- semantics rather than inventing a second n-ary implementation.
joinBalanced
  :: (Eq annotation, JoinSemilattice annotation)
  => [Triangulation 'Unconstrained annotation () () ()]
  -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
joinBalanced [] = Right (empty unitElementDefaults)
joinBalanced (first : rest) =
  interpretTournament joinNormalForm (planTournament (first :| rest))
{-# SPECIALIZE joinBalanced
  :: [Triangulation 'Unconstrained () () () ()]
  -> Either BuildError (Triangulation 'Unconstrained () () () ()) #-}
