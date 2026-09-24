{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | The sole physical planner for binary and n-ary joins. It derives exact
-- local compatibility facts, selects one schedule, and leaves execution to a
-- consumer; sequential and concurrent interpreters share the same tournament
-- tree rather than inventing pairing policies of their own.
module Moonlight.Planar.Internal.Join.Plan
  ( PairPlan (..)
  , planPair
  ) where

import Moonlight.Planar.Dcel (numVertices)
import Moonlight.Algebra.Pure.Join (JoinSemilattice (join))
import Moonlight.Planar.Internal.BulkLoad (empty)
import Moonlight.Planar.Internal.Join.Seam (SeamPlan, planSeam)
import Moonlight.Planar.Internal.Join.SiteSet
  ( SiteSet
  , siteSetFromTriangulation
  , siteSetRelation
  , siteSetUnionWith
  )
import Moonlight.Planar.Internal.Representation (Triangulation)
import Moonlight.Planar.Internal.Types
  ( ConstraintMode (Unconstrained)
  , SiteRelation (..)
  , unitElementDefaults
  )

data PairPlan annotation
  = ReturnLeftOperand
  | ReturnRightOperand
  | InsertLeftIntoRight !(SiteSet annotation)
  | InsertRightIntoLeft !(SiteSet annotation)
  | MergeSeparated !SeamPlan
  | RebuildCanonicalUnion !(SiteSet annotation)

-- | Stage cheap facts before exact set classification. Empty and structurally
-- identical operands return verbatim; skewed pairs preserve the larger value
-- through local insertion before seam planning is considered.
planPair
  :: (Eq annotation, JoinSemilattice annotation)
  => Triangulation 'Unconstrained annotation () () ()
  -> Triangulation 'Unconstrained annotation () () ()
  -> PairPlan annotation
planPair left right
  -- Two zero-site values may differ structurally; the package-owned empty is
  -- the identity, so preserve the other representative when only one is it.
  | leftCount == 0 && rightCount == 0 =
      if left == empty unitElementDefaults
        then ReturnRightOperand
        else ReturnLeftOperand
  | leftCount == 0 = ReturnRightOperand
  | rightCount == 0 = ReturnLeftOperand
  | left == right = ReturnLeftOperand
  | insertionIsCheaper leftCount rightCount = InsertLeftIntoRight leftSites
  | insertionIsCheaper rightCount leftCount = InsertRightIntoLeft rightSites
  | Just seamPlan <- planSeam left right = MergeSeparated seamPlan
  | otherwise =
      case siteSetRelation leftSites rightSites of
        EqualSites -> InsertLeftIntoRight leftSites
        LeftProperSubset -> InsertLeftIntoRight leftSites
        RightProperSubset -> InsertRightIntoLeft rightSites
        DisjointSites -> rebuildUnion
        PartialOverlap _ -> rebuildUnion
 where
  !leftCount = numVertices left
  !rightCount = numVertices right
  leftSites = siteSetFromTriangulation left
  rightSites = siteSetFromTriangulation right
  rebuildUnion =
    RebuildCanonicalUnion
      (siteSetUnionWith join leftSites rightSites)

-- A transaction reuses an existing topology only when the added side is small
-- enough that its expected local cavities beat one bulk sweep. This is an
-- internal cost estimate, deliberately not a caller-controlled threshold.
insertionIsCheaper :: Int -> Int -> Bool
insertionIsCheaper addition base = addition <= 64 || addition <= base `quot` 8
{-# INLINE insertionIsCheaper #-}
