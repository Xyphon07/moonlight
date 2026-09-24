{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | The indexed relation boundary. Instances implement a join-enriched
-- involutive category: composition preserves joins and bottom in each argument,
-- and dagger preserves joins, reverses composition, and is involutive.
--
-- Identities are 'Obj' arrows of the existing category. In a closed-crible
-- backend identities, composition, joins /and/ bottom include local closure.
-- This interface prescribes no matrix, finite carrier, or decision procedure.
module Moonlight.Category.Pure.Indexed.Relation
  ( Relation (..)
  , RelationInclusion (..)
  , EquivalenceClosure (..)
  ) where

import Moonlight.Algebra.Pure.Join (JoinSemilattice)
import Moonlight.Category.Pure.Indexed.Category (Category, Obj)

class
  (Category r, forall a b. JoinSemilattice (r a b)) =>
  Relation r
  where
  dagger :: r a b -> r b a
  bottomRelation :: Obj r a -> Obj r b -> r a b

-- | Exact inclusion is an optional capability. 'Right False' is a refutation,
-- never absence from an incomplete search index; inability to decide is 'Left'.
class Relation r => RelationInclusion r where
  type InclusionRefusal r
  includedIn :: r a b -> r a b -> Either (InclusionRefusal r) Bool

-- | Construct the exact least symmetric idempotent containing a seed.
-- Symbolic construction need not decide equality or enumerate its denotation.
class Relation r => EquivalenceClosure r where
  type ClosureRefusal r
  symmetricIdempotentClosure :: r a a -> Either (ClosureRefusal r) (r a a)
