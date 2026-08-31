
-- | Type-indexed covering families: the t'Exists', t'Dict' and
-- 'CoveringFamily'/'CoveringConstraints' machinery for enumerating and constraining
-- the members of a kind.
module Moonlight.Category.Pure.CoveringFamily
  ( CoveringFamily (..),
    Exists (..),
    Dict (..),
    CoveringConstraints (..),
    withMember,
    traverseMembers,
    traverseMembers_,
  )
where

import Data.Kind (Constraint, Type)

type Dict :: Constraint -> Type
-- | Evidence that a constraint holds.
data Dict (c :: Constraint) where
  Dict :: c => Dict c

type Exists :: forall k. (k -> Type) -> Type
-- | A covering-family member with its index hidden.
data Exists (w :: k -> Type) where
  Exists :: w member -> Exists w

type CoveringFamily :: forall k. (k -> Type) -> Constraint
-- | A finite enumeration of every witness in an indexed family.
class CoveringFamily (w :: k -> Type) where
  allMembers :: [Exists w]

type CoveringConstraints :: forall k. (k -> Type) -> (k -> Constraint) -> Constraint
-- | Evidence that every covering member satisfies a constraint family.
class CoveringFamily w => CoveringConstraints (w :: k -> Type) (c :: k -> Constraint) where
  constraintDict :: w member -> Dict (c member)

-- | Eliminate an existential member under its recovered constraint.
withMember ::
  forall k (w :: k -> Type) (c :: k -> Constraint) r.
  CoveringConstraints w c =>
  Exists w ->
  (forall (member :: k). c member => w member -> r) ->
  r
withMember (Exists witness) continuation =
  case constraintDict @k @w @c witness of
    Dict -> continuation witness

-- | Evaluate a constrained function at every covering member.
traverseMembers ::
  forall k (w :: k -> Type) (c :: k -> Constraint) r.
  CoveringConstraints w c =>
  (forall (member :: k). c member => w member -> r) ->
  [r]
traverseMembers continuation =
  fmap (\existential -> withMember @k @w @c existential continuation) (allMembers @k @w)

-- | Sequence one applicative action for every constrained member.
traverseMembers_ ::
  forall k (w :: k -> Type) (c :: k -> Constraint) m.
  (CoveringConstraints w c, Applicative m) =>
  (forall (member :: k). c member => w member -> m ()) ->
  m ()
traverseMembers_ continuation =
  sequenceAll (fmap (\existential -> withMember @k @w @c existential continuation) (allMembers @k @w))
  where
    sequenceAll :: Applicative f => [f ()] -> f ()
    sequenceAll [] = pure ()
    sequenceAll (x : xs) = x *> sequenceAll xs
