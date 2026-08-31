{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Core.Language
  ( ZipMatch (..),
    Language,
    sameNodeShape,
    zipSameNodeShape,
    HasConstructorTag (..),
  )
where

import Data.Foldable (toList)
import Data.Functor (void)
import Data.Kind (Constraint, Type)
import Data.Traversable (mapAccumL)
import Prelude (Bool, Maybe (Just, Nothing), Ord, Traversable, otherwise, sequenceA, (==))

type ZipMatch :: (Type -> Type) -> Constraint
-- | Structural one-layer matching. @zipMatch@ succeeds iff the two nodes have the same shape, aligning children positionally and pairing every child.
class Traversable f => ZipMatch f where
  zipMatch :: f left -> f right -> Maybe (f (left, right))

type Language :: (Type -> Type) -> Constraint
class (Traversable f, forall a. Ord a => Ord (f a)) => Language f

instance (Traversable f, forall a. Ord a => Ord (f a)) => Language f

sameNodeShape :: Language f => f left -> f right -> Bool
sameNodeShape leftNode rightNode =
  void leftNode == void rightNode

zipSameNodeShape ::
  Language f =>
  f left ->
  f right ->
  Maybe (f (left, right))
zipSameNodeShape leftNode rightNode
  | sameNodeShape leftNode rightNode =
      case mapAccumL consumeRightChild (toList rightNode) leftNode of
        ([], zippedNode) ->
          sequenceA zippedNode
        _ ->
          Nothing
  | otherwise =
      Nothing
  where
    consumeRightChild ::
      [right] ->
      left ->
      ([right], Maybe (left, right))
    consumeRightChild rightChildren leftChild =
      case rightChildren of
        rightChild : trailingRightChildren ->
          (trailingRightChildren, Just (leftChild, rightChild))
        [] ->
          ([], Nothing)

type HasConstructorTag :: (Type -> Type) -> Constraint
class (Language f, Ord (ConstructorTag f)) => HasConstructorTag f where
  type ConstructorTag f :: Type
  constructorTag :: f a -> ConstructorTag f
