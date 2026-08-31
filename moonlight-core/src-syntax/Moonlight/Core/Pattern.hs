{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Pattern
  ( Pattern (..),
    patternVariables,
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core.Identifier.EGraph (PatternVar)
import Prelude (Eq (..), Foldable, Ord (..), Ordering (..), Show (..), foldMap, showParen, showString, ($), (.))

type Pattern :: (Type -> Type) -> Type
-- | Authored open syntax: either a pattern variable or a language node whose children are patterns.
data Pattern f
  = PatternVar PatternVar
  | PatternNode (f (Pattern f))

instance (forall a. Ord a => Ord (f a)) => Eq (Pattern f) where
  leftPattern == rightPattern = compare leftPattern rightPattern == EQ

instance (forall a. Ord a => Ord (f a)) => Ord (Pattern f) where
  compare leftPattern rightPattern =
    case (leftPattern, rightPattern) of
      (PatternVar leftVar, PatternVar rightVar) -> compare leftVar rightVar
      (PatternVar _, PatternNode _) -> LT
      (PatternNode _, PatternVar _) -> GT
      (PatternNode leftNode, PatternNode rightNode) -> compare leftNode rightNode

instance (forall a. Show a => Show (f a)) => Show (Pattern f) where
  showsPrec precedence patternValue =
    case patternValue of
      PatternVar patternVar ->
        showParen (precedence > 10) $
          showString "PatternVar " . showsPrec 11 patternVar
      PatternNode node ->
        showParen (precedence > 10) $
          showString "PatternNode " . showsPrec 11 node

patternVariables :: Foldable f => Pattern f -> Set PatternVar
patternVariables patternValue =
  case patternValue of
    PatternVar patternVar ->
      Set.singleton patternVar
    PatternNode node ->
      foldMap patternVariables node
