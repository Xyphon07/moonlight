{-# LANGUAGE ScopedTypeVariables #-}

-- | The cheaply-canonicalizable slice of equational theory over a 'Language':
-- 'StructuralLaw' names node shapes whose canonical form is a sort (commutativity today).
-- Arbitrary equations belong to the e-graph layer, not here.
module Moonlight.Core.Theory
  ( StructuralLaw (..),
    TheorySpec (..),
    emptyTheorySpec,
    commutativeBinary,
    canonicalizeLayerByTheory,
    canonicalizePatternByTheory,
    expandPatternByTheory,
  )
where

import Data.Foldable (Foldable, toList)
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (traverse)
import Moonlight.Core.Language (Language)
import Moonlight.Core.Pattern (Pattern (..))
import Prelude (Ord (..), const, (.), (<$>))

type StructuralLaw :: (Type -> Type) -> Type -> Type
data StructuralLaw f a
  = Ordinary
  | CommutativeBinary !(a -> a -> f a)

type TheorySpec :: (Type -> Type) -> Type
data TheorySpec f = TheorySpec
  { tsClassify :: forall a. f a -> StructuralLaw f a
  }

emptyTheorySpec :: TheorySpec f
emptyTheorySpec = TheorySpec {tsClassify = const Ordinary}

commutativeBinary :: (a -> a -> f a) -> StructuralLaw f a
commutativeBinary =
  CommutativeBinary

canonicalizeLayerByTheory :: (Foldable f, Ord a) => TheorySpec f -> f a -> f a
canonicalizeLayerByTheory spec node =
  case tsClassify spec node of
    Ordinary -> node
    CommutativeBinary law -> canonicalizeCommutativeBinary law node

canonicalizePatternByTheory :: Language f => TheorySpec f -> Pattern f -> Pattern f
canonicalizePatternByTheory spec patternValue =
  case patternValue of
    PatternVar patternVar ->
      PatternVar patternVar
    PatternNode node ->
      PatternNode (canonicalizeLayerByTheory spec (canonicalizePatternByTheory spec <$> node))

canonicalizeCommutativeBinary :: (Foldable f, Ord a) => (a -> a -> f a) -> f a -> f a
canonicalizeCommutativeBinary rebuild node =
  case toList node of
    [left, right] ->
      if right < left
        then rebuild right left
        else rebuild left right
    _ -> node

expandPatternByTheory :: forall f. Language f => TheorySpec f -> Pattern f -> [Pattern f]
expandPatternByTheory spec =
  Set.toList . expandPatternOrbit
  where
    expandPatternOrbit :: Pattern f -> Set (Pattern f)
    expandPatternOrbit patternValue =
      case patternValue of
        PatternVar patternVar ->
          Set.singleton (PatternVar patternVar)
        PatternNode node ->
          Set.unions (localPatternNodeOrbit <$> traverse (Set.toList . expandPatternOrbit) node)

    localPatternNodeOrbit :: f (Pattern f) -> Set (Pattern f)
    localPatternNodeOrbit node =
      Set.map PatternNode (commutedNodeOrbit (tsClassify spec node) node)

    commutedNodeOrbit :: StructuralLaw f (Pattern f) -> f (Pattern f) -> Set (f (Pattern f))
    commutedNodeOrbit (CommutativeBinary rebuild) node =
      case toList node of
        [left, right] ->
          Set.fromList [node, rebuild right left]
        _ -> Set.singleton node
    commutedNodeOrbit Ordinary node =
      Set.singleton node
