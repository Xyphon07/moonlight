{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Composable chains of morphisms (and the 'FiniteComposableCategory' class) for
-- enumerating a finite category by chain dimension.
module Moonlight.Category.Pure.FiniteComposable
  ( ComposableChain,
    chainStartObject,
    chainVertices,
    chainMorphisms,
    ComposableChainError (..),
    SizedComposableChain,
    sizedChainDimension,
    sizedChainValue,
    chainDimension,
    chainTerminalObject,
    singletonComposableChain,
    mkComposableChain,
    sizedComposableChain,
    appendComposableMorphism,
    chainsOfDimension,
    FiniteComposableCategory (..),
  )
where

import Data.Bifunctor (first)
import Control.Monad (foldM)
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.List (genericTake)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, mapMaybe)
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Category (Category (..))

type ComposableChain :: Type -> Type
-- | A path whose adjacent morphism endpoints have been validated.  Every stored
-- step carries the target established while appending it, so vertex projection
-- is total and never has to query the category again.  The terminal object is
-- an O(1) view of the newest step; the strict finite step count is an opaque
-- cache with the same representable domain as the previous sequence-length
-- view.
data ComposableChain c = ComposableChain
  { chainStartObjectInternal :: Ob c,
    chainStepCountInternal :: !Int,
    chainValidatedStepsNewestFirst :: [(Mor c, Ob c)]
  }

type SizedComposableChain :: Type -> Type
-- | A validated path paired with its morphism count.
data SizedComposableChain c = SizedComposableChain
  { -- | The number of morphisms in the path.
    sizedChainDimension :: Natural,
    -- | The underlying validated path.
    sizedChainValue :: ComposableChain c
  }

type ComposableChainError :: Type -> Type
-- | A category failure or an endpoint incompatibility encountered while
-- extending a path.
data ComposableChainError c
  = ComposableChainCategoryError (CategoryError c)
  | ComposableChainEndpointMismatch (Ob c) (Ob c)

-- | Count the morphisms in a validated path.
chainDimension :: ComposableChain c -> Natural
chainDimension = fromIntegral . chainStepCountInternal
{-# INLINE chainDimension #-}

-- | The first vertex of a validated path.
chainStartObject :: ComposableChain c -> Ob c
chainStartObject = chainStartObjectInternal
{-# INLINE chainStartObject #-}

-- | The terminal vertex of a validated path.
chainTerminalObject :: ComposableChain c -> Ob c
chainTerminalObject chainValue =
  case chainValidatedStepsNewestFirst chainValue of
    [] -> chainStartObjectInternal chainValue
    (_, terminalObject) : _ -> terminalObject
{-# INLINE chainTerminalObject #-}

-- | The nonempty vertex sequence established by chain construction, in path
-- order.  Its final vertex is 'chainTerminalObject'.
chainVertices :: ComposableChain c -> NonEmpty (Ob c)
chainVertices chainValue =
  chainStartObjectInternal chainValue
    :| foldl'
      (\targets (_, targetObject) -> targetObject : targets)
      []
      (chainValidatedStepsNewestFirst chainValue)

-- | Project the morphisms in composition order.
chainMorphisms :: ComposableChain c -> [Mor c]
chainMorphisms chainValue =
  foldl'
    (\morphisms (morphism, _) -> morphism : morphisms)
    []
    (chainValidatedStepsNewestFirst chainValue)

-- | The dimension-zero path at an object.
singletonComposableChain :: Ob c -> ComposableChain c
singletonComposableChain objectValue =
  ComposableChain objectValue 0 []

-- | Validate a morphism sequence from a declared start object.
mkComposableChain :: (Category c, Eq (Ob c)) => c -> Ob c -> [Mor c] -> Either (ComposableChainError c) (ComposableChain c)
mkComposableChain categoryValue startObject =
  foldM (appendComposableMorphism categoryValue) (singletonComposableChain startObject)

-- | Attach the derived dimension to a validated path.
sizedComposableChain :: ComposableChain c -> SizedComposableChain c
sizedComposableChain chainValue =
  SizedComposableChain
    { sizedChainDimension = chainDimension chainValue,
      sizedChainValue = chainValue
    }

-- | Extend a path when the new morphism starts at its terminal object.
appendComposableMorphism :: (Category c, Eq (Ob c)) => c -> ComposableChain c -> Mor c -> Either (ComposableChainError c) (ComposableChain c)
appendComposableMorphism categoryValue chainValue morphism = do
  morphismSource <- first ComposableChainCategoryError (source categoryValue morphism)
  morphismTarget <- first ComposableChainCategoryError (target categoryValue morphism)
  let terminalObject = chainTerminalObject chainValue
  if morphismSource == terminalObject
    then
      Right
        ComposableChain
          { chainStartObjectInternal = chainStartObjectInternal chainValue,
            chainStepCountInternal = chainStepCountInternal chainValue + 1,
            chainValidatedStepsNewestFirst = (morphism, morphismTarget) : chainValidatedStepsNewestFirst chainValue
          }
    else Left (ComposableChainEndpointMismatch terminalObject morphismSource)

-- | Enumerate all composable paths at one exact dimension.
chainsOfDimension :: FiniteComposableCategory c => c -> Natural -> [ComposableChain c]
chainsOfDimension categoryValue dimensionBound =
  fromMaybe [] (indexByNatural dimensionBound (chainsByDimension categoryValue))

indexByNatural :: Natural -> [a] -> Maybe a
indexByNatural indexValue values =
  case (indexValue, values) of
    (_, []) -> Nothing
    (0, value : _) -> Just value
    (_, _ : rest) -> indexByNatural (indexValue - 1) rest

chainsByDimension :: FiniteComposableCategory c => c -> [[ComposableChain c]]
chainsByDimension categoryValue =
  iterate (extendChains categoryValue) (map singletonComposableChain (enumerateObjects categoryValue))

extendChains :: FiniteComposableCategory c => c -> [ComposableChain c] -> [ComposableChain c]
extendChains categoryValue chains =
  chains
    >>= ( \chainValue ->
            mapMaybe
              (either (const Nothing) Just . appendComposableMorphism categoryValue chainValue)
              (enumerateMorphismsFrom categoryValue (chainTerminalObject chainValue))
        )

extendGrowingChainsNonIdentity ::
  (FiniteComposableCategory c, Eq (Mor c)) =>
  c ->
  [ComposableChain c] ->
  [ComposableChain c]
extendGrowingChainsNonIdentity categoryValue chains =
  chains
    >>= ( \chainValue ->
            let terminalObject = chainTerminalObject chainValue
                terminalIdentity =
                  either (const Nothing) Just (identity categoryValue terminalObject)
             in enumerateMorphismsFrom categoryValue terminalObject
                  & mapMaybe
                    ( \morphism -> do
                        if Just morphism == terminalIdentity
                          then Nothing
                          else either (const Nothing) Just (appendComposableMorphism categoryValue chainValue morphism)
                    )
        )

type FiniteComposableCategory :: Type -> Constraint
-- | A finite category whose objects, morphisms, and composable paths can be
-- enumerated.
class (Category c, Eq (Ob c)) => FiniteComposableCategory c where
  enumerateObjects :: c -> [Ob c]
  enumerateMorphisms :: c -> [Mor c]
  enumerateMorphismsFrom :: c -> Ob c -> [Mor c]
  enumerateMorphismsFrom categoryValue sourceObject =
    enumerateMorphisms categoryValue
      & filter
        ( \morphism ->
            case source categoryValue morphism of
              Right morphismSource -> morphismSource == sourceObject
              Left _ -> False
        )

  enumerateComposableChains :: c -> Natural -> [SizedComposableChain c]
  default enumerateComposableChains :: c -> Natural -> [SizedComposableChain c]
  enumerateComposableChains categoryValue dimensionBound =
    genericTake (dimensionBound + 1) (chainsByDimension categoryValue)
      & foldMap (fmap sizedComposableChain)

  enumerateNonDegenerateChainsByDimension :: Eq (Mor c) => c -> Natural -> [[ComposableChain c]]
  enumerateNonDegenerateChainsByDimension categoryValue dimensionBound =
    genericTake
      (dimensionBound + 1)
      ( iterate
          (extendGrowingChainsNonIdentity categoryValue)
          (fmap singletonComposableChain (enumerateObjects categoryValue))
      )
