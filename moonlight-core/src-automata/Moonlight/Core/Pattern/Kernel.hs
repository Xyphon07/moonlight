{-# LANGUAGE LambdaCase #-}

-- | Compilation of authored patterns into intersectable bottom-up kernels consumed by the pattern automata.
module Moonlight.Core.Pattern.Kernel
  ( PatternKernelChildren (..),
    PatternKernelStateSpec (..),
    CompiledPatternKernel (..),
    compilePatternKernel,
    intersectCompiledPatternKernel,
    kernelStateChildren,
  )
where

import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Set qualified as Set
import Data.These (These (..))
import Moonlight.Core.Identifier.EGraph (PatternVar)
import Moonlight.Core.Language (ZipMatch (..))
import Moonlight.Core.Pattern (Pattern (..))
import Prelude

type PatternKernelChildren :: (Type -> Type) -> Type -> Type
data PatternKernelChildren f state
  = KernelMatchAny
  | KernelMatchNode (f state)
  | KernelMatchImpossible

type PatternKernelStateSpec :: (Type -> Type) -> Type -> Type
newtype PatternKernelStateSpec f state = PatternKernelStateSpec
  { runPatternKernelStateSpec :: ([PatternVar], PatternKernelChildren f state)
  }

patternKernelBindings :: PatternKernelStateSpec f state -> [PatternVar]
patternKernelBindings = fst . runPatternKernelStateSpec

patternKernelChildren :: PatternKernelStateSpec f state -> PatternKernelChildren f state
patternKernelChildren = snd . runPatternKernelStateSpec

type CompiledPatternKernel :: (Type -> Type) -> Type -> Type
data CompiledPatternKernel f state = CompiledPatternKernel
  { cpkRootState :: state,
    cpkOrderedStates :: [state],
    cpkStateSpec :: state -> PatternKernelStateSpec f state
  }

type PatternNodeId :: Type
newtype PatternNodeId = PatternNodeId
  { patternNodeIdKey :: Int
  }
  deriving stock (Eq, Ord, Show)

type FlatPatternNode :: (Type -> Type) -> Type
data FlatPatternNode f
  = FlatPatternVar PatternVar
  | FlatPatternNode (f PatternNodeId)

type PatternNodeBuilder :: (Type -> Type) -> Type
data PatternNodeBuilder f = PatternNodeBuilder
  { pnbNextKey :: Int,
    pnbNodes :: IntMap (FlatPatternNode f)
  }

emptyPatternNodeBuilder :: PatternNodeBuilder f
emptyPatternNodeBuilder =
  PatternNodeBuilder
    { pnbNextKey = 0,
      pnbNodes = IntMap.empty
    }

compilePatternKernel :: Traversable f => Pattern f -> CompiledPatternKernel f PatternNodeId
compilePatternKernel patternValue =
  let (rootNodeId, finalBuilder) =
        runState
          (compilePatternNode patternValue)
          emptyPatternNodeBuilder
      compiledNodesById = pnbNodes finalBuilder
   in CompiledPatternKernel
        { cpkRootState = rootNodeId,
          cpkOrderedStates = fmap PatternNodeId (IntMap.keys compiledNodesById),
          cpkStateSpec =
            \patternNodeId ->
              maybe
                (PatternKernelStateSpec ([], KernelMatchImpossible))
                compiledNodeKernelSpec
                (IntMap.lookup (patternNodeIdKey patternNodeId) compiledNodesById)
        }

compilePatternNode :: Traversable f => Pattern f -> State (PatternNodeBuilder f) PatternNodeId
compilePatternNode patternValue =
  case patternValue of
    PatternVar patternVar ->
      registerPatternNode (FlatPatternVar patternVar)
    PatternNode patternNode ->
      traverse compilePatternNode patternNode >>= registerPatternNode . FlatPatternNode

registerPatternNode :: FlatPatternNode f -> State (PatternNodeBuilder f) PatternNodeId
registerPatternNode flatPatternNode =
  state
    ( \builder ->
        let nextNodeId = PatternNodeId (pnbNextKey builder)
         in ( nextNodeId,
              builder
                { pnbNextKey = pnbNextKey builder + 1,
                  pnbNodes = IntMap.insert (patternNodeIdKey nextNodeId) flatPatternNode (pnbNodes builder)
                }
            )
    )

compiledNodeKernelSpec :: FlatPatternNode f -> PatternKernelStateSpec f PatternNodeId
compiledNodeKernelSpec compiledNode =
  case compiledNode of
    FlatPatternVar patternVar ->
      PatternKernelStateSpec ([patternVar], KernelMatchAny)
    FlatPatternNode patternNode ->
      PatternKernelStateSpec ([], KernelMatchNode patternNode)

intersectCompiledPatternKernel :: (ZipMatch f, Ord leftState, Ord rightState) => CompiledPatternKernel f leftState -> CompiledPatternKernel f rightState -> CompiledPatternKernel f (These leftState rightState)
intersectCompiledPatternKernel leftKernel rightKernel =
  CompiledPatternKernel
    { cpkRootState = rootState,
      cpkOrderedStates = reachableOrderedStates describeProductState rootState,
      cpkStateSpec = describeProductState
    }
  where
    rootState = These (cpkRootState leftKernel) (cpkRootState rightKernel)
    describeProductState productState =
      case productState of
        This leftState ->
          combineStateSpecs
            (cpkStateSpec leftKernel leftState)
            wildcardPatternKernelStateSpec
        That rightState ->
          combineStateSpecs
            wildcardPatternKernelStateSpec
            (cpkStateSpec rightKernel rightState)
        These leftState rightState ->
          combineStateSpecs
            (cpkStateSpec leftKernel leftState)
            (cpkStateSpec rightKernel rightState)

wildcardPatternKernelStateSpec :: PatternKernelStateSpec f state
wildcardPatternKernelStateSpec = PatternKernelStateSpec ([], KernelMatchAny)

combineStateSpecs :: ZipMatch f => PatternKernelStateSpec f leftState -> PatternKernelStateSpec f rightState -> PatternKernelStateSpec f (These leftState rightState)
combineStateSpecs leftSpec rightSpec =
  PatternKernelStateSpec
    ( patternKernelBindings leftSpec <> patternKernelBindings rightSpec,
      combineChildStates (patternKernelChildren leftSpec) (patternKernelChildren rightSpec)
    )

combineChildStates :: ZipMatch f => PatternKernelChildren f leftState -> PatternKernelChildren f rightState -> PatternKernelChildren f (These leftState rightState)
combineChildStates leftChildren rightChildren =
  case (leftChildren, rightChildren) of
    (KernelMatchImpossible, _) -> KernelMatchImpossible
    (_, KernelMatchImpossible) -> KernelMatchImpossible
    (KernelMatchAny, KernelMatchAny) -> KernelMatchAny
    (KernelMatchNode leftNode, KernelMatchAny) ->
      KernelMatchNode (fmap This leftNode)
    (KernelMatchAny, KernelMatchNode rightNode) ->
      KernelMatchNode (fmap That rightNode)
    (KernelMatchNode leftNode, KernelMatchNode rightNode) ->
      maybe KernelMatchImpossible KernelMatchNode (zipMatchedChildStates leftNode rightNode)

reachableOrderedStates :: (Ord state, Foldable f) => (state -> PatternKernelStateSpec f state) -> state -> [state]
reachableOrderedStates describeState rootState =
  reverse (snd (visitState mempty [] rootState))
  where
    visitState visited orderedStates currentState
      | Set.member currentState visited = (visited, orderedStates)
      | otherwise =
          let visitedWithState = Set.insert currentState visited
              (visitedAfterChildren, orderedStatesAfterChildren) =
                foldl'
                  (\(childVisited, childOrderedStates) childState -> visitState childVisited childOrderedStates childState)
                  (visitedWithState, orderedStates)
                  (kernelStateChildren (patternKernelChildren (describeState currentState)))
           in (visitedAfterChildren, currentState : orderedStatesAfterChildren)

kernelStateChildren :: Foldable f => PatternKernelChildren f state -> [state]
kernelStateChildren kernelChildren =
  case kernelChildren of
    KernelMatchAny -> []
    KernelMatchNode childStates -> toList childStates
    KernelMatchImpossible -> []

zipMatchedChildStates ::
  ZipMatch f =>
  f leftState ->
  f rightState ->
  Maybe (f (These leftState rightState))
zipMatchedChildStates leftChildren rightChildren =
  traverse matchedChildState
    =<< zipMatch (fmap This leftChildren) (fmap That rightChildren)
  where
    matchedChildState ::
      (These leftState rightState, These leftState rightState) ->
      Maybe (These leftState rightState)
    matchedChildState =
      \case
        (This leftState, That rightState) ->
          Just (These leftState rightState)
        _ ->
          Nothing
