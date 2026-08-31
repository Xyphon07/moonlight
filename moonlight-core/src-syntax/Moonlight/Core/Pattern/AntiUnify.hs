-- | Least-general-generalization runtime for concrete terms.
-- It owns binary and n-ary anti-unification, preserving shared constructor
-- structure and abstracting disagreements into fresh pattern variables with stored witnesses.
module Moonlight.Core.Pattern.AntiUnify
  ( BinaryLGGResult (..),
    NaryLGGResult (..),
    antiUnifyTerms,
    antiUnifyWithTermStore,
    antiUnifyAllTerms,
    antiUnifyAllWithTermStore,
  )
where

import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Data.Fix (Fix (..))
import Data.Foldable (foldlM)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Core.Identifier.EGraph
  ( PatternVar,
    mkPatternVar,
    patternVarKey,
  )
import Moonlight.Core.Language (ZipMatch (..))
import Moonlight.Core.Pattern (Pattern (..))
import Prelude

type BinaryLGGResult :: (Type -> Type) -> Type -> Type
data BinaryLGGResult f binding = BinaryLGGResult
  { binaryLggPattern :: !(Pattern f),
    binaryLggLeftBindings :: !(IntMap binding),
    binaryLggRightBindings :: !(IntMap binding),
    binaryLggSharedStructure :: !Int
  }

deriving stock instance (Eq (Pattern f), Eq binding) => Eq (BinaryLGGResult f binding)
deriving stock instance (Show (Pattern f), Show binding) => Show (BinaryLGGResult f binding)

type NaryLGGResult :: (Type -> Type) -> Type -> Type
data NaryLGGResult f binding = NaryLGGResult
  { naryLggPattern :: !(Pattern f),
    naryLggBindings :: !(NonEmpty (IntMap binding)),
    naryLggSharedStructure :: !Int
  }

deriving stock instance (Eq (Pattern f), Eq binding) => Eq (NaryLGGResult f binding)
deriving stock instance (Show (Pattern f), Show binding) => Show (NaryLGGResult f binding)

type BinaryAntiUnifyState :: Type -> Type -> Type
data BinaryAntiUnifyState store binding = BinaryAntiUnifyState
  { bausNextVar :: !PatternVar,
    bausStore :: !store,
    bausLeftBindings :: !(IntMap binding),
    bausRightBindings :: !(IntMap binding),
    bausSharedStructure :: !Int
  }

type NaryAntiUnifyState :: Type -> Type -> Type
data NaryAntiUnifyState store binding = NaryAntiUnifyState
  { nautsNextVar :: !PatternVar,
    nautsStore :: !store,
    nautsBindings :: !(NonEmpty (IntMap binding)),
    nautsSharedStructure :: !Int
  }

antiUnifyAllTerms :: ZipMatch f => NonEmpty (Fix f) -> NaryLGGResult f (Fix f)
antiUnifyAllTerms terms =
  fst
    ( runIdentity
        (antiUnifyAllWithTermStore (\termValue storeValue -> Identity (termValue, storeValue)) () terms)
    )

antiUnifyAllWithTermStore ::
  (ZipMatch f, Monad effect) =>
  (Fix f -> store -> effect (binding, store)) ->
  store ->
  NonEmpty (Fix f) ->
  effect (NaryLGGResult f binding, store)
antiUnifyAllWithTermStore insertTerm initialStore terms = do
  let initialState =
        NaryAntiUnifyState
          { nautsNextVar = mkPatternVar 0,
            nautsStore = initialStore,
            nautsBindings = fmap (const IntMap.empty) terms,
            nautsSharedStructure = 0
          }
  (patternValue, finalState) <- antiUnifyAllRecursive insertTerm terms initialState
  pure (lggFromNaryState patternValue finalState, nautsStore finalState)

antiUnifyAllRecursive ::
  (ZipMatch f, Monad effect) =>
  (Fix f -> store -> effect (binding, store)) ->
  NonEmpty (Fix f) ->
  NaryAntiUnifyState store binding ->
  effect (Pattern f, NaryAntiUnifyState store binding)
antiUnifyAllRecursive insertTerm terms state =
  case alignedChildLayer terms of
    Just childLayer -> do
      (childPatterns, childState) <-
        runStateT
          (traverse (StateT . antiUnifyAllRecursive insertTerm) childLayer)
          state
      pure (PatternNode childPatterns, childState {nautsSharedStructure = nautsSharedStructure childState + 1})
    Nothing ->
      freshNaryVariable insertTerm terms state

alignedChildLayer :: ZipMatch f => NonEmpty (Fix f) -> Maybe (f (NonEmpty (Fix f)))
alignedChildLayer terms =
  case NonEmpty.reverse terms of
    Fix lastLayer :| reversedLeadingTerms ->
      foldlM
        prependAlignedChildren
        (fmap NonEmpty.singleton lastLayer)
        reversedLeadingTerms

prependAlignedChildren :: ZipMatch f => f (NonEmpty (Fix f)) -> Fix f -> Maybe (f (NonEmpty (Fix f)))
prependAlignedChildren accumulatedRows (Fix nextLayer) =
  fmap
    (fmap prependAlignedChild)
    (zipMatch nextLayer accumulatedRows)

prependAlignedChild :: (child, NonEmpty child) -> NonEmpty child
prependAlignedChild (nextChild, accumulatedRow) =
  NonEmpty.cons nextChild accumulatedRow

freshNaryVariable ::
  Monad effect =>
  (Fix f -> store -> effect (binding, store)) ->
  NonEmpty (Fix f) ->
  NaryAntiUnifyState store binding ->
  effect (Pattern f, NaryAntiUnifyState store binding)
freshNaryVariable insertTerm terms state = do
  let patternVar = nautsNextVar state
  (bindings, storeAfterTerms) <-
    runStateT
      (traverse (StateT . insertTerm) terms)
      (nautsStore state)
  let
      nextState =
        state
          { nautsNextVar = succ patternVar,
            nautsStore = storeAfterTerms,
            nautsBindings =
              NonEmpty.zipWith
                (IntMap.insert (patternVarKey patternVar))
                bindings
                (nautsBindings state)
          }
  pure (PatternVar patternVar, nextState)

lggFromNaryState :: Pattern f -> NaryAntiUnifyState store binding -> NaryLGGResult f binding
lggFromNaryState patternValue state =
  NaryLGGResult
    { naryLggPattern = patternValue,
      naryLggBindings = nautsBindings state,
      naryLggSharedStructure = nautsSharedStructure state
    }

antiUnifyTerms :: ZipMatch f => Fix f -> Fix f -> BinaryLGGResult f (Fix f)
antiUnifyTerms leftTerm rightTerm =
  fst
    ( runIdentity
        (antiUnifyWithTermStore (\termValue storeValue -> Identity (termValue, storeValue)) () leftTerm rightTerm)
    )

antiUnifyWithTermStore ::
  (ZipMatch f, Monad effect) =>
  (Fix f -> store -> effect (binding, store)) ->
  store ->
  Fix f ->
  Fix f ->
  effect (BinaryLGGResult f binding, store)
antiUnifyWithTermStore insertTerm initialStore leftTerm rightTerm = do
  let initialState =
        BinaryAntiUnifyState
          { bausNextVar = mkPatternVar 0,
            bausStore = initialStore,
            bausLeftBindings = IntMap.empty,
            bausRightBindings = IntMap.empty,
            bausSharedStructure = 0
          }
  (patternValue, finalState) <- antiUnifyRecursive insertTerm leftTerm rightTerm initialState
  pure (lggFromState patternValue finalState, bausStore finalState)

antiUnifyRecursive ::
  (ZipMatch f, Monad effect) =>
  (Fix f -> store -> effect (binding, store)) ->
  Fix f ->
  Fix f ->
  BinaryAntiUnifyState store binding ->
  effect (Pattern f, BinaryAntiUnifyState store binding)
antiUnifyRecursive insertTerm leftTerm@(Fix leftLayer) rightTerm@(Fix rightLayer) state =
  case zipMatch leftLayer rightLayer of
    Just matchedChildren -> do
      (childPatterns, childState) <-
        runStateT
          ( traverse
              (\(leftChild, rightChild) -> StateT (antiUnifyRecursive insertTerm leftChild rightChild))
              matchedChildren
          )
          state
      pure (PatternNode childPatterns, childState {bausSharedStructure = bausSharedStructure childState + 1})
    Nothing ->
      freshVariable insertTerm leftTerm rightTerm state

freshVariable ::
  Monad effect =>
  (Fix f -> store -> effect (binding, store)) ->
  Fix f ->
  Fix f ->
  BinaryAntiUnifyState store binding ->
  effect (Pattern f, BinaryAntiUnifyState store binding)
freshVariable insertTerm leftTerm rightTerm state = do
  let patternVar = bausNextVar state
      patternKey = patternVarKey patternVar
  (leftBinding, storeAfterLeft) <- insertTerm leftTerm (bausStore state)
  (rightBinding, storeAfterRight) <- insertTerm rightTerm storeAfterLeft
  let
      nextState =
        state
          { bausNextVar = succ patternVar,
            bausStore = storeAfterRight,
            bausLeftBindings = IntMap.insert patternKey leftBinding (bausLeftBindings state),
            bausRightBindings = IntMap.insert patternKey rightBinding (bausRightBindings state)
          }
  pure (PatternVar patternVar, nextState)

lggFromState :: Pattern f -> BinaryAntiUnifyState store binding -> BinaryLGGResult f binding
lggFromState patternValue state =
  BinaryLGGResult
    { binaryLggPattern = patternValue,
      binaryLggLeftBindings = bausLeftBindings state,
      binaryLggRightBindings = bausRightBindings state,
      binaryLggSharedStructure = bausSharedStructure state
    }
