{-# LANGUAGE DerivingStrategies #-}

-- | A monadic, name-binding builder for finite lattices that reads as mathematics
-- and compiles to a runtime-validated 'ContextLattice'.
--
-- You declare elements and an order (@a \`below\` b@ for @a ≤ b@); the runner infers
-- the universe and the (unique) top and bottom, then hands the order to
-- 'compileContextLattice', which transitively closes it, /derives/ join and meet, and
-- proves lattice-hood. The builder adds only the ergonomic frontend — every lattice
-- obligation (missing/ambiguous join or meet, antisymmetry, top-greatest,
-- bottom-least) stays with 'compileContextLattice' and surfaces as 'InvalidLattice'.
module Moonlight.FiniteLattice.Presentation
  ( LatticeBuilder,
    ElemRef,
    LatticeBuildError (..),
    LatticeBuilderPatternFailure (..),
    element,
    elements,
    below,
    belowAll,
    latticeOf,
    boundedLatticeOf,
  )
where

import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.FiniteLattice.Core
  ( ContextLattice,
    ContextLatticeCompileError,
    compileContextLattice,
    contextOrderDecl,
  )

-- | An opaque reference to a declared element, carrying its value. Obtained from
-- 'element'; never constructed directly, so an order edge can only mention elements
-- the presentation declared.
type ElemRef :: Type -> Type
newtype ElemRef c = ElemRef c
  deriving stock (Eq, Show)

type LatticeBuildError :: Type -> Type
data LatticeBuildError c
  = DuplicateElement c
  | EmptyLattice
  | NoTop
  | AmbiguousTop [c]
  | NoBottom
  | AmbiguousBottom [c]
  | BuilderPatternFailure LatticeBuilderPatternFailure
  | InvalidLattice (ContextLatticeCompileError c)
  deriving stock (Eq, Show)

-- | Refutable do-pattern failure emitted by the 'MonadFail' instance.
type LatticeBuilderPatternFailure :: Type
newtype LatticeBuilderPatternFailure = LatticeBuilderPatternFailure
  { lbpfMessage :: String
  }
  deriving stock (Eq, Show)

type DeclarationList :: Type -> Type
type DeclarationList value = [value] -> [value]

type BuilderState :: Type -> Type
data BuilderState c = BuilderState
  { bsElements :: !(DeclarationList c),
    bsElementCount :: !Int,
    bsElementSet :: !(Set c),
    bsEdges :: !(DeclarationList (c, c)),
    bsStrictSources :: !(Set c),
    bsStrictTargets :: !(Set c),
    bsTrackBounds :: !Bool,
    bsErrors :: !(DeclarationList (LatticeBuildError c))
  }


emptyDeclarationList :: DeclarationList value
emptyDeclarationList =
  id

appendDeclaredValues :: DeclarationList value -> [value] -> DeclarationList value
appendDeclaredValues declaredValuesToDate newValues =
  declaredValuesToDate . (newValues <>)

declaredValues :: DeclarationList value -> [value]
declaredValues values =
  values []

initialState :: BuilderState c
initialState =
  BuilderState
    { bsElements = emptyDeclarationList,
      bsElementCount = 0,
      bsElementSet = Set.empty,
      bsEdges = emptyDeclarationList,
      bsStrictSources = Set.empty,
      bsStrictTargets = Set.empty,
      bsTrackBounds = True,
      bsErrors = emptyDeclarationList
    }

-- | A pure lattice-presentation builder. The 'Maybe' lets refutable @do@-patterns
-- (e.g. @[a, b, c] <- elements [..]@) short-circuit via 'fail' while preserving the
-- declarations accumulated so far for error reporting.
type LatticeBuilder :: Type -> Type -> Type
newtype LatticeBuilder c a = LatticeBuilder
  {unLatticeBuilder :: BuilderState c -> (Maybe a, BuilderState c)}

instance Functor (LatticeBuilder c) where
  fmap f (LatticeBuilder run) =
    LatticeBuilder (\state -> let (result, state') = run state in (fmap f result, state'))

instance Applicative (LatticeBuilder c) where
  pure value = LatticeBuilder (\state -> (Just value, state))
  LatticeBuilder runF <*> LatticeBuilder runA =
    LatticeBuilder
      ( \state ->
          case runF state of
            (Nothing, state') -> (Nothing, state')
            (Just f, state') ->
              let (result, state'') = runA state'
               in (fmap f result, state'')
      )

instance Monad (LatticeBuilder c) where
  LatticeBuilder run >>= k =
    LatticeBuilder
      ( \state ->
          case run state of
            (Nothing, state') -> (Nothing, state')
            (Just value, state') -> unLatticeBuilder (k value) state'
      )

instance MonadFail (LatticeBuilder c) where
  fail message =
    LatticeBuilder (\state -> (Nothing, recordError (BuilderPatternFailure (LatticeBuilderPatternFailure message)) state))

recordError :: LatticeBuildError c -> BuilderState c -> BuilderState c
recordError buildError state =
  state {bsErrors = appendDeclaredValues (bsErrors state) [buildError]}

element :: Ord c => c -> LatticeBuilder c (ElemRef c)
element value =
  LatticeBuilder
    ( \state ->
        if Set.member value (bsElementSet state)
          then (Just (ElemRef value), recordError (DuplicateElement value) state)
          else
            ( Just (ElemRef value),
              recordNewElement value state
            )
    )

elements :: Ord c => [c] -> LatticeBuilder c [ElemRef c]
elements values =
  LatticeBuilder
    ( \state ->
        let valueCount = length values
            valueSet = elementValueSet values
         in if Set.size valueSet == valueCount && elementBatchDisjoint state valueSet
              then
                ( Just (coerce values),
                  recordNewElements values valueCount valueSet state
                )
              else
                let batch = classifyElementBatch (bsElementSet state) values
                    newValues = reverse (ebNewValues batch)
                 in ( Just (coerce values),
                      state
                        { bsElements = appendElementDeclarations state newValues,
                          bsElementCount = bsElementCount state + ebNewCount batch,
                          bsElementSet = ebSeen batch,
                          bsErrors = appendDeclaredValues (bsErrors state) (fmap DuplicateElement (reverse (ebDuplicateValues batch)))
                        }
                    )
    )

-- | Declare that the first element is below the second in the order (@a ≤ b@).
below :: Ord c => ElemRef c -> ElemRef c -> LatticeBuilder c ()
below (ElemRef lowerValue) (ElemRef upperValue) =
  LatticeBuilder
    ( \state ->
        ( Just (),
          state
            { bsEdges = appendDeclaredValues (bsEdges state) [(lowerValue, upperValue)],
              bsStrictSources =
                if not (bsTrackBounds state) || lowerValue == upperValue
                  then bsStrictSources state
                  else Set.insert lowerValue (bsStrictSources state),
              bsStrictTargets =
                if not (bsTrackBounds state) || lowerValue == upperValue
                  then bsStrictTargets state
                  else Set.insert upperValue (bsStrictTargets state)
            }
        )
    )

belowAll :: Ord c => [(ElemRef c, ElemRef c)] -> LatticeBuilder c ()
belowAll edgeRefs =
  LatticeBuilder
    ( \state ->
        let edgeValues = coerce edgeRefs
            stateWithEdges = state {bsEdges = appendDeclaredValues (bsEdges state) edgeValues}
         in ( Just (),
              if bsTrackBounds state
                then
                  stateWithEdges
                    { bsStrictSources = Set.union (strictSources edgeValues) (bsStrictSources state),
                      bsStrictTargets = Set.union (strictTargets edgeValues) (bsStrictTargets state)
                    }
                else stateWithEdges
            )
    )

-- | Run a presentation, inferring the universe and the unique top and bottom, then
-- compiling and proving lattice-hood, or returning the first fault.
latticeOf :: Ord c => LatticeBuilder c a -> Either (LatticeBuildError c) (ContextLattice c)
latticeOf builder =
  let (_, state) = unLatticeBuilder builder initialState
   in case declaredValues (bsErrors state) of
        (firstError : _) -> Left firstError
        [] -> compilePresentation state

-- | Run a presentation with declared top and bottom, skipping top/bottom
-- inference while preserving the same compile-time lattice proof.
boundedLatticeOf :: Ord c => c -> c -> LatticeBuilder c a -> Either (LatticeBuildError c) (ContextLattice c)
boundedLatticeOf topValue bottomValue builder =
  let (_, state) = unLatticeBuilder builder (initialState {bsTrackBounds = False})
   in case declaredValues (bsErrors state) of
        (firstError : _) -> Left firstError
        [] -> compileBoundedPresentation topValue bottomValue state

compilePresentation :: Ord c => BuilderState c -> Either (LatticeBuildError c) (ContextLattice c)
compilePresentation state =
  case declaredValues (bsElements state) of
    [] -> Left EmptyLattice
    universeList ->
      do
        let edgeList = declaredValues (bsEdges state)
        topValue <- inferTop universeList (bsStrictSources state)
        bottomValue <- inferBottom universeList (bsStrictTargets state)
        first
          InvalidLattice
          (compileContextLattice (bsElementSet state) (contextOrderDecl topValue bottomValue edgeList))

compileBoundedPresentation :: Ord c => c -> c -> BuilderState c -> Either (LatticeBuildError c) (ContextLattice c)
compileBoundedPresentation topValue bottomValue state =
  if bsElementCount state == 0
    then Left EmptyLattice
    else
      first
        InvalidLattice
        (compileContextLattice (bsElementSet state) (contextOrderDecl topValue bottomValue (declaredValues (bsEdges state))))

-- | The unique maximal element (one with no strictly-outgoing edge). In a finite
-- poset a unique maximal element is the greatest, so this is the top.
inferTop :: Ord c => [c] -> Set c -> Either (LatticeBuildError c) c
inferTop universeList strictSourceSet =
  case filter (`Set.notMember` strictSourceSet) universeList of
    [topValue] -> Right topValue
    [] -> Left NoTop
    candidates -> Left (AmbiguousTop candidates)

-- | The unique minimal element (one with no strictly-incoming edge).
inferBottom :: Ord c => [c] -> Set c -> Either (LatticeBuildError c) c
inferBottom universeList strictTargetSet =
  case filter (`Set.notMember` strictTargetSet) universeList of
    [bottomValue] -> Right bottomValue
    [] -> Left NoBottom
    candidates -> Left (AmbiguousBottom candidates)

type ElementBatch :: Type -> Type
data ElementBatch c = ElementBatch
  { ebSeen :: !(Set c),
    ebNewCount :: !Int,
    ebNewValues :: ![c],
    ebDuplicateValues :: ![c]
  }

classifyElementBatch :: Ord c => Set c -> [c] -> ElementBatch c
classifyElementBatch initialSeen =
  List.foldl' classifyElement (ElementBatch initialSeen 0 [] [])

classifyElement :: Ord c => ElementBatch c -> c -> ElementBatch c
classifyElement batch value
  | Set.member value (ebSeen batch) =
      batch {ebDuplicateValues = value : ebDuplicateValues batch}
  | otherwise =
      batch
        { ebSeen = Set.insert value (ebSeen batch),
          ebNewCount = ebNewCount batch + 1,
          ebNewValues = value : ebNewValues batch
        }

recordNewElement :: Ord c => c -> BuilderState c -> BuilderState c
recordNewElement value state =
  state
    { bsElements =
        if bsTrackBounds state
          then appendDeclaredValues (bsElements state) [value]
          else bsElements state,
      bsElementCount = bsElementCount state + 1,
      bsElementSet = Set.insert value (bsElementSet state)
    }

recordNewElements :: Ord c => [c] -> Int -> Set c -> BuilderState c -> BuilderState c
recordNewElements values valueCount valueSet state =
  state
    { bsElements = appendElementDeclarations state values,
      bsElementCount = bsElementCount state + valueCount,
      bsElementSet =
        if Set.null (bsElementSet state)
          then valueSet
          else Set.union valueSet (bsElementSet state)
    }

elementBatchDisjoint :: Ord c => BuilderState c -> Set c -> Bool
elementBatchDisjoint state valueSet =
  Set.null (bsElementSet state) || Set.null (Set.intersection (bsElementSet state) valueSet)

appendElementDeclarations :: BuilderState c -> [c] -> DeclarationList c
appendElementDeclarations state values =
  if bsTrackBounds state
    then appendDeclaredValues (bsElements state) values
    else bsElements state

elementValueSet :: Ord c => [c] -> Set c
elementValueSet values
  | strictlyAscending values = Set.fromDistinctAscList values
  | otherwise = Set.fromList values

strictlyAscending :: Ord c => [c] -> Bool
strictlyAscending values =
  and [leftValue < rightValue | (leftValue, rightValue) <- zip values (drop 1 values)]

strictSources :: Ord c => [(c, c)] -> Set c
strictSources edgeValues =
  Set.fromList [lowerValue | (lowerValue, upperValue) <- edgeValues, lowerValue /= upperValue]

strictTargets :: Ord c => [(c, c)] -> Set c
strictTargets edgeValues =
  Set.fromList [upperValue | (lowerValue, upperValue) <- edgeValues, lowerValue /= upperValue]
