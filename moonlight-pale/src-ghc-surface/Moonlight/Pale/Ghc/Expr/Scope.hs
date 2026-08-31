module Moonlight.Pale.Ghc.Expr.Scope
  ( ScopeId,
    ScopeIdFailure (..),
    ScopeCtx (..),
    ScopeIndex,
    ScopeIndexFailure (..),
    ScopeLookupFailure (..),
    FreeScopeSummary,
    mkScopeId,
    scopeIdKey,
    rootScopeId,
    mkScopeIndex,
    scopeIndexRoot,
    scopeParentId,
    scopeDepthOf,
    scopeIsAncestorOf,
    scopeComparable,
    scopeLca,
    scopeCtxLeq,
    scopeCtxMeet,
    scopeCtxJoin,
    scopeObservedCount,
    scopeObservedContexts,
    scopeTopCtx,
    scopeBottomCtx,
    binderIntroScope,
    binderSiteScope,
    emptyFreeScopeSummary,
    singletonFreeScopeSummary,
    mergeFreeScopeSummary,
    mergeFreeScopeSummaryBy,
    mergeFreeScopeSummaryByEither,
    deleteFreeScopeSummary,
    freeScopeSummaryContains,
    freeScopeSummarySize,
    freeScopeSummaryToList,
    freeScopeSupportAnchor,
  )
where

import Control.Monad (foldM, when)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Primitive.SmallArray
  ( SmallArray,
    indexSmallArray,
    sizeofSmallArray,
    smallArrayFromList,
  )
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (absurd)
import Moonlight.Core (BinderId (..), binderIdKey)

type ScopeId :: Type
newtype ScopeId = ScopeId Int
  deriving stock (Eq, Ord, Show)

type ScopeIndex :: Type
data ScopeIndex = ScopeIndex
  { siParent :: !(Vector Int),
    siDepth :: !(Vector Int),
    siSubtreeEnd :: !(Vector Int),
    siLift :: !(Vector (Vector Int)),
    siHasBranch :: !Bool,
    siDeepest :: !ScopeId,
    siRoot :: !ScopeId,
    siBinderIntro :: !(Vector ScopeId)
  }
  deriving stock (Eq, Ord, Show)

type FreeScopeSummary :: Type
newtype FreeScopeSummary = FreeScopeSummary (SmallArray ScopeId)

instance Eq FreeScopeSummary where
  FreeScopeSummary leftArray == FreeScopeSummary rightArray =
    scopeArrayToList leftArray == scopeArrayToList rightArray

instance Ord FreeScopeSummary where
  compare (FreeScopeSummary leftArray) (FreeScopeSummary rightArray) =
    compare (scopeArrayToList leftArray) (scopeArrayToList rightArray)

instance Show FreeScopeSummary where
  showsPrec precedence (FreeScopeSummary scopeArray) =
    showParen
      (precedence > 10)
      (showString "FreeScopeSummary " . shows (scopeArrayToList scopeArray))

scopeArrayToList :: SmallArray ScopeId -> [ScopeId]
scopeArrayToList scopeArray =
  fmap (indexSmallArray scopeArray) [0 .. sizeofSmallArray scopeArray - 1]

type ScopeIdFailure :: Type
data ScopeIdFailure
  = NegativeScopeId !Int
  deriving stock (Eq, Ord, Show)

type ScopeCtx :: Type
data ScopeCtx
  = ActualScope !ScopeId
  | IncompatibleScope
  deriving stock (Eq, Ord, Show)

type ScopeLookupFailure :: Type
data ScopeLookupFailure
  = ScopeIdOutsideIndex !ScopeId !Int
  | BinderIdOutsideIndex !BinderId !Int
  | ScopeLiftLevelOutsideIndex !Int !Int
  deriving stock (Eq, Ord, Show)

type ScopeIndexFailure :: Type
data ScopeIndexFailure
  = ScopeIndexEmpty
  | ScopeRootParentInvalid !Int
  | ScopeParentEdgeInvalid !ScopeId !Int
  | ScopeBinderIntroInvalid !BinderId !Int
  | ScopeSubtreeEndMissing !ScopeId
  | ScopeLiftMissing !ScopeId
  | ScopeParentNotPreorder !ScopeId !Int
  deriving stock (Eq, Ord, Show)

mkScopeId :: Int -> Either ScopeIdFailure ScopeId
mkScopeId scopeKey
  | scopeKey < 0 =
      Left (NegativeScopeId scopeKey)
  | otherwise =
      Right (ScopeId scopeKey)

scopeIdKey :: ScopeId -> Int
scopeIdKey (ScopeId scopeKey) =
  scopeKey

rootScopeId :: ScopeId
rootScopeId =
  ScopeId 0

mkScopeIndex :: Vector Int -> Vector Int -> Either ScopeIndexFailure ScopeIndex
mkScopeIndex parentVector binderIntroVector = do
  case V.toList parentVector of
    [] ->
      Left ScopeIndexEmpty
    rootParent : _ ->
      when (rootParent /= 0) (Left (ScopeRootParentInvalid rootParent))
  traverse_ validateParentEdge (zip [1 ..] (drop 1 (V.toList parentVector)))
  traverse_ validateBinderIntro (zip [0 ..] (V.toList binderIntroVector))
  preorder <- buildPreorderIndex parentVector
  liftVector <- buildLift parentVector
  pure
    ScopeIndex
      { siParent = parentVector,
        siDepth = preorderDepth preorder,
        siSubtreeEnd = preorderSubtreeEnd preorder,
        siLift = liftVector,
        siHasBranch = preorderHasBranch preorder,
        siDeepest = preorderDeepest preorder,
        siRoot = rootScopeId,
        siBinderIntro = V.map ScopeId binderIntroVector
      }
  where
    validateParentEdge (scopeKey, parentKey) =
      when (parentKey < 0 || parentKey >= scopeKey) $
        Left (ScopeParentEdgeInvalid (ScopeId scopeKey) parentKey)

    validateBinderIntro (binderKey, introScopeKey) =
      when (introScopeKey < 0 || introScopeKey >= V.length parentVector) $
        Left (ScopeBinderIntroInvalid (BinderId binderKey) introScopeKey)

scopeIndexRoot :: ScopeIndex -> ScopeId
scopeIndexRoot =
  siRoot

scopeParentId :: ScopeIndex -> ScopeId -> Either ScopeLookupFailure ScopeId
scopeParentId scopeIndex scopeId =
  ScopeId <$> scopeVectorValue ScopeIdOutsideIndex scopeId (siParent scopeIndex)

scopeDepthOf :: ScopeIndex -> ScopeId -> Either ScopeLookupFailure Int
scopeDepthOf scopeIndex scopeId =
  scopeVectorValue ScopeIdOutsideIndex scopeId (siDepth scopeIndex)

scopeIsAncestorOf :: ScopeIndex -> ScopeId -> ScopeId -> Either ScopeLookupFailure Bool
scopeIsAncestorOf scopeIndex leftScope rightScope = do
  leftEnd <- scopeVectorValue ScopeIdOutsideIndex leftScope (siSubtreeEnd scopeIndex)
  _ <- scopeVectorValue ScopeIdOutsideIndex rightScope (siSubtreeEnd scopeIndex)
  let leftKey = scopeIdKey leftScope
      rightKey = scopeIdKey rightScope
  pure (leftKey <= rightKey && rightKey < leftEnd)

scopeComparable :: ScopeIndex -> ScopeId -> ScopeId -> Either ScopeLookupFailure Bool
scopeComparable scopeIndex leftScope rightScope =
  (||)
    <$> scopeIsAncestorOf scopeIndex leftScope rightScope
    <*> scopeIsAncestorOf scopeIndex rightScope leftScope

scopeLca :: ScopeIndex -> ScopeId -> ScopeId -> Either ScopeLookupFailure ScopeId
scopeLca scopeIndex leftScope rightScope = do
  leftAncestor <- scopeIsAncestorOf scopeIndex leftScope rightScope
  rightAncestor <- scopeIsAncestorOf scopeIndex rightScope leftScope
  if leftAncestor
    then pure leftScope
    else
      if rightAncestor
        then pure rightScope
        else scopeParentId scopeIndex =<< climb leftScope (V.length (siLift scopeIndex) - 1)
  where
    climb currentScope liftIndex
      | liftIndex < 0 =
          pure currentScope
      | otherwise = do
          ancestorScope <- liftAncestor scopeIndex liftIndex currentScope
          ancestorOfRight <- scopeIsAncestorOf scopeIndex ancestorScope rightScope
          if ancestorOfRight
            then climb currentScope (liftIndex - 1)
            else climb ancestorScope (liftIndex - 1)

scopeCtxLeq :: ScopeIndex -> ScopeCtx -> ScopeCtx -> Either ScopeLookupFailure Bool
scopeCtxLeq _ IncompatibleScope IncompatibleScope =
  Right True
scopeCtxLeq _ IncompatibleScope _ =
  Right False
scopeCtxLeq _ _ IncompatibleScope =
  Right True
scopeCtxLeq scopeIndex (ActualScope leftScope) (ActualScope rightScope) =
  scopeIsAncestorOf scopeIndex leftScope rightScope

scopeCtxMeet :: ScopeIndex -> ScopeCtx -> ScopeCtx -> Either ScopeLookupFailure ScopeCtx
scopeCtxMeet _ IncompatibleScope rightCtx =
  Right rightCtx
scopeCtxMeet _ leftCtx IncompatibleScope =
  Right leftCtx
scopeCtxMeet scopeIndex (ActualScope leftScope) (ActualScope rightScope) =
  ActualScope <$> scopeLca scopeIndex leftScope rightScope

scopeCtxJoin :: ScopeIndex -> ScopeCtx -> ScopeCtx -> Either ScopeLookupFailure ScopeCtx
scopeCtxJoin _ IncompatibleScope _ =
  Right IncompatibleScope
scopeCtxJoin _ _ IncompatibleScope =
  Right IncompatibleScope
scopeCtxJoin scopeIndex (ActualScope leftScope) (ActualScope rightScope) = do
  leftAncestor <- scopeIsAncestorOf scopeIndex leftScope rightScope
  rightAncestor <- scopeIsAncestorOf scopeIndex rightScope leftScope
  pure $
    if leftAncestor
      then ActualScope rightScope
      else
        if rightAncestor
          then ActualScope leftScope
          else IncompatibleScope

scopeObservedCount :: ScopeIndex -> Int
scopeObservedCount =
  V.length . siParent

scopeObservedContexts :: ScopeIndex -> Either ScopeLookupFailure [ScopeCtx]
scopeObservedContexts scopeIndex =
  Right
    ( fmap (ActualScope . ScopeId) [0 .. V.length (siParent scopeIndex) - 1]
        <> [IncompatibleScope | siHasBranch scopeIndex]
    )

scopeTopCtx :: ScopeIndex -> Either ScopeLookupFailure ScopeCtx
scopeTopCtx scopeIndex =
  Right
    ( if siHasBranch scopeIndex
        then IncompatibleScope
        else ActualScope (siDeepest scopeIndex)
    )

scopeBottomCtx :: ScopeIndex -> ScopeCtx
scopeBottomCtx =
  ActualScope . siRoot

binderIntroScope :: ScopeIndex -> BinderId -> Either ScopeLookupFailure ScopeId
binderIntroScope scopeIndex binderId =
  binderVectorValue binderId (siBinderIntro scopeIndex)

binderSiteScope :: ScopeIndex -> BinderId -> Either ScopeLookupFailure ScopeId
binderSiteScope scopeIndex binderId =
  scopeParentId scopeIndex =<< binderIntroScope scopeIndex binderId

emptyFreeScopeSummary :: FreeScopeSummary
emptyFreeScopeSummary =
  FreeScopeSummary (smallArrayFromList [])

singletonFreeScopeSummary :: ScopeId -> FreeScopeSummary
singletonFreeScopeSummary scopeId =
  FreeScopeSummary (smallArrayFromList [scopeId])

mergeFreeScopeSummary :: ScopeIndex -> FreeScopeSummary -> FreeScopeSummary -> Either ScopeLookupFailure FreeScopeSummary
mergeFreeScopeSummary scopeIndex =
  mergeFreeScopeSummaryByEither (scopeDepthOf scopeIndex)

mergeFreeScopeSummaryBy :: (ScopeId -> Int) -> FreeScopeSummary -> FreeScopeSummary -> FreeScopeSummary
mergeFreeScopeSummaryBy depthOf leftSummary rightSummary =
  either absurd id (mergeFreeScopeSummaryByEither (Right . depthOf) leftSummary rightSummary)

mergeFreeScopeSummaryByEither ::
  (ScopeId -> Either failure Int) ->
  FreeScopeSummary ->
  FreeScopeSummary ->
  Either failure FreeScopeSummary
mergeFreeScopeSummaryByEither depthOf leftSummary rightSummary =
  FreeScopeSummary . smallArrayFromList
    <$> go (freeScopeSummaryToList leftSummary) (freeScopeSummaryToList rightSummary)
  where
    go leftValues rightValues =
      case (leftValues, rightValues) of
        ([], []) ->
          Right []
        ([], _) ->
          Right rightValues
        (_, []) ->
          Right leftValues
        (leftScope : remainingLeft, rightScope : remainingRight)
          | leftScope == rightScope ->
              (leftScope :) <$> go remainingLeft remainingRight
          | otherwise -> do
              leftDepth <- depthOf leftScope
              rightDepth <- depthOf rightScope
              case compare leftDepth rightDepth of
                GT ->
                  (leftScope :) <$> go remainingLeft rightValues
                LT ->
                  (rightScope :) <$> go leftValues remainingRight
                EQ ->
                  case compare leftScope rightScope of
                    LT ->
                      (leftScope :) <$> go remainingLeft rightValues
                    GT ->
                      (rightScope :) <$> go leftValues remainingRight
                    EQ ->
                      (leftScope :) <$> go remainingLeft remainingRight

deleteFreeScopeSummary :: ScopeId -> FreeScopeSummary -> FreeScopeSummary
deleteFreeScopeSummary targetScope summaryValue =
  FreeScopeSummary
    ( smallArrayFromList
        (filter (/= targetScope) (freeScopeSummaryToList summaryValue))
    )

freeScopeSummaryContains :: ScopeId -> FreeScopeSummary -> Bool
freeScopeSummaryContains targetScope summaryValue =
  go 0
  where
    FreeScopeSummary scopeArray = summaryValue
    go indexValue
      | indexValue >= sizeofSmallArray scopeArray =
          False
      | indexSmallArray scopeArray indexValue == targetScope =
          True
      | otherwise =
          go (indexValue + 1)

freeScopeSummarySize :: FreeScopeSummary -> Int
freeScopeSummarySize (FreeScopeSummary scopeArray) =
  sizeofSmallArray scopeArray

freeScopeSummaryToList :: FreeScopeSummary -> [ScopeId]
freeScopeSummaryToList (FreeScopeSummary scopeArray) =
  scopeArrayToList scopeArray

freeScopeSupportAnchor :: ScopeIndex -> FreeScopeSummary -> ScopeId
freeScopeSupportAnchor scopeIndex (FreeScopeSummary scopeArray)
  | sizeofSmallArray scopeArray > 0 =
      indexSmallArray scopeArray 0
  | otherwise =
      siRoot scopeIndex

type PreorderIndex :: Type
data PreorderIndex = PreorderIndex
  { preorderDepth :: !(Vector Int),
    preorderSubtreeEnd :: !(Vector Int),
    preorderHasBranch :: !Bool,
    preorderDeepest :: !ScopeId
  }

type OpenScope :: Type
data OpenScope = OpenScope
  { openScopeKey :: !Int,
    openScopeDepth :: !Int
  }

type PreorderBuild :: Type
data PreorderBuild = PreorderBuild
  { pbOpenScopes :: ![OpenScope],
    pbDepthsRev :: ![Int],
    pbSubtreeEnds :: !(IntMap.IntMap Int),
    pbChildCounts :: !(IntMap.IntMap Int),
    pbHasBranch :: !Bool,
    pbDeepest :: !(ScopeId, Int)
  }

buildPreorderIndex :: Vector Int -> Either ScopeIndexFailure PreorderIndex
buildPreorderIndex parentVector = do
  finalBuild <-
    foldM
      appendPreorderScope
      PreorderBuild
        { pbOpenScopes = [OpenScope 0 0],
          pbDepthsRev = [0],
          pbSubtreeEnds = IntMap.empty,
          pbChildCounts = IntMap.singleton 0 0,
          pbHasBranch = False,
          pbDeepest = (rootScopeId, 0)
        }
      (zip [1 ..] (drop 1 (V.toList parentVector)))
  let scopeCount = V.length parentVector
      subtreeEnds =
        foldr
          (\openScope -> IntMap.insert (openScopeKey openScope) scopeCount)
          (pbSubtreeEnds finalBuild)
          (pbOpenScopes finalBuild)
  materializedEnds <-
    V.fromList
      <$> traverse
        ( \scopeKey ->
            maybe
              (Left (ScopeSubtreeEndMissing (ScopeId scopeKey)))
              Right
              (IntMap.lookup scopeKey subtreeEnds)
        )
        [0 .. scopeCount - 1]
  pure
    PreorderIndex
      { preorderDepth = V.fromList (reverse (pbDepthsRev finalBuild)),
        preorderSubtreeEnd = materializedEnds,
        preorderHasBranch = pbHasBranch finalBuild,
        preorderDeepest = fst (pbDeepest finalBuild)
      }

appendPreorderScope :: PreorderBuild -> (Int, Int) -> Either ScopeIndexFailure PreorderBuild
appendPreorderScope buildState (scopeKey, parentKey) = do
  (closedScopes, parentDepth, remainingOpen) <-
    closeScopesUntil parentKey (pbOpenScopes buildState)
  let childCount = IntMap.findWithDefault 0 parentKey (pbChildCounts buildState) + 1
      scopeDepth = parentDepth + 1
      deepestValue =
        if scopeDepth > snd (pbDeepest buildState)
          then (ScopeId scopeKey, scopeDepth)
          else pbDeepest buildState
  pure
    buildState
      { pbOpenScopes = OpenScope scopeKey scopeDepth : remainingOpen,
        pbDepthsRev = scopeDepth : pbDepthsRev buildState,
        pbSubtreeEnds =
          foldr
            (`IntMap.insert` scopeKey)
            (pbSubtreeEnds buildState)
            closedScopes,
        pbChildCounts =
          IntMap.insert scopeKey 0
            (IntMap.insert parentKey childCount (pbChildCounts buildState)),
        pbHasBranch = pbHasBranch buildState || childCount > 1,
        pbDeepest = deepestValue
      }
  where
    closeScopesUntil targetScope openScopes =
      case break ((== targetScope) . openScopeKey) openScopes of
        (_, []) ->
          Left (ScopeParentNotPreorder (ScopeId scopeKey) parentKey)
        (closedScopes, parentScope : survivingScopes) ->
          Right
            ( fmap openScopeKey closedScopes,
              openScopeDepth parentScope,
              parentScope : survivingScopes
            )

buildLift :: Vector Int -> Either ScopeIndexFailure (Vector (Vector Int))
buildLift parentVector =
  V.fromList . reverse
    <$> foldM appendLevel [parentVector] [1 .. levelCount - 1]
  where
    scopeCount = V.length parentVector

    levelCount =
      max 1 (length (takeWhile (< scopeCount) (iterate (* 2) 1)))

    appendLevel :: [Vector Int] -> Int -> Either ScopeIndexFailure [Vector Int]
    appendLevel levels _ =
      case levels of
        [] ->
          Left (ScopeLiftMissing rootScopeId)
        previousLevel : _ -> do
          nextLevel <- traverse (nextAncestor previousLevel) previousLevel
          Right (nextLevel : levels)

    nextAncestor :: Vector Int -> Int -> Either ScopeIndexFailure Int
    nextAncestor previousLevel ancestorKey =
      maybe
        (Left (ScopeLiftMissing (ScopeId ancestorKey)))
        Right
        (previousLevel V.!? ancestorKey)

liftAncestor :: ScopeIndex -> Int -> ScopeId -> Either ScopeLookupFailure ScopeId
liftAncestor scopeIndex liftIndex scopeId =
  case siLift scopeIndex V.!? liftIndex of
    Nothing ->
      Left (ScopeLiftLevelOutsideIndex liftIndex (V.length (siLift scopeIndex)))
    Just liftLevel ->
      ScopeId <$> scopeVectorValue ScopeIdOutsideIndex scopeId liftLevel

scopeVectorValue :: (ScopeId -> Int -> ScopeLookupFailure) -> ScopeId -> Vector value -> Either ScopeLookupFailure value
scopeVectorValue failure scopeId vectorValue =
  maybe
    (Left (failure scopeId (V.length vectorValue)))
    Right
    (vectorValue V.!? scopeIdKey scopeId)

binderVectorValue :: BinderId -> Vector ScopeId -> Either ScopeLookupFailure ScopeId
binderVectorValue binderId vectorValue =
  maybe
    (Left (BinderIdOutsideIndex binderId (V.length vectorValue)))
    Right
    (vectorValue V.!? binderIdKey binderId)
