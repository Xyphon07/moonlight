{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.State
  ( BinderOwner (..),
    GroupMachinery (..),
    emptyGroupMachinery,
    ConvState (..),
    ConvM,
    throwConvert,
    liftConvertEither,
    runInstanceMethodSection,
    initialConvState,
    initialConvStateWithRecordFieldEnvironment,
    resolveRecordWildcardFields,
    resolveVarRef,
    currentScopeId,
    withScope,
    freshChildScope,
    scopeDepthInState,
    mkConvExpr,
    throwUnsupportedExpression,
    freshBinderAnn,
    recordLambdaSite,
    recordLetSite,
    currentScopeAlgebra,
    modifyGroupMachinery,
    freshBindingGroupId,
    registerBindingOwners,
    withActiveBindingRow,
    recordBindingDependency,
    takeBindingDependencies
  )
where

import Control.Monad.State.Strict (StateT (..), gets, modify', runStateT, state)
import Control.Monad.Trans.Class (lift)
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Types.Name.Reader (RdrName)
import Moonlight.Core (BinderId (..), binderIdKey)
import Moonlight.Pale.Ghc.Expr.Convert.FreeScopes (ScopeAlgebra (..))
import Moonlight.Pale.Ghc.Expr.Convert.FreeScopes qualified as FreeScopes
import Moonlight.Pale.Ghc.Expr.Convert.Obstruction
import Moonlight.Pale.Ghc.Expr.Convert.Row
import Moonlight.Pale.Ghc.Expr.Opaque
import Moonlight.Pale.Ghc.Expr.Scope
import Moonlight.Pale.Ghc.Expr.Syntax

type BinderOwner :: Type
newtype BinderOwner = BinderOwner
  { binderOwnerGroup :: BindingGroupId
  }
  deriving stock (Eq, Ord, Show)

type GroupMachinery :: Type
data GroupMachinery = GroupMachinery
  { gmNextGroupId :: !Int,
    gmBinderOwners :: !(IntMap BinderOwner),
    gmActiveRows :: !(IntMap Int),
    gmDependencies :: !(IntMap (IntMap (Set BinderId)))
  }

emptyGroupMachinery :: GroupMachinery
emptyGroupMachinery =
  GroupMachinery 0 IntMap.empty IntMap.empty IntMap.empty

type ConvState :: Type
data ConvState = ConvState
  { csRecordFieldEnvironment :: !RecordFieldEnvironment,
    csNextBinderId :: !Int,
    csNextScopeId :: !Int,
    csCurrentScope :: !ScopeId,
    csScopeParentsRev :: ![Int],
    csScopeDepths :: !(IntMap Int),
    csBinderIntroRev :: ![Int],
    csBinderIntroMap :: !(IntMap ScopeId),
    csGroupMachinery :: !GroupMachinery,
    csLambdaSites :: ![BinderAnn],
    csLetSites :: ![BinderAnn]
  }

type ConvM :: Type -> Type
type ConvM = StateT ConvState (Either ConvertObstruction)

throwConvert :: ConvertObstruction -> ConvM value
throwConvert =
  lift . Left

liftConvertEither :: Either ConvertObstruction value -> ConvM value
liftConvertEither =
  either throwConvert pure

runInstanceMethodSection ::
  Maybe SourceRegion ->
  ConvM value ->
  ConvM (Either InstanceMethodObstruction value)
runInstanceMethodSection methodRegion action =
  StateT
    ( \checkpointState ->
        case runStateT action checkpointState of
          Right (value, methodState) ->
            Right (Right value, methodState)
          Left obstruction ->
            case recoverableInstanceMethodObstruction methodRegion obstruction of
              Just methodObstruction ->
                Right (Left methodObstruction, checkpointState)
              Nothing ->
                Left obstruction
    )

initialConvState :: ConvState
initialConvState =
  initialConvStateWithRecordFieldEnvironment emptyRecordFieldEnvironment

initialConvStateWithRecordFieldEnvironment ::
  RecordFieldEnvironment ->
  ConvState
initialConvStateWithRecordFieldEnvironment recordFieldEnvironment =
  ConvState
    { csRecordFieldEnvironment = recordFieldEnvironment,
      csNextBinderId = 0,
      csNextScopeId = 1,
      csCurrentScope = rootScopeId,
      csScopeParentsRev = [0],
      csScopeDepths = IntMap.singleton 0 0,
      csBinderIntroRev = [],
      csBinderIntroMap = IntMap.empty,
      csGroupMachinery = emptyGroupMachinery,
      csLambdaSites = [],
      csLetSites = []
    }

resolveRecordWildcardFields ::
  SourceRegion ->
  RdrName ->
  ConvM [RdrName]
resolveRecordWildcardFields wildcardRegion constructorName = do
  recordFieldEnvironment <- gets csRecordFieldEnvironment
  either
    (throwConvert . ConvertRecordWildcardResolutionUnavailable wildcardRegion)
    pure
    (resolveRecordFieldEnvironment recordFieldEnvironment constructorName)

resolveVarRef :: Env -> RdrName -> ConvM HsVarRef
resolveVarRef env nameValue =
  case Map.lookup nameValue env of
    Nothing ->
      pure (GlobalName nameValue)
    Just binderAnn -> do
      recordBindingDependency binderAnn
      pure (LocalName binderAnn)

currentScopeId :: ConvM ScopeId
currentScopeId =
  gets csCurrentScope

withScope :: ScopeId -> ConvM value -> ConvM value
withScope scopeId action = do
  previousScope <- gets csCurrentScope
  modify' (\stateValue -> stateValue {csCurrentScope = scopeId})
  resultValue <- action
  modify' (\stateValue -> stateValue {csCurrentScope = previousScope})
  pure resultValue

freshChildScope :: ConvM ScopeId
freshChildScope = do
  parentScope <- gets csCurrentScope
  parentDepth <- scopeDepthInState parentScope
  nextScopeId <- gets csNextScopeId
  childScope <-
    either
      (throwConvert . ConvertFreshScopeIdFailure nextScopeId)
      pure
      (mkScopeId nextScopeId)
  let !parentKey = scopeIdKey parentScope
  modify'
    ( \stateValue ->
        stateValue
          { csNextScopeId = nextScopeId + 1,
            csScopeParentsRev = parentKey : csScopeParentsRev stateValue,
            csScopeDepths = IntMap.insert nextScopeId (parentDepth + 1) (csScopeDepths stateValue)
          }
    )
  pure childScope

scopeDepthInState :: ScopeId -> ConvM Int
scopeDepthInState scopeId = do
  depthMap <- gets csScopeDepths
  maybe
    (throwConvert (ConvertMissingScopeDepth scopeId))
    pure
    (IntMap.lookup (scopeIdKey scopeId) depthMap)

mkConvExpr :: Maybe SourceRegion -> HsExprF Expr -> ConvM Expr
mkConvExpr region nodeValue = do
  occurrenceScope <- currentScopeId
  scopeAlgebra <- currentScopeAlgebra
  freeScopes <- liftConvertEither (FreeScopes.freeScopesExpr scopeAlgebra nodeValue)
  pure
    Expr
      { exprRegion = region,
        exprScope = occurrenceScope,
        exprFreeScopes = freeScopes,
        exprNode = nodeValue
      }

throwUnsupportedExpression :: Maybe SourceRegion -> HsOpaqueTag -> ConvM value
throwUnsupportedExpression region opaqueTag =
  throwConvert (ConvertUnsupportedExpression region opaqueTag)

freshBinderAnn :: RdrName -> ConvM BinderAnn
freshBinderAnn binderName = do
  nextBinderId <- gets csNextBinderId
  introScope <- gets csCurrentScope
  let !introKey = scopeIdKey introScope
  modify'
    ( \stateValue ->
        stateValue
          { csNextBinderId = nextBinderId + 1,
            csBinderIntroRev = introKey : csBinderIntroRev stateValue,
            csBinderIntroMap = IntMap.insert nextBinderId introScope (csBinderIntroMap stateValue)
          }
    )
  pure
    BinderAnn
      { baId = BinderId nextBinderId,
        baName = binderName
      }

recordLambdaSite :: BinderAnn -> ConvM ()
recordLambdaSite binderAnn =
  modify' (\stateValue -> stateValue {csLambdaSites = binderAnn : csLambdaSites stateValue})

recordLetSite :: BinderAnn -> ConvM ()
recordLetSite binderAnn =
  modify' (\stateValue -> stateValue {csLetSites = binderAnn : csLetSites stateValue})

currentScopeAlgebra :: ConvM (ScopeAlgebra ConvertObstruction)
currentScopeAlgebra = do
  depthMap <- gets csScopeDepths
  introMap <- gets csBinderIntroMap
  pure
    ScopeAlgebra
      { saScopeDepth =
          \scopeId ->
            maybe
              (Left (ConvertMissingScopeSummaryDepth scopeId))
              Right
              (IntMap.lookup (scopeIdKey scopeId) depthMap),
        saBinderIntro =
          \binderAnn ->
            maybe
              (Left (ConvertMissingBinderIntro (baId binderAnn)))
              Right
              (IntMap.lookup (binderIdKey (baId binderAnn)) introMap)
      }

modifyGroupMachinery :: (GroupMachinery -> GroupMachinery) -> ConvM ()
modifyGroupMachinery adjustMachinery =
  modify'
    ( \stateValue ->
        stateValue {csGroupMachinery = adjustMachinery (csGroupMachinery stateValue)}
    )

freshBindingGroupId :: ConvM BindingGroupId
freshBindingGroupId = do
  nextGroupId <- gets (gmNextGroupId . csGroupMachinery)
  modifyGroupMachinery
    (\machineryValue -> machineryValue {gmNextGroupId = nextGroupId + 1})
  pure (BindingGroupId nextGroupId)

registerBindingOwners :: BindingGroupId -> NonEmpty HsPatF -> ConvM ()
registerBindingOwners bindingGroupId bindingPatterns =
  modifyGroupMachinery
    ( \machineryValue ->
        machineryValue
          { gmBinderOwners =
              foldr
                (uncurry IntMap.insert)
                (gmBinderOwners machineryValue)
                [ ( binderIdKey (baId binderAnn),
                    BinderOwner bindingGroupId
                  )
                | bindingPatternValue <- NonEmpty.toList bindingPatterns,
                  binderAnn <- patBinders bindingPatternValue
                ]
          }
    )

withActiveBindingRow :: BindingGroupId -> Int -> ConvM value -> ConvM value
withActiveBindingRow bindingGroupId rowIndex action = do
  let groupKey = bindingGroupIdKey bindingGroupId
  previousRow <- gets (IntMap.lookup groupKey . gmActiveRows . csGroupMachinery)
  modifyGroupMachinery
    ( \machineryValue ->
        machineryValue
          { gmActiveRows =
              IntMap.insert groupKey rowIndex (gmActiveRows machineryValue)
          }
    )
  resultValue <- action
  modifyGroupMachinery
    ( \machineryValue ->
        machineryValue
          { gmActiveRows =
              maybe
                (IntMap.delete groupKey (gmActiveRows machineryValue))
                (\previousRowIndex -> IntMap.insert groupKey previousRowIndex (gmActiveRows machineryValue))
                previousRow
          }
    )
  pure resultValue

recordBindingDependency :: BinderAnn -> ConvM ()
recordBindingDependency binderAnn = do
  maybeOwner <-
    gets
      (IntMap.lookup (binderIdKey (baId binderAnn)) . gmBinderOwners . csGroupMachinery)
  case maybeOwner of
    Nothing ->
      pure ()
    Just binderOwner -> do
      let groupKey = bindingGroupIdKey (binderOwnerGroup binderOwner)
      maybeSourceRow <-
        gets (IntMap.lookup groupKey . gmActiveRows . csGroupMachinery)
      case maybeSourceRow of
        Nothing ->
          pure ()
        Just sourceRow ->
          modifyGroupMachinery
            ( \machineryValue ->
                machineryValue
                  { gmDependencies =
                      IntMap.insertWith
                        (IntMap.unionWith Set.union)
                        groupKey
                        (IntMap.singleton sourceRow (Set.singleton (baId binderAnn)))
                        (gmDependencies machineryValue)
                  }
            )

takeBindingDependencies ::
  BindingGroupId ->
  [BinderAnn] ->
  ConvM (IntMap (Set BinderId))
takeBindingDependencies bindingGroupId bindingAnnotations = do
  let groupKey = bindingGroupIdKey bindingGroupId
  state
    ( \stateValue ->
        let machineryValue = csGroupMachinery stateValue
         in ( IntMap.findWithDefault
                IntMap.empty
                groupKey
                (gmDependencies machineryValue),
              stateValue
                { csGroupMachinery =
                    machineryValue
                      { gmBinderOwners =
                          foldr
                            ( \binderAnn binderOwners ->
                                IntMap.delete
                                  (binderIdKey (baId binderAnn))
                                  binderOwners
                            )
                            (gmBinderOwners machineryValue)
                            bindingAnnotations,
                        gmDependencies =
                          IntMap.delete groupKey (gmDependencies machineryValue)
                      }
                }
            )
    )
