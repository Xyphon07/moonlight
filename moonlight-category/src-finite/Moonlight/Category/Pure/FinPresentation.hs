{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

{-| A name-binding builder for finite categories that compiles to the validated
'FinCat' semantic representation.

The builder has two deliberately separate dialects:

* A finite-poset dialect. 'below' declares strict generating inequalities;
  transitive closure is computed and cyclic declarations are rejected. Identities
  are implicit, so the resulting category carries the corresponding reflexive
  order.

* A fully enumerated finite-category dialect. 'arrow' declares every nonidentity
  morphism, 'identityAt' denotes an identity, and 'equate' supplies the composition
  table. The 'FinCat' constructors remain the semantic owners that validate
  closure and associativity.

A longer path equation is accepted only when each proper intermediate composite can
already be resolved from other equations. This module does not construct quotients
of free categories by arbitrary path congruences.
-}
module Moonlight.Category.Pure.FinPresentation
  ( FinBuilder,
    ObjRef,
    ArrowExpr,
    FinCatBuildError (..),
    object,
    objects,
    arrow,
    identityAt,
    below,
    after,
    equate,
    finCategory,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Bits (bit, (.|.))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinCatValidationError,
    FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    trustedFinCatWithGeneratorBasis,
    trustedDenseThinFinCatFromReachabilityRows,
  )
import Moonlight.Category.Pure.Finite.DenseReachability
  ( denseClosureCycleComponents,
    denseClosureReachabilityRows,
    denseReachabilityWithCycles,
  )

-- | An opaque reference to an object declared by 'object'.
type ObjRef :: Type
data ObjRef = ObjRef !FinObjectId !String
  deriving stock (Eq, Show)

-- | An opaque morphism expression. Expressions are identities, named nonidentity
-- morphisms, or composites built with 'after'.
type ArrowExpr :: Type
data ArrowExpr
  = ExprIdentity !FinObjectId !String
  | ExprGen !FinGeneratorId !FinObjectId !FinObjectId !String
  | ExprComp !ArrowExpr !ArrowExpr
  deriving stock (Eq, Ord)

instance Show ArrowExpr where
  showsPrec precedence expression =
    case expression of
      ExprIdentity _ objectName ->
        showString "id[" . showString objectName . showChar ']'
      ExprGen _ _ _ arrowName ->
        showString arrowName
      ExprComp leftExpression rightExpression ->
        showParen
          (precedence > 9)
          ( showsPrec 10 leftExpression
              . showString " `after` "
              . showsPrec 9 rightExpression
          )

-- | Faults discovered while building or compiling a presentation.
type FinCatBuildError :: Type
data FinCatBuildError
  = DuplicateObjectName String
  | DuplicateArrowName String
  | DanglingObjectReference FinObjectId
  | MixedPresentationModes
  | CyclicStrictOrder [FinObjectId]
  | NonComposablePath ArrowExpr ArrowExpr FinObjectId FinObjectId
  | NonParallelEquation
      ArrowExpr
      ArrowExpr
      (FinObjectId, FinObjectId)
      (FinObjectId, FinObjectId)
  | UnsupportedEquation ArrowExpr ArrowExpr
  | UnresolvedComposite ArrowExpr
  | ConflictingComposition ArrowExpr ArrowExpr ArrowExpr ArrowExpr
  | IdentityEquationMismatch ArrowExpr ArrowExpr ArrowExpr
  | BuilderPatternFailure String
  | InvalidPresentation (NonEmpty FinCatValidationError)
  deriving stock (Eq, Show)

type ArrowDecl :: Type
data ArrowDecl =
  ArrowDecl !FinGeneratorId !FinObjectId !FinObjectId

type BuilderState :: Type
data BuilderState = BuilderState
  { builderObjectIds :: !(Map String FinObjectId),
    builderNextObject :: !Int,
    builderArrows :: !(Map String ArrowDecl),
    builderNextArrow :: !Int,
    builderBelowEdgesRev :: ![(FinObjectId, FinObjectId)],
    builderEquationsRev :: ![(ArrowExpr, ArrowExpr)],
    builderFirstError :: !(Maybe FinCatBuildError)
  }

initialState :: BuilderState
initialState =
  BuilderState
    { builderObjectIds = Map.empty,
      builderNextObject = 0,
      builderArrows = Map.empty,
      builderNextArrow = 0,
      builderBelowEdgesRev = [],
      builderEquationsRev = [],
      builderFirstError = Nothing
    }

-- | A pure presentation builder. The 'Maybe' lets refutable @do@-patterns
-- (e.g. @[a, b, c] <- objects [..]@) short-circuit via 'fail' while still
-- preserving the declarations accumulated so far for error reporting.
type FinBuilder :: Type -> Type
newtype FinBuilder a = FinBuilder {unFinBuilder :: BuilderState -> (Maybe a, BuilderState)}

instance Functor FinBuilder where
  fmap f (FinBuilder run) =
    FinBuilder (\state -> let (result, state') = run state in (fmap f result, state'))

instance Applicative FinBuilder where
  pure value = FinBuilder (\state -> (Just value, state))
  FinBuilder runF <*> FinBuilder runA =
    FinBuilder
      ( \state ->
          case runF state of
            (Nothing, state') -> (Nothing, state')
            (Just f, state') ->
              let (result, state'') = runA state'
               in (fmap f result, state'')
      )

instance Monad FinBuilder where
  FinBuilder run >>= k =
    FinBuilder
      ( \state ->
          case run state of
            (Nothing, state') -> (Nothing, state')
            (Just value, state') -> unFinBuilder (k value) state'
      )

instance MonadFail FinBuilder where
  fail message =
    FinBuilder (\state -> (Nothing, recordError (BuilderPatternFailure message) state))

recordError :: FinCatBuildError -> BuilderState -> BuilderState
recordError buildError state =
  case builderFirstError state of
    Nothing -> state {builderFirstError = Just buildError}
    Just _ -> state
{-# INLINE recordError #-}

objectReferenceDeclared :: BuilderState -> FinObjectId -> Bool
objectReferenceDeclared state (FinObjectId objectIndex) =
  objectIndex >= 0 && objectIndex < builderNextObject state
{-# INLINE objectReferenceDeclared #-}

recordObjectReference :: FinObjectId -> BuilderState -> BuilderState
recordObjectReference objectId state =
  if objectReferenceDeclared state objectId
    then state
    else recordError (DanglingObjectReference objectId) state
{-# INLINE recordObjectReference #-}

-- | Declare an object, returning an opaque reference for subsequent declarations.
object :: String -> FinBuilder ObjRef
object objectName =
  FinBuilder
    ( \state ->
        case Map.lookup objectName (builderObjectIds state) of
          Just objectId ->
            ( Just (ObjRef objectId objectName),
              recordError (DuplicateObjectName objectName) state
            )
          Nothing ->
            let objectId = FinObjectId (builderNextObject state)
                nextState =
                  state
                    { builderObjectIds =
                        Map.insert
                          objectName
                          objectId
                          (builderObjectIds state),
                      builderNextObject =
                        builderNextObject state + 1
                    }
             in (Just (ObjRef objectId objectName), nextState)
    )

-- | Declare several objects in source order.
objects :: [String] -> FinBuilder [ObjRef]
objects = traverse object

-- | Declare a named nonidentity morphism. Every nonidentity morphism of a general
-- presentation must be declared explicitly.
arrow :: ObjRef -> ObjRef -> String -> FinBuilder ArrowExpr
arrow (ObjRef sourceId _) (ObjRef targetId _) arrowName =
  FinBuilder
    ( \state ->
        case Map.lookup arrowName (builderArrows state) of
          Just (ArrowDecl generatorId existingSource existingTarget) ->
            ( Just
                ( ExprGen
                    generatorId
                    existingSource
                    existingTarget
                    arrowName
                ),
              recordError (DuplicateArrowName arrowName) state
            )
          Nothing ->
            let checkedState =
                  recordObjectReference targetId
                    (recordObjectReference sourceId state)
                generatorId =
                  FinGeneratorId (builderNextArrow checkedState)
                nextState =
                  checkedState
                    { builderArrows =
                        Map.insert
                          arrowName
                          (ArrowDecl generatorId sourceId targetId)
                          (builderArrows checkedState),
                      builderNextArrow =
                        builderNextArrow checkedState + 1
                    }
             in ( Just
                    (ExprGen generatorId sourceId targetId arrowName),
                  nextState
                )
    )

-- | The identity morphism at a declared object.
identityAt :: ObjRef -> ArrowExpr
identityAt (ObjRef objectId objectName) =
  ExprIdentity objectId objectName
{-# INLINE identityAt #-}

-- | Declare a strict generating inequality. Cyclic strict inequalities are
-- rejected; identities and transitive consequences are supplied by compilation.
below :: ObjRef -> ObjRef -> FinBuilder ()
below (ObjRef sourceId _) (ObjRef targetId _) =
  FinBuilder
    ( \state ->
        let checkedState =
              recordObjectReference targetId
                (recordObjectReference sourceId state)
         in ( Just (),
              checkedState
                { builderBelowEdgesRev =
                    (sourceId, targetId)
                      : builderBelowEdgesRev checkedState
                }
            )
    )

-- | @g `after` f@ denotes @g ∘ f@: first @f@, then @g@.
infixr 9 `after`

after :: ArrowExpr -> ArrowExpr -> ArrowExpr
after = ExprComp
{-# INLINE after #-}

-- | Record an equation between parallel morphism expressions.
equate :: ArrowExpr -> ArrowExpr -> FinBuilder ()
equate leftExpression rightExpression =
  FinBuilder
    ( \state ->
        ( Just (),
          state
            { builderEquationsRev =
                (leftExpression, rightExpression)
                  : builderEquationsRev state
            }
        )
    )

-- | Compile a presentation to a validated finite category.
finCategory :: FinBuilder a -> Either FinCatBuildError FinCat
finCategory builder =
  case unFinBuilder builder initialState of
    (_, state) ->
      case builderFirstError state of
        Just buildError -> Left buildError
        Nothing -> compilePresentation state

compilePresentation :: BuilderState -> Either FinCatBuildError FinCat
compilePresentation state =
  let objectCount = builderNextObject state
      objectSet =
        Set.fromDistinctAscList
          (FinObjectId <$> [0 .. objectCount - 1])
      belowEdges =
        reverse (builderBelowEdgesRev state)
      arrows =
        Map.elems (builderArrows state)
      equations =
        reverse (builderEquationsRev state)
      hasBelow =
        not (null belowEdges)
      hasGeneral =
        not (null arrows) || not (null equations)
   in case (hasBelow, hasGeneral) of
        (True, True) ->
          Left MixedPresentationModes
        (True, False) ->
          compileStrictOrder objectCount objectSet belowEdges
        (False, _) ->
          compileGeneral objectCount objectSet arrows equations

compileStrictOrder ::
  Int ->
  Set FinObjectId ->
  [(FinObjectId, FinObjectId)] ->
  Either FinCatBuildError FinCat
compileStrictOrder objectCount objectSet belowEdges =
  case
    danglingEndpoints
      objectCount
      (belowEdges >>= \(sourceId, targetId) -> [sourceId, targetId])
    of
      badObject : _ ->
        Left (DanglingObjectReference badObject)
      [] ->
        let closure =
              denseReachabilityWithCycles
                (importRowsFromEdges objectCount belowEdges)
            closedRows =
              denseClosureReachabilityRows closure
            cycleComponents =
              denseClosureCycleComponents closure
         in if null cycleComponents
              then
                Right
                  ( trustedDenseThinFinCatFromReachabilityRows
                      objectSet
                      closedRows
                  )
              else
                Left
                  (CyclicStrictOrder (objectIdsFromComponents cycleComponents))

compileGeneral ::
  Int ->
  Set FinObjectId ->
  [ArrowDecl] ->
  [(ArrowExpr, ArrowExpr)] ->
  Either FinCatBuildError FinCat
compileGeneral objectCount objectSet arrows equations =
  case danglingEndpoints objectCount (arrowEndpointIds arrows) of
    badObject : _ ->
      Left (DanglingObjectReference badObject)
    [] ->
      case firstDanglingEquationObject objectCount equations of
        Just badObject ->
          Left (DanglingObjectReference badObject)
        Nothing -> do
          compositionMap <- compileEquations equations
          let generatorBasis =
                Set.fromList (arrowMorphismId <$> arrows)
          first
            InvalidPresentation
            ( trustedFinCatWithGeneratorBasis
                generatorBasis
                objectSet
                (morphismMapFromArrows arrows)
                compositionMap
            )

arrowEndpointIds :: [ArrowDecl] -> [FinObjectId]
arrowEndpointIds =
  foldr
    ( \(ArrowDecl _ sourceId targetId) rest ->
        sourceId : targetId : rest
    )
    []

arrowMorphismId :: ArrowDecl -> FinMorphismId
arrowMorphismId (ArrowDecl generatorId _ _) =
  FinGeneratorMorphismId generatorId

morphismMapFromArrows ::
  [ArrowDecl] ->
  Map (FinObjectId, FinObjectId) [FinMorphismId]
morphismMapFromArrows =
  foldl' insertArrow Map.empty
  where
    insertArrow morphismMap (ArrowDecl generatorId sourceId targetId) =
      Map.insertWith
        (<>)
        (sourceId, targetId)
        [FinGeneratorMorphismId generatorId]
        morphismMap

type CompositionKey :: Type
type CompositionKey =
  (FinMorphismId, FinMorphismId)

type OrientedEquation :: Type
data OrientedEquation =
  OrientedEquation !ArrowExpr !ArrowExpr !FinMorphismId

type CompositionClaim :: Type
data CompositionClaim =
  CompositionClaim !ArrowExpr !ArrowExpr !FinMorphismId

type BlockedExpression :: Type
data BlockedExpression =
  BlockedExpression !CompositionKey !ArrowExpr

type EquationAttempt :: Type
data EquationAttempt
  = EquationBlocked !BlockedExpression
  | EquationSatisfied
  | EquationClaims !CompositionKey !CompositionClaim

type WaitingEquation :: Type
data WaitingEquation =
  WaitingEquation !ArrowExpr !OrientedEquation

compileEquations ::
  [(ArrowExpr, ArrowExpr)] ->
  Either FinCatBuildError (Map CompositionKey FinMorphismId)
compileEquations equations = do
  preparedEquations <- traverse prepareEquation equations
  claims <-
    solveEquations
      Map.empty
      Map.empty
      (Seq.fromList (catMaybes preparedEquations))
  pure (fmap compositionClaimResultId claims)

prepareEquation ::
  (ArrowExpr, ArrowExpr) ->
  Either FinCatBuildError (Maybe OrientedEquation)
prepareEquation (leftExpression, rightExpression) = do
  leftEndpoints <- expressionEndpoints leftExpression
  rightEndpoints <- expressionEndpoints rightExpression
  if leftEndpoints == rightEndpoints
    then orientParallelEquation leftExpression rightExpression
    else
      Left
        ( NonParallelEquation
            leftExpression
            rightExpression
            leftEndpoints
            rightEndpoints
        )

orientParallelEquation ::
  ArrowExpr ->
  ArrowExpr ->
  Either FinCatBuildError (Maybe OrientedEquation)
orientParallelEquation leftExpression rightExpression
  | leftExpression == rightExpression =
      Right Nothing
  | otherwise =
      case
        ( leftExpression,
          rightExpression,
          atomicMorphismId leftExpression,
          atomicMorphismId rightExpression
        )
        of
          (ExprComp _ _, _, _, Just resultId) ->
            Right
              ( Just
                  ( OrientedEquation
                      leftExpression
                      rightExpression
                      resultId
                  )
              )
          (_, ExprComp _ _, Just resultId, _) ->
            Right
              ( Just
                  ( OrientedEquation
                      rightExpression
                      leftExpression
                      resultId
                  )
              )
          (_, _, Just leftId, Just rightId)
            | leftId == rightId ->
                Right Nothing
          _ ->
            Left
              (UnsupportedEquation leftExpression rightExpression)

atomicMorphismId :: ArrowExpr -> Maybe FinMorphismId
atomicMorphismId expression =
  case expression of
    ExprIdentity objectId _ ->
      Just (FinIdentityId objectId)
    ExprGen generatorId _ _ _ ->
      Just (FinGeneratorMorphismId generatorId)
    ExprComp _ _ ->
      Nothing

expressionEndpoints ::
  ArrowExpr ->
  Either FinCatBuildError (FinObjectId, FinObjectId)
expressionEndpoints expression =
  case expression of
    ExprIdentity objectId _ ->
      Right (objectId, objectId)
    ExprGen _ sourceId targetId _ ->
      Right (sourceId, targetId)
    ExprComp leftExpression rightExpression -> do
      (leftSource, leftTarget) <-
        expressionEndpoints leftExpression
      (rightSource, rightTarget) <-
        expressionEndpoints rightExpression
      if rightTarget == leftSource
        then Right (rightSource, leftTarget)
        else
          Left
            ( NonComposablePath
                leftExpression
                rightExpression
                rightTarget
                leftSource
            )

solveEquations ::
  Map CompositionKey CompositionClaim ->
  Map CompositionKey (Seq WaitingEquation) ->
  Seq OrientedEquation ->
  Either FinCatBuildError (Map CompositionKey CompositionClaim)
solveEquations !claims !waiting !ready =
  case Seq.viewl ready of
    EmptyL ->
      case firstWaitingExpression waiting of
        Nothing ->
          Right claims
        Just unresolvedExpression ->
          Left (UnresolvedComposite unresolvedExpression)
    equation :< remaining -> do
      attempt <- attemptEquation claims equation
      case attempt of
        EquationBlocked
          (BlockedExpression dependencyKey unresolvedExpression) ->
            solveEquations
              claims
              ( enqueueWaitingEquation
                  dependencyKey
                  (WaitingEquation unresolvedExpression equation)
                  waiting
              )
              remaining
        EquationSatisfied ->
          solveEquations claims waiting remaining
        EquationClaims compositionKey proposedClaim -> do
          (claimWasInserted, nextClaims) <-
            insertCompositionClaim
              compositionKey
              proposedClaim
              claims
          if claimWasInserted
            then
              let awakened =
                    Map.lookup compositionKey waiting
                  nextWaiting =
                    Map.delete compositionKey waiting
                  nextReady =
                    remaining
                      Seq.>< maybe
                        Seq.empty
                        waitingEquations
                        awakened
               in solveEquations
                    nextClaims
                    nextWaiting
                    nextReady
            else
              solveEquations nextClaims waiting remaining

waitingEquations ::
  Seq WaitingEquation ->
  Seq OrientedEquation
waitingEquations =
  fmap
    (\(WaitingEquation _ equation) -> equation)

firstWaitingExpression ::
  Map CompositionKey (Seq WaitingEquation) ->
  Maybe ArrowExpr
firstWaitingExpression waiting = do
  (_, equations) <- Map.lookupMin waiting
  case Seq.viewl equations of
    EmptyL ->
      Nothing
    WaitingEquation unresolvedExpression _ :< _ ->
      Just unresolvedExpression

enqueueWaitingEquation ::
  CompositionKey ->
  WaitingEquation ->
  Map CompositionKey (Seq WaitingEquation) ->
  Map CompositionKey (Seq WaitingEquation)
enqueueWaitingEquation dependencyKey equation =
  Map.alter
    ( \maybeEquations ->
        Just
          ( maybe
              (Seq.singleton equation)
              (|> equation)
              maybeEquations
          )
    )
    dependencyKey

attemptEquation ::
  Map CompositionKey CompositionClaim ->
  OrientedEquation ->
  Either FinCatBuildError EquationAttempt
attemptEquation
  claims
  (OrientedEquation compositeExpression resultExpression resultId) =
    case compositeExpression of
      ExprComp leftExpression rightExpression ->
        case resolveArrowExpression claims leftExpression of
          Left blockedExpression ->
            Right (EquationBlocked blockedExpression)
          Right leftId ->
            case resolveArrowExpression claims rightExpression of
              Left blockedExpression ->
                Right (EquationBlocked blockedExpression)
              Right rightId ->
                case
                  identityComposite
                    leftExpression
                    leftId
                    rightExpression
                    rightId
                  of
                    Just (actualId, expectedExpression)
                      | actualId == resultId ->
                          Right EquationSatisfied
                      | otherwise ->
                          Left
                            ( IdentityEquationMismatch
                                compositeExpression
                                resultExpression
                                expectedExpression
                            )
                    Nothing ->
                      Right
                        ( EquationClaims
                            (leftId, rightId)
                            ( CompositionClaim
                                compositeExpression
                                resultExpression
                                resultId
                            )
                        )
      _ ->
        Left
          (UnsupportedEquation compositeExpression resultExpression)

resolveArrowExpression ::
  Map CompositionKey CompositionClaim ->
  ArrowExpr ->
  Either BlockedExpression FinMorphismId
resolveArrowExpression claims expression =
  case expression of
    ExprIdentity objectId _ ->
      Right (FinIdentityId objectId)
    ExprGen generatorId _ _ _ ->
      Right (FinGeneratorMorphismId generatorId)
    ExprComp leftExpression rightExpression -> do
      leftId <-
        resolveArrowExpression claims leftExpression
      rightId <-
        resolveArrowExpression claims rightExpression
      case
        identityComposite
          leftExpression
          leftId
          rightExpression
          rightId
        of
          Just (composedId, _) ->
            Right composedId
          Nothing ->
            let compositionKey =
                  (leftId, rightId)
             in case Map.lookup compositionKey claims of
                  Nothing ->
                    Left
                      ( BlockedExpression
                          compositionKey
                          expression
                      )
                  Just claim ->
                    Right (compositionClaimResultId claim)

identityComposite ::
  ArrowExpr ->
  FinMorphismId ->
  ArrowExpr ->
  FinMorphismId ->
  Maybe (FinMorphismId, ArrowExpr)
identityComposite leftExpression leftId rightExpression rightId =
  case leftId of
    FinIdentityId _ ->
      Just (rightId, rightExpression)
    FinGeneratorMorphismId _ ->
      case rightId of
        FinIdentityId _ ->
          Just (leftId, leftExpression)
        FinGeneratorMorphismId _ ->
          Nothing

insertCompositionClaim ::
  CompositionKey ->
  CompositionClaim ->
  Map CompositionKey CompositionClaim ->
  Either
    FinCatBuildError
    (Bool, Map CompositionKey CompositionClaim)
insertCompositionClaim compositionKey proposedClaim claims =
  case Map.lookup compositionKey claims of
    Nothing ->
      Right
        ( True,
          Map.insert compositionKey proposedClaim claims
        )
    Just existingClaim
      | compositionClaimResultId existingClaim
          == compositionClaimResultId proposedClaim ->
          Right (False, claims)
      | otherwise ->
          Left
            ( ConflictingComposition
                (compositionClaimComposite existingClaim)
                (compositionClaimResultExpression existingClaim)
                (compositionClaimComposite proposedClaim)
                (compositionClaimResultExpression proposedClaim)
            )

compositionClaimComposite :: CompositionClaim -> ArrowExpr
compositionClaimComposite
  (CompositionClaim compositeExpression _ _) =
    compositeExpression

compositionClaimResultExpression ::
  CompositionClaim ->
  ArrowExpr
compositionClaimResultExpression
  (CompositionClaim _ resultExpression _) =
    resultExpression

compositionClaimResultId ::
  CompositionClaim ->
  FinMorphismId
compositionClaimResultId
  (CompositionClaim _ _ resultId) =
    resultId

firstDanglingEquationObject ::
  Int ->
  [(ArrowExpr, ArrowExpr)] ->
  Maybe FinObjectId
firstDanglingEquationObject objectCount =
  foldr
    ( \(leftExpression, rightExpression) nextDanglingObject ->
        firstDanglingExpressionObject objectCount leftExpression
          <|> firstDanglingExpressionObject objectCount rightExpression
          <|> nextDanglingObject
    )
    Nothing

firstDanglingExpressionObject ::
  Int ->
  ArrowExpr ->
  Maybe FinObjectId
firstDanglingExpressionObject objectCount expression =
  case expression of
    ExprIdentity objectId _ ->
      danglingObject objectCount objectId
    ExprGen _ sourceId targetId _ ->
      danglingObject objectCount sourceId
        <|> danglingObject objectCount targetId
    ExprComp leftExpression rightExpression ->
      firstDanglingExpressionObject objectCount leftExpression
        <|> firstDanglingExpressionObject objectCount rightExpression

danglingObject :: Int -> FinObjectId -> Maybe FinObjectId
danglingObject objectCount objectId@(FinObjectId objectIndex) =
  if objectIndex < 0 || objectIndex >= objectCount
    then Just objectId
    else Nothing

importRowsFromEdges ::
  Int ->
  [(FinObjectId, FinObjectId)] ->
  Vector Integer
importRowsFromEdges objectCount edges =
  let rowsBySource =
        foldl' insertEdge IntMap.empty edges
   in Vector.generate
        objectCount
        (\sourceIndex ->
           IntMap.findWithDefault 0 sourceIndex rowsBySource
        )
  where
    insertEdge ::
      IntMap Integer ->
      (FinObjectId, FinObjectId) ->
      IntMap Integer
    insertEdge
      rows
      (FinObjectId sourceIndex, FinObjectId targetIndex)
        | sourceIndex >= 0,
          sourceIndex < objectCount,
          targetIndex >= 0,
          targetIndex < objectCount =
            IntMap.insertWith
              (.|.)
              sourceIndex
              (bit targetIndex)
              rows
        | otherwise =
            rows

objectIdsFromComponents ::
  [NonEmpty Int] ->
  [FinObjectId]
objectIdsFromComponents components =
  fmap FinObjectId (Set.toAscList (foldr insertComponent Set.empty components))
  where
    insertComponent :: NonEmpty Int -> Set Int -> Set Int
    insertComponent component objectIds =
      foldr Set.insert objectIds component

danglingEndpoints ::
  Int ->
  [FinObjectId] ->
  [FinObjectId]
danglingEndpoints objectCount =
  foldr
    ( \objectId rest ->
        case danglingObject objectCount objectId of
          Nothing ->
            rest
          Just badObject ->
            badObject : rest
    )
    []
