{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Equivalence
  ( renderRoundTripEquivalent,
    renderRoundTripGuardStatementsEquivalent,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Core (Pattern (..), binderIdKey)
import Moonlight.Pale.Ghc.Expr.NameRender (renderRdrName)
import Moonlight.Pale.Ghc.Expr.Syntax

data AlphaEnv = AlphaEnv
  { aeLeftLevels :: !(IntMap Int),
    aeRightLevels :: !(IntMap Int),
    aeNextLevel :: !Int
  }

emptyAlphaEnv :: AlphaEnv
emptyAlphaEnv =
  AlphaEnv IntMap.empty IntMap.empty 0

renderRoundTripEquivalent :: Pattern HsExprF -> Pattern HsExprF -> Bool
renderRoundTripEquivalent =
  equivalentExpr emptyAlphaEnv

renderRoundTripGuardStatementsEquivalent ::
  [HsGuardStmtF (Pattern HsExprF)] ->
  [HsGuardStmtF (Pattern HsExprF)] ->
  Bool
renderRoundTripGuardStatementsEquivalent leftGuards rightGuards =
  maybe
    False
    (const True)
    (equivalentGuards emptyAlphaEnv leftGuards rightGuards)

equivalentExpr :: AlphaEnv -> Pattern HsExprF -> Pattern HsExprF -> Bool
equivalentExpr alphaEnv leftValue rightValue =
  case (stripParens leftValue, stripParens rightValue) of
    (PatternVar leftVar, PatternVar rightVar) ->
      leftVar == rightVar
    (PatternNode leftNode, PatternNode rightNode) ->
      equivalentNode alphaEnv leftNode rightNode
    _ ->
      False

stripParens :: Pattern HsExprF -> Pattern HsExprF
stripParens = \case
  PatternNode (ParF innerValue) -> stripParens innerValue
  patternValue -> patternValue

equivalentNode :: AlphaEnv -> HsExprF (Pattern HsExprF) -> HsExprF (Pattern HsExprF) -> Bool
equivalentNode alphaEnv leftNode rightNode =
  case (leftNode, rightNode) of
    (VarF leftRef, VarF rightRef) ->
      equivalentVarRef alphaEnv leftRef rightRef
    (AppF leftFunction leftArgument, AppF rightFunction rightArgument) ->
      equivalentExpr alphaEnv leftFunction rightFunction
        && equivalentExpr alphaEnv leftArgument rightArgument
    (LamF leftBinder leftBody, LamF rightBinder rightBody) ->
      maybe False (\bodyEnv -> equivalentExpr bodyEnv leftBody rightBody) (bindPair alphaEnv leftBinder rightBinder)
    (LetF leftMode leftBindings leftBody, LetF rightMode rightBindings rightBody) ->
      leftMode == rightMode
        && maybe
          False
          ( \(bindingEnv, rhsPairs) ->
              all
                (\(leftRhs, rightRhs) -> equivalentExpr bindingEnv leftRhs rightRhs)
                rhsPairs
                && equivalentExpr bindingEnv leftBody rightBody
          )
          (equivalentBindingGroup alphaEnv leftBindings rightBindings)
    (OpChainF leftFirst leftTail, OpChainF rightFirst rightTail) ->
      equivalentExpr alphaEnv leftFirst rightFirst
        && equivalentOpChainTail alphaEnv (NonEmpty.toList leftTail) (NonEmpty.toList rightTail)
    (SectionLF leftExpr leftOperator, SectionLF rightExpr rightOperator) ->
      equivalentExpr alphaEnv leftExpr rightExpr
        && equivalentExpr alphaEnv leftOperator rightOperator
    (SectionRF leftOperator leftExpr, SectionRF rightOperator rightExpr) ->
      equivalentExpr alphaEnv leftOperator rightOperator
        && equivalentExpr alphaEnv leftExpr rightExpr
    (LitF leftLiteral, LitF rightLiteral) ->
      equivalentLit leftLiteral rightLiteral
    (OverLitF leftLiteral, OverLitF rightLiteral) ->
      leftLiteral == rightLiteral
    (IfF leftCondition leftThen leftElse, IfF rightCondition rightThen rightElse) ->
      all
        id
        [ equivalentExpr alphaEnv leftCondition rightCondition,
          equivalentExpr alphaEnv leftThen rightThen,
          equivalentExpr alphaEnv leftElse rightElse
        ]
    (CaseF leftScrutinee leftAlternatives, CaseF rightScrutinee rightAlternatives) ->
      equivalentExpr alphaEnv leftScrutinee rightScrutinee
        && equivalentAlternativeList alphaEnv leftAlternatives rightAlternatives
    (DoF leftStatements, DoF rightStatements) ->
      maybe False (const True) (equivalentStatements alphaEnv leftStatements rightStatements)
    (NegF leftExpr, NegF rightExpr) ->
      equivalentExpr alphaEnv leftExpr rightExpr
    (ExplicitListF leftExprs, ExplicitListF rightExprs) ->
      equivalentExprList alphaEnv leftExprs rightExprs
    (ExplicitTupleF leftBoxity leftSlots, ExplicitTupleF rightBoxity rightSlots) ->
      leftBoxity == rightBoxity
        && equivalentTupleSlots alphaEnv leftSlots rightSlots
    (RecordConF leftConstructor leftFields, RecordConF rightConstructor rightFields) ->
      equivalentExpr alphaEnv leftConstructor rightConstructor
        && equivalentFields alphaEnv leftFields rightFields
    (RecordUpdF leftRecord leftFields, RecordUpdF rightRecord rightFields) ->
      equivalentExpr alphaEnv leftRecord rightRecord
        && equivalentFields alphaEnv leftFields rightFields
    (ArithSeqF leftSeq, ArithSeqF rightSeq) ->
      equivalentArithSeq alphaEnv leftSeq rightSeq
    (GuardedF leftAlts, GuardedF rightAlts) ->
      equivalentGuardedAlts alphaEnv leftAlts rightAlts
    (ClausesF leftClauses, ClausesF rightClauses) ->
      equivalentClauses alphaEnv leftClauses rightClauses
    (MultiIfF leftAlts, MultiIfF rightAlts) ->
      equivalentGuardedAlts alphaEnv leftAlts rightAlts
    (ExprWithTySigF leftExpr leftType, ExprWithTySigF rightExpr rightType) ->
      equivalentExpr alphaEnv leftExpr rightExpr && leftType == rightType
    (AppTypeF leftExpr leftType, AppTypeF rightExpr rightType) ->
      equivalentExpr alphaEnv leftExpr rightExpr && leftType == rightType
    _ ->
      False

equivalentVarRef :: AlphaEnv -> HsVarRef -> HsVarRef -> Bool
equivalentVarRef alphaEnv leftRef rightRef =
  case (leftRef, rightRef) of
    (GlobalName leftName, GlobalName rightName) ->
      renderRdrName leftName == renderRdrName rightName
    (LocalName leftBinder, LocalName rightBinder) ->
      case
          ( IntMap.lookup (binderIdKey (baId leftBinder)) (aeLeftLevels alphaEnv),
            IntMap.lookup (binderIdKey (baId rightBinder)) (aeRightLevels alphaEnv)
          )
        of
          (Just leftLevel, Just rightLevel) ->
            leftLevel == rightLevel
          (Nothing, Nothing) ->
            baId leftBinder == baId rightBinder
          _ ->
            False
    _ ->
      False

bindPair :: AlphaEnv -> BinderAnn -> BinderAnn -> Maybe AlphaEnv
bindPair alphaEnv leftBinder rightBinder =
  let leftKey = binderIdKey (baId leftBinder)
      rightKey = binderIdKey (baId rightBinder)
   in case
        ( IntMap.lookup leftKey (aeLeftLevels alphaEnv),
          IntMap.lookup rightKey (aeRightLevels alphaEnv)
        )
      of
        (Nothing, Nothing) ->
          let nextLevel = aeNextLevel alphaEnv
           in Just
                alphaEnv
                  { aeLeftLevels = IntMap.insert leftKey nextLevel (aeLeftLevels alphaEnv),
                    aeRightLevels = IntMap.insert rightKey nextLevel (aeRightLevels alphaEnv),
                    aeNextLevel = nextLevel + 1
                  }
        (Just leftLevel, Just rightLevel)
          | leftLevel == rightLevel ->
              Just alphaEnv
        _ ->
          Nothing

equivalentPattern :: AlphaEnv -> HsPatF -> HsPatF -> Maybe AlphaEnv
equivalentPattern alphaEnv leftPattern rightPattern =
  case (stripPatParens leftPattern, stripPatParens rightPattern) of
    (PVarP leftBinder, PVarP rightBinder) ->
      bindPair alphaEnv leftBinder rightBinder
    (PWildP, PWildP) ->
      Just alphaEnv
    (PConP leftName leftSubs, PConP rightName rightSubs)
      | renderRdrName leftName == renderRdrName rightName ->
          equivalentPatternList alphaEnv leftSubs rightSubs
    (PTupleP leftBoxity leftSubs, PTupleP rightBoxity rightSubs)
      | leftBoxity == rightBoxity ->
          equivalentPatternList alphaEnv leftSubs rightSubs
    (PListP leftSubs, PListP rightSubs) ->
      equivalentPatternList alphaEnv leftSubs rightSubs
    (PLitP leftLit, PLitP rightLit)
      | equivalentLit leftLit rightLit ->
          Just alphaEnv
    (POverLitP leftLit, POverLitP rightLit)
      | leftLit == rightLit ->
          Just alphaEnv
    (PAsP leftBinder leftSub, PAsP rightBinder rightSub) ->
      bindPair alphaEnv leftBinder rightBinder
        >>= \boundEnv -> equivalentPattern boundEnv leftSub rightSub
    (PBangP leftSub, PBangP rightSub) ->
      equivalentPattern alphaEnv leftSub rightSub
    (PLazyP leftSub, PLazyP rightSub) ->
      equivalentPattern alphaEnv leftSub rightSub
    (PRecP leftName leftFields, PRecP rightName rightFields)
      | renderRdrName leftName == renderRdrName rightName ->
          equivalentPatternFields alphaEnv leftFields rightFields
    _ ->
      Nothing

stripPatParens :: HsPatF -> HsPatF
stripPatParens = \case
  PParP innerPattern -> stripPatParens innerPattern
  patternValue -> patternValue

equivalentPatternList :: AlphaEnv -> [HsPatF] -> [HsPatF] -> Maybe AlphaEnv
equivalentPatternList alphaEnv leftPatterns rightPatterns =
  zipExact leftPatterns rightPatterns
    >>= foldM
      (\currentEnv (leftPattern, rightPattern) -> equivalentPattern currentEnv leftPattern rightPattern)
      alphaEnv

equivalentPatternFields ::
  AlphaEnv ->
  [HsRecPatItem] ->
  [HsRecPatItem] ->
  Maybe AlphaEnv
equivalentPatternFields alphaEnv leftItems rightItems =
  zipExact leftItems rightItems
    >>= foldM compareItem alphaEnv
  where
    compareItem ::
      AlphaEnv ->
      (HsRecPatItem, HsRecPatItem) ->
      Maybe AlphaEnv
    compareItem currentEnv = \case
      ( HsRecPatField leftName (HsRecPatExplicit leftPattern),
        HsRecPatField rightName (HsRecPatExplicit rightPattern)
        )
          | renderRdrName leftName == renderRdrName rightName ->
              equivalentPattern currentEnv leftPattern rightPattern
      ( HsRecPatField leftName (HsRecPatPun leftBinder),
        HsRecPatField rightName (HsRecPatPun rightBinder)
        )
          | renderRdrName leftName == renderRdrName rightName ->
              bindPair currentEnv leftBinder rightBinder
      ( HsRecPatWildcard _ leftBinders,
        HsRecPatWildcard _ rightBinders
        ) ->
          zipExact leftBinders rightBinders
            >>= foldM
              (\binderEnv (leftBinder, rightBinder) -> bindPair binderEnv leftBinder rightBinder)
              currentEnv
      _ ->
        Nothing

equivalentBindingGroup ::
  AlphaEnv ->
  [(HsPatF, Pattern HsExprF)] ->
  [(HsPatF, Pattern HsExprF)] ->
  Maybe (AlphaEnv, [(Pattern HsExprF, Pattern HsExprF)])
equivalentBindingGroup alphaEnv leftBindings rightBindings = do
  bindingPairs <- zipExact leftBindings rightBindings
  bindingEnv <-
    foldM
      ( \currentEnv ((leftPattern, _), (rightPattern, _)) ->
          equivalentPattern currentEnv leftPattern rightPattern
      )
      alphaEnv
      bindingPairs
  pure
    ( bindingEnv,
      fmap
        (\((_, leftRhs), (_, rightRhs)) -> (leftRhs, rightRhs))
        bindingPairs
    )

equivalentAlternativeList ::
  AlphaEnv ->
  [(HsPatF, Pattern HsExprF)] ->
  [(HsPatF, Pattern HsExprF)] ->
  Bool
equivalentAlternativeList alphaEnv leftAlternatives rightAlternatives =
  maybe
    False
    (all equivalentAlternative)
    (zipExact leftAlternatives rightAlternatives)
  where
    equivalentAlternative ((leftPattern, leftRhs), (rightPattern, rightRhs)) =
      maybe
        False
        (\rhsEnv -> equivalentExpr rhsEnv leftRhs rightRhs)
        (equivalentPattern alphaEnv leftPattern rightPattern)

equivalentStatements ::
  AlphaEnv ->
  [HsStmtF (Pattern HsExprF)] ->
  [HsStmtF (Pattern HsExprF)] ->
  Maybe AlphaEnv
equivalentStatements alphaEnv leftStatements rightStatements =
  zipExact leftStatements rightStatements >>= foldM equivalentStatement alphaEnv

equivalentStatement ::
  AlphaEnv ->
  (HsStmtF (Pattern HsExprF), HsStmtF (Pattern HsExprF)) ->
  Maybe AlphaEnv
equivalentStatement alphaEnv = \case
  (BindStmtF leftPattern leftExpr, BindStmtF rightPattern rightExpr)
    | equivalentExpr alphaEnv leftExpr rightExpr ->
        equivalentPattern alphaEnv leftPattern rightPattern
  (BodyStmtF leftExpr, BodyStmtF rightExpr)
    | equivalentExpr alphaEnv leftExpr rightExpr ->
        Just alphaEnv
  (LetStmtF leftMode leftBindings, LetStmtF rightMode rightBindings)
    | leftMode == rightMode ->
        equivalentBindingGroup alphaEnv leftBindings rightBindings
          >>= \(bindingEnv, rhsPairs) ->
            if all (uncurry (equivalentExpr bindingEnv)) rhsPairs
              then Just bindingEnv
              else Nothing
  _ ->
    Nothing

equivalentGuardedAlts ::
  AlphaEnv ->
  [GuardedAltF (Pattern HsExprF)] ->
  [GuardedAltF (Pattern HsExprF)] ->
  Bool
equivalentGuardedAlts alphaEnv leftAlts rightAlts =
  maybe False (all equivalentAlt) (zipExact leftAlts rightAlts)
  where
    equivalentAlt (leftAlt, rightAlt) =
      maybe
        False
        (\bodyEnv -> equivalentExpr bodyEnv (gaBody leftAlt) (gaBody rightAlt))
        (equivalentGuards alphaEnv (gaGuards leftAlt) (gaGuards rightAlt))

equivalentGuards ::
  AlphaEnv ->
  [HsGuardStmtF (Pattern HsExprF)] ->
  [HsGuardStmtF (Pattern HsExprF)] ->
  Maybe AlphaEnv
equivalentGuards alphaEnv leftGuards rightGuards =
  zipExact leftGuards rightGuards >>= foldM equivalentGuard alphaEnv

equivalentGuard ::
  AlphaEnv ->
  (HsGuardStmtF (Pattern HsExprF), HsGuardStmtF (Pattern HsExprF)) ->
  Maybe AlphaEnv
equivalentGuard alphaEnv = \case
  (GuardBoolF leftExpr, GuardBoolF rightExpr)
    | equivalentExpr alphaEnv leftExpr rightExpr ->
        Just alphaEnv
  (GuardPatF leftPattern leftExpr, GuardPatF rightPattern rightExpr)
    | equivalentExpr alphaEnv leftExpr rightExpr ->
        equivalentPattern alphaEnv leftPattern rightPattern
  (GuardLetF leftMode leftBindings, GuardLetF rightMode rightBindings)
    | leftMode == rightMode ->
        equivalentBindingGroup alphaEnv leftBindings rightBindings
          >>= \(bindingEnv, rhsPairs) ->
            if all (uncurry (equivalentExpr bindingEnv)) rhsPairs
              then Just bindingEnv
              else Nothing
  _ ->
    Nothing

equivalentClauses ::
  AlphaEnv ->
  [([HsPatF], Pattern HsExprF)] ->
  [([HsPatF], Pattern HsExprF)] ->
  Bool
equivalentClauses alphaEnv leftClauses rightClauses =
  maybe False (all equivalentClause) (zipExact leftClauses rightClauses)
  where
    equivalentClause ((leftPatterns, leftBody), (rightPatterns, rightBody)) =
      maybe
        False
        (\bodyEnv -> equivalentExpr bodyEnv leftBody rightBody)
        (equivalentPatternList alphaEnv leftPatterns rightPatterns)

equivalentExprList ::
  AlphaEnv ->
  [Pattern HsExprF] ->
  [Pattern HsExprF] ->
  Bool
equivalentExprList alphaEnv leftExprs rightExprs =
  maybe
    False
    (all (uncurry (equivalentExpr alphaEnv)))
    (zipExact leftExprs rightExprs)

equivalentOpChainTail ::
  AlphaEnv ->
  [(Pattern HsExprF, Pattern HsExprF)] ->
  [(Pattern HsExprF, Pattern HsExprF)] ->
  Bool
equivalentOpChainTail alphaEnv leftTail rightTail =
  maybe False (all equivalentPair) (zipExact leftTail rightTail)
  where
    equivalentPair ((leftOperator, leftOperand), (rightOperator, rightOperand)) =
      equivalentExpr alphaEnv leftOperator rightOperator
        && equivalentExpr alphaEnv leftOperand rightOperand

equivalentTupleSlots ::
  AlphaEnv ->
  [TupleSlot (Pattern HsExprF)] ->
  [TupleSlot (Pattern HsExprF)] ->
  Bool
equivalentTupleSlots alphaEnv leftSlots rightSlots =
  maybe False (all equivalentSlot) (zipExact leftSlots rightSlots)
  where
    equivalentSlot = \case
      (TupleMissing, TupleMissing) -> True
      (TuplePresent leftExpr, TuplePresent rightExpr) ->
        equivalentExpr alphaEnv leftExpr rightExpr
      _ -> False

equivalentFields ::
  AlphaEnv ->
  [(NormalizedFieldLabel, Pattern HsExprF)] ->
  [(NormalizedFieldLabel, Pattern HsExprF)] ->
  Bool
equivalentFields alphaEnv leftFields rightFields =
  maybe False (all equivalentField) (zipExact leftFields rightFields)
  where
    equivalentField ((leftLabel, leftExpr), (rightLabel, rightExpr)) =
      leftLabel == rightLabel && equivalentExpr alphaEnv leftExpr rightExpr

equivalentArithSeq ::
  AlphaEnv ->
  NormalizedArithSeq (Pattern HsExprF) ->
  NormalizedArithSeq (Pattern HsExprF) ->
  Bool
equivalentArithSeq alphaEnv leftSeq rightSeq =
  case (leftSeq, rightSeq) of
    (ArithSeqFrom leftFrom, ArithSeqFrom rightFrom) ->
      equivalentExpr alphaEnv leftFrom rightFrom
    (ArithSeqFromThen leftFrom leftThen, ArithSeqFromThen rightFrom rightThen) ->
      equivalentExpr alphaEnv leftFrom rightFrom
        && equivalentExpr alphaEnv leftThen rightThen
    (ArithSeqFromTo leftFrom leftTo, ArithSeqFromTo rightFrom rightTo) ->
      equivalentExpr alphaEnv leftFrom rightFrom
        && equivalentExpr alphaEnv leftTo rightTo
    (ArithSeqFromThenTo leftFrom leftThen leftTo, ArithSeqFromThenTo rightFrom rightThen rightTo) ->
      all
        id
        [ equivalentExpr alphaEnv leftFrom rightFrom,
          equivalentExpr alphaEnv leftThen rightThen,
          equivalentExpr alphaEnv leftTo rightTo
        ]
    _ ->
      False

equivalentLit :: NormalizedLit -> NormalizedLit -> Bool
equivalentLit leftLiteral rightLiteral =
  normalizeMultiline leftLiteral == normalizeMultiline rightLiteral
  where
    normalizeMultiline = \case
      NormalizedMultilineString value -> NormalizedString value
      literalValue -> literalValue

zipExact :: [left] -> [right] -> Maybe [(left, right)]
zipExact leftValues rightValues =
  case (leftValues, rightValues) of
    ([], []) ->
      Just []
    (leftValue : remainingLeft, rightValue : remainingRight) ->
      ((leftValue, rightValue) :) <$> zipExact remainingLeft remainingRight
    _ ->
      Nothing
