module Moonlight.Homology.Pure.Constraint.Algebra
  ( evaluateTopologicalConstraint,
    evaluateTopologicalConstraints,
  )
where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Moonlight.Homology.Pure.Chain
  ( EulerCharacteristic (..),
    HomologicalDegree (..),
    PersistencePair (..),
    TopologyWitness (..),
  )
import Moonlight.Homology.Pure.Constraint.Core
import Moonlight.Homology.Pure.FiniteAbelian
  ( finiteAbelianCardinality,
    finiteAbelianCyclicSummandMultiplicity,
    finiteAbelianExactOrderElementCount,
    finiteAbelianFilteredCardinality,
    finiteAbelianSummandCount,
    isPrime,
    matchesOptional,
    normalizeTorsionOrders,
  )
import Moonlight.Homology.Pure.Graded.Query
  ( degreeSelectionFromMaybe,
    preserveDegreewiseQuery,
    selectAllDegrees,
    selectDegree,
  )
import Moonlight.Homology.Pure.GradedTorsion
  ( GradedTorsionFamily,
    gradedTorsionAtDegree,
    gradedTorsionCombined,
    gradedTorsionOrderSupport,
    gradedTorsionPresent,
    gradedTorsionPrimaryOrderSupport,
  )
import Moonlight.Homology.Pure.Filtration (FiltrationValue (..))
import Moonlight.Homology.Pure.Skeleton
  ( SkeletonSignature (..),
    skeletonSignatureWithinTolerance,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold (MacroScaffoldIR)
import Moonlight.Homology.Pure.Topology.ScaffoldSummary (mkMacroScaffoldTopologyView)
import Moonlight.Homology.Pure.TopologyObserver
  ( observeBettiVector,
    observeCoefficientRepresentativeCycleCount,
    observeEulerCharacteristic,
    observeExactRepresentativeClassCount,
    observeHarmonicCount,
    observePersistencePairs,
    observeScaffoldSummary,
    observeTorsionFamily,
    runTopologyObserver,
  )

evaluateTopologicalConstraint ::
  TopologyWitness MacroScaffoldIR spectral FiltrationValue coefficient basis ->
  TopologicalConstraint ->
  [TopologicalViolation]
evaluateTopologicalConstraint witnessValue constraintValue =
  let topologyView = mkMacroScaffoldTopologyView witnessValue
      torsionFamily = runTopologyObserver observeTorsionFamily topologyView
   in case constraintValue of
    TargetBettiConstraint targetValue ->
      let observedBetti = runTopologyObserver observeBettiVector topologyView
       in if observedBetti == targetBettiVector targetValue
        then []
        else [BettiViolation targetValue observedBetti]
    PersistenceBudgetConstraint budgetValue ->
      let observedCount =
            countPersistentFeatures
              budgetValue
              (runTopologyObserver
                (observePersistencePairs (degreeSelectionFromMaybe (persistenceBudgetDegree budgetValue)))
                topologyView)
       in checkBound (persistenceBudgetCountBound budgetValue) observedCount
            (PersistenceBudgetViolation budgetValue observedCount)
    EulerBoundConstraint eulerBoundValue ->
      case runTopologyObserver observeEulerCharacteristic topologyView of
        Nothing -> [EulerWitnessMissing eulerBoundValue]
        Just observedEuler ->
          checkBound (requiredEulerBound eulerBoundValue) (unEulerCharacteristic observedEuler)
            (EulerBoundViolation eulerBoundValue observedEuler)
    LoopRoleConstraint loopRoleValue ->
      let degreeSelectionValue = selectDegree (loopTargetDegree loopRoleValue)
          observedCount =
            if runTopologyObserver (observeExactRepresentativeClassCount selectAllDegrees) topologyView == 0
              then
                runTopologyObserver (observeCoefficientRepresentativeCycleCount degreeSelectionValue) topologyView
              else
                runTopologyObserver (observeExactRepresentativeClassCount degreeSelectionValue) topologyView
       in checkBound (loopCountBound loopRoleValue) observedCount
            (LoopRoleViolation loopRoleValue observedCount)
    RequireTorsionInvariantConstraint torsionConstraintValue ->
      case gradedTorsionAtDegree
        (requiredTorsionDegree torsionConstraintValue)
        torsionFamily of
        Nothing ->
          [IntegralHomologyWitnessMissing constraintValue]
        Just torsionValue ->
          let observedCount =
                torsionValue
                  & finiteAbelianCyclicSummandMultiplicity
                    (requiredTorsionInvariant torsionConstraintValue)
           in checkBound (requiredTorsionMultiplicity torsionConstraintValue) observedCount
                (RequireTorsionInvariantViolation torsionConstraintValue observedCount)
    RequireElementOrderConstraint elementConstraintValue ->
      withTorsionPresent constraintValue torsionFamily $ \_ ->
        let observedCount =
              gradedTorsionCombined
                (selectDegree (requiredElementDegree elementConstraintValue))
                torsionFamily
                & finiteAbelianExactOrderElementCount
                  (requiredElementOrder elementConstraintValue)
         in checkBound (requiredElementMultiplicity elementConstraintValue) observedCount
              (RequireElementOrderViolation elementConstraintValue observedCount)
    RequireOrderSupportConstraint supportConstraintValue ->
      withTorsionPresent constraintValue torsionFamily $ \_ ->
        let observedSupport =
              gradedTorsionOrderSupport
                (preserveDegreewiseQuery (degreeSelectionFromMaybe (requiredOrderSupportDegree supportConstraintValue)))
                torsionFamily
            missingOrders =
              requiredSupportedOrders supportConstraintValue
                & normalizeTorsionOrders
                & filter (`notElem` observedSupport)
            forbiddenOrders =
              requiredForbiddenOrders supportConstraintValue
                & normalizeTorsionOrders
                & filter (`elem` observedSupport)
         in if null missingOrders && null forbiddenOrders
              then []
              else [RequireOrderSupportViolation supportConstraintValue missingOrders forbiddenOrders]
    PrimaryOrderSupportBudgetConstraint budgetValue ->
      if not (isPrime (primarySupportBudgetPrime budgetValue))
        then [InvalidPrimaryPrime constraintValue (primarySupportBudgetPrime budgetValue)]
        else
          withTorsionPresent constraintValue torsionFamily $ \_ ->
            let observedCount =
                  maybe [] id
                    (gradedTorsionPrimaryOrderSupport
                    (primarySupportBudgetPrime budgetValue)
                    (preserveDegreewiseQuery (degreeSelectionFromMaybe (primarySupportBudgetDegree budgetValue)))
                    torsionFamily)
                    & length
                    & toInteger
             in checkBound (primarySupportBudgetBound budgetValue) observedCount
                  (PrimaryOrderSupportBudgetViolation budgetValue observedCount)
    RequirePrimaryOrderSupportConstraint supportConstraintValue ->
      if not (isPrime (requiredPrimarySupportPrime supportConstraintValue))
        then [InvalidPrimaryPrime constraintValue (requiredPrimarySupportPrime supportConstraintValue)]
        else
          withTorsionPresent constraintValue torsionFamily $ \_ ->
            let observedSupport =
                  maybe [] id
                    (gradedTorsionPrimaryOrderSupport
                    (requiredPrimarySupportPrime supportConstraintValue)
                    (preserveDegreewiseQuery (degreeSelectionFromMaybe (requiredPrimarySupportDegree supportConstraintValue)))
                    torsionFamily)
                missingOrders =
                  requiredPrimarySupportedOrders supportConstraintValue
                    & normalizeTorsionOrders
                    & filter (`notElem` observedSupport)
                forbiddenOrders =
                  requiredPrimaryForbiddenOrders supportConstraintValue
                    & normalizeTorsionOrders
                    & filter (`elem` observedSupport)
             in if null missingOrders && null forbiddenOrders
                  then []
                  else [RequirePrimaryOrderSupportViolation supportConstraintValue missingOrders forbiddenOrders]
    TorsionBudgetConstraint budgetValue ->
      withTorsionPresent constraintValue torsionFamily $ \_ ->
        let observedCount =
              observeTorsionBudget
                (torsionBudgetDegree budgetValue)
                (torsionBudgetOrder budgetValue)
                (torsionBudgetMeasure budgetValue)
                torsionFamily
         in checkBound (torsionBudgetBound budgetValue) observedCount
              (TorsionBudgetViolation budgetValue observedCount)
    RequireCyclicOrderConstraint cyclicOrderValue ->
      withTorsionPresent constraintValue torsionFamily $ \_ ->
        case gradedTorsionAtDegree
          (requiredCyclicDegree cyclicOrderValue)
          torsionFamily of
          Nothing ->
            [IntegralHomologyWitnessMissing constraintValue]
          Just torsionValue ->
            let observedCount =
                  torsionValue
                    & finiteAbelianCyclicSummandMultiplicity
                      (requiredCyclicOrder cyclicOrderValue)
             in checkBound (requiredCyclicMultiplicity cyclicOrderValue) observedCount
                  (RequireCyclicOrderViolation cyclicOrderValue observedCount)
    SingularityBudgetConstraint singularityBudgetValue ->
      case runTopologyObserver observeScaffoldSummary topologyView of
        Nothing -> [MacroScaffoldMissing constraintValue]
        Just scaffoldSummaryValue ->
          singularityBounds singularityBudgetValue
            & Map.toList
            >>= ( \(criticalKindValue, boundValue) ->
                    let observedCount =
                          signatureCriticalCounts scaffoldSummaryValue
                            & Map.findWithDefault 0 criticalKindValue
                     in checkBound boundValue observedCount
                          (SingularityBudgetViolation criticalKindValue boundValue observedCount)
               )
    HarmonicLoopBudgetConstraint harmonicBudgetValue ->
      let observedCount =
            runTopologyObserver
              (observeHarmonicCount (selectDegree (harmonicLoopDegree harmonicBudgetValue)))
              topologyView
       in checkBound (harmonicLoopCountBound harmonicBudgetValue) observedCount
            (HarmonicLoopBudgetViolation harmonicBudgetValue observedCount)
    SkeletonAdherenceConstraint adherenceValue ->
      case runTopologyObserver observeScaffoldSummary topologyView of
        Nothing -> [MacroScaffoldMissing constraintValue]
        Just scaffoldSummaryValue ->
          if skeletonWithinTolerance adherenceValue scaffoldSummaryValue
                then []
                else [SkeletonAdherenceViolation adherenceValue scaffoldSummaryValue]

evaluateTopologicalConstraints ::
  TopologyWitness MacroScaffoldIR spectral FiltrationValue coefficient basis ->
  [TopologicalConstraint] ->
  [TopologicalViolation]
evaluateTopologicalConstraints witnessValue =
  foldMap (evaluateTopologicalConstraint witnessValue)

countPersistentFeatures :: PersistenceBudget -> [PersistencePair FiltrationValue] -> Int
countPersistentFeatures budgetValue persistencePairsValue =
  persistencePairsValue
    & filter (meetsLifetimeBudget (persistenceBudgetMinimumLifetime budgetValue))
    & length

meetsLifetimeBudget :: FiltrationValue -> PersistencePair FiltrationValue -> Bool
meetsLifetimeBudget minimumLifetime pairValue =
  case persistenceDeath pairValue of
    Nothing -> True
    Just deathValue ->
      filtrationDifference deathValue (persistenceBirth pairValue) >= unFiltrationValue minimumLifetime

filtrationDifference :: FiltrationValue -> FiltrationValue -> Double
filtrationDifference endValue startValue =
  unFiltrationValue endValue - unFiltrationValue startValue

observeTorsionBudget ::
  Maybe HomologicalDegree ->
  Maybe Integer ->
  TorsionBudgetMeasure ->
  GradedTorsionFamily ->
  Integer
observeTorsionBudget degreeConstraint orderConstraint measure torsionFamily =
  let selectionValue = degreeSelectionFromMaybe degreeConstraint
      observedTorsion =
        gradedTorsionCombined selectionValue torsionFamily
   in case measure of
        TorsionSummandCount ->
          finiteAbelianSummandCount orderConstraint observedTorsion
        TorsionTotalCardinality ->
          finiteAbelianFilteredCardinality orderConstraint observedTorsion
        TorsionElementOrderCount ->
          case orderConstraint of
            Nothing -> finiteAbelianCardinality observedTorsion
            Just orderValue ->
              finiteAbelianExactOrderElementCount orderValue observedTorsion
        TorsionOrderSupportCount ->
          gradedTorsionOrderSupport (preserveDegreewiseQuery selectionValue) torsionFamily
            & filter (matchesOptional (fmap abs orderConstraint) . abs)
            & length
            & toInteger

withTorsionPresent ::
  TopologicalConstraint ->
  GradedTorsionFamily ->
  (GradedTorsionFamily -> [TopologicalViolation]) ->
  [TopologicalViolation]
withTorsionPresent constraintValue torsionFamily evaluator =
  if gradedTorsionPresent torsionFamily
    then evaluator torsionFamily
    else [IntegralHomologyWitnessMissing constraintValue]

skeletonWithinTolerance :: SkeletonAdherence -> SkeletonSignature -> Bool
skeletonWithinTolerance adherenceValue observedSignature =
  skeletonSignatureWithinTolerance
    (skeletonTolerance adherenceValue)
    (skeletonTargetSignature adherenceValue)
    observedSignature
