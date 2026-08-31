module RepairTests
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Delta.Repair
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

data RepairObstruction
  = BelowTarget Int
  | CannotRepair
  deriving stock (Eq, Show)

data RepairCorrectionValue
  = Increment
  | Noop
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "repair"
    [ convergenceTest,
      noProgressBudgetTest,
      budgetExhaustionTest,
      mixedObstructionBudgetTest,
      irreducibleTraceTest,
      foldTraceOrderingTest,
      focusRepairPreservesObstructedFocusStateTest,
      productRepairPreservesInspectedStateTest,
      sequenceRepairShortCircuitsLeftBeforeRightTest,
      sequenceRepairRunsRightAfterLeftConvergesTest,
      productRepairAccumulatesBothObstructionsInOrderTest
    ]

convergenceTest :: TestTree
convergenceTest =
  testCase "bounded repair converges" $
    boundedRepair (incrementKernel 2) (Config 4) 0
      @?= ResultConverged 2 2

noProgressBudgetTest :: TestTree
noProgressBudgetTest =
  testCase "no-op corrections exhaust budget instead of trusting a stable hash" $
    boundedRepair noProgressKernel (Config 2) 0
      @?= ResultBudgetExhausted 0 (BelowTarget 1 :| []) [] 2

budgetExhaustionTest :: TestTree
budgetExhaustionTest =
  testCase "repair reports budget exhaustion with current obstruction" $
    boundedRepair (incrementKernel 3) (Config 1) 0
      @?= ResultBudgetExhausted 1 (BelowTarget 3 :| []) [] 1

mixedObstructionBudgetTest :: TestTree
mixedObstructionBudgetTest =
  testCase "budget exhaustion preserves terminal irreducible obstructions" $
    boundedRepair mixedObstructionKernel (Config 2) 0
      @?= ResultBudgetExhausted 2 (CannotRepair :| [BelowTarget 3]) [CannotRepair] 2

irreducibleTraceTest :: TestTree
irreducibleTraceTest =
  testCase "irreducible obstruction is traced as typed correction" $ do
    let (result, traceValue) = boundedRepairTraced irreducibleKernel (Config 4) 0
    result @?= ResultStuck 0 (CannotRepair :| []) 0
    traceProjection traceValue
      @?= [(CannotRepair :| [], Irreducible CannotRepair :| [], 0, [CannotRepair])]

foldTraceOrderingTest :: TestTree
foldTraceOrderingTest =
  testCase "trace preserves fold order" $
    traceProjection (snd (boundedRepairTraced (incrementKernel 2) (Config 4) 0))
      @?=
        [ (BelowTarget 2 :| [], Applied (BelowTarget 2) Increment :| [], 1, []),
          (BelowTarget 2 :| [], Applied (BelowTarget 2) Increment :| [], 1, [])
        ]

focusRepairPreservesObstructedFocusStateTest :: TestTree
focusRepairPreservesObstructedFocusStateTest =
  testCase "focusRepair embeds obstructed inner state" $
    check (focusRepair snd replaceFocus focusNormalizingKernel) ("outer", 0 :: Int)
      @?= StepObstructed ("outer", 1) (BelowTarget 1 :| [])

productRepairPreservesInspectedStateTest :: TestTree
productRepairPreservesInspectedStateTest =
  testCase "productRepair preserves inspection state instead of returning the original" $
    check (productRepair normalizingConvergedKernel normalizingObstructedKernel) (0 :: Int)
      @?= StepObstructed 11 (Right (BelowTarget 11) :| [])

sequenceRepairShortCircuitsLeftBeforeRightTest :: TestTree
sequenceRepairShortCircuitsLeftBeforeRightTest =
  testCase "SequenceRepairShortCircuitsLeftBeforeRight" $ do
    let (result, traceValue) =
          boundedRepairTraced
            (sequenceRepair leftObstructedIncrementKernel rightKernelMustNotRun)
            (Config 1)
            (0 :: Int)
    result @?= ResultBudgetExhausted 1 (Left (BelowTarget 3) :| []) [] 1
    traceProjection traceValue
      @?= [(Left (BelowTarget 3) :| [], Applied (Left (BelowTarget 3)) (Left Increment) :| [], 1, [])]

sequenceRepairRunsRightAfterLeftConvergesTest :: TestTree
sequenceRepairRunsRightAfterLeftConvergesTest =
  testCase "SequenceRepairRunsRightAfterLeftConverges" $
    check
      (sequenceRepair normalizingConvergedKernel normalizingObstructedKernel)
      (0 :: Int)
      @?= StepObstructed 11 (Right (BelowTarget 11) :| [])

productRepairAccumulatesBothObstructionsInOrderTest :: TestTree
productRepairAccumulatesBothObstructionsInOrderTest =
  testCase "ProductRepairAccumulatesBothObstructionsInOrder" $
    check
      (productRepair normalizingObstructedKernel normalizingObstructedKernel)
      (0 :: Int)
      @?= StepObstructed 20 (Left (BelowTarget 10) :| [Right (BelowTarget 20)])

incrementKernel :: Int -> Kernel Int RepairObstruction RepairCorrectionValue
incrementKernel target =
  Kernel
    { check = \state ->
        if state >= target
          then StepConverged state
          else StepObstructed state (BelowTarget target :| []),
      residuate = \obstruction ->
        case obstruction of
          BelowTarget _ -> Just Increment
          CannotRepair -> Nothing,
      applyKernelCorrection = \state correction ->
        case correction of
          Increment -> state + 1
          Noop -> state
    }

noProgressKernel :: Kernel Int RepairObstruction RepairCorrectionValue
noProgressKernel =
  Kernel
    { check = \state -> StepObstructed state (BelowTarget 1 :| []),
      residuate = const (Just Noop),
      applyKernelCorrection = \state correction ->
        case correction of
          Increment -> state + 1
          Noop -> state
    }

irreducibleKernel :: Kernel Int RepairObstruction RepairCorrectionValue
irreducibleKernel =
  Kernel
    { check = \state -> StepObstructed state (CannotRepair :| []),
      residuate = const Nothing,
      applyKernelCorrection = \state _ -> state
    }

mixedObstructionKernel :: Kernel Int RepairObstruction RepairCorrectionValue
mixedObstructionKernel =
  Kernel
    { check = \state -> StepObstructed state (CannotRepair :| [BelowTarget 3]),
      residuate = \obstruction ->
        case obstruction of
          BelowTarget _ -> Just Increment
          CannotRepair -> Nothing,
      applyKernelCorrection = \state correction ->
        case correction of
          Increment -> state + 1
          Noop -> state
    }

replaceFocus :: (outer, focus) -> focus -> (outer, focus)
replaceFocus (outer, _) focus =
  (outer, focus)

focusNormalizingKernel :: Kernel Int RepairObstruction RepairCorrectionValue
focusNormalizingKernel =
  Kernel
    { check = \focus -> StepObstructed (focus + 1) (BelowTarget 1 :| []),
      residuate = const Nothing,
      applyKernelCorrection = \focus _ -> focus
    }

normalizingConvergedKernel :: Kernel Int RepairObstruction RepairCorrectionValue
normalizingConvergedKernel =
  Kernel
    { check = \state -> StepConverged (state + 1),
      residuate = const Nothing,
      applyKernelCorrection = \state _ -> state
    }

normalizingObstructedKernel :: Kernel Int RepairObstruction RepairCorrectionValue
normalizingObstructedKernel =
  Kernel
    { check = \state -> StepObstructed (state + 10) (BelowTarget (state + 10) :| []),
      residuate = const Nothing,
      applyKernelCorrection = \state _ -> state
    }

leftObstructedIncrementKernel :: Kernel Int RepairObstruction RepairCorrectionValue
leftObstructedIncrementKernel =
  Kernel
    { check = \state -> StepObstructed state (BelowTarget 3 :| []),
      residuate = \obstruction ->
        case obstruction of
          BelowTarget _ -> Just Increment
          CannotRepair -> Nothing,
      applyKernelCorrection = \state correction ->
        case correction of
          Increment -> state + 1
          Noop -> state
    }

rightKernelMustNotRun :: Kernel Int RepairObstruction RepairCorrectionValue
rightKernelMustNotRun =
  Kernel
    { check = \state -> StepObstructed state (CannotRepair :| []),
      residuate = const Nothing,
      applyKernelCorrection = \state _ -> state
    }

traceProjection ::
  Trace obstruction correction ->
  [(NonEmpty obstruction, NonEmpty (Correction obstruction correction), Natural, [obstruction])]
traceProjection =
  fmap (\roundValue -> (obstructions roundValue, corrections roundValue, applied roundValue, irreducible roundValue)) . rounds
