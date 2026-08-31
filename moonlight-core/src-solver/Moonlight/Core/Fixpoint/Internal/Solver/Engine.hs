-- | The monotone solving engine: arena-based equation evaluation, acyclic and
-- cyclic component solving, widening/narrowing, and the public @solve*@ entry
-- points — the only @runST@ seals over the private arena/queue/bitset.
module Moonlight.Core.Fixpoint.Internal.Solver.Engine
  ( solveMonotone,
    solveDenseMonotone,
    solveIncremental,
  )
where

import Control.Monad (void, when)
import Control.Monad.ST (ST, runST)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector
import Moonlight.Core.Fixpoint.Internal.Solver.Arena qualified as Arena
import Moonlight.Core.Fixpoint.Internal.Solver.Plan
  ( dense,
    equationsForOutput,
    equationsUsingInput,
    validateDeltas,
    validateSnapshot,
  )
import Moonlight.Core.Fixpoint.Internal.Solver.Types
  ( ConvergencePlan (..),
    DeltaDomain (..),
    Equation (..),
    EquationId (..),
    Evaluation,
    OutputUpdate,
    Component (..),
    Obstruction,
    Plan (..),
    Result,
    Snapshot (..),
    WideningPolicy (..),
    equationIdKey,
  )
import Moonlight.Core.Fixpoint.Internal.Solver.WorkQueue qualified as WorkQueue
import Prelude

solveMonotone :: DeltaDomain value delta -> Plan value delta -> Vector value -> Either Obstruction (Result value delta)
solveMonotone domain plan values =
  validateSnapshot plan snapshot
    *> pure (solveFullSnapshot domain plan snapshot)
  where
    snapshot =
      Snapshot values

solveDenseMonotone ::
  DeltaDomain value delta ->
  Int ->
  (Int -> Evaluation value value) ->
  (Int -> value) ->
  Either Obstruction (Result value delta)
solveDenseMonotone domain valueCount evaluate initialValue =
  dense count evaluate
    >>= \plan ->
      pure
        ( runST $ do
            arena <- Arena.new domain snapshot
            traverse_ (solveComponentM domain plan arena) (components plan)
            Arena.toResult arena
        )
  where
    count =
      max 0 valueCount
    snapshot =
      Snapshot (Vector.generate count initialValue)

solveIncremental ::
  DeltaDomain value delta ->
  Plan value delta ->
  Snapshot value delta ->
  IntMap delta ->
  Either Obstruction (Result value delta)
solveIncremental domain plan snapshot deltas =
  validateSnapshot plan snapshot
    *> validateDeltas domain plan deltas
    *> pure
      ( runST $ do
          arena <- Arena.new domain snapshot
          case convergencePlan plan of
            FiniteHeightScc ->
              solveFiniteIncrementalM domain plan arena deltas
            Widening {} ->
              Arena.seed domain arena deltas
                *> traverse_ (solveComponentM domain plan arena) (components plan)
          Arena.toResult arena
      )

solveFullSnapshot :: DeltaDomain value delta -> Plan value delta -> Snapshot value delta -> Result value delta
solveFullSnapshot domain plan snapshot =
  runST $ do
    arena <- Arena.new domain snapshot
    traverse_ (solveComponentM domain plan arena) (components plan)
    Arena.toResult arena
{-# INLINE solveFullSnapshot #-}

solveFiniteIncrementalM ::
  DeltaDomain value delta ->
  Plan value delta ->
  Arena.Arena state value delta ->
  IntMap delta ->
  ST state ()
solveFiniteIncrementalM domain plan arena deltas = do
  queue <- WorkQueue.new (MVector.length (Arena.values arena))
  seedQueued domain arena queue deltas
  WorkQueue.drain queue (evaluatePendingInputM finiteOutputUpdate domain plan arena queue)
{-# INLINE solveFiniteIncrementalM #-}

seedQueued ::
  DeltaDomain value delta ->
  Arena.Arena state value delta ->
  WorkQueue.WorkQueue state ->
  IntMap delta ->
  ST state ()
seedQueued domain arena queue =
  traverse_ (uncurry seedQueuedDelta) . IntMap.toAscList
  where
    seedQueuedDelta key deltaValue
      | deltaNull domain deltaValue = pure ()
      | otherwise = do
          changed <- Arena.seedDeltaM domain arena key deltaValue
          when changed (WorkQueue.enqueue queue key)
{-# INLINE seedQueued #-}

solveComponentM :: DeltaDomain value delta -> Plan value delta -> Arena.Arena state value delta -> Component -> ST state ()
solveComponentM domain plan arena component =
  case component of
    AcyclicOutput output ->
      traverse_ (evaluateFullEquationM domain arena) (equationsForOutput output plan)
    CyclicOutputs outputs ->
      solveCyclicComponentM domain plan arena outputs

solveCyclicComponentM :: DeltaDomain value delta -> Plan value delta -> Arena.Arena state value delta -> IntSet -> ST state ()
solveCyclicComponentM domain plan arena outputs = do
  queue <- WorkQueue.new (MVector.length (Arena.values arena))
  seedCyclicComponentM widenOutput domain plan arena outputs queue
  WorkQueue.drain queue (evaluateCyclicInputM widenOutput domain plan arena outputs queue)
  narrowCyclicComponentM (convergencePlan plan) domain plan arena outputs
  where
    widenOutput =
      convergenceWidenOutput (convergencePlan plan) outputs

finiteOutputUpdate :: OutputUpdate value
finiteOutputUpdate _ _ newValue =
  newValue

convergenceWidenOutput :: ConvergencePlan value -> IntSet -> OutputUpdate value
convergenceWidenOutput convergence outputs =
  case convergence of
    FiniteHeightScc ->
      finiteOutputUpdate
    Widening policy ->
      headedOutputUpdate (IntSet.intersection (wideningHeads policy) outputs) (widenAt policy)

narrowCyclicComponentM ::
  ConvergencePlan value ->
  DeltaDomain value delta ->
  Plan value delta ->
  Arena.Arena state value delta ->
  IntSet ->
  ST state ()
narrowCyclicComponentM convergence domain plan arena outputs =
  case convergence of
    FiniteHeightScc ->
      pure ()
    Widening policy
      | IntSet.null componentHeads ->
          pure ()
      | otherwise -> do
          queue <- WorkQueue.new (MVector.length (Arena.values arena))
          seedCyclicComponentM narrowOutput domain plan arena outputs queue
          WorkQueue.drain queue (evaluateCyclicInputM narrowOutput domain plan arena outputs queue)
      where
        componentHeads =
          IntSet.intersection (wideningHeads policy) outputs
        narrowOutput =
          headedOutputUpdate componentHeads (narrowAt policy)

headedOutputUpdate :: IntSet -> (Int -> value -> value -> value) -> OutputUpdate value
headedOutputUpdate heads update key oldValue newValue
  | IntSet.member key heads =
      update key oldValue newValue
  | otherwise =
      newValue

seedCyclicComponentM ::
  OutputUpdate value ->
  DeltaDomain value delta ->
  Plan value delta ->
  Arena.Arena state value delta ->
  IntSet ->
  WorkQueue.WorkQueue state ->
  ST state ()
seedCyclicComponentM updateOutput domain plan arena outputs queue =
  traverse_ seedOutput (IntSet.toAscList outputs)
  where
    seedOutput outputKey =
      traverse_ seedEquation (equationsForOutput (EquationId outputKey) plan)
    seedEquation equation = do
      changed <- evaluateFullEquationChangedWithM updateOutput domain arena equation
      if changed
        then WorkQueue.enqueue queue (equationIdKey (equationOutput equation))
        else pure ()

evaluateCyclicInputM ::
  OutputUpdate value ->
  DeltaDomain value delta ->
  Plan value delta ->
  Arena.Arena state value delta ->
  IntSet ->
  WorkQueue.WorkQueue state ->
  Int ->
  ST state ()
evaluateCyclicInputM updateOutput domain plan arena componentOutputs queue inputKey = do
  inputDelta <- Arena.takePendingDelta domain arena inputKey
  traverse_ (step inputDelta) relevantEquations
  where
    input = EquationId inputKey
    relevantEquations =
      filter ((`IntSet.member` componentOutputs) . unEquationId . equationOutput) (equationsUsingInput input plan)
    step inputDelta equation = do
      changed <- evaluateEquationForInputM updateOutput domain arena input inputDelta equation
      if changed
        then WorkQueue.enqueue queue (equationIdKey (equationOutput equation))
        else pure ()

evaluatePendingInputM ::
  OutputUpdate value ->
  DeltaDomain value delta ->
  Plan value delta ->
  Arena.Arena state value delta ->
  WorkQueue.WorkQueue state ->
  Int ->
  ST state ()
evaluatePendingInputM updateOutput domain plan arena queue inputKey = do
  inputDelta <- Arena.takePendingDelta domain arena inputKey
  traverse_ (step inputDelta) (equationsUsingInput input plan)
  where
    input =
      EquationId inputKey
    step inputDelta equation = do
      changed <- evaluateEquationForInputM updateOutput domain arena input inputDelta equation
      if changed
        then WorkQueue.enqueue queue (equationIdKey (equationOutput equation))
        else pure ()
{-# INLINE evaluatePendingInputM #-}

evaluateEquationForInputM ::
  OutputUpdate value ->
  DeltaDomain value delta ->
  Arena.Arena state value delta ->
  EquationId ->
  delta ->
  Equation value delta ->
  ST state Bool
evaluateEquationForInputM updateOutput domain arena input inputDelta equation =
  case evaluateDelta equation of
    Just derivative
      | not (deltaNull domain inputDelta) ->
          applyDeltaWithM updateOutput domain arena (equationOutput equation) (derivative input inputDelta)
    _ ->
      evaluateFullEquationChangedWithM updateOutput domain arena equation

evaluateFullEquationM :: DeltaDomain value delta -> Arena.Arena state value delta -> Equation value delta -> ST state ()
evaluateFullEquationM domain arena equation =
  void (evaluateFullEquationChangedM domain arena equation)

evaluateFullEquationChangedM :: DeltaDomain value delta -> Arena.Arena state value delta -> Equation value delta -> ST state Bool
evaluateFullEquationChangedM =
  evaluateFullEquationChangedWithM finiteOutputUpdate

evaluateFullEquationChangedWithM :: OutputUpdate value -> DeltaDomain value delta -> Arena.Arena state value delta -> Equation value delta -> ST state Bool
evaluateFullEquationChangedWithM updateOutput domain arena equation = do
  let outputKey = equationIdKey (equationOutput equation)
  oldValue <- MVector.read (Arena.values arena) outputKey
  newValue <- Arena.evaluate arena (evaluateFull equation)
  applyDeltaWithM updateOutput domain arena (equationOutput equation) (deltaBetween domain oldValue newValue)

applyDeltaWithM :: OutputUpdate value -> DeltaDomain value delta -> Arena.Arena state value delta -> EquationId -> delta -> ST state Bool
applyDeltaWithM updateOutput domain arena (EquationId key) deltaValue
  | deltaNull domain deltaValue = pure False
  | otherwise = do
      oldValue <- MVector.read (Arena.values arena) key
      let candidateValue =
            deltaApply domain deltaValue oldValue
          newValue =
            updateOutput key oldValue candidateValue
          effectiveDelta =
            deltaBetween domain oldValue newValue
      if deltaNull domain effectiveDelta
        then pure False
        else do
          MVector.write (Arena.values arena) key newValue
          Arena.mergePendingDelta domain arena key effectiveDelta
          pure True
