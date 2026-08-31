-- | Solver plan construction from equations: id-range validation, dependency
-- SCC classification, snapshot/delta validation, and plan lookups. Pure.
module Moonlight.Core.Fixpoint.Internal.Solver.Plan
  ( planFromEquations,
    planWithConvergenceFromEquations,
    dense,
    validateSnapshot,
    validateDeltas,
    equationsForOutput,
    equationsUsingInput,
  )
where

import Data.Graph qualified as Graph
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Core.Fixpoint.Internal.Solver.Types
  ( ConvergencePlan (..),
    DeltaDomain (..),
    Equation (..),
    EquationId (..),
    Evaluation,
    Component (..),
    Obstruction (..),
    Plan (..),
    Snapshot (..),
    equationIdKey,
    evaluationInputs,
  )
import Prelude

-- | Build a solver plan over a caller-declared number of value slots.
--
-- Every equation id (output and input) must lie in @[0, valueCount)@;
-- ids outside that range are rejected as obstructions. Dense solving
-- allocates an arena of exactly @valueCount@ slots, so plan memory is
-- the declared capacity, never an inferred maximum id.
planFromEquations :: Foldable container => Int -> container (Equation value delta) -> Either Obstruction (Plan value delta)
planFromEquations =
  planWithConvergenceFromEquations FiniteHeightScc

-- | 'planFromEquations' with an explicit convergence plan; the same
-- id-capacity contract applies.
planWithConvergenceFromEquations ::
  Foldable container =>
  ConvergencePlan value ->
  Int ->
  container (Equation value delta) ->
  Either Obstruction (Plan value delta)
planWithConvergenceFromEquations convergence valueCount equations =
  fromVector convergence valueCount (Vector.fromList (foldr (:) [] equations))

dense ::
  Int ->
  (Int -> Evaluation value value) ->
  Either Obstruction (Plan value delta)
dense valueCount evaluate =
  fromVector FiniteHeightScc count equations
  where
    count =
      max 0 valueCount
    equations =
      Vector.generate count denseEquation
    denseEquation key =
      Equation
        { equationOutput = EquationId key,
          evaluateFull = evaluate key,
          evaluateDelta = Nothing
        }

indexEquationsByOutput :: Vector (Equation value delta) -> IntMap [Equation value delta]
indexEquationsByOutput =
  Vector.foldr insertEquationByOutput IntMap.empty

insertEquationByOutput :: Equation value delta -> IntMap [Equation value delta] -> IntMap [Equation value delta]
insertEquationByOutput equation =
  IntMap.insertWith (<>) (equationIdKey (equationOutput equation)) [equation]

indexUsersByInput :: (Equation value delta -> IntSet) -> Vector (Equation value delta) -> IntMap [Equation value delta]
indexUsersByInput dependenciesOf =
  Vector.foldr (insertEquationByInput dependenciesOf) IntMap.empty

insertEquationByInput :: (Equation value delta -> IntSet) -> Equation value delta -> IntMap [Equation value delta] -> IntMap [Equation value delta]
insertEquationByInput dependenciesOf equation users =
  IntSet.foldr
    (\input -> IntMap.insertWith (<>) input [equation])
    users
    (dependenciesOf equation)

componentsFromEquations :: (Equation value delta -> IntSet) -> Vector (Equation value delta) -> [Component]
componentsFromEquations dependenciesOf equations =
  fmap fromScc $
    Graph.stronglyConnComp
      [ (EquationId output, output, IntSet.toAscList inputs)
        | (output, inputs) <- IntMap.toAscList outputDependencies
      ]
  where
    outputDependencies =
      Vector.foldr (insertEquationDependencies dependenciesOf) IntMap.empty equations
    fromScc (Graph.AcyclicSCC output) = AcyclicOutput output
    fromScc (Graph.CyclicSCC outputs) = CyclicOutputs (IntSet.fromList (fmap unEquationId outputs))

insertEquationDependencies :: (Equation value delta -> IntSet) -> Equation value delta -> IntMap IntSet -> IntMap IntSet
insertEquationDependencies dependenciesOf equation =
  IntMap.insertWith
    IntSet.union
    (equationIdKey (equationOutput equation))
    (dependenciesOf equation)

equationDependencies :: Equation value delta -> IntSet
equationDependencies =
  evaluationInputs . evaluateFull

fromVector :: ConvergencePlan value -> Int -> Vector (Equation value delta) -> Either Obstruction (Plan value delta)
fromVector convergence valueCount equationVector =
  case firstInvalidEquationId capacity equationVector of
    Just obstruction ->
      Left obstruction
    Nothing ->
      Right
        Plan
          { valueCount = capacity,
            convergencePlan = convergence,
            components = componentsFromEquations equationDependencies equationVector,
            equationsByOutput = indexEquationsByOutput equationVector,
            usersByInput = indexUsersByInput equationDependencies equationVector
          }
  where
    capacity =
      max 0 valueCount

firstInvalidEquationId :: Int -> Vector (Equation value delta) -> Maybe Obstruction
firstInvalidEquationId capacity =
  Vector.foldr firstInvalidInEquation Nothing
  where
    firstInvalidInEquation :: Equation value delta -> Maybe Obstruction -> Maybe Obstruction
    firstInvalidInEquation equation found =
      firstInvalidId (equationOutput equation) (firstInvalidInputs (equationDependencies equation) found)
    firstInvalidInputs :: IntSet -> Maybe Obstruction -> Maybe Obstruction
    firstInvalidInputs inputs found =
      IntSet.foldr (\key -> firstInvalidId (EquationId key)) found inputs
    firstInvalidId :: EquationId -> Maybe Obstruction -> Maybe Obstruction
    firstInvalidId equationId@(EquationId key) found
      | key < 0 = Just (NegativeEquationId equationId)
      | key >= capacity = Just (EquationIdExceedsCapacity equationId capacity)
      | otherwise = found

validateSnapshot :: Plan value delta -> Snapshot value delta -> Either Obstruction ()
validateSnapshot plan snapshot
  | snapshotSize /= valueCount plan =
      Left (SnapshotSizeMismatch (valueCount plan) snapshotSize)
  | otherwise =
      Right ()
  where
    snapshotSize =
      Vector.length (snapshotValues snapshot)

validateDeltas :: DeltaDomain value delta -> Plan value delta -> IntMap delta -> Either Obstruction ()
validateDeltas domain plan =
  foldr validateDelta (Right ()) . IntMap.toAscList
  where
    planCapacity =
      valueCount plan
    validateDelta (key, deltaValue) next
      | deltaNull domain deltaValue =
          next
      | key < 0 || key >= planCapacity =
          Left (DeltaOutOfBounds (EquationId key) planCapacity)
      | otherwise =
          next

equationsForOutput :: EquationId -> Plan value delta -> [Equation value delta]
equationsForOutput output plan =
  IntMap.findWithDefault [] (equationIdKey output) (equationsByOutput plan)

equationsUsingInput :: EquationId -> Plan value delta -> [Equation value delta]
equationsUsingInput input plan =
  IntMap.findWithDefault [] (equationIdKey input) (usersByInput plan)
