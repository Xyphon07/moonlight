module FixpointSpec (tests) where

import Moonlight.Core
  ( ConvergencePlan (..),
    DeltaDomain (..),
    Equation (..),
    EquationId (..),
    Evaluation,
    FixpointDivergence (..),
    Obstruction (..),
    Result,
    Snapshot (..),
    WideningPolicy (..),
    fixpointBounded,
    fixpointBoundedM,
    queueFromList,
    reachabilityFromInt,
    reschedulingWorklistFoldIntSet,
    solveIncremental,
    solveDenseMonotone,
    solveMonotone,
    planFromEquations,
    planWithConvergenceFromEquations,
    resultSnapshot,
    resultValues,
    readEquationValue,
    traverseOnceIntSet,
    worklistFold,
  )
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as UVector
import Moonlight.Core
  ( Edge (..),
    csrFromRows,
    csrOffsets,
    csrTargets,
    csrTranspose,
    deleteSnapshotEdge,
    sccClosureCacheFor,
    frozenDigraphFromSuccessors,
    frozenReachabilityFrom,
    frozenReachabilityWithCache,
    frozenReachabilityWithPolicy,
    graphForward,
    snapshotFromFrozen,
    snapshotReachabilityFrom,
    insertSnapshotEdge,
    mkReachabilityPolicy,
  )
import Data.IntSet qualified as IntSet
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "bounded fixpoint"
    [ testCase "fixpointBounded reports the last pre-exhaustion state" $
        fixpointBounded 2 decreaseToZero (5 :: Int)
          @?= Left (FixpointDivergence 2 3),
      testCase "fixpointBounded returns a stable value when it converges in budget" $
        fixpointBounded 5 decreaseToZero (3 :: Int)
          @?= Right 0,
      testCase "fixpointBoundedM returns typed divergence through monadic steps" $
        (fixpointBoundedM 2 (Right . decreaseToZero) (5 :: Int) :: Either String (Either (FixpointDivergence Int) Int))
          @?= Right (Left (FixpointDivergence 2 3)),
      testCase "worklistFold preserves FIFO frontier order" $
        reverse
          (worklistFold step [] (queueFromList [0 :: Int]))
          @?= [0, 1, 2],
      testCase "traverseOnceIntSet processes each key once through a fresh frontier" $
        traverseOnceIntSet intSetStep [] (IntSet.singleton 0)
          @?= [2, 1, 0],
      testCase "reschedulingWorklistFoldIntSet reruns a dequeued key" $
        IntMap.findWithDefault 0 0 (reschedulingWorklistFoldIntSet rescheduleSelf IntMap.empty (IntSet.singleton 0))
          @?= 2,
      testCase "reschedulingWorklistFoldIntSet deduplicates keys while queued" $
        reverse (reschedulingWorklistFoldIntSet enqueueSharedKey [] (IntSet.fromList [0, 2]))
          @?= [0, 2, 1],
      testCase "solveMonotone propagates through a cyclic monotone component" $
        fmap resultValues (solveCyclicPropagation initialCycleValues)
          @?= Right propagatedCycleValues,
      testCase "solveMonotone full snapshot evaluates a chain after dependencies" $
        assertFullSnapshotDependencyClosure 3 chainDependencySuccessors,
      testCase "solveMonotone full snapshot evaluates a diamond after dependencies" $
        assertFullSnapshotDependencyClosure 4 diamondDependencySuccessors,
      testCase "plan construction rejects an equation id at or beyond the declared capacity" $
        fmap
          resultValues
          ( planFromEquations 1 cyclicPropagationEquations
              >>= \plan -> solveMonotone intSetDeltaDomain plan initialCycleValues
          )
          @?= Left (EquationIdExceedsCapacity (EquationId 1) 1),
      testCase "solver snapshots must exactly match plan capacity" $
        case planFromEquations 1 ([] :: [Equation Int Int]) of
          Left obstruction ->
            assertFailure ("empty exact-capacity plan failed: " <> show obstruction)
          Right plan ->
            ( fmap resultValues (solveMonotone intGrowthDeltaDomain plan Vector.empty),
              fmap resultValues (solveMonotone intGrowthDeltaDomain plan (Vector.fromList [0, 1]))
            )
              @?= (Left (SnapshotSizeMismatch 1 0), Left (SnapshotSizeMismatch 1 2)),
      testCase "solver deltas are bounded by plan capacity" $
        case planFromEquations 1 ([] :: [Equation Int Int]) of
          Left obstruction ->
            assertFailure ("empty exact-capacity plan failed: " <> show obstruction)
          Right plan ->
            fmap resultValues
              (solveIncremental intGrowthDeltaDomain plan (Snapshot (Vector.singleton 0)) (IntMap.singleton 1 5))
              @?= Left (DeltaOutOfBounds (EquationId 1) 1),
      testCase "dense plans reject reads outside their declared capacity" $
        fmap resultValues
          (solveDenseMonotone intGrowthDeltaDomain 1 (const (readEquationValue (EquationId 1))) (const 0))
          @?= Left (EquationIdExceedsCapacity (EquationId 1) 1),
      testCase "solveIncremental reuses a snapshot and propagates new deltas through the cycle" $
        case planFromEquations 2 cyclicPropagationEquations of
          Left obstruction ->
            assertFailure ("cyclic propagation plan failed: " <> show obstruction)
          Right plan ->
            case solveMonotone intSetDeltaDomain plan initialCycleValues of
              Left obstruction ->
                assertFailure ("initial cyclic solve failed: " <> show obstruction)
              Right firstResult ->
                fmap resultValues
                  ( solveIncremental
                      intSetDeltaDomain
                      plan
                      (resultSnapshot firstResult)
                      (IntMap.singleton 0 (IntSet.singleton 7))
                  )
                  @?= Right incrementalCycleValues,
      testCase "solveIncremental agrees with full recomputation after input growth" $
        case planFromEquations 2 cyclicPropagationEquations of
          Left obstruction ->
            assertFailure ("cyclic propagation plan failed: " <> show obstruction)
          Right plan ->
            case solveMonotone intSetDeltaDomain plan initialCycleValues of
              Left obstruction ->
                assertFailure ("initial cyclic solve failed: " <> show obstruction)
              Right firstResult ->
                fmap resultValues
                  ( solveIncremental
                      intSetDeltaDomain
                      plan
                      (resultSnapshot firstResult)
                      (IntMap.singleton 0 (IntSet.singleton 7))
                  )
                  @?= fmap resultValues (solveMonotone intSetDeltaDomain plan fullRecomputeCycleValues),
      testCase "evaluation reads author the dependency plan used by incremental solving" $
        case planFromEquations 2 [derivedDependencyEquation] of
          Left obstruction ->
            assertFailure ("derived dependency plan failed: " <> show obstruction)
          Right plan ->
            fmap resultValues
              ( solveIncremental
                  intGrowthDeltaDomain
                  plan
                  (Snapshot (Vector.fromList [0, 0]))
                  (IntMap.singleton 0 7)
              )
              @?= fmap resultValues
                (solveMonotone intGrowthDeltaDomain plan (Vector.fromList [7, 0])),
      testCase "solveIncremental uses derivatives to propagate through acyclic users" $
        case planFromEquations 3 acyclicDerivativeEquations of
          Left obstruction ->
            assertFailure ("acyclic derivative plan failed: " <> show obstruction)
          Right plan ->
            fmap resultValues
              ( solveIncremental
                  intSetDeltaDomain
                  plan
                  (Snapshot solvedAcyclicDerivativeValues)
                  (IntMap.singleton 0 (IntSet.singleton 7))
              )
              @?= Right incrementedAcyclicDerivativeValues,
      testCase "solveIncremental ignores saturated external seed deltas" $
        case planFromEquations 2 [cappedSeedPropagationEquation] of
          Left obstruction ->
            assertFailure ("capped seed propagation plan failed: " <> show obstruction)
          Right plan ->
            fmap resultValues
              ( solveIncremental
                  cappedIntDeltaDomain
                  plan
                  (Snapshot cappedSeedSnapshotValues)
                  (IntMap.singleton 0 5)
              )
              @?= Right cappedSeedSnapshotValues,
      testCase "Widening applies the explicit widening head callback" $
        case planWithConvergenceFromEquations wideningPlan 1 [wideningGrowthEquation] of
          Left obstruction ->
            assertFailure ("widening plan failed: " <> show obstruction)
          Right plan ->
            fmap resultValues
              (solveMonotone intGrowthDeltaDomain plan (Vector.singleton 0))
              @?= Right (Vector.singleton 100),
      testCase "CSR construction consumes only its declared row prefix" $
        let csr = csrFromRows 2 ([[1], [0]] <> error "discarded CSR tail was forced")
         in (csrOffsets csr, csrTargets csr)
              @?= (UVector.fromList [0, 1, 2], UVector.fromList [1, 0]),
      testCase "CSR construction pads short inputs and ignores long suffixes" $
        let shortCsr = csrFromRows 3 [[2]]
            longCsr = csrFromRows 2 [[1], [0], [99]]
         in (csrOffsets shortCsr, csrTargets shortCsr, csrOffsets longCsr, csrTargets longCsr)
              @?= (UVector.fromList [0, 1, 1, 1], UVector.singleton 2, UVector.fromList [0, 1, 2], UVector.fromList [1, 0]),
      testCase "CSR construction deduplicates and sorts row targets" $
        let csr = csrFromRows 1 [[3, 1, 3, 2]]
         in csrTargets csr @?= UVector.fromList [1, 2, 3],
      testCase "CSR transpose reverses every edge and retains isolated vertices" $
        let transposed =
              csrTranspose
                (graphForward (frozenDigraphFromSuccessors 5 transposeFixtureSuccessors))
         in (csrOffsets transposed, csrTargets transposed)
              @?= (UVector.fromList [0, 1, 3, 3, 5, 5], UVector.fromList [3, 0, 4, 0, 1]),
      testCase "CSR transpose is involutive after ingress normalization" $
        let forward =
              graphForward (frozenDigraphFromSuccessors 3 normalizedTransposeFixtureSuccessors)
         in csrTranspose (csrTranspose forward) @?= forward,
      testCase "CSR transpose is total for empty and isolated graphs" $
        let emptyForward = graphForward (frozenDigraphFromSuccessors 0 (const IntSet.empty))
            isolatedForward = graphForward (frozenDigraphFromSuccessors 3 (const IntSet.empty))
         in (csrTranspose emptyForward, csrTranspose isolatedForward)
              @?= (emptyForward, isolatedForward),
      testCase "CSR reachability agrees with IntSet closure through an SCC" $
        frozenReachabilityFrom (frozenDigraphFromSuccessors 4 graphStep) (IntSet.singleton 0)
          @?= reachabilityFromInt graphStep (IntSet.singleton 0),
      testCase "CSR reachability agrees with IntSet closure across topology and seed matrix" $
        traverse_ assertReachabilityCase reachabilityCases,
      testCase "CSR policy variants and hot SCC cache preserve cold reachability" $
        assertPolicyAndCacheReachabilityCase,
      testCase "SCC caches are constructed for exactly one frozen graph" $
        assertSccCacheGraphOwnership,
      testCase "graph snapshot semantic no-ops preserve the original snapshot" $
        assertGraphSnapshotNoOps,
      testCase "graph snapshot overlay and tombstones agree with IntSet closure" $
        snapshotReachabilityFrom editedSnapshot (IntSet.singleton 0)
          @?= reachabilityFromInt editedGraphStep (IntSet.singleton 0)
    ]
  where
    step :: [Int] -> Int -> ([Int], [Int])
    step seen current =
      ( current : seen,
        case current of
          0 -> [1, 2]
          _ -> []
      )

    intSetStep :: [Int] -> Int -> ([Int], IntSet.IntSet)
    intSetStep seen current =
      ( current : seen,
        case current of
          0 -> IntSet.fromList [0, 1]
          1 -> IntSet.fromList [0, 2]
          _ -> IntSet.empty
      )

    rescheduleSelf :: IntMap.IntMap Int -> Int -> (IntMap.IntMap Int, IntSet.IntSet)
    rescheduleSelf visits current =
      ( nextVisits,
        if current == 0 && visitCount == 0
          then IntSet.singleton 0
          else IntSet.empty
      )
      where
        visitCount =
          IntMap.findWithDefault 0 current visits
        nextVisits =
          IntMap.insert current (visitCount + 1) visits

    enqueueSharedKey :: [Int] -> Int -> ([Int], IntSet.IntSet)
    enqueueSharedKey seen current =
      ( current : seen,
        case current of
          0 -> IntSet.singleton 1
          2 -> IntSet.singleton 1
          _ -> IntSet.empty
      )

    graphStep :: Int -> IntSet.IntSet
    graphStep current =
      case current of
        0 -> IntSet.singleton 1
        1 -> IntSet.fromList [0, 2]
        2 -> IntSet.singleton 3
        _ -> IntSet.empty

    transposeFixtureSuccessors :: Int -> IntSet.IntSet
    transposeFixtureSuccessors source =
      case source of
        0 -> IntSet.fromList [1, 3]
        1 -> IntSet.singleton 3
        3 -> IntSet.singleton 0
        4 -> IntSet.singleton 1
        _ -> IntSet.empty

    normalizedTransposeFixtureSuccessors :: Int -> IntSet.IntSet
    normalizedTransposeFixtureSuccessors source =
      case source of
        0 -> IntSet.fromList [-1, 1, 1, 3]
        1 -> IntSet.singleton 2
        2 -> IntSet.singleton 0
        _ -> IntSet.empty

    assertReachabilityCase (vertexCount, seeds, expand) =
      frozenReachabilityFrom (frozenDigraphFromSuccessors vertexCount expand) seeds
        @?= reachabilityFromInt expand seeds

    assertPolicyAndCacheReachabilityCase =
      case (mkReachabilityPolicy 1.0e100 1.0e100 maxBound, mkReachabilityPolicy 0 0 0) of
        (Right sparsePushOnlyPolicy, Right densePullBiasedPolicy) ->
          let graph =
                frozenDigraphFromSuccessors 96 (sccHeavySuccessors 96)
              seeds =
                IntSet.fromList [0, 8, 16]
              cold =
                frozenReachabilityFrom graph seeds
              sparse =
                frozenReachabilityWithPolicy sparsePushOnlyPolicy graph seeds
              dense =
                frozenReachabilityWithPolicy densePullBiasedPolicy graph seeds
              coldCache =
                sccClosureCacheFor graph
              (firstCached, cache1) =
                frozenReachabilityWithCache coldCache seeds
              (hotCached, _cache2) =
                frozenReachabilityWithCache cache1 seeds
           in (sparse, dense, firstCached, hotCached) @?= (cold, cold, cold, cold)
        (Left policyError, _) ->
          assertFailure ("sparse policy construction failed: " <> show policyError)
        (_, Left policyError) ->
          assertFailure ("dense policy construction failed: " <> show policyError)

    assertSccCacheGraphOwnership =
      let seeds =
            IntSet.singleton 1
          graphWithEdge =
            frozenDigraphFromSuccessors 2 graphWithBackwardEdge
          graphWithoutEdge =
            frozenDigraphFromSuccessors 2 (const IntSet.empty)
          cacheWithEdge =
            sccClosureCacheFor graphWithEdge
          (_firstReachable, warmedCache) =
            frozenReachabilityWithCache cacheWithEdge seeds
          (_hotReachable, hotCache) =
            frozenReachabilityWithCache warmedCache seeds
          cacheWithoutEdge =
            sccClosureCacheFor graphWithoutEdge
          (withoutEdgeReachable, _coldCache) =
            frozenReachabilityWithCache cacheWithoutEdge seeds
          (withEdgeReachable, _stillBoundCache) =
            frozenReachabilityWithCache hotCache seeds
       in (withoutEdgeReachable, withEdgeReachable)
            @?= (frozenReachabilityFrom graphWithoutEdge seeds, frozenReachabilityFrom graphWithEdge seeds)

    graphWithBackwardEdge :: Int -> IntSet.IntSet
    graphWithBackwardEdge current =
      case current of
        1 -> IntSet.singleton 0
        _ -> IntSet.empty

    assertGraphSnapshotNoOps =
      let graph =
            frozenDigraphFromSuccessors 2 graphWithBackwardEdge
          snapshot =
            snapshotFromFrozen graph
       in ( insertSnapshotEdge (Edge 1 0) snapshot,
            insertSnapshotEdge (Edge 0 2) snapshot,
            deleteSnapshotEdge (Edge 0 1) snapshot,
            deleteSnapshotEdge (Edge (-1) 0) snapshot
          )
            @?= (snapshot, snapshot, snapshot, snapshot)

    reachabilityCases =
      [ (16, IntSet.singleton 0, chainSuccessors 16),
        (32, IntSet.fromList [0, 3, 7], fanoutSuccessors 4 32),
        (36, IntSet.fromList [0, 5], gridSuccessors 6 36),
        (64, IntSet.singleton 0, powerLawLikeSuccessors 64),
        (48, IntSet.fromList [0, 16, 32], sccHeavySuccessors 48)
      ]

    chainSuccessors size current =
      IntSet.fromAscList (filter (< size) [current + 1])

    fanoutSuccessors degree size current =
      IntSet.fromAscList (filter (< size) [current + 1 .. current + degree])

    gridSuccessors width size current =
      IntSet.fromList (filter (< size) (rightNeighbor <> downNeighbor))
      where
        rightNeighbor =
          [current + 1 | current `mod` width /= width - 1]
        downNeighbor =
          [current + width]

    powerLawLikeSuccessors size current =
      IntSet.fromList (filter (< size) (fmap (current +) (takeWhile (< size) powersOfTwo)))
      where
        powersOfTwo =
          take (1 + current `mod` 6) (iterate (* 2) 1)

    sccHeavySuccessors size current =
      IntSet.fromList (filter (< size) [cycleNeighbor, current + componentSize])
      where
        componentSize =
          8
        componentStart =
          current - current `mod` componentSize
        cycleNeighbor =
          componentStart + ((current + 1) `mod` componentSize)

    editedSnapshot =
      deleteSnapshotEdge (Edge 1 2) $
        insertSnapshotEdge (Edge 4 5) $
          insertSnapshotEdge (Edge 0 4) $
            snapshotFromFrozen (frozenDigraphFromSuccessors 6 editedBaseGraphStep)

    editedBaseGraphStep :: Int -> IntSet.IntSet
    editedBaseGraphStep current =
      case current of
        0 -> IntSet.singleton 1
        1 -> IntSet.singleton 2
        2 -> IntSet.singleton 3
        _ -> IntSet.empty

    editedGraphStep :: Int -> IntSet.IntSet
    editedGraphStep current =
      case current of
        0 -> IntSet.fromList [1, 4]
        4 -> IntSet.singleton 5
        _ -> IntSet.empty

decreaseToZero :: Int -> Int
decreaseToZero current =
  if current <= 0
    then 0
    else current - 1

intSetDeltaDomain :: DeltaDomain IntSet.IntSet IntSet.IntSet
intSetDeltaDomain =
  DeltaDomain
    { deltaEmpty = IntSet.empty,
      deltaNull = IntSet.null,
      deltaMerge = IntSet.union,
      deltaApply = IntSet.union,
      deltaBetween = \oldValue newValue -> IntSet.difference newValue oldValue
    }

initialCycleValues :: Vector.Vector IntSet.IntSet
initialCycleValues =
  Vector.fromList [IntSet.singleton 0, IntSet.empty]

propagatedCycleValues :: Vector.Vector IntSet.IntSet
propagatedCycleValues =
  Vector.fromList [IntSet.singleton 0, IntSet.singleton 0]

incrementalCycleValues :: Vector.Vector IntSet.IntSet
incrementalCycleValues =
  Vector.fromList [IntSet.fromList [0, 7], IntSet.fromList [0, 7]]

fullRecomputeCycleValues :: Vector.Vector IntSet.IntSet
fullRecomputeCycleValues =
  Vector.fromList [IntSet.fromList [0, 7], IntSet.empty]

solvedAcyclicDerivativeValues :: Vector.Vector IntSet.IntSet
solvedAcyclicDerivativeValues =
  Vector.fromList [IntSet.singleton 0, IntSet.singleton 0, IntSet.singleton 0]

incrementedAcyclicDerivativeValues :: Vector.Vector IntSet.IntSet
incrementedAcyclicDerivativeValues =
  Vector.fromList [IntSet.fromList [0, 7], IntSet.fromList [0, 7], IntSet.fromList [0, 7]]

cappedSeedSnapshotValues :: Vector.Vector Int
cappedSeedSnapshotValues =
  Vector.fromList [10, 0]

cappedIntDeltaDomain :: DeltaDomain Int Int
cappedIntDeltaDomain =
  DeltaDomain
    { deltaEmpty = 0,
      deltaNull = (== 0),
      deltaMerge = max,
      deltaApply = \deltaValue oldValue -> min 10 (oldValue + deltaValue),
      deltaBetween = \oldValue newValue -> max 0 (newValue - oldValue)
    }

cappedSeedPropagationEquation :: Equation Int Int
cappedSeedPropagationEquation =
  Equation
    { equationOutput = EquationId 1,
      evaluateFull = solverIntValue 0,
      evaluateDelta =
        Just
          ( \changedInput inputDelta ->
              if changedInput == EquationId 0
                then inputDelta
                else 0
          )
    }

solveCyclicPropagation ::
  Vector.Vector IntSet.IntSet ->
  Either Obstruction (Result IntSet.IntSet IntSet.IntSet)
solveCyclicPropagation values =
  planFromEquations 2 cyclicPropagationEquations
    >>= \plan -> solveMonotone intSetDeltaDomain plan values

assertFullSnapshotDependencyClosure ::
  Int ->
  (Int -> IntSet.IntSet) ->
  Assertion
assertFullSnapshotDependencyClosure valueCount successors =
  case planFromEquations valueCount (dependencyPropagationEquations valueCount successors) of
    Left obstruction ->
      assertFailure ("dependency propagation plan failed: " <> show obstruction)
    Right plan ->
      fmap resultValues
        (solveMonotone intSetDeltaDomain plan (singletonDependencyValues valueCount))
        @?= Right (expectedDependencyClosureValues valueCount successors)

dependencyPropagationEquations ::
  Int ->
  (Int -> IntSet.IntSet) ->
  [Equation IntSet.IntSet IntSet.IntSet]
dependencyPropagationEquations valueCount successors =
  [ propagationEquation target source
    | source <- [0 .. valueCount - 1],
      target <- IntSet.toAscList (successors source)
  ]

singletonDependencyValues :: Int -> Vector.Vector IntSet.IntSet
singletonDependencyValues valueCount =
  Vector.generate valueCount IntSet.singleton

expectedDependencyClosureValues ::
  Int ->
  (Int -> IntSet.IntSet) ->
  Vector.Vector IntSet.IntSet
expectedDependencyClosureValues valueCount successors =
  Vector.generate valueCount ancestorsOf
  where
    ancestorsOf target =
      IntSet.fromList
        [ source
          | source <- [0 .. valueCount - 1],
            IntSet.member target (reachabilityFromInt successors (IntSet.singleton source))
        ]

chainDependencySuccessors :: Int -> IntSet.IntSet
chainDependencySuccessors source =
  case source of
    0 -> IntSet.singleton 1
    1 -> IntSet.singleton 2
    _ -> IntSet.empty

diamondDependencySuccessors :: Int -> IntSet.IntSet
diamondDependencySuccessors source =
  case source of
    0 -> IntSet.fromList [1, 2]
    1 -> IntSet.singleton 3
    2 -> IntSet.singleton 3
    _ -> IntSet.empty

cyclicPropagationEquations :: [Equation IntSet.IntSet IntSet.IntSet]
cyclicPropagationEquations =
  [ propagationEquation 0 1,
    propagationEquation 1 0
  ]

acyclicDerivativeEquations :: [Equation IntSet.IntSet IntSet.IntSet]
acyclicDerivativeEquations =
  [ derivativeOnlyPropagationEquation 1 0,
    derivativeOnlyPropagationEquation 2 1
  ]

propagationEquation :: Int -> Int -> Equation IntSet.IntSet IntSet.IntSet
propagationEquation output input =
  Equation
    { equationOutput = EquationId output,
      evaluateFull = solverIntSetValue input,
      evaluateDelta =
        Just
          ( \changedInput inputDelta ->
              if changedInput == EquationId input
                then inputDelta
                else IntSet.empty
          )
    }

derivativeOnlyPropagationEquation :: Int -> Int -> Equation IntSet.IntSet IntSet.IntSet
derivativeOnlyPropagationEquation output input =
  Equation
    { equationOutput = EquationId output,
      evaluateFull = IntSet.singleton 999 <$ readEquationValue (EquationId input),
      evaluateDelta =
        Just
          ( \changedInput inputDelta ->
              if changedInput == EquationId input
                then inputDelta
                else IntSet.empty
          )
    }

solverIntSetValue :: Int -> Evaluation IntSet.IntSet IntSet.IntSet
solverIntSetValue key =
  readEquationValue (EquationId key)

derivedDependencyEquation :: Equation Int Int
derivedDependencyEquation =
  Equation
    { equationOutput = EquationId 1,
      evaluateFull = solverIntValue 0,
      evaluateDelta = Nothing
    }

intGrowthDeltaDomain :: DeltaDomain Int Int
intGrowthDeltaDomain =
  DeltaDomain
    { deltaEmpty = 0,
      deltaNull = (== 0),
      deltaMerge = max,
      deltaApply = (+),
      deltaBetween = \oldValue newValue -> max 0 (newValue - oldValue)
    }

wideningPlan :: ConvergencePlan Int
wideningPlan =
  Widening
    WideningPolicy
      { wideningHeads = IntSet.singleton 0,
        widenAt = \_key _oldValue _newValue -> 100,
        narrowAt = \_key _oldValue newValue -> newValue
      }

wideningGrowthEquation :: Equation Int Int
wideningGrowthEquation =
  Equation
    { equationOutput = EquationId 0,
      evaluateFull = min 10 . (+ 1) <$> solverIntValue 0,
      evaluateDelta = Nothing
    }

solverIntValue :: Int -> Evaluation Int Int
solverIntValue key =
  readEquationValue (EquationId key)
