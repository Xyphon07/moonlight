{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Krylov.Lanczos
  ( lanczosSymmetric,
    LanczosRestartProjection,
    lanczosRestartProjectionBasisColumns,
    lanczosRestartProjectionProjectedPairs,
    lanczosRestartedProjection,
    ritzLockThreshold,
  )
where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Primitive.PrimArray
  ( MutablePrimArray,
    newPrimArray,
    readPrimArray,
    setPrimArray,
    writePrimArray,
  )
import qualified Data.Vector as Box
import qualified Data.Vector.Mutable as BoxM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.Eigen.Kernels
  ( epsDouble,
    finiteDouble,
    forDescendingIndex,
    forIndex,
    hypotStable,
  )
import Moonlight.LinAlg.Internal.VectorOps (dotU, normU, scaleU, subScaledU)
import Moonlight.LinAlg.Pure.Krylov.Config (LanczosConfig, lanczosIterations, lanczosTolerance)
import Moonlight.LinAlg.Pure.Krylov.Decomposition (LanczosDecomposition, mkLanczosDecomposition)
import Moonlight.LinAlg.Pure.Krylov.Internal
  ( linearCombinationColumnsU,
    normalizeSeed,
    validateIterationCount,
    validateSquareOperator,
  )
import Moonlight.LinAlg.Pure.Krylov.Selection (SpectrumEnd (..), sortForSpectrumBy)
import Moonlight.LinAlg.Pure.Krylov.SelectedTridiagonal (inverseIterationResidualToleranceBound, selectedSymmetricTridiagonalEigenpairColumnsDirect)
import Moonlight.LinAlg.Pure.Operator (LinearOperator, OperatorSymmetry (SelfAdjointOperator), operatorShape, runOperatorU)
import Moonlight.LinAlg.Pure.Spectral.Result (Eigenpairs, eigenpairCount, eigenpairResidualNorms, eigenpairValues, eigenpairVectorAt, eigenpairsFromColumns)
import Moonlight.LinAlg.Pure.Structured.Tridiagonal (SymmetricTridiagonal, mkSymmetricTridiagonal, mkSymmetricTridiagonalVectors)
import Prelude

newtype ActiveDimension = ActiveDimension
  { activeDimensionValue :: Int
  }
  deriving stock (Eq, Show)

data LanczosState
  = LanczosRunning !ActiveDimension !Double !(U.Vector Double) !(U.Vector Double)
  | LanczosConverged !ActiveDimension !Double
  | LanczosBreakdown !ActiveDimension !Double
  | LanczosRestarting !ActiveDimension !Double
  deriving stock (Eq, Show)

data LanczosArena s = LanczosArena
  { lanczosBasisArena :: !(BoxM.MVector s (U.Vector Double)),
    lanczosAlphaArena :: !(UM.MVector s Double),
    lanczosBetaArena :: !(UM.MVector s Double)
  }

data LanczosRestartProjection = LanczosRestartProjection
  { lanczosRestartProjectionBasisColumns :: !(Box.Vector (U.Vector Double)),
    lanczosRestartProjectionProjectedPairs :: !Eigenpairs
  }
  deriving stock (Eq, Show)

data RitzPair = RitzPair
  { ritzPairValue :: !Double,
    ritzPairVector :: !(U.Vector Double),
    ritzPairResidualNorm :: !Double,
    ritzPairProjectedResidualNorm :: !Double,
    ritzPairBoundaryCoupling :: !Double
  }
  deriving stock (Eq, Show)

data RitzCandidate = RitzCandidate
  { ritzCandidateValue :: !Double,
    ritzCandidateProjectedVector :: !(U.Vector Double),
    ritzCandidateProjectedResidualNorm :: !Double,
    ritzCandidateBoundaryCoupling :: !Double
  }
  deriving stock (Eq, Show)

data RestartSeed = RestartSeed
  { restartSeedBasisColumns :: !(Box.Vector (U.Vector Double)),
    restartSeedRetainedValues :: !(U.Vector Double),
    restartSeedSpikeCouplings :: !(U.Vector Double)
  }
  deriving stock (Eq, Show)

data RestartContext = RestartContext
  { restartLockedPairs :: ![RitzPair],
    restartSeed :: !RestartSeed,
    restartRetainedPairs :: ![RitzPair]
  }
  deriving stock (Eq, Show)

data ExpandedWindow = ExpandedWindow
  { expandedWindowBasisColumns :: !(Box.Vector (U.Vector Double)),
    expandedWindowProjectedOperator :: !BorderedProjectedOperator,
    expandedWindowBoundaryResidualNorm :: !Double,
    expandedWindowBoundaryVector :: !(Maybe (U.Vector Double)),
    expandedWindowState :: !LanczosState
  }
  deriving stock (Eq, Show)

data BorderedProjectedOperator = BorderedProjectedOperator
  { borderedRetainedValues :: !(U.Vector Double),
    borderedSpikeCouplings :: !(U.Vector Double),
    borderedKrylovDiagonal :: !(U.Vector Double),
    borderedKrylovOffDiagonal :: !(U.Vector Double)
  }
  deriving stock (Eq, Show)

data BorderedProjectionReduction = BorderedProjectionReduction
  { borderedReductionBasisColumns :: !(Box.Vector (U.Vector Double)),
    borderedReductionTridiagonal :: !SymmetricTridiagonal
  }
  deriving stock (Eq, Show)

data BorderedReductionArena s = BorderedReductionArena
  { borderedReductionArenaDimension :: !Int,
    borderedReductionArenaPayload :: !(MutablePrimArray s Double),
    borderedReductionArenaMatrixOffset :: !Int,
    borderedReductionArenaBasisOffset :: !Int
  }

data RestartSeedResult = RestartSeedResult
  { restartSeedResultSeed :: !RestartSeed,
    restartSeedResultRetainedPairs :: ![RitzPair]
  }
  deriving stock (Eq, Show)

lanczosSymmetric :: LanczosConfig -> LinearOperator 'SelfAdjointOperator -> U.Vector Double -> Either MoonlightError LanczosDecomposition
lanczosSymmetric config op seedVector = do
  validateSquareOperator "Lanczos" op
  let (_, cols) = operatorShape op
  targetIterations <- validateIterationCount "Lanczos" (lanczosIterations config)
  firstBasis <- normalizeSeed "Lanczos" cols (lanczosTolerance config) seedVector
  let boundedIterations = min targetIterations cols
      zeroVector = U.replicate cols 0.0
      tolerance = lanczosTolerance config
   in runST $ do
        arena <- newLanczosArena boundedIterations
        BoxM.unsafeWrite (lanczosBasisArena arena) 0 firstBasis
        finalStateResult <-
          runLanczosState
            op
            tolerance
            boundedIterations
            arena
            (LanczosRunning (ActiveDimension 1) 0.0 zeroVector firstBasis)
        case finalStateResult of
          Left err -> pure (Left err)
          Right finalState -> freezeLanczosState arena finalState

lanczosRestartedProjection ::
  LanczosConfig ->
  SpectrumEnd ->
  Int ->
  LinearOperator 'SelfAdjointOperator ->
  U.Vector Double ->
  Either MoonlightError LanczosRestartProjection
lanczosRestartedProjection config spectrumEnd requestedCount op seedVector
  | requestedCount <= 0 =
      Left (InvariantViolation "restarted Lanczos requires a positive requested count")
  | otherwise = do
      validateSquareOperator "restarted Lanczos" op
      let (_, cols) = operatorShape op
          tolerance = lanczosTolerance config
      if requestedCount > cols
        then Left (InvariantViolation "restarted Lanczos requested count exceeds operator dimension")
        else do
          targetIterations <- validateIterationCount "restarted Lanczos" (lanczosIterations config)
          firstBasis <- normalizeSeed "restarted Lanczos" cols tolerance seedVector
          let capacity = min targetIterations cols
          restartLoop
            op
            spectrumEnd
            requestedCount
            tolerance
            capacity
            cols
            (maxRestartCycles cols capacity)
            (RestartContext [] (initialRestartSeed firstBasis) [])

initialRestartSeed :: U.Vector Double -> RestartSeed
initialRestartSeed firstBasis =
  RestartSeed
    { restartSeedBasisColumns = Box.singleton firstBasis,
      restartSeedRetainedValues = U.empty,
      restartSeedSpikeCouplings = U.empty
    }

newLanczosArena :: Int -> ST s (LanczosArena s)
newLanczosArena capacity = do
  basisArena <- BoxM.unsafeNew capacity
  alphaArena <- UM.unsafeNew capacity
  betaArena <- UM.unsafeNew (max 0 (capacity - 1))
  pure
    LanczosArena
      { lanczosBasisArena = basisArena,
        lanczosAlphaArena = alphaArena,
        lanczosBetaArena = betaArena
      }

runLanczosState ::
  LinearOperator 'SelfAdjointOperator ->
  Double ->
  Int ->
  LanczosArena s ->
  LanczosState ->
  ST s (Either MoonlightError LanczosState)
runLanczosState op tolerance capacity arena state =
  case state of
    LanczosRunning activeDimension previousBeta previousBasis currentBasis -> do
      nextState <- stepLanczosState op tolerance capacity arena activeDimension previousBeta previousBasis currentBasis
      case nextState of
        Left err -> pure (Left err)
        Right stateValue -> runLanczosState op tolerance capacity arena stateValue
    LanczosConverged{} -> pure (Right state)
    LanczosBreakdown{} -> pure (Right state)
    LanczosRestarting{} -> pure (Right state)

stepLanczosState ::
  LinearOperator 'SelfAdjointOperator ->
  Double ->
  Int ->
  LanczosArena s ->
  ActiveDimension ->
  Double ->
  U.Vector Double ->
  U.Vector Double ->
  ST s (Either MoonlightError LanczosState)
stepLanczosState op tolerance capacity arena activeDimension previousBeta previousBasis currentBasis =
  case runOperatorU op currentBasis of
    Left err ->
      pure (Left err)
    Right imageVector ->
      case removePreviousDirection imageVector of
        Left err ->
          pure (Left err)
        Right withPreviousRemoved ->
          case dotU currentBasis withPreviousRemoved of
            Left err ->
              pure (Left err)
            Right alphaValue ->
              case subScaledU withPreviousRemoved alphaValue currentBasis of
                Left err ->
                  pure (Left err)
                Right projectedCurrent -> do
                  residualResult <- orthogonalizeAgainstArena arena activeDimension projectedCurrent
                  case residualResult of
                    Left err ->
                      pure (Left err)
                    Right residualVector -> do
                      let activeCount = activeDimensionValue activeDimension
                          currentIndex = activeCount - 1
                          betaValue = normU residualVector
                      UM.unsafeWrite (lanczosAlphaArena arena) currentIndex alphaValue
                      if betaValue <= tolerance
                        then pure (Right (LanczosConverged activeDimension betaValue))
                        else
                          if activeCount >= capacity
                            then pure (Right (LanczosRestarting activeDimension betaValue))
                            else do
                              let nextBasis = scaleU (1.0 / betaValue) residualVector
                                  nextActiveDimension = ActiveDimension (activeCount + 1)
                              UM.unsafeWrite (lanczosBetaArena arena) currentIndex betaValue
                              BoxM.unsafeWrite (lanczosBasisArena arena) activeCount nextBasis
                              pure (Right (LanczosRunning nextActiveDimension betaValue currentBasis nextBasis))
  where
    removePreviousDirection imageVector =
      if activeDimensionValue activeDimension == 1
        then Right imageVector
        else subScaledU imageVector previousBeta previousBasis

orthogonalizeAgainstArena ::
  LanczosArena s ->
  ActiveDimension ->
  U.Vector Double ->
  ST s (Either MoonlightError (U.Vector Double))
orthogonalizeAgainstArena arena activeDimension inputVector = do
  reducedOnce <- projectAgainstArenaOnce arena activeDimension inputVector
  case reducedOnce of
    Left err -> pure (Left err)
    Right reducedVector -> projectAgainstArenaOnce arena activeDimension reducedVector

projectAgainstArenaOnce ::
  LanczosArena s ->
  ActiveDimension ->
  U.Vector Double ->
  ST s (Either MoonlightError (U.Vector Double))
projectAgainstArenaOnce arena activeDimension inputVector =
  projectBasisIndex 0 inputVector
  where
    activeCount = activeDimensionValue activeDimension
    projectBasisIndex basisIndex workingVector
      | basisIndex >= activeCount = pure (Right workingVector)
      | otherwise = do
          basisVector <- BoxM.unsafeRead (lanczosBasisArena arena) basisIndex
          case dotU basisVector workingVector of
            Left err -> pure (Left err)
            Right coefficient ->
              case subScaledU workingVector coefficient basisVector of
                Left err -> pure (Left err)
                Right nextVector -> projectBasisIndex (basisIndex + 1) nextVector

expandRestartWindow ::
  LinearOperator 'SelfAdjointOperator ->
  Double ->
  Int ->
  Box.Vector (U.Vector Double) ->
  RestartSeed ->
  Either MoonlightError ExpandedWindow
expandRestartWindow op tolerance capacity lockedVectors seedValue
  | capacity <= 0 =
      Left (InvariantViolation "restarted Lanczos active capacity must be positive")
  | Box.null (restartSeedBasisColumns seedValue) =
      Left (InvariantViolation "restarted Lanczos requires a non-empty restart basis")
  | Box.length (restartSeedBasisColumns seedValue) > capacity =
      Left (InvariantViolation "restarted Lanczos restart seed exceeds the active capacity")
  | U.length (restartSeedRetainedValues seedValue) /= U.length (restartSeedSpikeCouplings seedValue) =
      Left (InvariantViolation "restarted Lanczos retained Ritz values must match spike couplings")
  | Box.length (restartSeedBasisColumns seedValue) /= U.length (restartSeedRetainedValues seedValue) + 1 =
      Left (InvariantViolation "restarted Lanczos seed basis must contain retained vectors plus one Krylov boundary vector")
  | otherwise =
      runST $ do
        arena <- newLanczosArena capacity
        let seedBasis = restartSeedBasisColumns seedValue
            seedCount = Box.length seedBasis
        traverse_ (writeSeedBasis arena) (zip [0 :: Int ..] (Box.toList seedBasis))
        case reverse (Box.toList seedBasis) of
          [] -> pure (Left (InvariantViolation "restarted Lanczos requires a non-empty bounded restart basis"))
          currentBasis : _ ->
            expandRestartFirstKrylovState op tolerance capacity lockedVectors seedValue arena seedCount currentBasis

writeSeedBasis :: LanczosArena s -> (Int, U.Vector Double) -> ST s ()
writeSeedBasis arena (basisIndex, basisVector) =
  BoxM.unsafeWrite (lanczosBasisArena arena) basisIndex basisVector

expandRestartFirstKrylovState ::
  LinearOperator 'SelfAdjointOperator ->
  Double ->
  Int ->
  Box.Vector (U.Vector Double) ->
  RestartSeed ->
  LanczosArena s ->
  Int ->
  U.Vector Double ->
  ST s (Either MoonlightError ExpandedWindow)
expandRestartFirstKrylovState op tolerance capacity lockedVectors seedValue arena activeCount currentBasis =
  case runOperatorU op currentBasis of
    Left err -> pure (Left err)
    Right imageVector ->
      case removeRetainedDirections imageVector of
        Left err -> pure (Left err)
        Right withRetainedRemoved ->
          case dotU currentBasis withRetainedRemoved of
            Left err -> pure (Left err)
            Right alphaValue ->
              case subScaledU withRetainedRemoved alphaValue currentBasis of
                Left err -> pure (Left err)
                Right projectedCurrent -> do
                  residualResult <-
                    orthogonalizeAgainstLockedAndArena
                      lockedVectors
                      arena
                      (ActiveDimension activeCount)
                      projectedCurrent
                  case residualResult of
                    Left err ->
                      pure (Left err)
                    Right residualVector -> do
                      let currentIndex = activeCount - 1
                          betaValue = normU residualVector
                      UM.unsafeWrite (lanczosAlphaArena arena) currentIndex alphaValue
                      if betaValue <= tolerance
                        then
                          freezeExpandedWindow
                            seedValue
                            arena
                            (LanczosConverged (ActiveDimension activeCount) betaValue)
                            Nothing
                        else
                          let nextBasis = scaleU (1.0 / betaValue) residualVector
                           in if activeCount >= capacity
                                then
                                  freezeExpandedWindow
                                    seedValue
                                    arena
                                    (LanczosRestarting (ActiveDimension activeCount) betaValue)
                                    (Just nextBasis)
                                else do
                                  BoxM.unsafeWrite (lanczosBasisArena arena) activeCount nextBasis
                                  UM.unsafeWrite (lanczosBetaArena arena) currentIndex betaValue
                                  expandRestartState
                                    op
                                    tolerance
                                    capacity
                                    lockedVectors
                                    seedValue
                                    arena
                                    (activeCount + 1)
                                    betaValue
                                    currentBasis
                                    nextBasis
  where
    retainedBasis = Box.take (U.length (restartSeedRetainedValues seedValue)) (restartSeedBasisColumns seedValue)
    retainedCouplings = U.toList (restartSeedSpikeCouplings seedValue)
    removeRetainedDirections imageVector =
      foldM
        (\workingVector (basisVector, couplingValue) -> subScaledU workingVector couplingValue basisVector)
        imageVector
        (zip (Box.toList retainedBasis) retainedCouplings)

expandRestartState ::
  LinearOperator 'SelfAdjointOperator ->
  Double ->
  Int ->
  Box.Vector (U.Vector Double) ->
  RestartSeed ->
  LanczosArena s ->
  Int ->
  Double ->
  U.Vector Double ->
  U.Vector Double ->
  ST s (Either MoonlightError ExpandedWindow)
expandRestartState op tolerance capacity lockedVectors seedValue arena activeCount previousBeta previousBasis currentBasis
  | activeCount > capacity =
      freezeExpandedWindow seedValue arena (LanczosRestarting (ActiveDimension activeCount) previousBeta) Nothing
  | otherwise =
      case runOperatorU op currentBasis of
        Left err -> pure (Left err)
        Right imageVector ->
          case subScaledU imageVector previousBeta previousBasis of
            Left err -> pure (Left err)
            Right withPreviousRemoved ->
              case dotU currentBasis withPreviousRemoved of
                Left err -> pure (Left err)
                Right alphaValue ->
                  case subScaledU withPreviousRemoved alphaValue currentBasis of
                    Left err -> pure (Left err)
                    Right projectedCurrent -> do
                      residualResult <-
                        orthogonalizeAgainstLockedAndArena
                          lockedVectors
                          arena
                          (ActiveDimension activeCount)
                          projectedCurrent
                      case residualResult of
                        Left err -> pure (Left err)
                        Right residualVector -> do
                          let currentIndex = activeCount - 1
                              betaValue = normU residualVector
                          UM.unsafeWrite (lanczosAlphaArena arena) currentIndex alphaValue
                          if betaValue <= tolerance
                            then
                              freezeExpandedWindow
                                seedValue
                                arena
                                (LanczosConverged (ActiveDimension activeCount) betaValue)
                                Nothing
                            else do
                              let nextBasis = scaleU (1.0 / betaValue) residualVector
                              if activeCount >= capacity
                                then
                                  freezeExpandedWindow
                                    seedValue
                                    arena
                                    (LanczosRestarting (ActiveDimension activeCount) betaValue)
                                    (Just nextBasis)
                                else do
                                  BoxM.unsafeWrite (lanczosBasisArena arena) activeCount nextBasis
                                  UM.unsafeWrite (lanczosBetaArena arena) currentIndex betaValue
                                  expandRestartState
                                    op
                                    tolerance
                                    capacity
                                    lockedVectors
                                    seedValue
                                    arena
                                    (activeCount + 1)
                                    betaValue
                                    currentBasis
                                    nextBasis

orthogonalizeAgainstLockedAndArena ::
  Box.Vector (U.Vector Double) ->
  LanczosArena s ->
  ActiveDimension ->
  U.Vector Double ->
  ST s (Either MoonlightError (U.Vector Double))
orthogonalizeAgainstLockedAndArena lockedVectors arena activeDimension inputVector =
  case projectAgainstVectorListTwice (Box.toList lockedVectors) inputVector of
    Left err -> pure (Left err)
    Right selectivelyReduced -> orthogonalizeAgainstArena arena activeDimension selectivelyReduced

freezeExpandedWindow :: RestartSeed -> LanczosArena s -> LanczosState -> Maybe (U.Vector Double) -> ST s (Either MoonlightError ExpandedWindow)
freezeExpandedWindow seedValue arena state boundaryVector =
  let activeCount =
        case state of
          LanczosRunning activeDimension _ _ _ -> activeDimensionValue activeDimension
          LanczosConverged activeDimension _ -> activeDimensionValue activeDimension
          LanczosBreakdown activeDimension _ -> activeDimensionValue activeDimension
          LanczosRestarting activeDimension _ -> activeDimensionValue activeDimension
      retainedCount = U.length (restartSeedRetainedValues seedValue)
      krylovCount = activeCount - retainedCount
      boundaryResidual =
        case state of
          LanczosRunning _ residual _ _ -> residual
          LanczosConverged _ residual -> residual
          LanczosBreakdown _ residual -> residual
          LanczosRestarting _ residual -> residual
   in do
        basisVectors <- Box.freeze (BoxM.slice 0 activeCount (lanczosBasisArena arena))
        krylovDiagonal <- U.freeze (UM.slice retainedCount krylovCount (lanczosAlphaArena arena))
        krylovOffDiagonal <- U.freeze (UM.slice retainedCount (max 0 (krylovCount - 1)) (lanczosBetaArena arena))
        pure $ do
          projectedOperator <-
            mkBorderedProjectedOperator
              (restartSeedRetainedValues seedValue)
              (restartSeedSpikeCouplings seedValue)
              krylovDiagonal
              krylovOffDiagonal
          Right
            ExpandedWindow
              { expandedWindowBasisColumns = basisVectors,
                expandedWindowProjectedOperator = projectedOperator,
                expandedWindowBoundaryResidualNorm = boundaryResidual,
                expandedWindowBoundaryVector = boundaryVector,
                expandedWindowState = state
              }

restartLoop ::
  LinearOperator 'SelfAdjointOperator ->
  SpectrumEnd ->
  Int ->
  Double ->
  Int ->
  Int ->
  Int ->
  RestartContext ->
  Either MoonlightError LanczosRestartProjection
restartLoop op spectrumEnd requestedCount tolerance capacity ambientDimension remainingCycles context
  | length (restartLockedPairs context) >= requestedCount =
      finalizeRestartProjection spectrumEnd requestedCount (restartLockedPairs context) []
  | remainingCycles <= 0 =
      finalizeRestartProjection spectrumEnd requestedCount (restartLockedPairs context) (restartRetainedPairs context)
  | otherwise = do
      let lockedVectors = Box.fromList (ritzPairVector <$> restartLockedPairs context)
      expandedWindow <- expandRestartWindow op tolerance capacity lockedVectors (restartSeed context)
      let activeBasis = expandedWindowBasisColumns expandedWindow
          selectedCount =
            min
              (Box.length activeBasis)
              ( max
                  1
                  (requestedCount - length (restartLockedPairs context) + restartGuardCount requestedCount capacity)
              )
      cycleCandidates <-
        selectedRitzCandidatesFromProjectedOperator
          spectrumEnd
          selectedCount
          (expandedWindowProjectedOperator expandedWindow)
          (expandedWindowBoundaryResidualNorm expandedWindow)
      let candidateIsLocked =
            ritzCandidateIsLocked tolerance ambientDimension (expandedWindowProjectedOperator expandedWindow)
          cycleLockedCandidates =
            take
              (requestedCount - length (restartLockedPairs context))
              (filter candidateIsLocked cycleCandidates)
      cycleLiftedPairs <- traverse (ritzPairFromCandidate activeBasis) cycleLockedCandidates
      let pairIsLocked =
            ritzPairIsLocked tolerance ambientDimension (expandedWindowProjectedOperator expandedWindow)
          (cycleLockedPairs, demotedCandidates) =
            partitionEithers
              [ if pairIsLocked liftedPair then Left liftedPair else Right liftedCandidate
              | (liftedCandidate, liftedPair) <- zip cycleLockedCandidates cycleLiftedPairs
              ]
          cycleUnlockedCandidates =
            demotedCandidates <> filter (not . candidateIsLocked) cycleCandidates
          nextLockedPairs =
            take requestedCount $
              sortForSpectrumBy
                spectrumEnd
                ritzPairValue
                (restartLockedPairs context <> cycleLockedPairs)
      if length nextLockedPairs >= requestedCount
        then finalizeRestartProjection spectrumEnd requestedCount nextLockedPairs []
        else
          if lanczosStateTerminal (expandedWindowState expandedWindow)
            then do
              let terminalUnlockedCandidates =
                    take
                      (requestedCount - length nextLockedPairs)
                      (sortForSpectrumBy spectrumEnd ritzCandidateValue cycleUnlockedCandidates)
              cycleUnlockedPairs <- traverse (ritzPairFromCandidate activeBasis) terminalUnlockedCandidates
              finalizeRestartProjection spectrumEnd requestedCount nextLockedPairs cycleUnlockedPairs
            else do
              seedResult <-
                restartSeedFromRitzCandidates
                  spectrumEnd
                  requestedCount
                  tolerance
                  capacity
                  ambientDimension
                  expandedWindow
                  nextLockedPairs
                  cycleUnlockedCandidates
              restartLoop
                op
                spectrumEnd
                requestedCount
                tolerance
                capacity
                ambientDimension
                (remainingCycles - 1)
                (RestartContext nextLockedPairs (restartSeedResultSeed seedResult) (restartSeedResultRetainedPairs seedResult))

lanczosStateTerminal :: LanczosState -> Bool
lanczosStateTerminal state =
  case state of
    LanczosConverged{} -> True
    LanczosBreakdown{} -> True
    LanczosRunning{} -> False
    LanczosRestarting{} -> False

finalizeRestartProjection ::
  SpectrumEnd ->
  Int ->
  [RitzPair] ->
  [RitzPair] ->
  Either MoonlightError LanczosRestartProjection
finalizeRestartProjection spectrumEnd requestedCount lockedPairs candidatePairs =
  let finalPairs =
        take requestedCount $
          sortForSpectrumBy
            spectrumEnd
            ritzPairValue
            (lockedPairs <> candidatePairs)
      finalBasis = Box.fromList (ritzPairVector <$> finalPairs)
   in if Box.length finalBasis < requestedCount
    then Left (InvariantViolation "restarted Lanczos final subspace is smaller than the requested eigenspace")
    else do
      projectedPairs <- finalProjectedPairsFromRitzPairs finalPairs
      Right
        LanczosRestartProjection
          { lanczosRestartProjectionBasisColumns = finalBasis,
            lanczosRestartProjectionProjectedPairs = projectedPairs
          }

selectedRitzCandidatesFromProjectedOperator ::
  SpectrumEnd ->
  Int ->
  BorderedProjectedOperator ->
  Double ->
  Either MoonlightError [RitzCandidate]
selectedRitzCandidatesFromProjectedOperator spectrumEnd requestedCount projectedOperator boundaryResidualNorm = do
  projectedPairs <- selectedProjectedPairsFromBorderedOperator spectrumEnd requestedCount projectedOperator boundaryResidualNorm
  traverse
    (ritzCandidateFromProjectedPair projectedOperator boundaryResidualNorm projectedPairs)
    [0 .. eigenpairCount projectedPairs - 1]

selectedProjectedPairsFromBorderedOperator ::
  SpectrumEnd ->
  Int ->
  BorderedProjectedOperator ->
  Double ->
  Either MoonlightError Eigenpairs
selectedProjectedPairsFromBorderedOperator spectrumEnd requestedCount projectedOperator boundaryResidualNorm
  | requestedCount <= 0 =
      Left (InvariantViolation "projected restarted Lanczos eigensolve requires a positive requested count")
  | requestedCount > borderedProjectedOperatorDimension projectedOperator =
      Left (InvariantViolation "projected restarted Lanczos eigensolve requested count exceeds basis dimension")
  | otherwise = do
      selectedColumns <- selectedBorderedProjectedColumns spectrumEnd requestedCount projectedOperator
      columnsWithResiduals <- traverse (projectedPairColumn projectedOperator boundaryResidualNorm) selectedColumns
      eigenpairsFromColumns
        (borderedProjectedOperatorDimension projectedOperator)
        columnsWithResiduals

selectedBorderedProjectedColumns ::
  SpectrumEnd ->
  Int ->
  BorderedProjectedOperator ->
  Either MoonlightError [(Double, U.Vector Double, Double)]
selectedBorderedProjectedColumns spectrumEnd requestedCount projectedOperator =
  if U.null (borderedRetainedValues projectedOperator)
    then do
      tridiagonalValue <-
        mkSymmetricTridiagonalVectors
          (borderedKrylovDiagonal projectedOperator)
          (borderedKrylovOffDiagonal projectedOperator)
      selectedSymmetricTridiagonalEigenpairColumnsDirect spectrumEnd requestedCount tridiagonalValue
    else do
      reduction <- reduceBorderedProjectedOperator projectedOperator
      reducedColumns <-
        selectedSymmetricTridiagonalEigenpairColumnsDirect
          spectrumEnd
          requestedCount
          (borderedReductionTridiagonal reduction)
      traverse (liftReducedBorderedColumn reduction) reducedColumns

liftReducedBorderedColumn ::
  BorderedProjectionReduction ->
  (Double, U.Vector Double, Double) ->
  Either MoonlightError (Double, U.Vector Double, Double)
liftReducedBorderedColumn reduction (eigenvalue, reducedVector, reducedResidualNorm) = do
  projectedVector <- normalizeProjectedCoefficientVector =<< linearCombinationColumnsU (borderedReductionBasisColumns reduction) reducedVector
  Right (eigenvalue, projectedVector, reducedResidualNorm)

projectedPairColumn ::
  BorderedProjectedOperator ->
  Double ->
  (Double, U.Vector Double, Double) ->
  Either MoonlightError (Double, U.Vector Double, Double)
projectedPairColumn projectedOperator boundaryResidualNorm (eigenvalue, eigenvector, selectedResidualNorm) = do
  projectedVector <- normalizeProjectedCoefficientVector eigenvector
  residualEvidence <- projectedResidualEvidence projectedOperator boundaryResidualNorm eigenvalue projectedVector
  if finiteDouble selectedResidualNorm
    then Right (eigenvalue, projectedVector, max residualEvidence selectedResidualNorm)
    else Left (InvariantViolation "bordered projected eigensolve produced a non-finite selected residual")

ritzCandidateFromProjectedPair ::
  BorderedProjectedOperator ->
  Double ->
  Eigenpairs ->
  Int ->
  Either MoonlightError RitzCandidate
ritzCandidateFromProjectedPair projectedOperator boundaryResidualNorm projectedPairs columnIndex = do
  eigenvalue <-
    case eigenpairValues projectedPairs U.!? columnIndex of
      Nothing -> Left (InvariantViolation "restarted Lanczos projected eigenvalue index out of bounds")
      Just value -> Right value
  projectedVector <- eigenpairVectorAt columnIndex projectedPairs
  projectedResidualNorm <-
    case eigenpairResidualNorms projectedPairs U.!? columnIndex of
      Nothing -> Left (InvariantViolation "restarted Lanczos projected residual index out of bounds")
      Just value -> Right value
  boundaryCoupling <- projectedBoundaryCoupling projectedOperator boundaryResidualNorm projectedVector
  if finiteDouble projectedResidualNorm && finiteDouble boundaryCoupling
    then
      Right
        RitzCandidate
          { ritzCandidateValue = eigenvalue,
            ritzCandidateProjectedVector = projectedVector,
            ritzCandidateProjectedResidualNorm = projectedResidualNorm,
            ritzCandidateBoundaryCoupling = boundaryCoupling
          }
    else Left (InvariantViolation "restarted Lanczos produced a non-finite projected Ritz residual")

ritzPairFromCandidate ::
  Box.Vector (U.Vector Double) ->
  RitzCandidate ->
  Either MoonlightError RitzPair
ritzPairFromCandidate basisColumns candidate = do
  liftedVector <- normalizeLiftedVector =<< linearCombinationColumnsU basisColumns projectedVector
  let residualNorm = ritzCandidateProjectedResidualNorm candidate
  if finiteDouble residualNorm && finiteDouble projectedResidualNorm
    then
      Right
        RitzPair
          { ritzPairValue = ritzCandidateValue candidate,
            ritzPairVector = liftedVector,
            ritzPairResidualNorm = residualNorm,
            ritzPairProjectedResidualNorm = projectedResidualNorm,
            ritzPairBoundaryCoupling = ritzCandidateBoundaryCoupling candidate
          }
    else Left (InvariantViolation "restarted Lanczos produced a non-finite Ritz residual")
  where
    projectedVector = ritzCandidateProjectedVector candidate
    projectedResidualNorm = ritzCandidateProjectedResidualNorm candidate

normalizeLiftedVector :: U.Vector Double -> Either MoonlightError (U.Vector Double)
normalizeLiftedVector vectorValue =
  let vectorNorm = normU vectorValue
   in if finiteDouble vectorNorm && vectorNorm > 0.0
        then Right (scaleU (1.0 / vectorNorm) vectorValue)
        else Left (InvariantViolation "restarted Lanczos produced a degenerate lifted Ritz vector")

restartSeedFromRitzCandidates ::
  SpectrumEnd ->
  Int ->
  Double ->
  Int ->
  Int ->
  ExpandedWindow ->
  [RitzPair] ->
  [RitzCandidate] ->
  Either MoonlightError RestartSeedResult
restartSeedFromRitzCandidates spectrumEnd requestedCount tolerance capacity ambientDimension expandedWindow lockedPairs cycleCandidates = do
  let lockedVectors = ritzPairVector <$> lockedPairs
      remainingWanted = max 1 (requestedCount - length lockedPairs)
      retainedCandidates =
        take
          (restartRetainedCount capacity remainingWanted (length cycleCandidates))
          (sortForSpectrumBy spectrumEnd ritzCandidateValue cycleCandidates)
  case expandedWindowBoundaryVector expandedWindow of
    Just boundaryVector ->
      if U.length boundaryVector == ambientDimension
        then do
          retainedPairs <- traverse (ritzPairFromCandidate (expandedWindowBasisColumns expandedWindow)) retainedCandidates
          Right
            RestartSeedResult
              { restartSeedResultSeed =
                  RestartSeed
                    { restartSeedBasisColumns = Box.fromList ((ritzPairVector <$> retainedPairs) <> [boundaryVector]),
                      restartSeedRetainedValues = U.fromList (ritzPairValue <$> retainedPairs),
                      restartSeedSpikeCouplings = U.fromList (ritzPairBoundaryCoupling <$> retainedPairs)
                    },
                restartSeedResultRetainedPairs = retainedPairs
              }
        else Left (InvariantViolation "restarted Lanczos boundary vector dimension mismatch")
    Nothing ->
      if null retainedCandidates
        then do
          seedBasis <- canonicalRestartBasis tolerance ambientDimension lockedVectors
          Right
            RestartSeedResult
              { restartSeedResultSeed =
                  RestartSeed
                    { restartSeedBasisColumns = seedBasis,
                      restartSeedRetainedValues = U.empty,
                      restartSeedSpikeCouplings = U.empty
                    },
                restartSeedResultRetainedPairs = []
              }
        else Left (InvariantViolation "restarted Lanczos cannot retain Ritz values without a boundary vector")

finalProjectedPairsFromRitzPairs :: [RitzPair] -> Either MoonlightError Eigenpairs
finalProjectedPairsFromRitzPairs finalPairs =
  let projectedDimension = length finalPairs
   in eigenpairsFromColumns
        projectedDimension
        (zipWith finalProjectedPairColumn [0 ..] finalPairs)
  where
    finalProjectedPairColumn columnIndex ritzPair =
      ( ritzPairValue ritzPair,
        unitVector (length finalPairs) columnIndex,
        ritzPairProjectedResidualNorm ritzPair
      )

mkBorderedProjectedOperator ::
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either MoonlightError BorderedProjectedOperator
mkBorderedProjectedOperator retainedValues spikeCouplings krylovDiagonal krylovOffDiagonal
  | U.length retainedValues /= U.length spikeCouplings =
      Left (InvariantViolation "bordered projected operator retained value count must match spike count")
  | U.null krylovDiagonal =
      Left (InvariantViolation "bordered projected operator requires a non-empty Krylov block")
  | U.length krylovOffDiagonal /= U.length krylovDiagonal - 1 =
      Left (InvariantViolation "bordered projected operator Krylov off-diagonal length mismatch")
  | U.any (not . finiteDouble) retainedValues
      || U.any (not . finiteDouble) spikeCouplings
      || U.any (not . finiteDouble) krylovDiagonal
      || U.any (not . finiteDouble) krylovOffDiagonal =
      Left (InvariantViolation "bordered projected operator entries must be finite")
  | otherwise =
      Right
        BorderedProjectedOperator
          { borderedRetainedValues = retainedValues,
            borderedSpikeCouplings = spikeCouplings,
            borderedKrylovDiagonal = krylovDiagonal,
            borderedKrylovOffDiagonal = krylovOffDiagonal
          }

borderedProjectedOperatorDimension :: BorderedProjectedOperator -> Int
borderedProjectedOperatorDimension projectedOperator =
  U.length (borderedRetainedValues projectedOperator) + U.length (borderedKrylovDiagonal projectedOperator)

applyBorderedProjectedOperatorU ::
  BorderedProjectedOperator ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double)
applyBorderedProjectedOperatorU projectedOperator inputVector =
  let retainedCount = U.length (borderedRetainedValues projectedOperator)
      krylovCount = U.length (borderedKrylovDiagonal projectedOperator)
      projectedDimension = retainedCount + krylovCount
   in if U.length inputVector /= projectedDimension
        then Left (InvariantViolation "bordered projected operator input dimension mismatch")
        else
          Right
            ( U.generate
                projectedDimension
                (borderedProjectedOperatorEntry projectedOperator inputVector retainedCount krylovCount)
            )

borderedProjectedOperatorEntry ::
  BorderedProjectedOperator ->
  U.Vector Double ->
  Int ->
  Int ->
  Int ->
  Double
borderedProjectedOperatorEntry projectedOperator inputVector retainedCount krylovCount entryIndex =
  if entryIndex < retainedCount
    then
      let retainedValue = borderedRetainedValues projectedOperator `U.unsafeIndex` entryIndex
          spikeValue = borderedSpikeCouplings projectedOperator `U.unsafeIndex` entryIndex
          retainedEntry = inputVector `U.unsafeIndex` entryIndex
          firstKrylovEntry = inputVector `U.unsafeIndex` retainedCount
       in retainedValue * retainedEntry + spikeValue * firstKrylovEntry
    else
      let krylovIndex = entryIndex - retainedCount
          diagonalValue = borderedKrylovDiagonal projectedOperator `U.unsafeIndex` krylovIndex
          centerEntry = inputVector `U.unsafeIndex` entryIndex
          leftEntry =
            if krylovIndex <= 0
              then U.sum (U.zipWith (*) (borderedSpikeCouplings projectedOperator) (U.take retainedCount inputVector))
              else (borderedKrylovOffDiagonal projectedOperator `U.unsafeIndex` (krylovIndex - 1)) * (inputVector `U.unsafeIndex` (entryIndex - 1))
          rightEntry =
            if krylovIndex + 1 >= krylovCount
              then 0.0
              else (borderedKrylovOffDiagonal projectedOperator `U.unsafeIndex` krylovIndex) * (inputVector `U.unsafeIndex` (entryIndex + 1))
       in leftEntry + diagonalValue * centerEntry + rightEntry

reduceBorderedProjectedOperator ::
  BorderedProjectedOperator ->
  Either MoonlightError BorderedProjectionReduction
reduceBorderedProjectedOperator projectedOperator =
  runST $ do
    let projectedDimension = borderedProjectedOperatorDimension projectedOperator
        reductionTolerance = borderedReductionTolerance projectedOperator
    arena <- newBorderedReductionArena projectedDimension
    initializeBorderedReductionArena projectedOperator arena
    chaseBorderedReductionBulges arena reductionTolerance
    diagonalValues <- freezeBorderedReductionDiagonal arena
    offDiagonalValues <- freezeBorderedReductionOffDiagonal arena
    basisColumns <- freezeBorderedReductionBasis arena
    pure $ do
      tridiagonalValue <- mkSymmetricTridiagonalVectors diagonalValues offDiagonalValues
      Right
        BorderedProjectionReduction
          { borderedReductionBasisColumns = basisColumns,
            borderedReductionTridiagonal = tridiagonalValue
          }

newBorderedReductionArena :: Int -> ST s (BorderedReductionArena s)
newBorderedReductionArena projectedDimension = do
  payload <- newPrimArray payloadLength
  setPrimArray payload 0 payloadLength 0.0
  pure
    BorderedReductionArena
      { borderedReductionArenaDimension = projectedDimension,
        borderedReductionArenaPayload = payload,
        borderedReductionArenaMatrixOffset = matrixOffset,
        borderedReductionArenaBasisOffset = basisOffset
      }
  where
    matrixOffset = 0
    matrixLength = projectedDimension * projectedDimension
    basisOffset = matrixOffset + matrixLength
    basisLength = projectedDimension * projectedDimension
    payloadLength = basisOffset + basisLength

initializeBorderedReductionArena :: BorderedProjectedOperator -> BorderedReductionArena s -> ST s ()
initializeBorderedReductionArena projectedOperator arena = do
  forIndex 0 retainedCount $ \retainedIndex ->
    writeBorderedMatrixEntry arena retainedIndex retainedIndex (borderedRetainedValues projectedOperator `U.unsafeIndex` retainedIndex)
  forIndex 0 krylovCount $ \krylovIndex ->
    writeBorderedMatrixEntry arena (retainedCount + krylovIndex) (retainedCount + krylovIndex) (borderedKrylovDiagonal projectedOperator `U.unsafeIndex` krylovIndex)
  forIndex 0 retainedCount $ \retainedIndex ->
    writeSymmetricBorderedMatrixEntry arena retainedIndex retainedCount (borderedSpikeCouplings projectedOperator `U.unsafeIndex` retainedIndex)
  forIndex 0 (max 0 (krylovCount - 1)) $ \krylovIndex ->
    writeSymmetricBorderedMatrixEntry
      arena
      (retainedCount + krylovIndex)
      (retainedCount + krylovIndex + 1)
      (borderedKrylovOffDiagonal projectedOperator `U.unsafeIndex` krylovIndex)
  forIndex 0 projectedDimension $ \basisIndex ->
    writeBorderedBasisEntry arena basisIndex basisIndex 1.0
  where
    retainedCount = U.length (borderedRetainedValues projectedOperator)
    krylovCount = U.length (borderedKrylovDiagonal projectedOperator)
    projectedDimension = retainedCount + krylovCount

chaseBorderedReductionBulges :: BorderedReductionArena s -> Double -> ST s ()
chaseBorderedReductionBulges arena reductionTolerance =
  forIndex 0 (max 0 (projectedDimension - 2)) $ \columnIndex ->
    forDescendingIndex (projectedDimension - 1) (columnIndex + 2) $ \rowIndex ->
      annihilateBorderedReductionEntry arena reductionTolerance columnIndex (rowIndex - 1) rowIndex
  where
    projectedDimension = borderedReductionArenaDimension arena

annihilateBorderedReductionEntry ::
  BorderedReductionArena s ->
  Double ->
  Int ->
  Int ->
  Int ->
  ST s ()
annihilateBorderedReductionEntry arena reductionTolerance columnIndex leftIndex rightIndex = do
  targetValue <- readBorderedMatrixEntry arena rightIndex columnIndex
  if abs targetValue <= reductionTolerance
    then do
      writeBorderedMatrixEntry arena rightIndex columnIndex 0.0
      writeBorderedMatrixEntry arena columnIndex rightIndex 0.0
    else do
      pivotValue <- readBorderedMatrixEntry arena leftIndex columnIndex
      let radiusValue = hypotStable pivotValue targetValue
      if radiusValue <= 0.0
        then do
          writeBorderedMatrixEntry arena rightIndex columnIndex 0.0
          writeBorderedMatrixEntry arena columnIndex rightIndex 0.0
        else do
          let cosineValue = pivotValue / radiusValue
              sineValue = targetValue / radiusValue
          applyBorderedReductionGivens arena leftIndex rightIndex cosineValue sineValue
          writeBorderedMatrixEntry arena leftIndex columnIndex radiusValue
          writeBorderedMatrixEntry arena columnIndex leftIndex radiusValue
          writeBorderedMatrixEntry arena rightIndex columnIndex 0.0
          writeBorderedMatrixEntry arena columnIndex rightIndex 0.0

applyBorderedReductionGivens ::
  BorderedReductionArena s ->
  Int ->
  Int ->
  Double ->
  Double ->
  ST s ()
applyBorderedReductionGivens arena leftIndex rightIndex cosineValue sineValue = do
  forIndex 0 projectedDimension $ \columnIndex -> do
    leftEntry <- readBorderedMatrixEntry arena leftIndex columnIndex
    rightEntry <- readBorderedMatrixEntry arena rightIndex columnIndex
    writeBorderedMatrixEntry arena leftIndex columnIndex (cosineValue * leftEntry + sineValue * rightEntry)
    writeBorderedMatrixEntry arena rightIndex columnIndex ((negate sineValue) * leftEntry + cosineValue * rightEntry)
  forIndex 0 projectedDimension $ \rowIndex -> do
    leftEntry <- readBorderedMatrixEntry arena rowIndex leftIndex
    rightEntry <- readBorderedMatrixEntry arena rowIndex rightIndex
    writeBorderedMatrixEntry arena rowIndex leftIndex (cosineValue * leftEntry + sineValue * rightEntry)
    writeBorderedMatrixEntry arena rowIndex rightIndex ((negate sineValue) * leftEntry + cosineValue * rightEntry)
  rotateBorderedReductionBasisColumns arena leftIndex rightIndex cosineValue sineValue
  where
    projectedDimension = borderedReductionArenaDimension arena

rotateBorderedReductionBasisColumns ::
  BorderedReductionArena s ->
  Int ->
  Int ->
  Double ->
  Double ->
  ST s ()
rotateBorderedReductionBasisColumns arena leftIndex rightIndex cosineValue sineValue =
  forIndex 0 projectedDimension $ \rowIndex -> do
    leftEntry <- readBorderedBasisEntry arena rowIndex leftIndex
    rightEntry <- readBorderedBasisEntry arena rowIndex rightIndex
    writeBorderedBasisEntry arena rowIndex leftIndex (cosineValue * leftEntry + sineValue * rightEntry)
    writeBorderedBasisEntry arena rowIndex rightIndex ((negate sineValue) * leftEntry + cosineValue * rightEntry)
  where
    projectedDimension = borderedReductionArenaDimension arena

freezeBorderedReductionDiagonal :: BorderedReductionArena s -> ST s (U.Vector Double)
freezeBorderedReductionDiagonal arena =
  U.generateM projectedDimension $ \entryIndex ->
    readBorderedMatrixEntry arena entryIndex entryIndex
  where
    projectedDimension = borderedReductionArenaDimension arena

freezeBorderedReductionOffDiagonal :: BorderedReductionArena s -> ST s (U.Vector Double)
freezeBorderedReductionOffDiagonal arena =
  U.generateM (max 0 (projectedDimension - 1)) $ \entryIndex ->
    readBorderedMatrixEntry arena entryIndex (entryIndex + 1)
  where
    projectedDimension = borderedReductionArenaDimension arena

freezeBorderedReductionBasis :: BorderedReductionArena s -> ST s (Box.Vector (U.Vector Double))
freezeBorderedReductionBasis arena =
  Box.generateM projectedDimension $ \columnIndex ->
    U.generateM projectedDimension $ \rowIndex ->
      readBorderedBasisEntry arena rowIndex columnIndex
  where
    projectedDimension = borderedReductionArenaDimension arena

writeSymmetricBorderedMatrixEntry :: BorderedReductionArena s -> Int -> Int -> Double -> ST s ()
writeSymmetricBorderedMatrixEntry arena rowIndex columnIndex entryValue = do
  writeBorderedMatrixEntry arena rowIndex columnIndex entryValue
  writeBorderedMatrixEntry arena columnIndex rowIndex entryValue

readBorderedMatrixEntry :: BorderedReductionArena s -> Int -> Int -> ST s Double
readBorderedMatrixEntry arena rowIndex columnIndex =
  readPrimArray (borderedReductionArenaPayload arena) (borderedMatrixEntryOffset arena rowIndex columnIndex)

writeBorderedMatrixEntry :: BorderedReductionArena s -> Int -> Int -> Double -> ST s ()
writeBorderedMatrixEntry arena rowIndex columnIndex entryValue =
  writePrimArray (borderedReductionArenaPayload arena) (borderedMatrixEntryOffset arena rowIndex columnIndex) entryValue

readBorderedBasisEntry :: BorderedReductionArena s -> Int -> Int -> ST s Double
readBorderedBasisEntry arena rowIndex columnIndex =
  readPrimArray (borderedReductionArenaPayload arena) (borderedBasisEntryOffset arena rowIndex columnIndex)

writeBorderedBasisEntry :: BorderedReductionArena s -> Int -> Int -> Double -> ST s ()
writeBorderedBasisEntry arena rowIndex columnIndex entryValue =
  writePrimArray (borderedReductionArenaPayload arena) (borderedBasisEntryOffset arena rowIndex columnIndex) entryValue

borderedMatrixEntryOffset :: BorderedReductionArena s -> Int -> Int -> Int
borderedMatrixEntryOffset arena rowIndex columnIndex =
  borderedReductionArenaMatrixOffset arena + rowIndex * borderedReductionArenaDimension arena + columnIndex

borderedBasisEntryOffset :: BorderedReductionArena s -> Int -> Int -> Int
borderedBasisEntryOffset arena rowIndex columnIndex =
  borderedReductionArenaBasisOffset arena + columnIndex * borderedReductionArenaDimension arena + rowIndex

borderedReductionTolerance :: BorderedProjectedOperator -> Double
borderedReductionTolerance projectedOperator =
  256.0
    * epsDouble
    * sqrt (fromIntegral (max 1 (borderedProjectedOperatorDimension projectedOperator)) :: Double)
    * max 1.0 (borderedProjectedOperatorInfinityBound projectedOperator)

borderedProjectedOperatorInfinityBound :: BorderedProjectedOperator -> Double
borderedProjectedOperatorInfinityBound projectedOperator =
  maximum [1.0, retainedBound, firstKrylovBound, tailKrylovBound]
  where
    retainedValues = borderedRetainedValues projectedOperator
    spikeValues = borderedSpikeCouplings projectedOperator
    krylovDiagonal = borderedKrylovDiagonal projectedOperator
    krylovOffDiagonal = borderedKrylovOffDiagonal projectedOperator
    offDiagonalAt :: Int -> Double
    offDiagonalAt entryIndex = maybe 0.0 abs (krylovOffDiagonal U.!? entryIndex)
    retainedBound =
      if U.null retainedValues
        then 0.0
        else U.maximum (U.zipWith (\value spike -> abs value + abs spike) retainedValues spikeValues)
    firstKrylovBound =
      case krylovDiagonal U.!? 0 of
        Nothing -> 0.0
        Just firstDiagonal -> abs firstDiagonal + U.sum (U.map abs spikeValues) + offDiagonalAt 0
    tailKrylovBound =
      if U.length krylovDiagonal <= 1
        then 0.0
        else
          U.maximum
            ( U.imap
                (\entryIndex diagonalValue -> offDiagonalAt entryIndex + abs diagonalValue + offDiagonalAt (entryIndex + 1))
                (U.drop 1 krylovDiagonal)
            )

projectedResidualEvidence ::
  BorderedProjectedOperator ->
  Double ->
  Double ->
  U.Vector Double ->
  Either MoonlightError Double
projectedResidualEvidence projectedOperator boundaryResidualNorm eigenvalue projectedVector = do
  projectedImage <- applyBorderedProjectedOperatorU projectedOperator projectedVector
  projectedResidual <- subScaledU projectedImage eigenvalue projectedVector
  boundaryCoupling <- projectedBoundaryCoupling projectedOperator boundaryResidualNorm projectedVector
  let projectedNorm = normU projectedResidual
      residualNorm = sqrt (projectedNorm * projectedNorm + boundaryCoupling * boundaryCoupling)
  if finiteDouble residualNorm
    then Right residualNorm
    else Left (InvariantViolation "bordered projected eigensolve produced a non-finite residual")

projectedBoundaryCoupling ::
  BorderedProjectedOperator ->
  Double ->
  U.Vector Double ->
  Either MoonlightError Double
projectedBoundaryCoupling projectedOperator boundaryResidualNorm projectedVector =
  case projectedVector U.!? (borderedProjectedOperatorDimension projectedOperator - 1) of
    Nothing -> Left (InvariantViolation "bordered projected eigenvector boundary coefficient index out of bounds")
    Just coefficient -> Right (boundaryResidualNorm * coefficient)

normalizeProjectedCoefficientVector :: U.Vector Double -> Either MoonlightError (U.Vector Double)
normalizeProjectedCoefficientVector vectorValue =
  let vectorNorm = normU vectorValue
   in if finiteDouble vectorNorm && vectorNorm > 0.0
        then Right (scaleU (1.0 / vectorNorm) vectorValue)
        else Left (InvariantViolation "bordered projected eigensolve produced a degenerate coefficient vector")

unitVector :: Int -> Int -> U.Vector Double
unitVector dimension activeIndex =
  U.generate dimension (\entryIndex -> if entryIndex == activeIndex then 1.0 else 0.0)

orthonormalizeCandidateVectors ::
  Double ->
  [U.Vector Double] ->
  [U.Vector Double] ->
  Either MoonlightError (Box.Vector (U.Vector Double))
orthonormalizeCandidateVectors tolerance lockedVectors candidateVectors =
  Box.fromList . reverse
    <$> foldM appendCandidate [] candidateVectors
  where
    appendCandidate acceptedRev candidateVector = do
      lockedReduced <- projectAgainstVectorListTwice lockedVectors candidateVector
      activeReduced <- projectAgainstVectorListTwice acceptedRev lockedReduced
      let candidateNorm = normU activeReduced
      if finiteDouble candidateNorm && candidateNorm > tolerance
        then Right (scaleU (1.0 / candidateNorm) activeReduced : acceptedRev)
        else Right acceptedRev

canonicalRestartBasis ::
  Double ->
  Int ->
  [U.Vector Double] ->
  Either MoonlightError (Box.Vector (U.Vector Double))
canonicalRestartBasis tolerance ambientDimension lockedVectors =
  case listToMaybe (filter (not . Box.null) candidateBases) of
    Just basisValue -> Right basisValue
    Nothing -> Left (InvariantViolation "restarted Lanczos could not construct a restart vector orthogonal to locked Ritz vectors")
  where
    coordinateVectors =
      U.generate ambientDimension
        <$> [ \rowIndex -> if rowIndex == coordinateIndex then 1.0 else 0.0
            | coordinateIndex <- [0 .. ambientDimension - 1]
            ]
    candidateBases =
      catMaybes
        ( either
            (const Nothing)
            Just
            . orthonormalizeCandidateVectors tolerance lockedVectors
            . pure
            <$> coordinateVectors
        )

projectAgainstVectorListTwice ::
  [U.Vector Double] ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double)
projectAgainstVectorListTwice basisVectors inputVector =
  projectAgainstVectorListOnce basisVectors inputVector >>= projectAgainstVectorListOnce basisVectors

projectAgainstVectorListOnce ::
  [U.Vector Double] ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double)
projectAgainstVectorListOnce basisVectors inputVector =
  foldM projectOne inputVector basisVectors
  where
    projectOne workingVector basisVector = do
      coefficient <- dotU basisVector workingVector
      subScaledU workingVector coefficient basisVector

ritzPairIsLocked :: Double -> Int -> BorderedProjectedOperator -> RitzPair -> Bool
ritzPairIsLocked tolerance ambientDimension projectedOperator ritzPair =
  max (ritzPairResidualNorm ritzPair) (ritzPairProjectedResidualNorm ritzPair)
    <= ritzLockToleranceBound tolerance ambientDimension projectedOperator (ritzPairValue ritzPair)

ritzCandidateIsLocked :: Double -> Int -> BorderedProjectedOperator -> RitzCandidate -> Bool
ritzCandidateIsLocked tolerance ambientDimension projectedOperator candidate =
  ritzCandidateProjectedResidualNorm candidate
    <= ritzLockToleranceBound tolerance ambientDimension projectedOperator (ritzCandidateValue candidate)

ritzLockToleranceBound :: Double -> Int -> BorderedProjectedOperator -> Double -> Double
ritzLockToleranceBound tolerance ambientDimension projectedOperator eigenvalue =
  max
    (ritzLockThreshold tolerance ambientDimension eigenvalue)
    ( inverseIterationResidualToleranceBound
        (borderedProjectedOperatorInfinityBound projectedOperator)
        eigenvalue
        (borderedProjectedOperatorDimension projectedOperator)
    )

ritzLockThreshold :: Double -> Int -> Double -> Double
ritzLockThreshold tolerance ambientDimension eigenvalue =
  max
    tolerance
    (128.0 * epsDouble * sqrt (fromIntegral (max 1 ambientDimension) :: Double) * max 1.0 (abs eigenvalue))

restartGuardCount :: Int -> Int -> Int
restartGuardCount requestedCount capacity =
  max 1 (min requestedCount (max 1 (capacity `quot` 2)))

restartRetainedCount :: Int -> Int -> Int -> Int
restartRetainedCount capacity remainingWanted candidateCount =
  min candidateCount (max 1 (min retainedRoom (remainingWanted + restartGuardCount remainingWanted capacity)))
  where
    retainedRoom =
      if capacity <= 1
        then 1
        else capacity - 1

maxRestartCycles :: Int -> Int -> Int
maxRestartCycles ambientDimension capacity =
  max 1 (4 * max 1 ambientDimension * max 1 (ambientDimension `quot` max 1 capacity))

freezeLanczosState :: LanczosArena s -> LanczosState -> ST s (Either MoonlightError LanczosDecomposition)
freezeLanczosState arena state =
  case state of
    LanczosConverged activeDimension finalResidual ->
      freezeLanczosDecomposition arena activeDimension finalResidual
    LanczosBreakdown activeDimension finalResidual ->
      freezeLanczosDecomposition arena activeDimension finalResidual
    LanczosRestarting activeDimension finalResidual ->
      freezeLanczosDecomposition arena activeDimension finalResidual
    LanczosRunning{} ->
      pure (Left (InvariantViolation "Lanczos reached an unfinished running state"))

freezeLanczosDecomposition :: LanczosArena s -> ActiveDimension -> Double -> ST s (Either MoonlightError LanczosDecomposition)
freezeLanczosDecomposition arena activeDimension finalResidual = do
  basisVectors <- Box.freeze (BoxM.slice 0 activeCount (lanczosBasisArena arena))
  alphaValues <- U.freeze (UM.slice 0 activeCount (lanczosAlphaArena arena))
  betaValues <- U.freeze (UM.slice 0 (max 0 (activeCount - 1)) (lanczosBetaArena arena))
  pure $ do
    projectedTridiagonal <- mkSymmetricTridiagonal (U.toList alphaValues) (U.toList betaValues)
    mkLanczosDecomposition basisVectors projectedTridiagonal finalResidual
  where
    activeCount = activeDimensionValue activeDimension
