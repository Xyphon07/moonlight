{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Spectral.Solve
  ( EigenSolveConfig (..),
    defaultEigenSolveConfig,
    withEigenFallbackLanczosConfig,
    withEigenFallbackInitialVector,
    denseSpectralFallbackDimensionThreshold,
    solveEigenRequest,
  )
where

import Data.Bifunctor (first)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Vector.Storable qualified as S
import Data.Vector.Unboxed qualified as U
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
  )
import Moonlight.LinAlg.Internal.Eigen.Symmetric
  ( SymmetricEigenResult (..),
    symmetricEigenPairsDenseUnchecked,
  )
import Moonlight.LinAlg.Pure.Dense.Flat
  ( DenseDoubleMatrix,
    denseDoubleMatrixToRowMajorVector,
    denseDoubleMatrixVectorProduct,
    mkDenseDoubleMatrixRowMajor,
  )
import Moonlight.LinAlg.Pure.Krylov.Config (LanczosConfig, defaultLanczosConfig, positiveCountValue)
import Moonlight.LinAlg.Pure.Krylov.CascadicGraph
  ( CascadicGraphObstruction (..),
    cascadicGraphLaplacianEigenpairs,
  )
import Moonlight.LinAlg.Pure.Krylov.Projected
  ( projectedEigenpairsFromRestartedLanczos,
    projectedEigenvaluesFromRestartedLanczos,
  )
import Moonlight.LinAlg.Pure.Krylov.SelectedTridiagonal
  ( symmetricTridiagonalFromCSR,
    selectedSymmetricTridiagonalEigenpairsDirect,
    selectedSymmetricTridiagonalEigenvaluesDirect,
  )
import Moonlight.LinAlg.Pure.Krylov.Selection (SpectrumEnd (..))
import Moonlight.LinAlg.Pure.Sparse.Types (SparseCSR)
import Moonlight.LinAlg.Pure.Operator.Internal
  ( LinearOperator (..),
    OperatorSource (..),
    OperatorSymmetry (SelfAdjointOperator),
    operatorDimension,
    runOperatorU,
  )
import Moonlight.LinAlg.Pure.Spectral.Request (EigenRequest (..))
import Moonlight.LinAlg.Pure.Spectral.Result
  ( Eigenpairs,
    eigenpairValues,
    eigenpairsFromColumns,
    mapEigenpairValues,
  )
import Prelude

data DiagonalOrder
  = DiagonalAscending
  | DiagonalDescending

data DiagonalOrderScan = DiagonalOrderScan
  { diagonalScanPrevious :: !Double,
    diagonalScanAscending :: !Bool,
    diagonalScanDescending :: !Bool
  }

data EigenSolveConfig = EigenSolveConfig
  { eigenFallbackLanczosConfig :: !LanczosConfig,
    eigenFallbackInitialVector :: !(Maybe (U.Vector Double))
  }
  deriving stock (Eq, Show)

defaultEigenSolveConfig :: EigenSolveConfig
defaultEigenSolveConfig =
  EigenSolveConfig
    { eigenFallbackLanczosConfig = defaultLanczosConfig,
      eigenFallbackInitialVector = Nothing
    }

withEigenFallbackLanczosConfig :: LanczosConfig -> EigenSolveConfig -> EigenSolveConfig
withEigenFallbackLanczosConfig lanczosConfig config =
  config {eigenFallbackLanczosConfig = lanczosConfig}

withEigenFallbackInitialVector :: U.Vector Double -> EigenSolveConfig -> EigenSolveConfig
withEigenFallbackInitialVector seedVector config =
  config {eigenFallbackInitialVector = Just seedVector}

solveEigenRequest ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  EigenRequest result ->
  Either MoonlightError result
solveEigenRequest config operatorValue requestValue = do
  let dimension = operatorDimension operatorValue
      requestedCount = eigenRequestCount requestValue
      scaleValue = operatorSourceScale operatorValue
      shiftValue = operatorIdentityShift operatorValue
  validateSpectralCount requestedCount dimension
  if scaleValue == 0.0
    then solveZeroScale shiftValue dimension requestValue
    else solveAffineRequest config operatorValue scaleValue shiftValue requestValue

solveAffineRequest ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  Double ->
  Double ->
  EigenRequest result ->
  Either MoonlightError result
solveAffineRequest config operatorValue scaleValue shiftValue requestValue =
  case requestValue of
    EigenvaluesRequest spectrumEnd count ->
      transformValues scaleValue shiftValue
        <$> solveSourceEigenvalues config operatorValue (transportSpectrumEnd scaleValue spectrumEnd) (positiveCountValue count)
    EigenpairsRequest spectrumEnd count ->
      transformPairs scaleValue shiftValue
        =<< solveSourceEigenpairs config operatorValue (transportSpectrumEnd scaleValue spectrumEnd) (positiveCountValue count)

solveSourceEigenvalues ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError (U.Vector Double)
solveSourceEigenvalues config operatorValue spectrumEnd requestedCount =
  case operatorSource operatorValue of
    DiagonalSource diagonalEntries -> diagonalValues spectrumEnd requestedCount diagonalEntries
    PathLaplacianSource dimension -> pathLaplacianValues spectrumEnd requestedCount dimension
    SymmetricTridiagonalSource tridiagonalValue ->
      selectedSymmetricTridiagonalEigenvaluesDirect spectrumEnd requestedCount tridiagonalValue
    SelfAdjointCSRSource csrValue ->
      symmetricTridiagonalFromCSR csrValue >>= \case
        Right tridiagonalValue -> selectedSymmetricTridiagonalEigenvaluesDirect spectrumEnd requestedCount tridiagonalValue
        Left _ -> genericFallbackValues config (sourceOperator operatorValue) spectrumEnd requestedCount
    GraphLaplacianCSRSource csrValue ->
      graphLaplacianFallbackValues
        config
        (sourceOperator operatorValue)
        csrValue
        spectrumEnd
        requestedCount
    DeclaredSelfAdjointSource _ _ -> genericFallbackValues config (sourceOperator operatorValue) spectrumEnd requestedCount

solveSourceEigenpairs ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError Eigenpairs
solveSourceEigenpairs config operatorValue spectrumEnd requestedCount =
  case operatorSource operatorValue of
    DiagonalSource diagonalEntries -> diagonalPairs spectrumEnd requestedCount diagonalEntries
    PathLaplacianSource dimension -> pathLaplacianPairs spectrumEnd requestedCount dimension
    SymmetricTridiagonalSource tridiagonalValue ->
      selectedSymmetricTridiagonalEigenpairsDirect spectrumEnd requestedCount tridiagonalValue
    SelfAdjointCSRSource csrValue ->
      symmetricTridiagonalFromCSR csrValue >>= \case
        Right tridiagonalValue ->
          selectedSymmetricTridiagonalEigenpairsDirect spectrumEnd requestedCount tridiagonalValue
        Left _ -> genericFallbackPairs config (sourceOperator operatorValue) spectrumEnd requestedCount
    GraphLaplacianCSRSource csrValue ->
      graphLaplacianFallbackPairs
        config
        (sourceOperator operatorValue)
        csrValue
        spectrumEnd
        requestedCount
    DeclaredSelfAdjointSource _ _ -> genericFallbackPairs config (sourceOperator operatorValue) spectrumEnd requestedCount

graphLaplacianFallbackValues ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  SparseCSR Double ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError (U.Vector Double)
graphLaplacianFallbackValues config operatorValue csrValue spectrumEnd requestedCount =
  eigenpairValues
    <$> graphLaplacianFallbackPairs
      config
      operatorValue
      csrValue
      spectrumEnd
      requestedCount

graphLaplacianFallbackPairs ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  SparseCSR Double ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError Eigenpairs
graphLaplacianFallbackPairs config operatorValue csrValue spectrumEnd requestedCount
  | shouldUseCascadicGraphFallback spectrumEnd requestedCount operatorValue =
      case
        cascadicGraphLaplacianEigenpairs
          (eigenFallbackLanczosConfig config)
          requestedCount
          csrValue
        of
        Right pairs -> Right pairs
        Left (CascadicGraphCoarseningStalled _) ->
          genericFallbackPairs config operatorValue spectrumEnd requestedCount
        Left obstruction -> Left (cascadicGraphObstructionError obstruction)
  | otherwise = genericFallbackPairs config operatorValue spectrumEnd requestedCount

shouldUseCascadicGraphFallback ::
  SpectrumEnd ->
  Int ->
  LinearOperator 'SelfAdjointOperator ->
  Bool
shouldUseCascadicGraphFallback spectrumEnd requestedCount operatorValue =
  spectrumEnd == SmallestEigenvalues
    && operatorDimension operatorValue >= cascadicGraphFallbackDimensionThreshold
    && requestedCount <= cascadicGraphFallbackModeCountThreshold

cascadicGraphFallbackDimensionThreshold :: Int
cascadicGraphFallbackDimensionThreshold = 4096

cascadicGraphFallbackModeCountThreshold :: Int
cascadicGraphFallbackModeCountThreshold = 8

cascadicGraphObstructionError :: CascadicGraphObstruction -> MoonlightError
cascadicGraphObstructionError obstruction =
  case obstruction of
    CascadicGraphBackendFailure errorValue -> errorValue
    CascadicGraphCoarseningStalled dimension ->
      InvariantViolation
        ( "cascadic graph coarsening stalled at dimension "
            <> show dimension
        )
    CascadicGraphIncompleteAssignment vertexIndex ->
      InvariantViolation
        ( "cascadic graph coarsening omitted vertex "
            <> show vertexIndex
        )
    CascadicGraphInvalidRequest requestedCount dimension ->
      InvariantViolation
        ( "cascadic graph eigensolve requested "
            <> show requestedCount
            <> " modes from dimension "
            <> show dimension
        )
    CascadicGraphRankLoss requiredCount actualCount ->
      InvariantViolation
        ( "cascadic graph refinement lost block rank: required "
            <> show requiredCount
            <> " but retained "
            <> show actualCount
        )
    CascadicGraphRefinementBudgetExceeded dimension residualTarget actualResidual ->
      InvariantViolation
        ( "cascadic graph refinement exhausted its budget at dimension "
            <> show dimension
            <> ": residual target "
            <> show residualTarget
            <> ", actual "
            <> show actualResidual
        )

data GenericSpectralFallback
  = DenseSpectralFallback
  | RestartedLanczosSpectralFallback

-- | Densify generic self-adjoint fallback through n=512; measured banded SPD
-- benches favor dense below this cutoff.
denseSpectralFallbackDimensionThreshold :: Int
denseSpectralFallbackDimensionThreshold = 512

-- | High-demand requests remain cheaper as one bounded dense solve than as
-- hundreds of selected-mode restart cycles.
denseSpectralHighDemandDimensionThreshold :: Int
denseSpectralHighDemandDimensionThreshold = 1024

genericFallbackDispatch :: Int -> LinearOperator 'SelfAdjointOperator -> GenericSpectralFallback
genericFallbackDispatch requestedCount operatorValue
  | dimension <= denseSpectralFallbackDimensionThreshold = DenseSpectralFallback
  | dimension <= denseSpectralHighDemandDimensionThreshold
      && requestedCount >= denseRequestCardinalityFloor dimension = DenseSpectralFallback
  | otherwise = RestartedLanczosSpectralFallback
  where
    dimension = operatorDimension operatorValue

denseRequestCardinalityFloor :: Int -> Int
denseRequestCardinalityFloor dimension =
  max 1 ((dimension + 3) `quot` 4)

genericFallbackValues ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError (U.Vector Double)
genericFallbackValues config operatorValue spectrumEnd requestedCount =
  case genericFallbackDispatch requestedCount operatorValue of
    DenseSpectralFallback -> denseFallbackValues operatorValue spectrumEnd requestedCount
    RestartedLanczosSpectralFallback -> lanczosFallbackValues config operatorValue spectrumEnd requestedCount

genericFallbackPairs ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError Eigenpairs
genericFallbackPairs config operatorValue spectrumEnd requestedCount =
  case genericFallbackDispatch requestedCount operatorValue of
    DenseSpectralFallback -> denseFallbackPairs operatorValue spectrumEnd requestedCount
    RestartedLanczosSpectralFallback -> lanczosFallbackPairs config operatorValue spectrumEnd requestedCount

denseFallbackValues ::
  LinearOperator 'SelfAdjointOperator ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError (U.Vector Double)
denseFallbackValues operatorValue spectrumEnd requestedCount = do
  (_, eigenResult) <- denseFallbackEigenResult operatorValue
  let ascendingValues = symmetricEigenResultValues eigenResult
  pure
    ( U.fromList
        ( (ascendingValues S.!)
            <$> selectedSpectrumIndices spectrumEnd requestedCount (operatorDimension operatorValue)
        )
    )

denseFallbackPairs ::
  LinearOperator 'SelfAdjointOperator ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError Eigenpairs
denseFallbackPairs operatorValue spectrumEnd requestedCount = do
  (denseMatrix, eigenResult) <- denseFallbackEigenResult operatorValue
  let dimension = operatorDimension operatorValue
  columns <-
    traverse
      (denseFallbackPairColumn denseMatrix eigenResult)
      (selectedSpectrumIndices spectrumEnd requestedCount dimension)
  eigenpairsFromColumns dimension columns

denseFallbackEigenResult ::
  LinearOperator 'SelfAdjointOperator ->
  Either MoonlightError (DenseDoubleMatrix, SymmetricEigenResult)
denseFallbackEigenResult operatorValue = do
  let dimension = operatorDimension operatorValue
  entryCount <-
    first
      (const (InvariantViolation "dense spectral fallback cardinality exceeds Int range"))
      (checkedNonNegativeProduct dimension dimension)
  imageColumns <- traverse (runOperatorU operatorValue . unitVector dimension) [0 .. dimension - 1]
  let columnPayload = U.concat imageColumns
      rowMajorPayload =
        S.generate
          entryCount
          ( \flatIndex ->
              let (rowIndex, columnIndex) = flatIndex `quotRem` dimension
               in columnPayload U.! (columnIndex * dimension + rowIndex)
          )
  denseMatrix <- mkDenseDoubleMatrixRowMajor dimension dimension rowMajorPayload
  eigenResult <- symmetricEigenPairsDenseUnchecked dimension denseMatrix
  pure (denseMatrix, eigenResult)

selectedSpectrumIndices :: SpectrumEnd -> Int -> Int -> [Int]
selectedSpectrumIndices spectrumEnd requestedCount dimension =
  case spectrumEnd of
    SmallestEigenvalues -> [0 .. requestedCount - 1]
    LargestEigenvalues -> [dimension - 1, dimension - 2 .. dimension - requestedCount]

denseFallbackPairColumn ::
  DenseDoubleMatrix ->
  SymmetricEigenResult ->
  Int ->
  Either MoonlightError (Double, U.Vector Double, Double)
denseFallbackPairColumn denseMatrix eigenResult columnIndex = do
  let eigenvalue = symmetricEigenResultValues eigenResult S.! columnIndex
      vectorPayload = denseDoubleMatrixToRowMajorVector (symmetricEigenResultVectors eigenResult)
      dimension = S.length (symmetricEigenResultValues eigenResult)
      eigenvector = U.generate dimension (\rowIndex -> vectorPayload S.! (rowIndex * dimension + columnIndex))
  imageVector <- denseDoubleMatrixVectorProduct denseMatrix (S.convert eigenvector)
  pure (eigenvalue, eigenvector, residualNorm eigenvalue eigenvector (S.convert imageVector))

residualNorm :: Double -> U.Vector Double -> U.Vector Double -> Double
residualNorm eigenvalue eigenvector imageVector =
  sqrt
    ( U.sum
        ( U.map
            (\entryValue -> entryValue * entryValue)
            (U.zipWith (\imageEntry vectorEntry -> imageEntry - eigenvalue * vectorEntry) imageVector eigenvector)
        )
    )

lanczosFallbackValues ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError (U.Vector Double)
lanczosFallbackValues config operatorValue spectrumEnd requestedCount =
  projectedEigenvaluesFromRestartedLanczos
    (eigenFallbackLanczosConfig config)
    spectrumEnd
    requestedCount
    operatorValue
    (fallbackSeed config (operatorDimension operatorValue))

lanczosFallbackPairs ::
  EigenSolveConfig ->
  LinearOperator 'SelfAdjointOperator ->
  SpectrumEnd ->
  Int ->
  Either MoonlightError Eigenpairs
lanczosFallbackPairs config operatorValue spectrumEnd requestedCount =
  projectedEigenpairsFromRestartedLanczos
    (eigenFallbackLanczosConfig config)
    spectrumEnd
    requestedCount
    operatorValue
    (fallbackSeed config (operatorDimension operatorValue))

fallbackSeed :: EigenSolveConfig -> Int -> U.Vector Double
fallbackSeed config dimension =
  case eigenFallbackInitialVector config of
    Just seedVector -> seedVector
    Nothing -> U.generate dimension (\indexValue -> if indexValue == 0 then 1.0 else 0.0)

sourceOperator :: LinearOperator 'SelfAdjointOperator -> LinearOperator 'SelfAdjointOperator
sourceOperator operatorValue =
  operatorValue {operatorSourceScale = 1.0, operatorIdentityShift = 0.0}

diagonalValues :: SpectrumEnd -> Int -> U.Vector Double -> Either MoonlightError (U.Vector Double)
diagonalValues spectrumEnd requestedCount diagonalEntries =
  Right (diagonalSelectedValues spectrumEnd requestedCount diagonalEntries)

diagonalPairs :: SpectrumEnd -> Int -> U.Vector Double -> Either MoonlightError Eigenpairs
diagonalPairs spectrumEnd requestedCount diagonalEntries =
  eigenpairsFromColumns (U.length diagonalEntries)
    ( fmap
        (\(entryIndex, eigenvalue) -> (eigenvalue, unitVector (U.length diagonalEntries) entryIndex, 0.0))
        (diagonalSelectedEntries spectrumEnd requestedCount diagonalEntries)
    )

diagonalSelectedValues :: SpectrumEnd -> Int -> U.Vector Double -> U.Vector Double
diagonalSelectedValues spectrumEnd requestedCount diagonalEntries =
  case diagonalOrder diagonalEntries of
    Just DiagonalAscending -> orderedAscendingValues spectrumEnd requestedCount diagonalEntries
    Just DiagonalDescending -> orderedDescendingValues spectrumEnd requestedCount diagonalEntries
    Nothing ->
      U.fromList . fmap snd $
        diagonalSelectedEntriesBySort spectrumEnd requestedCount diagonalEntries

diagonalSelectedEntries :: SpectrumEnd -> Int -> U.Vector Double -> [(Int, Double)]
diagonalSelectedEntries spectrumEnd requestedCount diagonalEntries =
  case diagonalOrder diagonalEntries of
    Just DiagonalAscending -> orderedAscendingEntries spectrumEnd requestedCount diagonalEntries
    Just DiagonalDescending -> orderedDescendingEntries spectrumEnd requestedCount diagonalEntries
    Nothing -> diagonalSelectedEntriesBySort spectrumEnd requestedCount diagonalEntries

diagonalOrder :: U.Vector Double -> Maybe DiagonalOrder
diagonalOrder diagonalEntries
  | U.length diagonalEntries <= 1 = Just DiagonalAscending
  | otherwise =
      orderFromScan
        ( U.foldl'
            scanDiagonalOrder
            (DiagonalOrderScan (diagonalEntries `U.unsafeIndex` 0) True True)
            (U.drop 1 diagonalEntries)
        )

scanDiagonalOrder :: DiagonalOrderScan -> Double -> DiagonalOrderScan
scanDiagonalOrder scanValue entryValue =
  DiagonalOrderScan
    { diagonalScanPrevious = entryValue,
      diagonalScanAscending = diagonalScanAscending scanValue && diagonalScanPrevious scanValue <= entryValue,
      diagonalScanDescending = diagonalScanDescending scanValue && diagonalScanPrevious scanValue >= entryValue
    }

orderFromScan :: DiagonalOrderScan -> Maybe DiagonalOrder
orderFromScan scanValue
  | diagonalScanAscending scanValue = Just DiagonalAscending
  | diagonalScanDescending scanValue = Just DiagonalDescending
  | otherwise = Nothing

orderedAscendingValues :: SpectrumEnd -> Int -> U.Vector Double -> U.Vector Double
orderedAscendingValues spectrumEnd requestedCount diagonalEntries =
  case spectrumEnd of
    SmallestEigenvalues -> U.take requestedCount diagonalEntries
    LargestEigenvalues -> U.reverse (U.drop (U.length diagonalEntries - requestedCount) diagonalEntries)

orderedDescendingValues :: SpectrumEnd -> Int -> U.Vector Double -> U.Vector Double
orderedDescendingValues spectrumEnd requestedCount diagonalEntries =
  case spectrumEnd of
    SmallestEigenvalues -> U.reverse (U.drop (U.length diagonalEntries - requestedCount) diagonalEntries)
    LargestEigenvalues -> U.take requestedCount diagonalEntries

orderedAscendingEntries :: SpectrumEnd -> Int -> U.Vector Double -> [(Int, Double)]
orderedAscendingEntries spectrumEnd requestedCount diagonalEntries =
  diagonalEntriesAt
    diagonalEntries
    ( case spectrumEnd of
        SmallestEigenvalues -> [0 .. requestedCount - 1]
        LargestEigenvalues -> [U.length diagonalEntries - 1, U.length diagonalEntries - 2 .. U.length diagonalEntries - requestedCount]
    )

orderedDescendingEntries :: SpectrumEnd -> Int -> U.Vector Double -> [(Int, Double)]
orderedDescendingEntries spectrumEnd requestedCount diagonalEntries =
  diagonalEntriesAt
    diagonalEntries
    ( case spectrumEnd of
        SmallestEigenvalues -> [U.length diagonalEntries - 1, U.length diagonalEntries - 2 .. U.length diagonalEntries - requestedCount]
        LargestEigenvalues -> [0 .. requestedCount - 1]
    )

diagonalEntriesAt :: U.Vector Double -> [Int] -> [(Int, Double)]
diagonalEntriesAt diagonalEntries =
  fmap (\entryIndex -> (entryIndex, diagonalEntries `U.unsafeIndex` entryIndex))

diagonalSelectedEntriesBySort :: SpectrumEnd -> Int -> U.Vector Double -> [(Int, Double)]
diagonalSelectedEntriesBySort spectrumEnd requestedCount diagonalEntries =
  take requestedCount (sortIndexedValues spectrumEnd (U.toList (U.indexed diagonalEntries)))

pathLaplacianValues :: SpectrumEnd -> Int -> Int -> Either MoonlightError (U.Vector Double)
pathLaplacianValues spectrumEnd requestedCount dimension =
  Right
    ( U.generate
        requestedCount
        ( \entryIndex ->
            pathLaplacianEigenvalueAt dimension $
              case spectrumEnd of
                SmallestEigenvalues -> entryIndex
                LargestEigenvalues -> dimension - entryIndex - 1
        )
    )

pathLaplacianPairs :: SpectrumEnd -> Int -> Int -> Either MoonlightError Eigenpairs
pathLaplacianPairs spectrumEnd requestedCount dimension =
  eigenpairsFromColumns dimension $
    pathLaplacianColumn dimension <$> selectedModeIndices spectrumEnd requestedCount dimension

pathLaplacianColumn :: Int -> Int -> (Double, U.Vector Double, Double)
pathLaplacianColumn dimension modeIndex =
  let eigenvalue = pathLaplacianEigenvalueAt dimension modeIndex
      theta = pi * fromIntegral modeIndex / fromIntegral (max 1 dimension)
      eigenvector =
        if modeIndex == 0
          then U.replicate dimension (1.0 / sqrt (fromIntegral (max 1 dimension)))
          else
            let normalizer = sqrt (2.0 / fromIntegral dimension)
             in U.generate dimension (\rowIndex -> normalizer * cos (theta * (fromIntegral rowIndex + 0.5)))
   in (eigenvalue, eigenvector, pathLaplacianResidualNorm dimension eigenvalue eigenvector)

pathLaplacianResidualNorm :: Int -> Double -> U.Vector Double -> Double
pathLaplacianResidualNorm dimension eigenvalue eigenvector =
  sqrt
    ( U.ifoldl'
        ( \squaredNorm rowIndex _ ->
            let residualEntry = pathLaplacianResidualEntry dimension eigenvalue eigenvector rowIndex
             in squaredNorm + residualEntry * residualEntry
        )
        0.0
        eigenvector
    )

pathLaplacianResidualEntry :: Int -> Double -> U.Vector Double -> Int -> Double
pathLaplacianResidualEntry dimension eigenvalue eigenvector rowIndex =
  let centerValue = eigenvector `U.unsafeIndex` rowIndex
      degree
        | dimension == 1 = 0.0
        | rowIndex == 0 || rowIndex + 1 == dimension = 1.0
        | otherwise = 2.0
      leftValue =
        if rowIndex <= 0
          then 0.0
          else eigenvector `U.unsafeIndex` (rowIndex - 1)
      rightValue =
        if rowIndex + 1 >= dimension
          then 0.0
          else eigenvector `U.unsafeIndex` (rowIndex + 1)
      imageValue = degree * centerValue - leftValue - rightValue
   in imageValue - eigenvalue * centerValue
{-# INLINE pathLaplacianResidualEntry #-}

selectedModeIndices :: SpectrumEnd -> Int -> Int -> [Int]
selectedModeIndices spectrumEnd requestedCount dimension =
  case spectrumEnd of
    SmallestEigenvalues -> [0 .. requestedCount - 1]
    LargestEigenvalues -> [dimension - 1, dimension - 2 .. dimension - requestedCount]

pathLaplacianEigenvalueAt :: Int -> Int -> Double
pathLaplacianEigenvalueAt matrixSize modeIndex =
  2.0 - 2.0 * cos (pi * fromIntegral modeIndex / fromIntegral (max 1 matrixSize))

sortIndexedValues :: SpectrumEnd -> [(Int, Double)] -> [(Int, Double)]
sortIndexedValues spectrumEnd =
  sortBy
    ( case spectrumEnd of
        SmallestEigenvalues -> comparing snd
        LargestEigenvalues -> flip (comparing snd)
    )

transformValues :: Double -> Double -> U.Vector Double -> U.Vector Double
transformValues scaleValue shiftValue =
  U.map (\eigenvalue -> scaleValue * eigenvalue + shiftValue)

transformPairs :: Double -> Double -> Eigenpairs -> Either MoonlightError Eigenpairs
transformPairs scaleValue shiftValue =
  mapEigenpairValues
    (\eigenvalue -> scaleValue * eigenvalue + shiftValue)
    (abs scaleValue *)

transportSpectrumEnd :: Double -> SpectrumEnd -> SpectrumEnd
transportSpectrumEnd scaleValue spectrumEnd
  | scaleValue < 0.0 =
      case spectrumEnd of
        SmallestEigenvalues -> LargestEigenvalues
        LargestEigenvalues -> SmallestEigenvalues
  | otherwise = spectrumEnd

solveZeroScale :: Double -> Int -> EigenRequest result -> Either MoonlightError result
solveZeroScale eigenvalue dimension requestValue =
  case requestValue of
    EigenvaluesRequest _ count -> Right (U.replicate (positiveCountValue count) eigenvalue)
    EigenpairsRequest _ count ->
      eigenpairsFromColumns
        dimension
        ((\entryIndex -> (eigenvalue, unitVector dimension entryIndex, 0.0)) <$> [0 .. positiveCountValue count - 1])

eigenRequestCount :: EigenRequest result -> Int
eigenRequestCount requestValue =
  case requestValue of
    EigenvaluesRequest _ count -> positiveCountValue count
    EigenpairsRequest _ count -> positiveCountValue count

validateSpectralCount :: Int -> Int -> Either MoonlightError ()
validateSpectralCount requestedCount dimension
  | dimension <= 0 = Left (InvariantViolation "spectral solve requires a positive operator dimension")
  | requestedCount <= 0 = Left (InvariantViolation "spectral request count must be positive")
  | requestedCount > dimension = Left (InvariantViolation "spectral request count exceeds operator dimension")
  | otherwise = Right ()

unitVector :: Int -> Int -> U.Vector Double
unitVector dimension selectedIndex =
  U.generate dimension (\entryIndex -> if entryIndex == selectedIndex then 1.0 else 0.0)
