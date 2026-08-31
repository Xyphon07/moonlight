{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Homology.Pure.Topology.Spectral
  ( graphLaplacian,
    laplacianEntry,
    weightedGraphLaplacian,
    weightedGraphSparseLaplacian,
    SparseSpectralConfig (..),
    defaultSparseSpectralConfig,
    weightedGraphSpectralModes,
    weightedGraphSparseSpectralModes,
    weightedGraphSpectralGap,
    gapFromModes,
    leadingModeTransport,
    smallestEigenpairs,
    largestEigenpairs,
    powerIteration,
    traceMatrix,
    subtractFromDiagonal,
    basisVector,
  )
where

import Data.Function ((&))
import Data.Bifunctor (first)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U
import Moonlight.Core (spectralGap)
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Topology.Core
import Moonlight.LinAlg.Internal.Primitives (dotProduct, matrixVectorProduct, scaleVector, subVector, vectorNorm)
import Moonlight.LinAlg.Pure.Dense.Decomposition (symmetricEigenPairs)
import Moonlight.LinAlg.Pure.Krylov.Config (mkPositiveCount)
import Moonlight.LinAlg.Pure.Krylov.Selection (SpectrumEnd (SmallestEigenvalues))
import Moonlight.LinAlg.Pure.Operator (graphLaplacianLinearOperator)
import Moonlight.LinAlg.Pure.Sparse.Structured qualified as LinAlgSparse
import Moonlight.LinAlg.Pure.Sparse.Types (SparseCSR)
import Moonlight.LinAlg.Pure.Spectral.Request (EigenRequest (..))
import Moonlight.LinAlg.Pure.Spectral.Result (Eigenpairs, eigenpairValues, eigenpairVectorAt)
import Moonlight.LinAlg.Pure.Spectral.Solve
  ( EigenSolveConfig,
    defaultEigenSolveConfig,
    solveEigenRequest,
    withEigenFallbackInitialVector,
  )

data SparseSpectralConfig = SparseSpectralConfig
  { sscEigenSolveConfig :: !EigenSolveConfig
  }
  deriving stock (Eq, Show)

defaultSparseSpectralConfig :: SparseSpectralConfig
defaultSparseSpectralConfig =
  SparseSpectralConfig
    { sscEigenSolveConfig = defaultEigenSolveConfig
    }

graphLaplacian :: Graph1Skeleton -> [[Double]]
graphLaplacian skeleton =
  let vertexCount = graphVertexCount skeleton
      adjacency = graphAdjacency skeleton
   in enumerateFromZero vertexCount
        & fmap
          ( \rowIndexValue ->
              enumerateFromZero vertexCount
                & fmap
                  ( \columnIndexValue ->
                      laplacianEntry adjacency rowIndexValue columnIndexValue
                  )
          )

laplacianEntry :: Map.Map Int (Set.Set Int) -> Int -> Int -> Double
laplacianEntry adjacency rowIndexValue columnIndexValue =
  let degreeValue =
        adjacency
          & Map.findWithDefault Set.empty rowIndexValue
          & Set.size
          & fromIntegral
   in if rowIndexValue == columnIndexValue
        then degreeValue
        else
          if Set.member columnIndexValue (Map.findWithDefault Set.empty rowIndexValue adjacency)
            then -1.0
            else 0.0

weightedGraphLaplacian :: Int -> [(Int, Int, Double)] -> [[Double]]
weightedGraphLaplacian vertexCount weightedEdges =
  let adjacencyWeights = symmetricAdjacencyWeights vertexCount weightedEdges
      degreeWeights =
        Map.toList adjacencyWeights
          & foldr
            (\((sourceVertex, _), edgeWeight) -> Map.insertWith (+) sourceVertex edgeWeight)
            Map.empty
   in enumerateFromZero vertexCount
        & fmap
          (\rowIndexValue ->
             enumerateFromZero vertexCount
               & fmap (weightedLaplacianEntry adjacencyWeights degreeWeights rowIndexValue)
          )

weightedGraphSparseLaplacian :: Int -> [(Int, Int, Double)] -> Either HomologyFailure (SparseCSR Double)
weightedGraphSparseLaplacian vertexCount weightedEdges =
  if vertexCount < 0
    then Left (BackendFailure "weighted graph sparse Laplacian requires a non-negative vertex count")
    else
      LinAlgSparse.graphLaplacianCSR
        (enumerateFromZero vertexCount)
        (cleanedSparseGraphEdges vertexCount weightedEdges)
        & first (BackendFailure . show)

weightedGraphSpectralModes ::
  Int ->
  Int ->
  [(Int, Int, Double)] ->
  Either HomologyFailure [GraphSpectralMode]
weightedGraphSpectralModes requestedModeCount vertexCount weightedEdges
  | requestedModeCount <= 0 = Right []
  | vertexCount <= 0 = Right []
  | otherwise =
      let laplacian = weightedGraphLaplacian vertexCount weightedEdges
       in fmap (fmap toSpectralMode) (smallestEigenpairs requestedModeCount laplacian)

weightedGraphSparseSpectralModes ::
  SparseSpectralConfig ->
  Int ->
  Int ->
  [(Int, Int, Double)] ->
  Either HomologyFailure [GraphSpectralMode]
weightedGraphSparseSpectralModes config requestedModeCount vertexCount weightedEdges
  | requestedModeCount <= 0 = Right []
  | vertexCount <= 0 = Right []
  | otherwise = do
      let modeCount = min requestedModeCount vertexCount
          solveConfig =
            withEigenFallbackInitialVector
              (sparseSpectralSeed vertexCount)
              (sscEigenSolveConfig config)
      countValue <- first (BackendFailure . show) (mkPositiveCount modeCount)
      operatorValue <-
        first
          (BackendFailure . show)
          ( graphLaplacianLinearOperator
              (enumerateFromZero vertexCount)
              (cleanedSparseGraphEdges vertexCount weightedEdges)
          )
      eigenpairs <-
        solveEigenRequest
          solveConfig
          operatorValue
          (EigenpairsRequest SmallestEigenvalues countValue)
          & first (BackendFailure . show)
      eigenpairSpectralPairs eigenpairs
        & fmap (fmap toSpectralMode)

weightedGraphSpectralGap ::
  Int ->
  Int ->
  [(Int, Int, Double)] ->
  Either HomologyFailure (Maybe Double)
weightedGraphSpectralGap requestedModeCount vertexCount weightedEdges =
  fmap
    (spectralGap . fmap spectralEigenvalue)
    (weightedGraphSparseSpectralModes defaultSparseSpectralConfig requestedModeCount vertexCount weightedEdges)

leadingModeTransport :: [GraphSpectralMode] -> [GraphSpectralMode] -> Maybe Double
leadingModeTransport leftModes rightModes = do
  leftMode <- preferredMode leftModes
  rightMode <- preferredMode rightModes
  modeCosineSimilarity leftMode rightMode

smallestEigenpairs :: Int -> [[Double]] -> Either HomologyFailure [(Double, [Double])]
smallestEigenpairs requestedModeCount matrixRows =
  sortedSymmetricEigenpairs requestedModeCount matrixRows
    & fmap (List.sortOn fst)

largestEigenpairs :: Int -> [[Double]] -> Either HomologyFailure [(Double, [Double])]
largestEigenpairs requestedModeCount matrixRows =
  sortedSymmetricEigenpairs requestedModeCount matrixRows
    & fmap (List.sortOn (Down . fst))

sortedSymmetricEigenpairs :: Int -> [[Double]] -> Either HomologyFailure [(Double, [Double])]
sortedSymmetricEigenpairs requestedModeCount matrixRows =
  let matrixSize = length matrixRows
      modeCount = min requestedModeCount matrixSize
   in first
        (BackendFailure . show)
        (symmetricEigenPairs matrixSize matrixRows)
        & fmap (take modeCount)

powerIteration :: Int -> Double -> [[Double]] -> [Double] -> Either HomologyFailure (Double, [Double])
powerIteration iterationLimit tolerance matrixRows initialVector = do
  seedNorm <- liftLinAlg (vectorNorm initialVector)
  let seedVector =
        if seedNorm <= tolerance
          then basisVector (length initialVector) 0
          else scaleVector (1.0 / seedNorm) initialVector
  iterateStep 0 seedVector
  where
    liftLinAlg :: Show error => Either error value -> Either HomologyFailure value
    liftLinAlg = first (BackendFailure . show)
    iterateStep iterationIndex vectorValue
      | iterationIndex >= iterationLimit = Left (NonConvergent iterationLimit)
      | otherwise = do
          projectedVector <- liftLinAlg (matrixVectorProduct matrixRows vectorValue)
          projectedNorm <- liftLinAlg (vectorNorm projectedVector)
          if projectedNorm <= tolerance
            then Right (0.0, vectorValue)
            else do
              let nextVector = scaleVector (1.0 / projectedNorm) projectedVector
              imageVector <- liftLinAlg (matrixVectorProduct matrixRows nextVector)
              eigenvalueValue <- liftLinAlg (dotProduct nextVector imageVector)
              residualVector <- liftLinAlg (subVector imageVector (scaleVector eigenvalueValue nextVector))
              residualNorm <- liftLinAlg (vectorNorm residualVector)
              if residualNorm <= tolerance
                then Right (eigenvalueValue, nextVector)
                else iterateStep (iterationIndex + 1) nextVector

toSpectralMode :: (Double, [Double]) -> GraphSpectralMode
toSpectralMode (eigenvalueValue, eigenVectorValue) =
  let supportedCoefficients =
        eigenVectorValue
          & zip [0 :: Int ..]
          & filter (\(_, coefficientValue) -> abs coefficientValue > 1.0e-10)
      positiveSupport =
        supportedCoefficients
          & filter (\(_, coefficientValue) -> coefficientValue > 1.0e-10)
          & fmap fst
      negativeSupport =
        supportedCoefficients
          & filter (\(_, coefficientValue) -> coefficientValue < -1.0e-10)
          & fmap fst
      posSize = length positiveSupport
      negSize = length negativeSupport
      criticality = 1.0 / (1.0 + log (fromIntegral (1 + min posSize negSize)))
   in GraphSpectralMode
        { spectralEigenvalue = eigenvalueValue,
          spectralCoefficients = supportedCoefficients,
          spectralPositiveSupport = positiveSupport,
          spectralNegativeSupport = negativeSupport,
          spectralSupportCriticality = criticality
        }

traceMatrix :: [[Double]] -> Double
traceMatrix matrixRows =
  matrixRows
    & zip [0 :: Int ..]
    & foldl'
      ( \accumulator (rowIndexValue, rowValue) ->
          accumulator
            + case drop rowIndexValue rowValue of
              entryValue : _ -> entryValue
              [] -> 0.0
      )
      0.0

-- | @subtractFromDiagonal d m@ computes @d·I − m@ (note the orientation:
-- the matrix is subtracted FROM the scaled identity, not the reverse).
subtractFromDiagonal :: Double -> [[Double]] -> [[Double]]
subtractFromDiagonal diagonalValue matrixRows =
  matrixRows
    & zip [0 :: Int ..]
    & fmap
      ( \(rowIndexValue, rowValue) ->
          rowValue
            & zip [0 :: Int ..]
            & fmap
              ( \(columnIndexValue, entryValue) ->
                  if rowIndexValue == columnIndexValue
                    then diagonalValue - entryValue
                    else negate entryValue
              )
      )

basisVector :: Int -> Int -> [Double]
basisVector vectorSize selectedIndex =
  enumerateFromZero vectorSize
    & fmap (\indexValue -> if indexValue == selectedIndex then 1.0 else 0.0)

-- | Symmetrized edge weights with a documented cleaning contract: entries
-- with non-positive weight, out-of-range endpoints, or equal endpoints
-- (self-loops) are silently dropped rather than rejected — the Laplacian
-- consumers treat such input as absent edges. Callers needing validation
-- must check before lowering.
symmetricAdjacencyWeights :: Int -> [(Int, Int, Double)] -> Map.Map (Int, Int) Double
symmetricAdjacencyWeights vertexCount =
  foldr insertWeight Map.empty
  where
    insertWeight (sourceVertex, targetVertex, edgeWeight) accumulatedWeights
      | edgeWeight <= 0.0 = accumulatedWeights
      | sourceVertex < 0 || targetVertex < 0 = accumulatedWeights
      | sourceVertex >= vertexCount || targetVertex >= vertexCount = accumulatedWeights
      | sourceVertex == targetVertex = accumulatedWeights
      | otherwise =
          Map.insertWith (+) (sourceVertex, targetVertex) edgeWeight
            . Map.insertWith (+) (targetVertex, sourceVertex) edgeWeight
            $ accumulatedWeights

weightedLaplacianEntry ::
  Map.Map (Int, Int) Double ->
  Map.Map Int Double ->
  Int ->
  Int ->
  Double
weightedLaplacianEntry adjacencyWeights degreeWeights rowIndexValue columnIndexValue =
  if rowIndexValue == columnIndexValue
    then Map.findWithDefault 0.0 rowIndexValue degreeWeights
    else negate (Map.findWithDefault 0.0 (rowIndexValue, columnIndexValue) adjacencyWeights)

cleanedSparseGraphEdges ::
  Int ->
  [(Int, Int, Double)] ->
  [LinAlgSparse.GraphEdge Int]
cleanedSparseGraphEdges vertexCount =
  mapMaybe
    ( \(sourceVertex, targetVertex, edgeWeight) ->
        if edgeWeight <= 0.0
          || sourceVertex < 0
          || targetVertex < 0
          || sourceVertex >= vertexCount
          || targetVertex >= vertexCount
          || sourceVertex == targetVertex
          then Nothing
          else
            Just
              ( LinAlgSparse.GraphEdge
                  sourceVertex
                  targetVertex
                  edgeWeight
              )
    )

sparseSpectralSeed :: Int -> U.Vector Double
sparseSpectralSeed vertexCount =
  if vertexCount <= 0
    then U.empty
    else
      U.generate
        vertexCount
        (\indexValue -> 1.0 / fromIntegral (indexValue + 1))

eigenpairSpectralPairs :: Eigenpairs -> Either HomologyFailure [(Double, [Double])]
eigenpairSpectralPairs pairs =
  traverse eigenpairSpectralPair (U.toList (U.indexed (eigenpairValues pairs)))
  where
    eigenpairSpectralPair (columnIndex, eigenvalue) =
      eigenpairVectorAt columnIndex pairs
        & first (BackendFailure . show)
        & fmap (\eigenvector -> (eigenvalue, U.toList eigenvector))

preferredMode :: [GraphSpectralMode] -> Maybe GraphSpectralMode
preferredMode spectralModes =
  case filter ((> 1.0e-10) . spectralEigenvalue) spectralModes of
    preferredValue : _ -> Just preferredValue
    [] ->
      case spectralModes of
        firstMode : _ -> Just firstMode
        [] -> Nothing

modeCosineSimilarity :: GraphSpectralMode -> GraphSpectralMode -> Maybe Double
modeCosineSimilarity leftMode rightMode =
  let leftCoefficients = coefficientMap leftMode
      rightCoefficients = coefficientMap rightMode
      supportKeys = Set.union (Map.keysSet leftCoefficients) (Map.keysSet rightCoefficients)
      dotValue =
        supportKeys
          & foldr
            (\cellIndexValue ->
               (+)
                 ( Map.findWithDefault 0.0 cellIndexValue leftCoefficients
                     * Map.findWithDefault 0.0 cellIndexValue rightCoefficients
                 )
            )
            0.0
      leftNorm = normOf leftCoefficients
      rightNorm = normOf rightCoefficients
   in if leftNorm <= 1.0e-10 || rightNorm <= 1.0e-10
        then Nothing
        else Just (abs dotValue / (leftNorm * rightNorm))

coefficientMap :: GraphSpectralMode -> Map.Map Int Double
coefficientMap =
  Map.fromList . spectralCoefficients

normOf :: Map.Map Int Double -> Double
normOf coefficientValues =
  coefficientValues
    & Map.elems
    & fmap (\coefficientValue -> coefficientValue * coefficientValue)
    & sum
    & sqrt

gapFromModes :: [GraphSpectralMode] -> Maybe Double
gapFromModes = spectralGap . fmap spectralEigenvalue
