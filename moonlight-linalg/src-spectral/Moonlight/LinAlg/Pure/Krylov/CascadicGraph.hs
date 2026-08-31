{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Krylov.CascadicGraph
  ( CascadicGraphObstruction (..),
    cascadicGraphLaplacianEigenpairs,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Box
import Data.Vector.Storable qualified as S
import Data.Vector.Unboxed qualified as U
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
    fieldValueValid,
  )
import Moonlight.LinAlg.Internal.Eigen.Kernels (epsDouble)
import Moonlight.LinAlg.Internal.Eigen.Symmetric
  ( SymmetricEigenResult (..),
    symmetricEigenPairsDenseUnchecked,
  )
import Moonlight.LinAlg.Internal.VectorOps (normU, subScaledU)
import Moonlight.LinAlg.Pure.Dense.Flat
  ( denseDoubleMatrixToRowMajorVector,
    mkDenseDoubleMatrixRowMajor,
  )
import Moonlight.LinAlg.Pure.Krylov.Config
  ( LanczosConfig,
    lanczosIterations,
    lanczosTolerance,
  )
import Moonlight.LinAlg.Pure.Krylov.Internal
  ( linearCombinationColumnsU,
    orthonormalizeBlock,
  )
import Moonlight.LinAlg.Pure.Sparse.Assembly (canonicalCSRFromEntries)
import Moonlight.LinAlg.Pure.Sparse.Solver.Common (sparseDiagonal)
import Moonlight.LinAlg.Pure.Sparse.Structured
  ( GraphEdge (..),
    graphLaplacianCSR,
  )
import Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCSR,
    cooEntries,
    csrCols,
    csrMatVecVector,
    csrRows,
    csrToCOO,
  )
import Moonlight.LinAlg.Pure.Spectral.Result
  ( Eigenpairs,
    eigenpairsFromColumns,
  )
import Prelude

type CascadicGraphObstruction :: Type
data CascadicGraphObstruction
  = CascadicGraphBackendFailure !MoonlightError
  | CascadicGraphCoarseningStalled !Int
  | CascadicGraphIncompleteAssignment !Int
  | CascadicGraphInvalidRequest !Int !Int
  | CascadicGraphRankLoss !Int !Int
  | CascadicGraphRefinementBudgetExceeded !Int !Double !Double
  deriving stock (Eq, Show)

type GraphLaplacianLevel :: Type
data GraphLaplacianLevel = GraphLaplacianLevel
  { graphLevelMasses :: !(U.Vector Double),
    graphLevelEdges :: !(Box.Vector (GraphEdge Int)),
    graphLevelMatrix :: !(SparseCSR Double)
  }

type GraphAggregation :: Type
data GraphAggregation = GraphAggregation
  { graphAggregationFineToCoarse :: !(U.Vector Int),
    graphAggregationCoarseMasses :: !(U.Vector Double),
    graphAggregationCoarseEdges :: !(Box.Vector (GraphEdge Int))
  }

type AggregationFold :: Type
data AggregationFold = AggregationFold
  { aggregationAssignments :: !(IntMap Int),
    aggregationNextIndex :: !Int
  }

type WeightedNeighbor :: Type
data WeightedNeighbor = WeightedNeighbor
  { weightedNeighborVertex :: !Int,
    weightedNeighborEdgeWeight :: !Double
  }

type RitzBlock :: Type
data RitzBlock = RitzBlock
  { ritzBlockValues :: !(U.Vector Double),
    ritzBlockColumns :: !(Box.Vector (U.Vector Double)),
    ritzBlockResidualVectors :: !(Box.Vector (U.Vector Double)),
    ritzBlockResidualNorms :: !(U.Vector Double)
  }

type RefinementState :: Type
data RefinementState = RefinementState
  { refinementStepCount :: !Int,
    refinementRitzBlock :: !RitzBlock
  }

cascadicGraphLaplacianEigenpairs ::
  LanczosConfig ->
  Int ->
  SparseCSR Double ->
  Either CascadicGraphObstruction Eigenpairs
cascadicGraphLaplacianEigenpairs config requestedCount fineMatrix = do
  validateCascadicRequest requestedCount fineMatrix
  fineLevel <- initialGraphLaplacianLevel fineMatrix
  if Box.null (graphLevelEdges fineLevel)
    then zeroGraphEigenpairs requestedCount (csrRows fineMatrix)
    else do
      refinementLimit <- cascadicRefinementLimit config
      finalBlock <-
        solveCascadicLevel
          requestedCount
          refinementLimit
          (cascadicResidualTarget config (csrRows fineMatrix))
          True
          fineLevel
      ritzBlockToEigenpairs finalBlock

validateCascadicRequest ::
  Int ->
  SparseCSR Double ->
  Either CascadicGraphObstruction ()
validateCascadicRequest requestedCount matrixValue
  | csrRows matrixValue <= 0 || csrRows matrixValue /= csrCols matrixValue =
      Left
        ( CascadicGraphBackendFailure
            (InvariantViolation "cascadic graph eigensolve requires a positive square matrix")
        )
  | requestedCount <= 0 || requestedCount > csrRows matrixValue =
      Left (CascadicGraphInvalidRequest requestedCount (csrRows matrixValue))
  | otherwise = Right ()

initialGraphLaplacianLevel ::
  SparseCSR Double ->
  Either CascadicGraphObstruction GraphLaplacianLevel
initialGraphLaplacianLevel matrixValue = do
  coordinateValue <- first CascadicGraphBackendFailure (csrToCOO matrixValue)
  let dimension = csrRows matrixValue
      edges =
        Box.fromList
          [ GraphEdge rowIndex columnIndex (negate entryValue)
            | (rowIndex, columnIndex, entryValue) <- cooEntries coordinateValue,
              rowIndex < columnIndex,
              entryValue < 0.0
          ]
  pure
    GraphLaplacianLevel
      { graphLevelMasses = U.replicate dimension 1.0,
        graphLevelEdges = edges,
        graphLevelMatrix = matrixValue
      }

zeroGraphEigenpairs ::
  Int ->
  Int ->
  Either CascadicGraphObstruction Eigenpairs
zeroGraphEigenpairs requestedCount dimension =
  first CascadicGraphBackendFailure
    ( eigenpairsFromColumns
        dimension
        [ (0.0, unitVector dimension columnIndex, 0.0)
          | columnIndex <- [0 .. requestedCount - 1]
        ]
    )

solveCascadicLevel ::
  Int ->
  Int ->
  Double ->
  Bool ->
  GraphLaplacianLevel ->
  Either CascadicGraphObstruction RitzBlock
solveCascadicLevel requestedCount refinementLimit residualTarget isFinest levelValue
  | graphLevelDimension levelValue <= cascadicCoarsestDimension =
      coarsestRitzBlock requestedCount levelValue
  | otherwise = do
      aggregation <- heavyEdgeAggregation levelValue
      coarseLevel <- graphLaplacianCoarseLevel aggregation
      coarseBlock <-
        solveCascadicLevel
          requestedCount
          refinementLimit
          residualTarget
          False
          coarseLevel
      prolongedColumns <-
        prolongRitzColumns
          levelValue
          aggregation
          (ritzBlockColumns coarseBlock)
      initialBlock <-
        rayleighRitzBlock
          requestedCount
          (graphLevelMatrix levelValue)
          prolongedColumns
      refineRitzBlock
        requestedCount
        refinementLimit
        residualTarget
        isFinest
        levelValue
        initialBlock

cascadicCoarsestDimension :: Int
cascadicCoarsestDimension = 256

cascadicResidualTarget :: LanczosConfig -> Int -> Double
cascadicResidualTarget config dimension =
  max
    (sqrt (lanczosTolerance config))
    (128.0 * epsDouble * sqrt (fromIntegral (max 1 dimension) :: Double))

cascadicRefinementLimit ::
  LanczosConfig ->
  Either CascadicGraphObstruction Int
cascadicRefinementLimit config =
  first
    ( CascadicGraphBackendFailure
        . const (InvariantViolation "cascadic graph refinement budget exceeds Int range")
    )
    (checkedNonNegativeProduct 5 (lanczosIterations config))

heavyEdgeAggregation ::
  GraphLaplacianLevel ->
  Either CascadicGraphObstruction GraphAggregation
heavyEdgeAggregation levelValue = do
  fineToCoarse <-
    U.generateM
      fineDimension
      ( \vertexIndex ->
          case IntMap.lookup vertexIndex (aggregationAssignments finalFold) of
            Nothing -> Left (CascadicGraphIncompleteAssignment vertexIndex)
            Just coarseIndex -> Right coarseIndex
      )
  let coarseDimension = aggregationNextIndex finalFold
  if coarseDimension >= fineDimension
    then Left (CascadicGraphCoarseningStalled fineDimension)
    else
      let coarseMasses =
            U.accumulate
              (+)
              (U.replicate coarseDimension 0.0)
              (U.zip fineToCoarse (graphLevelMasses levelValue))
          coarseEdgeMap =
            Box.foldl'
              (collapseFineEdge fineToCoarse)
              Map.empty
              (graphLevelEdges levelValue)
          coarseEdges =
            Box.fromList
              [ GraphEdge leftIndex rightIndex edgeWeight
                | ((leftIndex, rightIndex), edgeWeight) <- Map.toAscList coarseEdgeMap
              ]
       in Right
            GraphAggregation
              { graphAggregationFineToCoarse = fineToCoarse,
                graphAggregationCoarseMasses = coarseMasses,
                graphAggregationCoarseEdges = coarseEdges
              }
  where
    fineDimension = graphLevelDimension levelValue
    adjacency = graphAdjacency (graphLevelEdges levelValue)
    finalFold =
      foldl'
        (assignHeavyEdgeAggregate adjacency)
        (AggregationFold IntMap.empty 0)
        [0 .. fineDimension - 1]

graphAdjacency ::
  Box.Vector (GraphEdge Int) ->
  IntMap [WeightedNeighbor]
graphAdjacency =
  Box.foldr
    ( \edgeValue ->
        IntMap.insertWith
          (<>)
          (graphEdgeLeft edgeValue)
          [ WeightedNeighbor
              (graphEdgeRight edgeValue)
              (graphEdgeWeight edgeValue)
          ]
          . IntMap.insertWith
            (<>)
            (graphEdgeRight edgeValue)
            [ WeightedNeighbor
                (graphEdgeLeft edgeValue)
                (graphEdgeWeight edgeValue)
            ]
    )
    IntMap.empty

assignHeavyEdgeAggregate ::
  IntMap [WeightedNeighbor] ->
  AggregationFold ->
  Int ->
  AggregationFold
assignHeavyEdgeAggregate adjacency foldValue vertexIndex =
  case IntMap.lookup vertexIndex (aggregationAssignments foldValue) of
    Just _ -> foldValue
    Nothing ->
      let selectedNeighbor =
            foldl'
              (selectHeavierUnassignedNeighbor (aggregationAssignments foldValue))
              Nothing
              (IntMap.findWithDefault [] vertexIndex adjacency)
          aggregateIndex = aggregationNextIndex foldValue
          withVertex =
            IntMap.insert
              vertexIndex
              aggregateIndex
              (aggregationAssignments foldValue)
          withNeighbor =
            case selectedNeighbor of
              Nothing -> withVertex
              Just neighborValue ->
                IntMap.insert
                  (weightedNeighborVertex neighborValue)
                  aggregateIndex
                  withVertex
       in AggregationFold
            { aggregationAssignments = withNeighbor,
              aggregationNextIndex = aggregateIndex + 1
            }

selectHeavierUnassignedNeighbor ::
  IntMap Int ->
  Maybe WeightedNeighbor ->
  WeightedNeighbor ->
  Maybe WeightedNeighbor
selectHeavierUnassignedNeighbor assignments selected candidate
  | IntMap.member (weightedNeighborVertex candidate) assignments = selected
  | otherwise =
      case selected of
        Nothing -> Just candidate
        Just selectedValue
          | weightedNeighborEdgeWeight candidate > weightedNeighborEdgeWeight selectedValue ->
              Just candidate
          | weightedNeighborEdgeWeight candidate == weightedNeighborEdgeWeight selectedValue
              && weightedNeighborVertex candidate < weightedNeighborVertex selectedValue ->
              Just candidate
          | otherwise -> selected

collapseFineEdge ::
  U.Vector Int ->
  Map (Int, Int) Double ->
  GraphEdge Int ->
  Map (Int, Int) Double
collapseFineEdge fineToCoarse edgeMap edgeValue =
  let leftAggregate = fineToCoarse `U.unsafeIndex` graphEdgeLeft edgeValue
      rightAggregate = fineToCoarse `U.unsafeIndex` graphEdgeRight edgeValue
   in if leftAggregate == rightAggregate
        then edgeMap
        else
          Map.insertWith
            (+)
            (min leftAggregate rightAggregate, max leftAggregate rightAggregate)
            (graphEdgeWeight edgeValue)
            edgeMap

graphLaplacianCoarseLevel ::
  GraphAggregation ->
  Either CascadicGraphObstruction GraphLaplacianLevel
graphLaplacianCoarseLevel aggregation = do
  let coarseMasses = graphAggregationCoarseMasses aggregation
      coarseDimension = U.length coarseMasses
      coarseEdges = graphAggregationCoarseEdges aggregation
  laplacianMatrix <-
    first CascadicGraphBackendFailure
      ( graphLaplacianCSR
          [0 .. coarseDimension - 1]
          (Box.toList coarseEdges)
      )
  coordinateValue <- first CascadicGraphBackendFailure (csrToCOO laplacianMatrix)
  normalizedMatrix <-
    first CascadicGraphBackendFailure
      ( canonicalCSRFromEntries
          coarseDimension
          coarseDimension
          [ ( rowIndex,
              columnIndex,
              entryValue
                / sqrt
                  ( (coarseMasses `U.unsafeIndex` rowIndex)
                      * (coarseMasses `U.unsafeIndex` columnIndex)
                  )
            )
            | (rowIndex, columnIndex, entryValue) <- cooEntries coordinateValue
          ]
      )
  pure
    GraphLaplacianLevel
      { graphLevelMasses = coarseMasses,
        graphLevelEdges = coarseEdges,
        graphLevelMatrix = normalizedMatrix
      }

prolongRitzColumns ::
  GraphLaplacianLevel ->
  GraphAggregation ->
  Box.Vector (U.Vector Double) ->
  Either CascadicGraphObstruction (Box.Vector (U.Vector Double))
prolongRitzColumns fineLevel aggregation coarseColumns =
  Box.mapM prolongColumn coarseColumns
  where
    fineMasses = graphLevelMasses fineLevel
    coarseMasses = graphAggregationCoarseMasses aggregation
    fineToCoarse = graphAggregationFineToCoarse aggregation
    fineDimension = graphLevelDimension fineLevel
    prolongColumn coarseColumn
      | U.length coarseColumn /= U.length coarseMasses =
          Left
            ( CascadicGraphBackendFailure
                (InvariantViolation "cascadic graph coarse eigenvector dimension mismatch")
            )
      | otherwise =
          Right
            ( U.generate
                fineDimension
                ( \fineIndex ->
                    let coarseIndex = fineToCoarse `U.unsafeIndex` fineIndex
                        fineMass = fineMasses `U.unsafeIndex` fineIndex
                        coarseMass = coarseMasses `U.unsafeIndex` coarseIndex
                     in sqrt (fineMass / coarseMass)
                          * (coarseColumn `U.unsafeIndex` coarseIndex)
                )
            )

coarsestRitzBlock ::
  Int ->
  GraphLaplacianLevel ->
  Either CascadicGraphObstruction RitzBlock
coarsestRitzBlock requestedCount levelValue = do
  eigenResult <- denseSymmetricEigenResult (graphLevelMatrix levelValue)
  let dimension = graphLevelDimension levelValue
      vectorPayload =
        denseDoubleMatrixToRowMajorVector
          (symmetricEigenResultVectors eigenResult)
      initialColumns =
        Box.generate
          requestedCount
          ( \columnIndex ->
              U.generate
                dimension
                ( \rowIndex ->
                    vectorPayload S.! (rowIndex * dimension + columnIndex)
                )
          )
  rayleighRitzBlock
    requestedCount
    (graphLevelMatrix levelValue)
    initialColumns

denseSymmetricEigenResult ::
  SparseCSR Double ->
  Either CascadicGraphObstruction SymmetricEigenResult
denseSymmetricEigenResult matrixValue = do
  let dimension = csrRows matrixValue
  entryCount <-
    first
      ( CascadicGraphBackendFailure
          . const (InvariantViolation "cascadic coarse dense cardinality exceeds Int range")
      )
      (checkedNonNegativeProduct dimension dimension)
  imageColumns <-
    traverse
      ( first CascadicGraphBackendFailure
          . csrMatVecVector matrixValue
          . unitVector dimension
      )
      [0 .. dimension - 1]
  let columnPayload = U.concat imageColumns
      rowMajorPayload =
        S.generate
          entryCount
          ( \flatIndex ->
              let (rowIndex, columnIndex) = flatIndex `quotRem` dimension
               in columnPayload `U.unsafeIndex` (columnIndex * dimension + rowIndex)
          )
  denseMatrix <-
    first CascadicGraphBackendFailure
      (mkDenseDoubleMatrixRowMajor dimension dimension rowMajorPayload)
  first CascadicGraphBackendFailure
    (symmetricEigenPairsDenseUnchecked dimension denseMatrix)

refineRitzBlock ::
  Int ->
  Int ->
  Double ->
  Bool ->
  GraphLaplacianLevel ->
  RitzBlock ->
  Either CascadicGraphObstruction RitzBlock
refineRitzBlock requestedCount refinementLimit residualTarget requireTarget levelValue initialBlock = do
  diagonalValues <-
    first
      (CascadicGraphBackendFailure . InvariantViolation . show)
      (sparseDiagonal (graphLevelMatrix levelValue))
  refineState
    diagonalValues
    RefinementState
      { refinementStepCount = 0,
        refinementRitzBlock = initialBlock
      }
  where
    refineState diagonalValues stateValue
      | ritzBlockMaximumResidual (refinementRitzBlock stateValue) <= residualTarget =
          Right (refinementRitzBlock stateValue)
      | refinementStepCount stateValue >= refinementLimit =
          if requireTarget
            then
              Left
                ( CascadicGraphRefinementBudgetExceeded
                    (graphLevelDimension levelValue)
                    residualTarget
                    (ritzBlockMaximumResidual (refinementRitzBlock stateValue))
                )
            else Right (refinementRitzBlock stateValue)
      | otherwise = do
          candidateColumns <-
            smoothedRitzColumns
              diagonalValues
              (refinementRitzBlock stateValue)
          nextBlock <-
            rayleighRitzBlock
              requestedCount
              (graphLevelMatrix levelValue)
              candidateColumns
          refineState
            diagonalValues
            RefinementState
              { refinementStepCount = refinementStepCount stateValue + 1,
                refinementRitzBlock = nextBlock
              }

smoothedRitzColumns ::
  U.Vector Double ->
  RitzBlock ->
  Either CascadicGraphObstruction (Box.Vector (U.Vector Double))
smoothedRitzColumns diagonalValues blockValue = do
  candidateColumns <-
    Box.zipWithM
      (smoothRitzColumn diagonalValues)
      (ritzBlockColumns blockValue)
      (ritzBlockResidualVectors blockValue)
  orthonormalColumns <-
    first CascadicGraphBackendFailure
      ( orthonormalizeBlock
          True
          cascadicRankTolerance
          Box.empty
          candidateColumns
      )
  requireBlockRank (Box.length candidateColumns) orthonormalColumns

smoothRitzColumn ::
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either CascadicGraphObstruction (U.Vector Double)
smoothRitzColumn diagonalValues columnValue residualVector
  | U.length columnValue /= U.length diagonalValues
      || U.length residualVector /= U.length diagonalValues =
      Left
        ( CascadicGraphBackendFailure
            (InvariantViolation "cascadic graph smoother dimension mismatch")
        )
  | otherwise =
      Right
        ( U.generate
            (U.length columnValue)
            ( \entryIndex ->
                let diagonalValue = diagonalValues `U.unsafeIndex` entryIndex
                    columnEntry = columnValue `U.unsafeIndex` entryIndex
                    residualEntry = residualVector `U.unsafeIndex` entryIndex
                 in if abs diagonalValue <= cascadicRankTolerance
                      then columnEntry
                      else
                        columnEntry
                          - cascadicSmoothingWeight
                            * residualEntry
                            / diagonalValue
            )
        )

cascadicSmoothingWeight :: Double
cascadicSmoothingWeight = 0.72

cascadicRankTolerance :: Double
cascadicRankTolerance = 1.0e-13

rayleighRitzBlock ::
  Int ->
  SparseCSR Double ->
  Box.Vector (U.Vector Double) ->
  Either CascadicGraphObstruction RitzBlock
rayleighRitzBlock requestedCount matrixValue candidateColumns = do
  orthonormalColumns <-
    first CascadicGraphBackendFailure
      ( orthonormalizeBlock
          True
          cascadicRankTolerance
          Box.empty
          candidateColumns
      )
  basisColumns <- requireBlockRank requestedCount orthonormalColumns
  basisImages <-
    Box.mapM
      (first CascadicGraphBackendFailure . csrMatVecVector matrixValue)
      basisColumns
  projectedResult <- projectedSymmetricEigenResult basisColumns basisImages
  let projectedDimension = Box.length basisColumns
      projectedVectorPayload =
        denseDoubleMatrixToRowMajorVector
          (symmetricEigenResultVectors projectedResult)
      selectedValues =
        U.generate
          requestedCount
          (S.unsafeIndex (symmetricEigenResultValues projectedResult))
      projectedColumn columnIndex =
        U.generate
          projectedDimension
          ( \rowIndex ->
              projectedVectorPayload
                S.! (rowIndex * projectedDimension + columnIndex)
          )
  selectedColumns <-
    Box.generateM
      requestedCount
      (linearCombinationColumnsU basisColumns . projectedColumn)
      & first CascadicGraphBackendFailure
  selectedImages <-
    Box.generateM
      requestedCount
      (linearCombinationColumnsU basisImages . projectedColumn)
      & first CascadicGraphBackendFailure
  residualVectors <-
    Box.generateM
      requestedCount
      ( \columnIndex ->
          case
            ( selectedImages Box.!? columnIndex,
              selectedColumns Box.!? columnIndex,
              selectedValues U.!? columnIndex
            )
            of
            (Just imageVector, Just columnValue, Just eigenvalue) ->
              first CascadicGraphBackendFailure
                (subScaledU imageVector eigenvalue columnValue)
            _ ->
              Left
                ( CascadicGraphBackendFailure
                    (InvariantViolation "cascadic graph Ritz column extraction failed")
                )
      )
  let residualNorms = U.generate requestedCount (normU . Box.unsafeIndex residualVectors)
  if U.all fieldValueValid selectedValues && U.all fieldValueValid residualNorms
    then
      Right
        RitzBlock
          { ritzBlockValues = selectedValues,
            ritzBlockColumns = selectedColumns,
            ritzBlockResidualVectors = residualVectors,
            ritzBlockResidualNorms = residualNorms
          }
    else
      Left
        ( CascadicGraphBackendFailure
            (InvariantViolation "cascadic graph Ritz solve produced non-finite evidence")
        )

projectedSymmetricEigenResult ::
  Box.Vector (U.Vector Double) ->
  Box.Vector (U.Vector Double) ->
  Either CascadicGraphObstruction SymmetricEigenResult
projectedSymmetricEigenResult basisColumns basisImages = do
  let projectedDimension = Box.length basisColumns
  projectedEntryCount <-
    first
      ( CascadicGraphBackendFailure
          . const (InvariantViolation "cascadic projected cardinality exceeds Int range")
      )
      (checkedNonNegativeProduct projectedDimension projectedDimension)
  projectedPayload <-
    S.generateM
      projectedEntryCount
      ( \flatIndex ->
          let (rowIndex, columnIndex) = flatIndex `quotRem` projectedDimension
           in case
                ( basisColumns Box.!? rowIndex,
                  basisImages Box.!? columnIndex
                )
                of
                (Just rowVector, Just imageVector) ->
                  first CascadicGraphBackendFailure
                    (vectorInnerProduct rowVector imageVector)
                _ ->
                  Left
                    ( CascadicGraphBackendFailure
                        (InvariantViolation "cascadic projected basis lookup failed")
                    )
      )
  projectedMatrix <-
    first CascadicGraphBackendFailure
      ( mkDenseDoubleMatrixRowMajor
          projectedDimension
          projectedDimension
          projectedPayload
      )
  first CascadicGraphBackendFailure
    ( symmetricEigenPairsDenseUnchecked
        projectedDimension
        projectedMatrix
    )

vectorInnerProduct ::
  U.Vector Double ->
  U.Vector Double ->
  Either MoonlightError Double
vectorInnerProduct leftVector rightVector
  | U.length leftVector /= U.length rightVector =
      Left (InvariantViolation "cascadic graph inner-product dimension mismatch")
  | otherwise = Right (U.sum (U.zipWith (*) leftVector rightVector))

requireBlockRank ::
  Int ->
  Box.Vector (U.Vector Double) ->
  Either CascadicGraphObstruction (Box.Vector (U.Vector Double))
requireBlockRank requiredCount columns
  | Box.length columns < requiredCount =
      Left (CascadicGraphRankLoss requiredCount (Box.length columns))
  | otherwise = Right (Box.take requiredCount columns)

ritzBlockMaximumResidual :: RitzBlock -> Double
ritzBlockMaximumResidual =
  U.foldl' max 0.0 . ritzBlockResidualNorms

ritzBlockToEigenpairs ::
  RitzBlock ->
  Either CascadicGraphObstruction Eigenpairs
ritzBlockToEigenpairs blockValue =
  case ritzBlockColumns blockValue Box.!? 0 of
    Nothing -> Left (CascadicGraphRankLoss 1 0)
    Just firstColumn ->
      first CascadicGraphBackendFailure
        ( eigenpairsFromColumns
            (U.length firstColumn)
            [ ( eigenvalue,
                columnValue,
                residualNorm
              )
              | (eigenvalue, columnValue, residualNorm) <-
                  zip3
                    (U.toList (ritzBlockValues blockValue))
                    (Box.toList (ritzBlockColumns blockValue))
                    (U.toList (ritzBlockResidualNorms blockValue))
            ]
        )

graphLevelDimension :: GraphLaplacianLevel -> Int
graphLevelDimension = csrRows . graphLevelMatrix

unitVector :: Int -> Int -> U.Vector Double
unitVector dimension selectedIndex =
  U.generate
    dimension
    (\entryIndex -> if entryIndex == selectedIndex then 1.0 else 0.0)
