module Moonlight.Homology.Pure.Topology.Integral
  ( SmithDecomposition (..),
    IntegralHomologyDegreeWitness (..),
    integralHomologyGroupsOf,
    exactRepresentativeClassesOf,
    integralHomologyWitnessesOf,
    integralHomologyWitnessAt,
    smithDecompositionOf,
    representativeClassesFromSmith,
    vectorToIntegralRepresentative,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import qualified Data.List as List
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    incidenceMatrixAt,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryIncidence,
    boundaryCoefficient,
    boundaryEntries,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Homology.Pure.Chain
  ( ExactRepresentativeClass (..),
    HomologicalDegree (..),
    RepresentativeChain (..),
    RepresentativeCycle,
  )
import Moonlight.Homology.Pure.Failure (HomologyFailure (..), HomologyLaw (..))
import Moonlight.Homology.Pure.Group (HomologyGroup (..))
import Moonlight.Homology.Pure.Matrix.Validated
  ( validatedColumnAt,
    validatedDiagonal,
    validatedMatrixFromRows,
  )
import Moonlight.Homology.Pure.Topology.Core
import Moonlight.Homology.Pure.Topology.Graph
  ( GraphOneComplex (..),
    graphOneComplexFromComplex,
  )
import Moonlight.LinAlg.Pure.Dense.Dynamic (mkDynMatrix, withDynMatrix)
import Moonlight.LinAlg.Pure.Dense.Types (toListMatrix)
import Moonlight.LinAlg.Pure.Domain.Smith (SmithNormalForm (..), smithNormalForm)

type SmithDecomposition :: Type
data SmithDecomposition = SmithDecomposition
  { smithLeftRows :: [[Integer]],
    smithDiagonalRows :: [[Integer]],
    smithRightRows :: [[Integer]],
    smithLeftInverseRows :: [[Integer]],
    smithRightInverseRows :: [[Integer]],
    smithDiagonalValues :: [Integer],
    smithRankValue :: Int
  }
  deriving stock (Eq, Show)

type IntegralBoundarySummary :: Type
data IntegralBoundarySummary = IntegralBoundarySummary
  { integralBoundaryDegree :: HomologicalDegree,
    integralBoundarySourceRank :: Int,
    integralBoundaryTargetRank :: Int,
    integralBoundaryRows :: [[Integer]],
    integralBoundarySmith :: SmithDecomposition
  }
  deriving stock (Eq, Show)

type KernelCoordinateHomology :: Type
data KernelCoordinateHomology = KernelCoordinateHomology
  { kernelCoordinateGroup :: HomologyGroup Integer,
    kernelCoordinateImageSmith :: SmithDecomposition
  }

type IntegralHomologyDegreeWitness :: Type -> Type
data IntegralHomologyDegreeWitness basis = IntegralHomologyDegreeWitness
  { integralWitnessDegree :: HomologicalDegree,
    integralWitnessGroup :: HomologyGroup Integer,
    integralWitnessClasses :: [ExactRepresentativeClass basis]
  }

integralHomologyGroupsOf :: Integral r => FiniteChainComplex r -> Either HomologyFailure [HomologyGroup Integer]
integralHomologyGroupsOf finite =
  case graphIntegralHomologyGroupsOf finite of
    Just graphGroups -> graphGroups
    Nothing -> do
      preparedBoundaries <- prepareIntegralBoundarySummaries finite
      traverse (integralHomologyGroupAtPrepared preparedBoundaries) (dimensionsOf finite)

graphIntegralHomologyGroupsOf ::
  Integral r =>
  FiniteChainComplex r ->
  Maybe (Either HomologyFailure [HomologyGroup Integer])
graphIntegralHomologyGroupsOf finite =
  graphIntegralHomologyGroups (HomologicalDegree 1 `elem` dimensionsOf finite)
    <$> graphOneComplexFromComplex finite

graphIntegralHomologyGroups :: Bool -> GraphOneComplex -> Either HomologyFailure [HomologyGroup Integer]
graphIntegralHomologyGroups includesDegreeOne graph =
  let vertexCount = graphOneVertexCount graph
      edgeCount = graphOneEdgeCount graph
      componentCount = length (graphOneComponents graph)
      boundaryRank = vertexCount - componentCount
      h0 =
        HomologyGroup
          { freeRank = componentCount,
            torsionInvariants = []
          }
      h1 =
        HomologyGroup
          { freeRank = edgeCount - boundaryRank,
            torsionInvariants = []
          }
   in pure
        ( if includesDegreeOne
            then [h0, h1]
            else [h0]
        )

prepareIntegralBoundarySummaries ::
  Integral r =>
  FiniteChainComplex r ->
  Either HomologyFailure (IntMap.IntMap IntegralBoundarySummary)
prepareIntegralBoundarySummaries finite =
  traverse
    ( \degreeValue@(HomologicalDegree degreeIndex) -> do
        boundarySummary <- integralBoundarySummaryAt finite degreeValue
        pure (degreeIndex, boundarySummary)
    )
    (dimensionsOf finite <> [nextDegreeAfterFinite finite])
    & fmap IntMap.fromList

nextDegreeAfterFinite :: FiniteChainComplex r -> HomologicalDegree
nextDegreeAfterFinite finite =
  case dimensionsOf finite of
    [] -> HomologicalDegree 0
    degreeValues ->
      HomologicalDegree
        ( 1
            + maximum
              (fmap (\(HomologicalDegree degreeIndex) -> degreeIndex) degreeValues)
        )

integralBoundarySummaryAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  Either HomologyFailure IntegralBoundarySummary
integralBoundarySummaryAt finite degreeValue =
  let boundaryIncidenceValue = incidenceMatrixAt finite degreeValue
      rowCount = targetCardinality boundaryIncidenceValue
      columnCount = sourceCardinality boundaryIncidenceValue
      boundaryRows = integralBoundaryMatrixRows boundaryIncidenceValue
   in do
        smithValue <- smithDecompositionOf rowCount columnCount boundaryRows
        pure
          IntegralBoundarySummary
            { integralBoundaryDegree = degreeValue,
              integralBoundarySourceRank = columnCount,
              integralBoundaryTargetRank = rowCount,
              integralBoundaryRows = boundaryRows,
              integralBoundarySmith = smithValue
            }

integralHomologyGroupAtPrepared ::
  IntMap.IntMap IntegralBoundarySummary ->
  HomologicalDegree ->
  Either HomologyFailure (HomologyGroup Integer)
integralHomologyGroupAtPrepared preparedBoundaries degreeValue@(HomologicalDegree degreeIndex) = do
  boundarySummary <- requirePreparedBoundary preparedBoundaries degreeValue
  incomingSummary <- requirePreparedBoundary preparedBoundaries (HomologicalDegree (degreeIndex + 1))
  let chainRank = integralBoundarySourceRank boundarySummary
      boundaryRank = smithRankValue (integralBoundarySmith boundarySummary)
      incomingRank = smithRankValue (integralBoundarySmith incomingSummary)
      kernelRank = chainRank - boundaryRank
  if boundaryRank == 0
    then cokernelGroup chainRank incomingSummary
    else
      if incomingRank == 0
        then pure HomologyGroup {freeRank = kernelRank, torsionInvariants = []}
        else
          kernelCoordinateGroup
            <$> kernelCoordinateHomologyFromSummaries boundarySummary incomingSummary

requirePreparedBoundary ::
  IntMap.IntMap IntegralBoundarySummary ->
  HomologicalDegree ->
  Either HomologyFailure IntegralBoundarySummary
requirePreparedBoundary preparedBoundaries degreeValue@(HomologicalDegree degreeIndex) =
  case IntMap.lookup degreeIndex preparedBoundaries of
    Just boundarySummary -> Right boundarySummary
    Nothing ->
      Left
        ( InvalidTopologyInput
            ( "integral homology missing prepared boundary summary at degree "
                <> show degreeValue
            )
        )

cokernelGroup ::
  Int ->
  IntegralBoundarySummary ->
  Either HomologyFailure (HomologyGroup Integer)
cokernelGroup ambientRank incomingSummary =
  if incomingBoundaryCompatibleWithAmbient ambientRank incomingSummary
    then
      let incomingSmith = integralBoundarySmith incomingSummary
       in pure
            HomologyGroup
              { freeRank = ambientRank - smithRankValue incomingSmith,
                torsionInvariants =
                  smithDiagonalValues incomingSmith
                    & take (smithRankValue incomingSmith)
                    & torsionInvariantsFromDiagonal
              }
    else incompatibleIncomingBoundary ambientRank incomingSummary

kernelCoordinateHomologyFromSummaries ::
  IntegralBoundarySummary ->
  IntegralBoundarySummary ->
  Either HomologyFailure KernelCoordinateHomology
kernelCoordinateHomologyFromSummaries boundarySummary incomingSummary =
  let chainRank = integralBoundarySourceRank boundarySummary
   in if incomingBoundaryCompatibleWithAmbient chainRank incomingSummary
        then do
          let boundarySmith = integralBoundarySmith boundarySummary
              boundaryRank = smithRankValue boundarySmith
              kernelRank = chainRank - boundaryRank
          imageCoordinatesFull <-
            integerMatrixProduct
              chainRank
              (smithRightInverseRows boundarySmith)
              chainRank
              (ambientIncomingRows chainRank incomingSummary)
          let leadingImageCoordinates = takeRows boundaryRank imageCoordinatesFull
              kernelImageCoordinates = dropRows boundaryRank imageCoordinatesFull
          if allZeroMatrix leadingImageCoordinates
            then do
              imageSmith <-
                smithDecompositionOf
                  kernelRank
                  (integralBoundarySourceRank incomingSummary)
                  kernelImageCoordinates
              pure
                KernelCoordinateHomology
                  { kernelCoordinateGroup =
                      HomologyGroup
                        { freeRank = kernelRank - smithRankValue imageSmith,
                          torsionInvariants =
                            smithDiagonalValues imageSmith
                              & take (smithRankValue imageSmith)
                              & torsionInvariantsFromDiagonal
                        },
                    kernelCoordinateImageSmith = imageSmith
                  }
            else Left (LawViolation ChainNilpotenceLaw)
        else incompatibleIncomingBoundary chainRank incomingSummary

incomingBoundaryCompatibleWithAmbient :: Int -> IntegralBoundarySummary -> Bool
incomingBoundaryCompatibleWithAmbient ambientRank incomingSummary =
  integralBoundarySourceRank incomingSummary == 0
    || integralBoundaryTargetRank incomingSummary == ambientRank

ambientIncomingRows :: Int -> IntegralBoundarySummary -> [[Integer]]
ambientIncomingRows ambientRank incomingSummary =
  if integralBoundaryTargetRank incomingSummary == ambientRank
    then integralBoundaryRows incomingSummary
    else replicate ambientRank []

incompatibleIncomingBoundary ::
  Int ->
  IntegralBoundarySummary ->
  Either HomologyFailure a
incompatibleIncomingBoundary ambientRank incomingSummary =
  Left
    ( InvalidTopologyInput
        ( "incoming boundary at degree "
            <> show (integralBoundaryDegree incomingSummary)
            <> " targets "
            <> show (integralBoundaryTargetRank incomingSummary)
            <> " cells, expected "
            <> show ambientRank
        )
    )

torsionInvariantsFromDiagonal :: [Integer] -> [Integer]
torsionInvariantsFromDiagonal =
  filter (> 1) . fmap abs

exactRepresentativeClassesOf ::
  Integral r =>
  FiniteChainComplex r ->
  Either HomologyFailure [ExactRepresentativeClass Int]
exactRepresentativeClassesOf finite =
  integralHomologyWitnessesOf finite
    & fmap (concatMap integralWitnessClasses)

integralHomologyWitnessesOf ::
  Integral r =>
  FiniteChainComplex r ->
  Either HomologyFailure [IntegralHomologyDegreeWitness Int]
integralHomologyWitnessesOf finite = do
  preparedBoundaries <- prepareIntegralBoundarySummaries finite
  traverse (integralHomologyWitnessAtPrepared preparedBoundaries) (dimensionsOf finite)

integralHomologyWitnessAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  Either HomologyFailure (IntegralHomologyDegreeWitness Int)
integralHomologyWitnessAt finite degreeValue = do
  preparedBoundaries <- prepareIntegralBoundarySummaries finite
  integralHomologyWitnessAtPrepared preparedBoundaries degreeValue

integralHomologyWitnessAtPrepared ::
  IntMap.IntMap IntegralBoundarySummary ->
  HomologicalDegree ->
  Either HomologyFailure (IntegralHomologyDegreeWitness Int)
integralHomologyWitnessAtPrepared preparedBoundaries degreeValue@(HomologicalDegree degreeIndex) = do
  boundarySummary <- requirePreparedBoundary preparedBoundaries degreeValue
  incomingSummary <- requirePreparedBoundary preparedBoundaries (HomologicalDegree (degreeIndex + 1))
  integralHomologyWitnessFromSummaries degreeValue boundarySummary incomingSummary

integralHomologyWitnessFromSummaries ::
  HomologicalDegree ->
  IntegralBoundarySummary ->
  IntegralBoundarySummary ->
  Either HomologyFailure (IntegralHomologyDegreeWitness Int)
integralHomologyWitnessFromSummaries degreeValue boundarySummary incomingSummary = do
  kernelCoordinateHomology <-
    kernelCoordinateHomologyFromSummaries boundarySummary incomingSummary
  let chainRank = integralBoundarySourceRank boundarySummary
      boundarySmith = integralBoundarySmith boundarySummary
      boundaryRank = smithRankValue boundarySmith
      kernelRank = chainRank - boundaryRank
      kernelBasis = dropColumns boundaryRank (smithRightRows boundarySmith)
      imageSmith = kernelCoordinateImageSmith kernelCoordinateHomology
      homologyGroupValue = kernelCoordinateGroup kernelCoordinateHomology
  classBasis <-
    integerMatrixProduct
      kernelRank
      kernelBasis
      kernelRank
      (smithLeftInverseRows imageSmith)
  classes <-
    representativeClassesFromSmith
      degreeValue
      homologyGroupValue
      (smithDiagonalValues imageSmith)
      (smithRankValue imageSmith)
      classBasis
  pure
    IntegralHomologyDegreeWitness
      { integralWitnessDegree = degreeValue,
        integralWitnessGroup = homologyGroupValue,
        integralWitnessClasses = classes
      }

smithDecompositionOf ::
  Int ->
  Int ->
  [[Integer]] ->
  Either HomologyFailure SmithDecomposition
smithDecompositionOf rowCount columnCount matrixRows =
  if rowCount < 0 || columnCount < 0
    then Left (InvalidTopologyInput "matrix dimensions must be non-negative")
    else
      if not (rowsRespectWidth columnCount matrixRows) || length matrixRows /= rowCount
        then Left (InvalidTopologyInput "matrix rows do not match their declared shape")
        else
          if rowCount == 0 || columnCount == 0
            then
              Right
                SmithDecomposition
                  { smithLeftRows = identityMatrix rowCount,
                    smithDiagonalRows = zeroMatrix rowCount columnCount,
                    smithRightRows = identityMatrix columnCount,
                    smithLeftInverseRows = identityMatrix rowCount,
                    smithRightInverseRows = identityMatrix columnCount,
                    smithDiagonalValues = [],
                    smithRankValue = 0
                  }
            else do
              dynamicMatrix <-
                first (BackendFailure . show) (mkDynMatrix rowCount columnCount (concat matrixRows))
              (leftEntries, diagonalEntries, rightEntries, leftInverseEntries, rightInverseEntries) <-
                first (BackendFailure . show) $
                  withDynMatrix dynamicMatrix
                    ( \matrixValue ->
                        smithNormalForm matrixValue
                          >>= \smithValue ->
                            Right
                              ( toListMatrix (smithLeft smithValue),
                                toListMatrix (smithDiagonal smithValue),
                                toListMatrix (smithRight smithValue),
                                toListMatrix (smithLeftInverse smithValue),
                                toListMatrix (smithRightInverse smithValue)
                              )
                    )
                    >>= id
              let leftRows = chunkColumns rowCount leftEntries
                  diagonalRows = chunkColumns columnCount diagonalEntries
                  rightRows = chunkColumns columnCount rightEntries
                  leftInverseRows = chunkColumns rowCount leftInverseEntries
                  rightInverseRows = chunkColumns columnCount rightInverseEntries
              validatedDiagonalMatrix <- validatedMatrixFromRows diagonalRows
              let diagonalValues = validatedDiagonal validatedDiagonalMatrix
                  rankValue = length (filter (/= 0) diagonalValues)
              pure
                SmithDecomposition
                  { smithLeftRows = leftRows,
                    smithDiagonalRows = diagonalRows,
                    smithRightRows = rightRows,
                    smithLeftInverseRows = leftInverseRows,
                    smithRightInverseRows = rightInverseRows,
                    smithDiagonalValues = diagonalValues,
                    smithRankValue = rankValue
                  }

integralBoundaryMatrixRows :: Integral r => BoundaryIncidence r -> [[Integer]]
integralBoundaryMatrixRows incidence =
  let rowCount = targetCardinality incidence
      columnCount = sourceCardinality incidence
      rowBuckets =
        boundaryEntries incidence
          & List.foldl'
            ( \buckets entry ->
                IntMap.insertWith
                  (IntMap.unionWith (+))
                  (targetIndex entry)
                  (IntMap.singleton (sourceIndex entry) (fromIntegral (boundaryCoefficient entry)))
                  buckets
            )
            IntMap.empty
   in enumerateFromZero rowCount
        & fmap
          ( \rowIndexValue ->
              let rowValue = IntMap.findWithDefault IntMap.empty rowIndexValue rowBuckets
               in
              enumerateFromZero columnCount
                & fmap
                  ( \columnIndexValue ->
                      IntMap.findWithDefault 0 columnIndexValue rowValue
                  )
          )

integerMatrixProduct ::
  Int ->
  [[Integer]] ->
  Int ->
  [[Integer]] ->
  Either HomologyFailure [[Integer]]
integerMatrixProduct leftColumnCount leftRows rightRowCount rightRows =
  if leftColumnCount /= rightRowCount
    then Left (InvalidTopologyInput "integer matrix multiplication encountered mismatched inner dimensions")
    else
      if
        not (rowsRespectWidth leftColumnCount leftRows)
          || length rightRows /= rightRowCount
          || not (rowsRespectWidth (matrixColumnCount rightRows) rightRows)
        then Left (InvalidTopologyInput "integer matrix multiplication received malformed rows")
        else
          let rightColumns = integerSparseColumns (matrixColumnCount rightRows) rightRows
              leftSparseRows = fmap integerSparseRow leftRows
           in Right
                ( leftSparseRows
                    & fmap
                      ( \leftRow ->
                          rightColumns
                            & fmap (integerSparseDot leftRow)
                      )
                )

type IntegerSparseRow :: Type
type IntegerSparseRow = IntMap.IntMap Integer

integerSparseColumns :: Int -> [[Integer]] -> [IntegerSparseRow]
integerSparseColumns columnCount rows =
  let columnBuckets =
        rows
          & zip [0 :: Int ..]
          & List.foldl'
            ( \buckets (rowIndex, rowValue) ->
                rowValue
                  & zip [0 :: Int ..]
                  & List.foldl'
                    ( \innerBuckets (columnIndex, coefficient) ->
                        if coefficient == 0
                          then innerBuckets
                          else
                            IntMap.insertWith
                              IntMap.union
                              columnIndex
                              (IntMap.singleton rowIndex coefficient)
                              innerBuckets
                    )
                    buckets
            )
            IntMap.empty
   in enumerateFromZero columnCount
        & fmap (\columnIndex -> IntMap.findWithDefault IntMap.empty columnIndex columnBuckets)

integerSparseRow :: [Integer] -> IntegerSparseRow
integerSparseRow values =
  values
    & zip [0 :: Int ..]
    & List.foldl'
      ( \rowValue (columnIndex, coefficient) ->
          if coefficient == 0
            then rowValue
            else IntMap.insert columnIndex coefficient rowValue
      )
      IntMap.empty

integerSparseDot :: IntegerSparseRow -> IntegerSparseRow -> Integer
integerSparseDot leftRow rightColumn =
  IntMap.foldlWithKey'
    ( \accumulator columnIndex coefficient ->
        accumulator + coefficient * IntMap.findWithDefault 0 columnIndex rightColumn
    )
    0
    leftRow

representativeClassesFromSmith ::
  HomologicalDegree ->
  HomologyGroup Integer ->
  [Integer] ->
  Int ->
  [[Integer]] ->
  Either HomologyFailure [ExactRepresentativeClass Int]
representativeClassesFromSmith degreeValue homologyGroupValue diagonalValues imageRank classBasis =
  case validatedMatrixFromRows classBasis of
    Left failureValue -> Left failureValue
    Right validatedClassBasis -> do
      torsionValues <- torsionClasses validatedClassBasis diagonalValues imageRank
      freeValues <- freeClasses validatedClassBasis (freeRank homologyGroupValue)
      pure (torsionValues <> freeValues)
  where
    torsionClasses validatedClassBasis invariants rankValue =
      concat
        <$> traverse
          ( \(indexValue, invariantValue) ->
              if abs invariantValue > 1
                then do
                  representativeVector <- validatedColumnAt indexValue validatedClassBasis
                  pure
                    [ ExactRepresentativeClass
                        { exactClassDegree = degreeValue,
                          exactClassOrder = Just (abs invariantValue),
                          exactClassRepresentative =
                            vectorToIntegralRepresentative degreeValue representativeVector
                        }
                    ]
                else pure []
          )
          (zip [0 :: Int ..] (take rankValue invariants))
    freeClasses validatedClassBasis freeCount =
      traverse
        ( \offsetValue ->
            let classIndex = imageRank + offsetValue
             in do
                  representativeVector <- validatedColumnAt classIndex validatedClassBasis
                  pure
                    ExactRepresentativeClass
                      { exactClassDegree = degreeValue,
                        exactClassOrder = Nothing,
                        exactClassRepresentative =
                          vectorToIntegralRepresentative degreeValue representativeVector
                      }
        )
        (enumerateFromZero freeCount)

vectorToIntegralRepresentative :: HomologicalDegree -> [Integer] -> RepresentativeCycle Integer Int
vectorToIntegralRepresentative degreeValue vectorValue =
  RepresentativeChain
    { representativeDegree = degreeValue,
      representativeTerms =
        vectorValue
          & zip [0 :: Int ..]
          & filter (\(_, coefficientValue) -> coefficientValue /= 0)
          & fmap (\(basisIndexValue, coefficientValue) -> (coefficientValue, basisIndexValue))
    }
