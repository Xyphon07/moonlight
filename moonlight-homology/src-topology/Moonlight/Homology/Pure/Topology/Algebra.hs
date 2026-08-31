module Moonlight.Homology.Pure.Topology.Algebra
  ( eulerCharacteristicOf,
    freeBettiVector,
    representativeCyclesOverQ,
    representativeCocyclesOverQ,
    exactTopologyWitness,
    homologyBasisAt,
    cohomologyBasisAt,
    QuotientPresentation (..),
    mkQuotientPresentation,
    presentationCoordinates,
    quotientRepresentatives,
    vectorToRepresentative,
    representativeToVector,
  )
where

import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Moonlight.Homology.Boundary.Finite (FiniteChainComplex, incidenceMatrixAt)
import Moonlight.Homology.Pure.Chain
  ( EulerCharacteristic (..),
    HarmonicBasisElement,
    HomologicalDegree (..),
    RepresentativeChain (..),
    RepresentativeCocycle,
    RepresentativeCycle,
    TopologyWitness (..),
    emptyTopologyWitness,
  )
import Moonlight.Homology.Pure.Failure (HomologyFailure)
import Moonlight.Homology.Pure.Group (HomologyGroup (..))
import Moonlight.Homology.Pure.Topology.Core
import Moonlight.Homology.Pure.Topology.Integral (IntegralHomologyDegreeWitness (integralWitnessClasses, integralWitnessGroup), integralHomologyWitnessesOf)
import Moonlight.Homology.Pure.Topology.MacroScaffold (MacroScaffoldIR)
import Moonlight.Homology.Pure.Topology.SparseAlgebra
  ( sparseCohomologyBasisAt,
    sparseHomologyBasisAt,
  )
import Moonlight.Homology.Pure.Matrix.SparseLinAlg
  ( SparseCoordinateBasis,
    SparseMatrix (..),
    SparseRow,
    sparseBoundaryMatrix,
    sparseCoordinateBasis,
    sparseCoordinatesInBasis,
    sparseImageBasisOf,
    sparseImageBasisFromRref,
    sparseIndependentModulo,
    sparseKernelBasisOf,
    sparseKernelBasisFromRref,
    sparseMatrixFromRows,
    sparseRref,
    sparseRowFromDense,
    sparseRowLookup,
    sparseTransposeMatrix,
  )

eulerCharacteristicOf :: FiniteChainComplex r -> EulerCharacteristic
eulerCharacteristicOf finite =
  EulerCharacteristic
    ( alternatingSignedSum
        (fmap (cellCountAtDegree finite) (dimensionsOf finite))
    )

freeBettiVector :: Integral r => FiniteChainComplex r -> [Int]
freeBettiVector finite =
  dimensionsOf finite
    & fmap (length . homologyBasisAt finite)

representativeCyclesOverQ :: Integral r => FiniteChainComplex r -> [RepresentativeCycle Rational Int]
representativeCyclesOverQ finite =
  dimensionsOf finite
    >>= homologyBasisAt finite

representativeCocyclesOverQ :: Integral r => FiniteChainComplex r -> [RepresentativeCocycle Rational Int]
representativeCocyclesOverQ finite =
  dimensionsOf finite
    >>= cohomologyBasisAt finite

exactTopologyWitness ::
  Integral r =>
  FiniteChainComplex r ->
  Either HomologyFailure (TopologyWitness MacroScaffoldIR GraphSpectralMode FiltrationValue Rational Int)
exactTopologyWitness finite = do
  integralWitnesses <- integralHomologyWitnessesOf finite
  rationalBoundaries <- rationalBoundaryDecompositions finite
  let integralGroups = fmap integralWitnessGroup integralWitnesses
      integralClasses = integralWitnesses >>= integralWitnessClasses
  pure
    emptyTopologyWitness
      { topologyEulerCharacteristic = Just (eulerCharacteristicOf finite),
        topologyBettiVector = fmap freeRank integralGroups,
        topologyIntegralHomologyGroups = integralGroups,
        topologyExactRepresentativeClasses = integralClasses,
        topologyCoefficientRepresentativeCycles = representativeCyclesOverQPrepared finite rationalBoundaries,
        topologyCoefficientRepresentativeCocycles = representativeCocyclesOverQPrepared finite rationalBoundaries,
        topologyHarmonicBasis = [] :: [HarmonicBasisElement Rational Int]
      }

type QuotientPresentation :: Type -> Type
data QuotientPresentation r = QuotientPresentation
  { presentationAmbientDimension :: Int,
    presentationBasisVectors :: [[Rational]],
    presentationDenominatorBasis :: [[Rational]],
    presentationCoordinateBasis :: SparseCoordinateBasis,
    presentationRepresentatives :: [RepresentativeCocycle r Int]
  }
  deriving stock (Eq, Show)

type RationalBoundaryDecomposition :: Type
data RationalBoundaryDecomposition = RationalBoundaryDecomposition
  { rationalBoundaryDegree :: HomologicalDegree,
    rationalBoundaryKernelBasis :: [SparseRow],
    rationalBoundaryImageBasis :: [SparseRow],
    rationalCoboundaryKernelBasis :: [SparseRow],
    rationalCoboundaryImageBasis :: [SparseRow]
  }
  deriving stock (Eq, Show)

homologyBasisAt :: Integral r => FiniteChainComplex r -> HomologicalDegree -> [RepresentativeCycle Rational Int]
homologyBasisAt =
  sparseHomologyBasisAt

cohomologyBasisAt :: Integral r => FiniteChainComplex r -> HomologicalDegree -> [RepresentativeCocycle Rational Int]
cohomologyBasisAt =
  sparseCohomologyBasisAt

mkQuotientPresentation ::
  Int ->
  [[Rational]] ->
  [RepresentativeCocycle Rational Int] ->
  [[Rational]] ->
  QuotientPresentation Rational
mkQuotientPresentation ambientDimension basisVectors representatives denominatorBasis =
  QuotientPresentation
    { presentationAmbientDimension = ambientDimension,
      presentationBasisVectors = basisVectors,
      presentationDenominatorBasis = denominatorBasis,
      presentationCoordinateBasis =
        sparseCoordinateBasis
          (fmap sparseRowFromDense (basisVectors <> denominatorBasis)),
      presentationRepresentatives = representatives
    }

presentationCoordinates :: QuotientPresentation Rational -> [Rational] -> Maybe [Rational]
presentationCoordinates presentation vectorValue =
  if length vectorValue /= presentationAmbientDimension presentation
    then Nothing
    else
      sparseCoordinatesInBasis
        (presentationCoordinateBasis presentation)
        (sparseRowFromDense vectorValue)
        & fmap
          ( \coordinates ->
              enumerateFromZero (length (presentationBasisVectors presentation))
                & fmap (\indexValue -> sparseRowLookup indexValue coordinates)
          )

quotientRepresentatives ::
  HomologicalDegree ->
  Int ->
  [[Rational]] ->
  [[Rational]] ->
  [RepresentativeChain Rational Int]
quotientRepresentatives degreeValue ambientDimension currentMatrix incomingMatrix =
  let currentSparse =
        sparseMatrixFromRows ambientDimension currentMatrix
      incomingSparse =
        sparseMatrixFromRows (matrixColumnCount incomingMatrix) incomingMatrix
      kernelBasis = sparseKernelBasisOf ambientDimension currentSparse
      imageBasis = sparseImageBasisOf incomingSparse
      quotientBasis = sparseIndependentModulo imageBasis kernelBasis
   in quotientBasis
        & fmap (sparseVectorToRepresentative degreeValue)

vectorToRepresentative :: HomologicalDegree -> [Rational] -> RepresentativeChain Rational Int
vectorToRepresentative degreeValue vectorValue =
  RepresentativeChain
    { representativeDegree = degreeValue,
      representativeTerms =
        vectorValue
          & zip [0 :: Int ..]
          & filter (\(_, coefficientValue) -> coefficientValue /= 0)
          & fmap (\(basisIndexValue, coefficientValue) -> (coefficientValue, basisIndexValue))
    }

representativeToVector :: Int -> RepresentativeChain Rational Int -> [Rational]
representativeToVector ambientDimension representative =
  let coefficientMap =
        representativeTerms representative
          & foldr
            ( \(coefficientValue, basisIndexValue) mapValue ->
                Map.insertWith (+) basisIndexValue coefficientValue mapValue
            )
            Map.empty
   in enumerateFromZero ambientDimension
        & fmap (\indexValue -> Map.findWithDefault 0 indexValue coefficientMap)

rationalBoundaryDecompositions ::
  Integral r =>
  FiniteChainComplex r ->
  Either HomologyFailure (IntMap.IntMap RationalBoundaryDecomposition)
rationalBoundaryDecompositions finite =
  traverse
    ( \degreeValue@(HomologicalDegree degreeIndex) ->
        rationalBoundaryDecompositionAt finite degreeValue
          & fmap (\boundaryValue -> (degreeIndex, boundaryValue))
    )
    (dimensionsOf finite <> [nextRationalDegreeAfterFinite finite])
    & fmap IntMap.fromList

nextRationalDegreeAfterFinite :: FiniteChainComplex r -> HomologicalDegree
nextRationalDegreeAfterFinite finite =
  case dimensionsOf finite of
    [] -> HomologicalDegree 0
    degreeValues ->
      HomologicalDegree
        ( 1
            + maximum
              (fmap (\(HomologicalDegree degreeIndex) -> degreeIndex) degreeValues)
        )

rationalBoundaryDecompositionAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  Either HomologyFailure RationalBoundaryDecomposition
rationalBoundaryDecompositionAt finite degreeValue@(HomologicalDegree degreeIndex) =
  let sourceDimension = cellCountAtDegree finite degreeValue
      targetDimension = cellCountAtDegree finite (HomologicalDegree (degreeIndex - 1))
      boundaryMatrix =
        (sparseBoundaryMatrix (incidenceMatrixAt finite degreeValue))
          { smColumnCount = sourceDimension
          }
      boundaryRref = sparseRref boundaryMatrix
      coboundaryMatrix =
        (sparseTransposeMatrix boundaryMatrix)
          { smColumnCount = targetDimension
          }
      coboundaryRref = sparseRref coboundaryMatrix
   in Right
        RationalBoundaryDecomposition
          { rationalBoundaryDegree = degreeValue,
            rationalBoundaryKernelBasis =
              sparseKernelBasisFromRref (smColumnCount boundaryMatrix) boundaryRref,
            rationalBoundaryImageBasis =
              sparseImageBasisFromRref boundaryMatrix boundaryRref,
            rationalCoboundaryKernelBasis =
              sparseKernelBasisFromRref (smColumnCount coboundaryMatrix) coboundaryRref,
            rationalCoboundaryImageBasis =
              sparseImageBasisFromRref coboundaryMatrix coboundaryRref
          }

representativeCyclesOverQPrepared ::
  FiniteChainComplex r ->
  IntMap.IntMap RationalBoundaryDecomposition ->
  [RepresentativeCycle Rational Int]
representativeCyclesOverQPrepared finite preparedBoundaries =
  dimensionsOf finite
    >>= homologyBasisAtPrepared preparedBoundaries

representativeCocyclesOverQPrepared ::
  FiniteChainComplex r ->
  IntMap.IntMap RationalBoundaryDecomposition ->
  [RepresentativeCocycle Rational Int]
representativeCocyclesOverQPrepared finite preparedBoundaries =
  dimensionsOf finite
    >>= cohomologyBasisAtPrepared preparedBoundaries

homologyBasisAtPrepared ::
  IntMap.IntMap RationalBoundaryDecomposition ->
  HomologicalDegree ->
  [RepresentativeCycle Rational Int]
homologyBasisAtPrepared preparedBoundaries degreeValue@(HomologicalDegree degreeIndex) =
  let currentKernel =
        rationalBoundaryKernelBasis
          (rationalBoundaryAt preparedBoundaries degreeValue)
      incomingImage =
        rationalBoundaryImageBasis
          (rationalBoundaryAt preparedBoundaries (HomologicalDegree (degreeIndex + 1)))
   in sparseIndependentModulo incomingImage currentKernel
        & fmap (sparseVectorToRepresentative degreeValue)

cohomologyBasisAtPrepared ::
  IntMap.IntMap RationalBoundaryDecomposition ->
  HomologicalDegree ->
  [RepresentativeCocycle Rational Int]
cohomologyBasisAtPrepared preparedBoundaries degreeValue@(HomologicalDegree degreeIndex) =
  let currentKernel =
        rationalCoboundaryKernelBasis
          (rationalBoundaryAt preparedBoundaries (HomologicalDegree (degreeIndex + 1)))
      incomingImage =
        rationalCoboundaryImageBasis
          (rationalBoundaryAt preparedBoundaries degreeValue)
   in sparseIndependentModulo incomingImage currentKernel
        & fmap (sparseVectorToRepresentative degreeValue)

rationalBoundaryAt ::
  IntMap.IntMap RationalBoundaryDecomposition ->
  HomologicalDegree ->
  RationalBoundaryDecomposition
rationalBoundaryAt preparedBoundaries degreeValue@(HomologicalDegree degreeIndex) =
  case IntMap.lookup degreeIndex preparedBoundaries of
    Just boundaryValue -> boundaryValue
    Nothing ->
      RationalBoundaryDecomposition
        { rationalBoundaryDegree = degreeValue,
          rationalBoundaryKernelBasis = [],
          rationalBoundaryImageBasis = [],
          rationalCoboundaryKernelBasis = [],
          rationalCoboundaryImageBasis = []
        }

sparseVectorToRepresentative :: HomologicalDegree -> SparseRow -> RepresentativeChain Rational Int
sparseVectorToRepresentative degreeValue rowValue =
  RepresentativeChain
    { representativeDegree = degreeValue,
      representativeTerms =
        rowValue
          & IntMap.toAscList
          & filter (\(_, coefficientValue) -> coefficientValue /= 0)
          & fmap (\(basisIndexValue, coefficientValue) -> (coefficientValue, basisIndexValue))
    }
