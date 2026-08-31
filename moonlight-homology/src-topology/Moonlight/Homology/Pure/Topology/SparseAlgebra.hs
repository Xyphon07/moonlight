module Moonlight.Homology.Pure.Topology.SparseAlgebra
  ( sparseHomologyBasisAt,
    sparseCohomologyBasisAt,
    sparseFreeBettiVector,
    sparseQuotientRepresentatives,
  )
where

import Data.Function ((&))
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Homology.Boundary.Finite (FiniteChainComplex, incidenceMatrixAt)
import Moonlight.Homology.Pure.Chain
  ( HomologicalDegree (..),
    RepresentativeChain (..),
    RepresentativeCocycle,
    RepresentativeCycle,
  )
import Moonlight.Homology.Pure.Matrix.Shape
  ( cellCountAtDegree,
    dimensionsOf,
  )
import Moonlight.Homology.Pure.Topology.Graph
  ( GraphOneComplex (..),
    graphOneComplexFromComplex,
  )
import Moonlight.Homology.Pure.Matrix.SparseLinAlg
  ( SparseMatrix (..),
    SparseRow,
    sparseBoundaryColumns,
    sparseBoundaryMatrix,
    sparseIndependentModulo,
    sparseKernelBasisOf,
    sparseTransposeMatrix,
  )

sparseHomologyBasisAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  [RepresentativeCycle Rational Int]
sparseHomologyBasisAt finite degreeValue@(HomologicalDegree degreeIndex) =
  case degreeValue of
    HomologicalDegree 0 ->
      maybe
        (genericSparseHomologyBasisAt finite degreeValue degreeIndex)
        graphHomologyZeroRepresentatives
        (graphOneComplexFromComplex finite)
    _ ->
      genericSparseHomologyBasisAt finite degreeValue degreeIndex

sparseCohomologyBasisAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  [RepresentativeCocycle Rational Int]
sparseCohomologyBasisAt finite degreeValue@(HomologicalDegree degreeIndex) =
  case degreeValue of
    HomologicalDegree 0 ->
      maybe
        (genericSparseCohomologyBasisAt finite degreeValue degreeIndex)
        graphCohomologyZeroRepresentatives
        (graphOneComplexFromComplex finite)
    _ ->
      genericSparseCohomologyBasisAt finite degreeValue degreeIndex

genericSparseHomologyBasisAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  Int ->
  [RepresentativeCycle Rational Int]
genericSparseHomologyBasisAt finite degreeValue degreeIndex =
  sparseQuotientRepresentatives
    degreeValue
    (cellCountAtDegree finite degreeValue)
    (sparseBoundaryMatrix (incidenceMatrixAt finite degreeValue))
    ( Vector.toList
        (sparseBoundaryColumns (incidenceMatrixAt finite (HomologicalDegree (degreeIndex + 1))))
    )

genericSparseCohomologyBasisAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  Int ->
  [RepresentativeCocycle Rational Int]
genericSparseCohomologyBasisAt finite degreeValue degreeIndex =
  sparseQuotientRepresentatives
    degreeValue
    (cellCountAtDegree finite degreeValue)
    (sparseTransposeMatrix (sparseBoundaryMatrix (incidenceMatrixAt finite (HomologicalDegree (degreeIndex + 1)))))
    (smRows (sparseBoundaryMatrix (incidenceMatrixAt finite degreeValue)))

graphHomologyZeroRepresentatives :: GraphOneComplex -> [RepresentativeCycle Rational Int]
graphHomologyZeroRepresentatives graph =
  graphOneComponents graph
    & mapMaybe Set.lookupMin
    & fmap
      ( \vertexIndex ->
          RepresentativeChain
            { representativeDegree = HomologicalDegree 0,
              representativeTerms = [(1, vertexIndex)]
            }
      )

graphCohomologyZeroRepresentatives :: GraphOneComplex -> [RepresentativeCocycle Rational Int]
graphCohomologyZeroRepresentatives graph =
  graphOneComponents graph
    & fmap
      ( \component ->
          RepresentativeChain
            { representativeDegree = HomologicalDegree 0,
              representativeTerms = fmap (\vertexIndex -> (1, vertexIndex)) (Set.toAscList component)
            }
      )

sparseFreeBettiVector :: Integral r => FiniteChainComplex r -> [Int]
sparseFreeBettiVector finite =
  dimensionsOf finite
    & fmap (length . sparseHomologyBasisAt finite)

sparseQuotientRepresentatives ::
  HomologicalDegree ->
  Int ->
  SparseMatrix ->
  [SparseRow] ->
  [RepresentativeChain Rational Int]
sparseQuotientRepresentatives degreeValue ambientDimension currentMatrix imageGenerators =
  let kernelBasis = sparseKernelBasisOf ambientDimension currentMatrix
      quotientBasis = sparseIndependentModulo imageGenerators kernelBasis
   in fmap (sparseVectorToRepresentative degreeValue) quotientBasis

sparseVectorToRepresentative :: HomologicalDegree -> SparseRow -> RepresentativeChain Rational Int
sparseVectorToRepresentative degreeValue rowValue =
  RepresentativeChain
    { representativeDegree = degreeValue,
      representativeTerms =
        IntMap.toAscList rowValue
          & filter (\(_, coefficientValue) -> coefficientValue /= 0)
          & fmap (\(basisIndexValue, coefficientValue) -> (coefficientValue, basisIndexValue))
    }
