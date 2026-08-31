module Moonlight.Homology.Pure.Topology.Harmonic
  ( attachDiscoveredHarmonicLoops,
    discoverHarmonicLoops,
    harmonicBasisAt,
  )
where

import Data.Function ((&))
import Data.IntMap.Strict qualified as IntMap
import Data.Set qualified as Set
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    incidenceMatrixAt,
  )
import Moonlight.Homology.Pure.Chain
  ( HarmonicBasisElement (..),
    HomologicalDegree (..),
    incrementDegree,
    RepresentativeChain (..),
    RepresentativeCycle,
    TopologyWitness (..),
  )
import Moonlight.Homology.Pure.Carrier (BasisCellRef (..))
import Moonlight.Homology.Pure.Filtration (enumerateFromZero)
import Moonlight.Homology.Pure.Matrix.Shape (cellCountAtDegree)
import Moonlight.Homology.Pure.Topology.MacroScaffold
  ( HarmonicLoop (..),
    HarmonicLoopId (..),
    HarmonicLoopPeriod (..),
    HarmonicLoopWeight (..),
    MacroScaffoldIR (..),
    MorseReebArc (..),
    MorseReebScaffold (..),
    ReebArcId,
  )
import Moonlight.Homology.Pure.Matrix.SparseLinAlg
  ( SparseMatrix (..),
    SparseRow,
    sparseBoundaryMatrix,
    sparseKernelBasisOf,
    sparseTransposeMatrix,
  )

attachDiscoveredHarmonicLoops ::
  Integral r =>
  FiniteChainComplex r ->
  TopologyWitness MacroScaffoldIR spectral persistence Rational Int ->
  TopologyWitness MacroScaffoldIR spectral persistence Rational Int
attachDiscoveredHarmonicLoops finite witnessValue =
  let harmonicDegree = HomologicalDegree 1
      harmonicRows = harmonicBasisAt finite harmonicDegree
   in witnessValue
        { topologyMacroScaffold =
            fmap
              (attachLoopsWhenMissing harmonicRows)
              (topologyMacroScaffold witnessValue),
          topologyHarmonicBasis =
            if null (topologyHarmonicBasis witnessValue)
              then fmap (harmonicBasisElement harmonicDegree) harmonicRows
              else topologyHarmonicBasis witnessValue
        }

harmonicBasisAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  [SparseRow]
harmonicBasisAt finite degreeValue =
  let ambientDimension = cellCountAtDegree finite degreeValue
   in if ambientDimension == 0
        then []
        else
          sparseKernelBasisOf
            ambientDimension
            (harmonicConstraintMatrix finite degreeValue ambientDimension)

harmonicConstraintMatrix ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  Int ->
  SparseMatrix
harmonicConstraintMatrix finite degreeValue ambientDimension =
  SparseMatrix
    { smRows =
        boundaryRowsAt finite degreeValue
          <> coboundaryRowsAt finite (incrementDegree degreeValue),
      smColumnCount = ambientDimension
    }

boundaryRowsAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  [SparseRow]
boundaryRowsAt finite degreeValue =
  smRows (sparseBoundaryMatrix (incidenceMatrixAt finite degreeValue))

coboundaryRowsAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  [SparseRow]
coboundaryRowsAt finite degreeValue =
  smRows
    ( sparseTransposeMatrix
        (sparseBoundaryMatrix (incidenceMatrixAt finite degreeValue))
    )

attachLoopsWhenMissing ::
  [SparseRow] ->
  MacroScaffoldIR ->
  MacroScaffoldIR
attachLoopsWhenMissing harmonicRows scaffoldValue =
  if null (macroScaffoldHarmonicLoops scaffoldValue)
    then
      scaffoldValue
        { macroScaffoldHarmonicLoops =
            discoverHarmonicLoops scaffoldValue harmonicRows
        }
    else scaffoldValue

discoverHarmonicLoops ::
  MacroScaffoldIR ->
  [SparseRow] ->
  [HarmonicLoop]
discoverHarmonicLoops scaffoldValue harmonicRows =
  zip (fmap HarmonicLoopId (enumerateFromZero (length harmonicRows))) harmonicRows
    & fmap
      ( \(loopIdValue, harmonicRow) ->
          harmonicLoopFromRow scaffoldValue loopIdValue harmonicRow
      )

harmonicLoopFromRow ::
  MacroScaffoldIR ->
  HarmonicLoopId ->
  SparseRow ->
  HarmonicLoop
harmonicLoopFromRow scaffoldValue loopIdValue harmonicRow =
  let representativeValue =
        sparseRowToRepresentative (HomologicalDegree 1) harmonicRow
      periodValue =
        harmonicRow
          & IntMap.foldl' (\accumulator coefficientValue -> accumulator + coefficientValue * coefficientValue) 0
          & fromRational
      weightValue =
        harmonicRow
          & IntMap.foldl' (\accumulator coefficientValue -> accumulator + abs coefficientValue) 0
          & fromRational
   in HarmonicLoop
        { harmonicLoopId = loopIdValue,
          harmonicLoopDegree = HomologicalDegree 1,
          harmonicLoopCycle = representativeValue,
          harmonicLoopCocycle = representativeValue,
          harmonicLoopWeight = HarmonicLoopWeight weightValue,
          harmonicLoopPeriod =
            if periodValue == 0
              then Nothing
              else Just (HarmonicLoopPeriod periodValue),
          harmonicLoopSupport = supportArcIds scaffoldValue representativeValue
        }

sparseRowToRepresentative ::
  HomologicalDegree ->
  SparseRow ->
  RepresentativeCycle Rational BasisCellRef
sparseRowToRepresentative degreeValue harmonicRow =
  RepresentativeChain
    { representativeDegree = degreeValue,
      representativeTerms =
        IntMap.toAscList harmonicRow
          & filter ((/= 0) . snd)
          & fmap
            ( \(basisIndexValue, coefficientValue) ->
                ( coefficientValue,
                  BasisCellRef
                    { cellDegree = degreeValue,
                      cellIndex = basisIndexValue
                    }
                )
            )
    }

harmonicBasisElement :: HomologicalDegree -> SparseRow -> HarmonicBasisElement Rational Int
harmonicBasisElement degreeValue harmonicRow =
  HarmonicBasisElement
    { harmonicDegree = degreeValue,
      harmonicRepresentative =
        RepresentativeChain
          { representativeDegree = degreeValue,
            representativeTerms =
              IntMap.toAscList harmonicRow
                & filter ((/= 0) . snd)
                & fmap (\(basisIndexValue, coefficientValue) -> (coefficientValue, basisIndexValue))
          }
    }

supportArcIds ::
  MacroScaffoldIR ->
  RepresentativeCycle Rational BasisCellRef ->
  [ReebArcId]
supportArcIds scaffoldValue cycleValue =
  let cycleSupport =
        representativeTerms cycleValue
          & fmap snd
          & Set.fromList
      arcSupport arcValue =
        morseReebArcSupport arcValue
          & Set.fromList
   in morseReebArcs (macroScaffoldReeb scaffoldValue)
        & filter (\arcValue -> not (Set.null (Set.intersection cycleSupport (arcSupport arcValue))))
        & fmap morseReebArcId
