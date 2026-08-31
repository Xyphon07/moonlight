module Moonlight.Homology.Pure.Topology.Graph.Witness
  ( graphMacroScaffold,
    graphSpectralModes,
    graphTopologyWitness,
  )
where

import Moonlight.Homology.Pure.Chain (TopologyWitness (..), emptyTopologyWitness)
import Moonlight.Homology.Pure.Failure (HomologyFailure)
import Moonlight.Homology.Pure.Topology.Core (Graph1Skeleton (..), GraphEdge (..), GraphSpectralMode)
import Moonlight.Homology.Pure.Topology.Graph.Critical
  ( criticalNodes,
    graphDirectionField,
    scalarPotentialByVertex,
  )
import Moonlight.Homology.Pure.Topology.Graph.Reeb (graphReebArcs, graphSingularities)
import Moonlight.Homology.Pure.Topology.MacroScaffold
  ( MacroScaffoldIR (..),
    MorseReebScaffold (..),
    ScalarPotentialField,
  )
import Moonlight.Homology.Pure.Topology.Spectral
  ( defaultSparseSpectralConfig,
    weightedGraphSparseSpectralModes,
  )

graphMacroScaffold :: ScalarPotentialField -> Graph1Skeleton -> Either HomologyFailure MacroScaffoldIR
graphMacroScaffold scalarPotential skeleton = do
  potentials <- scalarPotentialByVertex skeleton scalarPotential
  directionField <- graphDirectionField potentials skeleton
  nodes <- criticalNodes scalarPotential potentials skeleton
  let arcs = graphReebArcs potentials skeleton nodes
  pure
    MacroScaffoldIR
      { macroScaffoldScalarPotential = scalarPotential,
        macroScaffoldReeb =
          MorseReebScaffold
            { morseReebNodes = nodes,
              morseReebArcs = arcs
            },
        macroScaffoldDirectionField = directionField,
        macroScaffoldSingularities = graphSingularities nodes arcs,
        macroScaffoldHarmonicLoops = []
      }

graphSpectralModes :: Int -> Graph1Skeleton -> Either HomologyFailure [GraphSpectralMode]
graphSpectralModes requestedModeCount skeleton
  | requestedModeCount <= 0 = Right []
  | graphVertexCount skeleton <= 0 = Right []
  | otherwise =
      weightedGraphSparseSpectralModes
        defaultSparseSpectralConfig
        requestedModeCount
        (graphVertexCount skeleton)
        (fmap unweightedGraphEdgeSupport (graphEdges skeleton))

unweightedGraphEdgeSupport :: GraphEdge -> (Int, Int, Double)
unweightedGraphEdgeSupport edgeValue =
  (graphEdgeSource edgeValue, graphEdgeTarget edgeValue, 1.0)

graphTopologyWitness ::
  Int ->
  Maybe ScalarPotentialField ->
  Graph1Skeleton ->
  Either HomologyFailure (TopologyWitness MacroScaffoldIR GraphSpectralMode persistence coefficient basis)
graphTopologyWitness requestedModeCount potentialValues skeleton = do
  spectralModes <- graphSpectralModes requestedModeCount skeleton
  scaffoldValue <- traverse (`graphMacroScaffold` skeleton) potentialValues
  pure
    emptyTopologyWitness
      { topologyMacroScaffold = scaffoldValue,
        topologyLowSpectralModes = spectralModes
      }
