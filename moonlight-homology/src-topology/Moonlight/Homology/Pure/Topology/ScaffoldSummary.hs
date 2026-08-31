module Moonlight.Homology.Pure.Topology.ScaffoldSummary
  ( SkeletonSignature (..),
    macroScaffoldSignature,
    macroScaffoldSummaryAlgebra,
    skeletonSignatureWithinTolerance,
    mkMacroScaffoldTopologyView,
    mkMacroScaffoldWitnessInterpreter,
  )
where

import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Moonlight.Homology.Pure.Chain (TopologyWitness)
import Moonlight.Homology.Pure.Skeleton (SkeletonSignature (..), skeletonSignatureWithinTolerance)
import Moonlight.Homology.Pure.Topology.MacroScaffold
  ( MacroScaffoldIR (..),
    MorseReebScaffold (..),
    Singularity (..),
  )
import Moonlight.Homology.Pure.TopologyObserver
  ( TopologyObserver,
    WitnessInterpreter,
    mkWitnessInterpreter,
  )
import Moonlight.Homology.Pure.TopologyView
  ( ScaffoldSummaryAlgebra,
    TopologyView,
    mkScaffoldSummaryAlgebra,
    mkTopologyView,
  )


macroScaffoldSignature :: MacroScaffoldIR -> SkeletonSignature
macroScaffoldSignature scaffoldValue =
  let criticalCounts =
        macroScaffoldSingularities scaffoldValue
          & fmap singularityKind
          & fmap (\criticalKindValue -> (criticalKindValue, 1))
          & Map.fromListWith (+)
   in SkeletonSignature
        { signatureCriticalCounts = criticalCounts,
          signatureArcCount = length (morseReebArcs (macroScaffoldReeb scaffoldValue))
        }

macroScaffoldSummaryAlgebra :: ScaffoldSummaryAlgebra MacroScaffoldIR SkeletonSignature
macroScaffoldSummaryAlgebra =
  mkScaffoldSummaryAlgebra macroScaffoldSignature


mkMacroScaffoldTopologyView ::
  TopologyWitness MacroScaffoldIR spectral persistence coefficient basis ->
  TopologyView SkeletonSignature MacroScaffoldIR spectral persistence coefficient basis
mkMacroScaffoldTopologyView =
  mkTopologyView macroScaffoldSummaryAlgebra

mkMacroScaffoldWitnessInterpreter ::
  TopologyObserver SkeletonSignature MacroScaffoldIR spectral persistence coefficient basis observed ->
  WitnessInterpreter SkeletonSignature MacroScaffoldIR spectral persistence coefficient basis observed
mkMacroScaffoldWitnessInterpreter =
  mkWitnessInterpreter macroScaffoldSummaryAlgebra
