module Moonlight.Homology.Pure.Topology.MacroScaffold
  ( module X,
    BasisCellRef (..),
    CellCarrier,
    CellCarrierError (..),
    carrierDegree,
    carrierCells,
    mkCellCarrier,
    MacroScaffoldIR (..),
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Carrier
  ( BasisCellRef (..),
    CellCarrier,
    CellCarrierError (..),
    carrierCells,
    carrierDegree,
    mkCellCarrier,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Direction as X
import Moonlight.Homology.Pure.Topology.MacroScaffold.HarmonicLoop as X
import Moonlight.Homology.Pure.Topology.MacroScaffold.Potential as X
import Moonlight.Homology.Pure.Topology.MacroScaffold.Reeb as X
import Moonlight.Homology.Pure.Topology.MacroScaffold.Singularity as X

type MacroScaffoldIR :: Type
data MacroScaffoldIR = MacroScaffoldIR
  { macroScaffoldScalarPotential :: ScalarPotentialField,
    macroScaffoldReeb :: MorseReebScaffold,
    macroScaffoldDirectionField :: DirectionField,
    macroScaffoldSingularities :: [Singularity],
    macroScaffoldHarmonicLoops :: [HarmonicLoop]
  }
  deriving stock (Eq, Show)
