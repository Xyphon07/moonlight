module Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Core
  ( MacroScaffoldCompositionError (..),
    ScaffoldOffsets (..),
    zeroScaffoldOffsets,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Moonlight.Homology.Pure.Chain
  ( HomologicalDegree,
  )
import Moonlight.Homology.Pure.Carrier
  ( CellCarrierError,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Direction
  ( DirectionFieldError,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Potential
  ( ScalarPotentialFieldError,
  )

import qualified Data.Map.Strict as Map

type MacroScaffoldCompositionError :: Type
data MacroScaffoldCompositionError
  = MismatchedScalarPotentialNormalizations
  | MismatchedScalarPotentialCarrierDegrees
  | InvalidComposedScalarPotentialCarrier CellCarrierError
  | InvalidComposedScalarPotential ScalarPotentialFieldError
  | MismatchedDirectionSymmetryOrders
  | MismatchedDirectionCarrierDegrees
  | MismatchedDirectionEncodingFamilies
  | InvalidComposedDirectionCarrier CellCarrierError
  | InvalidComposedDirectionField DirectionFieldError
  deriving stock (Eq, Show)

type ScaffoldOffsets :: Type
data ScaffoldOffsets = ScaffoldOffsets
  { soBasisOffsets :: Map HomologicalDegree Int,
    soNodeOffset :: Int,
    soArcOffset :: Int,
    soSingularityOffset :: Int,
    soLoopOffset :: Int
  }

zeroScaffoldOffsets :: ScaffoldOffsets
zeroScaffoldOffsets =
  ScaffoldOffsets
    { soBasisOffsets = Map.empty,
      soNodeOffset = 0,
      soArcOffset = 0,
      soSingularityOffset = 0,
      soLoopOffset = 0
    }
