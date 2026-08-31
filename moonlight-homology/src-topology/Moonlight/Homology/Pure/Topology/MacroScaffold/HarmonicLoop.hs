module Moonlight.Homology.Pure.Topology.MacroScaffold.HarmonicLoop
  ( HarmonicLoopId (..),
    HarmonicLoopWeight (..),
    HarmonicLoopPeriod (..),
    HarmonicLoop (..),
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Chain
  ( HomologicalDegree,
    RepresentativeCocycle,
    RepresentativeCycle,
  )
import Moonlight.Homology.Pure.Carrier (BasisCellRef)
import Moonlight.Homology.Pure.Topology.MacroScaffold.Reeb (ReebArcId)

type HarmonicLoopId :: Type
newtype HarmonicLoopId = HarmonicLoopId
  { unHarmonicLoopId :: Int
  }
  deriving stock (Eq, Ord, Show)

type HarmonicLoopWeight :: Type
newtype HarmonicLoopWeight = HarmonicLoopWeight
  { unHarmonicLoopWeight :: Double
  }
  deriving stock (Eq, Ord, Show)

type HarmonicLoopPeriod :: Type
newtype HarmonicLoopPeriod = HarmonicLoopPeriod
  { unHarmonicLoopPeriod :: Double
  }
  deriving stock (Eq, Ord, Show)

type HarmonicLoop :: Type
data HarmonicLoop = HarmonicLoop
  { harmonicLoopId :: HarmonicLoopId,
    harmonicLoopDegree :: HomologicalDegree,
    harmonicLoopCycle :: RepresentativeCycle Rational BasisCellRef,
    harmonicLoopCocycle :: RepresentativeCocycle Rational BasisCellRef,
    harmonicLoopWeight :: HarmonicLoopWeight,
    harmonicLoopPeriod :: Maybe HarmonicLoopPeriod,
    harmonicLoopSupport :: [ReebArcId]
  }
  deriving stock (Eq, Show)
