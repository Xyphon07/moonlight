module Moonlight.Homology.Pure.Topology.MacroScaffold.Reeb
  ( ReebNodeId (..),
    ReebArcId (..),
    MorseReebNode (..),
    Monotonicity (..),
    MorseReebArc (..),
    MorseReebScaffold (..),
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Carrier (BasisCellRef)
import Moonlight.Homology.Pure.Filtration (CriticalKind)
import Moonlight.Homology.Pure.Topology.MacroScaffold.Potential (PotentialValue)

type ReebNodeId :: Type
newtype ReebNodeId = ReebNodeId
  { unReebNodeId :: Int
  }
  deriving stock (Eq, Ord, Show)

type ReebArcId :: Type
newtype ReebArcId = ReebArcId
  { unReebArcId :: Int
  }
  deriving stock (Eq, Ord, Show)

type MorseReebNode :: Type
data MorseReebNode = MorseReebNode
  { morseReebNodeId :: ReebNodeId,
    morseReebNodeAnchor :: BasisCellRef,
    morseReebNodeKind :: CriticalKind,
    morseReebNodePotential :: PotentialValue
  }
  deriving stock (Eq, Show)

type Monotonicity :: Type
data Monotonicity
  = Ascending
  | Descending
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type MorseReebArc :: Type
data MorseReebArc = MorseReebArc
  { morseReebArcId :: ReebArcId,
    morseReebArcSource :: ReebNodeId,
    morseReebArcTarget :: ReebNodeId,
    morseReebArcMonotonicity :: Monotonicity,
    morseReebArcSupport :: [BasisCellRef]
  }
  deriving stock (Eq, Show)

type MorseReebScaffold :: Type
data MorseReebScaffold = MorseReebScaffold
  { morseReebNodes :: [MorseReebNode],
    morseReebArcs :: [MorseReebArc]
  }
  deriving stock (Eq, Show)
