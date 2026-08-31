module Moonlight.Homology.Pure.Topology.MacroScaffold.Singularity
  ( SingularityIndex (..),
    SingularityId (..),
    Singularity (..),
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Carrier (BasisCellRef)
import Moonlight.Homology.Pure.Filtration (CriticalKind)
import Moonlight.Homology.Pure.Topology.MacroScaffold.Potential (PotentialValue)
import Moonlight.Homology.Pure.Topology.MacroScaffold.Reeb (ReebArcId, ReebNodeId)

type SingularityIndex :: Type
newtype SingularityIndex = SingularityIndex
  { unSingularityIndex :: Rational
  }
  deriving stock (Eq, Ord, Show)

type SingularityId :: Type
newtype SingularityId = SingularityId
  { unSingularityId :: Int
  }
  deriving stock (Eq, Ord, Show)

type Singularity :: Type
data Singularity = Singularity
  { singularityId :: SingularityId,
    singularityAnchor :: BasisCellRef,
    singularityKind :: CriticalKind,
    singularityPotential :: Maybe PotentialValue,
    singularityIndex :: SingularityIndex,
    singularityReebNode :: Maybe ReebNodeId,
    singularityIncidentArcs :: [ReebArcId]
  }
  deriving stock (Eq, Show)
