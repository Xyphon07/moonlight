module Moonlight.Homology.Boundary.SNF
  ( SmithNormalForm (..),
    SNFReducer (..),
    SNFCapability,
    computeSmithNormalForm,
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Boundary.LinAlg (BoundaryIncidence)
import Moonlight.Core (Capability, withCapability)
import Moonlight.Homology.Pure.Failure (HomologyFailure)
import Moonlight.Homology.Pure.Phase (HomologyPhase, RequirePhase2)

type SmithNormalForm :: Type -> Type
data SmithNormalForm r = SmithNormalForm
  { leftRank :: Int,
    rightRank :: Int,
    diagonalEntries :: [r]
  }
  deriving stock (Eq, Show)

type SNFReducer :: Type -> Type
newtype SNFReducer r = SNFReducer
  { runSNFReducer :: BoundaryIncidence r -> Either HomologyFailure (SmithNormalForm r)
  }

type SNFCapability :: HomologyPhase -> Type -> Type
type SNFCapability phase r =
  Capability RequirePhase2 phase (SNFReducer r)

computeSmithNormalForm :: SNFCapability phase r -> BoundaryIncidence r -> Either HomologyFailure (SmithNormalForm r)
computeSmithNormalForm capability boundaryIncidence =
  withCapability capability
    (\reducer -> runSNFReducer reducer boundaryIncidence)
