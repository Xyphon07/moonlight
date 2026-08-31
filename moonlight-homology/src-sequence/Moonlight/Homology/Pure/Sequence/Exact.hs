module Moonlight.Homology.Pure.Sequence.Exact
  ( MayerVietorisReducer (..),
    MayerVietorisCapability,
    mayerVietoris,
  )
where

import Data.Kind (Type)
import Moonlight.Core (Capability, withCapability)
import Moonlight.Homology.Pure.Failure (HomologyFailure)
import Moonlight.Homology.Pure.Phase (HomologyPhase, RequirePhase2)

type MayerVietorisReducer :: Type -> Type
newtype MayerVietorisReducer space = MayerVietorisReducer
  { runMayerVietorisReducer :: space -> space -> space -> Either HomologyFailure space
  }

type MayerVietorisCapability :: HomologyPhase -> Type -> Type
type MayerVietorisCapability phase space =
  Capability RequirePhase2 phase (MayerVietorisReducer space)

mayerVietoris :: MayerVietorisCapability phase space -> space -> space -> space -> Either HomologyFailure space
mayerVietoris capability a b c =
  withCapability capability
    (\reducer -> runMayerVietorisReducer reducer a b c)
