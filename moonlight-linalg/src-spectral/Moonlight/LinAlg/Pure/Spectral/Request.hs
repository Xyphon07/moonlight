{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Spectral.Request
  ( EigenRequest (..),
  )
where

import Data.Kind (Type)
import Data.Vector.Unboxed qualified as U
import Moonlight.LinAlg.Pure.Krylov.Config (PositiveCount)
import Moonlight.LinAlg.Pure.Krylov.Selection (SpectrumEnd)
import Moonlight.LinAlg.Pure.Spectral.Result (Eigenpairs)
import Prelude

type EigenRequest :: Type -> Type
data EigenRequest result where
  EigenvaluesRequest ::
    !SpectrumEnd ->
    !PositiveCount ->
    EigenRequest (U.Vector Double)
  EigenpairsRequest ::
    !SpectrumEnd ->
    !PositiveCount ->
    EigenRequest Eigenpairs
