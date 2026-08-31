module Moonlight.LinAlg.Pure.Sparse.Solver.Types
  ( SparseIterativeFailure (..),
    SparseIterativeResult (..),
    IC0Config (..),
    SparsePreconditionerFamily (..),
    defaultSparsePreconditionerFamily,
    SparseStationaryIterationConfig (..),
    SparseConjugateGradientConfig (..),
    SparseGMRESConfig (..),
  )
where

import Data.Kind (Type)
import qualified Data.Vector.Unboxed as U
import Moonlight.Core (MoonlightError)
import Prelude

type SparseIterativeFailure :: Type
data SparseIterativeFailure
  = SparseIterationBudgetExceeded Int
  | SparseInvalidInput String
  | SparseMissingDiagonal Int
  | SparseNonpositivePivot Int Double
  | SparseNonFiniteUpdate Int Int Double
  | SparseStructuralAsymmetry Int Int
  | SparseSuspectedNullspaceUnanchoredLaplacian Int Double
  | SparseInvalidDiagonalShift Double
  | SparseNonSquareSparsePreconditioner Int Int
  | SparseBackendFailure MoonlightError
  deriving stock (Eq, Show)

type SparseIterativeResult :: Type
data SparseIterativeResult = SparseIterativeResult
  { sparseSolution :: !(U.Vector Double),
    sparseIterations :: !Int,
    sparseResidualNorm :: !Double
  }
  deriving stock (Eq, Show)

type IC0Config :: Type
data IC0Config = IC0Config
  { ic0DiagonalShift :: !(Maybe Double)
  }
  deriving stock (Eq, Ord, Show, Read)

type SparsePreconditionerFamily :: Type
data SparsePreconditionerFamily
  = IdentitySparsePreconditionerFamily
  | DiagonalJacobiSparsePreconditionerFamily
  | ShiftedDiagonalJacobiSparsePreconditionerFamily Double
  | SsorSparsePreconditionerFamily Double
  | IncompleteCholesky0SparsePreconditionerFamily IC0Config
  deriving stock (Eq, Ord, Show, Read)

defaultSparsePreconditionerFamily :: SparsePreconditionerFamily
defaultSparsePreconditionerFamily = DiagonalJacobiSparsePreconditionerFamily

type SparseStationaryIterationConfig :: Type
data SparseStationaryIterationConfig = SparseStationaryIterationConfig
  { ssicTolerance :: Double,
    ssicIterationLimit :: Int,
    ssicDamping :: Double
  }
  deriving stock (Eq, Show)

type SparseConjugateGradientConfig :: Type
data SparseConjugateGradientConfig = SparseConjugateGradientConfig
  { scgcTolerance :: Double,
    scgcIterationLimit :: Int,
    scgcPreconditionerFamily :: SparsePreconditionerFamily
  }
  deriving stock (Eq, Show)

type SparseGMRESConfig :: Type
data SparseGMRESConfig = SparseGMRESConfig
  { sgcTolerance :: Double,
    sgcIterationLimit :: Int,
    sgcRestartDimension :: Int,
    sgcPreconditionerFamily :: SparsePreconditionerFamily
  }
  deriving stock (Eq, Show)
