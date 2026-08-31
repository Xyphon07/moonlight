-- | Eigenvalue and eigenpair requests and contiguous result views, dispatched by demand and operator structure above Krylov.
module Moonlight.LinAlg.Spectral
  ( EigenRequest (..),
    Eigenpairs,
    CertifiedSelectedEigenpairResult (..),
    SelectedEigenpairCertificationFailure (..),
    SelectedEigenpairOrthonormalityEvidence (..),
    SelectedEigenpairRequestOrderingEvidence (..),
    SelectedEigenpairResidualEvidence (..),
    certifySelectedEigenpairResult,
    eigenpairDimension,
    eigenpairValues,
    eigenpairVectorsColumnMajor,
    eigenpairResidualNorms,
    eigenpairCount,
    eigenpairVectorAt,
    EigenSolveConfig,
    defaultEigenSolveConfig,
    withEigenFallbackLanczosConfig,
    withEigenFallbackInitialVector,
    solveEigenRequest,
  )
where

import Moonlight.LinAlg.Pure.Spectral.Request
  ( EigenRequest (..),
  )
import Moonlight.LinAlg.Pure.Spectral.Result
  ( Eigenpairs,
    CertifiedSelectedEigenpairResult (..),
    SelectedEigenpairCertificationFailure (..),
    SelectedEigenpairOrthonormalityEvidence (..),
    SelectedEigenpairRequestOrderingEvidence (..),
    SelectedEigenpairResidualEvidence (..),
    certifySelectedEigenpairResult,
    eigenpairCount,
    eigenpairDimension,
    eigenpairResidualNorms,
    eigenpairValues,
    eigenpairVectorAt,
    eigenpairVectorsColumnMajor,
  )
import Moonlight.LinAlg.Pure.Spectral.Solve
  ( EigenSolveConfig,
    defaultEigenSolveConfig,
    withEigenFallbackLanczosConfig,
    withEigenFallbackInitialVector,
    solveEigenRequest,
  )
