-- | Dense linear-system solvers: direct, conjugate-gradient, and GMRES.
module Moonlight.LinAlg.Dense.Solver
  ( solveDirect,
    solveCG,
    solveGMRES,
  )
where

import Moonlight.LinAlg.Pure.Dense.Solver
  ( solveCG,
    solveDirect,
    solveGMRES,
  )
