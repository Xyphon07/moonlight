-- | Compile every focused test section against their dependency union. Empty
-- imports retain module and instance coherence without executing the focused
-- behavioral suites a second time.
module Main where

import BasisTests ()
import NumericTests ()
import PublicSurfaceSpec ()
import SolverTests ()
import SyntaxTests ()
import TermTests ()

main :: IO ()
main = pure ()
