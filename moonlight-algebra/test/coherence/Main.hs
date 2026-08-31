-- | Compile both focused test sections against their dependency union. Empty
-- imports retain module and instance coherence without executing the focused
-- behavioral suites a second time.
module Main (main) where

import AbstractTests ()
import FiniteLatticeTests ()

main :: IO ()
main = pure ()
