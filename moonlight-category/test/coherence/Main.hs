-- | Compile every focused test section against their dependency union. Empty
-- imports retain module and instance coherence without executing the focused
-- behavioral suites a second time.
module Main (main) where

import AbstractTests ()
import FacadeTests ()
import FiniteTests ()
import IndexedTests ()
import Moonlight.Category.Effect.Laws ()
import SimplicialTests ()
import SiteTests ()

main :: IO ()
main = pure ()
