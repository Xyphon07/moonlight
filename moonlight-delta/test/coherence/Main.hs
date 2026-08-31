-- | Compile every focused test section against their dependency union. Empty
-- imports retain module and instance coherence without executing the focused
-- behavioral suites a second time.
module Main (main) where

import CoreTests ()
import CrossCarrierLaws ()
import EpochTests ()
import PatchTests ()
import RepairTests ()

main :: IO ()
main = pure ()
