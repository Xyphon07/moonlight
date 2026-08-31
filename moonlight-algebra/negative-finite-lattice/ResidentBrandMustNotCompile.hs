{-# LANGUAGE GHC2024 #-}

module ResidentBrandMustNotCompile where

import Data.Coerce (coerce)
import Moonlight.FiniteLattice

-- Negative (must-not-compile) fixture. It lives outside every build
-- source-dir on purpose, so it is never part of the cabal build or the
-- distributed sdist. Compile it on its own with -fno-code and require
-- failure: the nominal role on the resident-key brand is the property under
-- test. If 'coerce' ever type-checks here, the brand has stopped being
-- nominal and the resident-key safety guarantee is broken.
forgeBrand :: ResidentContextKey s -> ResidentContextKey t
forgeBrand = coerce
