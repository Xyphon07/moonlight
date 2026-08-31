{-# LANGUAGE DataKinds #-}

module CompileFail.Phase1BettiLeak (forbiddenPhase1Capability) where

import Moonlight.Homology

forbiddenPhase1Capability :: BettiCapability 'Phase1 Rational
forbiddenPhase1Capability =
  fieldBettiCapability RationalFieldRankBackend
