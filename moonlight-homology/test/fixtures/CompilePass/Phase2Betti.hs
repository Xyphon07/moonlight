{-# LANGUAGE DataKinds #-}

module CompilePass.Phase2Betti (phase2Result) where

import Moonlight.Homology

phase2Result :: Either HomologyFailure [HomologyGroup Rational]
phase2Result = do
  finite <- mkFiniteChainComplexChecked (HomologicalDegree 0) (const emptyBoundaryIncidence)
  computeBettiNumbers
    (fieldBettiCapability RationalFieldRankBackend :: BettiCapability 'Phase2 Rational)
    finite
