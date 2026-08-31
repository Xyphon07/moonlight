{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module CompileFail.Phase2SpectralLeak (forbiddenPhase2Spectral) where

import Moonlight.Homology

forbiddenPhase2Spectral :: ()
forbiddenPhase2Spectral =
  requirePhase4Witness @'Phase2
