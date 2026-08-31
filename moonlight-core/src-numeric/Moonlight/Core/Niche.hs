-- | Checked niche vocabulary: topographic samples ('TopoSample') and stressor sets, validated at construction.
module Moonlight.Core.Niche
  ( NicheValidationError (..),
    TopoSample,
    mkTopoSample,
    flatTopoSample,
    topoSlope,
    topoAspect,
    topoCurvature,
    StressorId,
    mkStressorId,
    renderStressorId,
    defaultStressorId,
    ActiveStressor,
    mkActiveStressor,
    activeStressorId,
    activeStressorIntensity,
    ActiveStressorSet,
    emptyActiveStressorSet,
    activeStressorSetFromList,
    activeStressorSetEntries,
    activeStressorSetTopEntries,
    ContextSignature,
    emptyContextSignature,
    mkContextSignature,
    contextSignatureBins,
  )
where

import Moonlight.Core.Niche.Internal

