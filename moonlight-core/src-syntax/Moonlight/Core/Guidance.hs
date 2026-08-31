-- | Configuration and trace types for guided saturation: modes, checkpoints,
-- evidence, selections and per-round traces.
module Moonlight.Core.Guidance
  ( GuideMode (..),
    GuideCheckpoint (..),
    GuidanceConfig (..),
    GuideCheckpointHit (..),
    GuideEvidence (..),
    GuideSelection (..),
    GuideRoundTrace (..),
  )
where

import Data.Kind (Type)
import Prelude

type GuideMode :: Type
data GuideMode
  = GuidePrefer
  | GuideRequire
  deriving stock (Eq, Ord, Show, Read)

type GuideCheckpoint :: Type -> Type
data GuideCheckpoint patternValue = GuideCheckpoint
  { gcName :: String,
    gcMode :: GuideMode,
    gcTarget :: patternValue
  }

type GuidanceConfig :: Type -> Type
newtype GuidanceConfig patternValue = GuidanceConfig
  { gcCheckpoints :: [GuideCheckpoint patternValue]
  }

type GuideCheckpointHit :: Type -> Type
data GuideCheckpointHit classId = GuideCheckpointHit
  { gchCheckpointName :: String,
    gchMode :: GuideMode,
    gchPreviewClass :: classId
  }
  deriving stock (Eq, Ord, Show, Read)

type GuideEvidence :: Type -> Type
newtype GuideEvidence classId = GuideEvidence
  { geCheckpointHits :: [GuideCheckpointHit classId]
  }
  deriving stock (Eq, Ord, Show, Read)

type GuideSelection :: Type
data GuideSelection
  = GuidePassThrough
  | GuidePreferred
  | GuideRequired
  deriving stock (Eq, Ord, Show, Read)

type GuideRoundTrace :: Type
data GuideRoundTrace = GuideRoundTrace
  { grtIteration :: Int,
    grtEligibleCount :: Int,
    grtRetainedCount :: Int,
    grtGuidedCount :: Int,
    grtMatchedCheckpointCount :: Int,
    grtSelection :: GuideSelection
  }
  deriving stock (Eq, Ord, Show, Read)
