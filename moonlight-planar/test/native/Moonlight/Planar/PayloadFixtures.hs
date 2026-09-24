{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE BangPatterns #-}

-- | Position-bearing payload shared by insertion, persistence, and payload laws.
module Moonlight.Planar.PayloadFixtures
  ( SampleVertex (..)
  ) where

import Control.DeepSeq ( NFData )
import GHC.Generics ( Generic )
import Moonlight.Planar.Point (Point, HasPosition(..))


data SampleVertex = SampleVertex
  { samplePosition :: !(Point)
  , sampleLabel :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance HasPosition SampleVertex where
  position = samplePosition
