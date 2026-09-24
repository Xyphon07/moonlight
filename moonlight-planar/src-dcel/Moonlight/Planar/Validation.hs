-- | Whole-value DCEL validation and face-quality observations.
module Moonlight.Planar.Validation
  ( validateTopology
  , validateDelaunay
  , validateTriangulation
  , triangulationIsValid
  , faceArea
  , faceMinimumAngleDegrees
  ) where

import Moonlight.Planar.Internal.Validation
