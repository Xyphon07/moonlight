-- | Binary64 points and sealed, normalized query coordinates.
module Moonlight.Planar.Point
  ( Point (..)
  , QueryPoint
  , queryPointValue
  , PointValidationError (..)
  , HasPosition (..)
  , mkQueryPoint
  , canonicalPoint
  , mitigateUnderflow
  ) where

import Moonlight.Planar.Internal.Point
