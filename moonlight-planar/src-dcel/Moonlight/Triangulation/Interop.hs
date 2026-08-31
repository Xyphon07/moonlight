{-# LANGUAGE FlexibleInstances #-}

-- | Boundary conversions between Moonlight points and common coordinate pairs.
module Moonlight.Triangulation.Interop
  ( Coordinate2 (..)
  , mapCoordinate2
  ) where

import Data.Complex (Complex ((:+)))
import Moonlight.Triangulation.Types (Point (..))

-- | Minimal mint-like interchange class. It owns no geometry and introduces no
-- second point representation inside the triangulation; conversion happens only
-- at an ecosystem boundary.
class Coordinate2 value where
  toPoint :: value -> Point
  fromPoint :: Point -> value

instance Coordinate2 (Point) where
  toPoint = id
  fromPoint = id

instance Coordinate2 (Double, Double) where
  toPoint (x, y) = Point x y
  fromPoint (Point x y) = (x, y)

instance Coordinate2 (Complex Double) where
  toPoint (x :+ y) = Point x y
  fromPoint (Point x y) = x :+ y

-- | Transform a coordinate through the canonical 'Point' representation.
mapCoordinate2
  :: (Coordinate2 input, Coordinate2 output)
  => (Point -> Point)
  -> input
  -> output
mapCoordinate2 transform = fromPoint . transform . toPoint
