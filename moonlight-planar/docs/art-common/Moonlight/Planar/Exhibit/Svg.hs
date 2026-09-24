module Moonlight.Planar.Exhibit.Svg (polygon, circle, pointText, scalar) where

import Moonlight.Planar.Point (Point (..))
import Text.Printf (printf)

polygon :: String -> [Point] -> String
polygon attributes points = "<polygon points=\"" <> unwords (pointText <$> points) <> "\" " <> attributes <> "/>"

circle :: Point -> Double -> String -> String
circle (Point x y) radius attributes = "<circle cx=\"" <> scalar x <> "\" cy=\"" <> scalar y
  <> "\" r=\"" <> scalar radius <> "\" " <> attributes <> "/>"

pointText :: Point -> String
pointText (Point x y) = scalar x <> "," <> scalar y

scalar :: Double -> String
scalar = printf "%.3f"
