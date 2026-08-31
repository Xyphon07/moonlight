-- | Assertions and exact planar fixtures shared by the test slices. Nothing
-- here may depend on an optional package flag, so the minimal configuration
-- compiles the same helpers as the full one.
module Support
  ( requireRight
  , requireQueryPoint
  , assertEqual
  , assertValid
  , integerPoint
  , rectangleLoop
  , rectangleComponent
  ) where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Triangulation
  ( ExactLoop
  , ExactPoint
  , Point
  , PolygonComponent
  , QueryPoint
  , Triangulation
  , exactLoop
  , exactPoint
  , mkQueryPoint
  , polygonComponent
  , validateTriangulation
  )

requireRight :: Show error => String -> Either error value -> IO value
requireRight label value = case value of
  Left failure -> fail (label <> ": " <> show failure)
  Right result -> pure result

requireQueryPoint :: String -> Point -> IO QueryPoint
requireQueryPoint label = requireRight label . mkQueryPoint

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)

assertValid :: String -> Triangulation mode vertex directed undirected face -> IO ()
assertValid label triangulation =
  case validateTriangulation triangulation of
    [] -> pure ()
    violations -> fail (label <> " invariant violations: " <> show violations)

integerPoint :: Integer -> Integer -> ExactPoint
integerPoint x y = exactPoint (fromInteger x) (fromInteger y)

rectangleLoop :: Integer -> Integer -> Integer -> Integer -> IO ExactLoop
rectangleLoop minimumX minimumY maximumX maximumY =
  requireRight
    "rectangle loop"
    ( exactLoop
        ( integerPoint minimumX minimumY
            :| [ integerPoint maximumX minimumY
               , integerPoint maximumX maximumY
               , integerPoint minimumX maximumY
               ]
        )
    )

rectangleComponent
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO PolygonComponent
rectangleComponent minimumX minimumY maximumX maximumY =
  rectangleLoop minimumX minimumY maximumX maximumY
    >>= requireRight "rectangle component" . (`polygonComponent` [])
