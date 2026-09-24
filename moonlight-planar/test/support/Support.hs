{-# LANGUAGE NumericUnderscores #-}

-- | Assertions and exact planar fixtures shared by the test slices. Nothing
-- here may depend on an optional package flag, so the minimal configuration
-- compiles the same helpers as the full one.
module Support
  ( requireRight
  , requireQueryPoint
  , requireJust
  , randomPoints
  , assertEqual
  , assertValid
  , integerPoint
  , rectangleLoop
  , rectangleComponent
  , rectangleRegion
  , polygonComponentOf
  , polygonRegion
  , annulusRegion
  ) where

import Control.Monad (unless)
import qualified Data.Set as Set
import Data.Word (Word64)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Planar.Exact (ExactPoint, exactPoint)
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.Region
  ( ExactLoop, PlanarRegion, PolygonComponent, exactLoop, planarRegion, polygonComponent )
import Moonlight.Planar.Point (Point (..), QueryPoint)
import Moonlight.Planar.Types (Triangulation)
import Moonlight.Planar.Validation (validateTriangulation)

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

requireJust :: String -> Maybe value -> IO value
requireJust _ (Just value) = pure value
requireJust label Nothing = fail (label <> ": expected Just")

randomPoints :: Word64 -> Int -> [Point]
randomPoints seed count = take count (go seed Set.empty)
 where
  go :: Word64 -> Set.Set (Point) -> [Point]
  go state seen =
    let state1 = lcg state
        state2 = lcg state1
        x = unit state1 * 2 - 1
        y = unit state2 * 2 - 1
        point = Point x y
     in if Set.member point seen
          then go state2 seen
          else point : go state2 (Set.insert point seen)

  unit :: Word64 -> Double
  unit value = fromIntegral (value `mod` 9_007_199_254_740_881) / 9_007_199_254_740_881

  lcg :: Word64 -> Word64
  lcg value = value * 6_364_136_223_846_793_005 + 1_442_695_040_888_963_407

rectangleRegion :: Integer -> Integer -> Integer -> Integer -> IO PlanarRegion
rectangleRegion minimumX minimumY maximumX maximumY =
  rectangleComponent minimumX minimumY maximumX maximumY
    >>= requireRight "rectangle region" . planarRegion . (: [])

polygonComponentOf :: [(Integer, Integer)] -> IO PolygonComponent
polygonComponentOf coordinates =
  case map (uncurry integerPoint) coordinates of
    firstPoint : secondPoint : thirdPoint : remaining -> do
      loop <-
        requireRight
          "polygon loop"
          (exactLoop (firstPoint :| (secondPoint : thirdPoint : remaining)))
      requireRight "polygon component" (polygonComponent loop [])
    _ -> fail "polygon fixture requires at least three points"

polygonRegion :: [(Integer, Integer)] -> IO PlanarRegion
polygonRegion coordinates =
  polygonComponentOf coordinates
    >>= requireRight "polygon region" . planarRegion . (: [])

annulusRegion
  :: (Integer, Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> IO PlanarRegion
annulusRegion
  (outerMinX, outerMinY, outerMaxX, outerMaxY)
  (holeMinX, holeMinY, holeMaxX, holeMaxY) = do
    outer <- rectangleLoop outerMinX outerMinY outerMaxX outerMaxY
    hole <-
      requireRight
        "annulus hole loop"
        ( exactLoop
            ( integerPoint holeMinX holeMinY
                :| [ integerPoint holeMinX holeMaxY
                   , integerPoint holeMaxX holeMaxY
                   , integerPoint holeMaxX holeMinY
                   ]
            )
        )
    component <- requireRight "annulus component" (polygonComponent outer [hole])
    requireRight "annulus region" (planarRegion [component])
