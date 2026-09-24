module Main (main) where

import Moonlight.Planar.Example.AlphaBoundary
import Moonlight.Planar.Example.ConstrainedRegion
import Moonlight.Planar.Example.PlanarRegion
import Moonlight.Planar.Example.SetAlgebra
import Moonlight.Planar.Handles.Dynamic ()
import Moonlight.Planar.Session ()
import Moonlight.Planar.Voronoi ()
import System.Exit (die)

main :: IO ()
main = do
  assertRight "set algebra" (6, 2) siteCounts
  assertRight "constrained region" (11, 8) constrainedRegionCounts
  assertRight "alpha boundary" [BoundaryShape 4 0] alphaBoundaryShapes
  assertRight
    "planar region"
    expectedPlanarSummary
    planarExample
  putStrLn "examples: ok"

expectedPlanarSummary :: PlanarExampleSummary
expectedPlanarSummary =
  PlanarExampleSummary
    { booleanValuations =
        [ CellValuationSummary 1 (ExactFractionSummary 20 1)
        , CellValuationSummary 1 (ExactFractionSummary 4 1)
        , CellValuationSummary 1 (ExactFractionSummary 12 1)
        ]
    , morphologyAreas =
        fmap
          (`ExactFractionSummary` 1)
          [48, 36, 4, 16, 16]
    }

assertRight
  :: (Eq value, Show value, Show obstruction)
  => String
  -> value
  -> Either obstruction value
  -> IO ()
assertRight label expected result =
  case result of
    Left obstruction ->
      die (label <> " failed with " <> show obstruction)
    Right actual
      | actual == expected -> pure ()
      | otherwise ->
          die
            ( label
                <> " expected "
                <> show expected
                <> ", got "
                <> show actual
            )
