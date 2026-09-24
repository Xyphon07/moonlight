module Moonlight.Planar.Example.PlanarRegion
  ( ExactFractionSummary (..)
  , CellValuationSummary (..)
  , PlanarExampleError (..)
  , PlanarExampleSummary (..)
  , planarExample
  ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Moonlight.Planar.CellSet (ExactCellSet)
import Moonlight.Planar.Convex (ConvexError (..), convexPolygon)
import Moonlight.Planar.Exact (exactPoint)
import Moonlight.Planar.Internal.ExactRational (exactRationalDenominator, exactRationalNumerator)
import Moonlight.Planar.Minkowski (MinkowskiError (..), closeWith, erodeBy, minkowskiSum, openWith, polygonOffset, structuringElement)
import Moonlight.Planar.Overlay (OverlayError (..), OverlaySelectionError (..), overlayClosedIntersection, overlayClosedUnion, overlayLayers, overlayRegularizedDifference)
import Moonlight.Planar.Region (ExactLoop, PlanarLayer, PlanarRegion, RegionValidationError (..), exactLoop, exactLoopPoints, planarLayer, planarRegion, polygonComponent)
import Moonlight.Planar.Valuation (PlanarValuations, ValuationError (..), cellValuations, eulerCharacteristicValue, exactAreaValue, regionValuations, valuationArea, valuationEuler)

data ExactFractionSummary = ExactFractionSummary
  { fractionNumerator :: !Integer
  , fractionDenominator :: !Integer
  }
  deriving stock (Eq, Show)

data CellValuationSummary = CellValuationSummary
  { cellEulerCharacteristic :: !Int
  , cellExactArea :: !ExactFractionSummary
  }
  deriving stock (Eq, Show)

data PlanarExampleSummary = PlanarExampleSummary
  { booleanValuations :: ![CellValuationSummary]
  , morphologyAreas :: ![ExactFractionSummary]
  }
  deriving stock (Eq, Show)

data PlanarExampleError
  = PlanarRegionFailed !RegionValidationError
  | PlanarOverlayFailed !OverlayError
  | PlanarSelectionFailed !OverlaySelectionError
  | PlanarValuationFailed !ValuationError
  | PlanarConvexFailed !ConvexError
  | PlanarMinkowskiFailed !MinkowskiError
  deriving stock (Eq, Show)

-- | Compute exact Boolean valuations and morphology through the public facade.
planarExample :: Either PlanarExampleError PlanarExampleSummary
planarExample = do
  left <- rectangleRegion 0 0 4 4
  right <- rectangleRegion 2 1 6 3
  leftLayer <- insideLayer left
  rightLayer <- insideLayer right
  overlay <- first PlanarOverlayFailed (overlayLayers leftLayer rightLayer)
  unionCells <- first PlanarSelectionFailed (overlayClosedUnion id id overlay)
  intersectionCells <-
    first PlanarSelectionFailed (overlayClosedIntersection id id overlay)
  differenceCells <-
    first PlanarSelectionFailed (overlayRegularizedDifference id id overlay)
  cellSummaries <-
    traverse
      cellValuationSummary
      [unionCells, intersectionCells, differenceCells]

  kernelLoop <- rectangleLoop (-1) (-1) 1 1
  kernelPolygon <-
    first PlanarConvexFailed (convexPolygon (exactLoopPoints kernelLoop))
  kernel <- first PlanarMinkowskiFailed (structuringElement kernelPolygon)
  (sumRegion, _) <- first PlanarMinkowskiFailed (minkowskiSum left right)
  (offsetRegion, _) <- first PlanarMinkowskiFailed (polygonOffset kernel left)
  (insetRegion, _) <- first PlanarMinkowskiFailed (erodeBy kernel left)
  (openedRegion, _) <- first PlanarMinkowskiFailed (openWith kernel left)
  (closedRegion, _) <- first PlanarMinkowskiFailed (closeWith kernel left)
  areaSummaries <-
    traverse
      regionAreaSummary
      [sumRegion, offsetRegion, insetRegion, openedRegion, closedRegion]

  pure
    PlanarExampleSummary
      { booleanValuations = cellSummaries
      , morphologyAreas = areaSummaries
      }

rectangleLoop
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> Either PlanarExampleError ExactLoop
rectangleLoop minimumX minimumY maximumX maximumY =
  first PlanarRegionFailed
    ( exactLoop
        ( exactPoint (fromInteger minimumX) (fromInteger minimumY)
            :| [ exactPoint (fromInteger maximumX) (fromInteger minimumY)
               , exactPoint (fromInteger maximumX) (fromInteger maximumY)
               , exactPoint (fromInteger minimumX) (fromInteger maximumY)
               ]
        )
    )

rectangleRegion
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> Either PlanarExampleError PlanarRegion
rectangleRegion minimumX minimumY maximumX maximumY = do
  outer <- rectangleLoop minimumX minimumY maximumX maximumY
  component <- first PlanarRegionFailed (polygonComponent outer [])
  first PlanarRegionFailed (planarRegion [component])

insideLayer :: PlanarRegion -> Either PlanarExampleError (PlanarLayer Bool)
insideLayer =
  first PlanarRegionFailed
    . planarLayer False
    . Map.singleton True

cellValuationSummary
  :: ExactCellSet
  -> Either PlanarExampleError CellValuationSummary
cellValuationSummary cells = do
  valuations <- first PlanarValuationFailed (cellValuations cells)
  pure
    CellValuationSummary
      { cellEulerCharacteristic =
          eulerCharacteristicValue (valuationEuler valuations)
      , cellExactArea = exactAreaSummary valuations
      }

regionAreaSummary
  :: PlanarRegion
  -> Either PlanarExampleError ExactFractionSummary
regionAreaSummary region =
  exactAreaSummary
    <$> first PlanarValuationFailed (regionValuations region)

exactAreaSummary :: PlanarValuations -> ExactFractionSummary
exactAreaSummary valuations =
  let area = exactAreaValue (valuationArea valuations)
   in ExactFractionSummary
        { fractionNumerator = exactRationalNumerator area
        , fractionDenominator = exactRationalDenominator area
        }
