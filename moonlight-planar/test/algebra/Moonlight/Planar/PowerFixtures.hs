-- | Admitted sites and exact observations shared by regular, alpha, and bounded-cell laws.
module Moonlight.Planar.PowerFixtures
  ( admittedWeight
  , admittedSite
  , admittedWeightedSite
  , squareDomain
  , dispositionTag
  , powerDistance
  ) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Planar.Convex ( ConvexPolygon, convexPolygon )
import Moonlight.Planar.Exact ( ExactPoint, exactPointCoordinates, exactPointFromPoint )
import Moonlight.Planar.Exact (ExactRational)
import Moonlight.Planar.PowerDiagram ( powerSite, powerSitePosition, powerWeight, powerWeightExact,
  PowerCellDisposition(..), PowerSite(powerSiteWeight), PowerWeight )
import Moonlight.Planar.Point (Point(..))
import Support ( integerPoint, requireRight )


squareDomain :: IO ConvexPolygon
squareDomain =
  requireRight
    "square clipping domain"
    (convexPolygon (integerPoint 0 0 :| [integerPoint 10 0, integerPoint 10 10, integerPoint 0 10]))

admittedWeight :: Double -> IO PowerWeight
admittedWeight value =
  requireRight
    "finite power weight"
    (powerWeight value)

admittedSite :: String -> Point -> PowerWeight -> IO (PowerSite String)
admittedSite label point weight = requireRight "admitted power site" (powerSite label point weight)

admittedWeightedSite
  :: (String, Point, Double)
  -> IO (PowerSite String)
admittedWeightedSite (label, point, weightValue) = do
  weight <- admittedWeight weightValue
  admittedSite label point weight

dispositionTag :: Maybe (PowerCellDisposition label) -> String
dispositionTag Nothing = "missing"
dispositionTag (Just (PublishedPowerCell _)) = "published"
dispositionTag (Just (LowerDimensionalPowerCell _)) = "lower-dimensional"
dispositionTag (Just EmptyPowerCell) = "empty"
dispositionTag (Just (CoincidentEquivalentTo _)) = "coincident-equivalent"
dispositionTag (Just (CoincidentDominatedBy _)) = "coincident-dominated"

-- | Independent exact squared-distance specification, not the regular-neighbour implementation.
powerDistance :: ExactPoint -> PowerSite label -> IO ExactRational
powerDistance point site = do
  exactSite <- requireRight "site exact position" (exactPointFromPoint (powerSitePosition site))
  let (x, y) = exactPointCoordinates point
      (siteX, siteY) = exactPointCoordinates exactSite
      deltaX = x - siteX
      deltaY = y - siteY
  pure (deltaX * deltaX + deltaY * deltaY - powerWeightExact (powerSiteWeight site))
