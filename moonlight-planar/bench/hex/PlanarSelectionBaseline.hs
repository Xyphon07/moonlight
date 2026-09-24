{-# OPTIONS_GHC -O1 -fexpose-all-unfoldings #-}

-- | Frozen test oracle from commit 39029d4b84. Production never imports this
-- module. Both schedules receive the same fully forced admitted input.
module PlanarSelectionBaseline
  ( hexRegionByCenterInPlanarRegion
  , hexRegionFullyCoveredByPlanarRegion
  , hexRegionIntersectingPlanarRegion
  ) where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Hex.Coordinate (HexCoord (..))
import Moonlight.Hex.Element (HexVertex, hexCellVertices, hexVertexCoordinates)
import Moonlight.Hex.Region (HexLayout, HexRegion, hexRegionGenerate)
import Moonlight.Planar.Exact
  ( ExactPoint
  , ExactRational
  , SegmentRelation (SegmentsDisjoint)
  , exactOrient2d
  , exactPoint
  , exactPointCoordinates
  , exactSegmentRelation
  )
import Moonlight.Planar.Region
  ( PlanarRegion
  , RegionPointLocation (RegionExterior)
  , exactLoopPoints
  , planarRegionComponents
  , polygonHoleLoops
  , polygonOuterLoop
  , regionPointLocation
  )

hexRegionByCenterInPlanarRegion :: HexLayout -> PlanarRegion -> HexRegion
hexRegionByCenterInPlanarRegion layout region =
  hexRegionGenerate layout ((/= RegionExterior) . regionPointLocation region . exactCellCentre)

-- | Select cells whose entire closed hexagon is covered by the closed planar
-- region. A closed convex cell lies in the region exactly when its centre
-- does and no boundary segment of the region meets the cell's interior: the
-- interior is connected, so once the boundary is known to miss it, it lies
-- wholly on the centre's side. Boundary-only contact is therefore coverage,
-- and no overlay or gap publication is involved.
hexRegionFullyCoveredByPlanarRegion :: HexLayout -> PlanarRegion -> HexRegion
hexRegionFullyCoveredByPlanarRegion layout region =
  let boundary = planarBoundary region
   in hexRegionGenerate layout (cellCoveredByRegion region boundary)

-- | Select cells whose closed hexagon has any point in common with the closed
-- planar region. The region boundary is prepared once as an ephemeral view;
-- the admitted 'PlanarRegion' remains the sole geometry authority.
hexRegionIntersectingPlanarRegion :: HexLayout -> PlanarRegion -> HexRegion
hexRegionIntersectingPlanarRegion layout region =
  let boundary = planarBoundary region
   in hexRegionGenerate layout (cellIntersectsRegion region boundary)

exactCellCentre :: HexCoord -> ExactPoint
exactCellCentre (HexCoord q r) =
  exactPoint
    (fromInteger (3 * toInteger q))
    (fromInteger (2 * toInteger r + toInteger q))

-- | Closed axis-aligned extent, the cheap separation test in front of every
-- exact predicate: two closed sets with disjoint extents are disjoint.
data Extent = Extent !ExactRational !ExactRational !ExactRational !ExactRational

pointExtent :: ExactPoint -> Extent
pointExtent point =
  let (x, y) = exactPointCoordinates point
   in Extent x x y y

pointsExtent :: NonEmpty ExactPoint -> Extent
pointsExtent = foldr1 joinExtent . fmap pointExtent
 where
  joinExtent (Extent leftLow leftHigh leftBottom leftTop) (Extent rightLow rightHigh rightBottom rightTop) =
    Extent (min leftLow rightLow) (max leftHigh rightHigh) (min leftBottom rightBottom) (max leftTop rightTop)

extentsMeet :: Extent -> Extent -> Bool
extentsMeet (Extent leftLow leftHigh leftBottom leftTop) (Extent rightLow rightHigh rightBottom rightTop) =
  leftLow <= rightHigh && rightLow <= leftHigh && leftBottom <= rightTop && rightBottom <= leftTop

-- | The region boundary as an ephemeral view: every boundary vertex and every
-- boundary segment, each with its extent.
data PlanarBoundary = PlanarBoundary
  { boundaryPoints :: ![(Extent, ExactPoint)]
  , boundarySegments :: ![(Extent, ExactPoint, ExactPoint)]
  }

planarBoundary :: PlanarRegion -> PlanarBoundary
planarBoundary region =
  let loops = planarRegionComponents region >>= componentLoops
      points = loops >>= NonEmpty.toList . exactLoopPoints
   in PlanarBoundary
        { boundaryPoints = fmap (\point -> (pointExtent point, point)) points
        , boundarySegments =
            [ (pointsExtent (from :| [to]), from, to)
            | loop <- loops
            , (from, to) <- exactPointCycleSegments (NonEmpty.toList (exactLoopPoints loop))
            ]
        }
 where
  componentLoops component =
    polygonOuterLoop component : polygonHoleLoops component

cellCoveredByRegion :: PlanarRegion -> PlanarBoundary -> HexCoord -> Bool
cellCoveredByRegion region boundary coordinate =
  regionPointLocation region (exactCellCentre coordinate) /= RegionExterior
    && not
      ( any
          (\(extent, from, to) -> extentsMeet cellExtent extent && segmentMeetsConvexInterior cellPoints (from, to))
          (boundarySegments boundary)
      )
 where
  cellVertices = fmap exactVertexPoint (hexCellVertices coordinate)
  cellPoints = NonEmpty.toList cellVertices
  cellExtent = pointsExtent cellVertices

-- | Whether a segment has a point strictly inside the convex polygon whose
-- vertices are listed counter-clockwise. A segment on a side's line never
-- enters; otherwise it enters when an endpoint is strictly inside, when it
-- properly crosses a side, or when it touches the boundary at two distinct
-- points, since the chord between them is interior to a convex polygon.
segmentMeetsConvexInterior :: [ExactPoint] -> (ExactPoint, ExactPoint) -> Bool
segmentMeetsConvexInterior vertices (from, to)
  | any onSideLine sides = False
  | strictlyInside fromSides || strictlyInside toSides = True
  | any properlyCrosses sides = True
  | otherwise = length (List.nub contacts) >= 2
 where
  edges = exactPointCycleSegments vertices
  fromSides = fmap (\(a, b) -> exactOrient2d a b from) edges
  toSides = fmap (\(a, b) -> exactOrient2d a b to) edges
  vertexSides = fmap (exactOrient2d from to) vertices
  sides = zip (zip fromSides toSides) (zip vertexSides (drop 1 (cycle vertexSides)))
  onSideLine :: ((Ordering, Ordering), (Ordering, Ordering)) -> Bool
  onSideLine ((fromSide, toSide), _) = fromSide == EQ && toSide == EQ
  strictlyInside :: [Ordering] -> Bool
  strictlyInside = all (== GT)
  onBoundary :: [Ordering] -> Bool
  onBoundary orientations = all (/= LT) orientations && elem EQ orientations
  properlyCrosses :: ((Ordering, Ordering), (Ordering, Ordering)) -> Bool
  properlyCrosses ((fromSide, toSide), (startSide, endSide)) =
    opposite fromSide toSide && opposite startSide endSide
  opposite :: Ordering -> Ordering -> Bool
  opposite left right = (left == GT && right == LT) || (left == LT && right == GT)
  contacts =
    [point | (point, orientations) <- [(from, fromSides), (to, toSides)], onBoundary orientations]
      <> [vertex | (vertex, side) <- zip vertices vertexSides, side == EQ, withinSpan vertex]
  withinSpan :: ExactPoint -> Bool
  withinSpan vertex = extentsMeet (pointExtent vertex) (pointsExtent (from :| [to]))

cellIntersectsRegion :: PlanarRegion -> PlanarBoundary -> HexCoord -> Bool
cellIntersectsRegion region boundary coordinate =
  any ((/= RegionExterior) . regionPointLocation region) cellPoints
    || any
      (\(extent, point) -> extentsMeet cellExtent extent && pointInClosedHex cellSegments point)
      (boundaryPoints boundary)
    || any
      (\(extent, regionFrom, regionTo) ->
         extentsMeet cellExtent extent
           && any
             (\(cellFrom, cellTo) ->
                exactSegmentRelation cellFrom cellTo regionFrom regionTo /= SegmentsDisjoint)
             cellSegments)
      (boundarySegments boundary)
 where
  cellVertices = fmap exactVertexPoint (hexCellVertices coordinate)
  cellPoints = NonEmpty.toList cellVertices
  cellSegments = exactPointCycleSegments cellPoints
  cellExtent = pointsExtent cellVertices

pointInClosedHex :: [(ExactPoint, ExactPoint)] -> ExactPoint -> Bool
pointInClosedHex cellSegments query =
  all (\(from, to) -> exactOrient2d from to query /= LT) cellSegments

exactPointCycleSegments :: [ExactPoint] -> [(ExactPoint, ExactPoint)]
exactPointCycleSegments points =
  case points of
    [] -> []
    firstPoint : remaining -> zip points (remaining <> [firstPoint])

exactVertexPoint :: HexVertex -> ExactPoint
exactVertexPoint vertex =
  let (x, y) = hexVertexCoordinates vertex
   in exactPoint (fromInteger x) (fromInteger y)
