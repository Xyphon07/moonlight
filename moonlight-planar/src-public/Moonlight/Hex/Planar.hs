{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact planar interpretation of native hexagonal cells and regions.
module Moonlight.Hex.Planar
  ( HexPlanarObstruction (..)
  , hexCellExactLoop
  , hexRegionPlanarRegion
  , hexRegionByCenterInPlanarRegion
  , hexRegionFullyCoveredByPlanarRegion
  , hexRegionIntersectingPlanarRegion
  ) where

import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Foldable (foldlM)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Moonlight.Hex.Coordinate
  ( HexCoord (..)
  , HexDirection
  , allHexDirections
  , hexStepCoord
  )
import Moonlight.Hex.Element
  ( HexVertex
  , hexBoundaryFrom
  , hexBoundaryTo
  , hexCellBoundarySide
  , hexCellVertices
  , hexVertexCoordinates
  )
import Moonlight.Hex.Region
  ( HexLayout
  , HexRegion
  , HexRowSpan
  , foldHexRegionCoords
  , hexLayoutCellCount
  , hexLayoutOrigin
  , hexLayoutWidth
  , hexRegionGenerate
  , hexRegionGenerateRowSpans
  , hexRegionMember
  , hexRowSpan
  )
import Moonlight.Planar.Exact
  ( ExactPoint
  , ExactRational
  , SegmentRelation (SegmentsDisjoint)
  , exactOrient2d
  , exactPoint
  , exactPointCoordinates
  , exactRationalDenominator
  , exactRationalNumerator
  , exactSegmentRelation
  )
import Moonlight.Planar.Region
  ( ExactLoop
  , PlanarRegion
  , RegionPointLocation (RegionExterior)
  , RegionValidationError
  , emptyPlanarRegion
  , exactLoop
  , exactLoopPoints
  , planarRegion
  , planarRegionComponents
  , polygonComponent
  , polygonHoleLoops
  , polygonOuterLoop
  , regionPointLocation
  )

-- | Every typed obstruction in boundary descent and exact publication.
data HexPlanarObstruction
  = HexPlanarRegionInvalid !RegionValidationError
  | HexPlanarBoundaryOpen !HexVertex
  | HexPlanarBoundaryVectorUnexpected !HexVertex !HexVertex
  | HexPlanarBoundaryDegenerate !(NonEmpty HexVertex)
  | HexPlanarHoleUnowned !ExactLoop
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

hexCellExactLoop :: HexCoord -> Either HexPlanarObstruction ExactLoop
hexCellExactLoop =
  first HexPlanarRegionInvalid
    . exactLoop
    . fmap exactVertexPoint
    . hexCellVertices

-- | Trace the exterior sides of the selected native section, classify the
-- resulting exact loops by winding, and publish one canonical planar region.
-- Interior cell sides never enter the boundary graph.
hexRegionPlanarRegion :: HexRegion -> Either HexPlanarObstruction PlanarRegion
hexRegionPlanarRegion region
  | Set.null boundary = Right emptyPlanarRegion
  | otherwise = do
      vertexLoops <- traceBoundaryLoops boundary
      exactLoops <- traverse admitBoundaryLoop vertexLoops
      publishClassifiedLoops exactLoops
 where
  boundary = regionBoundaryEdges region

  admitBoundaryLoop
    :: NonEmpty HexVertex
    -> Either HexPlanarObstruction (Integer, ExactLoop)
  admitBoundaryLoop vertices = do
    let area = signedDoubleArea vertices
    if area == 0
      then Left (HexPlanarBoundaryDegenerate vertices)
      else
        fmap
          (\loop -> (area, loop))
          (first HexPlanarRegionInvalid (exactLoop (fmap exactVertexPoint vertices)))

-- | Select cells whose exact centre belongs to the closed planar region.
hexRegionByCenterInPlanarRegion :: HexLayout -> PlanarRegion -> HexRegion
hexRegionByCenterInPlanarRegion layout region
  | hexLayoutCellCount layout == 1 =
      hexRegionGenerate layout ((/= RegionExterior) . regionPointLocation region . exactCellCentre)
  | otherwise =
      let segments = axialBoundarySegments (planarBoundary region)
       in hexRegionGenerateRowSpans layout
            (mapMaybe (uncurry hexRowSpan) . centreRowRanges segments)

-- | Select cells whose entire closed hexagon is covered by the closed planar
-- region. A closed convex cell lies in the region exactly when its centre
-- does and no boundary segment of the region meets the cell's interior: the
-- interior is connected, so once the boundary is known to miss it, it lies
-- wholly on the centre's side. Boundary-only contact is therefore coverage,
-- and no overlay or gap publication is involved.
hexRegionFullyCoveredByPlanarRegion :: HexLayout -> PlanarRegion -> HexRegion
hexRegionFullyCoveredByPlanarRegion layout region
  | hexLayoutCellCount layout == 1 =
      let boundary = planarBoundary region
       in hexRegionGenerate layout (cellCoveredByRegion region (boundarySegments boundary))
  | otherwise = restrictPlanarBoundary layout region (cellCoveredByRegion region . boundarySegments)

-- | Select cells whose closed hexagon has any point in common with the closed
-- planar region. The region boundary is prepared once as an ephemeral view;
-- the admitted 'PlanarRegion' remains the sole geometry authority.
hexRegionIntersectingPlanarRegion :: HexLayout -> PlanarRegion -> HexRegion
hexRegionIntersectingPlanarRegion layout region =
  restrictPlanarBoundary layout region (cellIntersectsRegion region)

-- | Descent from whole centre runs away from the boundary, with the original
-- detailed classifier retained on a conservative exact boundary supercover.
-- A singleton context has no shared run work: it uses the original direct
-- observation rather than constructing an event plan around one predicate.
restrictPlanarBoundary
  :: HexLayout
  -> PlanarRegion
  -> (PlanarBoundary -> HexCoord -> Bool)
  -> HexRegion
restrictPlanarBoundary layout region classify
  | hexLayoutCellCount layout == 1 = hexRegionGenerate layout (classify boundary)
  | otherwise = hexRegionGenerateRowSpans layout selectedSpans
 where
  boundary = planarBoundary region
  segments = axialBoundarySegments boundary
  firstQ = toInteger (hexQ (hexLayoutOrigin layout))
  finalQ = firstQ + toInteger (hexLayoutWidth layout) - 1

  selectedSpans :: Int -> [HexRowSpan]
  selectedSpans row =
    let boundaryRanges = boundaryRowRanges layout segments row
        interiorRanges =
          if boundaryRanges == [(firstQ, finalQ)]
            then []
            else subtractRowRanges (centreRowRanges segments row) boundaryRanges
        selectedBoundary =
          [ (q, q)
          | (firstBoundaryQ, finalBoundaryQ) <- boundaryRanges
          , q <- [firstBoundaryQ .. finalBoundaryQ]
          , classify boundary (HexCoord (fromInteger q) row)
          ]
     in mapMaybe (uncurry hexRowSpan) (mergeRowRanges (interiorRanges <> selectedBoundary))
{-# INLINE restrictPlanarBoundary #-}

-- Query-local scalar observations of the existing exact boundary. The
-- positive-determinant affine coordinates are (u,v)=(x,3*y-x), placing axial
-- centres at (3*q,6*r). They author no second polygon or geometry inventory.
type AxialBoundarySegment = ((Rational, Rational), (Rational, Rational))
type RowRange = (Integer, Integer)

axialBoundarySegments :: PlanarBoundary -> [AxialBoundarySegment]
axialBoundarySegments = fmap axialSegment . boundarySegments
 where
  axialSegment :: (Extent, ExactPoint, ExactPoint) -> AxialBoundarySegment
  axialSegment (_, from, to) = (axialPoint from, axialPoint to)
  axialPoint :: ExactPoint -> (Rational, Rational)
  axialPoint point =
    let (x, y) = exactPointCoordinates point
        rational :: ExactRational -> Rational
        rational value = exactRationalNumerator value % exactRationalDenominator value
        u = rational x
     in (u, 3 * rational y - u)

-- | Pair half-open crossings of admitted closed loops, then separately add
-- closed boundary intersections and collinear spans. The former has even
-- parity; the latter preserves tangencies and vertices on integer centres.
centreRowRanges :: [AxialBoundarySegment] -> Int -> [RowRange]
centreRowRanges segments row =
  mergeRowRanges (mapMaybe quantizeRowRange (interiorSpans <> closedBoundarySpans))
 where
  rowV = fromInteger (6 * toInteger row)
  crossings = List.sort
    [ crossing
    | segment@((_, fromV), (_, toV)) <- segments
    , min fromV toV <= rowV && rowV < max fromV toV
    , Just crossing <- [crossingAt rowV segment]
    ]
  interiorSpans =
    [ pair
    | (index, pair) <- zip [0 :: Int ..] (zip crossings (drop 1 crossings))
    , even index
    ]
  closedBoundarySpans =
    [ boundarySpan
    | segment@((fromU, fromV), (toU, toV)) <- segments
    , min fromV toV <= rowV && rowV <= max fromV toV
    , boundarySpan <-
        if fromV == toV
          then [(min fromU toU, max fromU toU)]
          else maybe [] (\crossing -> [(crossing, crossing)]) (crossingAt rowV segment)
    ]

-- A horizontal support has no transverse crossing. All arithmetic remains
-- exact; neither row events nor their order are projected through binary64.
crossingAt :: Rational -> AxialBoundarySegment -> Maybe Rational
crossingAt rowV ((fromU, fromV), (toU, toV))
  | fromV == toV = Nothing
  | otherwise = Just (fromU + (rowV - fromV) * (toU - fromU) / (toV - fromV))

quantizeRowRange :: (Rational, Rational) -> Maybe RowRange
quantizeRowRange (firstU, finalU) =
  let firstQ = ceiling (firstU / 3)
      finalQ = floor (finalU / 3)
   in if firstQ <= finalQ then Just (firstQ, finalQ) else Nothing

-- | Every point of a hexagon lies in u +/- 2 and v +/- 4 about its centre.
-- Clip a segment to the CLOSED v strip before expanding its u interval; this
-- avoids the rectangular-area over-enumeration of an untrimmed diagonal.
-- Endpoints and loops contained inside a cell are retained by construction.
boundaryRowRanges :: HexLayout -> [AxialBoundarySegment] -> Int -> [RowRange]
boundaryRowRanges layout segments row =
  mergeRowRanges (mapMaybe (clipToLayout <=< quantizeRowRange <=< stripSpan) segments)
 where
  rowV = fromInteger (6 * toInteger row)
  lowerV = rowV - 4
  upperV = rowV + 4
  minimumQ = toInteger (hexQ (hexLayoutOrigin layout))
  maximumQ = minimumQ + toInteger (hexLayoutWidth layout) - 1

  stripSpan :: AxialBoundarySegment -> Maybe (Rational, Rational)
  stripSpan ((fromU, fromV), (toU, toV))
    | max fromV toV < lowerV || min fromV toV > upperV = Nothing
    | fromV == toV = Just (min fromU toU - 2, max fromU toU + 2)
    | otherwise =
        let firstParameter = (lowerV - fromV) / (toV - fromV)
            finalParameter = (upperV - fromV) / (toV - fromV)
            lowerParameter = max 0 (min firstParameter finalParameter)
            upperParameter = min 1 (max firstParameter finalParameter)
            firstU = fromU + lowerParameter * (toU - fromU)
            finalU = fromU + upperParameter * (toU - fromU)
         in Just (min firstU finalU - 2, max firstU finalU + 2)

  clipToLayout :: RowRange -> Maybe RowRange
  clipToLayout (firstQ, finalQ) =
    let clippedFirst = max minimumQ firstQ
        clippedFinal = min maximumQ finalQ
     in if clippedFirst <= clippedFinal then Just (clippedFirst, clippedFinal) else Nothing

mergeRowRanges :: [RowRange] -> [RowRange]
mergeRowRanges = reverse . foldl' merge [] . List.sortOn fst
 where
  merge :: [RowRange] -> RowRange -> [RowRange]
  merge [] range = [range]
  merge ((previousFirst, previousFinal) : remaining) range@(firstQ, finalQ)
    | firstQ <= previousFinal + 1 = (previousFirst, max previousFinal finalQ) : remaining
    | otherwise = range : (previousFirst, previousFinal) : remaining

subtractRowRanges :: [RowRange] -> [RowRange] -> [RowRange]
subtractRowRanges = foldl' (\remaining cut -> concatMap (subtractRange cut) remaining)
 where
  subtractRange :: RowRange -> RowRange -> [RowRange]
  subtractRange (cutFirst, cutFinal) range@(firstQ, finalQ)
    | cutFinal < firstQ || finalQ < cutFirst = [range]
    | otherwise =
        [(firstQ, cutFirst - 1) | firstQ < cutFirst]
          <> [(cutFinal + 1, finalQ) | cutFinal < finalQ]

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

cellCoveredByRegion :: PlanarRegion -> [(Extent, ExactPoint, ExactPoint)] -> HexCoord -> Bool
cellCoveredByRegion region segments coordinate =
  regionPointLocation region (exactCellCentre coordinate) /= RegionExterior
    && not
      ( any
          (\(extent, from, to) -> extentsMeet cellExtent extent && segmentMeetsConvexInterior cellPoints (from, to))
          segments
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

-- One oriented edge with selected area on its left.
data BoundaryEdge = BoundaryEdge !HexVertex !HexVertex
  deriving stock (Eq, Ord, Show)

regionBoundaryEdges :: HexRegion -> Set BoundaryEdge
regionBoundaryEdges region =
  foldHexRegionCoords addCell Set.empty region
 where
  addCell :: Set BoundaryEdge -> HexCoord -> Set BoundaryEdge
  addCell edges coordinate =
    foldl' (addDirection coordinate) edges allHexDirections

  addDirection
    :: HexCoord
    -> Set BoundaryEdge
    -> HexDirection
    -> Set BoundaryEdge
  addDirection coordinate edges direction =
    case hexStepCoord coordinate direction of
      Just neighbour | hexRegionMember neighbour region -> edges
      _ ->
        let side = hexCellBoundarySide coordinate direction
         in Set.insert
              (BoundaryEdge (hexBoundaryFrom side) (hexBoundaryTo side))
              edges

traceBoundaryLoops :: Set BoundaryEdge -> Either HexPlanarObstruction [NonEmpty HexVertex]
traceBoundaryLoops edges = descend edges []
 where
  outgoing =
    Set.foldl'
      (\index edge@(BoundaryEdge from _) -> Map.insertWith (<>) from [edge] index)
      Map.empty
      edges

  descend
    :: Set BoundaryEdge
    -> [NonEmpty HexVertex]
    -> Either HexPlanarObstruction [NonEmpty HexVertex]
  descend remaining reversedLoops =
    case Set.lookupMin remaining of
      Nothing -> Right (reverse reversedLoops)
      Just firstEdge@(BoundaryEdge firstVertex _) -> do
        (vertices, unconsumed) <- traceCycle outgoing firstVertex firstEdge (Set.delete firstEdge remaining)
        descend unconsumed (vertices : reversedLoops)

traceCycle
  :: Map HexVertex [BoundaryEdge]
  -> HexVertex
  -> BoundaryEdge
  -> Set BoundaryEdge
  -> Either HexPlanarObstruction (NonEmpty HexVertex, Set BoundaryEdge)
traceCycle outgoing firstVertex firstEdge remaining =
  walk firstEdge (firstVertex :| []) remaining
 where
  walk
    :: BoundaryEdge
    -> NonEmpty HexVertex
    -> Set BoundaryEdge
    -> Either HexPlanarObstruction (NonEmpty HexVertex, Set BoundaryEdge)
  walk current reversedVertices unconsumed =
    let BoundaryEdge _ endpoint = current
     in if endpoint == firstVertex
          then Right (NonEmpty.reverse reversedVertices, unconsumed)
          else do
            next <- selectNextBoundaryEdge outgoing unconsumed current endpoint
            walk next (endpoint NonEmpty.<| reversedVertices) (Set.delete next unconsumed)

selectNextBoundaryEdge
  :: Map HexVertex [BoundaryEdge]
  -> Set BoundaryEdge
  -> BoundaryEdge
  -> HexVertex
  -> Either HexPlanarObstruction BoundaryEdge
selectNextBoundaryEdge outgoing remaining current endpoint =
  case NonEmpty.nonEmpty (filter (`Set.member` remaining) (Map.findWithDefault [] endpoint outgoing)) of
    Nothing -> Left (HexPlanarBoundaryOpen endpoint)
    Just candidates -> do
      currentRank <- boundaryEdgeRank current
      ranked <- traverse (rankCandidate currentRank) candidates
      pure (snd (leastRanked ranked))
 where
  rankCandidate
    :: Int
    -> BoundaryEdge
    -> Either HexPlanarObstruction (Int, BoundaryEdge)
  rankCandidate currentRank candidate = do
    candidateRank <- boundaryEdgeRank candidate
    pure ((candidateRank - currentRank) `mod` 6, candidate)

  leastRanked :: NonEmpty (Int, BoundaryEdge) -> (Int, BoundaryEdge)
  leastRanked (firstRanked :| rest) = foldl' choose firstRanked rest

  choose :: (Int, BoundaryEdge) -> (Int, BoundaryEdge) -> (Int, BoundaryEdge)
  choose left right = if fst left <= fst right then left else right

boundaryEdgeRank :: BoundaryEdge -> Either HexPlanarObstruction Int
boundaryEdgeRank (BoundaryEdge from to) =
  let (fromX, fromY) = hexVertexCoordinates from
      (toX, toY) = hexVertexCoordinates to
   in case (toX - fromX, toY - fromY) of
        (2, 0) -> Right 0
        (1, 1) -> Right 1
        (-1, 1) -> Right 2
        (-2, 0) -> Right 3
        (-1, -1) -> Right 4
        (1, -1) -> Right 5
        _ -> Left (HexPlanarBoundaryVectorUnexpected from to)

publishClassifiedLoops :: [(Integer, ExactLoop)] -> Either HexPlanarObstruction PlanarRegion
publishClassifiedLoops classified = do
  let outerLoops = [(area, loop) | (area, loop) <- classified, area > 0]
      holeLoops = [loop | (area, loop) <- classified, area < 0]
  holesByOuter <- foldlM (assignHole outerLoops) Map.empty holeLoops
  components <-
    traverse
      (\(_, outer) ->
          first HexPlanarRegionInvalid
            (polygonComponent outer (Map.findWithDefault [] outer holesByOuter))
      )
      outerLoops
  first HexPlanarRegionInvalid (planarRegion components)

assignHole
  :: [(Integer, ExactLoop)]
  -> Map ExactLoop [ExactLoop]
  -> ExactLoop
  -> Either HexPlanarObstruction (Map ExactLoop [ExactLoop])
assignHole outerLoops assignments hole =
  case filter (\(_, outer) -> isRight (polygonComponent outer [hole])) outerLoops of
    [] -> Left (HexPlanarHoleUnowned hole)
    firstOwner : remainingOwners ->
      let (_, owner) = foldl' smallerOuter firstOwner remainingOwners
       in Right (Map.insertWith (<>) owner [hole] assignments)
 where
  smallerOuter :: (Integer, ExactLoop) -> (Integer, ExactLoop) -> (Integer, ExactLoop)
  smallerOuter left right = if fst left <= fst right then left else right

signedDoubleArea :: NonEmpty HexVertex -> Integer
signedDoubleArea vertices@(firstVertex :| rest) =
  sum
    [ leftX * rightY - leftY * rightX
    | (left, right) <- zip (NonEmpty.toList vertices) (rest <> [firstVertex])
    , let (leftX, leftY) = hexVertexCoordinates left
    , let (rightX, rightY) = hexVertexCoordinates right
    ]

exactVertexPoint :: HexVertex -> ExactPoint
exactVertexPoint vertex =
  let (x, y) = hexVertexCoordinates vertex
   in exactPoint (fromInteger x) (fromInteger y)
