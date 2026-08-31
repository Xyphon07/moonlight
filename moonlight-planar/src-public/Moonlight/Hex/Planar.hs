{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact planar interpretation of native hexagonal cells and regions.
module Moonlight.Hex.Planar
  ( HexPlanarObstruction (..)
  , hexCellExactLoop
  , hexRegionPlanarRegion
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Moonlight.Hex.Coordinate
  ( HexCoord
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
  ( HexRegion
  , foldHexRegionCoords
  , hexRegionMember
  )
import Moonlight.Triangulation.Exact (ExactPoint, exactPoint)
import Moonlight.Triangulation.Region
  ( ExactLoop
  , PlanarRegion
  , RegionValidationError
  , emptyPlanarRegion
  , exactLoop
  , planarRegion
  , polygonComponent
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
