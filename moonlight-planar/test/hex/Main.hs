module Main (main) where

import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Hex.Coordinate
import Moonlight.Hex.Element
import Moonlight.Hex.Planar
import Moonlight.Hex.Region
import Moonlight.Hex.Serialization
import Moonlight.Triangulation.Region
  ( exactLoopPoints
  , planarRegionComponents
  , polygonHoleLoops
  , polygonOuterLoop
  )
main :: IO ()
main = do
  coordinateAndElementLaws
  packedRegionLaws
  restrictionLaws
  gluingLaws
  planarLaws
  serializationLaws
  wordBoundaryLaws
  putStrLn "hex-laws: coordinate element boolean restriction gluing planar serialization word-boundary=passed"

type Assertion = IO ()

coordinateAndElementLaws :: Assertion
coordinateAndElementLaws = do
  layout <- requireRight (hexLayout (HexCoord (-2) (-2)) 5 5)
  let coordinates = Vector.toList (layoutCoords layout)
  traverse_
    (\coordinate ->
        traverse_
          (\direction ->
              case hexNeighbourCoord layout coordinate direction of
                Nothing -> pure ()
                Just neighbour -> do
                  assertEqual
                    "opposite direction returns to the source"
                    (Just coordinate)
                    (hexNeighbourCoord layout neighbour (oppositeHexDirection direction))
                  assertEqual
                    "adjacent cells share one canonical side"
                    (hexCellSide coordinate direction)
                    (hexCellSide neighbour (oppositeHexDirection direction))
          )
          allHexDirections
    )
    coordinates
  assertEqual "east overflow is typed absence" Nothing (hexStepCoord (HexCoord maxBound 0) HexEast)

packedRegionLaws :: Assertion
packedRegionLaws = do
  layout <- requireRight (hexLayout (HexCoord 0 0) 2 2)
  let universe = Set.fromList (Vector.toList (layoutCoords layout))
      subsets = powerset universe
  regions <- traverse (regionFromSet layout) subsets
  traverse_
    (\(leftSet, left) -> do
        assertRegion "idempotent union" leftSet =<< requireRight (hexRegionUnion left left)
        assertRegion "empty union identity" leftSet =<< requireRight (hexRegionUnion left (emptyHexRegion layout))
        assertRegion "full intersection identity" leftSet =<< requireRight (hexRegionIntersection left (fullHexRegion layout))
        assertRegion "relative complement" (universe Set.\\ leftSet) (complementHexRegion left)
        traverse_
          (\(rightSet, right) -> do
              assertRegion "union" (Set.union leftSet rightSet) =<< requireRight (hexRegionUnion left right)
              assertRegion "intersection" (Set.intersection leftSet rightSet) =<< requireRight (hexRegionIntersection left right)
              assertRegion "difference" (Set.difference leftSet rightSet) =<< requireRight (hexRegionDifference left right)
              assertRegion
                "symmetric difference"
                ((leftSet Set.\\ rightSet) `Set.union` (rightSet Set.\\ leftSet))
                =<< requireRight (hexRegionSymmetricDifference left right)
              assertEqual
                "subset"
                (Right (leftSet `Set.isSubsetOf` rightSet))
                (hexRegionSubsetOf left right)
          )
          regions
    )
    regions
  traverse_
    (\(_, left) ->
        traverse_
          (\(_, right) ->
              traverse_
                (\(_, third) -> do
                    leftAssociated <- requireRight (hexRegionUnion left right) >>= requireRight . (`hexRegionUnion` third)
                    rightAssociated <- requireRight (hexRegionUnion right third) >>= requireRight . hexRegionUnion left
                    assertEqual "union associativity" leftAssociated rightAssociated
                )
                regions
          )
          regions
    )
    regions

restrictionLaws :: Assertion
restrictionLaws = do
  outer <- requireRight (hexLayout (HexCoord (-2) (-2)) 6 5)
  inner <- requireRight (hexLayout (HexCoord 0 (-1)) 3 2)
  source <- requireRight (hexRegionFromCoords outer [HexCoord (-2) (-2), HexCoord 0 (-1), HexCoord 2 0, HexCoord 3 2])
  restricted <- requireRight (restrictHexRegion inner source)
  assertRegion "restriction" (Set.fromList [HexCoord 0 (-1), HexCoord 2 0]) restricted
  assertEqual "reframing back preserves the restricted section" restricted (reframeHexRegion inner (reframeHexRegion outer restricted))
  left <- requireRight (hexRegionFromCoords outer [HexCoord 0 (-1), HexCoord 1 0])
  right <- requireRight (hexRegionFromCoords outer [HexCoord 2 0, HexCoord 0 (-1)])
  unionBefore <- requireRight (hexRegionUnion left right) >>= requireRight . restrictHexRegion inner
  leftRestricted <- requireRight (restrictHexRegion inner left)
  rightRestricted <- requireRight (restrictHexRegion inner right)
  unionAfter <- requireRight (hexRegionUnion leftRestricted rightRestricted)
  assertEqual "restriction commutes with union" unionBefore unionAfter

gluingLaws :: Assertion
gluingLaws = do
  leftLayout <- requireRight (hexLayout (HexCoord 0 0) 2 2)
  rightLayout <- requireRight (hexLayout (HexCoord 1 0) 2 2)
  left <- requireRight (hexRegionFromCoords leftLayout [HexCoord 0 0, HexCoord 1 0])
  right <- requireRight (hexRegionFromCoords rightLayout [HexCoord 1 0, HexCoord 2 1])
  glued <- requireRight (glueCompatibleHexRegions (left NonEmpty.:| [right]))
  assertRegion "glued section" (Set.fromList [HexCoord 0 0, HexCoord 1 0, HexCoord 2 1]) glued
  thirdLayout <- requireRight (hexLayout (HexCoord 2 0) 2 2)
  third <- requireRight (hexRegionFromCoords thirdLayout [HexCoord 2 1, HexCoord 3 0])
  gluedThree <- requireRight (glueCompatibleHexRegions (left NonEmpty.:| [right, third]))
  assertRegion
    "three-section gluing"
    (Set.fromList [HexCoord 0 0, HexCoord 1 0, HexCoord 2 1, HexCoord 3 0])
    gluedThree
  disagreeing <- requireRight (hexRegionFromCoords rightLayout [HexCoord 2 1])
  assertEqual
    "overlap disagreement names its coordinate"
    (Left (HexOverlapDisagreement (HexCoord 1 0)))
    (glueCompatibleHexRegions (left NonEmpty.:| [disagreeing]))

planarLaws :: Assertion
planarLaws = do
  cellLoop <- requireRight (hexCellExactLoop (HexCoord 0 0))
  assertEqual "one cell has six exact vertices" 6 (NonEmpty.length (exactLoopPoints cellLoop))
  layout <- requireRight (hexLayout (HexCoord 0 0) 2 1)
  adjacent <- requireRight (hexRegionFromCoords layout [HexCoord 0 0, HexCoord 1 0])
  adjacentRegion <- requireRight (hexRegionPlanarRegion adjacent)
  case planarRegionComponents adjacentRegion of
    [component] -> do
      assertEqual "shared side is absent from the outer boundary" 10 (NonEmpty.length (exactLoopPoints (polygonOuterLoop component)))
      assertEqual "adjacent cells introduce no hole" [] (polygonHoleLoops component)
    components -> fail ("expected one adjacent-cell component, observed " <> show (length components))

  ringLayout <- requireRight (hexLayout (HexCoord (-1) (-1)) 3 3)
  let ringCoordinates =
        [ coordinate
        | direction <- NonEmpty.toList allHexDirections
        , Just coordinate <- [hexStepCoord (HexCoord 0 0) direction]
        ]
  ring <- requireRight (hexRegionFromCoords ringLayout ringCoordinates)
  ringRegion <- requireRight (hexRegionPlanarRegion ring)
  case planarRegionComponents ringRegion of
    [component] -> assertEqual "six-cell ring retains its hole" 1 (length (polygonHoleLoops component))
    components -> fail ("expected one ring component, observed " <> show (length components))

  nestedLayout <- requireRight (hexLayout (HexCoord (-4) (-4)) 9 9)
  let onRing :: Int -> HexCoord -> Bool
      onRing radius (HexCoord q r) =
        abs q <= radius
          && abs r <= radius
          && (abs q == radius || abs r == radius)
      nestedRings = hexRegionGenerate nestedLayout (\coordinate -> onRing 4 coordinate || onRing 2 coordinate)
  nestedRegion <- requireRight (hexRegionPlanarRegion nestedRings)
  assertEqual "nested rings retain two components" 2 (length (planarRegionComponents nestedRegion))
  assertEqual
    "each nested component owns its nearest hole"
    2
    (sum (fmap (length . polygonHoleLoops) (planarRegionComponents nestedRegion)))

serializationLaws :: Assertion
serializationLaws = do
  layout <- requireRight (hexLayout (HexCoord (-3) 4) 13 7)
  let source = hexRegionGenerate layout (\(HexCoord q r) -> (q + 2 * r) `mod` 5 == 0)
      bytes = encodeHexRegion source
      budget = HexDecodingBudget 4096 1024
  assertEqual "round trip" (Right source) (decodeHexRegion budget bytes)
  assertEqual
    "input byte budget is checked before parsing"
    (Left (HexInputByteBudgetExceeded (fromIntegral (BL.length bytes)) 1))
    (decodeHexRegion (HexDecodingBudget 1 1024) bytes)
  assertBool "wire representation is packed" (BL.length bytes < fromIntegral (hexLayoutCellCount layout))

wordBoundaryLaws :: Assertion
wordBoundaryLaws =
  traverse_
    (\cellCount -> do
        layout <- requireRight (hexLayout (HexCoord 0 0) cellCount 1)
        let region = hexRegionGenerate layout (\(HexCoord q _) -> even q)
            expected = Set.fromList [HexCoord q 0 | q <- [0 .. cellCount - 1], even q]
        assertRegion ("word boundary " <> show cellCount) expected region
        assertRegion ("double complement " <> show cellCount) expected (complementHexRegion (complementHexRegion region))
    )
    [63, 64, 65, 127, 128]

layoutCoords :: HexLayout -> Vector.Vector HexCoord
layoutCoords layout =
  Vector.mapMaybe (hexLayoutCoordAt layout) (Vector.enumFromN 0 (hexLayoutCellCount layout))

regionFromSet :: HexLayout -> Set HexCoord -> IO (Set HexCoord, HexRegion)
regionFromSet layout coordinates =
  fmap ((,) coordinates) (requireRight (hexRegionFromCoords layout coordinates))

assertRegion :: String -> Set HexCoord -> HexRegion -> Assertion
assertRegion label expected actual =
  assertEqual label expected (Set.fromList (Vector.toList (hexRegionCoords actual)))

powerset :: Ord value => Set value -> [Set value]
powerset = foldr (\value rest -> rest <> fmap (Set.insert value) rest) [Set.empty] . Set.toAscList

requireRight :: Show obstruction => Either obstruction value -> IO value
requireRight = either (fail . show) pure

assertEqual :: (Eq value, Show value) => String -> value -> value -> Assertion
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else fail (label <> ": expected " <> show expected <> ", observed " <> show actual)

assertBool :: String -> Bool -> Assertion
assertBool label condition = if condition then pure () else fail label
