module Main (main) where

import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Ratio ((%), denominator, numerator)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as U
import Data.Word (Word64)
import Moonlight.Hex.Coordinate
import Moonlight.Hex.Element
import Moonlight.Hex.Planar
import Moonlight.Hex.Region
import Moonlight.Hex.Topology
import Moonlight.Hex.Serialization
import Moonlight.Planar.Exact
  ( ExactPoint
  , exactPoint
  , exactPointCoordinates
  , exactRational
  , exactRationalDenominator
  , exactRationalNumerator
  )
import Moonlight.Planar.Region
  ( PlanarRegion
  , exactLoop
  , exactLoopPoints
  , planarRegion
  , planarRegionComponents
  , polygonComponent
  , polygonHoleLoops
  , polygonOuterLoop
  )
main :: IO ()
main = do
  coordinateAndElementLaws
  packedRegionLaws
  packedRowSpanLaws
  nativeTopologyLaws
  restrictionLaws
  gluingLaws
  planarLaws
  planarRestrictionOracleLaws
  serializationLaws
  wordBoundaryLaws
  putStrLn "hex-laws: coordinate element boolean topology restriction gluing planar serialization word-boundary=passed"

type Assertion = IO ()

packedRowSpanLaws :: Assertion
packedRowSpanLaws = do
  assertEqual "reversed row span is absent" Nothing (hexRowSpan 1 0)
  traverse_ checkLayout
    [ (origin, width, height)
    | origin <- [HexCoord (-73) (-5), HexCoord (2 ^ (55 :: Int)) (-11)]
    , width <- [1, 2, 63, 64, 65, 127, 128, 129]
    , height <- [1, 2, 3]
    ]
 where
  checkLayout :: (HexCoord, Int, Int) -> Assertion
  checkLayout (origin@(HexCoord originQ _), width, height) = do
    layout <- requireRight (hexLayout origin width height)
    let ranges :: Int -> [(Integer, Integer)]
        ranges row =
          if even row
            then [(negate (2 ^ (100 :: Int)), toInteger originQ + 1), (toInteger originQ, toInteger originQ + 4), (toInteger originQ + 8, toInteger originQ + 8)]
            else [(toInteger originQ + 3, 2 ^ (100 :: Int)), (toInteger originQ + 5, toInteger originQ + 7)]
        expected = hexRegionGenerate layout
          (\(HexCoord q r) -> any (\(firstQ, finalQ) -> firstQ <= toInteger q && toInteger q <= finalQ) (ranges r))
        actual = hexRegionGenerateRowSpans layout (mapMaybe (uncurry hexRowSpan) . ranges)
    assertEqual ("row-span packed identity " <> show (origin, width, height)) expected actual
    assertEqual "row-span canonical padding round trip" (Right actual)
      (hexRegionFromPackedWords layout (packedWords actual))

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
  assertEqual
    "coordinate-range overflow is typed"
    (Left (HexLayoutCoordinateRangeOverflow (HexCoord maxBound 0) 2 1))
    (hexLayout (HexCoord maxBound 0) 2 1)
  assertEqual
    "cell-count overflow is typed"
    (Left (HexLayoutCellCountOverflow (toInteger (maxBound :: Int) * 2)))
    (hexLayout (HexCoord minBound 0) maxBound 2)

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
  packedLayout <- requireRight (hexLayout (HexCoord 0 0) 65 1)
  let packedSource = hexRegionGenerate packedLayout (\(HexCoord q _) -> even q)
      sourceWords = packedWords packedSource
      effectfulSource =
        hexRegionGenerateM packedLayout (\(HexCoord q _) -> Right (even q))
          :: Either HexCoord HexRegion
      refusedSource =
        hexRegionGenerateM
          packedLayout
          (\coordinate ->
             if coordinate == HexCoord 2 0
               then Left coordinate
               else Right False)
          :: Either HexCoord HexRegion
  assertEqual "effectful generation agrees with pure generation" (Right packedSource) effectfulSource
  assertEqual "effectful generation preserves predicate refusal" (Left (HexCoord 2 0)) refusedSource
  assertEqual
    "canonical packed words admit without changing the region"
    (Right packedSource)
    (hexRegionFromPackedWords packedLayout sourceWords)
  assertEqual
    "packed word-count mismatch is typed"
    (Left (HexPackedRegionWordCountMismatch 2 1))
    (hexRegionFromPackedWords packedLayout (U.singleton 0))
  assertEqual
    "nonzero packed padding is rejected"
    (Left (HexPackedRegionNonCanonicalPadding (maxBound - 1)))
    (hexRegionFromPackedWords packedLayout (U.fromList [0, maxBound]))
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

nativeTopologyLaws :: Assertion
nativeTopologyLaws = do
  layout <- requireRight (hexLayout (HexCoord (-1) (-1)) 4 4)
  let selectedSet = Set.fromList [HexCoord 0 0, HexCoord 1 0, HexCoord 0 1]
  selected <- requireRight (hexRegionFromCoords layout selectedSet)
  let dilatedSet = referenceDilation layout selectedSet
      erodedSet = referenceErosion layout selectedSet
  assertRegion "native dilation" dilatedSet (hexRegionDilate selected)
  assertRegion "native erosion" erodedSet (hexRegionErode selected)
  assertRegion "native inner frontier" (selectedSet Set.\\ erodedSet) (hexRegionInnerFrontier selected)
  assertRegion "native outer frontier" (dilatedSet Set.\\ selectedSet) (hexRegionOuterFrontier selected)
  let opened = hexRegionOpening selected
      closed = hexRegionClosing selected
  assertEqual "opening is idempotent" opened (hexRegionOpening opened)
  assertEqual "closing is idempotent" closed (hexRegionClosing closed)
  assertEqual "opening is contractive" (Right True) (hexRegionSubsetOf opened selected)
  assertEqual "closing is extensive" (Right True) (hexRegionSubsetOf selected closed)

  boundaryLayout <- requireRight (hexLayout (HexCoord 0 0) 2 2)
  let boundaryUniverse = Set.fromList (Vector.toList (layoutCoords boundaryLayout))
  boundaryRegions <- traverse (regionFromSet boundaryLayout) (powerset boundaryUniverse)
  traverse_
    (\(boundarySet, boundaryRegion) -> do
       let referenceOpened = referenceDilation boundaryLayout (referenceErosion boundaryLayout boundarySet)
           referenceClosed = referenceErosion boundaryLayout (referenceDilation boundaryLayout boundarySet)
           nativeOpened = hexRegionOpening boundaryRegion
           nativeClosed = hexRegionClosing boundaryRegion
       assertRegion "bounded opening reference" referenceOpened nativeOpened
       assertRegion "bounded closing reference" referenceClosed nativeClosed
       assertEqual "bounded opening idempotence" nativeOpened (hexRegionOpening nativeOpened)
       assertEqual "bounded closing idempotence" nativeClosed (hexRegionClosing nativeClosed)
       assertEqual "bounded opening contraction" (Right True) (hexRegionSubsetOf nativeOpened boundaryRegion)
       assertEqual "bounded closing extension" (Right True) (hexRegionSubsetOf boundaryRegion nativeClosed))
    boundaryRegions
  traverse_
    (\(_, leftRegion) ->
       traverse_
         (\(_, rightRegion) ->
            assertEqual
              "bounded dilation/erosion adjunction"
              (hexRegionSubsetOf (hexRegionDilate leftRegion) rightRegion)
              (hexRegionSubsetOf leftRegion (hexRegionErode rightRegion)))
         boundaryRegions)
    boundaryRegions

  componentRegionValue <-
    requireRight
      ( hexRegionFromCoords
          layout
          [HexCoord (-1) (-1), HexCoord 0 (-1), HexCoord 2 0, HexCoord (-1) 2]
      )
  let components = hexRegionComponentLabels componentRegionValue
  assertEqual "three native components" 3 (hexComponentCount components)
  assertEqual "least selected cell seeds component zero" (Just 0) (hexComponentIndexAt (HexCoord (-1) (-1)) components)
  assertEqual "adjacent selected cell shares component" (Just 0) (hexComponentIndexAt (HexCoord 0 (-1)) components)
  assertEqual "next selected cell seeds component one" (Just 1) (hexComponentIndexAt (HexCoord 2 0) components)
  assertEqual "unselected cell has no component" Nothing (hexComponentIndexAt (HexCoord 0 0) components)
  firstComponent <- maybe (fail "expected component zero") pure (hexComponentRegion 0 components)
  assertRegion "component materialization" (Set.fromList [HexCoord (-1) (-1), HexCoord 0 (-1)]) firstComponent
  assertEqual "invalid component is refused" Nothing (hexComponentRegion 3 components)

  domain <-
    requireRight
      ( hexRegionFromCoords
          layout
          [ HexCoord (-1) (-1)
          , HexCoord 0 (-1)
          , HexCoord 1 (-1)
          , HexCoord 0 0
          , HexCoord 1 0
          , HexCoord 2 0
          ]
      )
  sources <- requireRight (hexRegionFromCoords layout [HexCoord (-1) (-1), HexCoord 2 0])
  distances <- requireRight (hexRegionDistancesWithin domain sources)
  assertEqual "source distance" (Just 0) (hexDistanceAt (HexCoord (-1) (-1)) distances)
  assertEqual "multi-source minimum distance" (Just 1) (hexDistanceAt (HexCoord 1 0) distances)
  assertEqual "unreachable coordinate has no distance" Nothing (hexDistanceAt (HexCoord (-1) 2) distances)
  assertEqual "maximum finite distance" (Just 2) (hexDistanceMaximum distances)
  assertEqual "distance map preserves its layout" layout (hexDistanceMapLayout distances)
  assertEqual "reachable region is the domain" domain (hexDistanceReachableRegion distances)
  invalidSources <- requireRight (hexRegionFromCoords layout [HexCoord (-1) 2])
  assertEqual
    "source outside traversal domain is typed"
    (Left (HexTraversalSourceOutsideDomain (HexCoord (-1) 2)))
    (hexRegionDistancesWithin domain invalidSources)

  traverse_
    (\cellCount -> do
       rowLayout <- requireRight (hexLayout (HexCoord 0 0) cellCount 2)
       let rowSource = hexRegionGenerate rowLayout (\(HexCoord q r) -> q == cellCount - 1 && r == 0)
           expected =
             Set.fromList
               [ HexCoord (cellCount - 1) 0
               , HexCoord (cellCount - 2) 0
               , HexCoord (cellCount - 1) 1
               , HexCoord (cellCount - 2) 1
               ]
       assertRegion ("packed row boundary dilation " <> show cellCount) expected (hexRegionDilate rowSource))
    [63, 64, 65, 127, 128]
 where
  referenceDilation :: HexLayout -> Set HexCoord -> Set HexCoord
  referenceDilation layout selected =
    Set.foldl'
      (\expanded coordinate ->
         foldl'
           (\result direction -> maybe result (`Set.insert` result) (hexNeighbourCoord layout coordinate direction))
           expanded
           allHexDirections)
      selected
      selected

  referenceErosion :: HexLayout -> Set HexCoord -> Set HexCoord
  referenceErosion layout selected =
    Set.filter
      (\coordinate -> all (maybe True (`Set.member` selected) . hexNeighbourCoord layout coordinate) allHexDirections)
      selected

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
  wideTarget <- requireRight (hexLayout (HexCoord (-5) (-7)) 130 67)
  leftStripLayout <- requireRight (hexLayout (HexCoord (-5) (-7)) 3 67)
  middleLayout <- requireRight (hexLayout (HexCoord (-4) (-5)) 65 31)
  rightStripLayout <- requireRight (hexLayout (HexCoord 122 (-7)) 3 67)
  let layouts = [leftStripLayout, middleLayout, rightStripLayout]
      membership (HexCoord q r) = (q + 3 * r) `mod` 7 <= 2
      localSection localLayout = hexRegionGenerate localLayout membership
      expected =
        hexRegionGenerate
          wideTarget
          (\coordinate -> any (`hexLayoutContains` coordinate) layouts && membership coordinate)
  wideGlued <-
    requireRight
      ( glueCompatibleHexRegions
          ( localSection leftStripLayout
              NonEmpty.:| [localSection middleLayout, localSection rightStripLayout]
          )
      )
  assertEqual "gluing visits disjoint local word spans without changing descent" expected wideGlued

planarLaws :: Assertion
planarLaws = do
  cellLoop <- requireRight (hexCellExactLoop (HexCoord 0 0))
  assertEqual "one cell has six exact vertices" 6 (NonEmpty.length (exactLoopPoints cellLoop))
  restrictionLayout <- requireRight (hexLayout (HexCoord (-1) (-1)) 3 3)
  singleCell <- requireRight (singletonHexRegion restrictionLayout (HexCoord 0 0))
  singleCellPlanar <- requireRight (hexRegionPlanarRegion singleCell)
  assertRegion
    "exact centre restriction"
    (Set.singleton (HexCoord 0 0))
    (hexRegionByCenterInPlanarRegion restrictionLayout singleCellPlanar)
  assertRegion
    "exact full-cell restriction"
    (Set.singleton (HexCoord 0 0))
    (hexRegionFullyCoveredByPlanarRegion restrictionLayout singleCellPlanar)
  let touchingCells =
        Set.fromList
          ( HexCoord 0 0
              : [ neighbour
                | direction <- NonEmpty.toList allHexDirections
                , Just neighbour <- [hexNeighbourCoord restrictionLayout (HexCoord 0 0) direction]
                ]
          )
  assertRegion
    "exact closed-intersection restriction retains edge-touching cells"
    touchingCells
    (hexRegionIntersectingPlanarRegion restrictionLayout singleCellPlanar)

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

-- Independent rational arithmetic oracle. It deliberately neither calls the
-- production point/segment predicates nor uses the row candidate index.
type RationalPoint = (Rational, Rational)

planarRestrictionOracleLaws :: Assertion
planarRestrictionOracleLaws = do
  traverse_ checkFixture
    [ ("empty region", [], [])
    , ("tiny island", [[(2 % 5, 1 % 10), (3 % 5, 1 % 10), (3 % 5, 3 % 10), (2 % 5, 3 % 10)]], [])
    , ("tiny hole", [rectangle (-12) (-12) 12 12], [rectangle (2 % 5) (1 % 10) (3 % 5) (3 % 10)])
    , ("vertex contact", [rectangle 2 0 3 1], [])
    , ("full side contact", [rectangle (-1) 1 1 2], [])
    , ("oblique collinear row", [[(0, 0), (9, 3), (9, 7), (0, 4)]], [])
    , ("thin corridor", [[(-22, -7), (22, 7), (22, 71 % 10), (-22, -69 % 10)]], [])
    , ("separated islands", [rectangle (-19) (-5) (-17) (-3), rectangle 17 3 19 5], [])
    , ("hole tangent to centre row", [rectangle (-12) (-12) 12 12], [[(0, 0), (1, 1), (-1, 1)]])
    , ("concave row events", [[(-6, -2), (6, -2), (6, 2), (3, 1), (0, 2), (-3, 1), (-6, 2)]], [])
    ]
 where
  rectangle :: Rational -> Rational -> Rational -> Rational -> [RationalPoint]
  rectangle left bottom right top = [(left, bottom), (right, bottom), (right, top), (left, top)]
  checkFixture (label, outers, holes) =
    traverse_
      (\translation@(translationQ, translationR) -> do
          layout <- requireRight (hexLayout (HexCoord (translationQ - 8) (translationR - 2)) 17 5)
          oneCell <- requireRight (hexLayout (HexCoord translationQ translationR) 1 1)
          region <- rationalRegion (fmap (fmap (translatePoint translation)) outers) (fmap (fmap (translatePoint translation)) holes)
          traverse_ (checkSelections label region) [layout, oneCell])
      [(0, 0), (-13, 7), (2 ^ (55 :: Int), negate (2 ^ (54 :: Int)))]
  translatePoint :: (Int, Int) -> RationalPoint -> RationalPoint
  translatePoint (q, r) (x, y) =
    (x + fromInteger (3 * toInteger q), y + fromInteger (2 * toInteger r + toInteger q))
  checkSelections label region layout = do
    let coordinates = Vector.toList (layoutCoords layout)
        loops = rationalRegionLoops region
        boundaries = concatMap (concatMap rationalCycle) loops
        centre :: HexCoord -> RationalPoint
        centre (HexCoord q r) = (fromInteger (3 * toInteger q), fromInteger (2 * toInteger r + toInteger q))
        cellPoints = fmap (\vertex -> let (x, y) = hexVertexCoordinates vertex in (fromInteger x, fromInteger y)) . NonEmpty.toList . hexCellVertices
        centreSelected = rationalRegionContains loops . centre
        covered coordinate = centreSelected coordinate && not (any (segmentHasInteriorPoint (cellPoints coordinate)) boundaries)
        intersecting coordinate =
          let points = cellPoints coordinate
              sides = rationalCycle points
           in any (rationalRegionContains loops) points
                || any (\(from, to) -> rationalConvexContains points from || any (rationalSegmentsMeet (from, to)) sides) boundaries
        expected predicate = Set.fromList (filter predicate coordinates)
    assertRegion (label <> " centre oracle") (expected centreSelected) (hexRegionByCenterInPlanarRegion layout region)
    assertRegion (label <> " coverage oracle") (expected covered) (hexRegionFullyCoveredByPlanarRegion layout region)
    assertRegion (label <> " intersection oracle") (expected intersecting) (hexRegionIntersectingPlanarRegion layout region)

rationalRegion :: [[RationalPoint]] -> [[RationalPoint]] -> IO PlanarRegion
rationalRegion outers holes = do
  admittedHoles <- traverse (admitLoop . reverse) holes
  components <- traverse (\outer -> admitLoop outer >>= \loop -> requireRight (polygonComponent loop admittedHoles)) outers
  requireRight (planarRegion components)
 where
  admitLoop points = do
    exactPoints <- traverse exactRationalPoint points
    maybe (fail "oracle fixture loop is empty") (requireRight . exactLoop) (NonEmpty.nonEmpty exactPoints)
  exactRationalPoint (x, y) =
    exactPoint <$> requireRight (exactRational (numerator x) (denominator x)) <*> requireRight (exactRational (numerator y) (denominator y))

rationalRegionLoops :: PlanarRegion -> [[[RationalPoint]]]
rationalRegionLoops =
  fmap (\component -> fmap (fmap rationalPoint . NonEmpty.toList . exactLoopPoints) (polygonOuterLoop component : polygonHoleLoops component))
    . planarRegionComponents
 where
  rationalPoint :: ExactPoint -> RationalPoint
  rationalPoint point =
    let (x, y) = exactPointCoordinates point
        rational value = exactRationalNumerator value % exactRationalDenominator value
     in (rational x, rational y)

rationalCycle :: [point] -> [(point, point)]
rationalCycle points = case points of
  [] -> []
  firstPoint : remaining -> zip points (remaining <> [firstPoint])

rationalCross :: RationalPoint -> RationalPoint -> RationalPoint -> Rational
rationalCross (ax, ay) (bx, by) (px, py) = (bx - ax) * (py - ay) - (by - ay) * (px - ax)

rationalOnSegment :: RationalPoint -> (RationalPoint, RationalPoint) -> Bool
rationalOnSegment point@(x, y) (from@(ax, ay), to@(bx, by)) =
  rationalCross from to point == 0 && min ax bx <= x && x <= max ax bx && min ay by <= y && y <= max ay by

rationalLoopLocation :: [RationalPoint] -> RationalPoint -> Ordering
rationalLoopLocation points query@(x, y)
  | any (rationalOnSegment query) sides = EQ
  | odd (length (filter crosses sides)) = GT
  | otherwise = LT
 where
  sides = rationalCycle points
  crosses ((ax, ay), (bx, by)) = (ay > y) /= (by > y) && x < ax + (y - ay) * (bx - ax) / (by - ay)

rationalRegionContains :: [[[RationalPoint]]] -> RationalPoint -> Bool
rationalRegionContains components point =
  any (\loops -> case loops of
      [] -> False
      outer : holes -> rationalLoopLocation outer point /= LT && all ((/= GT) . (`rationalLoopLocation` point)) holes)
    components

rationalConvexContains :: [RationalPoint] -> RationalPoint -> Bool
rationalConvexContains points query = all (\(from, to) -> rationalCross from to query >= 0) (rationalCycle points)

rationalSegmentsMeet :: (RationalPoint, RationalPoint) -> (RationalPoint, RationalPoint) -> Bool
rationalSegmentsMeet left@(a, b) right@(c, d) =
  any (`rationalOnSegment` right) [a, b]
    || any (`rationalOnSegment` left) [c, d]
    || (rationalCross a b c * rationalCross a b d < 0 && rationalCross c d a * rationalCross c d b < 0)

segmentHasInteriorPoint :: [RationalPoint] -> (RationalPoint, RationalPoint) -> Bool
segmentHasInteriorPoint polygon (from@(ax, ay), to@(bx, by)) =
  any strictlyInside (fmap pointAt parameters)
 where
  sides = rationalCycle polygon
  pointAt parameter = (ax + parameter * (bx - ax), ay + parameter * (by - ay))
  strictlyInside point = all (\(a, b) -> rationalCross a b point > 0) sides
  cuts = List.sort (0 : 1 :
    [ parameter
    | (a, b) <- sides
    , let fromSide = rationalCross a b from
    , let toSide = rationalCross a b to
    , fromSide /= toSide
    , let parameter = fromSide / (fromSide - toSide)
    , parameter > 0 && parameter < 1
    ])
  parameters = cuts <> zipWith (\left right -> (left + right) / 2) cuts (drop 1 cuts)

serializationLaws :: Assertion
serializationLaws = do
  layout <- requireRight (hexLayout (HexCoord (-3) 4) 13 7)
  let source = hexRegionGenerate layout (\(HexCoord q r) -> (q + 2 * r) `mod` 5 == 0)
      bytes = encodeHexRegion source
      budget = HexDecodingBudget 4096 1024
      (prefix, finalWord) = BL.splitAt (BL.length bytes - 8) bytes
      nonCanonicalBytes = prefix <> BL.cons 0x80 (BL.drop 1 finalWord)
  assertEqual "round trip" (Right source) (decodeHexRegion budget bytes)
  assertEqual
    "serialized padding is rejected by the region owner"
    ( Left
        ( HexSerializedPackedRegionInvalid
            (HexPackedRegionNonCanonicalPadding 0x8000000000000000)
        )
    )
    (decodeHexRegion budget nonCanonicalBytes)
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

packedWords :: HexRegion -> U.Vector Word64
packedWords = U.fromList . reverse . foldHexRegionPackedWords (flip (:)) []
