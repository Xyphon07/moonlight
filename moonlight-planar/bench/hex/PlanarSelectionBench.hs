module PlanarSelectionBench
  ( holedRegion
  , runPlanarRepeated
  ) where

import BenchMeasure (requireRight, timedValue)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%), denominator, numerator)
import Moonlight.Hex.Coordinate (HexCoord (..))
import Moonlight.Hex.Element (hexCellVertices, hexVertexCoordinates)
import Moonlight.Hex.Planar
  ( hexRegionByCenterInPlanarRegion
  , hexRegionFullyCoveredByPlanarRegion
  , hexRegionIntersectingPlanarRegion
  , hexRegionPlanarRegion
  )
import Moonlight.Hex.Region
  ( HexLayout
  , HexRegion
  , fullHexRegion
  , hexLayout
  , hexLayoutCellCount
  , hexLayoutWordCount
  , hexRegionCardinality
  )
import Moonlight.Planar.Exact (ExactPoint, exactPoint, exactRational)
import Moonlight.Planar.Region
  ( ExactLoop
  , PlanarRegion
  , exactLoop
  , planarRegion
  , polygonComponent
  )
import PlanarSelectionBaseline qualified as Baseline
import System.Mem (performGC)
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf)

-- Every function application below receives the same fully evaluated pair.
-- The region builders, output-agreement checks, and one-shot allocation
-- receipts are outside the repeated Tasty timing distribution.
runPlanarRepeated :: IO ()
runPlanarRepeated = do
  fixtures <- planarFixtures >>= evaluate . force
  traverse_ reportFixture fixtures
  defaultMain (fmap benchmarkFixture fixtures)

type PlanarFixture = (String, (HexLayout, PlanarRegion))

benchmarkFixture :: PlanarFixture -> Benchmark
benchmarkFixture (label, input) =
  bgroup label
    [ bgroup "centre"
        [ bench "baseline" (nf (uncurry Baseline.hexRegionByCenterInPlanarRegion) input)
        , bench "candidate" (nf (uncurry hexRegionByCenterInPlanarRegion) input)
        ]
    , bgroup "coverage"
        [ bench "baseline" (nf (uncurry Baseline.hexRegionFullyCoveredByPlanarRegion) input)
        , bench "candidate" (nf (uncurry hexRegionFullyCoveredByPlanarRegion) input)
        ]
    , bgroup "intersection"
        [ bench "baseline" (nf (uncurry Baseline.hexRegionIntersectingPlanarRegion) input)
        , bench "candidate" (nf (uncurry hexRegionIntersectingPlanarRegion) input)
        ]
    ]

reportFixture :: PlanarFixture -> IO ()
reportFixture (label, input@(layout, _)) = do
  baselineCentre <- measured "centre/baseline" Baseline.hexRegionByCenterInPlanarRegion
  candidateCentre <- measured "centre/candidate" hexRegionByCenterInPlanarRegion
  baselineCoverage <- measured "coverage/baseline" Baseline.hexRegionFullyCoveredByPlanarRegion
  candidateCoverage <- measured "coverage/candidate" hexRegionFullyCoveredByPlanarRegion
  baselineIntersection <- measured "intersection/baseline" Baseline.hexRegionIntersectingPlanarRegion
  candidateIntersection <- measured "intersection/candidate" hexRegionIntersectingPlanarRegion
  if baselineCentre == candidateCentre && baselineCoverage == candidateCoverage && baselineIntersection == candidateIntersection
    then putStrLn
      ( label <> "-matched-receipt: cells=" <> show (hexLayoutCellCount layout)
          <> " words=" <> show (hexLayoutWordCount layout)
          <> " selected=" <> show (hexRegionCardinality candidateCentre, hexRegionCardinality candidateCoverage, hexRegionCardinality candidateIntersection)
          <> " centre-coverage-intersection=equal"
      )
    else fail (label <> ": baseline/candidate packed outputs disagree")
 where
  measured :: String -> (HexLayout -> PlanarRegion -> HexRegion) -> IO HexRegion
  measured lane classify = do
    performGC
    timedValue (label <> "/" <> lane) (evaluate (force (uncurry classify input)))

planarFixtures :: IO [PlanarFixture]
planarFixtures = do
  holed <- holedRegion
  holedFixtures <- traverse (holedFixture holed) [("holed/1", 1, 1), ("holed/85", 17, 5), ("holed/4096", 64, 64)]
  alignedFixtures <- traverse alignedFixture [("aligned/1", HexCoord 0 0, 1, 1), ("aligned/85", HexCoord (-8) (-2), 17, 5)]
  contactFixtures <-
    concat <$> traverse boundaryFixtures
      [ ("vertex-contact", [rectangle 2 0 3 1], [])
      , ("side-contact", [rectangle (-1) 1 1 2], [])
      , ("tiny-island", [rectangle (2 % 5) (1 % 10) (3 % 5) (3 % 10)], [])
      , ("tiny-hole", [rectangle (-12) (-12) 12 12], [rectangle (2 % 5) (1 % 10) (3 % 5) (3 % 10)])
      , ("sliver", [sliver], [])
      , ("empty", [], [])
      ]
  translated <- rationalComponents [fmap (translatePoint largeOrigin) sliver] []
  translatedLayout <- requireRight (hexLayout (offsetOrigin largeOrigin (-8) (-2)) 17 5)
  negative <- rationalComponents [fmap (translatePoint (HexCoord (-31) (-17))) sliver] []
  negativeLayout <- requireRight (hexLayout (HexCoord (-39) (-19)) 17 5)
  dense <- rationalComponents [fmap (\(q, r) -> (3 * q, 2 * r + q)) [(-2, -2), (514, -2), (514, 514), (-2, 514)]] []
  denseLayout <- requireRight (hexLayout (HexCoord 0 0) 512 512)
  pure
    ( holedFixtures <> alignedFixtures <> contactFixtures
        <> [ ("large-translation/85", (translatedLayout, translated))
           , ("negative-translation/85", (negativeLayout, negative))
           , ("dense/262144", (denseLayout, dense))
           ]
    )
 where
  holedFixture :: PlanarRegion -> (String, Int, Int) -> IO PlanarFixture
  holedFixture region (label, width, height) = do
    layout <- requireRight (hexLayout (HexCoord 0 0) width height)
    pure (label, (layout, region))

  alignedFixture :: (String, HexCoord, Int, Int) -> IO PlanarFixture
  alignedFixture (label, origin, width, height) = do
    layout <- requireRight (hexLayout origin width height)
    region <- requireRight (hexRegionPlanarRegion (fullHexRegion layout))
    pure (label, (layout, region))

  boundaryFixtures :: (String, [[RationalPoint]], [[RationalPoint]]) -> IO [PlanarFixture]
  boundaryFixtures (label, outers, holes) = do
    region <- rationalComponents outers holes
    traverse
      (\(suffix, origin, width, height) -> do
          layout <- requireRight (hexLayout origin width height)
          pure (label <> suffix, (layout, region)))
      [("/1", HexCoord 0 0, 1, 1), ("/85", HexCoord (-8) (-2), 17, 5)]

  rectangle :: Rational -> Rational -> Rational -> Rational -> [RationalPoint]
  rectangle left bottom right top =
    [(left, bottom), (right, bottom), (right, top), (left, top)]

  sliver :: [RationalPoint]
  sliver = [(-22, -7), (22, 7), (22, 7 + 1 % 1024), (-22, -7 + 1 % 1024)]

  largeOrigin :: HexCoord
  largeOrigin = HexCoord (2 ^ (55 :: Int)) (negate (2 ^ (54 :: Int)))

  offsetOrigin :: HexCoord -> Int -> Int -> HexCoord
  offsetOrigin (HexCoord q r) dq dr = HexCoord (q + dq) (r + dr)

type RationalPoint = (Rational, Rational)

translatePoint :: HexCoord -> RationalPoint -> RationalPoint
translatePoint (HexCoord q r) (x, y) =
  ( x + fromInteger (3 * toInteger q)
  , y + fromInteger (2 * toInteger r + toInteger q)
  )

rationalComponents :: [[RationalPoint]] -> [[RationalPoint]] -> IO PlanarRegion
rationalComponents outers holes = do
  admittedHoles <- traverse (admitLoop . reverse) holes
  components <- traverse
    (\outer -> admitLoop outer >>= \loop -> requireRight (polygonComponent loop admittedHoles))
    outers
  requireRight (planarRegion components)
 where
  admitLoop :: [RationalPoint] -> IO ExactLoop
  admitLoop points = do
    exactPoints <- traverse pointOf points
    maybe (fail "planar benchmark loop is empty") (requireRight . exactLoop) (NonEmpty.nonEmpty exactPoints)

  pointOf :: RationalPoint -> IO ExactPoint
  pointOf (x, y) =
    exactPoint
      <$> requireRight (exactRational (numerator x) (denominator x))
      <*> requireRight (exactRational (numerator y) (denominator y))

holedRegion :: IO PlanarRegion
holedRegion = do
  outer <-
    loopOf
      [ (5, 2, 7, 3)
      , (541, 3, 29, 7)
      , (191, 1, 201, 2)
      , (852, 5, 556, 3)
      , (181, 3, 1711, 9)
      , (31, 3, 452, 3)
      ]
  square <- loopOf [(121, 3, 121, 3), (121, 3, 211, 3), (211, 3, 211, 3), (211, 3, 121, 3)]
  triangle <- loopOf [(241, 2, 241, 2), (241, 2, 321, 2), (451, 3, 281, 2)]
  cellHole <-
    requireRight
      ( exactLoop
          ( NonEmpty.reverse
              ( fmap
                  (\vertex -> let (x, y) = hexVertexCoordinates vertex in exactPoint (fromInteger x) (fromInteger y))
                  (hexCellVertices (HexCoord 30 20))
              )
          )
      )
  component <- requireRight (polygonComponent outer [square, cellHole, triangle])
  requireRight (planarRegion [component])
 where
  loopOf :: [(Integer, Integer, Integer, Integer)] -> IO ExactLoop
  loopOf fractions = do
    points <- traverse pointOf fractions
    case points of
      firstPoint : rest -> requireRight (exactLoop (firstPoint :| rest))
      [] -> fail "holedRegion: empty loop"
  pointOf :: (Integer, Integer, Integer, Integer) -> IO ExactPoint
  pointOf (xNumerator, xDenominator, yNumerator, yDenominator) =
    exactPoint
      <$> requireRight (exactRational xNumerator xDenominator)
      <*> requireRight (exactRational yNumerator yDenominator)
