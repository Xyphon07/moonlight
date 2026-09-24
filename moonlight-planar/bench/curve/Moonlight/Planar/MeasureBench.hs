-- | New-capability receipts for certified arc length. Preparation is timed
-- cold per family; inverse queries are timed against one prepared trail.
-- Every receipt is checked against the policy tolerance before it is printed.
module Moonlight.Planar.MeasureBench (benchmarks) where

import BenchMeasure (requireRight, timedProjection)
import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve
  ( ClosedTrail, CurveStep, Located, Subpath (..), closeWith, cubic, curveStep, ellipse
  , hermiteStep, line, locate, openTrail, rationalQuadratic, stepControlPoints )
import Moonlight.Planar.Curve.Measure
  ( ArcSample, Distance, MeasurePolicy, MeasuredTrail, distance, lengthBounds
  , lengthEnclosureLower, lengthEnclosureWidth, measurePolicy, measureSubpath, subdivisionBudget
  , measuredSpanPiece, measuredSpanStart, measuredSpans, pointAtLength, radicalPrecision
  , sampleResidual )
import Moonlight.Planar.Exact
  ( ExactRational, ExactVector (..), exactPoint, exactPointBitWidth, exactRational
  , exactRationalBitWidth, divideByPositive, positiveExact, positiveOne, ratioPositive )

data MeasureFamily
  = ExactCircle
  | SkewEllipse
  | HornSpans
  | CuspsAndOvershoot
  | ExtremeWeights
  deriving stock (Eq, Show)

data MeasureReceipt = MeasureReceipt
  { acceptedSpans :: !Int
  , maximumCoordinateBits :: !Int
  , enclosureWidth :: !ExactRational
  }
  deriving stock (Show)

instance NFData MeasureReceipt where
  rnf receipt = rnf (acceptedSpans receipt)
    `seq` rnf (maximumCoordinateBits receipt)
    `seq` rnf (enclosureWidth receipt)

data QueryReceipt = QueryReceipt
  { answeredQueries :: !Int
  , maximumResidual :: !ExactRational
  }
  deriving stock (Show)

instance Semigroup QueryReceipt where
  a <> b = QueryReceipt (answeredQueries a + answeredQueries b)
    (max (maximumResidual a) (maximumResidual b))

instance Monoid QueryReceipt where
  mempty = QueryReceipt 0 0

data StationOrder = Sorted | Unsorted
  deriving stock (Eq, Show)

benchmarks :: IO ()
benchmarks = do
  putStrLn "measure-benchmark: new capability; no baseline or speedup claim"
  putStrLn "measure-benchmark: tolerance=1/1000 absolute; max-depth=32; max-leaves=65536 per subpath; radical-precision=128; max-bits=16384"
  putStrLn "measure-benchmark: elapsed includes full output forcing; process max-live is cumulative"
  tolerance <- requireRight (exactRational 1 1000)
  policy <- benchmarkPolicy tolerance
  traverse_ (prepareFamily tolerance policy)
    [ExactCircle, SkewEllipse, HornSpans, CuspsAndOvershoot, ExtremeWeights]
  prepared <- geometry SkewEllipse >>= requireRight . measureSubpath policy >>= evaluate . force
  traverse_ (uncurry (queryStations tolerance prepared))
    [(count, order) | count <- [1, 64, 1024], order <- [Sorted, Unsorted]]
  endToEnd tolerance policy

benchmarkPolicy :: ExactRational -> IO MeasurePolicy
benchmarkPolicy tolerance = do
  admitted <- requireRight (positiveExact tolerance)
  precision <- requireRight (radicalPrecision 128)
  measurePolicy admitted precision <$> requireRight (subdivisionBudget 32 65536 16384)

prepareFamily :: ExactRational -> MeasurePolicy -> MeasureFamily -> IO ()
prepareFamily tolerance policy family = do
  source <- geometry family >>= evaluate . force
  measured <- timedProjection ("measure-" <> show family <> "-prepare") observe
    (requireRight (measureSubpath policy source))
  let receipt = observe measured
  putStrLn ("measure-" <> show family <> "-receipt: " <> show receipt)
  withinTolerance ("measure width for " <> show family) tolerance (enclosureWidth receipt)

-- One prepared trail answers every query and no query reuses another's work,
-- so sorted and unsorted stations exercise the same per-query path.
queryStations :: ExactRational -> MeasuredTrail -> Int -> StationOrder -> IO ()
queryStations tolerance prepared count order = do
  targets <- stations count order prepared >>= evaluate . force
  let label = "measure-query-" <> show count <> "-" <> show order
  samples <- timedProjection label (fmap sampleResidual)
    (requireRight (traverse (`pointAtLength` prepared) targets))
  reportQueries label tolerance samples

endToEnd :: ExactRational -> MeasurePolicy -> IO ()
endToEnd tolerance policy = do
  source <- geometry SkewEllipse >>= evaluate . force
  samples <- timedProjection "measure-prepare-and-query-64" (fmap sampleResidual)
    (do
      prepared <- requireRight (measureSubpath policy source)
      targets <- stations 64 Sorted prepared
      requireRight (traverse (`pointAtLength` prepared) targets))
  reportQueries "measure-prepare-and-query-64" tolerance samples

reportQueries :: String -> ExactRational -> [ArcSample] -> IO ()
reportQueries label tolerance samples = do
  let receipt = foldMap (QueryReceipt 1 . sampleResidual) samples
  putStrLn (label <> "-receipt: " <> show receipt)
  withinTolerance ("query residual for " <> label) tolerance (maximumResidual receipt)

-- Equally spaced distances over the certified lower length. The unsorted
-- order visits them by the stride 37, coprime to every benchmarked count.
stations :: Int -> StationOrder -> MeasuredTrail -> IO [Distance]
stations count order prepared = do
  divisor <- requireRight (positiveExact (fromIntegral count))
  let station :: Int -> ExactRational
      station index = divideByPositive (total * fromIntegral index) divisor
  requireRight (traverse (distance . station) (ordered [0 .. count - 1]))
 where
  total = lengthEnclosureLower (lengthBounds prepared)
  ordered = case order of
    Sorted -> id
    Unsorted -> fmap (\index -> index * 37 `mod` count)

observe :: MeasuredTrail -> MeasureReceipt
observe measured = MeasureReceipt
  (Seq.length (measuredSpans measured))
  (foldr (max . coordinateBits) 0 (measuredSpans measured))
  (lengthEnclosureWidth (lengthBounds measured))
 where
  -- Located starts and controls only; S5 replaces this with the shared
  -- span-bit observer.
  coordinateBits spanValue = foldr (max . vectorBits)
    (exactPointBitWidth (measuredSpanStart spanValue)) (stepControlPoints (measuredSpanPiece spanValue))
  vectorBits (ExactVector x y) = max (exactRationalBitWidth x) (exactRationalBitWidth y)

withinTolerance :: String -> ExactRational -> ExactRational -> IO ()
withinTolerance label tolerance value
  | value <= tolerance = pure ()
  | otherwise = fail (label <> " exceeds tolerance: " <> show value)

geometry :: MeasureFamily -> IO Subpath
geometry ExactCircle = pure (ClosedSubpath (ellipse (ExactVector 40 0) (ExactVector 0 40)))
geometry SkewEllipse = pure (ClosedSubpath (ellipse (ExactVector 70 15) (ExactVector (-12) 28)))
geometry HornSpans = pure (ClosedSubpath horn)
geometry CuspsAndOvershoot = pure (openSubpath pathologicalSteps)
geometry ExtremeWeights = do
  large <- requireRight (positiveExact 1000)
  huge <- requireRight (positiveExact 1000000)
  let small = ratioPositive positiveOne large
      conic u v = curveStep (rationalQuadratic (ExactVector 5 (-3)) u v) (ExactVector 2 7)
  pure (openSubpath [conic small large, conic large small, conic large huge])

openSubpath :: [CurveStep] -> Subpath
openSubpath = OpenSubpath . locate (exactPoint 0 0) . openTrail . Seq.fromList

horn :: Located ClosedTrail
horn = locate (exactPoint 0 0) (closeWith line (openTrail (Seq.fromList
  [ hermiteStep (ExactVector 28 (-50)) (ExactVector 15 (-65)) (ExactVector 40 (-30))
  , hermiteStep (ExactVector 22 (-30)) (ExactVector 40 (-30)) (ExactVector 8 (-35))
  , hermiteStep (ExactVector (-32) 35) (ExactVector (-45) 10) (ExactVector (-35) 25)
  , hermiteStep (ExactVector (-18) 45) (ExactVector (-35) 25) (ExactVector (-8) 60)
  ])))

pathologicalSteps :: [CurveStep]
pathologicalSteps =
  [ curveStep (cubic (ExactVector 60 80) (ExactVector (-60) 80)) (ExactVector 0 0)
  , curveStep (cubic (ExactVector 100 0) (ExactVector (-100) 0)) (ExactVector 1 0)
  , curveStep (cubic (ExactVector 1 80) (ExactVector (-1) (-80))) (ExactVector 2 0)
  ]
