-- | New-capability receipts, not a comparison with fixed-sample artwork.
-- Timings include exact leaf-bound observation and complete output forcing.
module Moonlight.Planar.CurveBench (benchmarks) where

import BenchMeasure (requireRight, timedProjection)
import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Data.Foldable (toList, traverse_)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine
  ( Affine2, affine2, identityAffine2 )
import Moonlight.Planar.Curve
  ( Subpath (..), CurveStep, ClosedTrail, Located, Path
  , closeWith, closedTrailSteps, cubic, curveStep, ellipse, hermiteStep, line
  , locate, locatedValue, location, transformPath, openTrail, path
  , pathSubpaths, stepControlPoints, trailSteps )
import Moonlight.Planar.Curve.Lowering
  ( LoweredPath, LoweringError, LoweringPolicy, loweredSpans, loweredTolerance
  , lowerClosedTrail, lowerOpenTrail, loweringPolicy, spanFrom, spanParameterFrom
  , spanParameterTo, spanSquaredBound, spanTo )
import Moonlight.Planar.Exact
  ( ExactRational, ExactVector (..), exactPoint, exactPointBitWidth
  , exactRational, exactRationalBitWidth, positiveExact, positiveExactValue )

data CurveFamily
  = HornSpans
  | ExactCircle
  | SkewEllipse
  | RepeatedMotifs
  | AnisotropicHorn
  | CuspsAndOvershoot
  deriving stock (Eq, Show)

data CurveReceipt = CurveReceipt
  { emittedLeaves :: !Int
  , maximumCoordinateBits :: !Int
  , maximumParameterBits :: !Int
  , checkedLeafBounds :: !Int
  , allLeafBoundsPass :: !Bool
  }
  deriving stock (Show)

instance NFData CurveReceipt where
  rnf receipt = rnf (emittedLeaves receipt)
    `seq` rnf (maximumCoordinateBits receipt)
    `seq` rnf (maximumParameterBits receipt)
    `seq` rnf (checkedLeafBounds receipt)
    `seq` rnf (allLeafBoundsPass receipt)

instance Semigroup CurveReceipt where
  a <> b = CurveReceipt
    (emittedLeaves a + emittedLeaves b)
    (max (maximumCoordinateBits a) (maximumCoordinateBits b))
    (max (maximumParameterBits a) (maximumParameterBits b))
    (checkedLeafBounds a + checkedLeafBounds b)
    (allLeafBoundsPass a && allLeafBoundsPass b)

instance Monoid CurveReceipt where
  mempty = CurveReceipt 0 0 0 0 True

benchmarks :: IO ()
benchmarks = do
  putStrLn "curve-benchmark: new capability; no fixed-sample speedup or topology claim"
  putStrLn "curve-benchmark: epsilon=1/8 in declared affine metric; max-depth=16; max-leaves=8192 per subpath"
  putStrLn "curve-benchmark: elapsed includes exact leaf-bound observations and full output forcing; process max-live is cumulative"
  traverse_ benchmarkFamily
    [HornSpans, ExactCircle, SkewEllipse, RepeatedMotifs, AnisotropicHorn, CuspsAndOvershoot]

benchmarkFamily :: CurveFamily -> IO ()
benchmarkFamily family = do
  tolerance <- requireRight (exactRational 1 8) >>= requireRight . positiveExact
  policy <- requireRight (loweringPolicy tolerance (metric family) 16 8192)
  authored <- evaluate (force (geometry family))
  let sourceBits = inputBits authored
      sourceSteps = sum (length . stepsOf <$> toList (pathSubpaths authored))
  result <- timedProjection ("curve-" <> show family) observe
    (requireRight (traverse (lowerSubpath policy) (toList (pathSubpaths authored))))
  let receipt = observe result
  putStrLn ("curve-" <> show family <> "-receipt: input-steps=" <> show sourceSteps
    <> " input-coordinate-bits=" <> show sourceBits
    <> " observed-coordinate-bit-growth=" <> show (max 0 (maximumCoordinateBits receipt - sourceBits))
    <> " " <> show receipt)
  if allLeafBoundsPass receipt
    then pure ()
    else fail ("curve leaf bound failed for " <> show family)

observe :: [LoweredPath] -> CurveReceipt
observe = foldMap observePath

observePath :: LoweredPath -> CurveReceipt
observePath lowered = foldMap observeSpan (loweredSpans lowered)
 where
  epsilon = positiveExactValue (loweredTolerance lowered)
  observeSpan spanValue = CurveReceipt 1
    (max (exactPointBitWidth (spanFrom spanValue)) (exactPointBitWidth (spanTo spanValue)))
    (max (exactRationalBitWidth (spanParameterFrom spanValue))
      (exactRationalBitWidth (spanParameterTo spanValue)))
    1 (spanSquaredBound spanValue >= 0 && spanSquaredBound spanValue <= epsilon * epsilon)

lowerSubpath :: LoweringPolicy -> Subpath -> Either LoweringError LoweredPath
lowerSubpath policy subpath = case subpath of
  OpenSubpath value -> lowerOpenTrail policy value
  ClosedSubpath value -> lowerClosedTrail policy value

inputBits :: Path -> Int
inputBits = foldr max 0 . fmap subpathBits . toList . pathSubpaths
 where
  subpathBits :: Subpath -> Int
  subpathBits subpath = foldr max 0
    (anchorBits subpath : fmap vectorBits (concatMap (toList . stepControlPoints) (stepsOf subpath)))
  anchorBits :: Subpath -> Int
  anchorBits (OpenSubpath value) = exactPointBitWidth (location value)
  anchorBits (ClosedSubpath value) = exactPointBitWidth (location value)
  vectorBits :: ExactVector -> Int
  vectorBits (ExactVector x y) = max (exactRationalBitWidth x) (exactRationalBitWidth y)

stepsOf :: Subpath -> [CurveStep]
stepsOf (OpenSubpath value) = toList (trailSteps (locatedValue value))
stepsOf (ClosedSubpath value) = toList (closedTrailSteps (locatedValue value))

metric :: CurveFamily -> Affine2
metric AnisotropicHorn = affine2 (ExactVector 12 2) (ExactVector 0 1) (ExactVector 0 0)
metric _ = identityAffine2

geometry :: CurveFamily -> Path
geometry HornSpans = closedPath horn
geometry ExactCircle = closedPath (ellipse (ExactVector 40 0) (ExactVector 0 40))
geometry SkewEllipse = closedPath (ellipse (ExactVector 70 15) (ExactVector (-12) 28))
geometry RepeatedMotifs = foldMap motif [0 .. 11]
geometry AnisotropicHorn = closedPath horn
geometry CuspsAndOvershoot = path (Seq.fromList (OpenSubpath . locate (exactPoint 0 0)
  . openTrail . Seq.singleton <$> pathologicalSteps))

closedPath :: Located ClosedTrail -> Path
closedPath = path . Seq.singleton . ClosedSubpath

-- Two Hermite rails share root and tip; changing a middle jet remains local.
horn :: Located ClosedTrail
horn = locate (exactPoint 0 0) (closeWith line (openTrail (Seq.fromList
  [ hermiteStep (ExactVector 28 (-50)) (ExactVector 15 (-65)) (ExactVector 40 (-30))
  , hermiteStep (ExactVector 22 (-30)) (ExactVector 40 (-30)) (ExactVector 8 (-35))
  , hermiteStep (ExactVector (-32) 35) (ExactVector (-45) 10) (ExactVector (-35) 25)
  , hermiteStep (ExactVector (-18) 45) (ExactVector (-35) 25) (ExactVector (-8) 60)
  ])))

motif :: Integer -> Path
motif index = transformPath placement
  (closedPath (ellipse (ExactVector 10 3) (ExactVector (-2) 4)))
 where
  n :: ExactRational
  n = fromInteger index
  placement = affine2 (ExactVector 1 n) (ExactVector 0 1) (ExactVector (25 * n) (3 * n))

pathologicalSteps :: [CurveStep]
pathologicalSteps =
  [ curveStep (cubic (ExactVector 60 80) (ExactVector (-60) 80)) (ExactVector 0 0)
  , curveStep (cubic (ExactVector 100 0) (ExactVector (-100) 0)) (ExactVector 1 0)
  , curveStep (cubic (ExactVector 1 80) (ExactVector (-1) (-80))) (ExactVector 2 0)
  ]
