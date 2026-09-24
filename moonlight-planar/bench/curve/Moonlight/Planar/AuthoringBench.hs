-- | Matched attachment fixture. Admission is outside the timed action;
-- ordered world curves and every requested port are fully forced inside it.
module Moonlight.Planar.AuthoringBench (benchmarks) where

import BenchMeasure (timedProjection)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine
import Moonlight.Planar.Curve
import Moonlight.Planar.Exact
import Moonlight.Planar.Illustration
import Moonlight.Planar.Illustration.Motif
import Moonlight.Planar.Illustration.Layout

data Port = Root | Tip

type Observation = ([Path], [AffineIso2])

type Instance = Motif Port ()

benchmarks :: IO ()
benchmarks = do
  source <- admitted (affine2 (ExactVector 2 1) (ExactVector 1 2) (ExactVector 7 (-3)))
  tip <- admitted (affine2 (ExactVector 1 0) (ExactVector 1 1) (ExactVector 8 12))
  targets <- traverse (admitted . target) [1 .. 4096]
  inputs <- evaluate (force (source, tip, targets))
  instances <- timedProjection "attachment-authoring-n4096" observe
    (evaluate (assemble inputs))
  let (curves, ports) = observe instances
      checksum = sum (pathChecksum <$> curves) + sum (frameChecksum <$> ports)
  putStrLn ("attachment-authoring-receipt: instances=4096 paths=" <> show (length curves)
    <> " ports=" <> show (length ports) <> " exact-checksum=" <> show checksum)
  putStrLn "attachment-authoring-contract: admitted-inputs-outside; full-world-path-and-port-NF-inside"
  traverse_ (benchmarkLayout source tip) [1024,2048]

admitted :: Affine2 -> IO AffineIso2
admitted = maybe (fail "benchmark frame is singular") pure . affineIso2

target :: Integer -> Affine2
target index =
  let n = fromInteger index
   in affine2 (ExactVector 1 (n * exactThird)) (ExactVector 0 2) (ExactVector (25 * n) (3 * n))

assemble :: (AffineIso2, AffineIso2, [AffineIso2]) -> [Instance]
assemble (source, tip, targets) = fmap instantiate targets
 where
  ports :: Port -> AffineIso2
  ports Root = source
  ports Tip = tip
  instantiate :: AffineIso2 -> Instance
  instantiate destination = attachMotif Root destination (motif artwork ports)
{-# NOINLINE assemble #-}

artwork :: Picture ()
artwork = fill NonZero (Solid (RGB 80 150 110))
  (ellipse (ExactVector 8 2) (ExactVector (-1) 12) :| [])

observe :: [Instance] -> Observation
observe instances =
  (concatMap (worldPaths . motifPicture) instances, concatMap (\value -> fmap (motifPort value) [Root,Tip]) instances)

-- New capability costs, not comparisons with a reference envelope/layout engine.
benchmarkLayout :: AffineIso2 -> AffineIso2 -> Int -> IO ()
benchmarkLayout source tip count = do
  targets <- traverse (admitted . target) [1 .. fromIntegral count]
  inputs <- evaluate (force (source, tip, targets))
  let instances = assemble inputs
  _ <- evaluate (force (observe instances))
  envelope <- timedProjection ("geometry-envelope-n" <> show count) observeEnvelope
    (evaluate (foldMap (geometryEnvelope . motifPicture) instances))
  putStrLn ("geometry-envelope-receipt: instances=" <> show count <> " observations=" <> show (observeEnvelope envelope))
  arranged <- timedProjection ("motif-layout-n" <> show count) (observe . toList)
    (evaluate (arrangeMotifs Horizontal 3 (Seq.fromList instances)))
  let (curves,ports) = observe (toList arranged)
  putStrLn ("motif-layout-receipt: instances=" <> show count
    <> " paths=" <> show (length curves) <> " ports=" <> show (length ports)
    <> " exact-checksum=" <> show (sum (pathChecksum <$> curves) + sum (frameChecksum <$> ports)))
  putStrLn "envelope-layout-contract: new-capability; prepared-inputs-outside; envelope-support-or-full-world-path-and-port-NF-inside"

observeEnvelope
  :: GeometryEnvelope
  -> (Maybe (ExactRational, ExactRational, ExactRational, ExactRational), [Maybe ExactRational])
observeEnvelope envelope =
  (fmap (\bounds -> (boundsMinimumX bounds, boundsMinimumY bounds, boundsMaximumX bounds, boundsMaximumY bounds))
    (geometryBounds envelope), fmap (`geometrySupport` envelope)
    [ExactVector 1 0, ExactVector 0 1, ExactVector (-1) 0, ExactVector 0 (-1), ExactVector 2 3])

worldPaths :: Picture part -> [Path]
worldPaths picture = foldPicture algebra picture identityAffine2
 where
  algebra :: PictureAlgebra part (Affine2 -> [Path])
  algebra = PictureAlgebra
    { paintSequence = \children frame -> concatMap ($ frame) (toList children)
    , paintFill = \_ _ contours frame -> [transformPath frame (path (Seq.fromList (ClosedSubpath <$> toList contours)))]
    , paintStroke = \_ curve frame -> [transformPath frame curve]
    , paintClip = \_ contours child frame ->
        transformPath frame (path (Seq.fromList (ClosedSubpath <$> toList contours))) : child frame
    , paintPlace = \placement child frame -> child (composeAffine2 frame placement)
    , paintOpacity = \_ child -> child
    , paintAnnotation = \_ child -> child
    }

pathChecksum :: Path -> ExactRational
pathChecksum = sum . fmap subpathChecksum . toList . pathSubpaths
 where
  subpathChecksum :: Subpath -> ExactRational
  subpathChecksum (OpenSubpath value) = trailChecksum (location value) (toList (trailSteps (locatedValue value)))
  subpathChecksum (ClosedSubpath value) = trailChecksum (location value) (toList (closedTrailSteps (locatedValue value)))
  trailChecksum :: ExactPoint -> [CurveStep] -> ExactRational
  trailChecksum origin steps =
    let (x,y) = exactPointCoordinates origin
     in x + y + sum (fmap (sum . fmap vectorChecksum . toList . stepControlPoints) steps)

frameChecksum :: AffineIso2 -> ExactRational
frameChecksum frame =
  let (x,y,offset) = affineColumns (affineIsoMap frame)
   in vectorChecksum x + vectorChecksum y + vectorChecksum offset

vectorChecksum :: ExactVector -> ExactRational
vectorChecksum (ExactVector x y) = x + y
