module Main (main) where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Planar.Affine (affine2)
import qualified Moonlight.Planar.Curve as Curve
import Moonlight.Planar.Exact
import Moonlight.Planar.Exhibit.Geometry (ShapeError, solidObj, triangulateShape)
import Moonlight.Planar.Point (Point (..), PointValidationError, queryPointValue)
import System.Environment (getArgs)
import System.Exit (die)

type XZ = (Double, Double)
data Part = Part String Double (NonEmpty XZ) [XZ]

data KnightError = KnightGeometry ShapeError | KnightPoint PointValidationError
  | KnightScalar ScalarRefinementError | KnightArithmetic ExactArithmeticError
  deriving stock (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> either (die . show) (writeFile path) knight
    _ -> die "usage: moonlight-planar-knight-rig OUTPUT.obj"

knight :: Either KnightError String
knight = ("# Native Moonlight Planar CDT - layered Knight fan art\n" <>) . concat
  <$> (parts >>= traverse build)
 where
  build :: Part -> Either KnightError String
  build (Part name thickness outline seeds) =
    solidObj name (const thickness) <$> first KnightGeometry (triangulateShape (fmap point outline :| []) (fmap point seeds))
  point :: XZ -> Point
  point (x,z) = Point (snap (750*x)) (snap (-750*z))
  snap :: Double -> Double
  snap v = fromIntegral (round (v*65536) :: Integer) / 65536

-- A continuous mask-and-horn silhouette; the rigid mask is not an image card.
mask :: Either KnightError (NonEmpty XZ)
mask = ((0,0.98) :|) . concat <$> sequence
  [ curve (0,0.98) (-0.31,0.96) (-0.41,1.10) (-0.39,1.32)
  , curve (-0.39,1.32) (-0.40,1.53) (-0.49,1.72) (-0.38,1.88)
  , curve (-0.38,1.88) (-0.40,1.68) (-0.20,1.64) (-0.22,1.45)
  , curve (-0.22,1.45) (-0.12,1.40) (0.12,1.40) (0.22,1.45)
  , curve (0.22,1.45) (0.20,1.64) (0.40,1.68) (0.38,1.88)
  , curve (0.38,1.88) (0.49,1.72) (0.40,1.53) (0.39,1.32)
  , curve (0.39,1.32) (0.41,1.10) (0.31,0.96) (0,0.98)
  ]

-- Existing sixteen samples per authored cubic, evaluated by the curve owner.
curve :: XZ -> XZ -> XZ -> XZ -> Either KnightError [XZ]
curve a b c d = do
  start <- exactXZ a
  controlA <- exactXZ b
  controlB <- exactXZ c
  end <- exactXZ d
  let step = Curve.curveStep
        (Curve.cubic (exactVectorFromPoints start controlA) (exactVectorFromPoints start controlB))
        (exactVectorFromPoints start end)
  parameters <- traverse (sampleParameter 16)
    [i | i <- [1..16], i < 16 || d /= (0,0.98)]
  traverse (embeddedXZ . translateExactPoint start . (`Curve.evaluateStep` step)) parameters

-- The ellipse is the canonical four rational conics. Twelve parameter samples
-- per quarter retain 48 vertices; rational parameter spacing is not uniform
-- angular spacing. Trigonometry here specifies the tilted coordinate frame only.
ellipse :: XZ -> XZ -> Double -> Either KnightError (NonEmpty XZ)
ellipse (cx,cz) (rx,rz) tilt = do
  center <- exactXZ (cx,cz)
  axisX <- exactXZ (rx*cos tilt,rx*sin tilt)
  axisY <- exactXZ (-rz*sin tilt,rz*cos tilt)
  let origin = exactPoint 0 0
      placement = affine2 (ExactVector 1 0) (ExactVector 0 1) (exactVectorFromPoints origin center)
      outline = Curve.transformLocatedClosedTrail placement
        (Curve.ellipse (exactVectorFromPoints origin axisX) (exactVectorFromPoints origin axisY))
      start = Curve.location outline
      (_, locatedSteps) = mapAccumL locateStep start
        (toList (Curve.closedTrailSteps (Curve.locatedValue outline)))
      locateStep :: ExactPoint -> Curve.CurveStep -> (ExactPoint,(ExactPoint,Curve.CurveStep))
      locateStep point step = (translateExactPoint point (Curve.curveStepEnd step),(point,step))
  parameters <- traverse (sampleParameter 12) [0..11]
  let samples = concatMap (\(point,step) ->
        map (translateExactPoint point . (`Curve.evaluateStep` step)) parameters) locatedSteps
  traverse embeddedXZ (start :| drop 1 samples)

sampleParameter :: Integer -> Integer -> Either KnightError UnitInterval
sampleParameter count index = first KnightArithmetic (exactRational index count)
  >>= first KnightScalar . unitInterval

exactXZ :: XZ -> Either KnightError ExactPoint
exactXZ (x,z) = first KnightPoint (exactPointFromPoint (Point x z))

-- Binary64 projection is a transport candidate; the unchanged snapping and CDT
-- admission below remain responsible for the actual mesh embedding.
embeddedXZ :: ExactPoint -> Either KnightError XZ
embeddedXZ point = toXZ . queryPointValue <$> first KnightPoint (exactPointToEmbeddingCandidate point)
 where
  toXZ :: Point -> XZ
  toXZ (Point x z) = (x,z)

ellipticPart :: String -> Double -> XZ -> XZ -> Double -> Either KnightError Part
ellipticPart name thickness center axes tilt =
  (\outline -> Part name thickness outline []) <$> ellipse center axes tilt

strip :: (Double -> XZ) -> (Double -> Double) -> NonEmpty XZ
strip center width = side (-1) 0 :|
  ([side (-1) (fromIntegral i/24) | i <- [1..24 :: Int]]
   <> [side 1 (fromIntegral i/24) | i <- [24,23..0 :: Int]])
 where
  side :: Double -> Double -> XZ
  side sign t = let (x,z)=center t in (x+sign*width t,z)

limb :: String -> Double -> Double -> Double -> Double -> Part
limb name x top bottom width = Part name 0.026
  (strip (\t -> (x + 0.022*sin(pi*t),top+(bottom-top)*t)) (\t -> width*(1-0.28*t)))
  [(x+0.022*sin(pi*t),top+(bottom-top)*t) | i <- [1..23 :: Int], let t=fromIntegral i/24]

cloak :: Int -> Part
cloak i = Part ("Cloak_"<>show i) 0.014
  (strip center width) [center (fromIntegral j/24) | j<-[1..23 :: Int]]
 where
  axis :: Double
  axis=fromIntegral (i-2)
  center :: Double -> XZ
  center t = (axis*(0.064+0.102*t) + 0.008*sin(pi*t),1.025-(0.55+0.025*cos(axis*1.5))*t)
  width :: Double -> Double
  width t = (0.094+0.018*t)*(1-0.48*t^ (8::Int))

parts :: Either KnightError [Part]
parts = sequence $
  [ (\outline -> Part "Mask" 0.052 outline []) <$> mask
  , ellipticPart "Eye_L" 0.003 (-0.177,1.218) (0.072,0.103) (-0.12)
  , ellipticPart "Eye_R" 0.003 (0.177,1.218) (0.072,0.103) 0.12
  , ellipticPart "Body" 0.068 (0,0.665) (0.16,0.255) 0
  , ellipticPart "Collar" 0.018 (0,0.991) (0.205,0.045) (-0.06)
  , Right (limb "Leg_L" (-0.115) 0.57 0.075 0.047)
  , Right (limb "Leg_R" 0.115 0.57 0.075 0.047)
  , ellipticPart "Foot_L" 0.033 (-0.086,0.051) (0.079,0.033) 0
  , ellipticPart "Foot_R" 0.033 (0.144,0.051) (0.079,0.033) 0
  , Right (limb "Arm_L" (-0.23) 0.935 0.57 0.037)
  , Right (limb "Arm_R" 0.23 0.935 0.57 0.037)
  , ellipticPart "Hand_L" 0.027 (-0.23,0.565) (0.045,0.052) (-0.1)
  , ellipticPart "Hand_R" 0.027 (0.23,0.565) (0.045,0.052) 0.1
  ] <> fmap (Right . cloak) [0..4]
