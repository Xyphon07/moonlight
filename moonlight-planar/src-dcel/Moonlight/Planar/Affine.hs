-- | Exact planar affine actions. General maps may collapse authored geometry;
-- only the non-singular refinement transports admitted polygon topology.
module Moonlight.Planar.Affine
  ( Affine2
  , affine2
  , affineColumns
  , identityAffine2
  , composeAffine2
  , transformPoint
  , transformVector
  , affineDeterminant
  , AffineIso2
  , affineIso2
  , affineIsoMap
  , identityAffineIso2
  , translationAffineIso2
  , composeAffineIso2
  , inverseAffineIso2
  , transformExactLoop
  , transformPolygonComponent
  , transformPlanarRegion
  , transformConvexPolygon
  ) where

import Control.DeepSeq (NFData (..))
import Data.List (sort)
import qualified Data.List.NonEmpty as NonEmpty
import Moonlight.Planar.Exact
  ( ExactPoint
  , ExactRational
  , ExactVector (..)
  , PositiveExact
  , addExactVectors
  , exactCross
  , exactPoint
  , exactPointCoordinates
  , divideByPositive
  , multiplyPositive
  , positiveExact
  , positiveOne
  , ratioPositive
  , translateExactPoint
  )
import Moonlight.Planar.Internal.BoundaryCycle (rotateCycleLeast)
import Moonlight.Planar.Internal.Region.Types
  ( ConvexPolygon (..)
  , ExactLoop (..)
  , PlanarRegion (..)
  , PolygonComponent (..)
  )

-- | Two linear columns followed by translation. No invertibility is assumed.
data Affine2 = Affine2 !ExactVector !ExactVector !ExactVector
  deriving stock (Eq, Ord, Show)

instance NFData Affine2 where
  rnf (Affine2 x y offset) = rnf x `seq` rnf y `seq` rnf offset

affine2 :: ExactVector -> ExactVector -> ExactVector -> Affine2
affine2 = Affine2

affineColumns :: Affine2 -> (ExactVector, ExactVector, ExactVector)
affineColumns (Affine2 x y offset) = (x, y, offset)

identityAffine2 :: Affine2
identityAffine2 = Affine2 (ExactVector 1 0) (ExactVector 0 1) (ExactVector 0 0)

-- | @composeAffine2 outer inner@ acts as @outer . inner@.
composeAffine2 :: Affine2 -> Affine2 -> Affine2
composeAffine2 outer@(Affine2 _ _ offset) (Affine2 x y innerOffset) =
  Affine2
    (transformVector outer x)
    (transformVector outer y)
    (addExactVectors (transformVector outer innerOffset) offset)

-- | Translation does not act on vectors.
transformVector :: Affine2 -> ExactVector -> ExactVector
transformVector (Affine2 (ExactVector a b) (ExactVector c d) _) (ExactVector x y) =
  ExactVector (a * x + c * y) (b * x + d * y)

transformPoint :: Affine2 -> ExactPoint -> ExactPoint
transformPoint transform@(Affine2 _ _ offset) point =
  let (x, y) = exactPointCoordinates point
      ExactVector mappedX mappedY = transformVector transform (ExactVector x y)
   in translateExactPoint (exactPoint mappedX mappedY) offset

affineDeterminant :: Affine2 -> ExactRational
affineDeterminant (Affine2 x y _) = exactCross x y

data Orientation = Preserving | Reversing
  deriving stock (Eq, Ord, Show)

-- | A nonzero determinant, including its sign and positive magnitude, admitted
-- once. Inversion consumes the same divisor witness without readmission.
data AffineIso2 = AffineIso2 !Affine2 !Orientation !PositiveExact
  deriving stock (Eq, Ord, Show)

instance NFData AffineIso2 where
  rnf (AffineIso2 transform orientation magnitude) =
    rnf transform `seq` orientation `seq` rnf magnitude

affineIso2 :: Affine2 -> Maybe AffineIso2
affineIso2 transform =
  let determinant = affineDeterminant transform
      orientation = if determinant < 0 then Reversing else Preserving
   in case positiveExact (abs determinant) of
        Left _ -> Nothing
        Right magnitude -> Just (AffineIso2 transform orientation magnitude)

affineIsoMap :: AffineIso2 -> Affine2
affineIsoMap (AffineIso2 transform _ _) = transform

identityAffineIso2 :: AffineIso2
identityAffineIso2 = AffineIso2 identityAffine2 Preserving positiveOne

-- | Translation preserves the identity linear frame. Its determinant is one
-- by construction, so no dynamic admission is required for layout or ports.
translationAffineIso2 :: ExactVector -> AffineIso2
translationAffineIso2 offset = AffineIso2
  (Affine2 (ExactVector 1 0) (ExactVector 0 1) offset) Preserving positiveOne

-- | Composition preserves invertibility; orientation signs multiply.
composeAffineIso2 :: AffineIso2 -> AffineIso2 -> AffineIso2
composeAffineIso2 (AffineIso2 outer outerOrientation outerMagnitude) (AffineIso2 inner innerOrientation innerMagnitude) =
  AffineIso2
    (composeAffine2 outer inner)
    (composeOrientation outerOrientation innerOrientation)
    (multiplyPositive outerMagnitude innerMagnitude)
 where
  composeOrientation :: Orientation -> Orientation -> Orientation
  composeOrientation Preserving orientation = orientation
  composeOrientation Reversing Preserving = Reversing
  composeOrientation Reversing Reversing = Preserving

-- | Exact inverse using the admitted determinant magnitude as divisor. The
-- inverse has the same orientation and reciprocal determinant magnitude.
inverseAffineIso2 :: AffineIso2 -> AffineIso2
inverseAffineIso2
  (AffineIso2 (Affine2 (ExactVector a b) (ExactVector c d) (ExactVector x y)) orientation magnitude) =
    AffineIso2
      (Affine2
        (ExactVector (divide d) (divide (negate b)))
        (ExactVector (divide (negate c)) (divide a))
        (ExactVector (divide (c * y - d * x)) (divide (b * x - a * y))))
      orientation
      (ratioPositive positiveOne magnitude)
 where
  divide :: ExactRational -> ExactRational
  divide numerator = divideByPositive (orient numerator) magnitude
  orient :: ExactRational -> ExactRational
  orient = case orientation of
    Preserving -> id
    Reversing -> negate

-- | Preserve the source winding and canonical least starting vertex. This
-- polygonal action intentionally differs from traversal-preserving curve maps.
transformExactLoop :: AffineIso2 -> ExactLoop -> ExactLoop
transformExactLoop (AffineIso2 transform orientation _) (ExactLoop points) =
  ExactLoop (rotateCycleLeast (orient (fmap (transformPoint transform) points)))
 where
  orient :: NonEmpty.NonEmpty ExactPoint -> NonEmpty.NonEmpty ExactPoint
  orient = case orientation of
    Preserving -> id
    Reversing -> NonEmpty.reverse

-- | Invertibility preserves boundary separation and hole containment.
transformPolygonComponent :: AffineIso2 -> PolygonComponent -> PolygonComponent
transformPolygonComponent transform (PolygonComponent outer holes) =
  PolygonComponent
    (transformExactLoop transform outer)
    (sort (fmap (transformExactLoop transform) holes))

-- | Disjoint interiors remain disjoint; transformed canonical order may differ.
transformPlanarRegion :: AffineIso2 -> PlanarRegion -> PlanarRegion
transformPlanarRegion transform (PlanarRegion components) =
  PlanarRegion (sort (fmap (transformPolygonComponent transform) components))

-- | Strict convexity survives every non-singular affine map.
transformConvexPolygon :: AffineIso2 -> ConvexPolygon -> ConvexPolygon
transformConvexPolygon transform (ConvexPolygon loop) =
  ConvexPolygon (transformExactLoop transform loop)
