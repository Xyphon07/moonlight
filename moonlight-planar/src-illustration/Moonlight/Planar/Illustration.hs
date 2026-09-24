{-# LANGUAGE DeriveFunctor #-}

-- | Ordered painting of canonical curves. Parts remain ordinary author-owned
-- values; annotations neither resolve references nor change geometry.
module Moonlight.Planar.Illustration
  ( Picture
  , Color (..)
  , Paint (..)
  , GradientStop (..)
  , GradientStops
  , gradientStops
  , gradientStopValues
  , FillRule (..)
  , StrokeUnits (..)
  , LineCap (..)
  , LineJoin (..)
  , MiterLimit
  , miterLimit
  , miterLimitValue
  , miterLimitFour
  , IllustrationError (..)
  , StrokeStyle (..)
  , fill
  , stroke
  , clip
  , place
  , opacity
  , annotate
  , PictureAlgebra (..)
  , foldPicture
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import Moonlight.Planar.Affine (Affine2)
import Moonlight.Planar.Curve (ClosedTrail, Located, Path)
import Moonlight.Planar.Exact (ExactPoint, ExactRational, PositiveExact, UnitInterval)

data Color = RGB !Word8 !Word8 !Word8
  deriving stock (Eq, Ord, Show)

-- | Gradient positions use the same local coordinates as their painted path.
-- Opacity is explicit, rather than encoded in a color string.
data GradientStop = GradientStop !UnitInterval !Color !UnitInterval
  deriving stock (Eq, Show)

newtype GradientStops = GradientStops (NonEmpty GradientStop)
  deriving stock (Eq, Show)

-- | One-time authoring canonicalization: increasing offsets, with stable
-- author order at coincident offsets (which express a discontinuous edge).
-- Browser-specific clamping of descending offsets is never needed.
gradientStops :: NonEmpty GradientStop -> GradientStops
gradientStops = GradientStops . NonEmpty.sortWith (\(GradientStop offset _ _) -> offset)

gradientStopValues :: GradientStops -> NonEmpty GradientStop
gradientStopValues (GradientStops stops) = stops

data Paint
  = Solid !Color
  | LinearGradient !ExactPoint !ExactPoint !GradientStops
  | RadialGradient !ExactPoint !PositiveExact !GradientStops
  deriving stock (Eq, Show)

data FillRule = NonZero | EvenOdd
  deriving stock (Eq, Ord, Show)

data StrokeUnits = LocalUnits | OutputUnits
  deriving stock (Eq, Ord, Show)

data LineCap = ButtCap | RoundCap | SquareCap
  deriving stock (Eq, Ord, Show)

newtype MiterLimit = MiterLimit ExactRational
  deriving stock (Eq, Show)

data IllustrationError = InvalidMiterLimit !ExactRational
  deriving stock (Eq, Show)

-- | SVG's miter ratio has a lower bound of one, not merely positivity.
miterLimit :: ExactRational -> Either IllustrationError MiterLimit
miterLimit value
  | value < 1 = Left (InvalidMiterLimit value)
  | otherwise = Right (MiterLimit value)

miterLimitValue :: MiterLimit -> ExactRational
miterLimitValue (MiterLimit value) = value

miterLimitFour :: MiterLimit
miterLimitFour = MiterLimit 4

data LineJoin = MiterJoin !MiterLimit | RoundJoin | BevelJoin
  deriving stock (Eq, Show)

data StrokeStyle = StrokeStyle
  { strokePaint :: !Paint
  , strokeWidth :: !PositiveExact
  , strokeUnits :: !StrokeUnits
  , strokeCap :: !LineCap
  , strokeJoin :: !LineJoin
  }
  deriving stock (Eq, Show)

-- The sequence, rather than a binary composition tree, makes the monoid's
-- associativity structural and gives publication stable sibling addresses.
newtype Picture part = Picture (Seq (Mark part))
  deriving stock (Eq, Show, Functor)

data Mark part
  = Filled !FillRule !Paint !(NonEmpty (Located ClosedTrail))
  | Stroked !StrokeStyle !Path
  | Clipped !FillRule !(NonEmpty (Located ClosedTrail)) !(Picture part)
  | Placed !Affine2 !(Picture part)
  | Transparent !UnitInterval !(Picture part)
  | Annotated !part !(Picture part)
  deriving stock (Eq, Show, Functor)

instance Semigroup (Picture part) where
  Picture behind <> Picture inFront = Picture (behind <> inFront)

instance Monoid (Picture part) where
  mempty = Picture Seq.empty

fill :: FillRule -> Paint -> NonEmpty (Located ClosedTrail) -> Picture part
fill rule paint contours = Picture (Seq.singleton (Filled rule paint contours))

stroke :: StrokeStyle -> Path -> Picture part
stroke style curve = Picture (Seq.singleton (Stroked style curve))

clip :: FillRule -> NonEmpty (Located ClosedTrail) -> Picture part -> Picture part
clip rule contours picture = Picture (Seq.singleton (Clipped rule contours picture))

place :: Affine2 -> Picture part -> Picture part
place affine picture = Picture (Seq.singleton (Placed affine picture))

-- | Opacity applies once to the composite, not independently to its children.
opacity :: UnitInterval -> Picture part -> Picture part
opacity amount picture = Picture (Seq.singleton (Transparent amount picture))

annotate :: part -> Picture part -> Picture part
annotate part picture = Picture (Seq.singleton (Annotated part picture))

-- | The interpreter boundary exposes painting operations, never SVG syntax or
-- a second geometry AST. A carrier may be a pure environment-reading function.
data PictureAlgebra part a = PictureAlgebra
  { paintSequence :: Seq a -> a
  , paintFill :: FillRule -> Paint -> NonEmpty (Located ClosedTrail) -> a
  , paintStroke :: StrokeStyle -> Path -> a
  , paintClip :: FillRule -> NonEmpty (Located ClosedTrail) -> a -> a
  , paintPlace :: Affine2 -> a -> a
  , paintOpacity :: UnitInterval -> a -> a
  , paintAnnotation :: part -> a -> a
  }

foldPicture :: PictureAlgebra part a -> Picture part -> a
foldPicture algebra = fold
 where
  fold (Picture marks) = paintSequence algebra (interpret <$> marks)
  interpret mark = case mark of
    Filled rule paint contours -> paintFill algebra rule paint contours
    Stroked style curve -> paintStroke algebra style curve
    Clipped rule contours picture -> paintClip algebra rule contours (fold picture)
    Placed affine picture -> paintPlace algebra affine (fold picture)
    Transparent amount picture -> paintOpacity algebra amount (fold picture)
    Annotated part picture -> paintAnnotation algebra part (fold picture)
