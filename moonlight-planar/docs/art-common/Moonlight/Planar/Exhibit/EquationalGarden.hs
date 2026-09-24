-- | A moonlit botanical plate authored with profiles, interpolated stations,
-- bowed lines and reusable local frames. No illustration-owned curve handles.
module Moonlight.Planar.Exhibit.EquationalGarden
  ( GardenControls (..)
  , defaultGardenControls
  , GardenPart (..)
  , gardenPartName
  , gardenPicture
  , GardenAttachmentError (..)
  , GardenPlant (..)
  , StemPort (..), LeafPort (..), BlossomPort (..)
  , gardenPlants, gardenLayoutPicture
  , leafMotif
  , petalOutline
  , leafOutline
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Data.Bifunctor (first)
import Moonlight.Planar.Affine
  ( Affine2, AffineIso2, affine2, affineIso2, affineIsoMap, composeAffineIso2
  , identityAffineIso2, translationAffineIso2 )
import Moonlight.Planar.Curve
  ( ClosedTrail, Located, OpenTrail, Subpath (..), circle, ellipse, path
  , locate, openTrail, curveStep, line, transformLocatedClosedTrail )
import Moonlight.Planar.Curve.Authoring
  ( Knot (..), ProfileStation (..), bowedTrail, cardinalOpen, polygonTrail, profileOutline )
import Moonlight.Planar.Curve.Frame
  ( FrameError, FramePolicy, FrameSite (..), exactTrailSite, framePolicy
  , measuredFrameIso, regularFrame )
import Moonlight.Planar.Curve.Measure (RadicalPrecisionError, radicalPrecision)
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), PositiveExact, ScalarRefinementError
  , UnitInterval, exactHalf, exactThird, exactPoint, exactPointCoordinates, positiveExact
  , positiveOne, positiveTwo, unitHalf, unitOne, unitZero, unitIntervalValue
  , boundsMinimumX, boundsMaximumX, boundsMinimumY, boundsMaximumY )
import Moonlight.Planar.Illustration
  ( Color (..), FillRule (..), GradientStop (..), LineCap (..), LineJoin (..)
  , Paint (..), Picture, StrokeStyle (..), StrokeUnits (..)
  , annotate, clip, fill, gradientStops, opacity, place, stroke )
import Moonlight.Planar.Illustration.Motif
  ( Motif, motif, motifPicture, motifPort, attachMotif, transformMotif )
import Moonlight.Planar.Illustration.Layout
  ( LayoutAxis (..), alignMotif, arrangeMotifs, geometryEnvelope, geometryBounds )

-- | Ordinary semantic controls: opening changes petals; breeze bends stems
-- and leaves. The unit interval is admitted once by its existing scalar owner.
data GardenControls = GardenControls
  { blossomOpening :: !UnitInterval
  , gardenBreeze :: !UnitInterval
  } deriving stock (Eq, Show)

defaultGardenControls :: GardenControls
defaultGardenControls = GardenControls unitHalf unitHalf

data GardenPart = NightSky | Moon | Ground | Stems | Leaves | Blossoms | Ribbon | Fireflies
  deriving stock (Eq, Ord, Show)

gardenPartName :: GardenPart -> String
gardenPartName part = case part of
  NightSky -> "night-sky"
  Moon -> "pierced-moon"
  Ground -> "garden-ground"
  Stems -> "interpolated-stems"
  Leaves -> "profile-leaves"
  Blossoms -> "repeated-profile-blossoms"
  Ribbon -> "flowing-profile-ribbon"
  Fireflies -> "fireflies"

data Palette = Palette !Color !Color !Color

data Plant = Plant
  { plantRoot :: !ExactPoint
  , plantFlower :: !ExactPoint
  , plantScale :: !ExactRational
  , plantTilt :: !ExactRational
  , plantPalette :: !Palette
  }

pearl, blue, lilac, amber :: Palette
pearl = Palette (RGB 40 100 114) (RGB 178 219 207) (RGB 249 246 213)
blue = Palette (RGB 35 68 106) (RGB 115 173 194) (RGB 219 236 227)
lilac = Palette (RGB 68 61 104) (RGB 162 159 194) (RGB 233 219 219)
amber = Palette (RGB 106 85 77) (RGB 211 177 136) (RGB 250 233 181)

plants :: [Plant]
plants =
  [ Plant (exactPoint 305 975) (exactPoint 237 447) (exactHalf + exactThird * exactHalf) (-exactThird) lilac
  , Plant (exactPoint 938 960) (exactPoint 1014 548) (exactHalf + exactHalf ^ (3 :: Int)) exactThird blue
  , Plant (exactPoint 626 971) (exactPoint 596 386) (1 + exactThird * exactHalf) (-exactHalf * exactThird) pearl
  , Plant (exactPoint 429 981) (exactPoint 376 650) (1 - exactHalf ^ (3 :: Int)) exactThird blue
  , Plant (exactPoint 806 973) (exactPoint 849 724) (1 - exactHalf ^ (2 :: Int)) (-exactThird) amber
  ]

-- | One tapered profile is reused for broad petals, narrow leaves and light
-- ribbons. Stations express center/half-span, never Bezier control handles.
lanceolate :: ExactRational -> ExactRational -> ExactRational -> Located ClosedTrail
lanceolate height width bend = profileOutline unitHalf $
  ProfileStation (exactPoint 0 0) (ExactVector 0 0) :|
    [ ProfileStation (exactPoint (bend * exactHalf) (negate height * exactHalf ^ (2 :: Int)))
        (ExactVector (width * exactHalf) 0)
    , ProfileStation (exactPoint bend (negate height * (exactHalf + exactHalf ^ (3 :: Int))))
        (ExactVector width 0)
    , ProfileStation (exactPoint (bend * exactHalf) (negate height * (1 - exactHalf ^ (3 :: Int))))
        (ExactVector (width * exactHalf) 0)
    , ProfileStation (exactPoint 0 (negate height)) (ExactVector 0 0)
    ]

petalOutline :: UnitInterval -> Located ClosedTrail
petalOutline opening =
  lanceolate 142 (19 + 17 * unitIntervalValue opening) (8 * (1 - unitIntervalValue opening))

leafOutline :: UnitInterval -> Located ClosedTrail
leafOutline = leafContour 24

leafContour :: ExactRational -> UnitInterval -> Located ClosedTrail
leafContour width breeze = lanceolate 158 width (24 * (2 * unitIntervalValue breeze - 1))

data StemPort = LowerLeafSocket | UpperLeafSocket | BlossomSocket
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data LeafPort = LeafRoot
  deriving stock (Eq, Ord, Show)

data BlossomPort = BlossomRoot
  deriving stock (Eq, Ord, Show)

-- | An authored socket matrix that collapses, a stem site with no regular
-- tangent frame, or a refused frame policy scalar.
data GardenAttachmentError
  = SingularGardenSocket !ExactPoint !StemPort
  | GardenFrameRefused !ExactPoint !StemPort !FrameError
  | GardenFramePrecisionRefused !RadicalPrecisionError
  | GardenFrameToleranceRefused !ScalarRefinementError
  deriving stock (Eq, Show)

-- An authored heterogeneous assembly, not a generic connection graph. Each
-- child retains its ports until the final ordered painting projection. The
-- stem trail is the one curve the stem paints and its sockets read.
data GardenPlant = GardenPlant
  { gardenStemTrail :: !(Located OpenTrail)
  , gardenStem :: !(Motif StemPort GardenPart)
  , gardenLowerLeaf :: !(Motif LeafPort GardenPart)
  , gardenUpperLeaf :: !(Motif LeafPort GardenPart)
  , gardenBlossom :: !(Motif BlossomPort GardenPart)
  }

gardenPlants :: GardenControls -> Either GardenAttachmentError [GardenPlant]
gardenPlants controls = do
  policy <- stemFramePolicy
  traverse (assemble policy) plants
 where
  leaf :: Motif LeafPort GardenPart
  leaf = leafMotif 24 (gardenBreeze controls)
  assemble :: FramePolicy -> Plant -> Either GardenAttachmentError GardenPlant
  assemble policy plant = do
    let trail = stemTrail controls plant
    stem <- stemMotif policy trail plant
    pure (GardenPlant trail stem
      (attachMotif LeafRoot (motifPort stem LowerLeafSocket) leaf)
      (attachMotif LeafRoot (motifPort stem UpperLeafSocket) leaf)
      (attachMotif BlossomRoot (motifPort stem BlossomSocket)
        (blossomMotif controls (plantPalette plant))))

gardenPicture :: GardenControls -> Either GardenAttachmentError (Picture GardenPart)
gardenPicture controls = do
  assemblies <- gardenPlants controls
  pure $ annotate NightSky sky
    <> annotate Moon moon
    <> annotate Ground ground
    <> annotate Stems (foldMap (motifPicture . gardenStem) assemblies <> grass)
    <> annotate Leaves (foldMap (\plant -> motifPicture (gardenLowerLeaf plant)
      <> motifPicture (gardenUpperLeaf plant)) assemblies)
    <> annotate Blossoms (foldMap (motifPicture . gardenBlossom) assemblies)
    <> annotate Ribbon ribbon
    <> annotate Fireflies fireflies

-- | Breeze bends the stem by moving its interpolated knots. This authors the
-- knots only; sockets read the stem trail itself, never this formula.
stemStation :: GardenControls -> Plant -> ExactRational -> ExactPoint
stemStation controls plant t =
  let (rx, ry) = exactPointCoordinates (plantRoot plant)
      (fx, fy) = exactPointCoordinates (plantFlower plant)
      sway = 86 * (2 * unitIntervalValue (gardenBreeze controls) - 1)
   in exactPoint (rx + t * (fx - rx) + 4 * t * (1 - t) * sway) (ry + t * (fy - ry))

-- | The root, two leaf knots and the flower, interpolated once. Knot @k@ is
-- the start of step @k@; the flower is the trail's end.
stemTrail :: GardenControls -> Plant -> Located OpenTrail
stemTrail controls plant = cardinalOpen unitHalf $ Interpolating (plantRoot plant) :|
  (Interpolating . stemStation controls plant <$> [exactThird, 2 * exactThird, 1])

-- | Leaf frames are normalized to within 2^-20 of unit scale, so the authored
-- leaf size survives the tangent frame.
stemFramePolicy :: Either GardenAttachmentError FramePolicy
stemFramePolicy = framePolicy
  <$> first GardenFramePrecisionRefused (radicalPrecision 32)
  <*> first GardenFrameToleranceRefused (positiveExact (exactHalf ^ (20 :: Int)))

plantStem :: Located OpenTrail -> Picture GardenPart
plantStem stem = ink positiveTwo (RGB 26 73 72) stem <> ink positiveOne (RGB 100 148 116) stem

-- | Leaf sockets keep their authored knots and take the stem's tangent frame
-- there; side and size are an authored lean relative to that tangent. The
-- blossom socket keeps its authored tilt at the flower, the trail's end.
stemMotif
  :: FramePolicy -> Located OpenTrail -> Plant
  -> Either GardenAttachmentError (Motif StemPort GardenPart)
stemMotif policy stem plant = do
  lower <- leafSocket LowerLeafSocket 1 (-1) 1
  upper <- leafSocket UpperLeafSocket 2 1 (1 - exactThird)
  flower <- admit BlossomSocket $ affine2
    (ExactVector (plantScale plant) (plantTilt plant * plantScale plant * exactHalf))
    (ExactVector 0 (plantScale plant * (1 - exactHalf ^ (3 :: Int))))
    (pointVector (plantFlower plant))
  pure $ motif (plantStem stem) $ \port -> case port of
    LowerLeafSocket -> lower
    UpperLeafSocket -> upper
    BlossomSocket -> flower
 where
  admit :: StemPort -> Affine2 -> Either GardenAttachmentError AffineIso2
  admit port = maybe (Left (SingularGardenSocket (plantRoot plant) port)) Right . affineIso2
  -- The quarter-turn taking the leaf's forward axis, local -y, onto the
  -- frame's tangent, followed by the lean: on an upright stem this is the
  -- former world-axis socket matrix.
  leafSocket :: StemPort -> Int -> ExactRational -> ExactRational -> Either GardenAttachmentError AffineIso2
  leafSocket port knot side size = do
    let scale = size * (exactHalf + plantScale plant * exactHalf)
        refused = GardenFrameRefused (plantRoot plant) port
    lean <- admit port $ affine2
      (ExactVector (negate side * scale) (scale * exactHalf))
      (ExactVector (negate scale * exactHalf) (negate side * scale))
      (ExactVector 0 0)
    site <- first refused (exactTrailSite (OpenSubpath stem) knot unitZero)
    frame <- first refused (regularFrame policy (ExactSite site))
    pure (composeAffineIso2 (measuredFrameIso frame) lean)

leafMotif :: ExactRational -> UnitInterval -> Motif LeafPort GardenPart
leafMotif width breeze = motif picture (const identityAffineIso2)
 where
  outline :: Located ClosedTrail
  outline = leafContour width breeze
  vein :: Located OpenTrail
  vein = bowedTrail ((2 * unitIntervalValue breeze - 1) * exactHalf ^ (3 :: Int))
    (exactPoint 0 0) (exactPoint 0 (-158))
  picture :: Picture GardenPart
  picture = fill NonZero (gradient (exactPoint (negate width) 0) (exactPoint width (-138))
            (Palette (RGB 13 44 52) (RGB 48 104 96) (RGB 114 162 130))) (outline :| [])
          <> clip NonZero (outline :| [])
            (ink positiveOne (RGB 151 180 133) vein
              <> foldMap sideVein [1 .. 5 :: Int])
  sideVein :: Int -> Picture GardenPart
  sideVein index =
    let y = negate (22 + 20 * fromIntegral index)
     in opacity unitHalf $ foldMap
          (\side -> ink positiveOne (RGB 96 150 120)
            (bowedTrail (side * exactHalf ^ (3 :: Int)) (exactPoint 0 (y + 14)) (exactPoint (side * (width + 2)) y)))
          [-1, 1]

blossomMotif :: GardenControls -> Palette -> Motif BlossomPort GardenPart
blossomMotif controls palette = motif picture (const identityAffineIso2)
 where
  picture :: Picture GardenPart
  picture = opacity unitHalf (place (uniformAt 170 (exactPoint 0 8))
    (fill NonZero (RadialGradient (exactPoint 0 0) positiveOne
      (gradientStops (GradientStop unitZero (RGB 131 199 168) unitHalf :|
        [GradientStop unitOne (RGB 57 130 136) unitZero])))
      (circle positiveOne :| [])))
    <> petals (blossomOpening controls) palette

-- | The same leaves, with geometric bounds and real port axes. Widening the
-- middle specimen displaces its successor; there is no authored successor x.
gardenLayoutPicture :: UnitInterval -> Picture GardenPart
gardenLayoutPicture opening =
  sky <> annotate Leaves (foldMap (\value -> motifPicture value <> diagnostic value) specimens)
 where
  specimens :: Seq.Seq (Motif LeafPort GardenPart)
  specimens = arrangeMotifs Horizontal 75 $ fmap
    (alignMotif Vertical unitOne 640 . transformMotif (translationAffineIso2 (ExactVector 150 0))) $
    Seq.fromList [leafMotif 24 unitHalf, leafMotif (24 + 70 * unitIntervalValue opening) unitHalf,
      leafMotif 24 unitHalf]
  diagnostic :: Motif LeafPort GardenPart -> Picture GardenPart
  diagnostic value = portAxes (motifPort value LeafRoot) <>
    case geometryBounds (geometryEnvelope (motifPicture value)) of
      Nothing -> mempty
      Just bounds -> stroke (StrokeStyle (Solid (RGB 182 164 109)) positiveOne
          OutputUnits ButtCap BevelJoin) $ path $ Seq.singleton $ ClosedSubpath $
        polygonTrail (exactPoint (boundsMinimumX bounds) (boundsMinimumY bounds) :|
          [ exactPoint (boundsMaximumX bounds) (boundsMinimumY bounds)
          , exactPoint (boundsMaximumX bounds) (boundsMaximumY bounds)
          , exactPoint (boundsMinimumX bounds) (boundsMaximumY bounds) ])

portAxes :: AffineIso2 -> Picture GardenPart
portAxes frame = foldMap axis [ExactVector 16 0, ExactVector 0 (-16)]
 where
  axis :: ExactVector -> Picture GardenPart
  axis direction = place (affineIsoMap frame) $
    ink positiveTwo (RGB 239 157 120)
      (locate (exactPoint 0 0) (openTrail (Seq.singleton (curveStep line direction))))

-- Exact rational frames: cardinal quarter-turns and scaled diagonal rotations.
-- One petal value supplies every spoke and both differently sized whorls.
spokes :: [(ExactRational, ExactRational)]
spokes =
  [(0,-1),(diagonal,-diagonal),(1,0),(diagonal,diagonal)
  ,(0,1),(-diagonal,diagonal),(-1,0),(-diagonal,-diagonal)]
 where
  diagonal :: ExactRational
  diagonal = 3 * exactHalf ^ (2 :: Int)

petals :: UnitInterval -> Palette -> Picture GardenPart
petals opening palette =
  whorl
    <> place (affine2 (ExactVector exactHalf exactHalf) (ExactVector (-exactHalf) exactHalf) (ExactVector 0 0))
      whorl
    <> flowerHeart
 where
  whorl :: Picture GardenPart
  whorl = foldMap (\direction -> place (spokeFrame direction) petal) spokes
  outline :: Located ClosedTrail
  outline = petalOutline opening
  petal :: Picture GardenPart
  petal = fill NonZero (gradient (exactPoint 0 6) (exactPoint 0 (-145)) palette) (outline :| [])
    <> clip NonZero (outline :| [])
      (opacity unitHalf
        (fill NonZero (Solid (RGB 238 243 216)) (lanceolate 138 3 4 :| []))
        <> ink positiveOne (RGB 137 184 170)
          (bowedTrail (exactHalf ^ (5 :: Int)) (exactPoint 0 0) (exactPoint 0 (-136))))
    <> stroke (StrokeStyle (Solid (RGB 172 207 189)) positiveOne OutputUnits RoundCap RoundJoin)
      (path (Seq.singleton (ClosedSubpath outline)))

flowerHeart :: Picture GardenPart
flowerHeart =
  fill NonZero (gradient (exactPoint (-20) (-20)) (exactPoint 20 20)
    (Palette (RGB 234 209 141) (RGB 159 158 87) (RGB 69 108 92)))
    (ellipse (ExactVector 25 0) (ExactVector 0 21) :| [])
    <> foldMap pollen spokes
    <> fill NonZero (Solid (RGB 230 223 161)) (circle positiveTwo :| [])
 where
  pollen :: (ExactRational, ExactRational) -> Picture GardenPart
  pollen (x, y) = place (uniformAt 2 (exactPoint (15*x) (12*y))) $
    fill NonZero (Solid (RGB 245 225 153)) (circle positiveOne :| [])

sky :: Picture GardenPart
sky = fill NonZero (gradient (exactPoint 150 0) (exactPoint 1050 1050)
    (Palette (RGB 5 18 31) (RGB 19 46 58) (RGB 7 22 34)))
    (polygonTrail (exactPoint 0 0 :| [exactPoint 1200 0, exactPoint 1200 1000, exactPoint 0 1000]) :| [])
  <> place (affine2 (ExactVector 550 0) (ExactVector 0 410) (ExactVector 830 310))
    (fill NonZero (RadialGradient (exactPoint 0 0) positiveOne
      (gradientStops (GradientStop unitZero (RGB 61 99 103) unitHalf :|
        [GradientStop unitOne (RGB 23 58 75) unitZero]))) (circle positiveOne :| []))
  <> foldMap star [0 .. 71 :: Int]
 where
  star :: Int -> Picture GardenPart
  star index =
    let x = 48 + fromIntegral ((index * 173 + 41) `mod` 1100)
        y = 44 + fromIntegral ((index * 97 + 17) `mod` 640)
        size = exactHalf + fromIntegral (index `mod` 3) * exactHalf
     in opacity unitHalf $ place (uniformAt size (exactPoint x y)) $
          fill NonZero (Solid (RGB 182 207 189)) (spark :| [])

moon :: Picture GardenPart
moon = place (uniformAt 152 (exactPoint 943 219)) $
  let outer = circle positiveOne
      hole = transformLocatedClosedTrail
        (uniformAt 1 (exactPoint exactHalf (-exactThird))) outer
      -- Clip the overlapping even-odd circles to the outer disc: the cutout
      -- is transparent and cannot create a second lobe outside the moon.
      moonDisc :: Picture GardenPart
      moonDisc = fill EvenOdd (gradient (exactPoint (-1) (-1)) (exactPoint 1 1)
        (Palette (RGB 225 231 195) (RGB 150 183 168) (RGB 74 123 128))) (outer :| [hole])
   in clip NonZero (outer :| []) moonDisc

ground :: Picture GardenPart
ground = place (uniformAt 1 (exactPoint 614 962)) $
  fill NonZero (gradient (exactPoint 0 (-75)) (exactPoint 0 38)
    (Palette (RGB 27 64 65) (RGB 13 39 48) (RGB 5 20 31)))
    (ellipse (ExactVector 598 0) (ExactVector 0 83) :| [])

grass :: Picture GardenPart
grass = opacity unitHalf $ foldMap blade [0 .. 17 :: Int]
 where
  blade :: Int -> Picture GardenPart
  blade index =
    let x = 150 + fromIntegral (index * 53)
        height = 90 + fromIntegral ((index * 71) `mod` 135)
        lean = fromIntegral ((index * 31) `mod` 90) - 45
     in ink positiveOne (RGB 76 124 111)
          (bowedTrail (lean * exactHalf ^ (8 :: Int)) (exactPoint x 976) (exactPoint (x+lean) (976-height)))

ribbon :: Picture GardenPart
ribbon =
  let stations = ProfileStation (exactPoint 205 904) (ExactVector 0 0) :|
        [ProfileStation (exactPoint 380 925) (ExactVector (-4) 15)
        ,ProfileStation (exactPoint 605 871) (ExactVector 0 21)
        ,ProfileStation (exactPoint 829 895) (ExactVector 3 16)
        ,ProfileStation (exactPoint 1023 852) (ExactVector 0 0)]
      outline = profileOutline unitHalf stations
   in fill NonZero (gradient (exactPoint 230 940) (exactPoint 1000 835)
        (Palette (RGB 61 88 87) (RGB 147 168 136) (RGB 51 102 99))) (outline :| [])
      <> clip NonZero (outline :| [])
        (ink positiveOne (RGB 211 211 158)
          (cardinalOpen unitHalf (Interpolating (exactPoint 205 904) :|
            (Interpolating <$> [exactPoint 380 930,exactPoint 605 880,exactPoint 829 899,exactPoint 1023 852]))))

fireflies :: Picture GardenPart
fireflies = foldMap glow [(142,685),(525,195),(735,576),(1101,753),(618,754),(279,816)]
 where
  glow :: (ExactRational, ExactRational) -> Picture GardenPart
  glow (x,y) = place (uniformAt 1 (exactPoint x y)) $
    place (uniformAt 15 (exactPoint 0 0))
      (fill NonZero (RadialGradient (exactPoint 0 0) positiveOne
        (gradientStops (GradientStop unitZero (RGB 231 224 143) unitHalf :|
          [GradientStop unitOne (RGB 163 198 157) unitZero]))) (circle positiveOne :| []))
      <> fill NonZero (Solid (RGB 250 239 174)) (spark :| [])

spark :: Located ClosedTrail
spark = polygonTrail (exactPoint 0 (-3) :|
  [exactPoint exactHalf (-exactHalf),exactPoint 3 0,exactPoint exactHalf exactHalf
  ,exactPoint 0 3,exactPoint (-exactHalf) exactHalf,exactPoint (-3) 0,exactPoint (-exactHalf) (-exactHalf)])

-- These local helpers are shared painting/placement expressions within this
-- artwork, not another public rendering or geometry layer.
gradient :: ExactPoint -> ExactPoint -> Palette -> Paint
gradient a b (Palette dark middle light) = LinearGradient a b $
  gradientStops (GradientStop unitZero dark unitOne :|
    [GradientStop unitHalf middle unitOne, GradientStop unitOne light unitOne])

ink :: PositiveExact -> Color -> Located OpenTrail -> Picture GardenPart
ink width color curve = stroke (StrokeStyle (Solid color) width OutputUnits RoundCap RoundJoin)
  (path (Seq.singleton (OpenSubpath curve)))

uniformAt :: ExactRational -> ExactPoint -> Affine2
uniformAt scale center = affine2 (ExactVector scale 0) (ExactVector 0 scale) (pointVector center)

spokeFrame :: (ExactRational, ExactRational) -> Affine2
spokeFrame (x,y) = affine2 (ExactVector (negate y) x) (ExactVector (negate x) (negate y)) (ExactVector 0 0)

pointVector :: ExactPoint -> ExactVector
pointVector = uncurry ExactVector . exactPointCoordinates
