-- | Artistic intent lives here; curves, paint and SVG remain library-owned.
module Moonlight.Planar.Exhibit.IllustrationStudy
  ( StudyControls, StudyControl (..), StudyControlError (..), defaultControls
  , editControl, StudyPart (..), partName, partContour, partPort
  , studyPicture, diagnosticPicture
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine
  ( AffineIso2, affine2, affineIsoMap, translationAffineIso2, identityAffineIso2 )
import Moonlight.Planar.Curve
  ( ClosedTrail, Located, OpenTrail, Subpath (..), ellipse, location, path, transformLocatedClosedTrail )
import Moonlight.Planar.Curve.Authoring
  ( Knot (Interpolating), ProfileStation (..), bowedTrail, cardinalClosed, cardinalOpen
  , polygonTrail, profileOutline )
import Moonlight.Planar.Exact
import Moonlight.Planar.Illustration
import Moonlight.Planar.Illustration.Motif
  ( Motif, motif, motifPicture, motifPort, attachMotif )

-- | Values are physical drawing units, not anonymous control-point indices.
data StudyControls = StudyControls
  { hornSweep :: !ExactRational
  , eyeSpacing :: !ExactRational
  , eyeTilt :: !ExactRational
  , cloakFullness :: !ExactRational
  , fullerWidth :: !ExactRational
  } deriving stock (Eq, Show)

data StudyControl = LeftHornSweep | EyeSpacing | EyeTilt | CloakFullness | FullerWidth
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data StudyControlError = ControlOutsideRange !StudyControl !ExactRational
  deriving stock (Eq, Show)

defaultControls :: StudyControls
defaultControls = StudyControls 0 57 7 0 12

-- | Absolute semantic edits. Bounds keep the authored family intentional.
editControl :: StudyControl -> ExactRational -> StudyControls -> Either StudyControlError StudyControls
editControl control value controls
  | value < low || value > high = Left (ControlOutsideRange control value)
  | otherwise = Right $ case control of
      LeftHornSweep -> controls { hornSweep = value }
      EyeSpacing -> controls { eyeSpacing = value }
      EyeTilt -> controls { eyeTilt = value }
      CloakFullness -> controls { cloakFullness = value }
      FullerWidth -> controls { fullerWidth = value }
 where
  (low, high) = case control of
    LeftHornSweep -> (-45, 45)
    EyeSpacing -> (40, 75)
    EyeTilt -> (-15, 15)
    CloakFullness -> (-25, 55)
    FullerWidth -> (5, 24)

data StudyPart = LeftHorn | RightHorn | Mask | LeftEye | RightEye | Cloak
  | LeftFoot | RightFoot | Blade | Fuller | Guard | Grip
  deriving stock (Eq, Ord, Show, Enum, Bounded)

partName :: StudyPart -> String
partName part = case part of
  LeftHorn -> "knight-left-horn"
  RightHorn -> "knight-right-horn"
  Mask -> "knight-mask"
  LeftEye -> "knight-left-eye"
  RightEye -> "knight-right-eye"
  Cloak -> "knight-cloak"
  LeftFoot -> "knight-left-foot"
  RightFoot -> "knight-right-foot"
  Blade -> "crescent-blade"
  Fuller -> "crescent-fuller"
  Guard -> "crescent-guard"
  Grip -> "crescent-grip"

-- | Geometry and its semantic root are authored together. Roots need not be
-- the first boundary point (notably horns and ellipses).
data PartGeometry
  = WorldContour !(Located ClosedTrail)
  | AttachedContour !(Located ClosedTrail) !ExactVector

data RootPort = RootPort

partPort :: StudyControls -> StudyPart -> AffineIso2
partPort controls part = motifPort (partMotif controls part (Solid ink)) RootPort

partMotif :: StudyControls -> StudyPart -> Paint -> Motif RootPort StudyPart
partMotif controls part paint = case partGeometry controls part of
  WorldContour contour -> motif
    (annotate part (fill NonZero paint (contour :| []))) (const (pointFrame (location contour)))
  AttachedContour contour rootOffset -> attachMotif RootPort (translationAffineIso2 rootOffset) $
    motif (annotate part (fill NonZero (localPaint rootOffset paint) (contour :| [])))
      (const identityAffineIso2)

partContour :: StudyControls -> StudyPart -> Located ClosedTrail
partContour controls part = case partGeometry controls part of
  WorldContour contour -> contour
  AttachedContour contour rootOffset ->
    transformLocatedClosedTrail (affineIsoMap (translationAffineIso2 rootOffset)) contour

partGeometry :: StudyControls -> StudyPart -> PartGeometry
partGeometry controls part = case part of
  LeftHorn -> AttachedContour (horn (hornSweep controls)) (ExactVector 336 359)
  RightHorn -> AttachedContour
    (transformLocatedClosedTrail
      (affine2 (ExactVector (-1) 0) (ExactVector 0 1) (ExactVector 0 0)) (horn 0))
    (ExactVector 484 359)
  LeftEye -> eyeGeometry (-1) controls
  RightEye -> eyeGeometry 1 controls

  Mask -> boundary $ cardinalClosed unitZero $ fmap (Interpolating . uncurry exactPoint)
    ((298,344) :| [(410,326),(522,344),(535,409),(507,485),(473,519)
      ,(410,529),(347,519),(313,485),(285,409)])
  Cloak -> boundary $ let f = cloakFullness controls in cardinalClosed unitHalf $
    fmap (Interpolating . uncurry exactPoint)
      ((359,499) :| [(320-f*exactHalf,612),(257-f,759),(288-f,775),(317-f,804)
        ,(350,798),(385,817),(420,805),(454,811),(488,795),(518+f,778)
        ,(560+f,751),(507+f*exactHalf,612),(461,499),(410,515)])
  LeftFoot -> boundary $ oval (357,825) (37,15) (-5)
  RightFoot -> boundary $ oval (464,825) (38,15) 5
  Blade -> boundary $ profileOutline unitHalf $
    ProfileStation (exactPoint 844 735) (ExactVector (-32) 0) :|
    [ ProfileStation (exactPoint 848 580) (ExactVector (-38) 0)
    , ProfileStation (exactPoint 874 420) (ExactVector (-32) 0)
    , ProfileStation (exactPoint 914 280) (ExactVector (-17) 0)
    , ProfileStation (exactPoint 958 194) (ExactVector 0 0) ]
  Fuller -> boundary $ let w=fullerWidth controls in profileOutline unitHalf $
    ProfileStation (exactPoint 894 350) (ExactVector 0 0) :|
    [ ProfileStation (exactPoint 871 440) (ExactVector (-w) 0)
    , ProfileStation (exactPoint 850 550) (ExactVector (-w*exactHalf) 0)
    , ProfileStation (exactPoint 843 630) (ExactVector 0 0) ]
  Guard -> boundary $ profileOutline unitZero $
    ProfileStation (exactPoint 743 736) (ExactVector 0 0) :|
    [ ProfileStation (exactPoint 783 760) (ExactVector 0 (-10))
    , ProfileStation (exactPoint 815 747) (ExactVector 0 (-10))
    , ProfileStation (exactPoint 843 735) (ExactVector 0 (-15))
    , ProfileStation (exactPoint 871 747) (ExactVector 0 (-10))
    , ProfileStation (exactPoint 903 760) (ExactVector 0 (-10))
    , ProfileStation (exactPoint 943 736) (ExactVector 0 0) ]
  Grip -> boundary $ profileOutline unitHalf $
    ProfileStation (exactPoint 843 753) (ExactVector (-14) 0) :|
    [ ProfileStation (exactPoint 843 795) (ExactVector (-11) 0)
    , ProfileStation (exactPoint 843 853) (ExactVector (-15) 0) ]

 where
  boundary :: Located ClosedTrail -> PartGeometry
  boundary = WorldContour

pointFrame :: ExactPoint -> AffineIso2
pointFrame point = let (x,y) = exactPointCoordinates point
                   in translationAffineIso2 (ExactVector x y)

-- World-space paints must be pulled back by the attachment translation.
-- All attached roots in this author are translations, so radii stay unchanged;
-- this helper does not pretend to support arbitrary affine radial gradients.
localPaint :: ExactVector -> Paint -> Paint
localPaint (ExactVector x y) paint = case paint of
  Solid color -> Solid color
  LinearGradient start end stops -> LinearGradient (local start) (local end) stops
  RadialGradient center radius stops -> RadialGradient (local center) radius stops
 where
  local :: ExactPoint -> ExactPoint
  local point = translateExactPoint point (ExactVector (negate x) (negate y))

eyeGeometry :: ExactRational -> StudyControls -> PartGeometry
eyeGeometry side controls =
  let center = exactPoint (410 + side * eyeSpacing controls) 420
      (x,y) = exactPointCoordinates center
      local = ellipse (ExactVector 27 (negate side * eyeTilt controls)) (ExactVector 0 43)
   in AttachedContour local (ExactVector x y)

type XY = (ExactRational, ExactRational)

oval :: XY -> XY -> ExactRational -> Located ClosedTrail
oval (x,y) (rx,ry) shear = transformLocatedClosedTrail
  (affine2 (ExactVector 1 0) (ExactVector 0 1) (ExactVector x y))
  (ellipse (ExactVector rx shear) (ExactVector 0 ry))

rectangle :: XY -> XY -> Located ClosedTrail
rectangle (x,y) (w,h) = polygonTrail $
  fmap (uncurry exactPoint) ((x,y) :| [(x+w,y),(x+w,y+h),(x,y+h)])

-- | One tapering horn profile; its mirror is the same authored shape. Sweep
-- changes the upper spine, not a collection of unrelated tangent handles.
horn :: ExactRational -> Located ClosedTrail
horn sweep = profileOutline unitZero $
  ProfileStation (exactPoint 0 0) (ExactVector (-18) 10) :|
  [ ProfileStation (exactPoint (-34) (-78)) (ExactVector (-14) 0)
  , ProfileStation (exactPoint (-55+sweep*exactHalf) (-146)) (ExactVector (-6) 0)
  , ProfileStation (exactPoint (-42+sweep) (-213)) (ExactVector 0 0) ]


ink, ivory, gold, blue :: Color
ink = RGB 11 23 36
ivory = RGB 227 242 236
gold = RGB 194 162 108
blue = RGB 95 169 194

linear :: XY -> XY -> Color -> Color -> Color -> Paint
linear a b lo mid hi = LinearGradient (uncurry exactPoint a) (uncurry exactPoint b)
  (gradientStops (GradientStop unitZero lo unitOne :|
    [GradientStop unitHalf mid unitOne, GradientStop unitOne hi unitOne]))

studyPicture :: StudyControls -> Picture StudyPart
studyPicture c = backdrop <> knight <> sword
 where
  shape :: StudyPart -> Paint -> Picture StudyPart
  shape part paint = motifPicture (partMotif c part paint)
  knight :: Picture StudyPart
  knight = shape LeftFoot (Solid ink) <> shape RightFoot (Solid ink)
    <> shape Cloak (linear (270,500) (540,810) (RGB 48 69 83) (RGB 24 43 57) (RGB 10 23 34))
    <> cloakFolds c
    <> shape LeftHorn porcelain <> shape RightHorn porcelain
    <> shape Mask porcelain <> maskRim c
    <> shape LeftEye (Solid ink) <> shape RightEye (Solid ink)
    <> eyeGlints c
  porcelain :: Paint
  porcelain = linear (300,330) (520,500) (RGB 166 196 198) ivory (RGB 199 221 217)
  bladeContours :: NonEmpty (Located ClosedTrail)
  bladeContours = partContour c Blade :| [partContour c Fuller]
  sword :: Picture StudyPart
  sword = shape Grip (linear (827,750) (859,855) ink (RGB 62 81 86) ink)
    <> gripBindings
    <> annotate Blade (fill EvenOdd steel bladeContours)
    <> annotate Blade (clip EvenOdd bladeContours (reflection <> bladeFacets))
    <> annotate Blade (outline blue (partContour c Blade))
    <> annotate Fuller (outline (RGB 161 208 211) (partContour c Fuller))
    <> shape Guard (linear (740,725) (938,778) (RGB 92 82 68) (RGB 229 211 162) gold)
    <> outline gold (partContour c Guard)
    <> fill NonZero (Solid gold) (oval (843,862) (17,12) 0 :| [])
    <> fill NonZero (linear (834,848) (850,870) blue ivory ink) (oval (843,859) (7,8) 0 :| [])
  steel :: Paint
  steel = linear (790,390) (912,590) (RGB 47 90 113) (RGB 178 220 223) (RGB 81 137 156)
  reflection :: Picture StudyPart
  reflection = opacity unitHalf $ fill NonZero
    (linear (760,420) (925,590) (RGB 116 196 214) ivory (RGB 95 157 180))
    (polygonTrail (fmap (uncurry exactPoint) ((760,530) :| [(928,413),(928,477),(760,594)])) :| [])
  bladeFacets :: Picture StudyPart
  bladeFacets = opacity unitHalf $ fill NonZero (Solid (RGB 226 247 240))
    (polygonTrail (fmap (uncurry exactPoint) ((838,727) :| [(860,510),(958,194),(883,480)])) :| [])

backdrop :: Picture StudyPart
backdrop = fill NonZero (linear (0,0) (1200,1000) (RGB 9 20 31) (RGB 19 38 53) (RGB 6 17 28))
  (rectangle (0,0) (1200,1000) :| [])
  <> opacity unitHalf (fill NonZero (Solid (RGB 42 65 78)) (oval (815,335) (246,246) 0 :| []))
  <> outline (RGB 65 87 96) (oval (815,335) (263,263) 0)
  <> fill NonZero (Solid (RGB 8 20 29)) (oval (608,873) (403,27) 0 :| [])
  <> foldMap star [(156,220),(210,387),(658,173),(1026,584),(690,706),(1060,230)]
  <> outline (RGB 64 88 98) (rectangle (54,54) (1092,892))
 where
  star :: XY -> Picture StudyPart
  star (x,y) = place (affine2 (ExactVector 1 0) (ExactVector 0 1) (ExactVector x y)) $
    fill NonZero (Solid (RGB 134 165 176))
      (polygonTrail (fmap (uncurry exactPoint)
        ((0,-5) :| [(2,-1),(6,0),(2,1),(0,5),(-2,1),(-6,0),(-2,-1)])) :| [])

outline :: Color -> Located ClosedTrail -> Picture part
outline color closed = stroke (pen color) (path (Seq.singleton (ClosedSubpath closed)))

pen :: Color -> StrokeStyle
pen color = StrokeStyle (Solid color) positiveOne LocalUnits RoundCap RoundJoin

-- | Decorative strokes are bowed chords or interpolated landmarks.
inkLine :: Color -> Located OpenTrail -> Picture part
inkLine color trail = stroke (pen color) (path (Seq.singleton (OpenSubpath trail)))

cloakFolds :: StudyControls -> Picture StudyPart
cloakFolds c = annotate Cloak $ clip NonZero (partContour c Cloak :| []) $
  foldMap foldLine [(-2),(-1),0,1,2]
 where
  foldLine :: ExactRational -> Picture StudyPart
  foldLine axis = inkLine (RGB 64 84 96) $
    bowedTrail (axis*exactThird*exactThird*exactHalf)
      (exactPoint (410+axis*19) 526)
      (exactPoint (410+axis*(51+cloakFullness c*exactHalf)) 813)

maskRim :: StudyControls -> Picture StudyPart
maskRim c = annotate Mask $ outline (RGB 145 178 182) (partContour c Mask)
  <> inkLine (RGB 244 252 242) (cardinalOpen unitHalf $
    fmap (Interpolating . uncurry exactPoint) ((302,369) :| [(294,419),(320,480),(353,504)]))

eyeGlints :: StudyControls -> Picture StudyPart
eyeGlints c = foldMap glint [LeftEye,RightEye]
 where
  glint :: StudyPart -> Picture StudyPart
  glint part = annotate part $ place (affineIsoMap (partPort c part)) $
    opacity unitHalf $ fill NonZero (Solid (RGB 89 124 139))
      (oval (-10,-17) (4,9) 0 :| [])

gripBindings :: Picture StudyPart
gripBindings = annotate Grip $ foldMap binding [0..6 :: Int]
 where
  binding :: Int -> Picture StudyPart
  binding i = place
    (affine2 (ExactVector 1 0) (ExactVector 0 1) (ExactVector 0 (770 + fromIntegral i*11)))
    (inkLine gold (bowedTrail (exactThird*exactHalf) (exactPoint 831 0) (exactPoint 855 0)))

-- | Visible landmarks are annotation, never geometric identity or mesh indices.
diagnosticPicture :: StudyControls -> Picture StudyPart
diagnosticPicture controls = studyPicture controls <> foldMap landmark [minBound..maxBound]
 where
  landmark :: StudyPart -> Picture StudyPart
  landmark part = annotate part $ outline (RGB 247 133 104)
    (transformLocatedClosedTrail
      (affineIsoMap (partPort controls part))
      (ellipse (ExactVector 5 0) (ExactVector 0 5)))
