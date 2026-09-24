-- | A standalone orbital sword authored from profiles, typed attachment frames,
-- repeated inscriptions and one affine pose. No hand-traced Bezier handles.
module Moonlight.Planar.Exhibit.SpaceSword
  ( SpaceSwordControls (..)
  , defaultSpaceSwordControls
  , overchargedSpaceSwordControls
  , SpaceSwordPart (..)
  , spaceSwordPartName
  , SpaceSwordError (..)
  , spaceSwordPicture
  , channelStations
  , channelCenterline
  , runeMeasurePolicy
  , runeFrames
  ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine
  ( Affine2, affine2, identityAffineIso2, translationAffineIso2 )
import Moonlight.Planar.Curve
  ( ClosedTrail, Located, OpenTrail, Subpath (..), circle, path )
import Moonlight.Planar.Curve.Authoring
  ( Knot (Interpolating), ProfileStation (..), bowedTrail, cardinalOpen
  , polygonTrail, profileOutline )
import Moonlight.Planar.Curve.Frame
  ( FrameError, FramePolicy, FrameSite (..), MeasuredFrame, RunError, fractionRun
  , framePolicy, measuredFrameIso, regularFrame, sampleRun )
import Moonlight.Planar.Curve.Measure
  ( MeasureError, MeasurePolicy, RadicalPrecisionError, SubdivisionBudgetError
  , measurePolicy, measureSubpath, radicalPrecision, subdivisionBudget )
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), PositiveExact, ScalarRefinementError
  , UnitInterval, divideByPositive, exactHalf, exactPoint, exactPointCoordinates, exactThird, positiveExact
  , positiveOne, positiveTwo, unitHalf, unitInterval, unitOne, unitZero, unitIntervalValue )
import Moonlight.Planar.Illustration
  ( Color (..), FillRule (..), GradientStop (..), LineCap (..), LineJoin (..)
  , Paint (..), Picture, StrokeStyle (..), StrokeUnits (..), annotate, clip
  , fill, gradientStops, opacity, place, stroke )
import Moonlight.Planar.Illustration.Motif
  ( Motif, attachMotif, motif, motifPicture, motifPort )

-- | Charge widens the recessed energy channel and lengthens its trailing wake.
-- Sweep bends the cutting edge without disturbing the hilt assembly.
data SpaceSwordControls = SpaceSwordControls
  { bladeCharge :: !UnitInterval
  , bladeSweep :: !ExactRational
  } deriving stock (Eq, Show)

defaultSpaceSwordControls :: SpaceSwordControls
defaultSpaceSwordControls = SpaceSwordControls unitHalf 28

overchargedSpaceSwordControls :: SpaceSwordControls
overchargedSpaceSwordControls = SpaceSwordControls unitOne 40

data SpaceSwordPart
  = VoidField
  | NebulaVeil
  | BladeAura
  | BladeShell
  | BladeCore
  | BladeRunes
  | Crossguard
  | ReactorGrip
  | Counterweight
  | IonWake
  deriving stock (Eq, Ord, Show, Enum, Bounded)

spaceSwordPartName :: SpaceSwordPart -> String
spaceSwordPartName part = case part of
  VoidField -> "void-field"
  NebulaVeil -> "nebula-veil"
  BladeAura -> "blade-aura"
  BladeShell -> "blade-shell"
  BladeCore -> "blade-core"
  BladeRunes -> "blade-runes"
  Crossguard -> "crossguard"
  ReactorGrip -> "reactor-grip"
  Counterweight -> "counterweight"
  IonWake -> "ion-wake"

data BladePort = BladeRoot | BladeTip

data RunePort = RuneRoot

data HiltPort = BladeSocket

-- | A refused authored scalar or policy, an unmeasured centerline, or a rune
-- station without a regular frame.
data SpaceSwordError
  = SwordPrecisionRefused !RadicalPrecisionError
  | SwordScalarRefused !ScalarRefinementError
  | SwordMeasurePolicyRefused !SubdivisionBudgetError
  | SwordRunRefused !RunError
  | SwordMeasureRefused !MeasureError
  | SwordFrameRefused !FrameError
  deriving stock (Eq, Show)

spaceSwordPicture :: SpaceSwordControls -> Either SpaceSwordError (Picture SpaceSwordPart)
spaceSwordPicture controls = do
  assembly <- swordAssembly controls
  pure (annotate VoidField voidField <> annotate NebulaVeil nebulaVeil <> assembly)

-- | Blade and hilt are attached in their authoring frame, then the complete
-- assembly receives one affine action. Geometry, clips, paint and ornament
-- therefore cannot drift apart when the weapon is posed.
swordAssembly :: SpaceSwordControls -> Either SpaceSwordError (Picture SpaceSwordPart)
swordAssembly controls = do
  bladeValue <- bladeMotif controls
  let hilt = hiltMotif
      blade = attachMotif BladeRoot (motifPort hilt BladeSocket) bladeValue
      assembled = annotate IonWake (ionWake controls)
        <> motifPicture blade
        <> motifPicture hilt
  pure (place swordPose assembled)

-- | A similarity pose: orthogonal columns of equal squared length. The socket
-- at @(604,745)@ lands at @(350,710)@, sending the blade toward the upper right.
swordPose :: Affine2
swordPose = affine2
  (ExactVector threeQuarters 1)
  (ExactVector (-1) threeQuarters)
  (ExactVector 642 (negate (452 + threeQuarters)))
 where
  threeQuarters = exactHalf + exactHalf * exactHalf

bladeMotif :: SpaceSwordControls -> Either SpaceSwordError (Motif BladePort SpaceSwordPart)
bladeMotif controls = do
  runes <- runeColumn controls
  pure (motif (picture runes) port)
 where
  charge = unitIntervalValue (bladeCharge controls)
  sweep = bladeSweep controls
  aura = bladeProfile (42 + 6 * charge) sweep
  shell = bladeProfile 34 sweep
  channel = profileOutline channelTension (channelStations charge sweep)
  picture runes = annotate BladeAura
      (opacity unitHalf (fill NonZero (Solid (RGB 27 118 151)) (aura :| [])))
    <> annotate BladeShell
      (fill NonZero shellPaint (shell :| [])
        <> clip NonZero (shell :| [])
          (bladeSpineFacet sweep <> cuttingBevel sweep <> bladeReflection sweep)
        <> outline (RGB 118 154 166) shell)
    <> annotate BladeCore
      (fill NonZero corePaint (channel :| [])
        <> outline (RGB 128 232 215) channel)
    <> annotate BladeRunes runes
  port selected = case selected of
    BladeRoot -> identityAffineIso2
    BladeTip -> translationAffineIso2 (ExactVector sweep (-625))

-- | The lower spine remains nearly straight while the cutting edge carries the
-- changing width and sweep. The silhouette is still one editable profile.
bladeProfile :: ExactRational -> ExactRational -> Located ClosedTrail
bladeProfile width sweep = profileOutline unitHalf $
  ProfileStation (exactPoint 0 0) (ExactVector width 0) :|
  [ ProfileStation (exactPoint (width * exactThird) (-150))
      (ExactVector (width + width * exactThird) 0)
  , ProfileStation (exactPoint (width * exactThird + sweep * exactThird) (-345))
      (ExactVector (width + 5) 0)
  , ProfileStation (exactPoint (sweep - width * exactThird) (-530))
      (ExactVector (width * exactHalf + 3) 0)
  , ProfileStation (exactPoint sweep (-625)) (ExactVector 0 0)
  ]

-- | The channel's author-owned stations, factored once: its painted outline
-- and its unpainted centerline are both derived from them at one tension.
channelStations :: ExactRational -> ExactRational -> NonEmpty ProfileStation
channelStations charge sweep =
  ProfileStation (exactPoint 8 (-28)) (ExactVector (4 + 2 * charge) 0) :|
  [ ProfileStation (exactPoint 12 (-145)) (ExactVector (5 + 2 * charge) 0)
  , ProfileStation (exactPoint 19 (-315)) (ExactVector (6 + 2 * charge) 0)
  , ProfileStation (exactPoint (sweep - 7) (-490)) (ExactVector (4 + charge) 0)
  , ProfileStation (exactPoint sweep (-579)) (ExactVector 0 0)
  ]

channelTension :: UnitInterval
channelTension = unitHalf

-- | The exact midpoint of the channel's two rails: cardinal interpolation is
-- affine-linear in its knots, so interpolating the station centers at the
-- rails' tension is their average. Charge moves only half-spans, not this.
channelCenterline :: ExactRational -> ExactRational -> Located OpenTrail
channelCenterline charge sweep = cardinalOpen channelTension
  ((\(ProfileStation center _) -> Interpolating center) <$> channelStations charge sweep)

bladeSpineFacet :: ExactRational -> Picture SpaceSwordPart
bladeSpineFacet sweep = fill NonZero (Solid (RGB 8 18 30)) $
  polygonTrail (exactPoint (-34) 0 :|
    [ exactPoint (-34) (-148)
    , exactPoint (-19) (-345)
    , exactPoint (sweep - 22) (-528)
    , exactPoint (sweep - 5) (-602)
    , exactPoint 2 (-340)
    , exactPoint (-5) (-80)
    ]) :| []

cuttingBevel :: ExactRational -> Picture SpaceSwordPart
cuttingBevel sweep = fill NonZero (Solid (RGB 148 182 190)) $
  polygonTrail (exactPoint 34 0 :|
    [ exactPoint 57 (-150)
    , exactPoint 63 (-345)
    , exactPoint (sweep + 20) (-530)
    , exactPoint sweep (-625)
    , exactPoint (sweep + 8) (-520)
    , exactPoint 43 (-320)
    , exactPoint 28 (-55)
    ]) :| []

bladeReflection :: ExactRational -> Picture SpaceSwordPart
bladeReflection sweep = opacity unitHalf $ fill NonZero (Solid (RGB 220 239 224)) $
  polygonTrail (exactPoint 27 (-302) :|
    [ exactPoint 42 (-336)
    , exactPoint (sweep + 13) (-485)
    , exactPoint (sweep + 6) (-522)
    , exactPoint 31 (-386)
    ]) :| []

-- | Three runes evenly spaced by arc length over the middle three fifths of
-- the channel centerline, each attached to the tangent frame at its station.
runeColumn :: SpaceSwordControls -> Either SpaceSwordError (Picture SpaceSwordPart)
runeColumn controls = foldMap attached <$> runeFrames controls
 where
  rune = runeMotif (unitIntervalValue (bladeCharge controls))
  attached :: MeasuredFrame -> Picture SpaceSwordPart
  attached frame = motifPicture (attachMotif RuneRoot (measuredFrameIso frame) rune)

-- | Arc-length tolerance of an eighth of a unit; frames within 2^-20 of unit.
runeMeasurePolicy :: Either SpaceSwordError (MeasurePolicy, FramePolicy)
runeMeasurePolicy = do
  precision <- first SwordPrecisionRefused (radicalPrecision 32)
  tolerance <- first SwordScalarRefused (positiveExact (exactHalf ^ (3 :: Int)))
  deficit <- first SwordScalarRefused (positiveExact (exactHalf ^ (20 :: Int)))
  budget <- first SwordMeasurePolicyRefused (subdivisionBudget 24 4096 4096)
  pure (measurePolicy tolerance precision budget, framePolicy precision deficit)

-- | The rune stations in run order: an even run of three over fractions 1/5
-- to 4/5 of the measured centerline, framed where the centerline passes.
runeFrames :: SpaceSwordControls -> Either SpaceSwordError (NonEmpty MeasuredFrame)
runeFrames controls = do
  (measuring, framing) <- runeMeasurePolicy
  five <- first SwordScalarRefused (positiveExact 5)
  firstStation <- first SwordScalarRefused (unitInterval (divideByPositive 1 five))
  finalStation <- first SwordScalarRefused (unitInterval (divideByPositive 4 five))
  run <- first SwordRunRefused (fractionRun 3 firstStation finalStation)
  measured <- first SwordMeasureRefused (measureSubpath measuring (OpenSubpath centerline))
  samples <- first SwordRunRefused (sampleRun run measured)
  first SwordFrameRefused (traverse (regularFrame framing . SampledSite) samples)
 where
  centerline = channelCenterline (unitIntervalValue (bladeCharge controls)) (bladeSweep controls)

-- | A diamond authored in its station's frame: its forward tip on local +x,
-- the tangent, and its narrow axis on local +y, the left normal.
runeMotif :: ExactRational -> Motif RunePort SpaceSwordPart
runeMotif charge = motif picture (const identityAffineIso2)
 where
  size = 5 + 2 * charge
  rune = polygonTrail (exactPoint size 0 :|
    [ exactPoint 0 (size * exactHalf)
    , exactPoint (negate size) 0
    , exactPoint 0 (negate (size * exactHalf))
    ])
  picture :: Picture SpaceSwordPart
  picture = opacity unitHalf (fill NonZero (Solid (RGB 108 221 202)) (rune :| []))
    <> outline (RGB 174 236 213) rune

ionWake :: SpaceSwordControls -> Picture SpaceSwordPart
ionWake controls = opacity unitHalf $
  ink positiveTwo (RGB 39 146 184) outer
    <> ink positiveOne (RGB 105 210 202) inner
 where
  charge = unitIntervalValue (bladeCharge controls)
  sweep = bladeSweep controls
  outer = cardinalOpen unitHalf (Interpolating (exactPoint 50 (-18)) :|
    (Interpolating <$>
      [ exactPoint (78 + 18 * charge) (-155)
      , exactPoint (72 + 27 * charge) (-355)
      , exactPoint (sweep + 38 + 36 * charge) (-574)
      ]))
  inner = bowedTrail (exactThird + charge * exactThird)
    (exactPoint 46 (-42)) (exactPoint (sweep + 22) (-545))

hiltMotif :: Motif HiltPort SpaceSwordPart
hiltMotif = motif picture port
 where
  picture = annotate Crossguard crossguard
    <> annotate ReactorGrip reactorGrip
    <> annotate Counterweight counterweight
  port BladeSocket = translationAffineIso2 (ExactVector 604 745)

crossguard :: Picture SpaceSwordPart
crossguard =
  fill NonZero guardPaint (guardContour :| [])
    <> fill NonZero (Solid (RGB 166 135 74)) (guardInlay :| [])
    <> outline (RGB 177 151 91) guardContour
    <> fill NonZero (Solid (RGB 12 29 39)) (socketContour :| [])
    <> outline (RGB 91 143 148) socketContour

guardContour :: Located ClosedTrail
guardContour = polygonTrail (exactPoint 495 750 :|
  [ exactPoint 552 730
  , exactPoint 591 735
  , exactPoint 604 721
  , exactPoint 618 735
  , exactPoint 663 728
  , exactPoint 718 746
  , exactPoint 658 765
  , exactPoint 618 757
  , exactPoint 604 770
  , exactPoint 590 758
  , exactPoint 550 766
  ])

guardInlay :: Located ClosedTrail
guardInlay = polygonTrail (exactPoint 512 750 :|
  [ exactPoint 556 738
  , exactPoint 590 744
  , exactPoint 590 750
  , exactPoint 552 756
  ])

socketContour :: Located ClosedTrail
socketContour = polygonTrail (exactPoint 584 733 :|
  [ exactPoint 624 733
  , exactPoint 632 758
  , exactPoint 619 774
  , exactPoint 589 774
  , exactPoint 576 758
  ])

reactorGrip :: Picture SpaceSwordPart
reactorGrip =
  fill NonZero gripPaint (gripContour :| [])
    <> clip NonZero (gripContour :| []) (foldMap gripBand [0..4 :: Int])
    <> outline (RGB 73 105 113) gripContour
    <> gripAxis

gripContour :: Located ClosedTrail
gripContour = profileOutline unitHalf $
  ProfileStation (exactPoint 604 766) (ExactVector 19 0) :|
  [ ProfileStation (exactPoint 604 815) (ExactVector 17 0)
  , ProfileStation (exactPoint 604 884) (ExactVector 20 0)
  , ProfileStation (exactPoint 604 922) (ExactVector 15 0)
  ]

gripBand :: Int -> Picture SpaceSwordPart
gripBand index = opacity unitHalf $ fill NonZero (Solid (RGB 148 124 78)) $
  polygonTrail (exactPoint 582 y :|
    [ exactPoint 626 (y + 12)
    , exactPoint 626 (y + 19)
    , exactPoint 582 (y + 7)
    ]) :| []
 where
  y = 780 + fromIntegral index * 27

gripAxis :: Picture SpaceSwordPart
gripAxis = opacity unitHalf $ ink positiveOne (RGB 81 159 158) $
  cardinalOpen unitHalf (Interpolating (exactPoint 604 772) :|
    (Interpolating <$> [exactPoint 600 822, exactPoint 607 875, exactPoint 604 916]))

counterweight :: Picture SpaceSwordPart
counterweight =
  fill NonZero guardPaint (counterweightContour :| [])
    <> outline (RGB 147 132 91) counterweightContour
    <> fill NonZero (Solid (RGB 91 198 184)) (emitterSlit :| [])
 where
  counterweightContour = polygonTrail (exactPoint 604 913 :|
    [ exactPoint 622 929
    , exactPoint 619 954
    , exactPoint 604 972
    , exactPoint 589 954
    , exactPoint 586 929
    ])
  emitterSlit = polygonTrail (exactPoint 599 939 :|
    [exactPoint 609 939, exactPoint 611 949, exactPoint 604 956, exactPoint 597 949])

voidField :: Picture SpaceSwordPart
voidField = fill NonZero voidPaint
    (polygonTrail (exactPoint 0 0 :|
      [exactPoint 1200 0,exactPoint 1200 1000,exactPoint 0 1000]) :| [])
  <> foldMap star stars
  <> foldMap brightStar brightStars
 where
  star :: (ExactRational, ExactRational, ExactRational) -> Picture SpaceSwordPart
  star (x,y,size) = place (uniformAt size (exactPoint x y)) $
    opacity unitHalf (fill NonZero (Solid (RGB 139 166 184)) (circle positiveOne :| []))
  brightStar :: (ExactRational, ExactRational, ExactRational) -> Picture SpaceSwordPart
  brightStar (x,y,size) = place (uniformAt size (exactPoint x y)) $
    opacity unitHalf (fill NonZero (Solid (RGB 173 205 207)) (spark :| []))
  stars :: [(ExactRational, ExactRational, ExactRational)]
  stars = [(82,132,1),(188,310,1),(298,91,2),(482,178,1),(699,85,1)
    ,(884,152,1),(1094,88,1),(1057,416,2),(946,817,1),(1111,912,1)]
  brightStars :: [(ExactRational, ExactRational, ExactRational)]
  brightStars = [(229,655,2),(741,798,2),(1010,598,1)]

nebulaVeil :: Picture SpaceSwordPart
nebulaVeil = opacity unitHalf $
  fill NonZero nebulaPaint (nebulaContour :| [])
 where
  nebulaContour = profileOutline unitHalf $
    ProfileStation (exactPoint (-120) 940) (ExactVector 76 108) :|
    [ ProfileStation (exactPoint 236 755) (ExactVector 66 101)
    , ProfileStation (exactPoint 596 506) (ExactVector 54 88)
    , ProfileStation (exactPoint 951 263) (ExactVector 42 67)
    , ProfileStation (exactPoint 1280 72) (ExactVector 24 36)
    ]

spark :: Located ClosedTrail
spark = polygonTrail (exactPoint 0 (-3) :|
  [exactPoint 1 (-1),exactPoint 4 0,exactPoint 1 1,exactPoint 0 4
  ,exactPoint (-1) 1,exactPoint (-4) 0,exactPoint (-1) (-1)])

voidPaint, nebulaPaint, shellPaint, corePaint, guardPaint, gripPaint :: Paint
voidPaint = linear (exactPoint 0 0) (exactPoint 1200 1000)
  (RGB 2 5 13) (RGB 12 16 37) (RGB 4 9 22)
nebulaPaint = linear (exactPoint 0 1000) (exactPoint 1200 0)
  (RGB 39 17 57) (RGB 25 72 86) (RGB 8 20 43)
shellPaint = linear (exactPoint (-40) 0) (exactPoint 70 (-625))
  (RGB 13 27 43) (RGB 104 126 137) (RGB 15 49 68)
corePaint = linear (exactPoint 8 (-28)) (exactPoint 40 (-579))
  (RGB 26 78 89) (RGB 120 229 204) (RGB 31 113 139)
guardPaint = linear (exactPoint 495 730) (exactPoint 718 766)
  (RGB 18 28 39) (RGB 112 99 72) (RGB 20 42 53)
gripPaint = linear (exactPoint 584 766) (exactPoint 624 922)
  (RGB 7 14 24) (RGB 40 51 54) (RGB 8 18 27)

linear :: ExactPoint -> ExactPoint -> Color -> Color -> Color -> Paint
linear a b low middle high = LinearGradient a b $ gradientStops
  (GradientStop unitZero low unitOne :|
    [GradientStop unitHalf middle unitOne, GradientStop unitOne high unitOne])

outline :: Color -> Located ClosedTrail -> Picture part
outline color curve = stroke
  (StrokeStyle (Solid color) positiveOne OutputUnits RoundCap RoundJoin)
  (path (Seq.singleton (ClosedSubpath curve)))

ink :: PositiveExact -> Color -> Located OpenTrail -> Picture part
ink width color curve = stroke
  (StrokeStyle (Solid color) width OutputUnits RoundCap RoundJoin)
  (path (Seq.singleton (OpenSubpath curve)))

uniformAt :: ExactRational -> ExactPoint -> Affine2
uniformAt scale center = affine2
  (ExactVector scale 0) (ExactVector 0 scale)
  (uncurry ExactVector (exactPointCoordinates center))
