module Moonlight.Planar.IllustrationSpec (tests) where

import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.List (isInfixOf, isPrefixOf, tails)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine (affine2, identityAffine2)
import Moonlight.Planar.Curve
  ( ClosedTrail, Located, Subpath (..), circle, closeWith, cubic, curveStep
  , line, locate, openTrail, path, quadratic )
import Moonlight.Planar.Exact
  ( ExactRational, ExactVector (..), exactPoint, exactHalf
  , exactThird, positiveExact, positiveOne, positiveTwo, unitHalf, unitOne, unitZero )
import Moonlight.Planar.Illustration
  ( Color (..), FillRule (..), GradientStop (..), IllustrationError (..)
  , LineCap (..), LineJoin (..), Paint (..), Picture, StrokeStyle (..)
  , StrokeUnits (..), annotate, clip, fill, miterLimit, miterLimitFour, opacity
  , place, stroke, gradientStops, gradientStopValues )
import Moonlight.Planar.Illustration.Svg
  ( SvgError (..), SvgOptions, renderDiagnosticSvg, renderSvg
  , svgOptions, svgPrecision, svgViewport )

tests :: IO ()
tests = traverse_ run cases
 where
  run :: (String, Either String ()) -> IO ()
  run (name, result) = either (fail . ((name <> ": ") <>)) pure result

cases :: [(String, Either String ())]
cases =
  [ ("ordered monoid", orderedMonoid)
  , ("native polynomial paths", nativePaths)
  , ("compound fill and scoped clip", compoundClip)
  , ("group opacity is not distributed", groupedOpacity)
  , ("stroke units and joins", strokeSemantics)
  , ("gradient definitions and stable IDs", gradientSemantics)
  , ("annotations escaped and diagnosed", annotations)
  , ("XML control characters refused", invalidXml)
  , ("conics bounded or refused", conicPublication)
  , ("affine output metric controls subdivision", affineSubdivision)
  , ("decimal precision and range refusals", precisionRefusals)
  , ("miter refinement", miterRefinement)
  ]

assert :: String -> Bool -> Either String ()
assert message condition = if condition then Right () else Left message

checked :: Show e => Either e a -> Either String a
checked = first show

options :: Int -> ExactRational -> ExactRational -> Int -> Int -> Either String SvgOptions
options digits numericTolerance curveTolerance depth leaves = do
  extent <- checked (positiveExact 100)
  viewport <- checked (svgViewport 100 100 (exactPoint 0 0) extent extent)
  numeric <- checked (positiveExact numericTolerance)
  precision <- checked (svgPrecision digits numeric)
  curves <- checked (positiveExact curveTolerance)
  checked (svgOptions viewport precision curves depth leaves)

standard :: Either String SvgOptions
standard = options 6 (exactHalf ^ (18 :: Int)) (exactHalf ^ (3 :: Int)) 18 10000

render :: Picture String -> Either String String
render picture = standard >>= \config -> checked (renderSvg id config picture)

square :: Located ClosedTrail
square = locate (exactPoint 0 0) $ closeWith line $ openTrail $ Seq.fromList
  [ curveStep line (ExactVector 10 0)
  , curveStep line (ExactVector 0 10)
  , curveStep line (ExactVector (-10) 0)
  ]

red, blue :: Picture String
red = fill NonZero (Solid (RGB 255 0 0)) (square :| [])
blue = fill NonZero (Solid (RGB 0 0 255)) (square :| [])

orderedMonoid :: Either String ()
orderedMonoid = do
  assert "picture monoid associativity" ((red <> blue) <> red == red <> (blue <> red))
  assert "picture monoid identities" (mempty <> red == red && red <> mempty == red)
  let named = annotate "horn" red
  assert "annotation functor identity" (fmap id named == named)
  assert "annotation functor composition"
    (fmap (("left." <>) . (<> ".tip")) named == (fmap ("left." <>) . fmap (<> ".tip")) named)
  forward <- render (red <> blue)
  backward <- render (blue <> red)
  assert "painter order was discarded" (forward /= backward)
  assert "red must paint before blue" ("fill=\"#ff0000\" fill-rule=\"nonzero\"/><path" `isInfixOf` forward)

style :: StrokeUnits -> StrokeStyle
style units = StrokeStyle (Solid (RGB 0 0 0)) positiveOne units RoundCap (MiterJoin miterLimitFour)

nativePaths :: Either String ()
nativePaths = do
  let curve = path $ Seq.singleton $ OpenSubpath $ locate (exactPoint 0 0) $ openTrail $ Seq.fromList
        [ curveStep (quadratic (ExactVector 3 4)) (ExactVector 6 0)
        , curveStep (cubic (ExactVector 1 2) (ExactVector 3 2)) (ExactVector 4 0)
        ]
  svg <- render (stroke (style LocalUnits) curve)
  assert "quadratic was not retained" ("Q 3.000000 4.000000 6.000000 0.000000" `isInfixOf` svg)
  assert "cubic controls lost their local origin" ("C 7.000000 2.000000 9.000000 2.000000 10.000000 0.000000" `isInfixOf` svg)

compoundClip :: Either String ()
compoundClip = do
  let hole = locate (exactPoint 3 3) $ closeWith line $ openTrail $ Seq.fromList
        [curveStep line (ExactVector 2 0), curveStep line (ExactVector 0 2), curveStep line (ExactVector (-2) 0)]
      contours = square :| [hole]
  svg <- render (clip EvenOdd contours (fill EvenOdd (Solid (RGB 4 5 6)) contours) <> blue)
  assert "compound path lost a contour" (count "M 3.000000 3.000000" svg == 2)
  assert "clip and fill winding differ" ("clip-rule=\"evenodd\"" `isInfixOf` svg && "fill-rule=\"evenodd\"" `isInfixOf` svg)
  assert "clip was not scoped" ("</g><path" `isInfixOf` svg)

groupedOpacity :: Either String ()
groupedOpacity = do
  svg <- render (opacity unitHalf (red <> blue))
  assert "opacity duplicated onto children" (count "opacity=" svg == 1)
  assert "opacity lost its group" ("<g opacity=\"0.500000\"><path" `isInfixOf` svg)

strokeSemantics :: Either String ()
strokeSemantics = do
  let curve = path (Seq.singleton (ClosedSubpath square))
      transform = affine2 (ExactVector 2 0) (ExactVector 0 3) (ExactVector 7 11)
  svg <- render (place transform (stroke (style LocalUnits) curve <> stroke (style OutputUnits) curve))
  assert "output stroke scaling not isolated" (count "non-scaling-stroke" svg == 1)
  assert "cap or miter lost" (count "stroke-linecap=\"round\"" svg == 2 && count "stroke-miterlimit=\"4.000000\"" svg == 2)
  assert "affine coefficient order incorrect" ("matrix(2.000000 0.000000 0.000000 3.000000 7.000000 11.000000)" `isInfixOf` svg)

gradientSemantics :: Either String ()
gradientSemantics = do
  let start = GradientStop unitZero (RGB 1 2 3) unitOne
      end = GradientStop unitOne (RGB 4 5 6) unitHalf
      final = GradientStop unitOne (RGB 7 8 9) unitOne
      stops = gradientStops (end :| [start, final])
      linear :: Picture String
      linear = fill NonZero (LinearGradient (exactPoint 0 0) (exactPoint 10 0) stops) (square :| [])
      radial :: Picture String
      radial = fill NonZero (RadialGradient (exactPoint 5 5) positiveTwo stops) (square :| [])
      picture = annotate "same" linear <> annotate "same" radial
  assert "gradient canonicalization unstable at equal offsets"
    (gradientStopValues stops == start :| [end, final])
  svg <- render picture
  again <- render picture
  assert "render nondeterministic" (svg == again)
  assert "gradient units not local user space" (count "gradientUnits=\"userSpaceOnUse\"" svg == 2)
  assert "gradient IDs not structural and unique"
    (count "id=\"paint-0-0-0\"" svg == 1 && count "id=\"paint-1-0-0\"" svg == 1)
  assert "gradient reference mismatch" ("fill=\"url(#paint-1-0-0)\"" `isInfixOf` svg)

annotations :: Either String ()
annotations = do
  config <- standard
  let picture = annotate "horn<&\"'" (place identityAffine2 red)
  svg <- checked (renderSvg id config picture)
  diagnostic <- checked (renderDiagnosticSvg id config picture)
  assert "label was not escaped" ("horn&lt;&amp;&quot;&apos;" `isInfixOf` svg)
  assert "diagnostic did not retain artwork" ("fill=\"#ff0000\"" `isInfixOf` diagnostic)
  assert "diagnostic lacks controls/axes/part caption" (all (`isInfixOf` diagnostic) ["polyline", "#288e94", "<text", "authoring guides"])
  assert "final artifact leaked guides" (not ("authoring guides" `isInfixOf` svg))

invalidXml :: Either String ()
invalidXml = do
  config <- standard
  case renderSvg id config (annotate "bad\NULlabel" red) of
    Left (SvgInvalidXmlCharacter '\NUL') -> Right ()
    outcome -> Left ("expected XML character refusal, got " <> show outcome)

conicPublication :: Either String ()
conicPublication = do
  let disc :: Picture String
      disc = fill NonZero (Solid (RGB 1 1 1)) (circle positiveTwo :| [])
  svg <- render disc
  assert "conic fallback not polygonal" ("L " `isInfixOf` svg && not ("Q " `isInfixOf` svg))
  config <- options 6 (exactHalf ^ (18 :: Int)) (exactHalf ^ (10 :: Int)) 0 1
  case renderSvg id config disc of
    Left (SvgLoweringRefused _) -> Right ()
    outcome -> Left ("expected conic budget refusal, got " <> show outcome)

affineSubdivision :: Either String ()
affineSubdivision = do
  let disc :: Picture String
      disc = fill NonZero (Solid (RGB 1 1 1)) (circle positiveTwo :| [])
      enlarge = affine2 (ExactVector 100 0) (ExactVector 0 100) (ExactVector 0 0)
  local <- render disc
  scaled <- render (place enlarge disc)
  assert "conic budget ignored output transform" (count "L " scaled > count "L " local)

precisionRefusals :: Either String ()
precisionRefusals = do
  strict <- options 2 (exactHalf ^ (20 :: Int)) 1 18 10000
  let shifted = affine2 (ExactVector 1 0) (ExactVector 0 1) (ExactVector exactThird 0)
  assert "inadequate decimal precision accepted" $ case renderSvg id strict (place shifted red) of
    Left (SvgNumericPrecisionRefused _ 2) -> True
    _ -> False
  config <- standard
  let huge = affine2 (ExactVector 1 0) (ExactVector 0 1) (ExactVector (10 ^ (310 :: Int)) 0)
  assert "nonfinite browser scalar accepted" $ case renderSvg id config (place huge red) of
    Left (SvgNumericRangeRefused _) -> True
    _ -> False
  tiny <- checked (positiveExact (exactHalf ^ (30 :: Int)))
  let tinyStroke = (style LocalUnits) { strokeWidth = tiny }
  assert "positive stroke rounded to zero" $ case renderSvg id config (stroke tinyStroke (path (Seq.singleton (ClosedSubpath square)))) of
    Left (SvgPositiveValueRoundedToZero _) -> True
    _ -> False

miterRefinement :: Either String ()
miterRefinement = do
  assert "miter limit below one accepted" (miterLimit exactHalf == Left (InvalidMiterLimit exactHalf))
  assert "miter limit one refused" (not (isLeft (miterLimit 1)))

count :: String -> String -> Int
count needle = length . filter (isPrefixOf needle) . tails
