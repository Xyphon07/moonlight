-- | Pure, bounded SVG publication. Native polynomial curves are retained;
-- rational conics descend through the canonical certified lowering owner.
-- Decimal rounding is checked per scalar, separately from the output-pixel
-- curve approximation bound. Neither receipt claims browser raster equivalence.
module Moonlight.Planar.Illustration.Svg
  ( SvgViewport
  , svgViewport
  , SvgPrecision
  , svgPrecision
  , SvgOptions
  , svgOptions
  , SvgError (..)
  , renderSvg
  , renderDiagnosticSvg
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.List (intercalate, mapAccumL)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sequence (Seq)
import Data.Word (Word8)
import Moonlight.Planar.Affine
  ( Affine2, affine2, affineColumns, identityAffine2, composeAffine2, transformPoint )
import Moonlight.Planar.Curve
  ( ClosedTrail, Located, CurveStep, CurveShapeView (..), Subpath (..)
  , location, locatedValue, locate, pathSubpaths, closedTrailSteps, trailSteps
  , curveStepShape, curveStepEnd, shapeView )
import Moonlight.Planar.Curve.Lowering
  ( LoweringError, loweringPolicy, lowerStep, loweredPoints )
import Moonlight.Planar.Exact
  ( ExactPoint, ExactVector (..), ExactRational, PositiveExact
  , exactPoint, exactPointCoordinates, translateExactPoint
  , exactRationalNumerator, exactRationalDenominator, exactRationalBitWidth
  , positiveExactValue, unitIntervalValue, divideByPositive )
import Moonlight.Planar.Illustration
  ( Picture, PictureAlgebra (..), foldPicture, Color (..), Paint (..)
  , GradientStop (..), gradientStopValues, FillRule (..), StrokeStyle (..), StrokeUnits (..)
  , LineCap (..), LineJoin (..), miterLimitValue )
import Numeric (showHex)

data SvgViewport = SvgViewport !Int !Int !ExactPoint !PositiveExact !PositiveExact
  deriving stock (Eq, Show)

-- | Pixel dimensions and an explicit, fixed view box. The interpreter uses
-- preserveAspectRatio="none", making the declared x/y pixel metrics literal.
svgViewport
  :: Int -> Int -> ExactPoint -> PositiveExact -> PositiveExact
  -> Either SvgError SvgViewport
svgViewport width height origin extentX extentY
  | width <= 0 || height <= 0 = Left (InvalidPixelDimensions width height)
  | otherwise = Right (SvgViewport width height origin extentX extentY)

data SvgPrecision = SvgPrecision !Int !PositiveExact
  deriving stock (Eq, Show)

-- | Maximum absolute error of each emitted decimal scalar. This is not a
-- claim about accumulated transform error or the browser's numeric parser.
svgPrecision :: Int -> PositiveExact -> Either SvgError SvgPrecision
svgPrecision digits tolerance
  | digits < 0 || digits > 30 = Left (InvalidDecimalPlaces digits)
  | otherwise = Right (SvgPrecision digits tolerance)

data SvgOptions = SvgOptions !SvgViewport !SvgPrecision !PositiveExact !Int !Int
  deriving stock (Eq, Show)

-- | Conic tolerance is in output pixels. Depth and leaves are hard budgets
-- per source conic; exhaustion is a typed refusal, never a coarse fallback.
svgOptions
  :: SvgViewport -> SvgPrecision -> PositiveExact -> Int -> Int
  -> Either SvgError SvgOptions
svgOptions viewport precision tolerance depth leaves =
  SvgOptions viewport precision tolerance depth leaves
    <$ first SvgLoweringRefused (loweringPolicy tolerance identityAffine2 depth leaves)

data SvgError
  = InvalidPixelDimensions !Int !Int
  | InvalidDecimalPlaces !Int
  | SvgNumericPrecisionRefused !ExactRational !Int
  | SvgNumericMagnitudeRefused !Int
  | SvgNumericRangeRefused !ExactRational
  | SvgPositiveValueRoundedToZero !ExactRational
  | SvgInvalidXmlCharacter !Char
  | SvgLoweringRefused !LoweringError
  deriving stock (Eq, Show)

data Rendered = Rendered
  { artwork :: String
  , guides :: String
  , firstAnchor :: Maybe ExactPoint
  }

instance Semigroup Rendered where
  Rendered a g p <> Rendered b h q = Rendered (a <> b) (g <> h) (p <|> q)

instance Monoid Rendered where
  mempty = Rendered "" "" Nothing

type Render = [Int] -> Affine2 -> Either SvgError Rendered

renderSvg :: (part -> String) -> SvgOptions -> Picture part -> Either SvgError String
renderSvg = renderDocument False

-- | The same picture and fixed camera, with an additional non-clipped guide
-- layer containing control polygons, anchors, local axes, and part labels.
renderDiagnosticSvg :: (part -> String) -> SvgOptions -> Picture part -> Either SvgError String
renderDiagnosticSvg = renderDocument True

renderDocument :: Bool -> (part -> String) -> SvgOptions -> Picture part -> Either SvgError String
renderDocument diagnostic label options@(SvgOptions viewport _ _ _ _) picture = do
  rendered <- foldPicture (svgAlgebra diagnostic label options) picture [] identityAffine2
  opening <- documentOpening options viewport
  pure $ opening <> artwork rendered
    <> (if diagnostic then "<g aria-label=\"authoring guides\">" <> guides rendered <> "</g>" else "")
    <> "</svg>"

svgAlgebra :: Bool -> (part -> String) -> SvgOptions -> PictureAlgebra part Render
svgAlgebra diagnostic label options = PictureAlgebra
  { paintSequence = \children address metric ->
      mconcat <$> traverse
        (\(index, child) -> child (address <> [index]) metric)
        (zip [0 ..] (toList children))
  , paintFill = \rule paint contours address metric -> do
      (definition, painted) <- renderPaint options (identifier "paint" address) paint
      (commands, controlGuides) <- closedCommands diagnostic options metric contours
      pure $ Rendered
        (definition <> element "path"
          [("d", commands), ("fill", painted), ("fill-rule", fillRuleText rule)])
        controlGuides (Just (location (NonEmpty.head contours)))
  , paintStroke = \style curve address metric -> do
      (definition, painted) <- renderPaint options (identifier "paint" address) (strokePaint style)
      width <- positiveNumber options (strokeWidth style)
      joins <- joinAttributes options (strokeJoin style)
      pieces <- traverse (subpathCommands diagnostic options metric) (toList (pathSubpaths curve))
      let commands = intercalate " " (fmap commandText pieces)
          anchors = foldr ((<|>) . commandAnchor) Nothing pieces
      pure $ Rendered
        (definition <> element "path"
          ([("d", commands), ("fill", "none"), ("stroke", painted)
           , ("stroke-width", width), ("stroke-linecap", capText (strokeCap style))]
           <> joins <> case strokeUnits style of
                LocalUnits -> []
                OutputUnits -> [("vector-effect", "non-scaling-stroke")]))
        (foldMap commandGuides pieces) anchors
  , paintClip = \rule contours child address metric -> do
      (commands, _) <- closedCommands False options metric contours
      body <- child (address <> [0]) metric
      let name = identifier "clip" address
          definition = "<defs><clipPath id=\"" <> name <> "\" clipPathUnits=\"userSpaceOnUse\">"
            <> element "path" [("d", commands), ("clip-rule", fillRuleText rule)]
            <> "</clipPath></defs>"
      pure body { artwork = definition <> group [("clip-path", "url(#" <> name <> ")")] (artwork body) }
  , paintPlace = \affine child address metric -> do
      matrix <- affineText options affine
      body <- child (address <> [0]) (composeAffine2 metric affine)
      axes <- if diagnostic then localAxes options else pure ""
      pure $ Rendered
        (group [("transform", matrix)] (artwork body))
        (group [("transform", matrix)] (guides body <> axes))
        (transformPoint affine <$> firstAnchor body)
  , paintOpacity = \amount child address metric -> do
      value <- number options (unitIntervalValue amount)
      body <- child (address <> [0]) metric
      pure body { artwork = group [("opacity", value)] (artwork body) }
  , paintAnnotation = \part child address metric -> do
      name <- escapeXml (label part)
      body <- child (address <> [0]) metric
      caption <- if diagnostic
        then maybe (pure "") (partCaption options name) (firstAnchor body)
        else pure ""
      pure body
        { artwork = group [("id", identifier "part" address), ("data-part", name)]
            ("<title>" <> name <> "</title>" <> artwork body)
        , guides = guides body <> caption
        }
  }

data Commands = Commands
  { commandText :: String
  , commandGuides :: String
  , commandAnchor :: Maybe ExactPoint
  }

closedCommands
  :: Bool -> SvgOptions -> Affine2 -> NonEmpty (Located ClosedTrail)
  -> Either SvgError (String, String)
closedCommands diagnostic options metric contours = do
  pieces <- traverse (subpathCommands diagnostic options metric . ClosedSubpath) (NonEmpty.toList contours)
  pure (intercalate " " (fmap commandText pieces), foldMap commandGuides pieces)

subpathCommands :: Bool -> SvgOptions -> Affine2 -> Subpath -> Either SvgError Commands
subpathCommands diagnostic options metric subpath = case subpath of
  OpenSubpath value -> renderTrail False (location value) (trailSteps (locatedValue value))
  ClosedSubpath value -> renderTrail True (location value) (closedTrailSteps (locatedValue value))
 where
  renderTrail :: Bool -> ExactPoint -> Seq CurveStep -> Either SvgError Commands
  renderTrail closed origin steps = do
    start <- pointText options origin
    let (_, locatedSteps) = mapAccumL
          (\point step -> (translateExactPoint point (curveStepEnd step), (point, step)))
          origin (toList steps)
    rendered <- traverse (uncurry (stepCommands options metric)) locatedSteps
    handles <- if diagnostic
      then foldMap id <$> traverse (uncurry (stepGuides options)) locatedSteps
      else pure ""
    pure $ Commands
      ("M " <> start <> foldMap (" " <>) rendered <> if closed then " Z" else "")
      handles (Just origin)

stepCommands :: SvgOptions -> Affine2 -> ExactPoint -> CurveStep -> Either SvgError String
stepCommands options@(SvgOptions _ _ tolerance depth leaves) metric origin step =
  case shapeView (curveStepShape step) of
    LinearView -> ("L " <>) <$> pointText options endpoint
    QuadraticView control -> polynomial "Q" [control] endpoint
    CubicView control1 control2 -> polynomial "C" [control1, control2] endpoint
    RationalQuadraticView _ _ _ -> do
      policy <- first SvgLoweringRefused $
        loweringPolicy tolerance (composeAffine2 (viewportMetric options) metric) depth leaves
      samples <- first SvgLoweringRefused (lowerStep policy (locate origin step))
      published <- traverse (pointText options) (NonEmpty.tail (loweredPoints samples))
      pure (intercalate " " (fmap ("L " <>) published))
 where
  endpoint = translateExactPoint origin (curveStepEnd step)
  polynomial opcode controls end =
    ((opcode <> " ") <>) . unwords <$> traverse (pointText options)
      (fmap (translateExactPoint origin) controls <> [end])

stepGuides :: SvgOptions -> ExactPoint -> CurveStep -> Either SvgError String
stepGuides options origin step = do
  let endpoint = translateExactPoint origin (curveStepEnd step)
      controls = case shapeView (curveStepShape step) of
        LinearView -> []
        QuadraticView c -> [c]
        CubicView c d -> [c, d]
        RationalQuadraticView c _ _ -> [c]
      points = [origin] <> fmap (translateExactPoint origin) controls <> [endpoint]
  coordinates <- traverse (pointText options) points
  markers <- traverse marker points
  pure $ element "polyline"
      [("points", unwords coordinates), ("fill", "none"), ("stroke", "#d52a96")
      , ("stroke-width", "0.7"), ("vector-effect", "non-scaling-stroke")]
    <> concat markers
 where
  marker point = do
    (x, y) <- pointAttributes options point
    pure (element "circle" [("cx", x), ("cy", y), ("r", "1.5"), ("fill", "#d52a96")])

renderPaint :: SvgOptions -> String -> Paint -> Either SvgError (String, String)
renderPaint options name paint = case paint of
  Solid color -> pure ("", colorText color)
  LinearGradient start end stops -> do
    (x1, y1) <- pointAttributes options start
    (x2, y2) <- pointAttributes options end
    renderedStops <- traverse (renderStop options) (gradientStopValues stops)
    pure (definition "linearGradient" [("x1", x1), ("y1", y1), ("x2", x2), ("y2", y2)]
      (foldMap id renderedStops), reference)
  RadialGradient center radius stops -> do
    (x, y) <- pointAttributes options center
    r <- positiveNumber options radius
    renderedStops <- traverse (renderStop options) (gradientStopValues stops)
    pure (definition "radialGradient" [("cx", x), ("cy", y), ("r", r)]
      (foldMap id renderedStops), reference)
 where
  reference = "url(#" <> name <> ")"
  definition tag attributes content =
    "<defs><" <> tag <> attributesText
      ([("id", name), ("gradientUnits", "userSpaceOnUse")] <> attributes)
      <> ">" <> content <> "</" <> tag <> "></defs>"

renderStop :: SvgOptions -> GradientStop -> Either SvgError String
renderStop options (GradientStop offset color alpha) = do
  position <- number options (unitIntervalValue offset)
  amount <- number options (unitIntervalValue alpha)
  pure (element "stop" [("offset", position), ("stop-color", colorText color), ("stop-opacity", amount)])

joinAttributes :: SvgOptions -> LineJoin -> Either SvgError [(String, String)]
joinAttributes options join = case join of
  RoundJoin -> pure [("stroke-linejoin", "round")]
  BevelJoin -> pure [("stroke-linejoin", "bevel")]
  MiterJoin limit -> do
    value <- number options (miterLimitValue limit)
    pure [("stroke-linejoin", "miter"), ("stroke-miterlimit", value)]

fillRuleText :: FillRule -> String
fillRuleText rule = case rule of NonZero -> "nonzero"; EvenOdd -> "evenodd"

capText :: LineCap -> String
capText cap = case cap of ButtCap -> "butt"; RoundCap -> "round"; SquareCap -> "square"

colorText :: Color -> String
colorText (RGB red green blue) = "#" <> foldMap channel [red, green, blue]
 where
  channel :: Word8 -> String
  channel value = let hex = showHex value "" in replicate (2 - length hex) '0' <> hex

identifier :: String -> [Int] -> String
identifier category address = category <> "-" <> intercalate "-" (fmap show address)

affineText :: SvgOptions -> Affine2 -> Either SvgError String
affineText options affine = do
  let (ExactVector a b, ExactVector c d, ExactVector e f) = affineColumns affine
  values <- traverse (number options) [a, b, c, d, e, f]
  pure ("matrix(" <> unwords values <> ")")

viewportMetric :: SvgOptions -> Affine2
viewportMetric (SvgOptions (SvgViewport width height _ extentX extentY) _ _ _ _) =
  affine2
    (ExactVector (divideByPositive (fromIntegral width) extentX) 0)
    (ExactVector 0 (divideByPositive (fromIntegral height) extentY))
    (ExactVector 0 0)

documentOpening :: SvgOptions -> SvgViewport -> Either SvgError String
documentOpening options (SvgViewport width height origin extentX extentY) = do
  xy <- pointText options origin
  x <- positiveNumber options extentX
  y <- positiveNumber options extentY
  pure $ "<svg xmlns=\"http://www.w3.org/2000/svg\""
    <> attributesText [("width", show width), ("height", show height)
       , ("viewBox", xy <> " " <> x <> " " <> y), ("preserveAspectRatio", "none")]
    <> ">"

partCaption :: SvgOptions -> String -> ExactPoint -> Either SvgError String
partCaption options name point = do
  (x, y) <- pointAttributes options point
  pure $ "<text" <> attributesText
    [("x", x), ("y", y), ("dx", "4"), ("dy", "-4"), ("fill", "#991b75")
    , ("font-size", "8"), ("font-family", "monospace")]
    <> ">" <> name <> "</text>"

localAxes :: SvgOptions -> Either SvgError String
localAxes options = do
  origin <- pointText options (exactPoint 0 0)
  x <- pointText options (exactPoint 12 0)
  y <- pointText options (exactPoint 0 12)
  pure $ element "path" [("d", "M " <> origin <> " L " <> x <> " M " <> origin <> " L " <> y)
    , ("fill", "none"), ("stroke", "#288e94"), ("stroke-width", "0.7")
    , ("vector-effect", "non-scaling-stroke")]

pointAttributes :: SvgOptions -> ExactPoint -> Either SvgError (String, String)
pointAttributes options point =
  let (x, y) = exactPointCoordinates point
   in (,) <$> number options x <*> number options y

pointText :: SvgOptions -> ExactPoint -> Either SvgError String
pointText options point = (\(x, y) -> x <> " " <> y) <$> pointAttributes options point

positiveNumber :: SvgOptions -> PositiveExact -> Either SvgError String
positiveNumber options value = do
  text <- number options (positiveExactValue value)
  if all (`elem` ("0." :: String)) text
    then Left (SvgPositiveValueRoundedToZero (positiveExactValue value))
    else Right text

-- Integer-only decimal publication. Ties round away from zero; no Double
-- conversion, exponent overflow, negative zero, or unchecked printf occurs.
number :: SvgOptions -> ExactRational -> Either SvgError String
number (SvgOptions _ (SvgPrecision digits tolerance) _ _ _) value
  | bits > 4096 = Left (SvgNumericMagnitudeRefused bits)
  | rounded > largestFinite * scale = Left (SvgNumericRangeRefused value)
  | errorNumerator * exactRationalDenominator allowed
      > exactRationalNumerator allowed * denominator * scale =
      Left (SvgNumericPrecisionRefused value digits)
  | otherwise = Right (sign <> whole <> fraction)
 where
  bits = exactRationalBitWidth value
  largestFinite = (2 ^ (53 :: Int) - 1) * 2 ^ (971 :: Int)
  numerator = exactRationalNumerator value
  denominator = exactRationalDenominator value
  scale = 10 ^ digits
  (quotient, remainder) = (abs numerator * scale) `quotRem` denominator
  rounded = quotient + if 2 * remainder >= denominator then 1 else 0
  signed = if numerator < 0 then negate rounded else rounded
  errorNumerator = abs (numerator * scale - signed * denominator)
  allowed = positiveExactValue tolerance
  (integerPart, fractionalPart) = rounded `quotRem` scale
  whole = show integerPart
  fractionalText = show fractionalPart
  fraction = if digits == 0 then "" else "." <> replicate (digits - length fractionalText) '0' <> fractionalText
  sign = if signed < 0 then "-" else ""

-- Attributes reaching these private printers are generated tokens, certified
-- numbers, or text already passed through escapeXml. No public attribute bag.
attributesText :: [(String, String)] -> String
attributesText = foldMap (\(name, value) -> " " <> name <> "=\"" <> value <> "\"")

element :: String -> [(String, String)] -> String
element tag attributes = "<" <> tag <> attributesText attributes <> "/>"

group :: [(String, String)] -> String -> String
group attributes body = "<g" <> attributesText attributes <> ">" <> body <> "</g>"

escapeXml :: String -> Either SvgError String
escapeXml = fmap concat . traverse escape
 where
  escape char = case char of
    '&' -> Right "&amp;"
    '<' -> Right "&lt;"
    '>' -> Right "&gt;"
    '"' -> Right "&quot;"
    '\'' -> Right "&apos;"
    _ | validXml char -> Right [char]
      | otherwise -> Left (SvgInvalidXmlCharacter char)
  validXml char = let code = ord char in
    code == 9 || code == 10 || code == 13
      || code >= 0x20 && code <= 0xd7ff
      || code >= 0xe000 && code <= 0xfffd
      || code >= 0x10000 && code <= 0x10ffff
