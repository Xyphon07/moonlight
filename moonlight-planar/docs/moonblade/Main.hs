module Main (main) where

import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Moonlight.Planar.Exhibit.Moonblade.Crescent as Crescent
import Moonlight.Planar.Exhibit.Geometry (ShapeError, Triangle (..), triangulateShape)
import Moonlight.Planar.Exhibit.Svg (circle, polygon, pointText, scalar)
import Moonlight.Planar.Point (Point (..))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import Text.Printf (printf)

data Material = Moonsteel | PaleGold

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [outputPath] -> either (die . show) (writeFile outputPath) moonblade
    ["crescent-solid", outputPath] -> either (die . show) (writeFile outputPath) Crescent.solid
    ["crescent-frames", outputDirectory] -> either (die . show)
      (\frames -> createDirectoryIfMissing True outputDirectory
        *> traverse_ (\(name, svg) -> writeFile (outputDirectory </> name) svg) frames)
      Crescent.frames
    _ -> die "usage: moonlight-planar-moonblade OUTPUT.svg | crescent-frames OUTPUT_DIRECTORY | crescent-solid OUTPUT.obj"

moonblade :: Either ShapeError String
moonblade = do
  blade <- triangulateShape (bladeOutline :| []) bladeSeeds
  guard <- triangulateShape (guardOutline :| []) guardSeeds
  pure $
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1400\" height=\"1600\" viewBox=\"0 0 1400 1600\">"
      <> "<title>Vesper - the astral moonblade</title><desc>A straight broad moon-glass blade with a luminous turquoise edge, flowing constellations, a symmetric bronze star guard, and a leather-wrapped grip. "
      <> show (length blade) <> " blade facets and " <> show (length guard)
      <> " guard facets are native Moonlight Planar constrained Delaunay triangles. Subtle facets support the materials rather than dominating the design. Light, ornament and composition are authored SVG.</desc>"
      <> definitions <> background
      <> "<g transform=\"translate(700 1110)\">"
      <> bladeAura
      <> hilt
      <> polygon "fill=\"url(#moonglass)\" stroke=\"#033c49\" stroke-width=\"11\" stroke-linejoin=\"round\"" (NonEmpty.toList bladeOutline)
      <> foldMap (uncurry (renderFacet Moonsteel)) (zip [0 ..] blade)
      <> bladeEdges
      <> bladeEngraving
      <> polygon "fill=\"url(#bronze)\" stroke=\"#241810\" stroke-width=\"9\" stroke-linejoin=\"round\"" (NonEmpty.toList guardOutline)
      <> foldMap (uncurry (renderFacet PaleGold)) (zip [0 ..] guard)
      <> polygon "fill=\"none\" stroke=\"#ebc493\" stroke-width=\"2.5\" stroke-linejoin=\"round\"" (NonEmpty.toList guardOutline)
      <> guardFiligree
      <> fittings
      <> "</g>" <> typography <> "</svg>"

bladePoint :: Double -> Double -> Point
bladePoint side t = quantize $
  Point (side * bladeWidth t) (-1000 * t)

bladeWidth :: Double -> Double
bladeWidth t = (100 - 6 * t) * if t <= 0.76 then 1 else cos (pi * (t - 0.76) / 0.48)

bladeOutline :: NonEmpty Point
bladeOutline = bladePoint (-1) 0 :|
  (fmap (bladePoint (-1) . fraction) [1 .. 48] <> fmap (bladePoint 1 . fraction) [47, 46 .. 0])

bladeSeeds :: [Point]
bladeSeeds =
  [ bladePoint (side * (0.2 + 0.5 * abs (sin (fromIntegral index * 1.71)))) ((fromIntegral index + 0.5) / 64)
  | index <- [0 .. 63 :: Int], side <- [-1, 1]
  ]

guardOutline :: NonEmpty Point
guardOutline = Point 0 (-138) :|
  (rightSide <> [Point 0 76] <> fmap mirror (reverse rightSide))
 where
  rightSide :: [Point]
  rightSide =
    [ Point 18 (-115), Point 27 (-39), Point 81 (-49)
    , Point 156 (-105), Point 174 (-98), Point 187 (-80), Point 173 (-63)
    , Point 111 (-13), Point 183 (-20), Point 196 (-7), Point 196 18
    , Point 182 29, Point 110 24, Point 171 77, Point 170 91
    , Point 152 108, Point 136 103, Point 64 53, Point 25 57, Point 17 76
    ]
  mirror :: Point -> Point
  mirror (Point x y) = Point (-x) y

guardSeeds :: [Point]
guardSeeds = [Point 0 0, Point 0 (-75), Point (-65) 0, Point 65 0, Point (-123) (-60), Point 123 (-60), Point (-120) 68, Point 120 68]

fraction :: Int -> Double
fraction index = fromIntegral index / 48

quantize :: Point -> Point
quantize (Point x y) = Point (snap x) (snap y)
 where
  snap :: Double -> Double
  snap value = fromIntegral (round (1024 * value) :: Integer) / 1024

renderFacet :: Material -> Int -> Triangle -> String
renderFacet material index (Triangle a@(Point ax _) b@(Point bx _) c@(Point cx _)) =
  let across = abs (ax + bx + cx) / 300
      shimmer = 0.5 + 0.5 * sin (fromIntegral index * 1.719)
      color = case material of
        Moonsteel -> rgb (16 + 12 * shimmer) (66 + 40 * across) (80 + 32 * across)
        PaleGold -> rgb (140 + 85 * shimmer) (94 + 78 * shimmer) (57 + 60 * shimmer)
      opacity = case material of Moonsteel -> "0.24"; PaleGold -> "0.17"
   in polygon ("fill=\"" <> color <> "\" fill-opacity=\"" <> opacity <> "\"") [a, b, c]

rgb :: Double -> Double -> Double -> String
rgb red green blue = printf "#%02x%02x%02x" (round red :: Int) (round green :: Int) (round blue :: Int)

bladeEdges :: String
bladeEdges =
  foldMap bevel [-1, 1]
  <> polygon "fill=\"none\" stroke=\"#43eadf\" stroke-width=\"7\" stroke-opacity=\"0.35\" stroke-linejoin=\"round\"" (NonEmpty.toList bladeOutline)
  <> polygon "fill=\"none\" stroke=\"#b4fff4\" stroke-width=\"2.5\" stroke-linejoin=\"round\"" (NonEmpty.toList bladeOutline)
 where
  bevel :: Double -> String
  bevel side = polygon "fill=\"url(#edge)\" fill-opacity=\"0.75\""
    (fmap (bladePoint side . fraction) [0 .. 48] <> fmap (bladePoint (side * 0.91) . fraction) [47, 46 .. 0])

bladeAura :: String
bladeAura = foldMap halo [0 .. 19 :: Int]
 where
  halo :: Int -> String
  halo index = polygon
    ("fill=\"none\" stroke=\"#30efd8\" stroke-width=\"" <> scalar (10 + 4 * fromIntegral index)
      <> "\" stroke-opacity=\"" <> scalar (0.05 * exp (negate (fromIntegral index) / 7))
      <> "\" stroke-linejoin=\"round\"")
    (NonEmpty.toList bladeOutline)

bladeEngraving :: String
bladeEngraving =
  "<g fill=\"none\" stroke=\"#49d8d2\" stroke-linecap=\"round\">"
  <> foldMap filament [0, 1, 2 :: Int]
  <> "<path d=\"M 22 -715 A 54 65 0 1 0 22 -596 A 43 63 0 0 1 22 -715 Z\" fill=\"#8defe0\" fill-opacity=\"0.07\" stroke-opacity=\"0.33\" stroke-width=\"1\"/>"
  <> "<path d=\"M 0 -927 L 8 -906 L 0 -883 L -8 -906 Z M 0 -941 V -930 M 0 -880 V -864\" stroke-opacity=\"0.58\" stroke-width=\"1.2\"/></g>"
  <> foldMap mote [0 .. 22 :: Int]
  <> "<g fill=\"none\" stroke=\"#8af4e5\" stroke-width=\"0.9\" stroke-opacity=\"0.36\"><path d=\"M -54 -309 L -17 -244 L 47 -356 M -54 -309 L 13 -417 L 47 -356\"/></g>"
 where
  filament :: Int -> String
  filament index =
    let phase = fromIntegral index * 1.9
        points = [Point (51 * sin (8.5 * t + phase) * sin (pi * t)) (-1000 * t) | sample <- [0 .. 96 :: Int], let t = 0.16 + 0.69 * fromIntegral sample / 96]
     in "<polyline points=\"" <> unwords (pointText <$> points) <> "\" stroke-width=\""
        <> scalar (if index == 0 then 2 else 0.8) <> "\" stroke-opacity=\"0.3\"/>"
  mote :: Int -> String
  mote index =
    let t = 0.16 + 0.68 * fromIntegral index / 23
        point = Point (60 * sin (fromIntegral index * 2.399)) (-1000 * t)
        radius = if index `mod` 4 == 0 then 3 else 1.5
     in circle point (radius * 4) "fill=\"url(#mote)\""
        <> circle point radius "fill=\"#b0fff0\""
        <> circle point (radius * 0.4) "fill=\"#f0fff8\""

hilt :: String
hilt =
  "<path d=\"M -31 49 H 31 L 27 248 L 34 263 H -34 L -27 248 Z\" fill=\"url(#grip)\" stroke=\"#3a261b\" stroke-width=\"4\"/>"
  <> foldMap binding [0 .. 8 :: Int]
  <> "<g fill=\"url(#bronze)\" stroke=\"#42291c\" stroke-width=\"2\"><rect x=\"-35\" y=\"72\" width=\"70\" height=\"13\" rx=\"3\"/><rect x=\"-32\" y=\"242\" width=\"64\" height=\"10\" rx=\"2\"/><rect x=\"-36\" y=\"258\" width=\"72\" height=\"11\" rx=\"3\"/></g>"
  <> "<path d=\"M -31 75 H 31 M -29 245 H 29 M -31 261 H 31\" stroke=\"#ecc391\" stroke-width=\"1.4\"/>"
 where
  binding :: Int -> String
  binding index =
    let y = 89 + fromIntegral index * 17
     in "<g transform=\"translate(0 " <> scalar y <> ")\"><path d=\"M -28 0 Q 0 12 28 17 M 28 0 Q 0 12 -28 17\" stroke=\"#31221b\" stroke-width=\"4\" fill=\"none\"/>"
        <> "<path d=\"M -28 -1 Q 0 11 28 16 M 28 -1 Q 0 11 -28 16\" stroke=\"#be8960\" stroke-width=\"1.6\" fill=\"none\"/>"
        <> "<path d=\"M 0 6 L 3 9 L 0 12 L -3 9 Z\" fill=\"#d3a268\"/></g>"

guardFiligree :: String
guardFiligree =
  polygon "fill=\"none\" stroke=\"#784a30\" stroke-width=\"3\" stroke-linejoin=\"round\""
    (fmap (\(Point x y) -> Point (0.94 * x) (0.88 * y)) (NonEmpty.toList guardOutline))
  <> "<path d=\"M -157 -83 L -62 -6 L 0 10 L 62 -6 L 157 -83 M -149 86 L -50 25 L 0 10 L 50 25 L 149 86 M 0 -117 V 53 M -178 6 H -78 M 78 6 H 178\" fill=\"none\" stroke=\"#75432a\" stroke-width=\"11\" stroke-linejoin=\"round\"/>"
  <> "<path d=\"M -157 -86 L -62 -9 L 0 7 L 62 -9 L 157 -86 M -149 83 L -50 22 L 0 7 L 50 22 L 149 83 M 0 -117 V 53 M -178 3 H -78 M 78 3 H 178\" fill=\"none\" stroke=\"#f3ce95\" stroke-width=\"2.5\" stroke-linejoin=\"round\"/>"
  <> "<path d=\"M -22 -33 L 0 -103 L 22 -33 L 0 7 Z\" fill=\"url(#bronze)\" stroke=\"#e5b47c\" stroke-width=\"1.2\"/>"

fittings :: String
fittings =
  "<path d=\"M 0 -17 L 8 0 L 27 8 L 8 16 L 0 36 L -8 16 L -27 8 L -8 0 Z\" fill=\"#8b5632\" stroke=\"#f0c890\" stroke-width=\"1.5\"/>"
  <> circle (Point 0 8) 17 "fill=\"url(#amber)\""
  <> "<path d=\"M 0 -7 L 3 5 L 14 8 L 3 11 L 0 25 L -3 11 L -14 8 L -3 5 Z\" fill=\"#ffe4a4\"/>"
  <> "<path d=\"M -32 270 H 32 L 42 284 V 324 L 29 340 H -29 L -42 324 V 284 Z\" fill=\"url(#bronze)\" stroke=\"#40291e\" stroke-width=\"4\"/>"
  <> "<path d=\"M -27 279 H 27 L 32 288 V 319 L 24 330 H -24 L -32 319 V 288 Z\" fill=\"#6c4938\" stroke=\"#e5b17c\" stroke-width=\"1.2\"/>"
  <> circle (Point 0 304) 25 "fill=\"url(#amber)\""
  <> "<path d=\"M 0 289 L 5 301 L 0 316 L -5 301 Z\" fill=\"#eec67e\"/>"
  <> "<ellipse cx=\"0\" cy=\"345\" rx=\"29\" ry=\"14\" fill=\"url(#gem)\" stroke=\"#4a3426\" stroke-width=\"3\"/><path d=\"M -43 334 H 43 L 38 344 H -38 Z\" fill=\"url(#bronze)\" stroke=\"#593925\" stroke-width=\"2\"/><path d=\"M -38 337 H 38\" stroke=\"#e7b982\" stroke-width=\"1.2\"/>"

background :: String
background =
  "<rect width=\"1400\" height=\"1600\" fill=\"url(#night)\"/>"
  <> "<ellipse cx=\"700\" cy=\"665\" rx=\"360\" ry=\"740\" fill=\"url(#atmosphere)\"/>"
  <> "<g fill=\"none\" stroke=\"#597275\" stroke-width=\"0.7\" opacity=\"0.23\"><circle cx=\"700\" cy=\"655\" r=\"360\"/><circle cx=\"700\" cy=\"655\" r=\"368\" stroke-dasharray=\"1 12\"/><path d=\"M 302 655 H 345 M 1055 655 H 1098 M 700 256 V 300 M 700 1010 V 1055\"/></g>"
  <> foldMap star [0 .. 139 :: Int]
  <> "<ellipse cx=\"700\" cy=\"1490\" rx=\"109\" ry=\"11\" fill=\"#000706\" opacity=\"0.4\"/>"
 where
  star :: Int -> String
  star index = circle
    (Point (55 + fromIntegral ((index * 947 + 71) `mod` 1290)) (55 + fromIntegral ((index * 389 + 173) `mod` 1490)))
    (if index `mod` 17 == 0 then 1 else 0.5) "fill=\"#94b9b0\" opacity=\"0.2\""

definitions :: String
definitions = "<defs>"
  <> "<radialGradient id=\"night\" cx=\"50%\" cy=\"43%\" r=\"75%\"><stop stop-color=\"#163035\"/><stop offset=\"0.5\" stop-color=\"#101f25\"/><stop offset=\"1\" stop-color=\"#091218\"/></radialGradient>"
  <> "<radialGradient id=\"atmosphere\"><stop stop-color=\"#126b66\" stop-opacity=\"0.14\"/><stop offset=\"1\" stop-color=\"#126b66\" stop-opacity=\"0\"/></radialGradient>"
  <> "<linearGradient id=\"moonglass\"><stop stop-color=\"#117b80\"/><stop offset=\"0.10\" stop-color=\"#14535e\"/><stop offset=\"0.28\" stop-color=\"#183a49\"/><stop offset=\"0.55\" stop-color=\"#142e3e\"/><stop offset=\"0.85\" stop-color=\"#155563\"/><stop offset=\"1\" stop-color=\"#1ca1a1\"/></linearGradient>"
  <> "<linearGradient id=\"edge\"><stop stop-color=\"#e0fff2\"/><stop offset=\"0.4\" stop-color=\"#4adccf\"/><stop offset=\"1\" stop-color=\"#a9fff2\"/></linearGradient>"
  <> "<linearGradient id=\"bronze\" x1=\"0\" y1=\"0\" x2=\"0.7\" y2=\"1\"><stop stop-color=\"#775038\"/><stop offset=\"0.23\" stop-color=\"#c88f5e\"/><stop offset=\"0.46\" stop-color=\"#efc28b\"/><stop offset=\"0.53\" stop-color=\"#a46c46\"/><stop offset=\"0.80\" stop-color=\"#be875a\"/><stop offset=\"1\" stop-color=\"#6d4833\"/></linearGradient>"
  <> "<linearGradient id=\"grip\"><stop stop-color=\"#37271e\"/><stop offset=\"0.45\" stop-color=\"#805637\"/><stop offset=\"0.68\" stop-color=\"#68482f\"/><stop offset=\"1\" stop-color=\"#2d211b\"/></linearGradient>"
  <> "<radialGradient id=\"gem\" cx=\"35%\" cy=\"22%\"><stop stop-color=\"#81e4d1\"/><stop offset=\"0.25\" stop-color=\"#188e98\"/><stop offset=\"1\" stop-color=\"#122e3e\"/></radialGradient>"
  <> "<radialGradient id=\"mote\"><stop stop-color=\"#69fce5\" stop-opacity=\"0.75\"/><stop offset=\"0.3\" stop-color=\"#39d5d0\" stop-opacity=\"0.3\"/><stop offset=\"1\" stop-color=\"#39d5d0\" stop-opacity=\"0\"/></radialGradient>"
  <> "<radialGradient id=\"amber\"><stop stop-color=\"#ffd889\" stop-opacity=\"0.8\"/><stop offset=\"1\" stop-color=\"#f9b44d\" stop-opacity=\"0\"/></radialGradient>"
  <> "</defs>"

typography :: String
typography =
  "<g fill=\"#bfbaa8\"><text x=\"90\" y=\"108\" font-family=\"Helvetica, sans-serif\" font-size=\"11\" letter-spacing=\"4\">MOONLIGHT / RELIQUARY</text>"
  <> "<text x=\"85\" y=\"249\" font-family=\"Georgia, serif\" font-size=\"73\" letter-spacing=\"-2\">Vesper</text>"
  <> "<text x=\"91\" y=\"283\" font-family=\"Helvetica, sans-serif\" font-size=\"11\" letter-spacing=\"4\">THE ASTRAL MOONBLADE</text>"
  <> "<path d=\"M 91 308 H 280\" stroke=\"#667d79\" stroke-width=\"0.7\"/></g>"
  <> "<g fill=\"#839f9d\" font-family=\"Georgia, serif\" font-size=\"17\" font-style=\"italic\"><text x=\"91\" y=\"346\">A small sky, held in bronze.</text></g>"
  <> "<g fill=\"#77918e\" font-family=\"Helvetica, sans-serif\" font-size=\"10\" letter-spacing=\"2\"><text x=\"91\" y=\"1536\">BRONZE / MOONGLASS / STARLIGHT</text><text x=\"1309\" y=\"1536\" text-anchor=\"end\">PLANAR STUDY 02</text></g>"
