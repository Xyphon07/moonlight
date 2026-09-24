module Moonlight.Planar.Exhibit.Moonblade.Crescent (frames, solid) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Moonlight.Planar.Exhibit.Geometry (ShapeError, Triangle (..), solidObj, triangulateShape)
import Moonlight.Planar.Exhibit.Svg (circle, polygon, scalar)
import Moonlight.Planar.Point (Point (..))
import Text.Printf (printf)

data Material = Moonsteel | PaleGold

solid :: Either ShapeError String
solid = do
  blade <- triangulateShape (bladeOutline :| [fullerOutline]) bladeSeeds
  guard <- triangulateShape (guardOutline :| []) guardSeeds
  pure $ "# Vesper: native crescent blade and guard, metres, Z up.\n"
    <> solidObj "Blade" bladeThickness blade
    <> solidObj "Guard" (const 0.012) guard
 where
  bladeThickness :: Point -> Double
  bladeThickness (Point x y) =
    let t = max 0 (min 1 (negate y / 900))
        across = abs ((x - bladeCenter t) / max 1 (bladeWidth t))
        edge = 0.00035 + 0.00085 * min 1 ((1 - t) / 0.05)
        tipTaper = min 1 ((1 - t) / 0.12)
     in edge + 0.009 * (1 - min 1 across) * (1 - 0.4 * t) * tipTaper

-- One immutable native mesh, sampled at 80 phases of a 6.4-second loop.
-- Only the rigid pose and illumination vary; the silhouette never deforms.
frames :: Either ShapeError [(FilePath, String)]
frames = do
  blade <- triangulateShape (bladeOutline :| [fullerOutline]) bladeSeeds
  guard <- triangulateShape (guardOutline :| []) guardSeeds
  let bladeFacets = foldMap (uncurry (renderFacet Moonsteel)) (zip [0 ..] blade)
      guardFacets = foldMap (uncurry (renderFacet PaleGold)) (zip [0 ..] guard)
      clip = "<defs><clipPath id=\"blade-clip\">"
        <> foldMap (\(Triangle a b c) -> polygon "" [a, b, c]) blade
        <> "</clipPath></defs>"
      render phase =
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1200\" height=\"900\" viewBox=\"0 0 1600 1200\">"
        <> "<title>Vesper - the crescent moonblade</title><desc>A rigid hovering moonblade with pulsing runes and a traveling reflection. Native constrained Delaunay facets preserve the pierced fuller.</desc>"
        <> definitions <> clip <> background phase
        <> "<g transform=\"translate(" <> scalar (525 + 5 * sin (2 * pi * phase))
        <> " " <> scalar (870 - 7 * (1 - cos (2 * pi * phase)))
        <> ") rotate(" <> scalar (40 + 0.6 * sin (2 * pi * phase)) <> ")\">"
        <> bladeAura phase <> hilt <> bladeFacets <> bladeEdges <> lightSweep phase
        <> polygon "fill=\"none\" stroke=\"#a6dbe9\" stroke-width=\"1.1\"" (NonEmpty.toList fullerOutline)
        <> bladeEngraving phase <> guardFacets
        <> polygon "fill=\"none\" stroke=\"#f5e4b7\" stroke-width=\"1.6\"" (NonEmpty.toList guardOutline)
        <> guardFiligree <> fittings
        <> "</g>" <> typography <> "</svg>"
  pure $ fmap (\index -> (printf "frame-%03d.svg" index, render (fromIntegral index / 80))) [0 .. 79 :: Int]

bladePoint :: Double -> Double -> Point
bladePoint side t = quantize $
  Point (bladeCenter t + side * bladeWidth t) (-900 * t)

bladeCenter :: Double -> Double
bladeCenter t = 12 * t + 40 * t * t

bladeWidth :: Double -> Double
bladeWidth t = (62 - 22 * t) * if t <= 0.76 then 1 else ((1 - t) / 0.24) ** 0.72

bladeOutline :: NonEmpty Point
bladeOutline = bladePoint (-1) 0 :|
  (fmap (bladePoint (-1) . fraction) [1 .. 48] <> fmap (bladePoint 1 . fraction) [47, 46 .. 0])

fullerPoint :: Double -> Double -> Point
fullerPoint side u =
  let t = 0.12 + 0.22 * u
   in quantize (Point (bladeCenter t + side * 7 * sin (pi * u)) (-900 * t))

fullerOutline :: NonEmpty Point
fullerOutline = fullerPoint (-1) 0 :|
  (fmap (fullerPoint (-1) . fraction) [1 .. 48] <> fmap (fullerPoint 1 . fraction) [47, 46 .. 1])

bladeSeeds :: [Point]
bladeSeeds = fmap
  (\index -> bladePoint (0.77 * sin (fromIntegral index * 2.39996)) ((fromIntegral index + 0.5) / 118))
  [0 .. 117 :: Int]

guardPoint :: Double -> Double -> Point
guardPoint height t = quantize (Point (-168 + 336 * t) (-75 + height * sin (pi * t)))

guardOutline :: NonEmpty Point
guardOutline = guardPoint 135 0 :|
  (fmap (guardPoint 135 . fraction) [1 .. 48] <> fmap (guardPoint 88 . fraction) [47, 46 .. 1])

guardSeeds :: [Point]
guardSeeds = fmap
  (\index -> guardPoint (110 + 13 * sin (fromIntegral index * 2.4)) ((fromIntegral index + 0.5) / 35))
  [0 .. 34 :: Int]

fraction :: Int -> Double
fraction index = fromIntegral index / 48

quantize :: Point -> Point
quantize (Point x y) = Point (snap x) (snap y)
 where
  snap :: Double -> Double
  snap value = fromIntegral (round (1024 * value) :: Integer) / 1024

renderFacet :: Material -> Int -> Triangle -> String
renderFacet material index (Triangle a@(Point ax ay) b@(Point bx by) c@(Point cx cy)) =
  let t = negate (ay + by + cy) / 2700
      across = ((ax + bx + cx) / 3 - bladeCenter t) / max 1 (bladeWidth (max 0 (min 1 t)))
      shimmer = 0.5 + 0.5 * sin (fromIntegral index * 1.719)
      light = max 0 (min 1 (0.35 + 0.23 * shimmer + 0.3 * across))
      color = case material of
        Moonsteel -> rgb (18 + 114 * light) (42 + 162 * light) (64 + 180 * light)
        PaleGold -> rgb (102 + 137 * shimmer) (92 + 128 * shimmer) (69 + 112 * shimmer)
      stroke = case material of Moonsteel -> "#a6d9e8"; PaleGold -> "#e9d9af"
   in polygon ("fill=\"" <> color <> "\" stroke=\"" <> stroke
      <> "\" stroke-width=\"0.48\" stroke-opacity=\"0.24\"") [a, b, c]

rgb :: Double -> Double -> Double -> String
rgb red green blue = printf "#%02x%02x%02x" (round red :: Int) (round green :: Int) (round blue :: Int)

bladeEdges :: String
bladeEdges =
  polygon "fill=\"url(#silver)\"" (fmap (bladePoint 1 . fraction) [0 .. 48] <> fmap (bladePoint 0.78 . fraction) [47, 46 .. 0])
  <> polygon "fill=\"#d8f4f1\" fill-opacity=\"0.75\"" (fmap (bladePoint (-1) . fraction) [0 .. 48] <> fmap (bladePoint (-0.94) . fraction) [47, 46 .. 0])
  <> polygon "fill=\"#d3f1ed\" fill-opacity=\"0.55\"" (fmap (bladePoint 0.08 . fraction) [18 .. 48] <> fmap (bladePoint 0.04 . fraction) [47, 46 .. 18])
  <> polygon "fill=\"none\" stroke=\"#deffff\" stroke-width=\"1.4\"" (NonEmpty.toList bladeOutline)

bladeAura :: Double -> String
bladeAura phase = "<g filter=\"url(#aura)\" opacity=\""
  <> scalar (0.85 + 0.15 * cos (2 * pi * phase)) <> "\">"
  <> polygon "fill=\"none\" stroke=\"#56cbf7\" stroke-width=\"11\" stroke-opacity=\"0.45\"" (NonEmpty.toList bladeOutline)
  <> "</g>"

-- The light is clipped to the selected triangles, so the real hole stays empty.
-- The band lies entirely outside the blade at both ends of the cycle.
lightSweep :: Double -> String
lightSweep phase = "<g clip-path=\"url(#blade-clip)\"><rect x=\"-100\" y=\""
  <> scalar (100 - 1250 * phase)
  <> "\" width=\"300\" height=\"200\" fill=\"url(#reflection)\"/></g>"

bladeEngraving :: Double -> String
bladeEngraving phase = foldMap rune [0 .. 8 :: Int]
 where
  rune :: Int -> String
  rune index =
    let t = 0.43 + fromIntegral index * 0.047
        Point x y = bladePoint (-0.36) t
        marking = case index `mod` 3 of
          0 -> "M -5 -6 L 0 0 L -5 6 M 0 0 L 7 0 M 3 -4 L 7 0 L 3 4"
          1 -> "M 0 -7 L 0 7 M -5 -3 L 0 2 L 5 -3 M -3 7 L 3 7"
          _ -> "M -5 0 L 0 -7 L 5 0 L 0 7 Z M -7 0 L 7 0"
     in "<path transform=\"translate(" <> scalar x <> " " <> scalar y <> ") rotate("
        <> scalar (atan ((12 + 80 * t) / 900) * 180 / pi) <> ")\" d=\"" <> marking
        <> "\" fill=\"none\" stroke=\"#e4e7bd\" stroke-width=\"1.2\" stroke-linecap=\"round\" opacity=\""
        <> scalar (0.72 + 0.23 * cos (2 * pi * phase - fromIntegral index * 0.7)) <> "\"/>"

hilt :: String
hilt =
  "<path d=\"M -24 21 L 24 21 L 20 193 L 0 214 L -20 193 Z\" fill=\"url(#grip)\" stroke=\"#899da7\" stroke-width=\"2\"/>"
  <> foldMap binding [0 .. 11 :: Int]
  <> "<path d=\"M -24 29 H 24 M -22 40 H 22 M -20 182 H 20 M -20 192 H 20\" stroke=\"#d7cba8\" stroke-width=\"5\"/>"
 where
  binding :: Int -> String
  binding index =
    let y = 48 + fromIntegral index * 11
     in "<path d=\"M -22 " <> scalar y <> " L 21 " <> scalar (y + 13)
        <> " M 21 " <> scalar y <> " L -22 " <> scalar (y + 13)
        <> "\" stroke=\"#55666e\" stroke-width=\"2\"/><path d=\"M -20 " <> scalar y
        <> " L 20 " <> scalar (y + 13) <> "\" stroke=\"#b4b1a0\" stroke-width=\"0.5\"/>"

guardFiligree :: String
guardFiligree =
  "<path d=\"M -146 -48 Q -85 42 0 36 Q 85 42 146 -48 M -125 -26 Q -68 47 -22 35 M 125 -26 Q 68 47 22 35\" fill=\"none\" stroke=\"#f8edc7\" stroke-width=\"1\"/>"
  <> foldMap (\index -> circle (guardPoint 112 (fromIntegral index / 16)) 1.8 "fill=\"#f5e3b2\" stroke=\"#6e664f\" stroke-width=\"0.5\"") [1 .. 15 :: Int]

fittings :: String
fittings =
  "<path d=\"M 0 -43 L 31 2 L 0 50 L -31 2 Z\" fill=\"#132d3c\" stroke=\"#ddd0a9\" stroke-width=\"3\"/>"
  <> "<path d=\"M 0 -28 L 18 2 L 0 32 L -18 2 Z\" fill=\"url(#gem)\" stroke=\"#b4e6ea\" stroke-width=\"1\"/>"
  <> "<path d=\"M 0 -28 L 0 32 M -18 2 H 18\" stroke=\"#deffff\" stroke-opacity=\"0.5\" stroke-width=\"0.7\"/>"
  <> circle (Point 0 226) 34 "fill=\"#101e2a\" stroke=\"#cec39e\" stroke-width=\"3\""
  <> circle (Point 0 226) 28 "fill=\"url(#gem)\" stroke=\"#719bb0\" stroke-width=\"0.8\""
  <> "<path d=\"M 9 202 A 25 25 0 1 0 9 250 A 27 27 0 0 1 9 202 Z\" fill=\"#e7e7cd\"/>"
  <> "<path d=\"M 0 261 L 0 277 M -4 269 L 0 277 L 4 269\" stroke=\"#cec39e\" fill=\"none\" stroke-width=\"2\"/>"

background :: Double -> String
background phase =
  "<rect width=\"1600\" height=\"1200\" fill=\"url(#night)\"/>"
  <> "<g opacity=\"" <> scalar (0.85 + 0.15 * cos (2 * pi * phase)) <> "\">"
  <> circle (Point 1040 337) 379 "fill=\"url(#moon-halo)\""
  <> circle (Point 1040 337) 264 "fill=\"url(#moon)\" stroke=\"#8bafbd\" stroke-opacity=\"0.16\" stroke-width=\"0.8\""
  <> "</g><g fill=\"none\" stroke=\"#7593a1\" stroke-width=\"0.55\" opacity=\"0.24\"><circle cx=\"1040\" cy=\"337\" r=\"302\"/><circle cx=\"1040\" cy=\"337\" r=\"310\" stroke-dasharray=\"1 9\"/><path d=\"M 718 139 A 379 379 0 0 1 1349 557\"/></g>"
  <> foldMap star [0 .. 219 :: Int]
  <> "<path d=\"M 1166 135 V 191 M 1138 163 H 1194\" stroke=\"#c4eff2\" stroke-width=\"0.8\" opacity=\""
  <> scalar (0.65 + 0.15 * cos (2 * pi * phase)) <> "\"/>"
 where
  star :: Int -> String
  star index = circle
    (Point (45 + fromIntegral ((index * 947 + 71) `mod` 1510)) (45 + fromIntegral ((index * 389 + 173) `mod` 1110)))
    (if index `mod` 17 == 0 then 1.25 else 0.6)
    ("fill=\"#aec7d4\" opacity=\"" <> scalar (0.28 + 0.1 * cos (2 * pi * phase + fromIntegral index * 1.71)) <> "\"")

definitions :: String
definitions = "<defs>"
  <> "<radialGradient id=\"night\" cx=\"68%\" cy=\"28%\" r=\"85%\"><stop stop-color=\"#1e394c\"/><stop offset=\"0.53\" stop-color=\"#0b1927\"/><stop offset=\"1\" stop-color=\"#050b14\"/></radialGradient>"
  <> "<radialGradient id=\"moon-halo\"><stop stop-color=\"#86b2cf\" stop-opacity=\"0.15\"/><stop offset=\"1\" stop-color=\"#86b2cf\" stop-opacity=\"0\"/></radialGradient>"
  <> "<radialGradient id=\"moon\" cx=\"33%\" cy=\"25%\" r=\"80%\"><stop stop-color=\"#7c9daa\" stop-opacity=\"0.30\"/><stop offset=\"0.6\" stop-color=\"#58778c\" stop-opacity=\"0.13\"/><stop offset=\"1\" stop-color=\"#182b3c\" stop-opacity=\"0.1\"/></radialGradient>"
  <> "<linearGradient id=\"silver\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"0.4\"><stop stop-color=\"#7193a4\"/><stop offset=\"0.35\" stop-color=\"#d5e7e7\"/><stop offset=\"0.65\" stop-color=\"#f0fcf0\"/><stop offset=\"1\" stop-color=\"#8ebccd\"/></linearGradient>"
  <> "<linearGradient id=\"reflection\" x1=\"0%\" y1=\"0%\" x2=\"0%\" y2=\"100%\"><stop stop-color=\"#d9ffff\" stop-opacity=\"0\"/><stop offset=\"0.35\" stop-color=\"#a4edff\" stop-opacity=\"0.08\"/><stop offset=\"0.5\" stop-color=\"#edffff\" stop-opacity=\"0.42\"/><stop offset=\"0.65\" stop-color=\"#a4edff\" stop-opacity=\"0.08\"/><stop offset=\"1\" stop-color=\"#d9ffff\" stop-opacity=\"0\"/></linearGradient>"
  <> "<linearGradient id=\"grip\"><stop stop-color=\"#101822\"/><stop offset=\"0.45\" stop-color=\"#344455\"/><stop offset=\"1\" stop-color=\"#080f18\"/></linearGradient>"
  <> "<radialGradient id=\"gem\" cx=\"30%\" cy=\"20%\"><stop stop-color=\"#cff6ed\"/><stop offset=\"0.35\" stop-color=\"#5397b7\"/><stop offset=\"1\" stop-color=\"#132a52\"/></radialGradient>"
  <> "<filter id=\"aura\" x=\"-50%\" y=\"-20%\" width=\"200%\" height=\"140%\"><feGaussianBlur stdDeviation=\"10\"/></filter></defs>"

typography :: String
typography =
  "<g fill=\"#d4dcd6\"><text x=\"86\" y=\"99\" font-family=\"Helvetica, sans-serif\" font-size=\"12\" letter-spacing=\"4\">MOONLIGHT / RELIQUARY No. 01</text>"
  <> "<path d=\"M 86 123 H 416\" stroke=\"#758891\" stroke-width=\"0.7\"/>"
  <> "<text x=\"79\" y=\"258\" font-family=\"Georgia, serif\" font-size=\"102\" letter-spacing=\"-4\">Vesper.</text>"
  <> "<text x=\"88\" y=\"301\" font-family=\"Helvetica, sans-serif\" font-size=\"15\" letter-spacing=\"7\">THE MOONBLADE</text></g>"
  <> "<g fill=\"#91a9b6\" font-family=\"Georgia, serif\" font-size=\"20\" font-style=\"italic\"><text x=\"88\" y=\"369\">An edge of borrowed light.</text><text x=\"88\" y=\"400\">A silence given shape.</text></g>"
  <> "<g fill=\"#94a7ad\" font-family=\"Helvetica, sans-serif\" font-size=\"11\" letter-spacing=\"2\"><text x=\"88\" y=\"1138\">MOONSTEEL / PALE GOLD / MIDNIGHT GLASS</text><text x=\"1512\" y=\"1138\" text-anchor=\"end\">FORGED IN PLANAR GEOMETRY</text></g>"
