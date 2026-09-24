module Main (main) where

import Data.Maybe (mapMaybe)
import qualified Data.Vector as Vector
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.Dcel (innerFaceVertexTriples, numInnerFaces, vertexPoint)
import Moonlight.Planar.Exhibit.Svg (polygon, circle, scalar)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Types (BuildError)
import Moonlight.Planar.Voronoi
  ( voronoiFaceAdjacentEdges, voronoiFaceSite, voronoiFaces
  , voronoiFrom, voronoiVertexPosition
  )
import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)

data Bloom = Bloom
  { bloomCenter :: !Point
  , bloomRadius :: !Double
  , bloomPetals :: !Int
  , bloomPhase :: !Double
  , bloomHue :: !Double
  }

data Blossom = Blossom
  { blossomPicture :: !String
  , blossomTriangleCount :: !Int
  , blossomCellCount :: !Int
  }

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [outputPath] -> either (die . show) (writeFile outputPath) garden
    _ -> die "usage: moonlight-planar-noctilucent-garden OUTPUT.svg"

garden :: Either BuildError String
garden = do
  blossoms <- traverse buildBlossom blooms
  pure $
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1600\" height=\"1200\" viewBox=\"0 0 1600 1200\">"
      <> "<title>Noctilucent garden</title><desc>A midnight botanical study. Five phyllotactic point clouds are triangulated by Moonlight Planar; their finite Voronoi cells become luminous glass petals. "
      <> show (sum (blossomTriangleCount <$> blossoms)) <> " native Delaunay triangles and "
      <> show (sum (blossomCellCount <$> blossoms)) <> " finite dual cells. Typography, stems, color and lighting are authored SVG, not additional geometry algorithms.</desc>"
      <> definitions
      <> "<rect width=\"1600\" height=\"1200\" fill=\"url(#night)\"/>"
      <> stars
      <> "<circle cx=\"1020\" cy=\"443\" r=\"407\" fill=\"url(#aureole)\"/>"
      <> orbits
      <> foldMap renderStem blooms
      <> foldMap renderLeaf leaves
      <> foldMap blossomPicture blossoms
      <> typography
      <> "</svg>"

blooms :: [Bloom]
blooms =
  [ Bloom (Point 1040 438) 300 7 0.35 209
  , Bloom (Point 638 668) 164 6 1.1 257
  , Bloom (Point 1295 819) 177 8 0.2 185
  , Bloom (Point 930 971) 103 5 0.9 28
  , Bloom (Point 400 873) 92 6 0.4 220
  ]

-- The surrounding ring supplies bounded dual cells; it is not painted.
bloomSites :: Bloom -> Vector.Vector Point
bloomSites bloom = Vector.fromList (fmap site [0 .. 239] <> fmap guardSite [0 .. 39])
 where
  site :: Int -> Point
  site index =
    let theta = fromIntegral index * pi * (3 - sqrt 5) + bloomPhase bloom
        fraction = sqrt ((fromIntegral index + 0.5) / 240)
        radial = bloomRadius bloom * fraction
          * (0.83 + 0.17 * cos (fromIntegral (bloomPetals bloom) * theta))
     in polar radial theta
  guardSite :: Int -> Point
  guardSite index = polar (bloomRadius bloom * 1.35) (2 * pi * fromIntegral index / 40)

buildBlossom :: Bloom -> Either BuildError Blossom
buildBlossom bloom = do
  mesh <- delaunayGeometry (bloomSites bloom)
  let visibleSite point@(Point x y) =
        let theta = atan2 y x
            limit = bloomRadius bloom * (0.84 + 0.17 * cos (fromIntegral (bloomPetals bloom) * theta))
         in magnitude point < limit
      cells = mapMaybe
        (\face ->
          let site = vertexPoint mesh (voronoiFaceSite face)
           in if visibleSite site
                then (\corners -> (site, corners)) <$>
                  traverse (voronoiVertexPosition mesh . voronoiFrom mesh)
                    (voronoiFaceAdjacentEdges mesh face)
                else Nothing)
        (voronoiFaces mesh)
      triangles = Vector.toList $ fmap
        (\(a, b, c) -> fmap (vertexPoint mesh) [a, b, c])
        (innerFaceVertexTriples mesh)
      visibleTriangles = filter (all visibleSite) triangles
      Point centerX centerY = bloomCenter bloom
      picture = "<g transform=\"translate(" <> scalar centerX <> " " <> scalar centerY <> ")\">"
        <> "<circle r=\"" <> scalar (bloomRadius bloom * 1.18) <> "\" fill=\"url(#bloom-light)\"/>"
        <> foldMap (renderCell bloom) cells
        <> "<g fill=\"none\" stroke=\"#d1e7ed\" stroke-width=\"0.55\" opacity=\"0.22\">"
        <> foldMap (polygon "") visibleTriangles <> "</g>"
        <> foldMap (renderPollen bloom . fst) cells
        <> "<circle r=\"9\" fill=\"#061019\" stroke=\"#dabb83\" stroke-width=\"0.8\"/>"
        <> "<circle r=\"2\" fill=\"#ffe5a2\"/></g>"
  pure (Blossom picture (numInnerFaces mesh) (length cells))

renderCell :: Bloom -> (Point, [Point]) -> String
renderCell bloom (site@(Point x y), corners) =
  let radial = magnitude site / bloomRadius bloom
      wave = sin (atan2 y x * fromIntegral (bloomPetals bloom) + radial * 11)
      hue = bloomHue bloom + 23 * wave + 24 * radial
      lightness = 18 + 30 * radial + 9 * wave
      -- Insetting is a presentation transform, not a second Voronoi solver.
      inset (Point px py) = Point (x + 0.87 * (px - x)) (y + 0.87 * (py - y))
      attributes = "fill=\"" <> hslColor hue 0.55 (lightness / 100)
        <> "\" stroke=\"" <> hslColor hue 0.65 0.75 <> "\" stroke-opacity=\"0.53\" stroke-width=\"0.65\""
   in polygon attributes (inset <$> corners)

renderPollen :: Bloom -> Point -> String
renderPollen bloom point =
  circle point (if magnitude point < bloomRadius bloom * 0.36 then 1.6 else 0.85)
    "fill=\"#f8d9a1\" opacity=\"0.85\""

renderStem :: Bloom -> String
renderStem bloom =
  let Point x y = bloomCenter bloom
   in "<path d=\"M " <> scalar x <> " " <> scalar y
      <> " C " <> scalar (x - 110) <> " " <> scalar (y + 260)
      <> ", 720 1040, 790 1198\" fill=\"none\" stroke=\"url(#stem)\" stroke-width=\"1.5\"/>"

leaves :: [(Point, Double, Double)]
leaves = [(Point 771 1057, -148, 210), (Point 821 1110, -36, 215), (Point 668 962, -163, 180)]

renderLeaf :: (Point, Double, Double) -> String
renderLeaf (Point x y, angle, size) =
  "<g transform=\"translate(" <> scalar x <> " " <> scalar y <> ") rotate(" <> scalar angle <> ")\">"
    <> "<path d=\"M 0 0 Q " <> scalar (size * 0.46) <> " -62 " <> scalar size <> " 0 Q "
    <> scalar (size * 0.46) <> " 53 0 0\" fill=\"#102d34\" fill-opacity=\"0.7\" stroke=\"#648b83\" stroke-width=\"0.6\"/>"
    <> foldMap vein [1 .. 17 :: Int]
    <> "<path d=\"M 0 0 L " <> scalar size <> " 0\" stroke=\"#b3ad86\" stroke-width=\"0.7\"/></g>"
 where
  vein :: Int -> String
  vein index =
    let fraction = fromIntegral index / 18
        position = fraction * size
        spanY = 26 * sin (pi * fraction)
     in "<path d=\"M " <> scalar (position - 16) <> " 0 L " <> scalar position <> " " <> scalar (-spanY)
        <> " M " <> scalar (position - 16) <> " 0 L " <> scalar position <> " " <> scalar spanY
        <> "\" stroke=\"#648b83\" stroke-width=\"0.5\" opacity=\"0.65\"/>"

orbits :: String
orbits = "<g fill=\"none\" stroke=\"#c9b88c\" stroke-width=\"0.65\">"
  <> "<circle cx=\"1040\" cy=\"438\" r=\"372\" opacity=\"0.34\"/>"
  <> "<circle cx=\"1040\" cy=\"438\" r=\"379\" opacity=\"0.16\" stroke-dasharray=\"1 8\"/>"
  <> "<path d=\"M 768 113 A 424 424 0 0 1 1430 608\" opacity=\"0.22\"/></g>"
  <> circle (Point 790 160) 4 "fill=\"#d8c08d\""

stars :: String
stars = foldMap star [0 .. 239 :: Int]
 where
  star :: Int -> String
  star index =
    let x = 45 + fromIntegral ((index * 947 + 113) `mod` 1510)
        y = 40 + fromIntegral ((index * 419 + 71) `mod` 1100)
     in circle (Point x y) (if index `mod` 13 == 0 then 1.1 else 0.55)
          "fill=\"#b1c8ca\" opacity=\"0.32\""

definitions :: String
definitions = "<defs>"
  <> "<radialGradient id=\"night\" cx=\"67%\" cy=\"40%\" r=\"75%\"><stop stop-color=\"#153442\"/><stop offset=\"0.6\" stop-color=\"#091922\"/><stop offset=\"1\" stop-color=\"#050d15\"/></radialGradient>"
  <> "<radialGradient id=\"aureole\"><stop offset=\"0.4\" stop-color=\"#38798a\" stop-opacity=\"0\"/><stop offset=\"0.77\" stop-color=\"#38798a\" stop-opacity=\"0.13\"/><stop offset=\"1\" stop-color=\"#38798a\" stop-opacity=\"0\"/></radialGradient>"
  <> "<radialGradient id=\"bloom-light\"><stop stop-color=\"#91c5e9\" stop-opacity=\"0.09\"/><stop offset=\"1\" stop-color=\"#91c5e9\" stop-opacity=\"0\"/></radialGradient>"
  <> "<linearGradient id=\"stem\" x2=\"0.4\" y2=\"1\"><stop stop-color=\"#9eb9a7\"/><stop offset=\"1\" stop-color=\"#466661\"/></linearGradient></defs>"

typography :: String
typography =
  "<g fill=\"#ddcdaa\"><text x=\"83\" y=\"93\" font-family=\"Helvetica, sans-serif\" font-size=\"13\" letter-spacing=\"4\">MOONLIGHT / BOTANICAL STUDIES</text>"
  <> "<path d=\"M 83 117 H 418\" stroke=\"#9c987d\" stroke-width=\"0.65\"/>"
  <> "<text x=\"78\" y=\"239\" font-family=\"Georgia, serif\" font-size=\"71\" letter-spacing=\"-3\">Noctilucent</text>"
  <> "<text x=\"81\" y=\"315\" font-family=\"Georgia, serif\" font-size=\"76\" font-style=\"italic\">garden.</text></g>"
  <> "<g fill=\"#9cb4b7\" font-family=\"Georgia, serif\" font-size=\"20\"><text x=\"85\" y=\"371\">Flowers for the hours</text><text x=\"85\" y=\"401\">when no one is looking.</text></g>"
  <> "<g fill=\"#a7b0a2\" font-family=\"Helvetica, sans-serif\" font-size=\"11\" letter-spacing=\"2\"><text x=\"85\" y=\"1111\">I / PHYLLOTAXIS</text><text x=\"85\" y=\"1135\">II / DELAUNAY</text><text x=\"85\" y=\"1159\">III / VORONOI</text><text x=\"1515\" y=\"1159\" text-anchor=\"end\">NATIVE GEOMETRY / STUDY 01</text></g>"

-- Emit RGB so native SVG previewers and browsers agree on the palette.
hslColor :: Double -> Double -> Double -> String
hslColor hue saturation lightness =
  let chroma = (1 - abs (2 * lightness - 1)) * saturation
      sector = hue / 60
      alternate = chroma * (1 - abs (sector - 2 * fromIntegral (floor (sector / 2) :: Int) - 1))
      offset = lightness - chroma / 2
      (red, green, blue) = case (floor sector :: Int) `mod` 6 of
        0 -> (chroma, alternate, 0)
        1 -> (alternate, chroma, 0)
        2 -> (0, chroma, alternate)
        3 -> (0, alternate, chroma)
        4 -> (alternate, 0, chroma)
        _ -> (chroma, 0, alternate)
      channel :: Double -> Int
      channel value = round (255 * (value + offset))
   in printf "#%02x%02x%02x" (channel red) (channel green) (channel blue)

polar :: Double -> Double -> Point
polar radius angle = Point (radius * cos angle) (radius * sin angle)

magnitude :: Point -> Double
magnitude (Point x y) = sqrt (x * x + y * y)
