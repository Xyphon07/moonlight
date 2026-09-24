module Moonlight.Planar.EquationalGardenSpec (tests) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Foldable (toList, traverse_)
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve (Path, Subpath (..), location, path, transformPath)
import Moonlight.Planar.Exact
  ( exactHalf, exactPoint, positiveExact, unitOne, unitZero )
import Moonlight.Planar.Exhibit.EquationalGarden
  ( GardenControls (..), GardenPart (..), defaultGardenControls, gardenPartName
  , gardenPicture, leafOutline, petalOutline, gardenPlants, GardenPlant (..)
  , StemPort (..), LeafPort (..), BlossomPort (..) )
import Moonlight.Planar.Illustration (Picture, PictureAlgebra (..), foldPicture)
import Moonlight.Planar.Illustration.Motif (motifPort)
import Moonlight.Planar.Illustration.Svg
  ( renderSvg, svgOptions, svgPrecision, svgViewport )

-- These are artistic dependency and publication tests. Curve interpolation
-- laws belong to the authoring tests, not a second implementation here.
tests :: IO ()
tests = do
  baseline <- either (fail . show) pure (gardenPicture defaultGardenControls)
  opened <- either (fail . show) pure (gardenPicture defaultGardenControls { blossomOpening = unitOne })
  wind <- either (fail . show) pure (gardenPicture defaultGardenControls { gardenBreeze = unitOne })
  traverse_ checkAttachments [defaultGardenControls,
    defaultGardenControls { blossomOpening = unitOne, gardenBreeze = unitOne }]
  traverse_ (checkLayer baseline opened [Blossoms]) parts
  traverse_ (checkLayer baseline wind [Stems, Leaves]) parts
  check "petal width edit changes the real contour"
    (petalOutline unitZero /= petalOutline unitOne)
  check "petal attachment remains fixed"
    (location (petalOutline unitZero) == location (petalOutline unitOne))
  check "leaf bend changes the real contour"
    (leafOutline unitZero /= leafOutline unitOne)
  check "leaf attachment remains fixed"
    (location (leafOutline unitZero) == location (leafOutline unitOne))
  case publishedGarden of
    Left failure -> fail failure
    Right svg -> do
      check "garden retains a clipped even-odd moon cutout"
        ("fill-rule=\"evenodd\"" `isInfixOf` svg && "<clipPath" `isInfixOf` svg)
      check "garden publishes both kinds of gradient"
        ("<linearGradient" `isInfixOf` svg && "<radialGradient" `isInfixOf` svg)
      traverse_ (\part -> check ("published layer: " <> gardenPartName part)
        (gardenPartName part `isInfixOf` svg)) parts
  putStrLn "equational garden: independent controls, stable attachments and full-scene publication ok"

checkAttachments :: GardenControls -> IO ()
checkAttachments controls = do
  assemblies <- either (fail . show) pure (gardenPlants controls)
  traverse_ (\plant -> do
    check "lower leaf matches its full stem socket frame"
      (motifPort (gardenLowerLeaf plant) LeafRoot == motifPort (gardenStem plant) LowerLeafSocket)
    check "upper leaf matches its full stem socket frame"
      (motifPort (gardenUpperLeaf plant) LeafRoot == motifPort (gardenStem plant) UpperLeafSocket)
    check "blossom matches its full stem socket frame"
      (motifPort (gardenBlossom plant) BlossomRoot == motifPort (gardenStem plant) BlossomSocket)) assemblies

parts :: [GardenPart]
parts = [NightSky, Moon, Ground, Stems, Leaves, Blossoms, Ribbon, Fireflies]

checkLayer :: Picture GardenPart -> Picture GardenPart -> [GardenPart] -> GardenPart -> IO ()
checkLayer baseline edited affected part = do
  let original = observedPaths part baseline
      candidate = observedPaths part edited
  check ("nonempty garden layer: " <> gardenPartName part) (not (null original))
  check ("semantic edit dependency: " <> gardenPartName part)
    ((original /= candidate) == (part `elem` affected))

-- A read-only projection of the existing algebra into canonical paths.
-- It preserves affine placement and sees clip boundaries; no geometry oracle
-- or alternate scene representation is introduced.
observedPaths :: GardenPart -> Picture GardenPart -> [Path]
observedPaths selected picture = foldPicture algebra picture Nothing
 where
  algebra :: PictureAlgebra GardenPart (Maybe GardenPart -> [Path])
  algebra = PictureAlgebra
    { paintSequence = \children inherited -> concatMap ($ inherited) (toList children)
    , paintFill = \_ _ contours inherited ->
        keep inherited (path (Seq.fromList (ClosedSubpath <$> NonEmpty.toList contours)))
    , paintStroke = \_ curve inherited -> keep inherited curve
    , paintClip = \_ contours child inherited ->
        keep inherited (path (Seq.fromList (ClosedSubpath <$> NonEmpty.toList contours))) <> child inherited
    , paintPlace = \frame child inherited -> transformPath frame <$> child inherited
    , paintOpacity = \_ child -> child
    , paintAnnotation = \part child _ -> child (Just part)
    }
  keep :: Maybe GardenPart -> Path -> [Path]
  keep inherited curve = if inherited == Just selected then [curve] else []

publishedGarden :: Either String String
publishedGarden = do
  width <- first show (positiveExact 1200)
  height <- first show (positiveExact 1000)
  numeric <- first show (positiveExact (exactHalf ^ (18 :: Int)))
  tolerance <- first show (positiveExact exactHalf)
  viewport <- first show (svgViewport 1200 1000 (exactPoint 0 0) width height)
  precision <- first show (svgPrecision 6 numeric)
  options <- first show (svgOptions viewport precision tolerance 20 16384)
  picture <- first show (gardenPicture defaultGardenControls)
  first show (renderSvg gardenPartName options picture)

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)
