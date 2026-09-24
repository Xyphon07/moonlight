module Moonlight.Planar.EquationalGardenSpec (tests) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Foldable (toList, traverse_)
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine
  ( AffineIso2, affine2, affineColumns, affineIso2, affineIsoMap, composeAffineIso2
  , inverseAffineIso2, transformPoint )
import Moonlight.Planar.Curve
  ( Located, OpenTrail, Path, Subpath (..), curveStepEnd, locatedValue, location, path
  , startJet, trailSteps, transformPath )
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), exactHalf, exactPoint, exactPointCoordinates
  , positiveExact, translateExactPoint, unitOne, unitZero )
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
  stillAir <- either (fail . show) pure (gardenPlants defaultGardenControls { gardenBreeze = unitZero })
  breeze <- either (fail . show) pure (gardenPlants defaultGardenControls)
  gale <- either (fail . show) pure (gardenPlants defaultGardenControls { gardenBreeze = unitOne })
  openedPlants <- either (fail . show) pure (gardenPlants defaultGardenControls { blossomOpening = unitOne })
  traverse_ socketsOnStem (stillAir <> breeze <> gale)
  traverse_ leanFollowsTangent (zip3 stillAir breeze gale)
  check "breeze leaves every blossom frame fixed"
    (all (\(a, b) -> blossomFrames a == blossomFrames b) (zip stillAir gale))
  check "opening leaves every stem trail and stem socket fixed"
    (all (\(a, b) -> gardenStemTrail a == gardenStemTrail b && stemFrames a == stemFrames b) (zip breeze openedPlants))
  check "breeze moves every leaf socket"
    (all (\(a, b) -> motifPort (gardenStem a) LowerLeafSocket /= motifPort (gardenStem b) LowerLeafSocket
      && motifPort (gardenStem a) UpperLeafSocket /= motifPort (gardenStem b) UpperLeafSocket) (zip stillAir gale))
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

-- The sockets read the painted trail: each leaf origin is a knot, the start
-- of step one or two, and the blossom origin is the trail's end. The knots
-- are recomputed here from the trail's own step displacements.
socketsOnStem :: GardenPlant -> IO ()
socketsOnStem plant = do
  let knots = stemKnots (gardenStemTrail plant)
      origin port = transformPoint (affineIsoMap (motifPort (gardenStem plant) port)) (exactPoint 0 0)
  check "stem has root, two leaf knots and the flower" (length knots == 4)
  check "lower leaf socket sits on its knot" (Just (origin LowerLeafSocket) == lookupIndex 1 knots)
  check "upper leaf socket sits on its knot" (Just (origin UpperLeafSocket) == lookupIndex 2 knots)
  check "blossom socket sits on the stem's end" (Just (origin BlossomSocket) == lookupIndex 3 knots)

-- Breeze turns a leaf socket only through the stem's tangent at its knot.
-- Undoing the exact, unnormalized tangent frame there (tangent, left normal)
-- leaves the authored lean and size, scaled by the positive normalization
-- factor: across breezes those relative matrices are positive multiples of
-- one another, and their origin is the knot itself.
leanFollowsTangent :: (GardenPlant, GardenPlant, GardenPlant) -> IO ()
leanFollowsTangent (still, middle, strong) = traverse_ relation [(LowerLeafSocket, 1), (UpperLeafSocket, 2)]
 where
  relation :: (StemPort, Int) -> IO ()
  relation (port, knot) = do
    relatives <- traverse (relative port knot) [still, middle, strong]
    check "leaf lean relative to the tangent is breeze-invariant up to normalization"
      (all (uncurry positiveMultiple) (zip relatives (drop 1 relatives)))
  relative :: StemPort -> Int -> GardenPlant -> IO (ExactVector, ExactVector, ExactVector)
  relative port knot plant = do
    let trail = gardenStemTrail plant
        steps = trailSteps (locatedValue trail)
    (tangent, point) <- maybe (fail "missing stem knot") pure
      ((,) <$> (startJet <$> Seq.lookup knot steps) <*> lookupIndex knot (stemKnots trail))
    let ExactVector tx ty = tangent
        (px, py) = exactPointCoordinates point
    frame <- maybe (fail "stationary stem tangent") pure
      (affineIso2 (affine2 tangent (ExactVector (negate ty) tx) (ExactVector px py)))
    pure (affineColumns (affineIsoMap (composeAffineIso2 (inverseAffineIso2 frame)
      (motifPort (gardenStem plant) port))))
  positiveMultiple :: (ExactVector, ExactVector, ExactVector) -> (ExactVector, ExactVector, ExactVector) -> Bool
  positiveMultiple (a, b, offset) (c, d, offset') =
    offset == ExactVector 0 0 && offset' == ExactVector 0 0
      && cross a c == 0 && cross b d == 0 && dot a c > 0 && dot b d > 0
      && dot a c * dot d d == dot b d * dot c c

stemKnots :: Located OpenTrail -> [ExactPoint]
stemKnots trail = toList (Seq.scanl (\point step -> translateExactPoint point (curveStepEnd step))
  (location trail) (trailSteps (locatedValue trail)))

lookupIndex :: Int -> [a] -> Maybe a
lookupIndex index = Seq.lookup index . Seq.fromList

blossomFrames :: GardenPlant -> (AffineIso2, AffineIso2)
blossomFrames plant = (motifPort (gardenStem plant) BlossomSocket, motifPort (gardenBlossom plant) BlossomRoot)

stemFrames :: GardenPlant -> [AffineIso2]
stemFrames plant = motifPort (gardenStem plant) <$> [minBound .. maxBound]

cross :: ExactVector -> ExactVector -> ExactRational
cross (ExactVector a b) (ExactVector c d) = a * d - b * c

dot :: ExactVector -> ExactVector -> ExactRational
dot (ExactVector a b) (ExactVector c d) = a * c + b * d

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
