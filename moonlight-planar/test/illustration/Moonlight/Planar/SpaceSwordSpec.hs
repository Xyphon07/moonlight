module Moonlight.Planar.SpaceSwordSpec (tests) where

import Control.Monad (unless)
import Data.Foldable (toList, traverse_)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve (Path, Subpath (..), path, transformPath)
import Moonlight.Planar.Exhibit.SpaceSword
import Moonlight.Planar.Illustration (Picture, PictureAlgebra (..), foldPicture)

tests :: IO ()
tests = do
  let baseline = spaceSwordPicture defaultSpaceSwordControls
      overcharged = spaceSwordPicture overchargedSpaceSwordControls
      affected = [BladeAura, BladeShell, BladeCore, BladeRunes, IonWake]
  traverse_ (checkPart baseline overcharged affected) [minBound .. maxBound]
  check "semantic charge/sweep edit changes the sword"
    (baseline /= overcharged)
  putStrLn "space sword: complete typed composition and selective overcharge edit ok"

checkPart
  :: Picture SpaceSwordPart
  -> Picture SpaceSwordPart
  -> [SpaceSwordPart]
  -> SpaceSwordPart
  -> IO ()
checkPart baseline edited affected part = do
  let original = observedPaths part baseline
      candidate = observedPaths part edited
  check ("nonempty space-sword part: " <> spaceSwordPartName part) (not (null original))
  check ("space-sword control dependency: " <> spaceSwordPartName part)
    ((original /= candidate) == (part `elem` affected))

-- A pure test projection of the existing Picture algebra. It preserves
-- placement and annotation ownership; it is not another geometry evaluator.
observedPaths :: SpaceSwordPart -> Picture SpaceSwordPart -> [Path]
observedPaths selected picture = foldPicture algebra picture Nothing
 where
  algebra :: PictureAlgebra SpaceSwordPart (Maybe SpaceSwordPart -> [Path])
  algebra = PictureAlgebra
    { paintSequence = \children inherited -> concatMap ($ inherited) (toList children)
    , paintFill = \_ _ contours inherited ->
        retain inherited (path (Seq.fromList (ClosedSubpath <$> NonEmpty.toList contours)))
    , paintStroke = \_ curve inherited -> retain inherited curve
    , paintClip = \_ _ child -> child
    , paintPlace = \frame child inherited -> transformPath frame <$> child inherited
    , paintOpacity = \_ child -> child
    , paintAnnotation = \part child _ -> child (Just part)
    }
  retain :: Maybe SpaceSwordPart -> Path -> [Path]
  retain inherited curve = if inherited == Just selected then [curve] else []

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)
