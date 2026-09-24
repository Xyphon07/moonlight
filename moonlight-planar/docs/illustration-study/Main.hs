-- | The only effect boundary of the study: validate output location, render
-- every fixed-camera variant, then publish ordinary SVG documents.
module Main (main) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Moonlight.Planar.Exact
import Moonlight.Planar.Exhibit.IllustrationStudy
import Moonlight.Planar.Exhibit.EquationalGarden
import Moonlight.Planar.Exhibit.SpaceSword
import Moonlight.Planar.Illustration.Svg
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), isAbsolute)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [directory] | isAbsolute directory -> do
      documents <- either die pure studyDocuments
      createDirectoryIfMissing True directory
      traverse_ (\(name,svg) -> writeFile (directory </> name) svg) documents
      putStrLn ("Wrote fixed-camera illustration studies to " <> directory)
    _ -> die "usage: moonlight-planar-illustration-study ABSOLUTE_OUTPUT_DIRECTORY"

studyDocuments :: Either String [(FilePath,String)]
studyDocuments = do
  width <- first show (positiveExact 1200)
  height <- first show (positiveExact 1000)
  roundingValue <- first show (exactRational 1 100000)
  toleranceValue <- first show (exactRational 1 4)
  rounding <- first show (positiveExact roundingValue)
  tolerance <- first show (positiveExact toleranceValue)
  viewport <- first show (svgViewport 1200 1000 (exactPoint 0 0) width height)
  precision <- first show (svgPrecision 5 rounding)
  options <- first show (svgOptions viewport precision tolerance 24 100000)
  edited <- first show $ foldM (\controls (control,value) -> editControl control value controls)
    defaultControls [(LeftHornSweep,30),(EyeSpacing,66),(EyeTilt,12),(CloakFullness,30),(FullerWidth,20)]
  let gardenEdited = defaultGardenControls { blossomOpening = unitOne, gardenBreeze = unitOne }
  garden <- first show (gardenPicture defaultGardenControls)
  editedGarden <- first show (gardenPicture gardenEdited)
  traverse (\(name, rendered) -> (name,) <$> first show rendered)
    [ ("knight-crescent.svg", renderSvg partName options (studyPicture defaultControls))
    , ("knight-crescent-edited.svg", renderSvg partName options (studyPicture edited))
    , ("knight-crescent-diagnostic.svg", renderDiagnosticSvg partName options (diagnosticPicture defaultControls))
    , ("equational-garden.svg", renderSvg gardenPartName options garden)
    , ("equational-garden-edited.svg", renderSvg gardenPartName options editedGarden)
    , ("equational-garden-diagnostic.svg", renderDiagnosticSvg gardenPartName options garden)
    , ("equational-garden-layout.svg", renderSvg gardenPartName options (gardenLayoutPicture unitZero))
    , ("equational-garden-layout-edited.svg", renderSvg gardenPartName options (gardenLayoutPicture unitOne))
    , ("space-sword.svg", renderSvg spaceSwordPartName options
        (spaceSwordPicture defaultSpaceSwordControls))
    , ("space-sword-overcharged.svg", renderSvg spaceSwordPartName options
        (spaceSwordPicture overchargedSpaceSwordControls))
    ]
