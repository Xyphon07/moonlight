{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Thin effect boundary for rendering benchmark receipts as SVG pictures.
module Main (main) where

import Control.Exception (IOException, displayException, try)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Moonlight.Triangulation.Bench.DelaunayCompare.Domain (allSizeBands)
import Moonlight.Triangulation.Bench.DelaunayCompare.Picture
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (readFile')

data PictureCommandFailure
  = PictureCommandUsage
  | PictureCommandReadFailed !FilePath !String
  | PictureCommandCreateDirectoryFailed !FilePath !String
  | PictureCommandReceiptObstructed !PictureObstruction
  | PictureCommandWriteFailed !FilePath !String

main :: IO ()
main = do
  arguments <- getArgs
  outcome <- runExceptT (runPictureCommand arguments)
  either (die . renderPictureCommandFailure) pure outcome

runPictureCommand :: [String] -> ExceptT PictureCommandFailure IO ()
runPictureCommand = \case
  [inputCsv, outputDirectory] -> do
    source <-
      ExceptT
        ( first
            (PictureCommandReadFailed inputCsv . displayException)
            <$> try @IOException (readFile' inputCsv)
        )
    receipt <- except (first PictureCommandReceiptObstructed (parseBenchmarkReceipt source))
    let pictures =
          ( \sizeBand ->
              (pictureFileName sizeBand, renderComparisonPicture sizeBand receipt)
          )
            <$> allSizeBands
    ExceptT
      ( first
          (PictureCommandCreateDirectoryFailed outputDirectory . displayException)
          <$> try @IOException (createDirectoryIfMissing True outputDirectory)
      )
    traverse_ (writePicture outputDirectory) pictures
  _ -> except (Left PictureCommandUsage)

writePicture :: FilePath -> (FilePath, String) -> ExceptT PictureCommandFailure IO ()
writePicture outputDirectory (fileName, svgDocument) =
  let outputPath = outputDirectory </> fileName
   in ExceptT
        ( first
            (PictureCommandWriteFailed outputPath . displayException)
            <$> try @IOException (writeFile outputPath svgDocument)
        )

renderPictureCommandFailure :: PictureCommandFailure -> String
renderPictureCommandFailure = \case
  PictureCommandUsage ->
    "usage: moonlight-triangulation-delaunay-pictures INPUT.csv OUTPUT_DIRECTORY"
  PictureCommandReadFailed path details ->
    "could not read benchmark CSV " <> path <> ": " <> details
  PictureCommandCreateDirectoryFailed path details ->
    "could not create picture directory " <> path <> ": " <> details
  PictureCommandReceiptObstructed obstruction ->
    renderPictureObstruction obstruction
  PictureCommandWriteFailed path details ->
    "could not write comparison picture " <> path <> ": " <> details
