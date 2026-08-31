module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
  ( ExceptT (..),
    except,
    runExceptT,
  )
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Moonlight.Triangulation.Exhibit.CategoryObservatory
  ( Exhibit,
    ExhibitError,
    buildExhibit,
    renderExhibitError,
  )
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Picture
  ( PictureObstruction,
    categoryObservatoryPictureFileName,
    renderCategoryObservatoryPicture,
    renderPictureObstruction,
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)

data CategoryObservatoryCommand
  = ExportJson
  | WritePicture !FilePath
  | VerifyArtifacts !FilePath

data CategoryObservatoryCommandError
  = CategoryObservatoryUsage ![String]
  | CategoryObservatoryConstructionFailed !ExhibitError
  | CategoryObservatoryPictureFailed !PictureObstruction
  | CategoryObservatoryDirectoryFailed !FilePath !IOException
  | CategoryObservatoryReadFailed !FilePath !IOException
  | CategoryObservatoryWriteFailed !FilePath !IOException
  | CategoryObservatoryArtifactMismatch !FilePath

main :: IO ()
main = do
  arguments <- getArgs
  result <- runExceptT (runCategoryObservatoryCommand arguments)
  either (die . renderCommandError) pure result

runCategoryObservatoryCommand ::
  [String] ->
  ExceptT CategoryObservatoryCommandError IO ()
runCategoryObservatoryCommand arguments = do
  command <- except (parseCommand arguments)
  exhibit <- requireExhibit
  case command of
    ExportJson -> lift (LazyByteString.putStrLn (Aeson.encode exhibit))
    WritePicture outputPath -> do
      picture <- requirePicture exhibit
      createOutputDirectory (takeDirectory outputPath)
      writeOutput outputPath picture
      lift (putStrLn ("wrote category-observatory picture " <> outputPath))
    VerifyArtifacts packageRoot -> do
      picture <- requirePicture exhibit
      let picturePath =
            packageRoot
              </> "docs"
              </> "category-observatory"
              </> categoryObservatoryPictureFileName
      actualPicture <- readOutput picturePath
      except
        ( if actualPicture == picture
            then Right ()
            else Left (CategoryObservatoryArtifactMismatch picturePath)
        )
      lift (putStrLn ("verified category-observatory picture " <> picturePath))

parseCommand ::
  [String] ->
  Either CategoryObservatoryCommandError CategoryObservatoryCommand
parseCommand arguments =
  case arguments of
    [] -> Right ExportJson
    ["picture", outputPath] -> Right (WritePicture outputPath)
    ["verify-artifacts", packageRoot] -> Right (VerifyArtifacts packageRoot)
    _ -> Left (CategoryObservatoryUsage arguments)

requireExhibit :: ExceptT CategoryObservatoryCommandError IO Exhibit
requireExhibit =
  except (first CategoryObservatoryConstructionFailed buildExhibit)

requirePicture ::
  Exhibit ->
  ExceptT CategoryObservatoryCommandError IO String
requirePicture exhibit =
  except
    ( first
        CategoryObservatoryPictureFailed
        (renderCategoryObservatoryPicture exhibit)
    )

createOutputDirectory ::
  FilePath ->
  ExceptT CategoryObservatoryCommandError IO ()
createOutputDirectory outputDirectory =
  liftIOException
    (CategoryObservatoryDirectoryFailed outputDirectory)
    (createDirectoryIfMissing True outputDirectory)

readOutput :: FilePath -> ExceptT CategoryObservatoryCommandError IO String
readOutput inputPath =
  liftIOException
    (CategoryObservatoryReadFailed inputPath)
    (readFile inputPath)

writeOutput ::
  FilePath ->
  String ->
  ExceptT CategoryObservatoryCommandError IO ()
writeOutput outputPath contents =
  liftIOException
    (CategoryObservatoryWriteFailed outputPath)
    (writeFile outputPath contents)

liftIOException ::
  (IOException -> CategoryObservatoryCommandError) ->
  IO value ->
  ExceptT CategoryObservatoryCommandError IO value
liftIOException wrapFailure =
  ExceptT . fmap (first wrapFailure) . try

renderCommandError :: CategoryObservatoryCommandError -> String
renderCommandError commandError =
  case commandError of
    CategoryObservatoryUsage arguments ->
      "invalid category-observatory command "
        <> show arguments
        <> "; expected no arguments (JSON) | picture OUTPUT.svg | verify-artifacts PACKAGE_ROOT"
    CategoryObservatoryConstructionFailed exhibitError ->
      renderExhibitError exhibitError
    CategoryObservatoryPictureFailed pictureError ->
      renderPictureObstruction pictureError
    CategoryObservatoryDirectoryFailed path caughtError ->
      "could not create category-observatory output directory "
        <> show path
        <> ": "
        <> show caughtError
    CategoryObservatoryReadFailed path caughtError ->
      "could not read category-observatory artifact "
        <> show path
        <> ": "
        <> show caughtError
    CategoryObservatoryWriteFailed path caughtError ->
      "could not write category-observatory artifact "
        <> show path
        <> ": "
        <> show caughtError
    CategoryObservatoryArtifactMismatch path ->
      "tracked category-observatory artifact differs from its canonical renderer: "
        <> show path
