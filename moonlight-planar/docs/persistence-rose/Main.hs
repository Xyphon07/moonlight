module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
  ( ExceptT (..),
    except,
    runExceptT,
  )
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Moonlight.Triangulation.Exhibit.PersistenceRose
  ( PersistenceRoseError,
    PersistenceRoseScene,
    alphaEclipseReceipt,
    buildAlphaEclipseScene,
    buildPersistenceRoseScene,
    persistenceRoseFrameSpecs,
    persistenceRoseReceipt,
    renderAlphaEclipse,
    renderAlphaEclipseReceipt,
    renderPersistenceRoseAnimationReceipt,
    renderPersistenceRoseFrame,
    renderPersistenceRosePoster,
    renderPersistenceRoseReceipt,
    stampPersistenceRoseGif,
    verifyPersistenceRoseGif,
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)
import Text.Printf (printf)
import Text.Read (readMaybe)

data PersistenceRoseCommand
  = WritePoster !FilePath
  | WriteEclipse !FilePath
  | WriteFrames !Int !FilePath
  | StampGif !FilePath !FilePath
  | VerifyArtifacts !FilePath
  | PrintReceipt

data PersistenceRoseCommandError
  = PersistenceRoseUsage ![String]
  | PersistenceRoseInvalidFrameCount !String
  | PersistenceRoseConstructionFailed !PersistenceRoseError
  | PersistenceRoseDirectoryFailed !FilePath !IOException
  | PersistenceRoseReadFailed !FilePath !IOException
  | PersistenceRoseWriteFailed !FilePath !IOException
  | PersistenceRoseArtifactMismatch !FilePath

instance Show PersistenceRoseCommandError where
  show commandError =
    case commandError of
      PersistenceRoseUsage arguments ->
        "invalid persistence-rose command "
          <> show arguments
          <> "; expected: poster OUTPUT.svg | eclipse OUTPUT.svg | frames FRAME_COUNT OUTPUT_DIRECTORY | stamp-gif INPUT.gif OUTPUT.gif | verify-artifacts PACKAGE_ROOT | receipt"
      PersistenceRoseInvalidFrameCount submitted ->
        "frame count must be an integer greater than one, received " <> show submitted
      PersistenceRoseConstructionFailed sceneError -> show sceneError
      PersistenceRoseDirectoryFailed path caughtError ->
        "could not create output directory " <> show path <> ": " <> show caughtError
      PersistenceRoseReadFailed path caughtError ->
        "could not read " <> show path <> ": " <> show caughtError
      PersistenceRoseWriteFailed path caughtError ->
        "could not write " <> show path <> ": " <> show caughtError
      PersistenceRoseArtifactMismatch path ->
        "tracked exhibit differs from its canonical renderer: " <> show path

main :: IO ()
main = do
  arguments <- getArgs
  result <- runExceptT (runPersistenceRoseCommand arguments)
  either (die . show) pure result

runPersistenceRoseCommand :: [String] -> ExceptT PersistenceRoseCommandError IO ()
runPersistenceRoseCommand arguments = do
  command <- except (parseCommand arguments)
  case command of
    WriteEclipse outputPath -> do
      scene <- except (first PersistenceRoseConstructionFailed buildAlphaEclipseScene)
      createOutputDirectory (takeDirectory outputPath)
      writeOutput outputPath (renderAlphaEclipse scene)
      lift
        ( putStrLn
            (renderAlphaEclipseReceipt (alphaEclipseReceipt scene) <> "\nwrote alpha eclipse " <> outputPath)
        )
    StampGif inputPath outputPath -> do
      scene <- requirePersistenceRoseScene
      inputBytes <- readBytes inputPath
      (receipt, stamped) <- except (first PersistenceRoseConstructionFailed (stampPersistenceRoseGif scene inputBytes))
      createOutputDirectory (takeDirectory outputPath)
      writeBytes outputPath stamped
      lift
        ( putStrLn
            (renderPersistenceRoseAnimationReceipt receipt <> "\nwrote stamped GIF " <> outputPath)
        )
    VerifyArtifacts packageRoot -> verifyTrackedArtifacts packageRoot
    PrintReceipt -> do
      scene <- requirePersistenceRoseScene
      lift (putStrLn (renderPersistenceRoseReceipt (persistenceRoseReceipt scene)))
    WritePoster outputPath -> do
      scene <- requirePersistenceRoseScene
      let receiptText = renderPersistenceRoseReceipt (persistenceRoseReceipt scene)
      createOutputDirectory (takeDirectory outputPath)
      writeOutput outputPath (renderPersistenceRosePoster scene)
      lift (putStrLn (receiptText <> "\nwrote poster " <> outputPath))
    WriteFrames frameCount outputDirectory -> do
      scene <- requirePersistenceRoseScene
      let receiptText = renderPersistenceRoseReceipt (persistenceRoseReceipt scene)
      frames <- except (first PersistenceRoseConstructionFailed (persistenceRoseFrameSpecs frameCount))
      createOutputDirectory outputDirectory
      traverse_
        (\(frameIndex, frame) -> writeOutput (framePath outputDirectory frameIndex) (renderPersistenceRoseFrame scene frame))
        (zip [0 :: Int ..] frames)
      lift
        ( putStrLn
            ( receiptText
                <> "\nwrote "
                <> show (length frames)
                <> " frames to "
                <> outputDirectory
            )
        )

requirePersistenceRoseScene :: ExceptT PersistenceRoseCommandError IO PersistenceRoseScene
requirePersistenceRoseScene =
  except (first PersistenceRoseConstructionFailed buildPersistenceRoseScene)

parseCommand :: [String] -> Either PersistenceRoseCommandError PersistenceRoseCommand
parseCommand arguments =
  case arguments of
    ["poster", outputPath] -> Right (WritePoster outputPath)
    ["eclipse", outputPath] -> Right (WriteEclipse outputPath)
    ["stamp-gif", inputPath, outputPath] -> Right (StampGif inputPath outputPath)
    ["verify-artifacts", packageRoot] -> Right (VerifyArtifacts packageRoot)
    ["frames", submittedCount, outputDirectory] ->
      case readMaybe submittedCount of
        Just frameCount
          | frameCount > 1 -> Right (WriteFrames frameCount outputDirectory)
        _ -> Left (PersistenceRoseInvalidFrameCount submittedCount)
    ["receipt"] -> Right PrintReceipt
    _ -> Left (PersistenceRoseUsage arguments)

createOutputDirectory :: FilePath -> ExceptT PersistenceRoseCommandError IO ()
createOutputDirectory outputDirectory =
  liftIOException
    (PersistenceRoseDirectoryFailed outputDirectory)
    (createDirectoryIfMissing True outputDirectory)

writeOutput :: FilePath -> String -> ExceptT PersistenceRoseCommandError IO ()
writeOutput outputPath contents =
  liftIOException (PersistenceRoseWriteFailed outputPath) (writeFile outputPath contents)

readOutput :: FilePath -> ExceptT PersistenceRoseCommandError IO String
readOutput inputPath =
  liftIOException (PersistenceRoseReadFailed inputPath) (readFile inputPath)

readBytes :: FilePath -> ExceptT PersistenceRoseCommandError IO ByteString
readBytes inputPath =
  liftIOException (PersistenceRoseReadFailed inputPath) (ByteString.readFile inputPath)

writeBytes :: FilePath -> ByteString -> ExceptT PersistenceRoseCommandError IO ()
writeBytes outputPath contents =
  liftIOException (PersistenceRoseWriteFailed outputPath) (ByteString.writeFile outputPath contents)

verifyTrackedArtifacts :: FilePath -> ExceptT PersistenceRoseCommandError IO ()
verifyTrackedArtifacts packageRoot = do
  roseScene <- requirePersistenceRoseScene
  eclipseScene <- except (first PersistenceRoseConstructionFailed buildAlphaEclipseScene)
  let exhibitRoot = packageRoot </> "docs" </> "persistence-rose"
      posterPath = exhibitRoot </> "moonlight-triangulation-persistence-rose.svg"
      eclipsePath = exhibitRoot </> "moonlight-triangulation-alpha-eclipse.svg"
      gifPath = exhibitRoot </> "moonlight-triangulation-persistence-rose.gif"
  verifyTextArtifact posterPath (renderPersistenceRosePoster roseScene)
  verifyTextArtifact eclipsePath (renderAlphaEclipse eclipseScene)
  gifBytes <- readBytes gifPath
  animationReceipt <-
    except
      (first PersistenceRoseConstructionFailed (verifyPersistenceRoseGif roseScene gifBytes))
  lift
    ( putStrLn
        ( "verified poster, alpha eclipse, and stamped GIF\n"
            <> renderPersistenceRoseAnimationReceipt animationReceipt
        )
    )

verifyTextArtifact
  :: FilePath
  -> String
  -> ExceptT PersistenceRoseCommandError IO ()
verifyTextArtifact artifactPath expectedContents = do
  actualContents <- readOutput artifactPath
  except
    ( if actualContents == expectedContents
        then Right ()
        else Left (PersistenceRoseArtifactMismatch artifactPath)
    )

liftIOException
  :: (IOException -> PersistenceRoseCommandError)
  -> IO value
  -> ExceptT PersistenceRoseCommandError IO value
liftIOException wrapFailure = ExceptT . fmap (first wrapFailure) . try

framePath :: FilePath -> Int -> FilePath
framePath outputDirectory frameIndex =
  outputDirectory </> printf "frame-%04d.svg" frameIndex
