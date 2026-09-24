{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Resource-safe dynamic boundary to the Rust crates under comparison.
module Moonlight.Planar.Bench.DelaunayCompare.Native
  ( NativeApi
  , NativeFixture
  , GeneratedCoordinates
  , withNativeApi
  , generateCoordinates
  , generatedCoordinatePairs
  , prepareNativeFixture
  , runNativeFixture
  , releaseNativeFixture
  ) where

import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (filterM)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Bifunctor (first)
import Data.Int (Int32)
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Moonlight.Planar.Bench.DelaunayCompare.Domain
import Foreign.C.Types (CDouble, CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as StorableVector
import qualified Data.Vector.Storable.Mutable as MutableStorableVector
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import System.Posix.DynamicLinker
  ( DL
  , RTLDFlags (RTLD_LOCAL, RTLD_NOW)
  , dlclose
  , dlopen
  , dlsym
  )
import System.Process
  ( CreateProcess (cwd)
  , proc
  , readCreateProcessWithExitCode
  )

type GenerateFunction = CInt -> CSize -> Ptr CDouble -> CSize -> IO CInt
type PrepareFunction = CInt -> Ptr CDouble -> CSize -> Ptr (Ptr ()) -> IO CInt
type RunFunction = Ptr () -> Ptr CSize -> Ptr CSize -> IO CInt
type ReleaseFunction = Ptr () -> IO ()

foreign import ccall safe "dynamic"
  bindGenerateFunction :: FunPtr GenerateFunction -> GenerateFunction

foreign import ccall safe "dynamic"
  bindPrepareFunction :: FunPtr PrepareFunction -> PrepareFunction

foreign import ccall safe "dynamic"
  bindRunFunction :: FunPtr RunFunction -> RunFunction

foreign import ccall unsafe "dynamic"
  bindReleaseFunction :: FunPtr ReleaseFunction -> ReleaseFunction

data NativeApi = NativeApi
  { nativeGenerate :: !GenerateFunction
  , nativePrepare :: !PrepareFunction
  , nativeRun :: !RunFunction
  , nativeRelease :: !ReleaseFunction
  }

newtype NativeFixture = NativeFixture (Ptr ())

data GeneratedCoordinates = GeneratedCoordinates
  { generatedPointCount :: !PointCount
  , generatedCoordinateValues :: !(StorableVector.Vector CDouble)
  }

withNativeApi
  :: (NativeApi -> IO (Either CompareObstruction result))
  -> IO (Either CompareObstruction result)
withNativeApi action = do
  manifestResult <- resolveRustManifest
  case manifestResult of
    Left obstruction -> pure (Left obstruction)
    Right manifest -> do
      libraryResult <- buildRustLibrary manifest
      case libraryResult of
        Left obstruction -> pure (Left obstruction)
        Right libraryPath -> withLoadedNativeApi libraryPath action

resolveRustManifest :: IO (Either CompareObstruction FilePath)
resolveRustManifest = do
  configured <- lookupEnv "MOONLIGHT_DELAUNAY_COMPARE_RUST_MANIFEST"
  let candidates =
        case configured of
          Just path -> path :| []
          Nothing ->
            "docs/delaunay-compare/rust/Cargo.toml"
              :| [ "foundation/moonlight-planar/docs/delaunay-compare/rust/Cargo.toml"
                 , "compiler/foundation/moonlight-planar/docs/delaunay-compare/rust/Cargo.toml"
                 ]
  existing <- filterM doesFileExist (NonEmpty.toList candidates)
  case existing of
    manifest : _ -> Right <$> makeAbsolute manifest
    [] -> pure (Left (RustManifestNotFound candidates))

buildRustLibrary :: FilePath -> IO (Either CompareObstruction FilePath)
buildRustLibrary manifest = do
  libraryNameResult <- dynamicLibraryName
  case libraryNameResult of
    Left obstruction -> pure (Left obstruction)
    Right libraryName -> do
      let rustRoot = takeDirectory manifest
          targetDirectory = rustRoot </> "target"
          command =
            ( proc
                "cargo"
                [ "build"
                , "--quiet"
                , "--release"
                , "--lib"
                , "--manifest-path"
                , manifest
                , "--target-dir"
                , targetDirectory
                ]
            )
              { cwd = Just rustRoot
              }
      invocation <- try @IOException (readCreateProcessWithExitCode command "")
      pure $
        case invocation of
          Left exception ->
            Left (CargoInvocationFailed manifest (displayException exception))
          Right (ExitFailure exitCode, standardOutput, standardError) ->
            Left
              ( CargoBuildFailed
                  manifest
                  exitCode
                  (standardError <> standardOutput)
              )
          Right (ExitSuccess, _, _) ->
            Right (targetDirectory </> "release" </> libraryName)

dynamicLibraryName :: IO (Either CompareObstruction FilePath)
dynamicLibraryName =
  pure $
    case os of
      "darwin" -> Right "libmoonlight_delaunay_compare_referents.dylib"
      "linux" -> Right "libmoonlight_delaunay_compare_referents.so"
      host -> Left (UnsupportedDynamicLibraryHost host)

withLoadedNativeApi
  :: FilePath
  -> (NativeApi -> IO (Either CompareObstruction result))
  -> IO (Either CompareObstruction result)
withLoadedNativeApi libraryPath action = do
  opened <- try @IOException (dlopen libraryPath [RTLD_NOW, RTLD_LOCAL])
  case opened of
    Left exception ->
      pure (Left (DynamicLibraryOpenFailed libraryPath (displayException exception)))
    Right handle ->
      bracket
        (pure handle)
        dlclose
        ( \loadedHandle -> do
            apiResult <- loadNativeApi libraryPath loadedHandle
            case apiResult of
              Left obstruction -> pure (Left obstruction)
              Right api -> action api
        )

loadNativeApi :: FilePath -> DL -> IO (Either CompareObstruction NativeApi)
loadNativeApi libraryPath handle =
  runExceptT $
    NativeApi
      <$> loadSymbol libraryPath handle "delaunay_compare_generate" bindGenerateFunction
      <*> loadSymbol libraryPath handle "delaunay_compare_prepare" bindPrepareFunction
      <*> loadSymbol libraryPath handle "delaunay_compare_run" bindRunFunction
      <*> loadSymbol libraryPath handle "delaunay_compare_release" bindReleaseFunction

loadSymbol
  :: FilePath
  -> DL
  -> String
  -> (FunPtr function -> boundFunction)
  -> ExceptT CompareObstruction IO boundFunction
loadSymbol libraryPath handle symbol bind =
  ExceptT $ do
    loaded <- try @IOException (dlsym handle symbol)
    pure $
      first
        (DynamicSymbolLoadFailed libraryPath symbol . displayException)
        (bind <$> loaded)

generateCoordinates
  :: NativeApi
  -> FixtureSpec
  -> IO (Either NativeStatus GeneratedCoordinates)
generateCoordinates api fixture = do
  let pointCount = fixturePointCount fixture
      coordinateCount = 2 * pointCountValue pointCount
  mutableCoordinates <- MutableStorableVector.new coordinateCount
  statusCode <-
    MutableStorableVector.unsafeWith mutableCoordinates $ \coordinatePointer ->
      nativeGenerate api
        (distributionTag (fixturePointDistribution fixture))
        (fromIntegral (pointCountValue pointCount))
        coordinatePointer
        (fromIntegral coordinateCount)
  case decodeNativeStatus statusCode of
    Left status -> pure (Left status)
    Right () -> do
      coordinates <- StorableVector.unsafeFreeze mutableCoordinates
      pure (Right (GeneratedCoordinates pointCount coordinates))

generatedCoordinatePairs :: GeneratedCoordinates -> Vector.Vector (Double, Double)
generatedCoordinatePairs generated =
  Vector.fromList
    ( unfoldr
        takeCoordinatePair
        (realToFrac <$> StorableVector.toList (generatedCoordinateValues generated))
    )
 where
  takeCoordinatePair :: [Double] -> Maybe ((Double, Double), [Double])
  takeCoordinatePair = \case
    x : y : remaining -> Just ((x, y), remaining)
    _ -> Nothing

prepareNativeFixture
  :: NativeApi
  -> NativeImplementation
  -> GeneratedCoordinates
  -> IO (Either NativePreparationFailure NativeFixture)
prepareNativeFixture api implementation coordinates =
  alloca $ \preparedOutput -> do
    poke preparedOutput nullPtr
    statusCode <-
      StorableVector.unsafeWith (generatedCoordinateValues coordinates) $ \coordinatePointer ->
        nativePrepare api
          (implementationTag implementation)
          coordinatePointer
          (fromIntegral (pointCountValue (generatedPointCount coordinates)))
          preparedOutput
    case decodeNativeStatus statusCode of
      Left status -> pure (Left (NativePreparationStatus status))
      Right () -> do
        prepared <- peek preparedOutput
        pure $
          if prepared == nullPtr
            then Left NativePreparationReturnedNull
            else Right (NativeFixture prepared)

runNativeFixture
  :: NativeApi
  -> NativeFixture
  -> IO (Either NativeRunFailure DelaunaySummary)
runNativeFixture api (NativeFixture prepared) =
  alloca $ \vertexCountOutput ->
    alloca $ \triangleCountOutput -> do
      statusCode <- nativeRun api prepared vertexCountOutput triangleCountOutput
      case decodeNativeStatus statusCode of
        Left status -> pure (Left (NativeRunStatus status))
        Right () -> do
          vertexCount <- peek vertexCountOutput
          triangleCount <- peek triangleCountOutput
          pure $ DelaunaySummary <$> cSizeToInt vertexCount <*> cSizeToInt triangleCount

releaseNativeFixture :: NativeApi -> NativeFixture -> IO ()
releaseNativeFixture api (NativeFixture prepared) = nativeRelease api prepared

decodeNativeStatus :: CInt -> Either NativeStatus ()
decodeNativeStatus statusCode =
  case fromIntegral statusCode :: Int32 of
    0 -> Right ()
    1 -> Left NativeNullPointer
    2 -> Left NativeUnknownTag
    3 -> Left NativeCoordinateCountMismatch
    4 -> Left NativeDistributionConstructionFailed
    5 -> Left NativeTriangulationFailed
    6 -> Left NativePanicked
    unknown -> Left (NativeUnknownStatus (fromIntegral unknown))

cSizeToInt :: CSize -> Either NativeRunFailure Int
cSizeToInt value =
  let integerValue = toInteger value
   in if integerValue > toInteger (maxBound :: Int)
        then Left (NativeCountExceedsHaskellInt integerValue)
        else Right (fromInteger integerValue)

distributionTag :: PointDistribution -> CInt
distributionTag = \case
  LocalInsertion -> 0
  Uniform -> 1

implementationTag :: NativeImplementation -> CInt
implementationTag = \case
  NativeSpade -> 0
  NativeSpadeHierarchy -> 1
  NativeCdt -> 2
  NativeDelaunator -> 3
