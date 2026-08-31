{-| Assertions that discovered sheaf imports match an allowed local-module manifest. -}
module Moonlight.Pale.Test.ImportDiscipline
  ( SheafManifest (..),
    assertSheafDiscipline,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.List (intercalate, isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Pale.Test.ImportDiscipline.Registry
  ( assertRegisteredSetMatches,
    discoverModuleSurfaces,
    moduleSurfaceIdentity,
    moduleSurfaceImportedNames,
    renderSourceDiscoveryFailure,
  )
import Moonlight.Pale.Test.Resources
  ( renderResourcePathError,
    resolvePackageDirectory,
  )
import Test.Tasty.HUnit (Assertion, assertFailure)

type SheafManifest :: Type
data SheafManifest = SheafManifest
  { sheafModulePrefix :: String,
    sheafAllowedImports :: Map String (Set String)
  }

assertSheafDiscipline :: FilePath -> FilePath -> SheafManifest -> Assertion
assertSheafDiscipline packageMarker relativeDirectory sheafManifest =
  let modulePrefix = sheafModulePrefix sheafManifest
      allowedImports = sheafAllowedImports sheafManifest
   in
    resolvePackageDirectory packageMarker relativeDirectory
      >>= either
        (assertFailure . renderResourcePathError)
        (\packageDirectory -> do
            discoverPrefixedModuleImports packageDirectory modulePrefix
              >>= either
                (assertFailure . intercalate "\n")
                ( \discoveredImports -> do
                    assertRegisteredSetMatches
                      "discovered sheaf modules must match the declared layer registry"
                      (Map.keysSet allowedImports)
                      (Map.keysSet discoveredImports)
                    let violations =
                          Map.toAscList discoveredImports
                            >>= \(moduleName, importedModules) ->
                              let allowedModules = Map.findWithDefault Set.empty moduleName allowedImports
                                  forbiddenModules =
                                    importedModules
                                      & flip Set.difference allowedModules
                                      & Set.toAscList
                               in
                                if null forbiddenModules
                                  then []
                                  else
                                    [ moduleName
                                        <> " imports forbidden local modules "
                                        <> show forbiddenModules
                                        <> "; imported local modules = "
                                        <> show (Set.toAscList importedModules)
                                        <> "; allowed local modules = "
                                        <> show (Set.toAscList allowedModules)
                                    ]
                    if null violations
                      then pure ()
                      else assertFailure (intercalate "\n" violations)
                )
        )

discoverPrefixedModuleImports :: FilePath -> String -> IO (Either [String] (Map String (Set String)))
discoverPrefixedModuleImports packageDirectory modulePrefix =
  discoverModuleSurfaces packageDirectory
    >>= pure
      . either
        (Left . fmap renderSourceDiscoveryFailure . NonEmpty.toList)
        (foldl' collectModule (Right Map.empty))
  where
    collectModule accumulatedModules moduleSurface =
      accumulatedModules
        >>= \modulesByName ->
          case moduleSurfaceIdentity moduleSurface of
            Nothing ->
              Right modulesByName
            Just moduleName
              | not (moduleNameWithinPrefix modulePrefix moduleName) ->
                  Right modulesByName
              | Map.member moduleName modulesByName ->
                  Left ["duplicate module identity discovered: " <> moduleName]
              | otherwise ->
                  Right
                    ( Map.insert
                        moduleName
                        (localModuleImports modulePrefix (moduleSurfaceImportedNames moduleSurface))
                        modulesByName
                    )

localModuleImports :: String -> Set String -> Set String
localModuleImports modulePrefix =
  Set.filter (moduleNameWithinPrefix modulePrefix)

moduleNameWithinPrefix :: String -> String -> Bool
moduleNameWithinPrefix modulePrefix moduleName =
  moduleNameComponents modulePrefix `isPrefixOf` moduleNameComponents moduleName

moduleNameComponents :: String -> [String]
moduleNameComponents moduleName =
  case break (== '.') moduleName of
    (component, []) ->
      [component]
    (component, _ : remainingName) ->
      component : moduleNameComponents remainingName
