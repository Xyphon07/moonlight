module ModuleSurfaceSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Moonlight.Pale.Ghc.ModuleSurface
  ( ExportChildSpec (..),
    ExportItem (..),
    ExportSpec (..),
    ModuleSurface (..),
    moduleSurfaceFromGhcPs,
    parseHsModule,
    renderGhcParseFailure,
    unParsedModuleName,
    unParsedName,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "pale.module-surface"
    [ testCase "GHC2024 keeps pattern available as a type-variable name" $
        assertParses
          "PatternTypeVariable.hs"
          [ "{-# LANGUAGE GHC2024 #-}",
            "module PatternTypeVariable where",
            "",
            "foo :: host pattern var -> ()",
            "foo _ = ()"
          ],
      testCase "PatternSynonyms is enabled only when requested by LANGUAGE pragma" $
        assertParses
          "PatternSynonymFixture.hs"
          [ "{-# LANGUAGE PatternSynonyms #-}",
            "module PatternSynonymFixture where",
            "",
            "pattern Unit = ()",
            "value = Unit"
          ],
      testCase "multiline LANGUAGE pragmas are parsed by GHC's header parser" $
        assertParses
          "MultilinePatternSynonymFixture.hs"
          [ "{-# LANGUAGE",
            "      PatternSynonyms",
            "  #-}",
            "module MultilinePatternSynonymFixture where",
            "pattern Unit = ()"
          ],
      testCase "OPTIONS_GHC extension flags are parsed by GHC's header parser" $
        assertParses
          "OptionsPatternSynonymFixture.hs"
          [ "{-# OPTIONS_GHC -XPatternSynonyms #-}",
            "module OptionsPatternSynonymFixture where",
            "pattern Unit = ()"
          ],
      testCase "a missing export list remains implicit rather than becoming empty" $ do
        moduleSurface <-
          parseSurface
            "Implicit.hs"
            ["module Implicit where", "value = ()"]
        surfaceExports moduleSurface @?= ImplicitExports,
      testCase "explicit exports preserve namespace, children, and module re-exports" $ do
        moduleSurface <-
          parseSurface
            "Explicit.hs"
            [ "{-# LANGUAGE ExplicitNamespaces #-}",
              "{-# LANGUAGE PatternSynonyms #-}",
              "module Explicit (value, Type(..), pattern Unit, module Data.List) where",
              "import Data.List",
              "data Type = Constructor",
              "pattern Unit = ()",
              "value = ()"
            ]
        case surfaceExports moduleSurface of
          ExplicitExports
            [ ExportValue valueName,
              ExportType typeName AllExportedChildren,
              ExportPattern patternName,
              ExportModule moduleName
              ] -> do
                unParsedName valueName @?= "value"
                unParsedName typeName @?= "Type"
                unParsedName patternName @?= "Unit"
                unParsedModuleName moduleName @?= "Data.List"
          otherExports ->
            assertFailure ("unexpected explicit export structure: " <> show otherExports)
    ]

assertParses :: FilePath -> [String] -> IO ()
assertParses sourcePath sourceLines =
  case parseHsModule sourcePath (unlines sourceLines) of
    Right _ ->
      pure ()
    Left parserError ->
      assertFailure
        ("expected parser success for " <> sourcePath <> ":\n" <> renderGhcParseFailure parserError)

parseSurface :: FilePath -> [String] -> IO ModuleSurface
parseSurface sourcePath sourceLines =
  case first renderGhcParseFailure (parseHsModule sourcePath (unlines sourceLines))
    >>= first show . moduleSurfaceFromGhcPs of
    Right moduleSurface ->
      pure moduleSurface
    Left surfaceError ->
      assertFailure ("expected module-surface success for " <> sourcePath <> ":\n" <> surfaceError)
