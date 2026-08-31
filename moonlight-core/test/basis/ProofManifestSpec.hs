{-# LANGUAGE CPP #-}

module ProofManifestSpec (tests) where

import Moonlight.Core
  ( ProofManifestError (..),
    parseTheoremManifestNames
  )
import SourceShape (assertSourceShape)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "ProofManifest"
    [ testCase "parser accepts the compact canonical manifest shape" testParseCanonicalManifest,
      testCase "parser rejects malformed manifest payloads" testRejectMalformedManifest,
      testCase "parser rejects non-JSON string escapes" testRejectNonJsonStrings,
      testCase "proof manifest codec stays parser-backed and normalized" testProofManifestBoundaryShape
    ]

testParseCanonicalManifest :: IO ()
testParseCanonicalManifest = do
  parseTheoremManifestNames "  {\"theorems\":[\"alpha\",\"beta.gamma\"]}\n"
    @?= Right ["alpha", "beta.gamma"]
  parseTheoremManifestNames "{\"theorems\": [\"alpha\"]}"
    @?= Right ["alpha"]

testRejectMalformedManifest :: IO ()
testRejectMalformedManifest = do
  parseTheoremManifestNames "{\"theorems\":[alpha]}"
    @?= Left ProofManifestParseFailure
  parseTheoremManifestNames "{\"laws\":[\"alpha\"]}"
    @?= Left ProofManifestParseFailure
  parseTheoremManifestNames "{\"theorems\":[\"\"]}"
    @?= Left EmptyTheoremManifestName
  parseTheoremManifestNames "{\"theorems\":[\" alpha\"]}"
    @?= Left (WhitespacePaddedTheoremManifestName " alpha")
  parseTheoremManifestNames "{\"theorems\":[\"alpha\",\"alpha\"]}"
    @?= Left (DuplicateTheoremManifestName "alpha")

testRejectNonJsonStrings :: IO ()
testRejectNonJsonStrings = do
  parseTheoremManifestNames "{\"theorems\":[\"alpha\nbeta\"]}"
    @?= Left ProofManifestParseFailure
  parseTheoremManifestNames "{\"theorems\":[\"\\uD800\"]}"
    @?= Left ProofManifestParseFailure

testProofManifestBoundaryShape :: IO ()
testProofManifestBoundaryShape =
  assertSourceShape
    __FILE__
    "src-basis/Moonlight/Core/ProofManifest.hs"
    [ "renderTheoremManifestJson theoremIdentifiers =",
      "intercalate \",\" (map quoteJsonString (canonicalTheoremManifestNames theoremIdentifiers))",
      "parseTheoremManifestNames source =",
      "parseManifestJson source >>= validateTheoremManifestNames",
      "validateTheoremManifestNames",
      "parseManifestJson source",
      "Set.toAscList . Set.fromList",
      "duplicateTheoremName =",
      "firstDuplicate",
      "unicodeEscapeParser"
    ]
    [ "maybe [] id (parseManifestJson source)",
      "last parses"
    ]
