module LawManifest
  ( lawManifestCase,
    lawProperty,
  )
where

import Moonlight.Core (IsLawName (..))
import Moonlight.Core
  ( canonicalTheoremManifestNames,
    parseTheoremManifestNames,
    renderTheoremManifestJson,
  )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Testable, testProperty)

lawProperty :: (IsLawName law, Testable property) => law -> property -> TestTree
lawProperty lawName =
  testProperty (lawNameText lawName)

lawManifestCase :: IsLawName law => String -> [law] -> TestTree
lawManifestCase owner lawNames =
  testCase (owner <> " law names satisfy proof manifest boundary") $
    case parseTheoremManifestNames renderedManifest of
      Left manifestError ->
        assertFailure (show manifestError)
      Right parsedLawNames -> do
        length manifestNames @?= length canonicalLawNames
        parsedLawNames @?= canonicalLawNames
  where
    manifestNames =
      lawNameText <$> lawNames
    canonicalLawNames =
      canonicalTheoremManifestNames manifestNames
    renderedManifest =
      renderTheoremManifestJson manifestNames
