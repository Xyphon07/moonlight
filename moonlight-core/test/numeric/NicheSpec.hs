{-# LANGUAGE DerivingStrategies #-}

module NicheSpec (tests) where

import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import Prelude (Double, Either (..), Eq, Int, Maybe (..), Ord, Show, String, fromIntegral, map, maxBound, show, traverse, uncurry, (/), (+), (.))
import Moonlight.Core (IsLawName (..), constructorLawName)
import Moonlight.Core
  ( ActiveStressor,
    MoonlightError (NonFiniteValue),
    MoonlightErrorContext (CanonicalizeContext),
    NicheValidationError (..),
    NonFiniteInput (NaNInput),
    activeStressorId,
    activeStressorIntensity,
    activeStressorSetEntries,
    activeStressorSetFromList,
    activeStressorSetTopEntries,
    contextSignatureBins,
    mkActiveStressor,
    mkContextSignature,
    mkStressorId,
    mkTopoSample,
    renderStressorId,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), assertFailure, testCase)

data NicheLawName
  = NicheActiveStressorSetCanonicalOrder
  | NicheActiveStressorSetDeduplicatesByMaximumIntensity
  | NicheActiveStressorTopEntriesPrefix
  | NicheActiveStressorTopEntriesSaturatesHugeLimit
  | NicheContextSignaturePreservesBins
  | NicheStressorIdRejectsEmpty
  | NicheTopoSampleRejectsNegativeSlope
  | NicheTopoSampleRejectsNonFiniteCanonicalScalar
  deriving stock (Eq, Ord, Show)

instance IsLawName NicheLawName where
  lawNameText = constructorLawName . show

lawCase :: NicheLawName -> Assertion -> TestTree
lawCase lawName =
  testCase (lawNameText lawName)

tests :: TestTree
tests =
  testGroup
    "Niche"
    [ lawCase NicheStressorIdRejectsEmpty testInvalidStressorId,
      lawCase NicheTopoSampleRejectsNegativeSlope testInvalidTopoSample,
      lawCase NicheTopoSampleRejectsNonFiniteCanonicalScalar testInvalidTopoSampleCanonicalScalar,
      lawCase NicheActiveStressorSetDeduplicatesByMaximumIntensity testActiveStressorSetCanonical,
      lawCase NicheActiveStressorSetCanonicalOrder testActiveStressorSetCanonical,
      lawCase NicheActiveStressorTopEntriesPrefix testActiveStressorTopEntries,
      lawCase NicheActiveStressorTopEntriesSaturatesHugeLimit testActiveStressorTopEntriesHugeLimit,
      lawCase NicheContextSignaturePreservesBins testContextSignature
    ]

testInvalidStressorId :: Assertion
testInvalidStressorId =
  mkStressorId (pack "") @?= Nothing

testInvalidTopoSample :: Assertion
testInvalidTopoSample =
  mkTopoSample (-1.0) 0.0 0.0 @?= Left (NegativeTopoSlope (-1.0))

testInvalidTopoSampleCanonicalScalar :: Assertion
testInvalidTopoSampleCanonicalScalar =
  mkTopoSample (0 / 0) 0.0 0.0 @?= Left (NicheCanonicalScalarRejected (NonFiniteValue CanonicalizeContext NaNInput))

testActiveStressorSetCanonical :: Assertion
testActiveStressorSetCanonical =
  case buildStressors of
    Left err -> assertFailure err
    Right stressors ->
      map describeStressor (activeStressorSetEntries (activeStressorSetFromList stressors))
        @?= [(pack "ash", 0.9), (pack "bog", 0.8), (pack "cinder", 0.8)]

testActiveStressorTopEntries :: Assertion
testActiveStressorTopEntries =
  case buildStressors of
    Left err -> assertFailure err
    Right stressors ->
      map describeStressor (activeStressorSetTopEntries 2 (activeStressorSetFromList stressors))
        @?= [(pack "ash", 0.9), (pack "bog", 0.8)]

testActiveStressorTopEntriesHugeLimit :: Assertion
testActiveStressorTopEntriesHugeLimit =
  case buildStressors of
    Left err -> assertFailure err
    Right stressors ->
      map describeStressor (activeStressorSetTopEntries justPastIntLimit (activeStressorSetFromList stressors))
        @?= [(pack "ash", 0.9), (pack "bog", 0.8), (pack "cinder", 0.8)]

justPastIntLimit :: Natural
justPastIntLimit =
  fromIntegral (maxBound :: Int) + 1


testContextSignature :: Assertion
testContextSignature =
  contextSignatureBins (mkContextSignature [3, 1, 4, 1]) @?= [3, 1, 4, 1]

buildStressors :: Either String [ActiveStressor]
buildStressors =
  case traverse mkStressorId stressorNames of
    Nothing -> Left "expected valid stressor ids"
    Just [bogId, ashId, cinderId] ->
      first show
        ( traverse
            (uncurry mkActiveStressor)
            [ (bogId, 0.2),
              (ashId, 0.9),
              (bogId, 0.8),
              (cinderId, 0.8)
            ]
        )
    Just _ -> Left "unexpected stressor id cardinality"
  where
    stressorNames = map pack ["bog", "ash", "cinder"]

describeStressor :: ActiveStressor -> (Text, Double)
describeStressor stressor =
  (renderStressorId (activeStressorId stressor), activeStressorIntensity stressor)
