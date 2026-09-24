module Main (main) where

import Data.ByteString qualified as ByteString
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Word (Word8)
import Moonlight.Planar.Exhibit.PersistenceRose
  ( AlphaEclipseReceipt (..),
    AlphaEclipseScene,
    PersistenceRoseAnimationReceipt (..),
    PersistenceRoseReceipt (..),
    PersistenceRoseScene,
    alphaEclipseReceipt,
    buildAlphaEclipseScene,
    buildPersistenceRoseScene,
    persistenceRoseFrameSpecs,
    persistenceRoseReceipt,
    renderPersistenceRoseFrame,
    renderPersistenceRosePoster,
    renderAlphaEclipse,
    stampPersistenceRoseGif,
    verifyPersistenceRoseGif,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )

main :: IO ()
main = defaultMain persistenceRoseTests

persistenceRoseTests :: TestTree
persistenceRoseTests =
  testGroup
    "persistence rose"
    [ testCase "scene builds from canonical owners" testSceneBuilds,
      testCase "hero holes agree with persistent beta one" testHeroAgreement,
      testCase "Sibson field has no fallback" testSibsonField,
      testCase "poster is deterministic SVG" testPosterDeterminism,
      testCase "frame sweep closes exactly" testFrameSweepClosure,
      testCase "alpha eclipse is the exact resident square interval" testAlphaEclipseSemantics,
      testCase "alpha eclipse is deterministic SVG" testAlphaEclipseDeterminism,
      testCase "GIF provenance binds the canonical frame section" testGifProvenance
    ]

withScene :: (PersistenceRoseScene -> Assertion) -> Assertion
withScene = withResult buildPersistenceRoseScene

withAlphaEclipse :: (AlphaEclipseScene -> Assertion) -> Assertion
withAlphaEclipse = withResult buildAlphaEclipseScene

withResult :: Show failure => Either failure value -> (value -> Assertion) -> Assertion
withResult result assertion = either (assertFailure . show) assertion result

testSceneBuilds :: Assertion
testSceneBuilds =
  withScene $ \scene -> do
    let receipt = persistenceRoseReceipt scene
    assertBool "the deterministic cover should contain at least one hundred sites" (roseSiteCount receipt >= 100)
    assertBool "Delaunay edges should be nontrivial" (roseEdgeCount receipt > roseSiteCount receipt)
    assertBool "bounded faces should be nontrivial" (roseFaceCount receipt > 0)
    assertBool "the alpha filtration should have many exact critical births" (roseCriticalBirthCount receipt > 20)
    assertBool "the exact barcode should contain finite H1 intervals" (roseFinitePersistentHoleCount receipt > 0)
    assertBool "the Voronoi dual should contain finite segments" (roseVoronoiSegmentCount receipt > 0)
    assertBool "the exact overlay and Minkowski mask should remain nonempty" (roseMaskComponentCount receipt > 0)

testHeroAgreement :: Assertion
testHeroAgreement =
  withScene $ \scene -> do
    let receipt = persistenceRoseReceipt scene
    assertBool "the hero state should display at least seven independent holes" (roseHeroBettiOne receipt >= 7)
    roseHeroGeometricHoleCount receipt @?= roseHeroBettiOne receipt

testSibsonField :: Assertion
testSibsonField =
  withScene $ \scene -> do
    let receipt = persistenceRoseReceipt scene
    assertBool "the field should contain a dense interior sample" (roseNaturalNeighborSampleCount receipt > 300)
    roseNaturalNeighborFallbackCount receipt @?= 0

testPosterDeterminism :: Assertion
testPosterDeterminism =
  withScene $ \scene -> do
    let firstPoster = renderPersistenceRosePoster scene
        secondPoster = renderPersistenceRosePoster scene
    firstPoster @?= secondPoster
    assertBool "poster should be an SVG document" ("<svg" `isPrefixOf` firstPoster && "</svg>\n" `isSuffixOf` firstPoster)
    assertBool
      "poster should explain beta one semantically"
      ("7 independent one-dimensional holes" `isInfixOf` firstPoster)
    assertBool
      "poster should identify the scale-local topology"
      ("TOPOLOGICAL HOLES AT THIS SCALE" `isInfixOf` firstPoster)
    assertBool
      "poster should omit the old alpha subtitle"
      (not ("EXACT ALPHA / PERSISTENT H1" `isInfixOf` firstPoster))
    assertBool
      "poster should omit the capability footer"
      (not ("VORONOI DUAL · SIBSON FIELD · EXACT OVERLAY" `isInfixOf` firstPoster))
    assertBool
      "poster should omit the receipt footer"
      (not ("glued bounded complement components at the hero state" `isInfixOf` firstPoster))

testFrameSweepClosure :: Assertion
testFrameSweepClosure =
  withScene $ \scene ->
    case persistenceRoseFrameSpecs 49 of
      Left frameError -> assertFailure (show frameError)
      Right frames ->
        case endpoints frames of
          Nothing -> assertFailure "a valid frame count produced no frames"
          Just (firstFrame, finalFrame) -> do
            let firstSvg = renderPersistenceRoseFrame scene firstFrame
            firstSvg @?= renderPersistenceRoseFrame scene finalFrame
            assertBool
              "animation should omit the old descent subtitle"
              (not ("ALPHA DESCENT / GLUING" `isInfixOf` firstSvg))

testAlphaEclipseSemantics :: Assertion
testAlphaEclipseSemantics =
  withAlphaEclipse $ \scene -> do
    let receipt = alphaEclipseReceipt scene
    eclipseSiteCount receipt @?= 4
    eclipseEdgeCount receipt @?= 5
    eclipseFaceCount receipt @?= 2
    (eclipseHoleBirthNumerator receipt, eclipseHoleBirthDenominator receipt) @?= (1, 1)
    (eclipseHoleDeathNumerator receipt, eclipseHoleDeathDenominator receipt) @?= (2, 1)
    (eclipseBettiBeforeBirth receipt, eclipseBettiAtBirth receipt, eclipseBettiAtDeath receipt)
      @?= (0, 1, 0)

testAlphaEclipseDeterminism :: Assertion
testAlphaEclipseDeterminism =
  withAlphaEclipse $ \scene -> do
    let firstSvg = renderAlphaEclipse scene
        secondSvg = renderAlphaEclipse scene
    firstSvg @?= secondSvg
    assertBool "eclipse should be an SVG document" ("<svg" `isPrefixOf` firstSvg && "</svg>\n" `isSuffixOf` firstSvg)
    assertBool "eclipse should name the exact finite interval" ("birth α² = 1" `isInfixOf` firstSvg && "death α² = 2" `isInfixOf` firstSvg)
    assertBool "eclipse should identify one fixed resident DCEL" ("ONE FIXED DELAUNAY DCEL" `isInfixOf` firstSvg)

testGifProvenance :: Assertion
testGifProvenance =
  withScene $ \scene ->
    case stampPersistenceRoseGif scene syntheticCanonicalGif of
      Left gifError -> assertFailure (show gifError)
      Right (stampedReceipt, stamped) -> do
        roseAnimationFrameCount stampedReceipt @?= 65
        case stampPersistenceRoseGif scene stamped of
          Left gifError -> assertFailure (show gifError)
          Right (restampedReceipt, restamped) -> do
            restampedReceipt @?= stampedReceipt
            restamped @?= stamped
        case verifyPersistenceRoseGif scene stamped of
          Left gifError -> assertFailure (show gifError)
          Right receipt -> do
            roseAnimationFrameCount receipt @?= 65
            roseAnimationFramesPerSecond receipt @?= 8
            (roseAnimationWidth receipt, roseAnimationHeight receipt) @?= (960, 540)

syntheticCanonicalGif :: ByteString.ByteString
syntheticCanonicalGif =
  ByteString.pack
    ( [ 0x47, 0x49, 0x46, 0x38, 0x39, 0x61,
        0xc0, 0x03,
        0x1c, 0x02,
        0x00, 0x00, 0x00
      ]
        <> concatMap syntheticGifFrame [0 .. 64]
        <> [0x3b]
    )

syntheticGifFrame :: Int -> [Word8]
syntheticGifFrame frameIndex =
  [ 0x21, 0xf9, 0x04, 0x00,
    if even frameIndex then 0x0d else 0x0c,
    0x00, 0x00, 0x00,
    0x2c,
    0x00, 0x00,
    0x00, 0x00,
    0x01, 0x00,
    0x01, 0x00,
    0x00,
    0x02,
    0x02, 0x44, 0x01,
    0x00
  ]

endpoints :: [value] -> Maybe (value, value)
endpoints values =
  case values of
    [] -> Nothing
    firstValue : remainingValues ->
      Just (firstValue, foldl' (\_ nextValue -> nextValue) firstValue remainingValues)
