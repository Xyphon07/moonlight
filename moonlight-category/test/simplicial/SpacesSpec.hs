{-# LANGUAGE DerivingStrategies #-}

module SpacesSpec
  ( carrierTests,
    lawfulCarrierSpec,
  )
where

import Data.Kind (Type)
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Category.Simplicial
  ( Dimension (..),
    GeneratedSSetObstruction (..),
    TruncatedNormalizedSSet,
    boundarySimplex,
    boundarySimplexGenerated,
    checkSimplicialLaws,
    generatedSimplicesAtDimension,
    hornSimplex,
    hornSimplexGenerated,
    indexSimplexIn,
    mkGeneratedSSet,
    normalizeGeneratedSSet,
    simplicialLawEq,
    simplicesAtDimension,
    standardSimplex,
    standardSimplexGenerated,
    unindexSimplex,
    validateGeneratedSSet,
  )
import Laws.Suite (LawSuiteConfig (..), mkLawfulCarrierSpec)
import Moonlight.Pale.Test.Laws.Suite (LawSuite)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC

type GeneratedStandardSimplex :: Type
data GeneratedStandardSimplex = GeneratedStandardSimplex
  { generatedSimplexDimension :: Natural,
    generatedTruncationBound :: Natural
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary GeneratedStandardSimplex where
  arbitrary = do
    simplexDimension <- QC.chooseInt (0, 4)
    truncationBound <- QC.chooseInt (max 0 simplexDimension, 5)
    pure
      GeneratedStandardSimplex
        { generatedSimplexDimension = fromIntegral simplexDimension,
          generatedTruncationBound = fromIntegral truncationBound
        }

carrierToSSet :: GeneratedStandardSimplex -> TruncatedNormalizedSSet [Natural]
carrierToSSet generatedValue =
  standardSimplex
    (generatedSimplexDimension generatedValue)
    (generatedTruncationBound generatedValue)

standardSimplexLawsHold :: GeneratedStandardSimplex -> QC.Property
standardSimplexLawsHold generatedValue =
  case checkSimplicialLaws (carrierToSSet generatedValue) of
    Right () -> QC.property True
    Left obstructions ->
      QC.counterexample
        ( "simplicial law obstruction count="
            <> show (length (NonEmpty.toList obstructions))
            <> "\nfirst obstruction="
            <> show (NonEmpty.head obstructions)
        )
        False

assertMaybeGeneratedSetValid :: String -> Maybe generated -> (generated -> Either obstructions ()) -> IO ()
assertMaybeGeneratedSetValid label generatedResult validateGenerated =
  case generatedResult of
    Nothing ->
      assertFailure (label <> " constructor rejected valid dimensions")
    Just generatedSet ->
      case validateGenerated generatedSet of
        Right () -> pure ()
        Left _ -> assertFailure (label <> " failed generated-set validation")

assertNormalizedLawsValid :: String -> TruncatedNormalizedSSet [Natural] -> IO ()
assertNormalizedLawsValid label simplicialSet =
  case checkSimplicialLaws simplicialSet of
    Right () -> pure ()
    Left obstructions ->
      assertFailure (label <> " failed simplicial laws: " <> show (NonEmpty.head obstructions))

assertNormalizedRowsEqual :: String -> Natural -> TruncatedNormalizedSSet [Natural] -> TruncatedNormalizedSSet [Natural] -> IO ()
assertNormalizedRowsEqual label upperBound expected actual =
  traverse_
    ( \dimensionValue ->
        assertEqual
          (label <> " dimension " <> show dimensionValue)
          (simplicesAtDimension expected dimensionValue)
          (simplicesAtDimension actual dimensionValue)
    )
    [0 .. upperBound]

pointRows :: Natural -> [[Natural]]
pointRows dimensionValue =
  case dimensionValue of
    0 -> [[0]]
    1 -> [[0, 0]]
    _ -> []

brokenPointRows :: Natural -> [[Natural]]
brokenPointRows dimensionValue =
  case dimensionValue of
    0 -> [[0]]
    _ -> []

pointFace :: dimension -> finite -> [Natural] -> Maybe [Natural]
pointFace _ _ simplexValue =
  case simplexValue of
    [0, 0] -> Just [0]
    _ -> Nothing

pointDegeneracy :: dimension -> finite -> [Natural] -> Maybe [Natural]
pointDegeneracy _ _ simplexValue =
  case simplexValue of
    [0] -> Just [0, 0]
    _ -> Nothing

lawfulCarrierSpec :: LawSuite
lawfulCarrierSpec =
  mkLawfulCarrierSpec
    "standard-simplex"
    LawSuiteConfig
      { lawSuiteName = "standard simplex simplicial laws",
        lawSuiteMaxSuccess = 300,
        lawSuiteCarrierToSSet = carrierToSSet,
        lawSuiteEquality = simplicialLawEq,
        lawSuiteRenderSimplex = show
      }

carrierTests :: TestTree
carrierTests =
  testGroup
    "Spaces"
    [ testCase "mkGeneratedSSet derives degenerates from degeneracy images" $
        case mkGeneratedSSet 1 pointRows pointFace pointDegeneracy of
          Left obstruction -> assertFailure ("expected checked generated set, got " <> show obstruction)
          Right generatedSet ->
            assertEqual
              "degenerate edge is removed by derived witness"
              []
              (simplicesAtDimension (normalizeGeneratedSSet generatedSet) 1),
      testCase "mkGeneratedSSet rejects degeneracy images outside the carrier" $
        case mkGeneratedSSet 1 brokenPointRows pointFace pointDegeneracy of
          Left obstruction ->
            assertEqual
              "degeneracy closure obstruction"
              (GeneratedDegeneracyOutsideCarrier 0 0 [0] [0, 0])
              (NonEmpty.head obstruction)
          Right _ -> assertFailure "expected generated-set construction obstruction",
      testCase "indexSimplexIn only tags simplices present at that dimension" $ do
        let simplex = standardSimplex 1 1
        case indexSimplexIn simplex (Dimension @1) [0, 1] of
          Nothing -> assertFailure "expected edge to index at dimension 1"
          Just indexed -> assertEqual "indexed edge" [0, 1] (unindexSimplex indexed)
        assertEqual
          "vertex is not a 1-simplex"
          Nothing
          (unindexSimplex <$> indexSimplexIn simplex (Dimension @1) [0]),
      testCase "standard 2-simplex has combinatorial simplex counts" $ do
        let generatedSet = standardSimplexGenerated 2 2
            simplex = standardSimplex 2 2
        assertEqual "generated 0-simplices" 3 (length (generatedSimplicesAtDimension generatedSet 0))
        assertEqual "generated 1-simplices" 6 (length (generatedSimplicesAtDimension generatedSet 1))
        assertEqual "generated 2-simplices" 10 (length (generatedSimplicesAtDimension generatedSet 2))
        assertEqual "normalized 0-simplices" 3 (length (simplicesAtDimension simplex 0))
        assertEqual "normalized 1-simplices" 3 (length (simplicesAtDimension simplex 1))
        assertEqual "normalized 2-simplices" 1 (length (simplicesAtDimension simplex 2)),
      testCase "standard simplex direct constructor matches generated normalization on small cases" $
        traverse_
          ( \(simplexDimension, truncationBound) ->
              assertNormalizedRowsEqual
                ("standard simplex " <> show simplexDimension <> " <= " <> show truncationBound)
                truncationBound
                (normalizeGeneratedSSet (standardSimplexGenerated simplexDimension truncationBound))
                (standardSimplex simplexDimension truncationBound)
          )
          [(0, 0), (1, 2), (2, 2), (3, 3), (4, 3)],
      testCase "standard 6-simplex direct constructor keeps only nondegenerate rows" $ do
        let simplex = standardSimplex 6 4
        assertEqual
          "nondegenerate row counts"
          [7, 21, 35, 35, 21]
          (length . simplicesAtDimension simplex <$> [0 .. 4]),
      testCase "boundary 2-simplex removes top nondegenerate triangle" $ do
        let simplex = boundarySimplex 2 2
        assertEqual "boundary 0-simplices" 3 (length (simplicesAtDimension simplex 0))
        assertEqual "boundary 1-simplices" 3 (length (simplicesAtDimension simplex 1))
        assertEqual "boundary 2-simplices" 0 (length (simplicesAtDimension simplex 2)),
      testCase "boundary simplex direct constructor matches generated normalization on small cases" $
        traverse_
          ( \(simplexDimension, truncationBound) ->
              assertNormalizedRowsEqual
                ("boundary simplex " <> show simplexDimension <> " <= " <> show truncationBound)
                truncationBound
                (normalizeGeneratedSSet (boundarySimplexGenerated simplexDimension truncationBound))
                (boundarySimplex simplexDimension truncationBound)
          )
          [(0, 0), (1, 2), (2, 2), (3, 3)],
      testCase "boundary 3-simplex direct constructor excludes the top identity simplex" $ do
        let simplex = boundarySimplex 3 3
        assertEqual "boundary 2-faces" 4 (length (simplicesAtDimension simplex 2))
        assertEqual "boundary 3-simplices" [] (simplicesAtDimension simplex 3)
        assertBool "top identity is absent" ([0, 1, 2, 3] `notElem` simplicesAtDimension simplex 3),
      testCase "boundary generated rows are closed by image omission beyond top dimension" $ do
        let boundaryOne = boundarySimplexGenerated 1 2
            boundaryTwo = boundarySimplexGenerated 2 3
            boundaryOneRows = generatedSimplicesAtDimension boundaryOne 2
            boundaryTwoRows = generatedSimplicesAtDimension boundaryTwo 3
        assertEqual "degenerate rows over boundary vertices" [[0, 0, 0], [1, 1, 1]] boundaryOneRows
        assertBool "boundary excludes degeneracies whose image hits every vertex" ([0, 0, 1, 2] `notElem` boundaryTwoRows)
        assertBool "boundary excludes second all-vertex degeneracy" ([0, 1, 1, 2] `notElem` boundaryTwoRows)
        assertBool "boundary excludes third all-vertex degeneracy" ([0, 1, 2, 2] `notElem` boundaryTwoRows)
        assertBool "boundary keeps rows omitting a vertex" ([0, 0, 1, 1] `elem` boundaryTwoRows)
        assertBool "boundary keeps rows omitting middle vertex" ([0, 0, 2, 2] `elem` boundaryTwoRows),
      testCase "horn removes one boundary face" $
        case hornSimplex 2 1 1 of
          Nothing -> assertBool "expected horn in dimension 2" False
          Just simplex -> do
            assertEqual "horn 0-simplices" 3 (length (simplicesAtDimension simplex 0))
            assertEqual "horn 1-simplices" 2 (length (simplicesAtDimension simplex 1)),
      testCase "horn simplex direct constructor matches generated normalization on small cases" $
        traverse_
          ( \(simplexDimension, missingFaceIndex, truncationBound) ->
              case (hornSimplexGenerated simplexDimension missingFaceIndex truncationBound, hornSimplex simplexDimension missingFaceIndex truncationBound) of
                (Just generatedSet, Just simplex) ->
                  assertNormalizedRowsEqual
                    ("horn simplex " <> show simplexDimension <> " missing " <> show missingFaceIndex <> " <= " <> show truncationBound)
                    truncationBound
                    (normalizeGeneratedSSet generatedSet)
                    simplex
                _ -> assertFailure "expected valid generated and normalized horn"
          )
          [(2, 0, 2), (2, 1, 2), (3, 1, 3)],
      testCase "horn 3-simplex direct constructor excludes exactly the missing face" $
        case hornSimplex 3 1 2 of
          Nothing -> assertFailure "expected horn in dimension 3"
          Just simplex -> do
            let twoFaces = simplicesAtDimension simplex 2
            assertBool "face opposite vertex 0 is retained" ([1, 2, 3] `elem` twoFaces)
            assertBool "face opposite vertex 2 is retained" ([0, 1, 3] `elem` twoFaces)
            assertBool "face opposite vertex 3 is retained" ([0, 1, 2] `elem` twoFaces)
            assertBool "missing face opposite vertex 1 is absent" ([0, 2, 3] `notElem` twoFaces),
      testCase "horn generated rows are the union of all non-missing faces in every dimension" $
        case hornSimplexGenerated 2 1 2 of
          Nothing -> assertBool "expected generated horn in dimension 2" False
          Just simplex -> do
            let edgeRows = generatedSimplicesAtDimension simplex 1
                triangleRows = generatedSimplicesAtDimension simplex 2
            assertBool "horn keeps face opposite vertex 0" ([1, 2] `elem` edgeRows)
            assertBool "horn keeps face opposite vertex 2" ([0, 1] `elem` edgeRows)
            assertBool "horn removes the missing face" ([0, 2] `notElem` edgeRows)
            assertBool "horn excludes degeneracy over the missing face" ([0, 0, 2] `notElem` triangleRows)
            assertBool "horn excludes the other degeneracy over the missing face" ([0, 2, 2] `notElem` triangleRows)
            assertBool "horn keeps degeneracy over retained face" ([0, 0, 1] `elem` triangleRows),
      testCase "exported generated spaces validate through the checked generated-set boundary" $ do
        case validateGeneratedSSet (standardSimplexGenerated 3 3) of
          Right () -> pure ()
          Left obstruction -> assertFailure ("standard simplex failed validation: " <> show (NonEmpty.head obstruction))
        case validateGeneratedSSet (boundarySimplexGenerated 3 3) of
          Right () -> pure ()
          Left obstruction -> assertFailure ("boundary simplex failed validation: " <> show (NonEmpty.head obstruction))
        assertMaybeGeneratedSetValid
          "horn simplex"
          (hornSimplexGenerated 3 1 3)
          validateGeneratedSSet,
      testCase "boundary and horn normalized spaces satisfy simplicial identities" $ do
        assertNormalizedLawsValid "boundary simplex" (boundarySimplex 3 3)
        case hornSimplex 3 1 3 of
          Nothing -> assertFailure "expected horn in dimension 3"
          Just simplex -> assertNormalizedLawsValid "horn simplex" simplex,
      testCase "hornSimplex rejects dimension 0" $
        case hornSimplex 0 0 0 of
          Nothing -> pure ()
          Just _ -> assertBool "expected hornSimplex to reject dimension 0" False,
      testCase "hornSimplex rejects out-of-bounds missing face" $
        case hornSimplex 2 3 2 of
          Nothing -> pure ()
          Just _ -> assertBool "expected hornSimplex to reject out-of-bounds face index" False,
      testCase "hornSimplexGenerated rejects dimension 0" $
        case hornSimplexGenerated 0 0 0 of
          Nothing -> pure ()
          Just _ -> assertBool "expected hornSimplexGenerated to reject dimension 0" False,
      QC.testProperty "standard simplices satisfy simplicial identities" $
        QC.withNumTests 200
          standardSimplexLawsHold
    ]
