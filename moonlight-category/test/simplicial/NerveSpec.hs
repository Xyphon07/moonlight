
module NerveSpec
  ( carrierTests,
    lawfulCarrierSpec,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Moonlight.Category.Effect.Fixture.FinCat (sampleFinCat)
import Moonlight.Category
  ( ComposableChain,
    FinCat,
    FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    allObjects,
    chainMorphisms,
    chainStartObject,
    chainsOfDimension,
    finMorId,
    finObjId,
    mkFinCat
  )
import Moonlight.Category.Simplicial
  ( NerveSimplex,
    applyFaceAtDimension,
    fillNerveInnerHorn,
    generatedSimplicesAtDimension,
    mkHorn,
    mkInnerHorn,
    nerveSimplexChain,
    nerveSimplexDimension,
    nerveSimplexFromChain,
    normalizeGeneratedSSet,
    normalizedNerve,
    simplicesAtDimension,
    unnormalizedNerve,
  )
import Laws.Suite (LawSuiteConfig (..), mkLawfulCarrierSpec)
import Moonlight.Pale.Test.Laws.Suite (LawSuite)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import qualified Test.Tasty.QuickCheck as QC

type GeneratedFiniteCategory :: Type
data GeneratedFiniteCategory = GeneratedFiniteCategory
  { generatedCategory :: FinCat,
    generatedTruncation :: Natural
  }

instance Show GeneratedFiniteCategory where
  show generatedValue =
    "GeneratedFiniteCategory(objects="
      <> show (length (allObjects (generatedCategory generatedValue)))
      <> ", truncation="
      <> show (generatedTruncation generatedValue)
      <> ")"

instance QC.Arbitrary GeneratedFiniteCategory where
  arbitrary = do
    objectCount <- QC.chooseInt (1, 5)
    let objectIds = [0 .. objectCount - 1]
        candidatePairs = [(sourceId, targetId) | sourceId <- objectIds, targetId <- objectIds, sourceId < targetId]
    pairFlags <- QC.vectorOf (length candidatePairs) QC.arbitrary
    let chosenPairs =
          zip candidatePairs pairFlags
            & filter snd
            & map fst
        closurePairs = transitiveClosure (Set.fromList chosenPairs)
        morphismMap = buildMorphismMap closurePairs
        compositionMap = buildCompositionMap closurePairs
        categoryValue =
          case mkFinCat (Set.fromList (fmap objectId objectIds)) morphismMap compositionMap of
            Right validCategory -> validCategory
            Left _ -> sampleFinCat
    truncationValue <- QC.chooseInt (2, 4)
    pure
      ( GeneratedFiniteCategory
          { generatedCategory = categoryValue,
            generatedTruncation = fromIntegral truncationValue
          }
      )

pairCode :: Int -> Int -> Int
pairCode sourceId targetId = sourceId * 1024 + targetId

objectId :: Int -> FinObjectId
objectId = FinObjectId

generatorMorphismId :: Int -> FinMorphismId
generatorMorphismId = FinGeneratorMorphismId . FinGeneratorId

pairGenerator :: Int -> Int -> FinMorphismId
pairGenerator sourceId targetId = generatorMorphismId (pairCode sourceId targetId)

transitiveClosure :: Set.Set (Int, Int) -> Set.Set (Int, Int)
transitiveClosure relation =
  let composedPairs =
        Set.toList relation
          >>= (\(sourceId, middleId) -> Set.toList relation & filter (\(middleId', _) -> middleId == middleId') & map (\(_, targetId) -> (sourceId, targetId)))
          & Set.fromList
      nextRelation = Set.union relation composedPairs
   in if nextRelation == relation
        then relation
        else transitiveClosure nextRelation

buildMorphismMap :: Set.Set (Int, Int) -> Map.Map (FinObjectId, FinObjectId) [FinMorphismId]
buildMorphismMap relation =
  Set.toAscList relation
    & map (\(sourceId, targetId) -> ((objectId sourceId, objectId targetId), [pairGenerator sourceId targetId]))
    & Map.fromList

buildCompositionMap :: Set.Set (Int, Int) -> Map.Map (FinMorphismId, FinMorphismId) FinMorphismId
buildCompositionMap relation =
  let relationList = Set.toAscList relation
      compositionRows =
        relationList
          >>= ( \(sourceId, middleId) ->
                  relationList
                    & filter (\(middleId', _) -> middleId == middleId')
                    & map
                      (\(_, targetId) -> ((pairGenerator middleId targetId, pairGenerator sourceId middleId), pairGenerator sourceId targetId))
              )
   in Map.fromList compositionRows

singleMorphismId :: ComposableChain FinCat -> Maybe FinMorphismId
singleMorphismId chainValue =
  case chainMorphisms chainValue of
    [morphism] -> Just (finMorId morphism)
    _ -> Nothing

lookupSingleMorphismChain :: FinMorphismId -> [ComposableChain FinCat] -> Maybe (ComposableChain FinCat)
lookupSingleMorphismChain morphismId chains =
  find (\chainValue -> singleMorphismId chainValue == Just morphismId) chains

simplexFingerprint :: NerveSimplex FinCat -> (Natural, FinObjectId, [FinMorphismId])
simplexFingerprint simplexValue =
  let chainValue = nerveSimplexChain simplexValue
   in ( nerveSimplexDimension simplexValue,
        finObjId (chainStartObject chainValue),
        map finMorId (chainMorphisms chainValue)
      )

sameSimplex :: Maybe (NerveSimplex FinCat) -> Maybe (NerveSimplex FinCat) -> Bool
sameSimplex leftSimplex rightSimplex =
  case (leftSimplex, rightSimplex) of
    (Nothing, Nothing) -> True
    (Just leftValue, Just rightValue) -> simplexFingerprint leftValue == simplexFingerprint rightValue
    _ -> False

innerHornFillMatchesSimplex :: GeneratedFiniteCategory -> Bool
innerHornFillMatchesSimplex generatedValue =
  let simplicialSet = normalizedNerve (generatedCategory generatedValue) (generatedTruncation generatedValue)
   in and
        [ case mkHorn nerveSimplexDimension (applyFaceAtDimension simplicialSet) simplexDimension missingFace faceEntries of
            Left _ -> False
            Right hornValue ->
              case mkInnerHorn hornValue of
                Left _ -> False
                Right innerHornValue ->
                  sameSimplex
                    (fillNerveInnerHorn (generatedCategory generatedValue) innerHornValue)
                    (Just simplexValue)
          | simplexDimension <- [2 .. generatedTruncation generatedValue],
            simplexValue <- simplicesAtDimension simplicialSet simplexDimension,
            missingFace <- [1 .. simplexDimension - 1],
            let faceEntries =
                  [ (faceIndex, faceSimplex)
                    | faceIndex <- [0 .. simplexDimension],
                      faceIndex /= missingFace,
                      faceSimplex <- maybe [] pure (applyFaceAtDimension simplicialSet simplexDimension faceIndex simplexValue)
                  ],
            fromIntegral (length faceEntries) == simplexDimension
        ]

lawfulCarrierSpec :: LawSuite
lawfulCarrierSpec =
  mkLawfulCarrierSpec
    "normalized nerve"
    LawSuiteConfig
      { lawSuiteName = "normalized nerve simplicial laws",
        lawSuiteMaxSuccess = 300,
        lawSuiteCarrierToSSet = \generatedValue -> normalizedNerve (generatedCategory generatedValue) (generatedTruncation generatedValue),
        lawSuiteEquality = sameSimplex,
        lawSuiteRenderSimplex = show . simplexFingerprint
      }

carrierTests :: TestTree
carrierTests =
  testGroup
    "Nerve"
    [ testCase "unnormalized and normalized carriers separate degenerate simplices" $ do
        let generatedSet = unnormalizedNerve sampleFinCat 1
            simplicialSet = normalizedNerve sampleFinCat 1
        assertEqual "unnormalized 0-simplices" 3 (length (generatedSimplicesAtDimension generatedSet 0))
        assertEqual "unnormalized 1-simplices" 6 (length (generatedSimplicesAtDimension generatedSet 1))
        assertEqual "normalized 0-simplices" 3 (length (simplicesAtDimension simplicialSet 0))
        assertEqual "normalized 1-simplices" 3 (length (simplicesAtDimension simplicialSet 1)),
      testCase "normalizing the unnormalized nerve agrees with the normalized nerve" $ do
        let normalizedSet = normalizedNerve sampleFinCat 2
            normalizedFromUnnormalized = normalizeGeneratedSSet (unnormalizedNerve sampleFinCat 2)
            fingerprintRows simplicialSet =
              fmap
                (Set.fromList . fmap simplexFingerprint . simplicesAtDimension simplicialSet)
                [0 .. 2]
        assertEqual
          "normalized simplex rows"
          (fingerprintRows normalizedSet)
          (fingerprintRows normalizedFromUnnormalized),
      testCase "inner horn filler reconstructs a composable 2-simplex" $ do
        let oneChains = chainsOfDimension sampleFinCat 1
            maybeFirst = lookupSingleMorphismChain (generatorMorphismId 10) oneChains
            maybeSecond = lookupSingleMorphismChain (generatorMorphismId 11) oneChains
        case (maybeFirst, maybeSecond) of
          (Just firstChain, Just secondChain) ->
            let simplicialSet = normalizedNerve sampleFinCat 2
                hornEntries = [(2, nerveSimplexFromChain firstChain), (0, nerveSimplexFromChain secondChain)]
             in case mkHorn nerveSimplexDimension (applyFaceAtDimension simplicialSet) 2 1 hornEntries of
                  Left _ -> assertBool "expected a compatible inner horn" False
                  Right hornValue ->
                    case mkInnerHorn hornValue of
                      Left _ -> assertBool "expected an inner horn" False
                      Right innerHornValue ->
                        case fillNerveInnerHorn sampleFinCat innerHornValue of
                          Nothing -> assertBool "expected horn filler for nerve of sample category" False
                          Just simplexValue -> do
                            assertEqual "filled simplex dimension" 2 (nerveSimplexDimension simplexValue)
                            assertEqual "filled simplex chain length" 2 (length (chainMorphisms (nerveSimplexChain simplexValue)))
          _ -> assertBool "expected generator chains in dimension 1" False,
      QC.testProperty "inner horns reconstruct original simplex in normalized nerves" $
        QC.withNumTests 200 innerHornFillMatchesSimplex
    ]
