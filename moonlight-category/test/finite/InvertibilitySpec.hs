module InvertibilitySpec
  ( tests,
  )
where

import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind (Type)
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Sum (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Category.Effect.Fixture.FinCat (sampleFinCat)
import Moonlight.Category
  ( FinCat,
    FinCatError (..),
    FinCatValidationError (..),
    FinGeneratorId (..),
    FinMor,
    FinMorphismId (..),
    FinObjectId (..),
    allMorphisms,
    allMorphismsFrom,
    allObjects,
    automorphismGroupAt,
    automorphismGroupoid,
    automorphismGroupoidFromIndex,
    automorphismGroupoidObjects,
    composeMor,
    coreGroupoid,
    coreGroupoidFromIndex,
    coreGroupoidMorphisms,
    coreGroupoidMorphismsBetween,
    coreGroupoidObjects,
    finCatExplicitMorphismMapView,
    finCatMorphismCountFrom,
    finCatMorphismCountTo,
    finObjId,
    finMorId,
    finMorSourceId,
    finMorTargetId,
    foldMapFinMorphismsFrom,
    foldMapFinMorphismsTo,
    forgetAutomorphismGroupoidMorphism,
    identity,
    invertibilityIndex,
    mkFinCat,
    mkFinMorphism,
    mkFinObject,
    mkFinTwoMor,
    source,
    target,
    vCompose,
  )
import Moonlight.Category.Notation (cod, composeIn, dom, hom)
import Moonlight.Category.Presentation
  ( FinCatBuildError,
    below,
    finCategory,
    objects,
  )
import Moonlight.Pale.Test.Assertions (withResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

type GeneratedComponent :: Type
data GeneratedComponent
  = ThinChainComponent Int
  | PairGroupoidComponent Int
  deriving stock (Eq, Show)

type GeneratedFiniteCategory :: Type
newtype GeneratedFiniteCategory = GeneratedFiniteCategory
  { unGeneratedFiniteCategory :: FinCat
  }
  deriving stock (Eq, Show)

type ComponentData :: Type
data ComponentData = ComponentData
  { cdObjects :: Set.Set FinObjectId,
    cdMorphisms :: Map.Map (FinObjectId, FinObjectId) [FinMorphismId],
    cdComposition :: Map.Map (FinMorphismId, FinMorphismId) FinMorphismId
  }

objectId :: Int -> FinObjectId
objectId = FinObjectId

generatorMorphismId :: Int -> FinMorphismId
generatorMorphismId = FinGeneratorMorphismId . FinGeneratorId

identityMorphismId :: Int -> FinMorphismId
identityMorphismId = FinIdentityId . objectId

instance QC.Arbitrary GeneratedComponent where
  arbitrary =
    QC.oneof
      [ ThinChainComponent <$> QC.chooseInt (1, 4),
        PairGroupoidComponent <$> QC.chooseInt (1, 4)
      ]

  shrink componentValue =
    case componentValue of
      ThinChainComponent sizeValue ->
        QC.shrink sizeValue
          >>= (\nextSize -> if nextSize >= 1 then [ThinChainComponent nextSize] else [])
      PairGroupoidComponent sizeValue ->
        QC.shrink sizeValue
          >>= (\nextSize -> if nextSize >= 1 then [PairGroupoidComponent nextSize] else [])

instance QC.Arbitrary GeneratedFiniteCategory where
  arbitrary =
    QC.suchThatMap
      (QC.chooseInt (1, 3) >>= (\componentCount -> QC.vectorOf componentCount QC.arbitrary))
      (buildGeneratedFiniteCategory >=> (Just . GeneratedFiniteCategory))

  shrink _ = []

buildGeneratedFiniteCategory :: [GeneratedComponent] -> Maybe FinCat
buildGeneratedFiniteCategory components =
  let (_, builtComponents) = mapAccumL buildComponent (0, 0) components
      componentObjects =
        builtComponents
          & foldMap cdObjects
      allMorphismBuckets =
        builtComponents
          & fmap cdMorphisms
          & Map.unions
      allCompositions =
        builtComponents
          & fmap cdComposition
          & Map.unions
   in either (const Nothing) Just (mkFinCat componentObjects allMorphismBuckets allCompositions)

componentCategory :: ComponentData -> Either (NonEmpty FinCatValidationError) FinCat
componentCategory component =
  mkFinCat (cdObjects component) (cdMorphisms component) (cdComposition component)

thinChainCategory :: Int -> Either (NonEmpty FinCatValidationError) FinCat
thinChainCategory sizeValue =
  componentCategory (snd (buildThinChainComponent 0 100 sizeValue))

pairGroupoidCategory :: Int -> Either (NonEmpty FinCatValidationError) FinCat
pairGroupoidCategory sizeValue =
  componentCategory (snd (buildPairGroupoidComponent 0 100 sizeValue))

buildComponent :: (Int, Int) -> GeneratedComponent -> ((Int, Int), ComponentData)
buildComponent (nextObjectId, nextGeneratorId) componentValue =
  case componentValue of
    ThinChainComponent sizeValue ->
      buildThinChainComponent nextObjectId nextGeneratorId sizeValue
    PairGroupoidComponent sizeValue ->
      buildPairGroupoidComponent nextObjectId nextGeneratorId sizeValue

buildThinChainComponent :: Int -> Int -> Int -> ((Int, Int), ComponentData)
buildThinChainComponent nextObjectId nextGeneratorId sizeValue =
  let rawObjectIds = take sizeValue [nextObjectId ..]
      generatorPairs =
        rawObjectIds
          >>= (\sourceObject ->
                 rawObjectIds
                   >>= (\targetObject ->
                          if sourceObject < targetObject
                            then [(sourceObject, targetObject)]
                            else []
                      )
              )
      pairToGenerator =
        zip generatorPairs [nextGeneratorId ..]
          & fmap (\(objectPair, generatorKey) -> (objectPair, generatorMorphismId generatorKey))
          & Map.fromList
      compositionMap =
        rawObjectIds
          >>= (\sourceObject ->
                 rawObjectIds
                   >>= (\middleObject ->
                          rawObjectIds
                            >>= (\targetObject ->
                                   if sourceObject < middleObject && middleObject < targetObject
                                     then compositionEntry pairToGenerator sourceObject middleObject targetObject
                                     else []
                               )
                       )
              )
          & Map.fromList
      nextState = (nextObjectId + sizeValue, nextGeneratorId + length generatorPairs)
   in (nextState, componentData rawObjectIds pairToGenerator compositionMap)

buildPairGroupoidComponent :: Int -> Int -> Int -> ((Int, Int), ComponentData)
buildPairGroupoidComponent nextObjectId nextGeneratorId sizeValue =
  let rawObjectIds = take sizeValue [nextObjectId ..]
      generatorPairs =
        rawObjectIds
          >>= (\sourceObject ->
                 rawObjectIds
                   >>= (\targetObject ->
                          if sourceObject /= targetObject
                            then [(sourceObject, targetObject)]
                            else []
                      )
              )
      pairToGenerator =
        zip generatorPairs [nextGeneratorId ..]
          & fmap (\(objectPair, generatorKey) -> (objectPair, generatorMorphismId generatorKey))
          & Map.fromList
      compositionMap =
        rawObjectIds
          >>= (\sourceObject ->
                 rawObjectIds
                   >>= (\middleObject ->
                          rawObjectIds
                            >>= (\targetObject ->
                                   if sourceObject /= middleObject && middleObject /= targetObject
                                     then groupoidCompositionEntry pairToGenerator sourceObject middleObject targetObject
                                     else []
                               )
                       )
              )
          & Map.fromList
      nextState = (nextObjectId + sizeValue, nextGeneratorId + length generatorPairs)
   in (nextState, componentData rawObjectIds pairToGenerator compositionMap)

componentData :: [Int] -> Map.Map (Int, Int) FinMorphismId -> Map.Map (FinMorphismId, FinMorphismId) FinMorphismId -> ComponentData
componentData rawObjectIds pairToGenerator compositionMap =
  ComponentData
    { cdObjects = Set.fromList (fmap objectId rawObjectIds),
      cdMorphisms =
        pairToGenerator
          & Map.toList
          & fmap (\((sourceObject, targetObject), generatorId) -> ((objectId sourceObject, objectId targetObject), [generatorId]))
          & Map.fromList,
      cdComposition = compositionMap
    }

compositionEntry :: Map.Map (Int, Int) FinMorphismId -> Int -> Int -> Int -> [((FinMorphismId, FinMorphismId), FinMorphismId)]
compositionEntry pairToGenerator sourceObject middleObject targetObject =
  case
    ( Map.lookup (middleObject, targetObject) pairToGenerator,
      Map.lookup (sourceObject, middleObject) pairToGenerator,
      Map.lookup (sourceObject, targetObject) pairToGenerator
    )
    of
      (Just leftGeneratorId, Just rightGeneratorId, Just composedGeneratorId) ->
        [((leftGeneratorId, rightGeneratorId), composedGeneratorId)]
      _ -> []

groupoidCompositionEntry :: Map.Map (Int, Int) FinMorphismId -> Int -> Int -> Int -> [((FinMorphismId, FinMorphismId), FinMorphismId)]
groupoidCompositionEntry pairToGenerator sourceObject middleObject targetObject =
  case
    ( Map.lookup (middleObject, targetObject) pairToGenerator,
      Map.lookup (sourceObject, middleObject) pairToGenerator,
      if sourceObject == targetObject
        then Just (identityMorphismId sourceObject)
        else Map.lookup (sourceObject, targetObject) pairToGenerator
    )
    of
      (Just leftGeneratorId, Just rightGeneratorId, Just composedGeneratorId) ->
        [((leftGeneratorId, rightGeneratorId), composedGeneratorId)]
      _ -> []

allPairs :: [a] -> [(a, a)]
allPairs values =
  values
    >>= (\leftValue ->
           values
             >>= (\rightValue -> [(leftValue, rightValue)])
       )

allTriples :: [a] -> [(a, a, a)]
allTriples values =
  values
    >>= (\firstValue ->
           values
             >>= (\secondValue ->
                    values
                      >>= (\thirdValue -> [(firstValue, secondValue, thirdValue)])
                )
       )

coreGroupoidIdentityClosureHolds :: GeneratedFiniteCategory -> Bool
coreGroupoidIdentityClosureHolds (GeneratedFiniteCategory categoryValue) =
  let groupoidValue = coreGroupoid categoryValue
   in coreGroupoidObjects groupoidValue
        & all
          ( \objectValue ->
              case identity groupoidValue objectValue of
                Right identityMorphism ->
                  identityMorphism `elem` coreGroupoidMorphismsBetween groupoidValue objectValue objectValue
                Left _ -> False
          )

coreGroupoidCompositionClosureHolds :: GeneratedFiniteCategory -> Bool
coreGroupoidCompositionClosureHolds (GeneratedFiniteCategory categoryValue) =
  let groupoidValue = coreGroupoid categoryValue
      morphismPairs =
        allPairs (coreGroupoidMorphisms groupoidValue)
          & filter (\(leftMorphism, rightMorphism) -> target groupoidValue rightMorphism == source groupoidValue leftMorphism)
   in morphismPairs
        & all
          ( \(leftMorphism, rightMorphism) ->
              case composeMor groupoidValue leftMorphism rightMorphism of
                Right composedMorphism ->
                  case (source groupoidValue rightMorphism, target groupoidValue leftMorphism) of
                    (Right sourceObject, Right targetObject) ->
                      composedMorphism
                        `elem` coreGroupoidMorphismsBetween
                          groupoidValue
                          sourceObject
                          targetObject
                    _ -> False
                Left _ -> False
          )

coreGroupoidAssociativityHolds :: GeneratedFiniteCategory -> Bool
coreGroupoidAssociativityHolds (GeneratedFiniteCategory categoryValue) =
  let groupoidValue = coreGroupoid categoryValue
      morphismTriples =
        allTriples (coreGroupoidMorphisms groupoidValue)
          & filter
            ( \(leftMorphism, middleMorphism, rightMorphism) ->
                target groupoidValue rightMorphism == source groupoidValue middleMorphism
                  && target groupoidValue middleMorphism == source groupoidValue leftMorphism
            )
   in morphismTriples
        & all
          ( \(leftMorphism, middleMorphism, rightMorphism) ->
              let leftAssociated =
                    composeMor groupoidValue middleMorphism rightMorphism
                      >>= composeMor groupoidValue leftMorphism
                  rightAssociated =
                    composeMor groupoidValue leftMorphism middleMorphism
                      >>= (\composedMorphism -> composeMor groupoidValue composedMorphism rightMorphism)
               in leftAssociated == rightAssociated
          )

isomorphicPairCategory :: FinCat
isomorphicPairCategory =
  case
    mkFinCat
      (Set.fromList [objectId 0, objectId 1])
      ( Map.fromList
          [ ((objectId 0, objectId 1), [generatorMorphismId 10]),
            ((objectId 1, objectId 0), [generatorMorphismId 11])
          ]
      )
      ( Map.fromList
          [ ((generatorMorphismId 11, generatorMorphismId 10), identityMorphismId 0),
            ((generatorMorphismId 10, generatorMorphismId 11), identityMorphismId 1)
          ]
      ) of
    Right categoryValue -> categoryValue
    Left _ -> sampleFinCat

parallelObjectIds :: Set.Set FinObjectId
parallelObjectIds =
  Set.fromList [objectId 0, objectId 1]

parallelSourceId :: FinObjectId
parallelSourceId = objectId 0

parallelTargetId :: FinObjectId
parallelTargetId = objectId 1

parallelFId :: FinMorphismId
parallelFId = generatorMorphismId 20

parallelGId :: FinMorphismId
parallelGId = generatorMorphismId 21

parallelGPrimeId :: FinMorphismId
parallelGPrimeId = generatorMorphismId 22

parallelHId :: FinMorphismId
parallelHId = generatorMorphismId 23

parallelCategory :: [FinMorphismId] -> Either (NonEmpty FinCatValidationError) FinCat
parallelCategory morphismIds =
  mkFinCat
    parallelObjectIds
    (Map.fromList [((parallelSourceId, parallelTargetId), morphismIds)])
    Map.empty

withParallelMorphisms :: (FinCat -> FinMor -> FinMor -> FinMor -> FinMor -> Assertion) -> Assertion
withParallelMorphisms assertion =
  withResult (parallelCategory [parallelFId, parallelGId, parallelGPrimeId, parallelHId]) $ \backgroundCategory ->
    withResult (traverse (mkFinMorphism backgroundCategory) [parallelFId, parallelGId, parallelGPrimeId, parallelHId]) $ \backgroundMorphisms ->
      case backgroundMorphisms of
        [f, g, gPrime, h] -> assertion backgroundCategory f g gPrime h
        _ -> assertFailure "expected four parallel morphisms from the finite-category fixture"

canonicalHomBucketOrderIsIdentityInvariant :: Assertion
canonicalHomBucketOrderIsIdentityInvariant =
  let firstPresentation = parallelCategory [parallelGId, parallelFId]
      secondPresentation = parallelCategory [parallelFId, parallelGId]
      expectedMorphismOrder = [identityMorphismId 0, identityMorphismId 1, parallelFId, parallelGId]
   in withResult firstPresentation $ \firstCategory ->
        withResult secondPresentation $ \restatedCategory -> do
          assertEqual "hom-bucket order must not change category identity" firstCategory restatedCategory
          assertEqual "morphism enumeration follows canonical category identity" expectedMorphismOrder (fmap finMorId (allMorphisms firstCategory))
          assertEqual "restated morphism enumeration follows the same canonical order" expectedMorphismOrder (fmap finMorId (allMorphisms restatedCategory))

explicitEmptyBucketIsPresentationNoise :: Assertion
explicitEmptyBucketIsPresentationNoise =
  let objectIds = Set.fromList [objectId 0, objectId 1]
      omittedBucket = mkFinCat objectIds Map.empty Map.empty
      explicitBucket = mkFinCat objectIds (Map.fromList [((objectId 0, objectId 1), [])]) Map.empty
   in withResult omittedBucket $ \firstCategory ->
        withResult explicitBucket $ \restatedCategory -> do
          assertEqual "valid empty hom-buckets canonicalize away" firstCategory restatedCategory
          assertEqual "stored category presentation prunes empty buckets" Map.empty (finCatExplicitMorphismMapView restatedCategory)

invalidEmptyBucketEndpointIsRejected :: Assertion
invalidEmptyBucketEndpointIsRejected =
  case mkFinCat (Set.singleton (objectId 0)) (Map.fromList [((objectId 0, objectId 1), [])]) Map.empty of
    Left failures ->
      assertBool
        "endpoint validation still sees invalid empty buckets"
        (MorphismEndpointOutsideObjects (objectId 0) (objectId 1) `elem` failures)
    Right _ -> assertFailure "expected invalid empty bucket endpoint to be rejected"

identityCompositionTableKeysAreRejected :: Assertion
identityCompositionTableKeysAreRejected =
  let morphismId = generatorMorphismId 30
      objectIds = Set.fromList [objectId 0, objectId 1]
      morphismMap = Map.fromList [((objectId 0, objectId 1), [morphismId])]
      compositionMap = Map.fromList [((identityMorphismId 1, morphismId), morphismId)]
   in case mkFinCat objectIds morphismMap compositionMap of
        Left failures ->
          assertBool
            "identity-keyed composition entries are dead table surface and must be rejected"
            (CompositionTableUsesIdentityKey (identityMorphismId 1) morphismId `elem` failures)
        Right _ -> assertFailure "expected identity-keyed composition entry to be rejected"

verticalCompositionRejectsNonSharedMiddleEdge :: Assertion
verticalCompositionRejectsNonSharedMiddleEdge =
  withParallelMorphisms $ \backgroundCategory f g gPrime h ->
    withResult (mkFinTwoMor f gPrime) $ \rightCell ->
      withResult (mkFinTwoMor g h) $ \topCell ->
        assertEqual
          "vertical composition requires equal shared 1-cell, not merely parallel object boundaries"
          (Left (FinCatTwoMorphismNotVerticallyComposable topCell rightCell))
          (vCompose backgroundCategory topCell rightCell)

verticalCompositionAcceptsSharedMiddleEdge :: Assertion
verticalCompositionAcceptsSharedMiddleEdge =
  withParallelMorphisms $ \backgroundCategory f g _ h ->
    withResult (mkFinTwoMor f g) $ \rightCell ->
      withResult (mkFinTwoMor g h) $ \topCell ->
        withResult (mkFinTwoMor f h) $ \resultCell ->
          assertEqual
            "vertical composition glues along the exact shared 1-cell"
            (Right resultCell)
            (vCompose backgroundCategory topCell rightCell)

wrongCategoryTwoCellBoundaryIsRejectedBeforeVerticalGluing :: Assertion
wrongCategoryTwoCellBoundaryIsRejectedBeforeVerticalGluing =
  withParallelMorphisms $ \backgroundCategory f g _ _ ->
    withResult (mkFinTwoMor f g) $ \rightCell ->
      withResult (parallelCategory [parallelGId, parallelHId]) $ \foreignCategory ->
        withResult (traverse (mkFinMorphism foreignCategory) [parallelGId, parallelHId]) $ \foreignMorphisms ->
          case foreignMorphisms of
            [foreignG, foreignH] ->
              withResult (mkFinTwoMor foreignG foreignH) $ \foreignTopCell ->
                case vCompose backgroundCategory foreignTopCell rightCell of
                  Left (FinCatMorphismWrongCategory _ _ wrongMorphismId) ->
                    assertEqual "wrong-category source boundary is rejected first" (finMorId foreignG) wrongMorphismId
                  Left otherFailure -> assertFailure ("expected wrong-category obstruction, got " <> show otherFailure)
                  Right _ -> assertFailure "expected wrong-category 2-cell boundary to be rejected"
            _ -> assertFailure "expected two foreign parallel morphisms"

assertSourceBucketsMatchEnumeration :: FinCat -> Assertion
assertSourceBucketsMatchEnumeration categoryValue =
  traverse_
    ( \objectValue ->
        assertEqual
          "source-bucket query must be the source-filtered carrier enumeration"
          (filter ((== finObjId objectValue) . finMorSourceId) (allMorphisms categoryValue))
          (allMorphismsFrom categoryValue objectValue)
    )
    (allObjects categoryValue)

explicitSourceBucketsMatchEnumeration :: Assertion
explicitSourceBucketsMatchEnumeration =
  withResult (parallelCategory [parallelFId, parallelGId, parallelGPrimeId, parallelHId]) assertSourceBucketsMatchEnumeration

thinSourceBucketsMatchEnumeration :: Assertion
thinSourceBucketsMatchEnumeration =
  assertSourceBucketsMatchEnumeration sampleFinCat

linearFinPresentation :: Int -> Either FinCatBuildError FinCat
linearFinPresentation objectCount =
  finCategory $ do
    declaredObjects <- objects (fmap (\index -> "x" <> show index) [0 .. objectCount - 1])
    traverse_ (uncurry below) (zip declaredObjects (drop 1 declaredObjects))

denseEndpointLookupMatchesMorphismConstruction :: Assertion
denseEndpointLookupMatchesMorphismConstruction =
  withResult (linearFinPresentation 4) $ \categoryValue ->
    case hom categoryValue (objectId 0) (objectId 3) of
      Nothing -> assertFailure "expected dense endpoint morphism from 0 to 3"
      Just morphism -> do
        assertEqual "dense endpoint lookup returns a morphism with source 0" (objectId 0) (dom morphism)
        assertEqual "dense endpoint lookup returns a morphism with target 3" (objectId 3) (cod morphism)

residentSourceAndTargetCountsAgreeWithFolds :: Assertion
residentSourceAndTargetCountsAgreeWithFolds =
  withResult (linearFinPresentation 5) $ \categoryValue -> do
    withResult (mkFinObject categoryValue (objectId 0)) $ \sourceObject ->
      assertEqual
        "resident source count agrees with the derived source enumeration"
        (length (allMorphismsFrom categoryValue sourceObject))
        (finCatMorphismCountFrom categoryValue (objectId 0))
    assertEqual
      "resident source count agrees with the resident source fold"
      (getSum (foldMapFinMorphismsFrom (const (Sum (1 :: Int))) categoryValue (objectId 0)))
      (finCatMorphismCountFrom categoryValue (objectId 0))
    assertEqual
      "resident target count agrees with source-filtered full enumeration"
      (length (filter ((== objectId 4) . finMorTargetId) (allMorphisms categoryValue)))
      (finCatMorphismCountTo categoryValue (objectId 4))
    assertEqual
      "resident target count agrees with the resident target fold"
      (getSum (foldMapFinMorphismsTo (const (Sum (1 :: Int))) categoryValue (objectId 4)))
      (finCatMorphismCountTo categoryValue (objectId 4))

denseThinCompositionIsEndpointComposition :: Assertion
denseThinCompositionIsEndpointComposition =
  withResult (linearFinPresentation 4) $ \categoryValue ->
    case
      ( hom categoryValue (objectId 1) (objectId 3),
        hom categoryValue (objectId 0) (objectId 1),
        hom categoryValue (objectId 0) (objectId 3)
      ) of
      (Just leftMorphism, Just rightMorphism, Just expectedMorphism) ->
        withResult (composeIn categoryValue leftMorphism rightMorphism) $ \composedMorphism ->
          assertEqual "dense thin composition is endpoint composition" (finMorId expectedMorphism) (finMorId composedMorphism)
      _ -> assertFailure "expected dense endpoint morphisms for 1->3, 0->1, and 0->3"

mkFinCatThinTotalOrderComposesByEndpoint :: Assertion
mkFinCatThinTotalOrderComposesByEndpoint =
  withResult (thinChainCategory 4) $ \categoryValue ->
    case
      ( hom categoryValue (objectId 1) (objectId 3),
        hom categoryValue (objectId 0) (objectId 1),
        hom categoryValue (objectId 0) (objectId 3)
      ) of
      (Just leftMorphism, Just rightMorphism, Just expectedMorphism) ->
        withResult (composeIn categoryValue leftMorphism rightMorphism) $ \composedMorphism ->
          assertEqual "checked thin total order composes by endpoint" (finMorId expectedMorphism) (finMorId composedMorphism)
      _ -> assertFailure "expected checked thin endpoint morphisms for 1->3, 0->1, and 0->3"

mkFinCatPairGroupoidComposesInversesToIdentities :: Assertion
mkFinCatPairGroupoidComposesInversesToIdentities =
  withResult (pairGroupoidCategory 3) $ \categoryValue ->
    case
      ( hom categoryValue (objectId 0) (objectId 1),
        hom categoryValue (objectId 1) (objectId 0)
      ) of
      (Just forward, Just backward) -> do
        withResult (composeIn categoryValue backward forward) $ \leftIdentity ->
          assertEqual "backward after forward is the source identity" (identityMorphismId 0) (finMorId leftIdentity)
        withResult (composeIn categoryValue forward backward) $ \rightIdentity ->
          assertEqual "forward after backward is the target identity" (identityMorphismId 1) (finMorId rightIdentity)
      _ -> assertFailure "expected inverse endpoint morphisms between 0 and 1"

thinShapeMissingCompositionIsRejected :: Assertion
thinShapeMissingCompositionIsRejected =
  let component = snd (buildThinChainComponent 0 100 3)
   in case Map.toAscList (cdComposition component) of
        [] -> assertFailure "expected a thin-chain composition entry"
        (((leftMorphism, rightMorphism), _) : _) ->
          case mkFinCat (cdObjects component) (cdMorphisms component) (Map.delete (leftMorphism, rightMorphism) (cdComposition component)) of
            Left failures ->
              assertBool
                "checked thin validation reports the missing composite"
                (MissingCompositionForPair leftMorphism rightMorphism `elem` failures)
            Right _ -> assertFailure "expected missing thin composition to be rejected"

thinShapeWrongEndpointCompositionIsRejected :: Assertion
thinShapeWrongEndpointCompositionIsRejected =
  let component = snd (buildThinChainComponent 0 100 3)
      maybeWrongResult =
        case Map.lookup (objectId 0, objectId 1) (cdMorphisms component) of
          Just [morphismId] -> Just morphismId
          _ -> Nothing
   in case (Map.toAscList (cdComposition component), maybeWrongResult) of
        (((leftMorphism, rightMorphism), _) : _, Just wrongResult) ->
          case mkFinCat (cdObjects component) (cdMorphisms component) (Map.insert (leftMorphism, rightMorphism) wrongResult (cdComposition component)) of
            Left failures ->
              assertBool
                "basic composition validation reports the wrong result endpoint"
                (CompositionResultEndpointMismatch leftMorphism rightMorphism wrongResult `elem` failures)
            Right _ -> assertFailure "expected wrong-endpoint thin composition to be rejected"
        _ -> assertFailure "expected a thin-chain composition entry and wrong-result morphism"

tests :: TestTree
tests =
  testGroup
    "Invertibility"
    [ testCase "FinCat canonicalizes hom-bucket order into category identity" canonicalHomBucketOrderIsIdentityInvariant,
      testCase "FinCat treats valid explicit empty buckets as presentation noise" explicitEmptyBucketIsPresentationNoise,
      testCase "FinCat rejects invalid empty bucket endpoints" invalidEmptyBucketEndpointIsRejected,
      testCase "FinCat rejects identity-keyed composition table entries" identityCompositionTableKeysAreRejected,
      testCase "FinCat vertical composition rejects non-shared middle 1-cells" verticalCompositionRejectsNonSharedMiddleEdge,
      testCase "FinCat vertical composition accepts exact shared middle 1-cells" verticalCompositionAcceptsSharedMiddleEdge,
      testCase "FinCat vertical composition rejects wrong-category boundaries before gluing" wrongCategoryTwoCellBoundaryIsRejectedBeforeVerticalGluing,
      testCase "FinCat explicit source buckets agree with carrier enumeration" explicitSourceBucketsMatchEnumeration,
      testCase "FinCat thin source buckets agree with carrier enumeration" thinSourceBucketsMatchEnumeration,
      testCase "FinCat dense endpoint lookup agrees with morphism construction" denseEndpointLookupMatchesMorphismConstruction,
      testCase "FinCat dense resident incident counts agree with folds" residentSourceAndTargetCountsAgreeWithFolds,
      testCase "FinCat dense thin composition is endpoint composition" denseThinCompositionIsEndpointComposition,
      testCase "mkFinCat checked thin total order composes by endpoint" mkFinCatThinTotalOrderComposesByEndpoint,
      testCase "mkFinCat checked pair groupoid composes inverses to identities" mkFinCatPairGroupoidComposesInversesToIdentities,
      testCase "mkFinCat checked thin validation rejects missing composition" thinShapeMissingCompositionIsRejected,
      testCase "mkFinCat checked thin validation rejects wrong endpoint composition" thinShapeWrongEndpointCompositionIsRejected,
      testCase "core groupoid from index matches direct core groupoid" $
        let indexValue = invertibilityIndex isomorphicPairCategory
            indexedGroupoid = coreGroupoidFromIndex isomorphicPairCategory indexValue
            directGroupoid = coreGroupoid isomorphicPairCategory
         in assertEqual
              "indexed and direct core groupoid morphisms should agree"
              (coreGroupoidMorphisms directGroupoid)
              (coreGroupoidMorphisms indexedGroupoid),
      testCase "core groupoid carries the whole invertible surface" $
        let groupoidValue = coreGroupoid isomorphicPairCategory
            bucketedMorphisms =
              coreGroupoidObjects groupoidValue
                >>= (\sourceObject ->
                       coreGroupoidObjects groupoidValue
                         >>= (\targetObject ->
                                coreGroupoidMorphismsBetween groupoidValue sourceObject targetObject
                            )
                    )
         in assertEqual
              "core groupoid morphisms should agree with their endpoint decomposition"
              (coreGroupoidMorphisms groupoidValue)
              bucketedMorphisms,
      testCase "automorphism groupoid from index matches direct automorphism groupoid" $
        let indexValue = invertibilityIndex sampleFinCat
            indexedGroupoid = automorphismGroupoidFromIndex sampleFinCat indexValue
            directGroupoid = automorphismGroupoid sampleFinCat
         in assertEqual
              "indexed and direct automorphism groupoid objects should agree"
              (automorphismGroupoidObjects directGroupoid)
              (automorphismGroupoidObjects indexedGroupoid),
      testCase "core groupoid endpoint query isolates the directed isomorphism bucket" $
        case coreGroupoidObjects (coreGroupoid isomorphicPairCategory) of
          sourceObject : targetObject : _ ->
            let groupoidValue = coreGroupoid isomorphicPairCategory
             in assertEqual
                  "expected exactly one forward invertible morphism in the core groupoid"
                  1
                  (length (coreGroupoidMorphismsBetween groupoidValue sourceObject targetObject))
          _ -> assertBool "expected two objects in isomorphic pair category" False,
      testCase "automorphism groupoid isolates object-local invertibles" $
        case automorphismGroupoidObjects (automorphismGroupoid sampleFinCat) of
          baseObject : _ ->
            let automorphismGroupoidValue = automorphismGroupoid sampleFinCat
             in assertEqual
                  "expected exactly one object-local automorphism in the automorphism groupoid"
                  1
                  (length (fmap forgetAutomorphismGroupoidMorphism (automorphismGroupAt automorphismGroupoidValue baseObject)))
          [] -> assertBool "expected sample category to have at least one object" False,
      QC.testProperty "core groupoid contains identities for generated finite categories" $
        QC.withNumTests 100 coreGroupoidIdentityClosureHolds,
      QC.testProperty "core groupoid is closed under composition for generated finite categories" $
        QC.withNumTests 100 coreGroupoidCompositionClosureHolds,
      QC.testProperty "core groupoid composition is associative for generated finite categories" $
        QC.withNumTests 100 coreGroupoidAssociativityHolds
    ]
