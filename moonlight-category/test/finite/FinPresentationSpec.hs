module FinPresentationSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Category
  ( FinCatValidationError (..),
    FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    composeMor,
    finCatNonIdentityMorphismCount,
    objectCount,
    finMorId,
    mkFinCat,
    mkFinObject,
  )
import Moonlight.Category.Pure.FinCat
  ( trustedFinCatWithGeneratorBasis,
  )
import Moonlight.Category.Pure.Thin
  ( composeThinMorphismBy,
    mkThinMorphismBy,
    thinMorphismSource,
    thinMorphismTarget,
  )
import Moonlight.Category.Pure.Poset
  ( PosetCat (..),
    mkPosetMor,
    posetSource,
    posetTarget,
  )
import Moonlight.Category.Notation
  ( composeIn,
    hom,
    idOf,
    reachableIn,
  )
import Moonlight.Category.Presentation
  ( FinCat,
    FinBuilder,
    FinCatBuildError (..),
    after,
    arrow,
    below,
    equate,
    finCategory,
    identityAt,
    object,
    objects,
  )
import Moonlight.Pale.Test.Assertions (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck qualified as QC

data SmallEndomorphismTable = SmallEndomorphismTable
  { smallEndomorphismMorphisms :: [FinMorphismId],
    smallEndomorphismComposition :: Map (FinMorphismId, FinMorphismId) FinMorphismId
  }
  deriving stock (Show)

instance QC.Arbitrary SmallEndomorphismTable where
  arbitrary = do
    morphismCount <- QC.chooseInt (0, 4)
    let morphismIds =
          FinGeneratorMorphismId . FinGeneratorId <$> [0 .. morphismCount - 1]
        resultIds =
          FinIdentityId unitObjectId : morphismIds
        composablePairs =
          morphismIds >>= (\right -> fmap (\left -> (left, right)) morphismIds)
    compositionKeys <- QC.sublistOf composablePairs
    compositionResults <- traverse (const (QC.elements resultIds)) compositionKeys
    pure (SmallEndomorphismTable morphismIds (Map.fromList (zip compositionKeys compositionResults)))

  shrink _ =
    []

tests :: TestTree
tests =
  testGroup
    "FinPresentation"
    [ testCase
        "a strict-order presentation compiles to the dense representation"
        testPosetChainIsDense,
      testCase
        "dense thin composition returns the morphism for the composite endpoints"
        testDenseThinCompositionUsesCompositeEndpointId,
      testCase
        "mkFinCat rejects dense thin composition results with wrong endpoints"
        testDenseThinCompositionResultEndpointMismatchRejected,
      testCase
        "thin morphism smart constructors reject invalid relations and compose defensively"
        testThinMorphismSmartConstructorsAndDefensiveCompose,
      testCase
        "poset smart constructors compose through the Category instance"
        testPosetSmartConstructorsCompose,
      testCase
        "a fully enumerated presentation realises its composite"
        testTriangleIsThinWithComposite,
      testCase
        "a longer path is lowered after its intermediate composites resolve"
        testLongPathEquation,
      testCase
        "identity equations present a two-object groupoid"
        testIdentityEquations,
      testCase
        "identity equations must agree with the unit laws"
        testIdentityEquationMismatchRejected,
      testCase
        "mixing strict-order and general modes is rejected"
        testMixedModesRejected,
      testCase
        "a cyclic strict-order presentation is rejected"
        testCyclicStrictOrderRejected,
      testCase
        "an incomplete general presentation is rejected by validation"
        testIncompletePresentationRejected,
      testCase
        "an unresolved proper subpath is reported"
        testUnresolvedCompositeRejected,
      testCase
        "a noncomposable path is rejected before FinCat validation"
        testNonComposablePathRejected,
      testCase
        "a nonparallel equation is rejected"
        testNonParallelEquationRejected,
      testCase
        "conflicting composition claims are rejected rather than overwritten"
        testConflictingCompositionRejected,
      testCase
        "an equality of distinct declared morphisms is not mistaken for a quotient"
        testUnsupportedEquationRejected,
      testCase
        "a reflexive atomic equation is harmless"
        testReflexiveEquationAccepted,
      testCase
        "a generator-backed non-thin presentation accepts an associative table"
        testGeneratorBackedAssociativePresentation,
      testCase
        "a generator-backed non-thin presentation rejects associativity violations"
        testGeneratorBackedAssociativityRejected,
      testCase
        "raw mkFinCat rejects associativity violations after generator reduction"
        testRawMkFinCatStillRejectsAssociativityViolation,
      QC.testProperty
        "mkFinCat generator validation agrees with exhaustive validation on small explicit tables"
        (QC.withNumTests 200 testMkFinCatMatchesExhaustiveValidator),
      testCase
        "mkFinCat catches a non-generator middle associativity violation"
        testNonGeneratorMiddleViolationRejected,
      testCase
        "a duplicate object name is rejected"
        testDuplicateObjectRejected
    ]

chainPoset :: Either FinCatBuildError FinCat
chainPoset =
  finCategory $ do
    [x, y, z] <- objects ["x", "y", "z"]
    below x y
    below y z

denseChainObjects :: Set FinObjectId
denseChainObjects =
  Set.fromList [FinObjectId 0, FinObjectId 1, FinObjectId 2]

denseChainMorphismMap :: Map (FinObjectId, FinObjectId) [FinMorphismId]
denseChainMorphismMap =
  Map.fromList
    [ ((FinObjectId 0, FinObjectId 1), [denseChain01]),
      ((FinObjectId 0, FinObjectId 2), [denseChain02]),
      ((FinObjectId 1, FinObjectId 2), [denseChain12])
    ]

denseChainComposition :: FinMorphismId -> Map (FinMorphismId, FinMorphismId) FinMorphismId
denseChainComposition compositeId =
  Map.singleton (denseChain12, denseChain01) compositeId

denseChain01 :: FinMorphismId
denseChain01 =
  FinGeneratorMorphismId (FinGeneratorId 0)

denseChain02 :: FinMorphismId
denseChain02 =
  FinGeneratorMorphismId (FinGeneratorId 1)

denseChain12 :: FinMorphismId
denseChain12 =
  FinGeneratorMorphismId (FinGeneratorId 2)

testDenseThinCompositionUsesCompositeEndpointId :: Assertion
testDenseThinCompositionUsesCompositeEndpointId =
  case mkFinCat denseChainObjects denseChainMorphismMap (denseChainComposition denseChain02) of
    Left validationErrors ->
      assertFailure ("expected the dense thin chain to validate, got " <> show validationErrors)
    Right category -> do
      representationTag category @?= "DenseThinFinCat"
      case
        ( hom category (FinObjectId 0) (FinObjectId 1),
          hom category (FinObjectId 1) (FinObjectId 2),
          hom category (FinObjectId 0) (FinObjectId 2)
        )
        of
          (Just leftStep, Just rightStep, Just expectedComposite) -> do
            composite <- expectRight (composeIn category rightStep leftStep)
            finMorId composite @?= finMorId expectedComposite
            finMorId composite @?= denseChain02
          _ ->
            assertFailure "expected the dense chain to expose all non-identity morphisms"

testDenseThinCompositionResultEndpointMismatchRejected :: Assertion
testDenseThinCompositionResultEndpointMismatchRejected =
  case mkFinCat denseChainObjects denseChainMorphismMap (denseChainComposition denseChain01) of
    Left validationErrors
      | any (== CompositionResultEndpointMismatch denseChain12 denseChain01 denseChain01) validationErrors ->
          pure ()
    other ->
      assertFailure
        ( "expected a composition-result endpoint mismatch for the dense chain, got "
            <> show other
        )

testThinMorphismSmartConstructorsAndDefensiveCompose :: Assertion
testThinMorphismSmartConstructorsAndDefensiveCompose = do
  mkThinMorphismBy (<=) (3 :: Int) 1 @?= Nothing
  case (mkThinMorphismBy (<=) (1 :: Int) 2, mkThinMorphismBy (<=) (2 :: Int) 4) of
    (Just firstStep, Just secondStep) -> do
      case composeThinMorphismBy (<=) secondStep firstStep of
        Just composite -> do
          thinMorphismSource composite @?= 1
          thinMorphismTarget composite @?= 4
        Nothing ->
          assertFailure "expected valid thin morphisms to compose"
    _ ->
      assertFailure "expected monotone thin morphisms to construct"
  case (mkThinMorphismBy (<=) (2 :: Int) 3, mkThinMorphismBy (\_ _ -> True) (4 :: Int) 2) of
    (Just validLeft, Just staleRight) ->
      composeThinMorphismBy (<=) validLeft staleRight @?= Nothing
    _ ->
      assertFailure "expected the permissive relation to build the stale morphism"

testPosetSmartConstructorsCompose :: Assertion
testPosetSmartConstructorsCompose = do
  mkPosetMor (3 :: Int) 1 @?= Nothing
  case (mkPosetMor (0 :: Int) 1, mkPosetMor (1 :: Int) 3) of
    (Just firstStep, Just secondStep) ->
      case composeMor (PosetCat :: PosetCat Int) secondStep firstStep of
        Right composite -> do
          posetSource composite @?= 0
          posetTarget composite @?= 3
        Left () ->
          assertFailure "expected comparable poset morphisms to compose"
    _ ->
      assertFailure "expected comparable poset morphisms to construct"

triangleGeneral :: Either FinCatBuildError FinCat
triangleGeneral =
  finCategory $ do
    a <- object "A"
    b <- object "B"
    c <- object "C"
    f <- arrow a b "f"
    g <- arrow b c "g"
    h <- arrow a c "h"
    equate (g `after` f) h

longPathGeneral :: Either FinCatBuildError FinCat
longPathGeneral =
  finCategory $ do
    a <- object "A"
    b <- object "B"
    c <- object "C"
    d <- object "D"

    f <- arrow a b "f"
    g <- arrow b c "g"
    h <- arrow c d "h"
    gf <- arrow a c "gf"
    hg <- arrow b d "hg"
    hgf <- arrow a d "hgf"

    -- Deliberately first: elaboration must not depend on declaration order.
    equate (h `after` g `after` f) hgf
    equate (g `after` f) gf
    equate (h `after` g) hg
    equate (hg `after` f) hgf

inversePairGeneral :: Either FinCatBuildError FinCat
inversePairGeneral =
  finCategory $ do
    a <- object "A"
    b <- object "B"
    f <- arrow a b "f"
    g <- arrow b a "g"

    equate (g `after` f) (identityAt a)
    equate (f `after` g) (identityAt b)

representationTag :: FinCat -> String
representationTag =
  takeWhile (/= ' ') . show

testPosetChainIsDense :: Assertion
testPosetChainIsDense =
  case chainPoset of
    Left buildError ->
      assertFailure
        ("expected a dense category, got " <> show buildError)
    Right category -> do
      representationTag category @?= "DenseThinFinCat"
      objectCount category @?= 3
      finCatNonIdentityMorphismCount category @?= 3
      assertBool
        "transitive closure provides the morphism 0 -> 2"
        ( reachableIn
            category
            (FinObjectId 0)
            (FinObjectId 2)
        )
      assertBool
        "the strict order is directional"
        ( not
            ( reachableIn
                category
                (FinObjectId 2)
                (FinObjectId 0)
            )
        )

testTriangleIsThinWithComposite :: Assertion
testTriangleIsThinWithComposite =
  case triangleGeneral of
    Left buildError ->
      assertFailure
        ("expected the triangle to compile, got " <> show buildError)
    Right category -> do
      representationTag category @?= "ThinFinCat"
      objectCount category @?= 3
      finCatNonIdentityMorphismCount category @?= 3
      case
        ( hom category (FinObjectId 0) (FinObjectId 1),
          hom category (FinObjectId 1) (FinObjectId 2),
          hom category (FinObjectId 0) (FinObjectId 2)
        )
        of
          (Just f, Just g, Just h) -> do
            composite <-
              expectRight (composeIn category g f)
            finMorId composite @?= finMorId h
          _ ->
            assertFailure "expected all three triangle morphisms"

testLongPathEquation :: Assertion
testLongPathEquation =
  case longPathGeneral of
    Left buildError ->
      assertFailure
        ( "expected the length-three presentation to compile, got "
            <> show buildError
        )
    Right category ->
      case
        ( hom category (FinObjectId 0) (FinObjectId 2),
          hom category (FinObjectId 2) (FinObjectId 3),
          hom category (FinObjectId 0) (FinObjectId 3)
        )
        of
          (Just gf, Just h, Just hgf) -> do
            composite <-
              expectRight (composeIn category h gf)
            finMorId composite @?= finMorId hgf
          _ ->
            assertFailure "expected gf, h, and hgf"

testIdentityEquations :: Assertion
testIdentityEquations =
  case inversePairGeneral of
    Left buildError ->
      assertFailure
        ("expected the inverse pair to compile, got " <> show buildError)
    Right category ->
      case
        ( hom category (FinObjectId 0) (FinObjectId 1),
          hom category (FinObjectId 1) (FinObjectId 0)
        )
        of
          (Just f, Just g) -> do
            object0 <-
              expectRight (mkFinObject category (FinObjectId 0))
            object1 <-
              expectRight (mkFinObject category (FinObjectId 1))
            sourceIdentity <-
              expectRight (composeIn category g f)
            targetIdentity <-
              expectRight (composeIn category f g)

            finMorId sourceIdentity
              @?= finMorId
                (idOf object0)

            finMorId targetIdentity
              @?= finMorId
                (idOf object1)
          _ ->
            assertFailure "expected both inverse morphisms"

testIdentityEquationMismatchRejected :: Assertion
testIdentityEquationMismatchRejected =
  case finCategory badIdentityPresentation of
    Left (IdentityEquationMismatch _ _ _) ->
      pure ()
    other ->
      assertFailure
        ("expected IdentityEquationMismatch, got " <> show other)
  where
    badIdentityPresentation = do
      a <- object "A"
      b <- object "B"
      f <- arrow a b "f"
      k <- arrow a b "k"

      equate (identityAt b `after` f) k

testMixedModesRejected :: Assertion
testMixedModesRejected =
  finCategory mixedPresentation @?= Left MixedPresentationModes
  where
    mixedPresentation = do
      a <- object "A"
      b <- object "B"
      _ <- arrow a b "f"
      below a b

testCyclicStrictOrderRejected :: Assertion
testCyclicStrictOrderRejected =
  case finCategory cyclicPresentation of
    Left (CyclicStrictOrder objectIds) ->
      assertBool
        "the cycle names some object"
        (not (null objectIds))
    other ->
      assertFailure
        ("expected CyclicStrictOrder, got " <> show other)
  where
    cyclicPresentation = do
      a <- object "A"
      b <- object "B"
      below a b
      below b a

testIncompletePresentationRejected :: Assertion
testIncompletePresentationRejected =
  case finCategory incompletePresentation of
    Left (InvalidPresentation _) ->
      pure ()
    other ->
      assertFailure
        ("expected InvalidPresentation, got " <> show other)
  where
    incompletePresentation = do
      a <- object "A"
      b <- object "B"
      c <- object "C"
      _ <- arrow a b "f"
      _ <- arrow b c "g"
      pure ()

testUnresolvedCompositeRejected :: Assertion
testUnresolvedCompositeRejected =
  case finCategory unresolvedPresentation of
    Left (UnresolvedComposite _) ->
      pure ()
    other ->
      assertFailure
        ("expected UnresolvedComposite, got " <> show other)
  where
    unresolvedPresentation = do
      a <- object "A"
      b <- object "B"
      c <- object "C"
      d <- object "D"
      f <- arrow a b "f"
      g <- arrow b c "g"
      h <- arrow c d "h"
      hgf <- arrow a d "hgf"

      equate (h `after` g `after` f) hgf

testNonComposablePathRejected :: Assertion
testNonComposablePathRejected =
  case finCategory badPathPresentation of
    Left (NonComposablePath _ _ _ _) ->
      pure ()
    other ->
      assertFailure
        ("expected NonComposablePath, got " <> show other)
  where
    badPathPresentation = do
      a <- object "A"
      b <- object "B"
      c <- object "C"
      d <- object "D"
      f <- arrow a b "f"
      g <- arrow c d "g"
      h <- arrow a d "h"

      equate (g `after` f) h

testNonParallelEquationRejected :: Assertion
testNonParallelEquationRejected =
  case finCategory nonParallelPresentation of
    Left (NonParallelEquation _ _ _ _) ->
      pure ()
    other ->
      assertFailure
        ("expected NonParallelEquation, got " <> show other)
  where
    nonParallelPresentation = do
      a <- object "A"
      b <- object "B"
      c <- object "C"
      f <- arrow a b "f"
      g <- arrow b c "g"
      h <- arrow b c "h"

      equate (g `after` f) h

testConflictingCompositionRejected :: Assertion
testConflictingCompositionRejected =
  case finCategory conflictingPresentation of
    Left (ConflictingComposition _ _ _ _) ->
      pure ()
    other ->
      assertFailure
        ("expected ConflictingComposition, got " <> show other)
  where
    conflictingPresentation = do
      a <- object "A"
      b <- object "B"
      c <- object "C"
      f <- arrow a b "f"
      g <- arrow b c "g"
      h <- arrow a c "h"
      k <- arrow a c "k"

      equate (g `after` f) h
      equate (g `after` f) k

testUnsupportedEquationRejected :: Assertion
testUnsupportedEquationRejected =
  case finCategory quotientPresentation of
    Left (UnsupportedEquation _ _) ->
      pure ()
    other ->
      assertFailure
        ("expected UnsupportedEquation, got " <> show other)
  where
    quotientPresentation = do
      a <- object "A"
      b <- object "B"
      f <- arrow a b "f"
      g <- arrow a b "g"

      equate f g

testReflexiveEquationAccepted :: Assertion
testReflexiveEquationAccepted =
  case finCategory reflexivePresentation of
    Left buildError ->
      assertFailure
        ( "expected a reflexive equation to be ignored, got "
            <> show buildError
        )
    Right category -> do
      objectCount category @?= 2
      finCatNonIdentityMorphismCount category @?= 1
  where
    reflexivePresentation = do
      a <- object "A"
      b <- object "B"
      f <- arrow a b "f"

      equate f f

testGeneratorBackedAssociativePresentation :: Assertion
testGeneratorBackedAssociativePresentation =
  case finCategory cyclicGroupPresentation of
    Left buildError ->
      assertFailure
        ( "expected the associative endomorphism table to compile, got "
            <> show buildError
        )
    Right category -> do
      representationTag category @?= "FinCat"
      objectCount category @?= 1
      finCatNonIdentityMorphismCount category @?= 2
  where
    cyclicGroupPresentation = do
      x <- object "A"
      a <- arrow x x "a"
      b <- arrow x x "b"

      equate (a `after` a) b
      equate (a `after` b) (identityAt x)
      equate (b `after` a) (identityAt x)
      equate (b `after` b) a

testGeneratorBackedAssociativityRejected :: Assertion
testGeneratorBackedAssociativityRejected =
  case finCategory nonAssociativePresentation of
    Left (InvalidPresentation errors)
      | containsAssociativityViolation errors ->
          pure ()
    other ->
      assertFailure
        ( "expected InvalidPresentation with AssociativityViolation, got "
            <> show other
        )

testRawMkFinCatStillRejectsAssociativityViolation :: Assertion
testRawMkFinCatStillRejectsAssociativityViolation =
  case
    mkFinCat
      unitObjectSet
      unitEndomorphismMap
      nonAssociativeCompositionTable
    of
      Left errors
        | containsAssociativityViolation errors ->
            pure ()
      other ->
        assertFailure
          ( "expected raw mkFinCat to reject the non-associative table, got "
              <> show other
          )

testMkFinCatMatchesExhaustiveValidator :: SmallEndomorphismTable -> QC.Property
testMkFinCatMatchesExhaustiveValidator (SmallEndomorphismTable morphismIds compositionMap) =
  let morphismMap =
        smallEndomorphismMorphismMap morphismIds
      reducedResult =
        mkFinCat unitObjectSet morphismMap compositionMap
      exhaustiveResult =
        trustedFinCatWithGeneratorBasis (Set.fromList morphismIds) unitObjectSet morphismMap compositionMap
   in QC.checkCoverage
        $ QC.cover 10 (acceptsFinCat reducedResult) "valid explicit tables"
        $ QC.cover 20 (not (acceptsFinCat reducedResult)) "invalid explicit tables"
        $ QC.counterexample
          ( "reduced="
              <> show reducedResult
              <> "\nexhaustive="
              <> show exhaustiveResult
              <> "\ncomposition="
              <> show compositionMap
          )
        $ acceptsFinCat reducedResult == acceptsFinCat exhaustiveResult

smallEndomorphismMorphismMap :: [FinMorphismId] -> Map (FinObjectId, FinObjectId) [FinMorphismId]
smallEndomorphismMorphismMap morphismIds =
  case morphismIds of
    [] -> Map.empty
    _ -> Map.singleton (unitObjectId, unitObjectId) morphismIds

acceptsFinCat :: Either errorValue value -> Bool
acceptsFinCat =
  either (const False) (const True)

testNonGeneratorMiddleViolationRejected :: Assertion
testNonGeneratorMiddleViolationRejected = do
  Map.lookup (aMorphismId, aMorphismId) nonGeneratorMiddleViolationTable @?= Just bMorphismId
  ( composeUnitEndomorphism nonGeneratorMiddleViolationTable aMorphismId bMorphismId
      >>= (\composed -> composeUnitEndomorphism nonGeneratorMiddleViolationTable composed aMorphismId)
    )
    @?= Just (FinIdentityId unitObjectId)
  ( composeUnitEndomorphism nonGeneratorMiddleViolationTable bMorphismId aMorphismId
      >>= composeUnitEndomorphism nonGeneratorMiddleViolationTable aMorphismId
    )
    @?= Just aMorphismId
  case mkFinCat unitObjectSet unitEndomorphismMap nonGeneratorMiddleViolationTable of
    Left errors
      | containsAssociativityViolation errors ->
          pure ()
    other ->
      assertFailure
        ( "expected generator validation to reject the non-generator-middle violation, got "
            <> show other
        )

nonGeneratorMiddleViolationTable :: Map (FinMorphismId, FinMorphismId) FinMorphismId
nonGeneratorMiddleViolationTable =
  Map.fromList
    [ ((aMorphismId, aMorphismId), bMorphismId),
      ((aMorphismId, bMorphismId), bMorphismId),
      ((bMorphismId, aMorphismId), FinIdentityId unitObjectId),
      ((bMorphismId, bMorphismId), bMorphismId)
    ]

composeUnitEndomorphism :: Map (FinMorphismId, FinMorphismId) FinMorphismId -> FinMorphismId -> FinMorphismId -> Maybe FinMorphismId
composeUnitEndomorphism compositionMap left right
  | left == FinIdentityId unitObjectId = Just right
  | right == FinIdentityId unitObjectId = Just left
  | otherwise = Map.lookup (left, right) compositionMap

nonAssociativePresentation :: FinBuilder ()
nonAssociativePresentation = do
  x <- object "A"
  a <- arrow x x "a"
  b <- arrow x x "b"

  equate (a `after` a) a
  equate (a `after` b) a
  equate (b `after` a) b
  equate (b `after` b) a

unitObjectId :: FinObjectId
unitObjectId =
  FinObjectId 0

unitObjectSet :: Set FinObjectId
unitObjectSet =
  Set.singleton unitObjectId

aMorphismId :: FinMorphismId
aMorphismId =
  FinGeneratorMorphismId (FinGeneratorId 0)

bMorphismId :: FinMorphismId
bMorphismId =
  FinGeneratorMorphismId (FinGeneratorId 1)

unitEndomorphismMap :: Map (FinObjectId, FinObjectId) [FinMorphismId]
unitEndomorphismMap =
  Map.singleton (unitObjectId, unitObjectId) [aMorphismId, bMorphismId]

nonAssociativeCompositionTable :: Map (FinMorphismId, FinMorphismId) FinMorphismId
nonAssociativeCompositionTable =
  Map.fromList
    [ ((aMorphismId, aMorphismId), aMorphismId),
      ((aMorphismId, bMorphismId), aMorphismId),
      ((bMorphismId, aMorphismId), bMorphismId),
      ((bMorphismId, bMorphismId), aMorphismId)
    ]

containsAssociativityViolation :: NonEmpty FinCatValidationError -> Bool
containsAssociativityViolation =
  any isAssociativityViolation . NonEmpty.toList
  where
    isAssociativityViolation validationError =
      case validationError of
        AssociativityViolation {} -> True
        _ -> False

testDuplicateObjectRejected :: Assertion
testDuplicateObjectRejected =
  finCategory duplicatePresentation
    @?= Left (DuplicateObjectName "A")
  where
    duplicatePresentation = do
      _ <- object "A"
      _ <- object "A"
      pure ()
