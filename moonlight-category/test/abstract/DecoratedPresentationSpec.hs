{-# LANGUAGE TypeFamilies #-}

module DecoratedPresentationSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Moonlight.Category
  ( Category (..),
    CompositionResult (..),
    HasPushouts (..),
    StructuredCompositionAlgebra (..),
    StructuredCospanError (..),
    compileDecoratedPresentation,
    compileDecoratedPresentationStructured,
    composeStructuredCospan,
    mkStructuredCospan,
    presentationGlue,
    presentationLeaf,
    structuredDecoration,
    structuredLeftLeg,
    structuredRightLeg,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "DecoratedPresentation"
    [ testCase "plain decorated presentation accumulates obligations compositionally" testCompileDecoratedPresentation,
      testCase "structured decorated presentation composes through the shared algebra" testCompileStructuredPresentation,
      testCase "structured cospan composition rejects boundary mismatches before pushout" testStructuredCospanBoundaryMismatchRejectedBeforePushout
    ]

testCompileDecoratedPresentation :: Assertion
testCompileDecoratedPresentation =
  let presentation =
        presentationGlue
          "outer"
          (presentationLeaf "alpha" ["a"])
          ( presentationGlue
              "inner"
              (presentationLeaf "beta" ["b"])
              (presentationLeaf "gamma" ["c"])
          )
      result =
        compileDecoratedPresentation
          (<>)
          (\boundary (leftIr, _) (rightIr, _) -> (leftIr <> "|" <> rightIr, [boundary]))
          presentation
   in result
        @?= CompositionResult
          { composedIR = "alpha|beta|gamma",
            composedObligations = ["inner", "outer"],
            composedDecoration = ["a", "b", "c"]
          }

testCompileStructuredPresentation :: Assertion
testCompileStructuredPresentation =
  let compositionAlgebra =
        StructuredCompositionAlgebra
          { toStructuredBoundary =
              \_ (_, decoration) ->
                either (const Nothing) Just (mkStructuredCospan TestCat leftBoundaryLeg rightBoundaryLeg decoration),
            fromStructuredComposition =
              \boundary (leftIr, _) (rightIr, _) structuredBoundary ->
                ( leftIr <> "+" <> rightIr,
                  [boundary <> ":" <> testDecorationTag (structuredDecoration structuredBoundary)]
                )
          }
      presentation =
        presentationGlue
          "seam"
          (presentationLeaf "left" (TestDecoration "left" ["l"]))
          (presentationLeaf "right" (TestDecoration "right" ["r"]))
   in case compileDecoratedPresentationStructured TestCat compositionAlgebra mergeTestDecorations presentation of
        Right result ->
          result
            @?= CompositionResult
              { composedIR = "left+right",
                composedObligations = ["seam:left-right"],
                composedDecoration = TestDecoration "left-right" ["l", "r"]
              }
        Left _ ->
          assertFailure "expected structured decorated presentation to compile"

testStructuredCospanBoundaryMismatchRejectedBeforePushout :: Assertion
testStructuredCospanBoundaryMismatchRejectedBeforePushout = do
  leftCospan <-
    expectStructuredCospan
      ( mkStructuredCospan
          TestCat
          (testMor "left-input" outerLeft leftApex)
          (testMor "left-output" sharedBoundary leftApex)
          "left"
      )
  validRightCospan <-
    expectStructuredCospan
      ( mkStructuredCospan
          TestCat
          (testMor "right-input" sharedBoundary rightApex)
          (testMor "right-output" outerRight rightApex)
          "right"
      )
  mismatchedRightCospan <-
    expectStructuredCospan
      ( mkStructuredCospan
          TestCat
          (testMor "right-input" wrongBoundary rightApex)
          (testMor "right-output" outerRight rightApex)
          "right"
      )

  case composeStructuredCospan TestCat (<>) leftCospan validRightCospan of
    Right composed -> do
      structuredDecoration composed @?= "leftright"
      structuredLeftLeg composed @?= testMor "pushout-left.left-input" outerLeft pushoutObject
      structuredRightLeg composed @?= testMor "pushout-right.right-output" outerRight pushoutObject
    Left _ ->
      assertFailure "expected matching structured cospan boundaries to compose"

  case composeStructuredCospan TestCat (<>) leftCospan mismatchedRightCospan of
    Left (StructuredCospanBoundaryMismatch leftOutput rightInput) -> do
      leftOutput @?= sharedBoundary
      rightInput @?= wrongBoundary
    other ->
      assertFailure
        ( "expected boundary mismatch before a pushout is requested, got "
            <> describeStructuredCospanResult other
        )

type TestCat :: Type
data TestCat = TestCat

type TestObj :: Type
data TestObj = TestObj String
  deriving stock (Eq, Show)

type TestMor :: Type
data TestMor = TestMor
  { testMorName :: String,
    testMorSource :: TestObj,
    testMorTarget :: TestObj
  }
  deriving stock (Eq, Show)

type TestDecoration :: Type
data TestDecoration = TestDecoration
  { testDecorationTag :: String,
    testDecorationPayload :: [String]
  }
  deriving stock (Eq, Show)

mergeTestDecorations :: TestDecoration -> TestDecoration -> TestDecoration
mergeTestDecorations leftDecoration rightDecoration =
  TestDecoration
    { testDecorationTag =
        testDecorationTag leftDecoration <> "-" <> testDecorationTag rightDecoration,
      testDecorationPayload =
        testDecorationPayload leftDecoration <> testDecorationPayload rightDecoration
    }

instance Category TestCat where
  type Ob TestCat = TestObj
  type Mor TestCat = TestMor

  identity _ objectValue = Right (testMor ("id:" <> show objectValue) objectValue objectValue)
  compose _ leftMorphism rightMorphism
    | testMorTarget rightMorphism == testMorSource leftMorphism =
        Right
          ( testMor
              (testMorName leftMorphism <> "." <> testMorName rightMorphism)
              (testMorSource rightMorphism)
              (testMorTarget leftMorphism),
            ()
          )
    | otherwise = Left ()
  source _ = Right . testMorSource
  target _ = Right . testMorTarget

instance HasPushouts TestCat where
  pushout _ leftMorphism rightMorphism
    | testMorSource leftMorphism == testMorSource rightMorphism =
        Just
          ( pushoutObject,
            testMor "pushout-left" (testMorTarget leftMorphism) pushoutObject,
            testMor "pushout-right" (testMorTarget rightMorphism) pushoutObject
          )
    | otherwise = Nothing

testMor :: String -> TestObj -> TestObj -> TestMor
testMor = TestMor

leftBoundaryLeg :: TestMor
leftBoundaryLeg =
  testMor "left-boundary" sharedBoundary sharedBoundary

rightBoundaryLeg :: TestMor
rightBoundaryLeg =
  testMor "right-boundary" sharedBoundary sharedBoundary

outerLeft :: TestObj
outerLeft =
  TestObj "outer-left"

outerRight :: TestObj
outerRight =
  TestObj "outer-right"

sharedBoundary :: TestObj
sharedBoundary =
  TestObj "shared-boundary"

wrongBoundary :: TestObj
wrongBoundary =
  TestObj "wrong-boundary"

leftApex :: TestObj
leftApex =
  TestObj "left-apex"

rightApex :: TestObj
rightApex =
  TestObj "right-apex"

pushoutObject :: TestObj
pushoutObject =
  TestObj "pushout"

expectStructuredCospan :: Either (StructuredCospanError TestCat) value -> IO value
expectStructuredCospan result =
  case result of
    Left _ -> assertFailure ("expected structured cospan, got " <> describeStructuredCospanResult result)
    Right value -> pure value

describeStructuredCospanResult :: Either (StructuredCospanError TestCat) a -> String
describeStructuredCospanResult result =
  case result of
    Right _ ->
      "Right <structured-cospan>"
    Left (StructuredCospanCategoryError _) ->
      "Left StructuredCospanCategoryError"
    Left (StructuredCospanBoundaryMismatch leftOutput rightInput) ->
      "Left (StructuredCospanBoundaryMismatch " <> show leftOutput <> " " <> show rightInput <> ")"
    Left (StructuredCospanPushoutMissing leftLeg rightLeg) ->
      "Left (StructuredCospanPushoutMissing " <> show leftLeg <> " " <> show rightLeg <> ")"
