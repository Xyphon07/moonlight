{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AdhesiveSpec
  ( tests,
  )
where

import Data.List (isInfixOf)
import Data.Foldable (traverse_)
import Moonlight.Category
  ( AdhesiveCategory (..),
    Category (..),
    HasPullbacks (..),
    HasPushouts (..),
    MonicMatchComponents (..),
    PBPOAdhesiveCategory (..),
    PBPOComplementComponents (..),
    PushoutComplementComponents (..),
    monicMatchArrow,
    pbpoComplement,
    pbpoComplementBorrowedLeg,
    pbpoComplementMonicMatch,
    pbpoComplementPullbackObject,
    pbpoComplementPullbackToBorrowed,
    pbpoComplementPullbackToMatch,
    pbpoComplementPushoutFromComplement,
    pbpoComplementPushoutFromMatch,
    pbpoComplementPushoutObject,
    pbpoComplementResidualLeg,
    pbpoComplementRuleLeg,
    pbpoPullbackSquareCommutes,
    pbpoPushoutSquareCommutes,
    pushoutComplement,
    pushoutComplementSquareCommutes,
    composeMor,
    witnessMonic,
  )
import Moonlight.Category.Effect.Harness.Adhesive qualified as AdhesiveHarness
import Moonlight.Category.Effect.Harness.Category qualified as CategoryHarness
import Moonlight.Category.Effect.Harness.Core (CategoryLaws (categoryAssociativity))
import Moonlight.Category.Effect.Harness.Limits qualified as LimitsHarness
import Moonlight.Category.Pure.Adhesive
  ( denseIntSetInterval,
    denseIntSetMember,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

data TestCategory = TestCategory

data NativePBPOCategory = NativePBPOCategory

data MissingMediatorCategory = MissingMediatorCategory

data BrokenCompositionCategory = BrokenCompositionCategory

data TestObject
  = ObjectK
  | ObjectL
  | ObjectD
  | ObjectG
  | ObjectP
  | ObjectQ
  | ObjectNativePullback
  | ObjectNativePushout
  deriving stock (Eq, Show)

data TestMorphism = TestMorphism
  { testMorphismSource :: !TestObject,
    testMorphismTarget :: !TestObject
  }
  deriving stock (Eq, Show)

data TestTwoMor

data TestCompositor = TestCompositor

newtype BrokenObject = BrokenObject TestObject
  deriving stock (Eq, Show)

data BrokenMorphism = BrokenMorphism
  { brokenMorphismSource :: !BrokenObject,
    brokenMorphismTarget :: !BrokenObject
  }
  deriving stock (Eq, Show)

data BrokenTwoMor

data BrokenCompositor

newtype NativeObject = NativeObject TestObject
  deriving stock (Eq, Show)

data NativeMorphism = NativeMorphism
  { nativeMorphismSource :: !NativeObject,
    nativeMorphismTarget :: !NativeObject
  }
  deriving stock (Eq, Show)

data NativeTwoMor

data NativeCompositor = NativeCompositor

newtype MissingMediatorObject = MissingMediatorObject TestObject
  deriving stock (Eq, Show)

data MissingMediatorMorphism = MissingMediatorMorphism
  { missingMediatorMorphismSource :: !MissingMediatorObject,
    missingMediatorMorphismTarget :: !MissingMediatorObject
  }
  deriving stock (Eq, Show)

data MissingMediatorTwoMor

data MissingMediatorCompositor = MissingMediatorCompositor

instance Category TestCategory where
  type Ob TestCategory = TestObject
  type Mor TestCategory = TestMorphism
  type TwoMor TestCategory = TestTwoMor
  type Compositor TestCategory = TestCompositor

  identity _ objectValue =
    Right (TestMorphism objectValue objectValue)

  compose _ leftMorphism rightMorphism
    | testMorphismTarget rightMorphism == testMorphismSource leftMorphism =
        Right (TestMorphism (testMorphismSource rightMorphism) (testMorphismTarget leftMorphism), TestCompositor)
    | otherwise =
        Left ()

  source _ =
    Right . testMorphismSource

  target _ =
    Right . testMorphismTarget

instance Category BrokenCompositionCategory where
  type Ob BrokenCompositionCategory = BrokenObject
  type Mor BrokenCompositionCategory = BrokenMorphism
  type TwoMor BrokenCompositionCategory = BrokenTwoMor
  type Compositor BrokenCompositionCategory = BrokenCompositor

  identity _ objectValue = Right (BrokenMorphism objectValue objectValue)
  compose _ _ _ = Left ()
  source _ = Right . brokenMorphismSource
  target _ = Right . brokenMorphismTarget

instance HasPullbacks TestCategory where
  pullback _ leftMorphism rightMorphism
    | testMorphismTarget leftMorphism == testMorphismTarget rightMorphism =
        Just
          ( ObjectP,
            TestMorphism ObjectP (testMorphismSource leftMorphism),
            TestMorphism ObjectP (testMorphismSource rightMorphism)
          )
    | otherwise =
        Nothing

  pullbackMediator _ leftMorphism rightMorphism coneLeft coneRight
    | testMorphismTarget leftMorphism == testMorphismTarget rightMorphism
        && testMorphismTarget coneLeft == testMorphismSource leftMorphism
        && testMorphismTarget coneRight == testMorphismSource rightMorphism
        && testMorphismSource coneLeft == testMorphismSource coneRight
        && composeMor @TestCategory TestCategory leftMorphism coneLeft == composeMor @TestCategory TestCategory rightMorphism coneRight =
        Just (TestMorphism (testMorphismSource coneLeft) ObjectP)
    | otherwise =
        Nothing

instance HasPushouts TestCategory where
  pushout _ leftMorphism rightMorphism
    | testMorphismSource leftMorphism == testMorphismSource rightMorphism =
        Just
          ( ObjectQ,
            TestMorphism (testMorphismTarget leftMorphism) ObjectQ,
            TestMorphism (testMorphismTarget rightMorphism) ObjectQ
          )
    | otherwise =
        Nothing

instance AdhesiveCategory TestCategory where
  monicMatchComponents _ morphism =
    Just (MonicMatchComponents morphism)

  pushoutComplementComponents _ _ _ =
    Just
      PushoutComplementComponents
        { pushoutComplementComponentObject = ObjectD,
          pushoutComplementComponentBorrowedLeg = TestMorphism ObjectD ObjectG,
          pushoutComplementComponentResidualLeg = TestMorphism ObjectK ObjectD
        }

instance PBPOAdhesiveCategory TestCategory

instance Category NativePBPOCategory where
  type Ob NativePBPOCategory = NativeObject
  type Mor NativePBPOCategory = NativeMorphism
  type TwoMor NativePBPOCategory = NativeTwoMor
  type Compositor NativePBPOCategory = NativeCompositor

  identity _ objectValue =
    Right (NativeMorphism objectValue objectValue)

  compose _ leftMorphism rightMorphism
    | nativeMorphismTarget rightMorphism == nativeMorphismSource leftMorphism =
        Right (NativeMorphism (nativeMorphismSource rightMorphism) (nativeMorphismTarget leftMorphism), NativeCompositor)
    | otherwise =
        Left ()

  source _ =
    Right . nativeMorphismSource

  target _ =
    Right . nativeMorphismTarget

instance HasPullbacks NativePBPOCategory where
  pullback _ _ _ =
    Nothing

  pullbackMediator _ _ _ _ _ =
    Nothing

instance HasPushouts NativePBPOCategory where
  pushout _ _ _ =
    Nothing

instance AdhesiveCategory NativePBPOCategory where
  monicMatchComponents _ morphism =
    Just (MonicMatchComponents morphism)

  pushoutComplementComponents _ _ _ =
    Nothing

instance PBPOAdhesiveCategory NativePBPOCategory where
  pbpoComplementComponents _ _ _ =
    Just
      PBPOComplementComponents
        { pbpoComplementComponentPullbackObject = NativeObject ObjectNativePullback,
          pbpoComplementComponentPullbackToBorrowed = NativeMorphism (NativeObject ObjectNativePullback) (NativeObject ObjectD),
          pbpoComplementComponentPullbackToMatch = NativeMorphism (NativeObject ObjectNativePullback) (NativeObject ObjectL),
          pbpoComplementComponentPushoutObject = NativeObject ObjectNativePushout,
          pbpoComplementComponentPushoutFromComplement = NativeMorphism (NativeObject ObjectD) (NativeObject ObjectNativePushout),
          pbpoComplementComponentPushoutFromMatch = NativeMorphism (NativeObject ObjectL) (NativeObject ObjectNativePushout),
          pbpoComplementComponentBorrowedLeg = NativeMorphism (NativeObject ObjectD) (NativeObject ObjectG),
          pbpoComplementComponentResidualLeg = NativeMorphism (NativeObject ObjectK) (NativeObject ObjectD)
        }

instance Category MissingMediatorCategory where
  type Ob MissingMediatorCategory = MissingMediatorObject
  type Mor MissingMediatorCategory = MissingMediatorMorphism
  type TwoMor MissingMediatorCategory = MissingMediatorTwoMor
  type Compositor MissingMediatorCategory = MissingMediatorCompositor

  identity _ objectValue =
    Right (MissingMediatorMorphism objectValue objectValue)

  compose _ leftMorphism rightMorphism
    | missingMediatorMorphismTarget rightMorphism == missingMediatorMorphismSource leftMorphism =
        Right
          ( MissingMediatorMorphism
              (missingMediatorMorphismSource rightMorphism)
              (missingMediatorMorphismTarget leftMorphism),
            MissingMediatorCompositor
          )
    | otherwise =
        Left ()

  source _ =
    Right . missingMediatorMorphismSource

  target _ =
    Right . missingMediatorMorphismTarget

instance HasPullbacks MissingMediatorCategory where
  pullback _ leftMorphism rightMorphism
    | missingMediatorMorphismTarget leftMorphism == missingMediatorMorphismTarget rightMorphism =
        Just
          ( MissingMediatorObject ObjectP,
            MissingMediatorMorphism (MissingMediatorObject ObjectP) (missingMediatorMorphismSource leftMorphism),
            MissingMediatorMorphism (MissingMediatorObject ObjectP) (missingMediatorMorphismSource rightMorphism)
          )
    | otherwise =
        Nothing

  pullbackMediator _ _ _ _ _ =
    Nothing

tests :: TestTree
tests =
  testGroup
    "Adhesive"
    [ testCase "PBPO complement carries pullback and pushout squares" testPBPOComplement,
      testCase "PBPO complement can be native rather than DPO-derived" testNativePBPOComplement,
      testCase "pushout complement witness square commutes" testPushoutComplementSquare,
      testCase "pullback mediator law rejects missing mediators for valid cones" testPullbackMediatorLawRejectsMissingMediator,
      testCase "pullback law rejects a missing construction on a valid cospan" testPullbackLawRejectsMissingConstruction,
      testCase "associativity law rejects failed composition on valid boundaries" testAssociativityLawRejectsFailedComposition,
      testCase "dense interval rejects overflowing bounds" testDenseIntervalRejectsOverflow,
      testCase "public adhesive surface keeps witness constructors opaque" testWitnessSurfaceOpaque
    ]

testDenseIntervalRejectsOverflow :: IO ()
testDenseIntervalRejectsOverflow = do
  denseIntSetInterval 512 maxBound 2 @?= Nothing
  denseIntSetInterval 512 511 2 @?= Nothing
  case denseIntSetInterval 512 510 2 of
    Nothing -> assertFailure "expected the final two in-range elements"
    Just interval -> do
      denseIntSetMember 510 interval @?= True
      denseIntSetMember 511 interval @?= True

testPBPOComplement :: IO ()
testPBPOComplement =
  let ruleLeg = TestMorphism ObjectK ObjectL
      matchArrow = TestMorphism ObjectL ObjectG
   in case witnessMonic @TestCategory TestCategory matchArrow >>= pbpoComplement @TestCategory TestCategory ruleLeg of
        Nothing ->
          assertFailure "expected PBPO complement witness"
        Just witness -> do
          pbpoComplementRuleLeg witness @?= ruleLeg
          monicMatchArrow (pbpoComplementMonicMatch witness) @?= matchArrow
          pbpoComplementPullbackObject witness @?= ObjectP
          pbpoComplementPullbackToBorrowed witness @?= TestMorphism ObjectP ObjectD
          pbpoComplementPullbackToMatch witness @?= TestMorphism ObjectP ObjectL
          pbpoComplementPushoutObject witness @?= ObjectQ
          pbpoComplementPushoutFromComplement witness @?= TestMorphism ObjectD ObjectQ
          pbpoComplementPushoutFromMatch witness @?= TestMorphism ObjectL ObjectQ
          pbpoComplementBorrowedLeg witness @?= TestMorphism ObjectD ObjectG
          pbpoComplementResidualLeg witness @?= TestMorphism ObjectK ObjectD
          pbpoPullbackSquareCommutes TestCategory witness @?= True
          pbpoPushoutSquareCommutes TestCategory witness @?= True

testNativePBPOComplement :: IO ()
testNativePBPOComplement =
  let ruleLeg = NativeMorphism (NativeObject ObjectK) (NativeObject ObjectL)
      matchArrow = NativeMorphism (NativeObject ObjectL) (NativeObject ObjectG)
   in case witnessMonic @NativePBPOCategory NativePBPOCategory matchArrow >>= pbpoComplement @NativePBPOCategory NativePBPOCategory ruleLeg of
        Nothing ->
          assertFailure "expected native PBPO complement witness"
        Just witness -> do
          pbpoComplementPullbackObject witness @?= NativeObject ObjectNativePullback
          pbpoComplementPushoutObject witness @?= NativeObject ObjectNativePushout

testPushoutComplementSquare :: IO ()
testPushoutComplementSquare =
  let ruleLeg = TestMorphism ObjectK ObjectL
      matchArrow = TestMorphism ObjectL ObjectG
   in case witnessMonic @TestCategory TestCategory matchArrow >>= pushoutComplement @TestCategory TestCategory ruleLeg of
        Nothing ->
          assertFailure "expected pushout complement witness"
        Just witness ->
          pushoutComplementSquareCommutes TestCategory witness @?= True

testPullbackMediatorLawRejectsMissingMediator :: IO ()
testPullbackMediatorLawRejectsMissingMediator =
  let objectValue =
        MissingMediatorObject
      morphismValue sourceValue targetValue =
        MissingMediatorMorphism (objectValue sourceValue) (objectValue targetValue)
      leftBase = morphismValue ObjectL ObjectG
      rightBase = morphismValue ObjectD ObjectG
      coneLeft = morphismValue ObjectK ObjectL
      coneRight = morphismValue ObjectK ObjectD
   in AdhesiveHarness.pullbackMediatorCommutes @MissingMediatorCategory MissingMediatorCategory leftBase rightBase coneLeft coneRight
        @?= False

testPullbackLawRejectsMissingConstruction :: IO ()
testPullbackLawRejectsMissingConstruction =
  let targetObject = NativeObject ObjectG
      leftMorphism = NativeMorphism (NativeObject ObjectL) targetObject
      rightMorphism = NativeMorphism (NativeObject ObjectD) targetObject
   in LimitsHarness.pullbackCommutative @NativePBPOCategory NativePBPOCategory leftMorphism rightMorphism
        @?= False

testAssociativityLawRejectsFailedComposition :: IO ()
testAssociativityLawRejectsFailedComposition =
  let laws = CategoryHarness.mkCategoryLaws @BrokenCompositionCategory BrokenCompositionCategory
      objectValue = BrokenObject
      morphismValue sourceValue targetValue =
        BrokenMorphism (objectValue sourceValue) (objectValue targetValue)
      firstMorphism = morphismValue ObjectK ObjectL
      secondMorphism = morphismValue ObjectL ObjectD
      thirdMorphism = morphismValue ObjectD ObjectG
   in categoryAssociativity laws firstMorphism secondMorphism thirdMorphism
        @?= False

testWitnessSurfaceOpaque :: IO ()
testWitnessSurfaceOpaque = do
  sourceText <- readFile "src-abstract/Moonlight/Category/Pure/Adhesive.hs"
  traverse_
    (\forbidden -> assertBool ("public surface contains " <> forbidden) (not (forbidden `isInfixOf` sourceText)))
    [ "MonicMatchWitness (..)",
      "PushoutComplementWitness (..)",
      "PBPOComplementWitness (..)",
      "data MonicMatchWitness c = MonicMatchWitness\n  {",
      "data PushoutComplementWitness c = PushoutComplementWitness\n  {",
      "data PBPOComplementWitness c = PBPOComplementWitness\n  {"
    ]
