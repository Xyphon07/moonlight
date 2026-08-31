{-# LANGUAGE DerivingStrategies #-}

module CoreTests
  ( tests,
  )
where

import Data.IntSet qualified as IntSet
import Moonlight.Core
  ( PartialOrder (..),
  )
import Moonlight.Delta.Frontier
import DeltaLaws
import Moonlight.Delta.Monotone
import Moonlight.Delta.Operator
import Moonlight.Delta.Scope
import Moonlight.Delta.Time
import Test.QuickCheck
  ( Gen,
    chooseInt,
    elements,
    listOf,
    oneof,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

newtype TotalTime = TotalTime
  { unTotalTime :: Int
  }
  deriving stock (Eq, Ord, Show)

instance PartialOrder TotalTime where
  leq left right =
    unTotalTime left <= unTotalTime right

data ProductTime = ProductTime
  { productTimeLeft :: !Int,
    productTimeRight :: !Int
  }
  deriving stock (Eq, Ord, Show)

instance PartialOrder ProductTime where
  leq left right =
    productTimeLeft left <= productTimeLeft right
      && productTimeRight left <= productTimeRight right

tupleProductTime :: (Int, Int) -> ProductTime
tupleProductTime (left, right) =
  ProductTime left right

tests :: TestTree
tests =
  testGroup
    "core"
    [ scopeTests,
      signedTests,
      frontierTests,
      operatorTests,
      monotoneTests
    ]

scopeTests :: TestTree
scopeTests =
  testGroup
    "scope"
    [ testCase "normalization preserves payload orthogonally" $
        let emptyPayloadDelta =
              scopedDelta (dirtyScope IntSet.empty) (Just (IntSet.singleton 1))
         in do
              scopedDeltaSupport emptyPayloadDelta @?= cleanScope
              scopedDeltaPayload emptyPayloadDelta @?= Just (IntSet.singleton 1)
              scopedDeltaNull emptyPayloadDelta @?= False,
      testCase "full scope can carry payload" $
        let deltaValue =
              scopedDelta fullScope (Just (IntSet.singleton 7)) ::
                Scoped IntSet.IntSet IntSet.IntSet
         in do
              normalizeScoped deltaValue @?= deltaValue
              scopedDeltaSupport deltaValue @?= fullScope
              scopedDeltaPayload deltaValue @?= Just (IntSet.singleton 7),
      testCase "payload merge uses Maybe semigroup structure" $
        let leftDelta =
              payloadDelta (IntSet.singleton 1) ::
                Scoped IntSet.IntSet IntSet.IntSet
            rightDelta =
              scopedDelta (dirtyScope (IntSet.singleton 2)) (Just (IntSet.singleton 3))
            emptyPayloadDelta =
              scopedDelta (dirtyScope (IntSet.singleton 4)) Nothing ::
                Scoped IntSet.IntSet IntSet.IntSet
         in do
              scopedDeltaPayload (leftDelta <> rightDelta)
                @?= Just (IntSet.fromList [1, 3])
              scopedDeltaPayload (leftDelta <> emptyPayloadDelta)
                @?= Just (IntSet.singleton 1),
      testCase "restriction intersects dirty scopes and bounds full scopes" $
        let leftRestriction = IntSet.fromList [1, 2, 3]
            rightRestriction = IntSet.fromList [2, 3, 4]
            restricted =
              restrictScope leftRestriction
                (restrictScope rightRestriction fullScope)
         in do
              restricted @?= dirtyScope (IntSet.fromList [2, 3])
              scopeKeys restricted @?= Just (IntSet.fromList [2, 3])
              restrictScope IntSet.empty fullScope @?= cleanScope,
      testCase "scope equality is canonical after normalization" $
        let emptyDirtyScope =
              dirtyScope IntSet.empty
         in do
              emptyDirtyScope @?= (cleanScope :: Scope IntSet.IntSet)
              compare emptyDirtyScope (cleanScope :: Scope IntSet.IntSet) @?= EQ,
      testCase "scope constructors and mapping normalize empty dirty scopes" $
        let dirty =
              dirtyScope (IntSet.singleton 1)
            mappedEmpty =
              mapScope (const IntSet.empty) dirty
         in do
              dirtyScope IntSet.empty @?= (cleanScope :: Scope IntSet.IntSet)
              mappedEmpty @?= (cleanScope :: Scope IntSet.IntSet)
    ]

signedTests :: TestTree
signedTests =
  signedLaws "signed" signedEntriesGen signedKeyGen

frontierTests :: TestTree
frontierTests =
  testGroup
    "frontier"
    [ frontierLaws "total time" totalTimesGen,
      frontierLaws "product time" productTimesGen,
      testCase "product frontier removes dominated points" $
        frontierPoints
          ( mkFrontier
              [ ProductTime 1 2,
                ProductTime 1 1,
                ProductTime 2 1
              ]
          )
          @?= [ProductTime 1 1],
      testCase "product frontier stores a searchable skyline staircase" $
        let frontier =
              mkProductFrontier2 [(0 :: Int, 3 :: Int), (1, 2), (2, 1), (3, 0), (3, 4)]
         in ( productFrontier2Points frontier,
              productFrontier2Contains (2, 2) frontier,
              productFrontier2Contains (0, 2) frontier
            )
              @?= ([(0, 3), (1, 2), (2, 1), (3, 0)], True, False),
      testCase "product frontier containment agrees with generic product order" $
        let diagonal = [(0 :: Int, 4 :: Int), (1, 3), (2, 2), (3, 1), (4, 0)]
            probes = [(0, 0), (1, 4), (2, 3), (4, 4), (5, 0)]
            genericFrontier = mkFrontier (fmap tupleProductTime diagonal)
            productFrontier = mkProductFrontier2 diagonal
         in fmap
              (\probe ->
                 ( frontierContains (tupleProductTime probe) genericFrontier,
                   productFrontier2Contains probe productFrontier
                 )
              )
              probes
              @?= fmap (\contains -> (contains, contains)) [False, True, True, True, True]
    ]

operatorTests :: TestTree
operatorTests =
  testGroup
    "operator"
    [ functorLaws "Timed" timedIntGen,
      functorLaws "OpResult" opResultIntGen,
      testCase "retime preserves payload" $
        retime
          (TotalTime 9)
          (Timed (TotalTime 1) ("payload" :: String))
          @?= Timed (TotalTime 9) "payload",
      testCase "retimeOpResult retimes every emitted value" $
        retimeOpResult
          (TotalTime 7)
          ( OpResult
              { orState = "state" :: String,
                orEmit =
                  [ Timed (TotalTime 1) ("a" :: String),
                    Timed (TotalTime 2) "b"
                  ]
              }
          )
          @?=
            OpResult
              { orState = "state",
                orEmit =
                  [ Timed (TotalTime 7) "a",
                    Timed (TotalTime 7) "b"
                  ]
              }
    ]

monotoneTests :: TestTree
monotoneTests =
  testGroup
    "monotone"
    [ monotoneLaws "monotone max" max monotoneSectionGen,
      monotoneLaws "monotone addition" (+) monotoneSectionGen
    ]

signedKeyGen :: Gen Int
signedKeyGen =
  chooseInt (0, 16)

signedEntriesGen :: Gen [(Int, Int)]
signedEntriesGen =
  listOf ((,) <$> signedKeyGen <*> chooseInt (-16, 16))

totalTimeGen :: Gen TotalTime
totalTimeGen =
  TotalTime <$> chooseInt (0, 64)

totalTimesGen :: Gen [TotalTime]
totalTimesGen =
  listOf totalTimeGen

productTimeGen :: Gen ProductTime
productTimeGen =
  ProductTime
    <$> chooseInt (0, 16)
    <*> chooseInt (0, 16)

productTimesGen :: Gen [ProductTime]
productTimesGen =
  listOf productTimeGen

timedIntGen :: Gen (Timed TotalTime Int)
timedIntGen =
  Timed <$> totalTimeGen <*> chooseInt (-16, 16)

opResultIntGen :: Gen (OpResult TotalTime String Int)
opResultIntGen =
  OpResult
    <$> elements ["left", "right"]
    <*> listOf timedIntGen

monotoneDeltaGen :: Gen (Monotone Int)
monotoneDeltaGen =
  oneof
    [ JoinDelta <$> chooseInt (-16, 16),
      ResetDelta <$> chooseInt (-16, 16)
    ]

monotoneSectionGen :: Gen (Monotone Int, Monotone Int, Int)
monotoneSectionGen =
  (,,)
    <$> monotoneDeltaGen
    <*> monotoneDeltaGen
    <*> chooseInt (-16, 16)
