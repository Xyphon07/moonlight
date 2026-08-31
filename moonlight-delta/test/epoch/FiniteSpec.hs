module FiniteSpec
  ( finiteTests,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (catMaybes)
import Moonlight.Delta.Epoch
import EpochSupport.Generators (mintEpochCase)
import EpochSupport.Reference
import EpochSupport.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

finiteTests :: TestTree
finiteTests =
  testGroup
    "finite exhaustive"
    [ testCase "finite five-key transports match the reference" $
        finiteTransportMismatches @?= [],
      testCase "finite three-key compositions match the reference" $
        finiteComposeMismatches @?= [],
      testCase "finite three-key associativity holds" $
        finiteAssociativityMismatches @?= []
    ]
finiteTransportMismatches ::
  [ ( Transport (IntMap Int) IntSet,
      Transport (IntMap Int) IntSet,
      IntSet,
      IntSet
    )
  ]
finiteTransportMismatches =
  take
    1
    [ (actualTransport, expectedTransport, actualChanged, expectedChanged)
      | epochCase <- finiteDeltas finiteFiveKeys,
        let deltaValue = edcDelta epochCase,
        let reference = referenceFromInput (edcInput epochCase),
        queryKeys <- finiteIntSets finiteFiveKeys,
        let actualTransport = transportKeys deltaValue queryKeys,
        let expectedTransport = referenceTransportKeys reference queryKeys,
        let actualChanged = changedKeysAcrossEpoch deltaValue,
        let expectedChanged = referenceChangedKeys reference,
        actualTransport /= expectedTransport || actualChanged /= expectedChanged
    ]

finiteComposeMismatches ::
  [ ( Either (ComposeError Int) EpochReference,
      Either (ComposeError Int) EpochReference
    )
  ]
finiteComposeMismatches =
  take
    1
    [ (actual, expected)
      | firstCase <- finiteDeltas finiteThreeKeys,
        let first = edcDelta firstCase,
        secondCase <- finiteDeltas finiteThreeKeys,
        let second = edcDelta secondCase,
        targetVersion first == sourceVersion second,
        targetKeys first == sourceKeys second,
        let actual = referenceFromDelta <$> composeDelta second first,
        let expected = referenceCompose (referenceFromDelta second) (referenceFromDelta first),
        actual /= expected
    ]

finiteAssociativityMismatches ::
  [ ( Either (ComposeError Int) EpochReference,
      Either (ComposeError Int) EpochReference
    )
  ]
finiteAssociativityMismatches =
  take
    1
    [ (fmap referenceFromDelta leftAssociated, fmap referenceFromDelta rightAssociated)
      | firstCase <- finiteStableDeltas finiteThreeKeys (versionFromKey 0) (versionFromKey 1),
        let first = edcDelta firstCase,
        secondCase <- finiteStableDeltas finiteThreeKeys (versionFromKey 1) (versionFromKey 2),
        let second = edcDelta secondCase,
        thirdCase <- finiteStableDeltas finiteThreeKeys (versionFromKey 2) (versionFromKey 3),
        let third = edcDelta thirdCase,
        let leftAssociated =
              composeDelta second first
                >>= composeDelta third,
        let rightAssociated =
              composeDelta third second
                >>= \thirdSecond -> composeDelta thirdSecond first,
        fmap referenceFromDelta leftAssociated /= fmap referenceFromDelta rightAssociated
    ]

finiteDeltas :: [Int] -> [EpochDeltaCase (IntMap Int) IntSet]
finiteDeltas universeKeys =
  [ deltaValue
    | sourceKeySet <- finiteIntSets universeKeys,
      targetKeySet <- finiteIntSets universeKeys,
      (transport, retired) <- finiteTransports sourceKeySet targetKeySet,
      changedKeys <- finiteChangedBasis sourceKeySet,
      let input =
            EpochInput
              { eiSource = Endpoint (versionFromKey 0) sourceKeySet,
                eiTarget = Endpoint (versionFromKey 1) targetKeySet,
                eiTransport = transport,
                eiRetired = retired,
                eiChanged = changedKeys
              },
      Just deltaValue <- [mintEpochCase input]
  ]

finiteStableDeltas ::
  [Int] ->
  Version ->
  Version ->
  [EpochDeltaCase (IntMap Int) IntSet]
finiteStableDeltas universeKeys sourceVersionValue targetVersionValue =
  [ deltaValue
    | keysValue <- finiteIntSets universeKeys,
      (transport, retired) <- finiteTransports keysValue keysValue,
      changedKeys <- finiteChangedBasis keysValue,
      let input =
            EpochInput
              { eiSource = Endpoint sourceVersionValue keysValue,
                eiTarget = Endpoint targetVersionValue keysValue,
                eiTransport = transport,
                eiRetired = retired,
                eiChanged = changedKeys
              },
      Just deltaValue <- [mintEpochCase input]
  ]

finiteTransports :: IntSet -> IntSet -> [(IntMap Int, IntSet)]
finiteTransports sourceKeySet targetKeySet =
  fmap transportFromChoices (traverse choicesForSourceKey (IntSet.toAscList sourceKeySet))
  where
    choicesForSourceKey sourceKey =
      Nothing
        : [ Just (sourceKey, targetKey)
            | targetKey <- IntSet.toAscList targetKeySet
          ]

    transportFromChoices choices =
      ( IntMap.fromAscList (catMaybes choices),
        IntSet.fromAscList
          [ sourceKey
            | (sourceKey, Nothing) <- zip (IntSet.toAscList sourceKeySet) choices
          ]
      )

finiteChangedBasis :: IntSet -> [IntSet]
finiteChangedBasis sourceKeySet =
  IntSet.empty
    : sourceKeySet
    : fmap IntSet.singleton (IntSet.toAscList sourceKeySet)

finiteIntSets :: [Int] -> [IntSet]
finiteIntSets universeKeys =
  fmap IntSet.fromList (subsequencesList universeKeys)

subsequencesList :: [a] -> [[a]]
subsequencesList values =
  case values of
    [] ->
      [[]]
    value : rest ->
      let restSubsequences = subsequencesList rest
       in restSubsequences <> fmap (value :) restSubsequences

finiteFiveKeys :: [Int]
finiteFiveKeys =
  [0 .. 4]

finiteThreeKeys :: [Int]
finiteThreeKeys =
  [0 .. 2]
