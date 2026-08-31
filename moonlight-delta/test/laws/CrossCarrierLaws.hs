module CrossCarrierLaws
  ( tests,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import DeltaLaws (deltaNormalizeLaws, deltaSupportLaws, functorLaws)
import EpochSupport.Generators
  ( contextProjectionCarrierIntGen,
    contextProjectionDeltaIntGen,
  )
import Moonlight.Delta.Epoch (emptyContextProjectionDelta)
import Moonlight.Delta.Scope
import Moonlight.Delta.Signed
import PatchSupport (patchDeltaGen)
import Test.QuickCheck (Gen, chooseInt, listOf, oneof)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "laws"
    [ normalizeTests,
      supportTests,
      functorLaws "ContextProjectionDelta" contextProjectionCarrierIntGen
    ]

normalizeTests :: TestTree
normalizeTests =
  testGroup
    "normalize"
    [ deltaNormalizeLaws "signed" signedDeltaGen,
      deltaNormalizeLaws "patch" patchDeltaGen,
      deltaNormalizeLaws "scope" scopedDeltaGen,
      deltaNormalizeLaws "context projection" contextProjectionDeltaIntGen
    ]

supportTests :: TestTree
supportTests =
  testGroup
    "support"
    [ deltaSupportLaws "signed" signedDeltaGen (pure emptySigned),
      deltaSupportLaws "scope" scopedDeltaGen (pure cleanDelta),
      deltaSupportLaws "context projection" contextProjectionDeltaIntGen (pure emptyContextProjectionDelta)
    ]

intSetGen :: Gen IntSet.IntSet
intSetGen =
  IntSet.fromList <$> listOf (chooseInt (0, 16))

maybeIntSetGen :: Gen (Maybe IntSet.IntSet)
maybeIntSetGen =
  oneof
    [ pure Nothing,
      Just <$> intSetGen
    ]

deltaScopeGen :: Gen (Scope IntSet.IntSet)
deltaScopeGen =
  oneof
    [ pure cleanScope,
      dirtyScope <$> intSetGen,
      pure fullScope
    ]

scopedDeltaGen :: Gen (Scoped IntSet.IntSet IntSet.IntSet)
scopedDeltaGen =
  Scoped <$> deltaScopeGen <*> maybeIntSetGen

signedDeltaGen :: Gen (Signed Int)
signedDeltaGen =
  signedFromChangeMap . Map.fromList <$> listOf signedMultiplicityChangeEntryGen

signedMultiplicityChangeEntryGen :: Gen (Int, MultiplicityChange)
signedMultiplicityChangeEntryGen =
  (,) <$> chooseInt (0, 16) <*> (MultiplicityChange . fromIntegral <$> chooseInt (-16, 16))
