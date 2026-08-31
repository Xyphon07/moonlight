module PatchTests
  ( patchDeltaGen,
    tests,
  )
where

import CodecSpec (codecTests)
import LawsSpec (lawTests)
import ReferenceSpec (referenceTests)
import ReplaySpec (replayTests)
import SemanticsSpec (semanticTests)
import DeltaHashSpec (deltaHashTests)
import PatchSupport (patchDeltaGen)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "patch"
    [ lawTests,
      semanticTests,
      codecTests,
      replayTests,
      referenceTests,
      deltaHashTests
    ]
