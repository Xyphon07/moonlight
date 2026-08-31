module AbstractTests
  ( tests,
  )
where

import Moonlight.Algebra.Effect.Laws qualified as EffectLaws
import OrderedLatticeSpec qualified
import QuantaleSpec qualified
import SparseVecSpec qualified
import Test.Tasty
  ( TestTree,
    testGroup,
  )

tests :: TestTree
tests =
  testGroup
    "moonlight-algebra"
    [ EffectLaws.tests,
      OrderedLatticeSpec.tests,
      QuantaleSpec.tests,
      SparseVecSpec.tests
    ]
