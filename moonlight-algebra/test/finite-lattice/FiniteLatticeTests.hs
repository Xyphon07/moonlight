module FiniteLatticeTests
  ( tests,
  )
where

import FiniteLatticeSpec qualified
import LawSpec qualified
import PresentationSpec qualified
import Test.Tasty
  ( TestTree,
    testGroup,
  )

tests :: TestTree
tests =
  testGroup
    "moonlight-algebra:finite-lattice"
    [ FiniteLatticeSpec.tests,
      LawSpec.tests,
      PresentationSpec.tests
    ]
