{-# LANGUAGE DerivingStrategies #-}

module SuiteSpec
  ( tests,
  )
where

import Moonlight.Core (IsLawName (..))
import Moonlight.Pale.Test.Laws.Suite
  ( hUnitLaw,
    lawGroup,
    namedHedgehogLaw,
    namedQuickCheckLaw,
    renderLawSuite,
    testTreeLaw,
  )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), testCase)

data SuiteLawName
  = QuickCheckIdentity
  | HedgehogIdentity
  deriving stock (Eq, Ord, Show)

instance IsLawName SuiteLawName where
  lawNameText lawName =
    case lawName of
      QuickCheckIdentity -> "quickcheck_identity"
      HedgehogIdentity -> "hedgehog_identity"

tests :: TestTree
tests =
  renderLawSuite
    ( lawGroup
        "Moonlight.Pale.Test.Laws.Suite"
        [ namedQuickCheckLaw QuickCheckIdentity (\value -> not (not value) == (value :: Bool)),
          namedHedgehogLaw HedgehogIdentity (pure True) id,
          hUnitLaw "hunit leaf" (True @?= True),
          testTreeLaw (testCase "embedded test leaf" (True @?= True)),
          lawGroup "nested group" [hUnitLaw "nested leaf" (True @?= True)]
        ]
    )
