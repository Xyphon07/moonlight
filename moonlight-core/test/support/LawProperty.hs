module LawProperty
  ( lawProperty,
  )
where

import Moonlight.Core (IsLawName (..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Testable, testProperty)

lawProperty :: (IsLawName law, Testable property) => law -> property -> TestTree
lawProperty lawName =
  testProperty (lawNameText lawName)
