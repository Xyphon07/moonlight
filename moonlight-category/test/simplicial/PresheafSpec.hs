module PresheafSpec
  ( tests,
  )
where

import Numeric.Natural (Natural)
import Moonlight.Category.Simplicial (allDeltaMorphisms)
import Moonlight.Category.Simplicial
  ( presheafObjectMap,
    generatedAsPresheaf,
    presheafCompositionLaw,
    presheafIdentityLaw,
  )
import Moonlight.Category.Simplicial (standardSimplexGenerated)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

identityLawsHoldForRange :: Natural -> Bool
identityLawsHoldForRange upperBound =
  let generatedPresheaf = generatedAsPresheaf (standardSimplexGenerated 3 upperBound)
   in and
        [ presheafIdentityLaw generatedPresheaf dimensionValue
          | dimensionValue <- [0 .. upperBound]
        ]

compositionLawsHoldForRange :: Natural -> Bool
compositionLawsHoldForRange upperBound =
  let generatedPresheaf = generatedAsPresheaf (standardSimplexGenerated 3 upperBound)
      compositionHoldsFor presheaf =
        and
          [ presheafCompositionLaw presheaf outer inner simplexValue
            | innerDomain <- [0 .. upperBound],
              innerCodomain <- [0 .. upperBound],
              outerCodomain <- [0 .. upperBound],
              inner <- allDeltaMorphisms innerDomain innerCodomain,
              outer <- allDeltaMorphisms innerCodomain outerCodomain,
              simplexValue <- presheafObjectMap presheaf outerCodomain
          ]
   in compositionHoldsFor generatedPresheaf

tests :: TestTree
tests =
  testGroup
    "Presheaf"
    [ testCase "generated presheaf satisfies identity" $
        assertBool "identity law failed" (identityLawsHoldForRange 3),
      testCase "generated presheaf satisfies composition" $
        assertBool "composition law failed" (compositionLawsHoldForRange 3)
    ]
