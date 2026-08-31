{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module PolynomialFunctorWitnessSpec
  ( tests,
  )
where

import Moonlight.Category
  ( Direction,
    Exists (..),
    ParameterizedDirection,
    ParameterizedPolynomialFunctor (..),
    PolynomialFunctor (..),
  )
import Moonlight.Category.Test.PolynomialFixture
  ( BranchPosition,
    DemoParameterizedPolynomial,
    DemoPolynomial,
    FullSliceBranchPosition,
    FullSliceRootPosition,
    RootPosition,
    TrimmedSliceRootPosition,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

rootDirectionWitness :: Direction DemoPolynomial RootPosition
rootDirectionWitness = True

branchDirectionWitness :: Direction DemoPolynomial BranchPosition
branchDirectionWitness = Just False

fullSliceRootDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial FullSliceRootPosition
fullSliceRootDirectionWitness = True

fullSliceBranchDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial FullSliceBranchPosition
fullSliceBranchDirectionWitness = Just True

trimmedSliceRootDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial TrimmedSliceRootPosition
trimmedSliceRootDirectionWitness = ()

tests :: TestTree
tests =
  testGroup
    "PolynomialFunctor"
    [ testGroup
        "closed witness families"
        [ testCase "position witnesses enumerate the polynomial support" $
            length demoPositions @?= 2,
          testCase "root positions admit the indexed direction carrier" $
            rootDirectionWitness @?= True,
          testCase "branch positions admit a distinct indexed direction carrier" $
            branchDirectionWitness @?= Just False
        ],
      testGroup
        "parameterized witness families"
        [ testCase "positionsAt enumerates the requested slice" $
            ( length (demoParameterizedPositions True),
              length (demoParameterizedPositions False)
            )
              @?= (2, 1),
          testCase "parameterized positions admit slice-specific directions" $
            ( fullSliceRootDirectionWitness,
              fullSliceBranchDirectionWitness,
              trimmedSliceRootDirectionWitness
            )
              @?= (True, Just True, ())
        ]
    ]

demoPositions :: [Exists (Position DemoPolynomial)]
demoPositions = allPositions

demoParameterizedPositions :: Bool -> [Exists (ParameterizedPosition DemoParameterizedPolynomial)]
demoParameterizedPositions = positionsAt
