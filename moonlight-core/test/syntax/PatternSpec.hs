{-# LANGUAGE DerivingStrategies #-}

module PatternSpec (tests) where

import Data.Kind (Type)
import Data.Foldable (toList)
import Data.List (sort)
import Moonlight.Core (Pattern (..), patternVariables)
import Moonlight.Core qualified as EGraph
import Moonlight.Core (IsLawName (..), constructorLawName)
import LawProperty (lawProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Gen, frequency, listOf, resize, sized)

data PatternLaw
  = PatternCataAfterAnaIdentity
  | PatternInterpreterCoherence
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName PatternLaw where
  lawNameText =
    constructorLawName . show

cataAfterAnaIdentity :: Eq seed => (seed -> recursive) -> (recursive -> seed) -> seed -> Bool
cataAfterAnaIdentity anamorphism catamorphism seed = catamorphism (anamorphism seed) == seed

interpreterCoherence :: Eq value => (seed -> recursive) -> (recursive -> value) -> (seed -> value) -> seed -> Bool
interpreterCoherence anamorphism interpretation seedInterpreter seed = seedInterpreter seed == interpretation (anamorphism seed)

type PatternSeed :: Type
data PatternSeed
  = PatternSeedVar EGraph.PatternVar
  | PatternSeedNode [PatternSeed]
  deriving stock (Eq, Show)

patternFromSeed :: PatternSeed -> Pattern []
patternFromSeed seed =
  case seed of
    PatternSeedVar patternVar ->
      PatternVar patternVar
    PatternSeedNode children ->
      PatternNode (map patternFromSeed children)

patternSeedFromPattern :: Pattern [] -> PatternSeed
patternSeedFromPattern patternValue =
  case patternValue of
    PatternVar patternVar ->
      PatternSeedVar patternVar
    PatternNode children ->
      PatternSeedNode (map patternSeedFromPattern children)

canonicalPatternVars :: [EGraph.PatternVar] -> [EGraph.PatternVar]
canonicalPatternVars =
  foldr
    ( \patternVar accumulatedPatternVars ->
        case accumulatedPatternVars of
          [] ->
            [patternVar]
          accumulatedHead : _ ->
            if patternVar == accumulatedHead
              then accumulatedPatternVars
              else patternVar : accumulatedPatternVars
    )
    []
    . sort

patternSeedVariables :: PatternSeed -> [EGraph.PatternVar]
patternSeedVariables seed =
  case seed of
    PatternSeedVar patternVar ->
      [patternVar]
    PatternSeedNode children ->
      canonicalPatternVars (foldMap patternSeedVariables children)

patternVariablesCanonical :: Pattern [] -> [EGraph.PatternVar]
patternVariablesCanonical =
  toList . patternVariables

samplePatternSeeds :: [PatternSeed]
samplePatternSeeds =
  [ PatternSeedVar (EGraph.mkPatternVar 1),
    PatternSeedNode [],
    PatternSeedNode [PatternSeedVar (EGraph.mkPatternVar 2), PatternSeedVar (EGraph.mkPatternVar 3)],
    PatternSeedNode
      [ PatternSeedVar (EGraph.mkPatternVar 4),
        PatternSeedNode [PatternSeedVar (EGraph.mkPatternVar 4), PatternSeedVar (EGraph.mkPatternVar 5)]
      ]
  ]

patternSeedAtomGen :: Gen PatternSeed
patternSeedAtomGen =
  PatternSeedVar . EGraph.mkPatternVar <$> arbitrary

patternSeedGenSized :: Int -> Gen PatternSeed
patternSeedGenSized size =
  case size of
    0 ->
      patternSeedAtomGen
    _ ->
      frequency
        [ (2, patternSeedAtomGen),
          (1, PatternSeedNode <$> childSeedListGen)
        ]
  where
    childSeedSize = size `div` 2
    childSeedListGen =
      resize childSeedSize (listOf (patternSeedGenSized childSeedSize))

instance Arbitrary PatternSeed where
  arbitrary =
    sized patternSeedGenSized

tests :: TestTree
tests =
  testGroup
    "Pattern"
    [ testCase "patternVariables collects variables from sample recursive patterns" $
        patternVariablesCanonical
          ( patternFromSeed
              ( PatternSeedNode
                  [ PatternSeedVar (EGraph.mkPatternVar 1),
                    PatternSeedNode [PatternSeedVar (EGraph.mkPatternVar 2), PatternSeedVar (EGraph.mkPatternVar 1)]
                  ]
              )
          )
          @?= [EGraph.mkPatternVar 1, EGraph.mkPatternVar 2],
      testCase "pattern ordering follows constructor declaration order" $
        compare (PatternVar (EGraph.mkPatternVar 0) :: Pattern []) (PatternNode [])
          @?= LT,
      testCase "pattern seed reconstruction holds across sample recursive patterns" $
        map (cataAfterAnaIdentity patternFromSeed patternSeedFromPattern) samplePatternSeeds
          @?= [True, True, True, True],
      testCase "pattern variable interpretation is coherent across sample recursive patterns" $
        map
          (interpreterCoherence patternFromSeed patternVariablesCanonical patternSeedVariables)
          samplePatternSeeds
          @?= [True, True, True, True],
      lawProperty PatternCataAfterAnaIdentity $
        cataAfterAnaIdentity patternFromSeed patternSeedFromPattern,
      lawProperty PatternInterpreterCoherence $
        interpreterCoherence patternFromSeed patternVariablesCanonical patternSeedVariables
    ]
