{-# LANGUAGE LambdaCase #-}

module AutomataBench
  ( automataBenchmarkPreflight,
    automataBenchmarks,
  )
where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Data.Fix (Fix (..), foldFix)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import BenchSupport (mixSemanticDigest)
import Moonlight.Core.Identifier.EGraph (mkPatternVar, patternVarKey)
import Moonlight.Core.Language
  ( ZipMatch (..),
    zipSameNodeShape,
  )
import Moonlight.Core.Pattern (Pattern (..))
import Moonlight.Core.Pattern.Automata
  ( PatternAutomaton,
    compileConjunctivePatternAutomaton,
    compilePatternAutomaton,
    matchPatternAutomaton,
    matchesPatternAutomaton,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )
import Prelude

data AutomataNode child
  = AutomataLeaf !Int
  | AutomataWrap !child
  | AutomataPair !child !child
  | AutomataBranch ![child]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance ZipMatch AutomataNode where
  zipMatch =
    zipSameNodeShape

automataBenchmarks :: Benchmark
automataBenchmarks =
  bgroup
    "automata"
    ( (automataSizes >>= automataBenchmarksForSize)
        <> [ env prepareNonlinearMatcherFixture $ \fixture ->
               bench
                 "nonlinear-binding/repeated-subterm-nodes=4096"
                 (nf nonlinearPatternBindingWeight fixture)
           ]
    )

automataBenchmarkPreflight :: IO ()
automataBenchmarkPreflight =
  traverse_ automataSizePreflight automataSizes
    *> prepareNonlinearMatcherFixture
    >>= \fixture ->
      expectEqual
        "nonlinear repeated subterm node count"
        4096
        (automataTermNodeCount nonlinearRepeatedSubterm)
        *> expectEqual
          "nonlinear repeated subterm match"
          (Just 1)
          (nonlinearPatternBindingWeight fixture)

automataSizes :: [Int]
automataSizes =
  [4, 16, 48]

automataSizePreflight :: Int -> IO ()
automataSizePreflight size =
  traverse_
    (\(label, result) -> expectCompiledMatch (caseLabel label size) result)
    [ ("compile+match", automataWeight compilePatternAutomaton size),
      ("conjunctive compile+match", automataWeight compileConjunctivePatternAutomaton' size)
    ]

expectCompiledMatch :: String -> Maybe Int -> IO ()
expectCompiledMatch label result =
  case result of
    Nothing ->
      fail (label <> " failed")
    Just weight ->
      () <$ evaluate (force weight)

automataBenchmarksForSize :: Int -> [Benchmark]
automataBenchmarksForSize size =
  [ bench (caseLabel "compile+match" size) (nf (automataWeight compilePatternAutomaton) size),
    bench (caseLabel "conjunctive compile+match" size) (nf (automataWeight compileConjunctivePatternAutomaton') size)
  ]

automataWeight :: (Pattern AutomataNode -> PatternAutomaton AutomataNode) -> Int -> Maybe Int
automataWeight compile size =
  if matchesPatternAutomaton (compile (benchPattern size)) (benchFixTerm size)
    then Just 1
    else Nothing

compileConjunctivePatternAutomaton' :: Pattern AutomataNode -> PatternAutomaton AutomataNode
compileConjunctivePatternAutomaton' patternValue =
  compileConjunctivePatternAutomaton (patternValue :| [patternValue])

benchPattern :: Int -> Pattern AutomataNode
benchPattern =
  fixPattern . benchFixTerm

fixPattern :: Fix AutomataNode -> Pattern AutomataNode
fixPattern (Fix node) =
  PatternNode (fmap fixPattern node)

benchFixTerm :: Int -> Fix AutomataNode
benchFixTerm size =
  foldr
    (\key accumulated -> Fix (AutomataPair (Fix (AutomataWrap accumulated)) (Fix (AutomataLeaf key))))
    (Fix (AutomataLeaf 0))
    [1 .. max 0 size]

data NonlinearMatcherFixture = NonlinearMatcherFixture
  !(Pattern AutomataNode)
  !(Fix AutomataNode)

instance NFData NonlinearMatcherFixture where
  rnf fixture =
    nonlinearMatcherInputDigest fixture `seq` ()

prepareNonlinearMatcherFixture :: IO NonlinearMatcherFixture
prepareNonlinearMatcherFixture =
  nonlinearMatcherFixture
    <$ evaluate (force (nonlinearMatcherInputDigest nonlinearMatcherFixture))

nonlinearMatcherFixture :: NonlinearMatcherFixture
nonlinearMatcherFixture =
  NonlinearMatcherFixture
    ( PatternNode
      ( AutomataBranch
          [ PatternVar repeatedPatternVariable,
            PatternVar repeatedPatternVariable
          ]
      )
    )
    ( Fix
      ( AutomataBranch
          [ nonlinearRepeatedSubterm,
            nonlinearRepeatedSubterm
          ]
      )
    )
  where
    repeatedPatternVariable = mkPatternVar 0

nonlinearRepeatedSubterm :: Fix AutomataNode
nonlinearRepeatedSubterm =
  Fix (AutomataWrap nonlinearBranchSpine)

nonlinearBranchSpine :: Fix AutomataNode
nonlinearBranchSpine =
  foldl'
    (\child nodeKey -> Fix (AutomataBranch [Fix (AutomataLeaf nodeKey), child]))
    (Fix (AutomataLeaf 0))
    [1 .. 2047]

nonlinearPatternBindingWeight :: NonlinearMatcherFixture -> Maybe Int
nonlinearPatternBindingWeight (NonlinearMatcherFixture patternValue termValue) =
  IntMap.size
    <$> matchPatternAutomaton
      (compilePatternAutomaton patternValue)
      termValue
      IntMap.empty

nonlinearMatcherInputDigest :: NonlinearMatcherFixture -> Int
nonlinearMatcherInputDigest (NonlinearMatcherFixture patternValue termValue) =
  mixSemanticDigest
    (automataPatternDigest patternValue)
    (automataTermDigest termValue)

automataPatternDigest :: Pattern AutomataNode -> Int
automataPatternDigest = \case
  PatternVar patternVariable ->
    mixSemanticDigest 41 (patternVarKey patternVariable)
  PatternNode node ->
    automataNodeDigest (fmap automataPatternDigest node)

automataTermDigest :: Fix AutomataNode -> Int
automataTermDigest =
  foldFix automataNodeDigest

automataTermNodeCount :: Fix AutomataNode -> Int
automataTermNodeCount =
  foldFix (automataNodeFold (const 1) ((1 +) . sum))

automataNodeDigest :: AutomataNode Int -> Int
automataNodeDigest =
  automataNodeFold (mixSemanticDigest 31) (foldl' mixSemanticDigest 37)

automataNodeFold :: (Int -> value) -> ([value] -> value) -> AutomataNode value -> value
automataNodeFold onLeaf onChildren = \case
  AutomataLeaf key ->
    onLeaf key
  AutomataWrap child ->
    onChildren [child]
  AutomataPair leftChild rightChild ->
    onChildren [leftChild, rightChild]
  AutomataBranch children ->
    onChildren children

expectEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
expectEqual label expected actual =
  if actual == expected
    then pure ()
    else fail (label <> " expected " <> show expected <> " but observed " <> show actual)

caseLabel :: String -> Int -> String
caseLabel label size =
  label <> "/" <> show size
