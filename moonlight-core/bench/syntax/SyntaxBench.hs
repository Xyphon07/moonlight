{-# LANGUAGE DeriveTraversable #-}

module SyntaxBench
  ( syntaxBenchmarkPreflight,
    syntaxBenchmarks,
  )
where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Fix (Fix (..), foldFix)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import BenchSupport
  ( caseLabel,
    keys,
    mixSemanticDigest,
    syntaxSizes,
  )
import Moonlight.Core.Identifier.EGraph
  ( ClassId (..),
    mkPatternVar,
    patternVarKey,
  )
import Moonlight.Core.Language
  ( ZipMatch (..),
    zipSameNodeShape,
  )
import Moonlight.Core.Pattern
  ( Pattern (..),
    patternVariables,
  )
import Moonlight.Core.Pattern.AntiUnify
  ( BinaryLGGResult (..),
    NaryLGGResult (..),
    antiUnifyAllTerms,
    antiUnifyTerms,
  )
import Moonlight.Core.Substitution
  ( Substitution (..),
    emptySubstitution,
    insertSubst,
    mergeSubstitutions,
  )
import Moonlight.Core.Theory
  ( StructuralLaw (..),
    TheorySpec (..),
    commutativeBinary,
    expandPatternByTheory,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )
import Prelude

data BenchNode a
  = BenchLeaf !Int
  | BenchUnary !a
  | BenchBinary !a !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance ZipMatch BenchNode where
  zipMatch =
    zipSameNodeShape

data WideNode child
  = WideLeaf !Int
  | WideBranch ![child]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance ZipMatch WideNode where
  zipMatch =
    zipSameNodeShape

newtype WideAntiUnificationFixture = WideAntiUnificationFixture
  { wideAntiUnificationTerms :: NonEmpty (Fix WideNode)
  }

instance NFData WideAntiUnificationFixture where
  rnf fixture =
    wideTermsInputDigest (wideAntiUnificationTerms fixture) `seq` ()

syntaxBenchmarks :: Benchmark
syntaxBenchmarks =
  bgroup
    "syntax"
    ( (syntaxSizes >>= syntaxBenchmarksForSize)
        <> [ env prepareWideAntiUnificationFixture $ \fixture ->
               bench
                 "anti-unify/nary lgg arity=512 terms=16"
                 (nf wideNaryAntiUnificationWeight fixture)
           ]
    )

syntaxBenchmarkPreflight :: IO ()
syntaxBenchmarkPreflight =
  traverse_
    (\size -> evaluateForced (binaryAntiUnificationWeight size, naryAntiUnificationWeight size))
    syntaxSizes
    *> (prepareWideAntiUnificationFixture >>= evaluateForced . wideNaryAntiUnificationWeight)

evaluateForced :: NFData value => value -> IO ()
evaluateForced value =
  () <$ evaluate (force value)

syntaxBenchmarksForSize :: Int -> [Benchmark]
syntaxBenchmarksForSize size =
  [ bench (caseLabel "pattern variables" size) (nf patternVariableWeight size),
    bench (caseLabel "commutative expansion" size) (nf theoryExpansionWeight size),
    bench (caseLabel "substitution incremental merge" size) (nf substitutionMergeWeight size),
    bench (caseLabel "substitution pair merge" size) (nf substitutionPairMergeWeight size),
    bench (caseLabel "hackage: containers IntMap checked union" size) (nf hackageContainersCheckedUnionWeight size),
    bench (caseLabel "same-shape zip" size) (nf shapeZipWeight size),
    bench (caseLabel "anti-unify/binary lgg" size) (nf binaryAntiUnificationWeight size),
    bench (caseLabel "anti-unify/nary lgg" size) (nf naryAntiUnificationWeight size)
  ]

binaryAntiUnificationWeight :: Int -> Int
binaryAntiUnificationWeight size =
  binaryLggSharedStructure
    (antiUnifyTerms (benchFixTerm size) (benchVariantFixTerm size))

naryAntiUnificationWeight :: Int -> Int
naryAntiUnificationWeight size =
  naryLggSharedStructure
    ( antiUnifyAllTerms
        (benchFixTerm size :| [benchVariantFixTerm size, benchFixTerm (size + 1)])
    )

prepareWideAntiUnificationFixture :: IO WideAntiUnificationFixture
prepareWideAntiUnificationFixture =
  pure (WideAntiUnificationFixture (wideAntiUnificationTermsForSeed 0))

wideNaryAntiUnificationWeight :: WideAntiUnificationFixture -> Int
wideNaryAntiUnificationWeight fixture =
  let result = antiUnifyAllTerms (wideAntiUnificationTerms fixture)
   in foldl'
        mixSemanticDigest
        ( mixSemanticDigest
            (naryLggSharedStructure result)
            (widePatternDigest (naryLggPattern result))
        )
        (fmap wideBindingRowDigest (naryLggBindings result))

wideTermsInputDigest :: NonEmpty (Fix WideNode) -> Int
wideTermsInputDigest =
  foldl' (\digest term -> mixSemanticDigest digest (wideTermDigest term)) 29

widePatternDigest :: Pattern WideNode -> Int
widePatternDigest = \case
  PatternVar patternVariable ->
    mixSemanticDigest 41 (patternVarKey patternVariable)
  PatternNode node ->
    wideNodeDigest (fmap widePatternDigest node)

wideBindingRowDigest :: IntMap (Fix WideNode) -> Int
wideBindingRowDigest =
  IntMap.foldlWithKey'
    (\digest key term -> mixSemanticDigest (mixSemanticDigest digest key) (wideTermDigest term))
    43

wideTermDigest :: Fix WideNode -> Int
wideTermDigest =
  foldFix wideNodeDigest

wideNodeDigest :: WideNode Int -> Int
wideNodeDigest = \case
  WideLeaf key ->
    mixSemanticDigest 31 key
  WideBranch childDigests ->
    foldl' mixSemanticDigest 37 childDigests

wideAntiUnificationTermsForSeed :: Int -> NonEmpty (Fix WideNode)
wideAntiUnificationTermsForSeed sampleSeed =
  wideTerm sampleSeed 0 :| fmap (wideTerm sampleSeed) [1 .. 15]

wideTerm :: Int -> Int -> Fix WideNode
wideTerm sampleSeed termOffset =
  Fix
    ( WideBranch
        [ Fix (WideLeaf (sampleSeed + termOffset + childIndex))
          | childIndex <- [0 .. 511]
        ]
    )

benchFixTerm :: Int -> Fix BenchNode
benchFixTerm size =
  foldr
    (\key accumulated -> Fix (BenchBinary (Fix (BenchLeaf key)) (Fix (BenchUnary accumulated))))
    (Fix (BenchLeaf 0))
    [1 .. max 0 size]

benchVariantFixTerm :: Int -> Fix BenchNode
benchVariantFixTerm size =
  foldr
    (\key accumulated -> Fix (BenchBinary (Fix (BenchLeaf (key + 1))) (Fix (BenchUnary accumulated))))
    (Fix (BenchLeaf 1))
    [1 .. max 0 size]

patternVariableWeight :: Int -> Int
patternVariableWeight =
  Set.size . patternVariables . benchPattern

theoryExpansionWeight :: Int -> Int
theoryExpansionWeight =
  sum . fmap (length . expandPatternByTheory commutativeTheory . binaryPatternForKey) . keys

substitutionMergeWeight :: Int -> Int
substitutionMergeWeight size =
  case foldl' mergeStep (Just emptySubstitution) (keys size) of
    Nothing -> 0
    Just substitution -> substitutionSize substitution

substitutionPairMergeWeight :: Int -> Int
substitutionPairMergeWeight size =
  case mergeSubstitutions (benchSubstitution size) (benchSubstitution size) of
    Nothing -> 0
    Just substitution -> substitutionSize substitution

hackageContainersCheckedUnionWeight :: Int -> Int
hackageContainersCheckedUnionWeight size =
  maybe 0 IntMap.size (checkedIntMapUnion (benchSubstitutionMap size) (benchSubstitutionMap size))

checkedIntMapUnion :: IntMap ClassId -> IntMap ClassId -> Maybe (IntMap ClassId)
checkedIntMapUnion left right =
  if IntMap.null conflicts
    then Just (IntMap.union left right)
    else Nothing
  where
    conflicts =
      IntMap.filter (uncurry (/=)) (IntMap.intersectionWith (,) left right)

shapeZipWeight :: Int -> Int
shapeZipWeight size =
  case zipSameNodeShape (benchNode size) (benchNode (size + 1)) of
    Nothing -> 0
    Just zipped -> length zipped

benchPattern :: Int -> Pattern BenchNode
benchPattern size =
  foldr
    (\key rest -> PatternNode (BenchBinary (PatternVar (mkPatternVar key)) rest))
    (PatternNode (BenchLeaf 0))
    (keys size)

binaryPatternForKey :: Int -> Pattern BenchNode
binaryPatternForKey key =
  PatternNode
    ( BenchBinary
        (PatternVar (mkPatternVar key))
        (PatternNode (BenchLeaf key))
    )

benchNode :: Int -> BenchNode Int
benchNode size =
  BenchBinary (size + 1) (size + 2)

commutativeTheory :: TheorySpec BenchNode
commutativeTheory =
  TheorySpec {tsClassify = classify}
  where
    classify node =
      case node of
        BenchBinary _left _right -> commutativeBinary BenchBinary
        _ -> Ordinary

    classify :: BenchNode value -> StructuralLaw BenchNode value

mergeStep :: Maybe Substitution -> Int -> Maybe Substitution
mergeStep maybeSubstitution key =
  maybeSubstitution >>= \substitution ->
    mergeSubstitutions substitution (insertSubst (mkPatternVar key) (ClassId key) emptySubstitution)

benchSubstitution :: Int -> Substitution
benchSubstitution =
  Substitution . benchSubstitutionMap

benchSubstitutionMap :: Int -> IntMap ClassId
benchSubstitutionMap size =
  IntMap.fromAscList
    [ (key, ClassId key)
    | key <- keys size
    ]

substitutionSize :: Substitution -> Int
substitutionSize (Substitution entries) =
  IntMap.size entries
