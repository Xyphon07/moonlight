module QuantaleSpec
  ( tests,
  )
where

import Data.List (maximumBy)
import Data.Ratio ((%))
import Hedgehog qualified as HH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    ChainOrder (..),
    ChainQuantale,
    IntegralQuantale,
    JoinSemilattice (..),
    Quantale (..),
    ResiduatedQuantale (..),
    joinLeq,
  )
import Moonlight.Algebra
  ( Lukasiewicz (..),
    Tropical (..),
    Viterbi (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.Hedgehog
  ( testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "quantale"
    [ quantaleLawTests "Bool" Gen.bool,
      quantaleLawTests "Viterbi Rational" genViterbiRational,
      quantaleLawTests "Lukasiewicz Rational" genLukasiewiczRational,
      quantaleLawTests "Tropical Rational" genTropicalRational,
      quantaleLawTests "(Bool, Viterbi Rational)" genBoolViterbiPair,
      testGroup
        "integral markers"
        [ integralQuantaleLawTests "Bool" Gen.bool,
          integralQuantaleLawTests "Viterbi Rational" genViterbiRational,
          integralQuantaleLawTests "Lukasiewicz Rational" genLukasiewiczRational,
          integralQuantaleLawTests "(Bool, Viterbi Rational)" genBoolViterbiPair
        ],
      testGroup
        "chain markers"
        [ chainQuantaleLawTests "Bool" Gen.bool,
          chainQuantaleLawTests "Viterbi Rational" genViterbiRational,
          chainQuantaleLawTests "Lukasiewicz Rational" genLukasiewiczRational,
          chainQuantaleLawTests "Tropical Rational (signed)" genTropicalSignedRational
        ],
      testGroup
        "the chain order induced by a selective join"
        [ chainOrderLawTests "Viterbi Rational" genViterbiRational,
          chainOrderLawTests "Lukasiewicz Rational" genLukasiewiczRational,
          chainOrderLawTests "Tropical Rational (signed)" genTropicalSignedRational
        ]
    ]

quantaleLawTests ::
  (ResiduatedQuantale carrier, Eq carrier, Show carrier) =>
  String ->
  HH.Gen carrier ->
  TestTree
quantaleLawTests label gen =
  testGroup
    label
    [ testProperty "tensor is associative" $ HH.property $ do
        left <- HH.forAll gen
        middle <- HH.forAll gen
        right <- HH.forAll gen
        tensor (tensor left middle) right HH.=== tensor left (tensor middle right),
      testProperty "tensorUnit is a left identity" $ HH.property $ do
        value <- HH.forAll gen
        tensor tensorUnit value HH.=== value,
      testProperty "tensorUnit is a right identity" $ HH.property $ do
        value <- HH.forAll gen
        tensor value tensorUnit HH.=== value,
      testProperty "tensor is commutative" $ HH.property $ do
        left <- HH.forAll gen
        right <- HH.forAll gen
        tensor left right HH.=== tensor right left,
      testProperty "tensor distributes over join on the left" $ HH.property $ do
        left <- HH.forAll gen
        middle <- HH.forAll gen
        right <- HH.forAll gen
        tensor left (join middle right)
          HH.=== join (tensor left middle) (tensor left right),
      testProperty "tensor distributes over join on the right" $ HH.property $ do
        left <- HH.forAll gen
        middle <- HH.forAll gen
        right <- HH.forAll gen
        tensor (join left middle) right
          HH.=== join (tensor left right) (tensor middle right),
      testProperty "bottom annihilates tensor on the left" $ HH.property $ do
        value <- HH.forAll gen
        tensor bottom value HH.=== bottom,
      testProperty "bottom annihilates tensor on the right" $ HH.property $ do
        value <- HH.forAll gen
        tensor value bottom HH.=== bottom,
      testProperty "residual is right adjoint to tensor" $ HH.property $ do
        left <- HH.forAll gen
        middle <- HH.forAll gen
        right <- HH.forAll gen
        joinLeq (tensor left middle) right HH.=== joinLeq left (residual middle right),
      testProperty "tensorUnit is the greatest element" $ HH.property $ do
        value <- HH.forAll gen
        HH.assert (joinLeq value tensorUnit)
    ]

integralQuantaleLawTests ::
  (IntegralQuantale carrier, Eq carrier, Show carrier) =>
  String ->
  HH.Gen carrier ->
  TestTree
integralQuantaleLawTests label gen =
  testProperty (label <> ": tensor is two-sided deflationary") $ HH.property $ do
    left <- HH.forAll gen
    right <- HH.forAll gen
    HH.assert (joinLeq (tensor left right) left)
    HH.assert (joinLeq (tensor left right) right)

chainQuantaleLawTests ::
  (ChainQuantale carrier, Eq carrier, Show carrier) =>
  String ->
  HH.Gen carrier ->
  TestTree
chainQuantaleLawTests label gen =
  testProperty (label <> ": join is selective") $ HH.property $ do
    left <- HH.forAll gen
    right <- HH.forAll gen
    HH.assert (join left right == left || join left right == right)

chainOrderLawTests ::
  (ChainQuantale carrier, Eq carrier, Show carrier) =>
  String ->
  HH.Gen carrier ->
  TestTree
chainOrderLawTests label gen =
  testGroup
    label
    [ testProperty "strict ascent is the lattice order minus equality" $ HH.property $ do
        left <- HH.forAll gen
        right <- HH.forAll gen
        (compare (ChainOrder left) (ChainOrder right) == LT)
          HH.=== (joinLeq left right && left /= right),
      testProperty "the order is antisymmetric" $ HH.property $ do
        left <- HH.forAll gen
        right <- HH.forAll gen
        compare (ChainOrder left) (ChainOrder left) HH.=== EQ
        (chainOrderLeq left right && chainOrderLeq right left)
          HH.=== (left == right),
      testProperty "the order is transitive" $ HH.property $ do
        left <- HH.forAll gen
        middle <- HH.forAll gen
        right <- HH.forAll gen
        let triple = [left, middle, right]
        HH.assert
          ( and
              [ not (chainOrderLeq lower between && chainOrderLeq between upper)
                  || chainOrderLeq lower upper
              | lower <- triple,
                between <- triple,
                upper <- triple
              ]
          ),
      testProperty "the greatest element under the order attains the join" $ HH.property $ do
        values <- HH.forAll (Gen.list (Range.linear 1 12) gen)
        getChainOrder (maximumBy compare (map ChainOrder values))
          HH.=== foldr1 join values
    ]
  where
    chainOrderLeq :: (ChainQuantale value, Eq value) => value -> value -> Bool
    chainOrderLeq lower upper =
      compare (ChainOrder lower) (ChainOrder upper) /= GT

genUnitIntervalRational :: HH.Gen Rational
genUnitIntervalRational = do
  denominator <- Gen.integral (Range.linear 1 12)
  numerator <- Gen.integral (Range.linear 0 denominator)
  pure (numerator % denominator)

genViterbiRational :: HH.Gen (Viterbi Rational)
genViterbiRational =
  Viterbi <$> genUnitIntervalRational

genLukasiewiczRational :: HH.Gen (Lukasiewicz Rational)
genLukasiewiczRational =
  Lukasiewicz <$> genUnitIntervalRational

genTropicalRational :: HH.Gen (Tropical Rational)
genTropicalRational =
  Gen.frequency
    [ (1, pure TropicalInfinity),
      (6, TropicalFinite <$> genNonnegativeRational)
    ]

genTropicalSignedRational :: HH.Gen (Tropical Rational)
genTropicalSignedRational =
  Gen.frequency
    [ (1, pure TropicalInfinity),
      (6, TropicalFinite <$> genSignedRational)
    ]

genSignedRational :: HH.Gen Rational
genSignedRational = do
  denominator <- Gen.integral (Range.linear 1 12)
  numerator <- Gen.integral (Range.linearFrom 0 (-12 * denominator) (12 * denominator))
  pure (numerator % denominator)

genNonnegativeRational :: HH.Gen Rational
genNonnegativeRational = do
  denominator <- Gen.integral (Range.linear 1 12)
  numerator <- Gen.integral (Range.linear 0 (12 * denominator))
  pure (numerator % denominator)

genBoolViterbiPair :: HH.Gen (Bool, Viterbi Rational)
genBoolViterbiPair =
  (,) <$> Gen.bool <*> genViterbiRational
