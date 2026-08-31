{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Moonlight.Algebra.Effect.Laws
  ( tests,
    testsWithConfig,
  )
where

import Prelude hiding (gcd)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as UVector
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import Moonlight.Algebra
import Moonlight.Algebra.Effect.LawNames (CommonLawName (..), LawName (..))
import Moonlight.Algebra.Test.Generators
  ( AlgebraGeneratorConfig,
    defaultAlgebraGeneratorConfig,
    genBatch,
    genFreeAbelianGroup,
    genIntBasis,
    genIntegerCoefficient,
    genIntegerLawValue,
    genLaneVector,
    genModulus,
    genOrientation,
    genPolynomial,
    genPowerSet,
    genSparseVec,
    genZn,
  )
import Data.Maybe (fromMaybe)
import Moonlight.Core
  ( FiniteUniverse (..),
    boundedEnumUniverse,
    sub,
  )
import Moonlight.Core qualified as Core
import qualified Moonlight.Pale.Test.Laws.Algebraic as Algebraic
import Moonlight.Pale.Test.Laws.Suite
  ( LawSuite,
    lawGroup,
    namedHedgehogLaw,
    renderLawSuite,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

type Atom :: Type
data Atom = Alpha | Beta | Gamma
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FiniteUniverse Atom where
  finiteUniverse =
    boundedEnumUniverse

type NonZeroGcdContext :: Type
data NonZeroGcdContext = NonZeroGcdContext
  { nonZeroGcdLeft :: Integer,
    nonZeroGcdRight :: Integer
  }
  deriving stock (Show)

type ModInverseContext :: Type
data ModInverseContext = ModInverseContext
  { modInverseCandidate :: Integer,
    modInverseModulus :: Integer
  }
  deriving stock (Show)

type CrtSoundContext :: Type
data CrtSoundContext = CrtSoundContext
  { crtLeftResidue :: Integer,
    crtLeftModulus :: Integer,
    crtRightResidue :: Integer,
    crtRightModulus :: Integer
  }
  deriving stock (Show)

type SomeNonZeroModulus :: Type -> Type
data SomeNonZeroModulus a where
  SomeNonZeroModulus :: NonZeroModulus modulus a -> SomeNonZeroModulus a

type ResidueView :: Type -> Type
data ResidueView a = ResidueView
  { residueModulusValue :: a,
    residueCandidateValue :: a
  }

genAtom :: HH.Gen Atom
genAtom =
  Gen.element [Alpha, Beta, Gamma]

genPair :: HH.Gen a -> HH.Gen b -> HH.Gen (a, b)
genPair genLeft genRight =
  (,) <$> genLeft <*> genRight

genPairOf :: HH.Gen a -> HH.Gen (a, a)
genPairOf genValue =
  genPair genValue genValue

genTriple :: HH.Gen a -> HH.Gen b -> HH.Gen c -> HH.Gen (a, b, c)
genTriple genLeft genMiddle genRight =
  (,,) <$> genLeft <*> genMiddle <*> genRight

genTripleOf :: HH.Gen a -> HH.Gen (a, a, a)
genTripleOf genValue =
  genTriple genValue genValue genValue

genNonZeroGcdContext :: AlgebraGeneratorConfig -> HH.Gen NonZeroGcdContext
genNonZeroGcdContext config =
  Gen.filter ((/= 0) . nonZeroGcdDivisor) $
    NonZeroGcdContext <$> genIntegerLawValue config <*> genIntegerLawValue config

genModInverseContext :: (Integer -> Bool) -> AlgebraGeneratorConfig -> HH.Gen ModInverseContext
genModInverseContext divisorPredicate config =
  Gen.filter (divisorPredicate . modInverseDivisor) $
    ModInverseContext <$> genIntegerLawValue config <*> genModulus config

genCrtSoundContext :: AlgebraGeneratorConfig -> HH.Gen CrtSoundContext
genCrtSoundContext config =
  CrtSoundContext
    <$> genIntegerLawValue config
    <*> genModulus config
    <*> genIntegerLawValue config
    <*> genModulus config

genIntegerUnit :: HH.Gen Integer
genIntegerUnit =
  Gen.element [1, -1]

genIntegerNonUnit :: AlgebraGeneratorConfig -> HH.Gen Integer
genIntegerNonUnit config =
  Gen.filter (not . isUnit) (genIntegerLawValue config)

applyPair :: (a -> b -> result) -> (a, b) -> result
applyPair propertyValue (leftValue, rightValue) =
  propertyValue leftValue rightValue

applyTriple :: (a -> b -> c -> result) -> (a, b, c) -> result
applyTriple propertyValue (leftValue, middleValue, rightValue) =
  propertyValue leftValue middleValue rightValue

nonZeroGcdDivisor :: NonZeroGcdContext -> Integer
nonZeroGcdDivisor context =
  gcd (nonZeroGcdLeft context) (nonZeroGcdRight context)

modInverseDivisor :: ModInverseContext -> Integer
modInverseDivisor context =
  gcd (modInverseCandidate context) (modInverseModulus context)

mkSomeNonZeroModulus :: IntegralDomain a => a -> Maybe (SomeNonZeroModulus a)
mkSomeNonZeroModulus value =
  withNonZeroModulus value SomeNonZeroModulus

canonicalResidueView :: CanonicalResidue modulus a -> ResidueView a
canonicalResidueView residue =
  withCanonicalResidue residue $ \modulus candidate ->
    withNonZeroModulusValue modulus $ \modulusValue ->
      ResidueView modulusValue candidate

monoidAssoc :: Additive (FreeAbelianGroup Int) -> Additive (FreeAbelianGroup Int) -> Additive (FreeAbelianGroup Int) -> Bool
monoidAssoc = Algebraic.monoidAssociativity (<>)

monoidLeftId :: Additive (FreeAbelianGroup Int) -> Bool
monoidLeftId = Algebraic.monoidLeftIdentity (<>) mempty

monoidRightId :: Additive (FreeAbelianGroup Int) -> Bool
monoidRightId = Algebraic.monoidRightIdentity (<>) mempty

groupInvLeft :: Additive (FreeAbelianGroup Int) -> Bool
groupInvLeft = Algebraic.groupLeftInverse (<>) groupInverse mempty

groupInvRight :: Additive (FreeAbelianGroup Int) -> Bool
groupInvRight = Algebraic.groupRightInverse (<>) groupInverse mempty

abelianGroupCommutativity :: (Eq group, AbelianGroup group) => group -> group -> Bool
abelianGroupCommutativity = Algebraic.abelianCommutativity (<>)

abelianComm :: Additive (FreeAbelianGroup Int) -> Additive (FreeAbelianGroup Int) -> Bool
abelianComm = abelianGroupCommutativity

laneVectorMonoidAssoc :: Additive LaneVector -> Additive LaneVector -> Additive LaneVector -> Bool
laneVectorMonoidAssoc = Algebraic.monoidAssociativity (<>)

laneVectorMonoidLeftId :: Additive LaneVector -> Bool
laneVectorMonoidLeftId = Algebraic.monoidLeftIdentity (<>) mempty

laneVectorMonoidRightId :: Additive LaneVector -> Bool
laneVectorMonoidRightId = Algebraic.monoidRightIdentity (<>) mempty

laneVectorGroupInvLeft :: Additive LaneVector -> Bool
laneVectorGroupInvLeft = Algebraic.groupLeftInverse (<>) groupInverse mempty

laneVectorGroupInvRight :: Additive LaneVector -> Bool
laneVectorGroupInvRight = Algebraic.groupRightInverse (<>) groupInverse mempty

laneVectorAbelianComm :: Additive LaneVector -> Additive LaneVector -> Bool
laneVectorAbelianComm = abelianGroupCommutativity

laneVectorSubtraction :: LaneVector -> LaneVector -> Bool
laneVectorSubtraction left right =
  sub left right == add left (Core.neg right)

freeMonoidAssoc :: Batch Int -> Batch Int -> Batch Int -> Bool
freeMonoidAssoc = Algebraic.monoidAssociativity (<>)

freeMonoidLeftId :: Batch Int -> Bool
freeMonoidLeftId = Algebraic.monoidLeftIdentity (<>) mempty

freeMonoidRightId :: Batch Int -> Bool
freeMonoidRightId = Algebraic.monoidRightIdentity (<>) mempty

ringMulComm :: Zn 7 -> Zn 7 -> Bool
ringMulComm = Algebraic.ringMultiplicativeCommutativity

ringAddAssoc :: Zn 7 -> Zn 7 -> Zn 7 -> Bool
ringAddAssoc = Algebraic.ringAdditiveAssociativity

latticeAbsorptionJoin :: PowerSet Atom -> PowerSet Atom -> Bool
latticeAbsorptionJoin = Algebraic.latticeAbsorptionJoin join meet

latticeAbsorptionMeet :: PowerSet Atom -> PowerSet Atom -> Bool
latticeAbsorptionMeet = Algebraic.latticeAbsorptionMeet join meet

heytingImpliesSelfTop :: PowerSet Atom -> Bool
heytingImpliesSelfTop value =
  implies value value == top

heytingMeetImplication :: PowerSet Atom -> PowerSet Atom -> Bool
heytingMeetImplication left right =
  meet left (implies left right) == meet left right

heytingConsequentMeetImplication :: PowerSet Atom -> PowerSet Atom -> Bool
heytingConsequentMeetImplication left right =
  meet right (implies left right) == right

heytingImplicationDistributesMeet :: PowerSet Atom -> PowerSet Atom -> PowerSet Atom -> Bool
heytingImplicationDistributesMeet left middle right =
  implies left (meet middle right) == meet (implies left middle) (implies left right)

heytingNegDefault :: PowerSet Atom -> Bool
heytingNegDefault value =
  neg value == implies value bottom

heytingEquivalenceDefault :: PowerSet Atom -> PowerSet Atom -> Bool
heytingEquivalenceDefault left right =
  (left <=> right) == meet (implies left right) (implies right left)

moduleDistribScalarAdd :: Integer -> Integer -> Polynomial Integer -> Bool
moduleDistribScalarAdd = Algebraic.moduleDistributivityScalar add add scale

moduleDistribVectorAdd :: Integer -> Polynomial Integer -> Polynomial Integer -> Bool
moduleDistribVectorAdd = Algebraic.moduleDistributivityVector add scale

polynomialCanonicalizationIdempotent :: Polynomial Integer -> Bool
polynomialCanonicalizationIdempotent = Algebraic.idempotentLaw normalizePolynomial

polynomialViewsCoherent :: Polynomial Integer -> Bool
polynomialViewsCoherent polynomialValue =
  all denseCoefficientMatches denseCoefficients
    && all supportCoefficientMatches (support @Integer polynomialValue)
  where
    denseCoefficients = zip [0 ..] (toCoefficients polynomialValue)
    coefficientsByDegree = Map.fromList denseCoefficients

    denseCoefficientMatches (degreeValue, coefficientValue) =
      coefficient @Integer degreeValue polynomialValue == coefficientValue

    supportCoefficientMatches degreeValue =
      Map.lookup degreeValue coefficientsByDegree
        == Just (coefficient @Integer degreeValue polynomialValue)

unitInverseOne :: () -> Bool
unitInverseOne () = isUnit (one :: Integer)

unitInverseCorrect :: Integer -> Bool
unitInverseCorrect value =
  case unitInverse value of
    Nothing -> False
    Just inverseValue -> mul value inverseValue == one && mul inverseValue value == one

unitInverseAbsent :: Integer -> Bool
unitInverseAbsent value =
  unitInverse value == Nothing

dividesInteger :: Integer -> Integer -> Bool
dividesInteger divisor value
  | divisor == 0 = value == 0
  | otherwise =
      maybe False ((== 0) . snd) $ do
        divisorRefined <- mkNonZeroDivisor divisor
        pure (divideWithRemainder value divisorRefined)

normalizeInteger :: Integer -> Integer -> Maybe Integer
normalizeInteger modulus value = do
  modulusRefined <- mkNonZeroDivisor modulus
  pure (snd (divideWithRemainder value modulusRefined))

gcdDividesLeft :: NonZeroGcdContext -> Bool
gcdDividesLeft context =
  dividesInteger (nonZeroGcdDivisor context) (nonZeroGcdLeft context)

gcdDividesRight :: NonZeroGcdContext -> Bool
gcdDividesRight context =
  dividesInteger (nonZeroGcdDivisor context) (nonZeroGcdRight context)

extGcdBezout :: Integer -> Integer -> Bool
extGcdBezout left right =
  let (gcdValue, coefficientLeft, coefficientRight) = extGcd left right
   in add (mul coefficientLeft left) (mul coefficientRight right) == gcdValue

modInverseCorrect :: ModInverseContext -> Bool
modInverseCorrect context =
  fromMaybe False $
    withNonZeroModulus (modInverseModulus context) $ \modulusRefined ->
      case modInverse (modInverseCandidate context) modulusRefined of
        Nothing -> False
        Just inverseValue ->
          dividesInteger
            (modInverseModulus context)
            (sub (mul (modInverseCandidate context) inverseValue) one)

modInverseAbsentNonunit :: ModInverseContext -> Bool
modInverseAbsentNonunit context =
  fromMaybe False $
    withNonZeroModulus (modInverseModulus context) $ \modulusRefined ->
      modInverse (modInverseCandidate context) modulusRefined == Nothing

crtSound :: CrtSoundContext -> Bool
crtSound =
  fromMaybe False . crtSoundResult

crtSoundResult :: CrtSoundContext -> Maybe Bool
crtSoundResult context = do
  let leftModulus = crtLeftModulus context
      rightModulus = crtRightModulus context
      divisor = gcd leftModulus rightModulus
  normalizedLeftResidue <- normalizeInteger leftModulus (crtLeftResidue context)
  normalizedRightResidue <- normalizeInteger rightModulus (crtRightResidue context)
  divisorRefined <- mkNonZeroDivisor divisor
  let (reducedRightModulus, _) = divideWithRemainder rightModulus divisorRefined
      congruencesCompatible = dividesInteger divisor (sub normalizedRightResidue normalizedLeftResidue)
      combinedModulusExpected = mul leftModulus reducedRightModulus
  SomeNonZeroModulus leftModulusRefined <- mkSomeNonZeroModulus leftModulus
  SomeNonZeroModulus rightModulusRefined <- mkSomeNonZeroModulus rightModulus
  pure $
    case
      crt
        (mkCanonicalResidue leftModulusRefined normalizedLeftResidue)
        (mkCanonicalResidue rightModulusRefined normalizedRightResidue) of
      Nothing -> not congruencesCompatible
      Just combinedResidue ->
        combinedResidueSound
          leftModulus
          normalizedLeftResidue
          rightModulus
          normalizedRightResidue
          combinedModulusExpected
          combinedResidue

combinedResidueSound :: Integer -> Integer -> Integer -> Integer -> Integer -> CanonicalResidue modulus Integer -> Bool
combinedResidueSound leftModulus normalizedLeftResidue rightModulus normalizedRightResidue combinedModulusExpected combinedResidue =
  let residueView = canonicalResidueView combinedResidue
      candidate = residueCandidateValue residueView
   in dividesInteger leftModulus (sub candidate normalizedLeftResidue)
        && dividesInteger rightModulus (sub candidate normalizedRightResidue)
        && residueModulusValue residueView == combinedModulusExpected

znGeneratorNormalized :: Zn 7 -> Bool
znGeneratorNormalized value =
  let modulus = znModulus (Proxy @7)
      residue = unZn value
   in residue >= 0 && residue < modulus

polynomialGeneratorCanonical :: Polynomial Integer -> Bool
polynomialGeneratorCanonical polynomialValue =
  normalizePolynomial polynomialValue == polynomialValue

freeAbelianGeneratorCanonical :: FreeAbelianGroup Int -> Bool
freeAbelianGeneratorCanonical groupValue =
  normalizeFreeAbelianGroup groupValue == groupValue

sparseVecGeneratorCanonical :: SparseVec Integer Int -> Bool
sparseVecGeneratorCanonical sparseVector =
  normalize sparseVector == sparseVector

powerSetGeneratorCanonical :: PowerSet Atom -> Bool
powerSetGeneratorCanonical powerSet =
  normalizePowerSet powerSet == powerSet

orientationGroupInvLeft :: Orientation -> Bool
orientationGroupInvLeft = Algebraic.groupLeftInverse (<>) groupInverse mempty

orientationGroupInvRight :: Orientation -> Bool
orientationGroupInvRight = Algebraic.groupRightInverse (<>) groupInverse mempty

orientationAbelianComm :: Orientation -> Orientation -> Bool
orientationAbelianComm = Algebraic.abelianCommutativity (<>)

tests :: TestTree
tests =
  testsWithConfig defaultAlgebraGeneratorConfig

testsWithConfig :: AlgebraGeneratorConfig -> TestTree
testsWithConfig config =
  testGroup
    "moonlight-algebra"
    [ renderLawSuite (lawGroup "laws" (algebraLawSuites config)),
      representationBoundaryTests config,
      boolSemiringTests,
      latticeImplementationTests
    ]

algebraLawSuites :: AlgebraGeneratorConfig -> [LawSuite]
algebraLawSuites config =
  let genFreeAbelianInt = genFreeAbelianGroup config (genIntBasis config)
      genAdditiveFreeAbelianInt = Additive <$> genFreeAbelianInt
      genAdditiveLaneVector = Additive <$> genLaneVector
      genFreeMonoidInt = genBatch config (genIntBasis config)
      genZn7 = genZn @7 config
      genCoefficient = genIntegerCoefficient config
      genInteger = genIntegerLawValue config
      genPolynomialInteger = genPolynomial config genCoefficient
      genPowerSetAtom = genPowerSet config genAtom
      genSparseVecIntegerInt = genSparseVec config (genIntBasis config) genCoefficient
   in [ lawGroup
          "additive-wrapper"
          [ namedHedgehogLaw MonoidAssoc (genTripleOf genAdditiveFreeAbelianInt) (applyTriple monoidAssoc),
            namedHedgehogLaw MonoidLeftId genAdditiveFreeAbelianInt monoidLeftId,
            namedHedgehogLaw MonoidRightId genAdditiveFreeAbelianInt monoidRightId,
            namedHedgehogLaw GroupInvLeft genAdditiveFreeAbelianInt groupInvLeft,
            namedHedgehogLaw GroupInvRight genAdditiveFreeAbelianInt groupInvRight,
            namedHedgehogLaw AbelianComm (genPairOf genAdditiveFreeAbelianInt) (applyPair abelianComm)
          ],
        lawGroup
          "lane-vector-additive"
          [ namedHedgehogLaw MonoidAssoc (genTripleOf genAdditiveLaneVector) (applyTriple laneVectorMonoidAssoc),
            namedHedgehogLaw MonoidLeftId genAdditiveLaneVector laneVectorMonoidLeftId,
            namedHedgehogLaw MonoidRightId genAdditiveLaneVector laneVectorMonoidRightId,
            namedHedgehogLaw GroupInvLeft genAdditiveLaneVector laneVectorGroupInvLeft,
            namedHedgehogLaw GroupInvRight genAdditiveLaneVector laneVectorGroupInvRight,
            namedHedgehogLaw AbelianComm (genPairOf genAdditiveLaneVector) (applyPair laneVectorAbelianComm),
            namedHedgehogLaw LaneVectorSubtraction (genPairOf genLaneVector) (applyPair laneVectorSubtraction)
          ],
        lawGroup
          "free-monoid"
          [ namedHedgehogLaw FreeMonoidAssoc (genTripleOf genFreeMonoidInt) (applyTriple freeMonoidAssoc),
            namedHedgehogLaw FreeMonoidLeftId genFreeMonoidInt freeMonoidLeftId,
            namedHedgehogLaw FreeMonoidRightId genFreeMonoidInt freeMonoidRightId
          ],
        lawGroup
          "ring"
          [ namedHedgehogLaw RingAddAssoc (genTripleOf genZn7) (applyTriple ringAddAssoc),
            namedHedgehogLaw RingMulComm (genPairOf genZn7) (applyPair ringMulComm)
          ],
        lawGroup
          "lattice"
          [ namedHedgehogLaw (CommonLaw LatticeAbsorptionJoin) (genPairOf genPowerSetAtom) (applyPair latticeAbsorptionJoin),
            namedHedgehogLaw (CommonLaw LatticeAbsorptionMeet) (genPairOf genPowerSetAtom) (applyPair latticeAbsorptionMeet)
          ],
        lawGroup
          "heyting"
          [ namedHedgehogLaw HeytingImpliesSelfTop genPowerSetAtom heytingImpliesSelfTop,
            namedHedgehogLaw HeytingMeetImplication (genPairOf genPowerSetAtom) (applyPair heytingMeetImplication),
            namedHedgehogLaw HeytingConsequentMeetImplication (genPairOf genPowerSetAtom) (applyPair heytingConsequentMeetImplication),
            namedHedgehogLaw HeytingImplicationDistributesMeet (genTripleOf genPowerSetAtom) (applyTriple heytingImplicationDistributesMeet),
            namedHedgehogLaw HeytingNegDefault genPowerSetAtom heytingNegDefault,
            namedHedgehogLaw HeytingEquivalenceDefault (genPairOf genPowerSetAtom) (applyPair heytingEquivalenceDefault)
          ],
        lawGroup
          "module"
          [ namedHedgehogLaw ModuleDistribScalarAdd (genTriple genCoefficient genCoefficient genPolynomialInteger) (applyTriple moduleDistribScalarAdd),
            namedHedgehogLaw ModuleDistribVectorAdd (genTriple genCoefficient genPolynomialInteger genPolynomialInteger) (applyTriple moduleDistribVectorAdd)
          ],
        lawGroup
          "unit"
          [ namedHedgehogLaw UnitInverseOne (pure ()) unitInverseOne,
            namedHedgehogLaw UnitInverseCorrect genIntegerUnit unitInverseCorrect,
            namedHedgehogLaw UnitInverseAbsent (genIntegerNonUnit config) unitInverseAbsent
          ],
        lawGroup
          "gcd"
          [ namedHedgehogLaw GcdDividesLeft (genNonZeroGcdContext config) gcdDividesLeft,
            namedHedgehogLaw GcdDividesRight (genNonZeroGcdContext config) gcdDividesRight,
            namedHedgehogLaw ExtGcdBezout (genPairOf genInteger) (applyPair extGcdBezout),
            namedHedgehogLaw ModInverseCorrect (genModInverseContext isUnit config) modInverseCorrect,
            namedHedgehogLaw ModInverseAbsentNonunit (genModInverseContext (not . isUnit) config) modInverseAbsentNonunit,
            namedHedgehogLaw CrtSound (genCrtSoundContext config) crtSound
          ],
        lawGroup
          "canonicalization"
          [ namedHedgehogLaw PolynomialCanonicalizationIdempotent genPolynomialInteger polynomialCanonicalizationIdempotent
          ],
        lawGroup
          "hedgehog-generators"
          [ namedHedgehogLaw ZnGeneratorNormalized genZn7 znGeneratorNormalized,
            namedHedgehogLaw PolynomialGeneratorCanonical genPolynomialInteger polynomialGeneratorCanonical,
            namedHedgehogLaw FreeAbelianGeneratorCanonical genFreeAbelianInt freeAbelianGeneratorCanonical,
            namedHedgehogLaw SparseVecGeneratorCanonical genSparseVecIntegerInt sparseVecGeneratorCanonical,
            namedHedgehogLaw PowerSetGeneratorCanonical genPowerSetAtom powerSetGeneratorCanonical
          ],
        lawGroup
          "orientation"
          [ namedHedgehogLaw OrientationGroupInvLeft genOrientation orientationGroupInvLeft,
            namedHedgehogLaw OrientationGroupInvRight genOrientation orientationGroupInvRight,
            namedHedgehogLaw OrientationAbelianComm (genPairOf genOrientation) (applyPair orientationAbelianComm)
          ]
      ]

representationBoundaryTests :: AlgebraGeneratorConfig -> TestTree
representationBoundaryTests config =
  testGroup
    "representation-boundaries"
    [ testCase "huge polynomial degree keeps sparse and dense observations coherent" $ do
        let hugeDegree = fromIntegral (maxBound :: Int) + 1
            hugePolynomial = monomial hugeDegree (1 :: Integer)
        coefficient @Integer hugeDegree hugePolynomial @?= 1
        take 1 (toCoefficients hugePolynomial) @?= [0]
        assertBool
          "a huge monomial is not the constant polynomial"
          (hugePolynomial /= fromCoefficients [1]),
      testProperty "ordinary polynomial sparse and dense views agree" $ HH.property $ do
        polynomialValue <- HH.forAll (genPolynomial config (genIntegerCoefficient config))
        HH.assert (polynomialViewsCoherent polynomialValue),
      testCase "lane vector construction pads short inputs" $
        laneVectorLanes (laneVectorFromLanes (UVector.fromList [1, 2]))
          @?= UVector.fromList (1 : 2 : replicate (laneCount - 2) 0),
      testCase "lane vector construction truncates long inputs" $
        laneVectorLanes (laneVectorFromLanes (UVector.generate (laneCount + 1) fromIntegral))
          @?= UVector.generate laneCount fromIntegral,
      testCase "product arity remains a Natural until list construction" $ do
        mkProductAlgebra @18446744073709551616 ([] :: [Int]) @?= Nothing
        take 1
          (toProductList (pure 7 :: ProductAlgebra 18446744073709551616 Int))
          @?= [7],
      testCase "Euclidean division rejects a zero divisor at construction" $
        assertBool
          "zero must not construct a NonZeroDivisor"
          (case mkNonZeroDivisor (0 :: Integer) of
             Nothing -> True
             Just _ -> False)
    ]

boolSemiringTests :: TestTree
boolSemiringTests =
  testGroup
    "bool-semiring"
    [ testCase "zero is additive identity and absorbing for multiplication" $
        assertBool
          "bool semiring zero laws"
          (all
             (\value ->
                add zero value == value
                  && add value zero == value
                  && mul zero value == zero
                  && mul value zero == zero
             )
             [False, True]
          ),
      testCase "one is multiplicative identity" $
        assertBool
          "bool semiring one laws"
          (all
             (\value ->
                mul one value == value
                  && mul value one == value
             )
             [False, True]
          ),
      testCase "multiplication distributes over addition" $
        assertBool
          "bool semiring distributivity"
          (all
             (\(leftValue, middleValue, rightValue) ->
                mul leftValue (add middleValue rightValue)
                  == add
                    (mul leftValue middleValue)
                    (mul leftValue rightValue)
                  && mul (add middleValue rightValue) leftValue
                    == add
                      (mul middleValue leftValue)
                      (mul rightValue leftValue)
             )
             [ (leftValue, middleValue, rightValue)
             | leftValue <- [False, True],
               middleValue <- [False, True],
               rightValue <- [False, True]
             ]
          )
    ]

latticeImplementationTests :: TestTree
latticeImplementationTests =
  testGroup
    "lattice-implementation"
    [ testCase "bounded and non-empty folds use the local semilattice tower" $ do
        joins ([] :: [Bool]) @?= False
        joins [False, True, False] @?= True
        joins1 (False :| [True, False]) @?= True
        meets ([] :: [Bool]) @?= True
        meets [True, False, True] @?= False
        meets1 (True :| [False, True]) @?= False,
      testCase "Join and Meet wrappers expose semilattice folds" $ do
        foldMap Join [False, True, False] @?= Join True
        foldMap Meet [True, False, True] @?= Meet False,
      testCase "joinLeq and meetLeq induce the Bool lattice order" $ do
        joinLeq False True @?= True
        joinLeq True False @?= False
        meetLeq False True @?= True
        meetLeq True False @?= False
        fromBool True @?= True
        fromBool False @?= False,
      testCase "true lattice fixpoints iterate the endomap with an explicit budget" $ do
        leastFixpoint 4 growPowerSet
          @?= Right (fromList [Alpha, Beta, Gamma] :: PowerSet Atom)
        greatestFixpoint 4 shrinkPowerSet
          @?= Right (fromList [] :: PowerSet Atom)
        iterateFixpointFrom 4 (fromList [Alpha, Beta] :: PowerSet Atom) shrinkPowerSet
          @?= Right (fromList [] :: PowerSet Atom),
      testCase "pre/post lattice closures are distinct from true fixpoints" $ do
        leastFixpoint 2 not @?= Left (FixpointDivergence 2 False)
        leastPreFixpoint 2 not @?= Right True
        greatestFixpoint 2 not @?= Left (FixpointDivergence 2 True)
        greatestPostFixpoint 2 not @?= Right False,
      testCase "seeded pre/post closures keep the old closure law under explicit names" $ do
        leastPreFixpointFrom 3 (fromList [Beta] :: PowerSet Atom) growPowerSet
          @?= Right (fromList [Alpha, Beta, Gamma] :: PowerSet Atom)
        greatestPostFixpointFrom 4 (fromList [Alpha, Beta] :: PowerSet Atom) shrinkPowerSet
          @?= Right (fromList [] :: PowerSet Atom),
      testCase "bounded lattice fixpoints report typed divergence" $
        leastFixpoint 2 growPowerSet
          @?= Left (FixpointDivergence 2 (fromList [Alpha, Beta] :: PowerSet Atom)),
      testCase "containers inherit finite-support lattice operations" $ do
        join (Set.fromList [1, 2 :: Int]) (Set.fromList [2, 3])
          @?= Set.fromList [1, 2, 3]
        meet (Set.fromList [1, 2 :: Int]) (Set.fromList [2, 3])
          @?= Set.fromList [2]
        join (IntSet.fromList [1, 2]) (IntSet.fromList [2, 3])
          @?= IntSet.fromList [1, 2, 3]
        meet (IntSet.fromList [1, 2]) (IntSet.fromList [2, 3])
          @?= IntSet.fromList [2]
        join
          (Map.fromList [("a", False), ("b", True)])
          (Map.fromList [("a", True), ("c", True)])
          @?= Map.fromList [("a", True), ("b", True), ("c", True)]
        meet
          (Map.fromList [("a", False), ("b", True)])
          (Map.fromList [("a", True), ("c", True)])
          @?= Map.fromList [("a", False)]
        join
          (IntMap.fromList [(1, False), (2, True)])
          (IntMap.fromList [(1, True), (3, True)])
          @?= IntMap.fromList [(1, True), (2, True), (3, True)]
        meet
          (IntMap.fromList [(1, False), (2, True)])
          (IntMap.fromList [(1, True), (3, True)])
          @?= IntMap.fromList [(1, False)],
      testCase "product algebras inherit componentwise lattice operations" $
        case
          ( mkProductAlgebra @3 [False, True, False],
            mkProductAlgebra @3 [True, False, False]
          ) of
          (Just left, Just right) -> do
            toProductList (join left right) @?= [True, True, False]
            toProductList (meet left right) @?= [False, False, False]
            toProductList (implies left right) @?= [True, False, True]
            toProductList (complement left) @?= [True, False, True]
          _ ->
            assertBool "test product arity is fixed at the type level" False
    ]

growPowerSet :: PowerSet Atom -> PowerSet Atom
growPowerSet current
  | member Gamma current = current
  | member Beta current = fromList [Alpha, Beta, Gamma]
  | member Alpha current = fromList [Alpha, Beta]
  | otherwise = fromList [Alpha]

shrinkPowerSet :: PowerSet Atom -> PowerSet Atom
shrinkPowerSet current
  | member Alpha current = fromList [Beta, Gamma]
  | member Beta current = fromList [Gamma]
  | otherwise = fromList []
