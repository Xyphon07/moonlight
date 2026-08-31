{-# LANGUAGE DerivingStrategies #-}

-- | Exact standard-form linear feasibility and optimization.
--
-- The admitted problem is @A x = b, x >= 0@.  A successful decision returns
-- either an exact nonnegative solution or a Farkas functional @y@ satisfying
-- @transpose A * y >= 0@ and @b dot y = -1@.  Optimization uses the same
-- Phase-I dictionary and Bland pivot order, then returns either a verified
-- primal/dual optimum or a feasible base with a normalized improving ray.
module Moonlight.LinAlg.Pure.Domain.LinearFeasibility
  ( StandardFeasibility,
    LinearFeasibilityError (..),
    FeasibilityDecision (..),
    NonNegativeSolution,
    FarkasCertificate,
    StandardLinearProgram,
    LinearOptimizationDecision (..),
    ExactLinearOptimum,
    LinearProgramUnboundedWitness,
    mkStandardFeasibility,
    mkStandardLinearProgram,
    decideLinearFeasibility,
    optimizeLinearProgram,
    nonNegativeSolutionVector,
    farkasCertificateVector,
    verifyNonNegativeSolution,
    verifyFarkasCertificate,
    exactLinearOptimumSolution,
    exactLinearOptimumDual,
    exactLinearOptimumValue,
    linearProgramUnboundedBase,
    linearProgramUnboundedDirection,
    verifyExactLinearOptimum,
    verifyLinearProgramUnboundedWitness,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Core (MoonlightError)
import Moonlight.LinAlg.Pure.Dense.Dynamic
  ( DynMatrix,
    DynVector,
    dynMatrixShape,
    dynMatrixToRows,
    dynVectorLength,
    dynVectorToList,
    mkDynVector,
  )
import Prelude

type StandardFeasibility :: Type
-- | An exact standard-form system @A x = b@ with @x >= 0@.
data StandardFeasibility = StandardFeasibility !(DynMatrix Rational) !(DynVector Rational)

type LinearFeasibilityError :: Type
data LinearFeasibilityError
  = FeasibilityRhsDimensionMismatch !Int !Int
  | FeasibilityObjectiveDimensionMismatch !Int !Int
  | FeasibilityCarrierConstructionFailed !MoonlightError
  | FeasibilityPhaseOneUnbounded !Int
  | FeasibilityDictionaryMalformed
  | FeasibilityPrimalCertificateRejected
  | FeasibilityFarkasCertificateRejected
  | FeasibilityOptimalCertificateRejected
  | FeasibilityUnboundedCertificateRejected
  deriving stock (Eq, Show)

type NonNegativeSolution :: Type
-- | An opaque primal witness whose entries are nonnegative and satisfy @A x = b@.
newtype NonNegativeSolution = NonNegativeSolution (DynVector Rational)

instance Eq NonNegativeSolution where
  left == right =
    dynVectorLength (nonNegativeSolutionVector left) == dynVectorLength (nonNegativeSolutionVector right)
      && dynVectorToList (nonNegativeSolutionVector left) == dynVectorToList (nonNegativeSolutionVector right)

instance Show NonNegativeSolution where
  show = show . dynVectorToList . nonNegativeSolutionVector

type FarkasCertificate :: Type
-- | An opaque obstruction @y@ normalized by @b dot y = -1@ and @A^T y >= 0@.
newtype FarkasCertificate = FarkasCertificate (DynVector Rational)

instance Eq FarkasCertificate where
  left == right =
    dynVectorLength (farkasCertificateVector left) == dynVectorLength (farkasCertificateVector right)
      && dynVectorToList (farkasCertificateVector left) == dynVectorToList (farkasCertificateVector right)

instance Show FarkasCertificate where
  show = show . dynVectorToList . farkasCertificateVector

type FeasibilityDecision :: Type
-- | The exhaustive semantic result; infeasibility is evidence, not an error.
data FeasibilityDecision
  = Feasible !NonNegativeSolution
  | Infeasible !FarkasCertificate
  deriving stock (Eq, Show)

type StandardLinearProgram :: Type
-- | Maximization of @c dot x@ over a 'StandardFeasibility' region.
data StandardLinearProgram = StandardLinearProgram !StandardFeasibility !(DynVector Rational)

type ExactLinearOptimum :: Type
-- | A primal/dual optimum satisfying exact feasibility and strong duality.
data ExactLinearOptimum = ExactLinearOptimum !NonNegativeSolution !(DynVector Rational) !Rational

instance Eq ExactLinearOptimum where
  left == right =
    exactLinearOptimumSolution left == exactLinearOptimumSolution right
      && vectorsEqual (exactLinearOptimumDual left) (exactLinearOptimumDual right)
      && exactLinearOptimumValue left == exactLinearOptimumValue right

instance Show ExactLinearOptimum where
  show optimum =
    show
      ( exactLinearOptimumSolution optimum,
        dynVectorToList (exactLinearOptimumDual optimum),
        exactLinearOptimumValue optimum
      )

type LinearProgramUnboundedWitness :: Type
-- | A feasible base and nonnegative null direction normalized by @c dot d = 1@.
data LinearProgramUnboundedWitness = LinearProgramUnboundedWitness !NonNegativeSolution !(DynVector Rational)

instance Eq LinearProgramUnboundedWitness where
  left == right =
    linearProgramUnboundedBase left == linearProgramUnboundedBase right
      && vectorsEqual (linearProgramUnboundedDirection left) (linearProgramUnboundedDirection right)

instance Show LinearProgramUnboundedWitness where
  show witness =
    show
      ( linearProgramUnboundedBase witness,
        dynVectorToList (linearProgramUnboundedDirection witness)
      )

type LinearOptimizationDecision :: Type
-- | Exact alternatives for a standard-form maximization problem.
data LinearOptimizationDecision
  = LinearProgramInfeasible !FarkasCertificate
  | LinearProgramOptimal !ExactLinearOptimum
  | LinearProgramUnbounded !LinearProgramUnboundedWitness
  deriving stock (Eq, Show)

nonNegativeSolutionVector :: NonNegativeSolution -> DynVector Rational
nonNegativeSolutionVector (NonNegativeSolution vectorValue) = vectorValue

farkasCertificateVector :: FarkasCertificate -> DynVector Rational
farkasCertificateVector (FarkasCertificate vectorValue) = vectorValue

exactLinearOptimumSolution :: ExactLinearOptimum -> NonNegativeSolution
exactLinearOptimumSolution (ExactLinearOptimum solution _ _) = solution

exactLinearOptimumDual :: ExactLinearOptimum -> DynVector Rational
exactLinearOptimumDual (ExactLinearOptimum _ dualVector _) = dualVector

exactLinearOptimumValue :: ExactLinearOptimum -> Rational
exactLinearOptimumValue (ExactLinearOptimum _ _ optimumValue) = optimumValue

linearProgramUnboundedBase :: LinearProgramUnboundedWitness -> NonNegativeSolution
linearProgramUnboundedBase (LinearProgramUnboundedWitness base _) = base

linearProgramUnboundedDirection :: LinearProgramUnboundedWitness -> DynVector Rational
linearProgramUnboundedDirection (LinearProgramUnboundedWitness _ direction) = direction

standardCoefficientMatrix :: StandardFeasibility -> DynMatrix Rational
standardCoefficientMatrix (StandardFeasibility coefficientMatrix _) = coefficientMatrix

standardRightHandSide :: StandardFeasibility -> DynVector Rational
standardRightHandSide (StandardFeasibility _ rightHandSide) = rightHandSide

linearProgramFeasibility :: StandardLinearProgram -> StandardFeasibility
linearProgramFeasibility (StandardLinearProgram feasibility _) = feasibility

linearProgramObjective :: StandardLinearProgram -> DynVector Rational
linearProgramObjective (StandardLinearProgram _ objective) = objective

mkStandardFeasibility ::
  DynMatrix Rational ->
  DynVector Rational ->
  Either LinearFeasibilityError StandardFeasibility
mkStandardFeasibility coefficientMatrix rightHandSide =
  let (rowCount, _) = dynMatrixShape coefficientMatrix
      rhsDimension = dynVectorLength rightHandSide
   in if rowCount == rhsDimension
        then Right (StandardFeasibility coefficientMatrix rightHandSide)
        else
          Left (FeasibilityRhsDimensionMismatch rowCount rhsDimension)

mkStandardLinearProgram ::
  StandardFeasibility ->
  DynVector Rational ->
  Either LinearFeasibilityError StandardLinearProgram
mkStandardLinearProgram feasibility objective =
  let (_, variableCount) = dynMatrixShape (standardCoefficientMatrix feasibility)
      objectiveDimension = dynVectorLength objective
   in if variableCount == objectiveDimension
        then Right (StandardLinearProgram feasibility objective)
        else Left (FeasibilityObjectiveDimensionMismatch variableCount objectiveDimension)

decideLinearFeasibility ::
  StandardFeasibility ->
  Either LinearFeasibilityError FeasibilityDecision
decideLinearFeasibility problem = do
  initialDictionary <- initialSimplexDictionary problem
  finalDictionary <- descendPhaseOne initialDictionary
  case compare (dictionaryObjective finalDictionary) 0 of
    EQ -> do
      solution <- primalSolution problem finalDictionary
      if verifyNonNegativeSolution problem solution
        then Right (Feasible solution)
        else Left FeasibilityPrimalCertificateRejected
    LT -> do
      certificate <- dualCertificate problem finalDictionary
      if verifyFarkasCertificate problem certificate
        then Right (Infeasible certificate)
        else Left FeasibilityFarkasCertificateRejected
    GT -> Left FeasibilityDictionaryMalformed

optimizeLinearProgram ::
  StandardLinearProgram ->
  Either LinearFeasibilityError LinearOptimizationDecision
optimizeLinearProgram linearProgram = do
  let problem = linearProgramFeasibility linearProgram
  initialDictionary <- initialSimplexDictionary problem
  phaseOneDictionary <- descendPhaseOne initialDictionary
  case compare (dictionaryObjective phaseOneDictionary) 0 of
    LT -> do
      certificate <- dualCertificate problem phaseOneDictionary
      if verifyFarkasCertificate problem certificate
        then Right (LinearProgramInfeasible certificate)
        else Left FeasibilityFarkasCertificateRejected
    GT -> Left FeasibilityDictionaryMalformed
    EQ -> do
      cleanedDictionary <- removeArtificialBasis phaseOneDictionary
      phaseTwoDictionary <- initializePhaseTwo (dynVectorToList (linearProgramObjective linearProgram)) cleanedDictionary
      phaseTwoOutcome <- descendPhaseTwo phaseTwoDictionary
      case phaseTwoOutcome of
        PhaseTwoUnbounded entering unboundedDictionary -> do
          witness <- unboundedWitness linearProgram entering unboundedDictionary
          if verifyLinearProgramUnboundedWitness linearProgram witness
            then Right (LinearProgramUnbounded witness)
            else Left FeasibilityUnboundedCertificateRejected
        PhaseTwoOptimal optimalDictionary -> do
          optimum <- exactOptimum linearProgram optimalDictionary
          if verifyExactLinearOptimum linearProgram optimum
            then Right (LinearProgramOptimal optimum)
            else Left FeasibilityOptimalCertificateRejected

verifyExactLinearOptimum :: StandardLinearProgram -> ExactLinearOptimum -> Bool
verifyExactLinearOptimum linearProgram optimum =
  let problem = linearProgramFeasibility linearProgram
      objectiveValues = dynVectorToList (linearProgramObjective linearProgram)
      solutionValues = dynVectorToList (nonNegativeSolutionVector (exactLinearOptimumSolution optimum))
      dualValues = dynVectorToList (exactLinearOptimumDual optimum)
      rhsValues = dynVectorToList (standardRightHandSide problem)
      (_, columnCount) = dynMatrixShape (standardCoefficientMatrix problem)
   in verifyNonNegativeSolution problem (exactLinearOptimumSolution optimum)
        && dynVectorLength (exactLinearOptimumDual optimum) == length rhsValues
        && exactLinearOptimumValue optimum == dot objectiveValues solutionValues
        && exactLinearOptimumValue optimum == dot rhsValues dualValues
        && either
          (const False)
          (\rows -> zipWith (>=) (columnProducts columnCount rows dualValues) objectiveValues == replicate columnCount True)
          (dynMatrixToRows (standardCoefficientMatrix problem))

verifyLinearProgramUnboundedWitness :: StandardLinearProgram -> LinearProgramUnboundedWitness -> Bool
verifyLinearProgramUnboundedWitness linearProgram witness =
  let problem = linearProgramFeasibility linearProgram
      direction = dynVectorToList (linearProgramUnboundedDirection witness)
      objectiveValues = dynVectorToList (linearProgramObjective linearProgram)
      (_, columnCount) = dynMatrixShape (standardCoefficientMatrix problem)
   in verifyNonNegativeSolution problem (linearProgramUnboundedBase witness)
        && length direction == columnCount
        && all (>= 0) direction
        && dot objectiveValues direction == 1
        && either
          (const False)
          (\rows -> all (== 0) (fmap (`dot` direction) rows))
          (dynMatrixToRows (standardCoefficientMatrix problem))

verifyNonNegativeSolution :: StandardFeasibility -> NonNegativeSolution -> Bool
verifyNonNegativeSolution problem solution =
  let values = dynVectorToList (nonNegativeSolutionVector solution)
      (_, columnCount) = dynMatrixShape (standardCoefficientMatrix problem)
      rhsValues = dynVectorToList (standardRightHandSide problem)
   in length values == columnCount
        && all (>= 0) values
        && either
          (const False)
          (\rows -> fmap (`dot` values) rows == rhsValues)
          (dynMatrixToRows (standardCoefficientMatrix problem))

verifyFarkasCertificate :: StandardFeasibility -> FarkasCertificate -> Bool
verifyFarkasCertificate problem certificate =
  let values = dynVectorToList (farkasCertificateVector certificate)
      (rowCount, columnCount) = dynMatrixShape (standardCoefficientMatrix problem)
      rhsValues = dynVectorToList (standardRightHandSide problem)
   in length values == rowCount
        && dot rhsValues values == (-1)
        && either
          (const False)
          (\rows -> all (>= 0) (columnProducts columnCount rows values))
          (dynMatrixToRows (standardCoefficientMatrix problem))

type DictionaryRow :: Type
data DictionaryRow = DictionaryRow
  { dictionaryBasicVariable :: !Int,
    dictionaryRowRhs :: !Rational,
    dictionaryRowCoefficients :: ![Rational],
    dictionaryRowTransform :: ![Rational]
  }

type SimplexDictionary :: Type
data SimplexDictionary = SimplexDictionary
  { dictionaryOriginalVariableCount :: !Int,
    dictionaryEquationSigns :: ![Rational],
    dictionaryRows :: ![DictionaryRow],
    dictionaryObjective :: !Rational,
    dictionaryReducedCosts :: ![Rational],
    dictionaryDual :: ![Rational]
  }

initialSimplexDictionary :: StandardFeasibility -> Either LinearFeasibilityError SimplexDictionary
initialSimplexDictionary problem = do
  coefficientRows <- first FeasibilityCarrierConstructionFailed (dynMatrixToRows (standardCoefficientMatrix problem))
  let (equationCount, variableCount) = dynMatrixShape (standardCoefficientMatrix problem)
      rhsValues = dynVectorToList (standardRightHandSide problem)
      signs = fmap (\value -> if value < 0 then (-1) else 1) rhsValues
      signedRows = zipWith (\signValue -> fmap (signValue *)) signs coefficientRows
      signedRhs = zipWith (*) signs rhsValues
      totalVariableCount = variableCount + equationCount
      rows =
        zipWith3
          (initialDictionaryRow variableCount equationCount totalVariableCount)
          [0 ..]
          signedRows
          signedRhs
      reducedOriginal =
        fmap
          (\columnIndex -> sum (fmap (valueAtOrZero columnIndex) signedRows))
          [0 .. variableCount - 1]
      reducedCosts = reducedOriginal <> replicate equationCount 0
  Right
    SimplexDictionary
      { dictionaryOriginalVariableCount = variableCount,
        dictionaryEquationSigns = signs,
        dictionaryRows = rows,
        dictionaryObjective = negate (sum signedRhs),
        dictionaryReducedCosts = reducedCosts,
        dictionaryDual = replicate equationCount (-1)
      }

initialDictionaryRow :: Int -> Int -> Int -> Int -> [Rational] -> Rational -> DictionaryRow
initialDictionaryRow variableCount equationCount _totalVariableCount rowIndex coefficientRow rhsValue =
  DictionaryRow
    { dictionaryBasicVariable = variableCount + rowIndex,
      dictionaryRowRhs = rhsValue,
      dictionaryRowCoefficients = coefficientRow <> replicate equationCount 0,
      dictionaryRowTransform =
        fmap (\candidateIndex -> if candidateIndex == rowIndex then 1 else 0) [0 .. equationCount - 1]
    }

descendPhaseOne :: SimplexDictionary -> Either LinearFeasibilityError SimplexDictionary
descendPhaseOne dictionary =
  case enteringVariable dictionary of
    Nothing -> Right dictionary
    Just entering -> do
      leaving <-
        maybe
          (Left (FeasibilityPhaseOneUnbounded entering))
          Right
          (leavingRow entering (dictionaryRows dictionary))
      pivoted <- pivotDictionary entering leaving dictionary
      descendPhaseOne pivoted

enteringVariable :: SimplexDictionary -> Maybe Int
enteringVariable dictionary =
  foldr
    (\(variableIndex, reducedCost) selected -> if reducedCost > 0 then Just variableIndex else selected)
    Nothing
    (zip [0 ..] (dictionaryReducedCosts dictionary))

leavingRow :: Int -> [DictionaryRow] -> Maybe Int
leavingRow entering rows =
  fmap leavingCandidateRow
    ( foldl'
        chooseLeavingCandidate
        Nothing
        (zipWith (leavingCandidate entering) [0 ..] rows)
    )

type LeavingCandidate :: Type
data LeavingCandidate = LeavingCandidate
  { leavingCandidateRatio :: !Rational,
    leavingCandidateBasic :: !Int,
    leavingCandidateRow :: !Int
  }

leavingCandidate :: Int -> Int -> DictionaryRow -> Maybe LeavingCandidate
leavingCandidate entering rowIndex row =
  let coefficient = valueAtOrZero entering (dictionaryRowCoefficients row)
   in if coefficient > 0
        then
          Just
            LeavingCandidate
              { leavingCandidateRatio = dictionaryRowRhs row / coefficient,
                leavingCandidateBasic = dictionaryBasicVariable row,
                leavingCandidateRow = rowIndex
              }
        else Nothing

chooseLeavingCandidate :: Maybe LeavingCandidate -> Maybe LeavingCandidate -> Maybe LeavingCandidate
chooseLeavingCandidate selected candidate =
  case (selected, candidate) of
    (Nothing, other) -> other
    (other, Nothing) -> other
    (Just left, Just right) ->
      if (leavingCandidateRatio right, leavingCandidateBasic right)
          < (leavingCandidateRatio left, leavingCandidateBasic left)
        then Just right
        else Just left

pivotDictionary :: Int -> Int -> SimplexDictionary -> Either LinearFeasibilityError SimplexDictionary
pivotDictionary entering leaving dictionary = do
  pivotRow <- maybe (Left FeasibilityDictionaryMalformed) Right (valueAt leaving (dictionaryRows dictionary))
  pivotValue <- maybe (Left FeasibilityDictionaryMalformed) Right (valueAt entering (dictionaryRowCoefficients pivotRow))
  reducedEntering <- maybe (Left FeasibilityDictionaryMalformed) Right (valueAt entering (dictionaryReducedCosts dictionary))
  if pivotValue == 0
    then Left FeasibilityDictionaryMalformed
    else do
      nextRows <- traverse (pivotRowAt entering leaving pivotRow pivotValue) (zip [0 ..] (dictionaryRows dictionary))
      let pivotScale = reducedEntering / pivotValue
          nextReducedCosts =
            fmap
              (pivotReducedCost entering (dictionaryBasicVariable pivotRow) pivotScale (dictionaryRowCoefficients pivotRow))
              (zip [0 ..] (dictionaryReducedCosts dictionary))
          nextDual = zipWith (+) (dictionaryDual dictionary) (fmap (pivotScale *) (dictionaryRowTransform pivotRow))
      Right
        dictionary
          { dictionaryRows = nextRows,
            dictionaryObjective = dictionaryObjective dictionary + pivotScale * dictionaryRowRhs pivotRow,
            dictionaryReducedCosts = nextReducedCosts,
            dictionaryDual = nextDual
          }

removeArtificialBasis :: SimplexDictionary -> Either LinearFeasibilityError SimplexDictionary
removeArtificialBasis dictionary =
  case firstArtificialBasic (dictionaryOriginalVariableCount dictionary) (dictionaryRows dictionary) of
    Nothing ->
      Right dictionary
    Just (rowIndex, row) ->
      case firstOriginalPivot (dictionaryOriginalVariableCount dictionary) row of
        Just entering -> pivotDictionary entering rowIndex dictionary >>= removeArtificialBasis
        Nothing ->
          removeArtificialBasis
            dictionary
              { dictionaryRows = removeAt rowIndex (dictionaryRows dictionary)
              }

firstArtificialBasic :: Int -> [DictionaryRow] -> Maybe (Int, DictionaryRow)
firstArtificialBasic originalVariableCount =
  foldr
    ( \(rowIndex, row) remaining ->
        if dictionaryBasicVariable row >= originalVariableCount
          then Just (rowIndex, row)
          else remaining
    )
    Nothing
    . zip [0 ..]

firstOriginalPivot :: Int -> DictionaryRow -> Maybe Int
firstOriginalPivot originalVariableCount row =
  foldr
    ( \(columnIndex, coefficient) remaining ->
        if coefficient /= 0 then Just columnIndex else remaining
    )
    Nothing
    (zip [0 .. originalVariableCount - 1] (take originalVariableCount (dictionaryRowCoefficients row)))

initializePhaseTwo :: [Rational] -> SimplexDictionary -> Either LinearFeasibilityError SimplexDictionary
initializePhaseTwo objectiveValues dictionary = do
  let variableCount = dictionaryOriginalVariableCount dictionary
      trimmedRows = trimRowsTo variableCount (dictionaryRows dictionary)
      basicCosts = fmap (\row -> valueAtOrZero (dictionaryBasicVariable row) objectiveValues) trimmedRows
      dualValues =
        foldr
          (zipWith (+))
          (replicate (length (dictionaryEquationSigns dictionary)) 0)
          (zipWith (\costValue row -> fmap (costValue *) (dictionaryRowTransform row)) basicCosts trimmedRows)
      basis = fmap dictionaryBasicVariable trimmedRows
      reducedCosts =
        fmap
          ( \columnIndex ->
              if columnIndex `elem` basis
                then 0
                else
                  valueAtOrZero columnIndex objectiveValues
                    - sum
                      ( zipWith
                          (\costValue row -> costValue * valueAtOrZero columnIndex (dictionaryRowCoefficients row))
                          basicCosts
                          trimmedRows
                      )
          )
          [0 .. variableCount - 1]
      objectiveValue = sum (zipWith (\costValue row -> costValue * dictionaryRowRhs row) basicCosts trimmedRows)
  if length objectiveValues /= variableCount
    then Left (FeasibilityObjectiveDimensionMismatch variableCount (length objectiveValues))
    else
      Right
        dictionary
          { dictionaryRows = trimmedRows,
            dictionaryObjective = objectiveValue,
            dictionaryReducedCosts = reducedCosts,
            dictionaryDual = dualValues
          }

trimRowsTo :: Int -> [DictionaryRow] -> [DictionaryRow]
trimRowsTo variableCount =
  fmap (\row -> row {dictionaryRowCoefficients = take variableCount (dictionaryRowCoefficients row)})

type PhaseTwoOutcome :: Type
data PhaseTwoOutcome
  = PhaseTwoOptimal !SimplexDictionary
  | PhaseTwoUnbounded !Int !SimplexDictionary

descendPhaseTwo :: SimplexDictionary -> Either LinearFeasibilityError PhaseTwoOutcome
descendPhaseTwo dictionary =
  case enteringVariable dictionary of
    Nothing -> Right (PhaseTwoOptimal dictionary)
    Just entering ->
      case leavingRow entering (dictionaryRows dictionary) of
        Nothing -> Right (PhaseTwoUnbounded entering dictionary)
        Just leaving -> pivotDictionary entering leaving dictionary >>= descendPhaseTwo

exactOptimum :: StandardLinearProgram -> SimplexDictionary -> Either LinearFeasibilityError ExactLinearOptimum
exactOptimum linearProgram dictionary = do
  let problem = linearProgramFeasibility linearProgram
      signs = dictionaryEquationSigns dictionary
      dualValues = zipWith (*) signs (dictionaryDual dictionary)
      (equationCount, _) = dynMatrixShape (standardCoefficientMatrix problem)
  solution <- primalSolution problem dictionary
  dualVector <- first FeasibilityCarrierConstructionFailed (mkDynVector equationCount dualValues)
  Right
    (ExactLinearOptimum solution dualVector (dictionaryObjective dictionary))

unboundedWitness :: StandardLinearProgram -> Int -> SimplexDictionary -> Either LinearFeasibilityError LinearProgramUnboundedWitness
unboundedWitness linearProgram entering dictionary = do
  let problem = linearProgramFeasibility linearProgram
      objectiveValues = dynVectorToList (linearProgramObjective linearProgram)
      (_, variableCount) = dynMatrixShape (standardCoefficientMatrix problem)
      basicDirections =
        fmap
          ( \row ->
              ( dictionaryBasicVariable row,
                negate (valueAtOrZero entering (dictionaryRowCoefficients row))
              )
          )
          (dictionaryRows dictionary)
      rawDirection =
        fmap
          ( \variableIndex ->
              if variableIndex == entering
                then 1
                else lookupOrZero variableIndex basicDirections
          )
          [0 .. variableCount - 1]
      objectiveSlope = dot objectiveValues rawDirection
  if objectiveSlope <= 0
    then Left FeasibilityDictionaryMalformed
    else do
      base <- primalSolution problem dictionary
      direction <- first FeasibilityCarrierConstructionFailed (mkDynVector variableCount (fmap (/ objectiveSlope) rawDirection))
      Right
        (LinearProgramUnboundedWitness base direction)

removeAt :: Int -> [a] -> [a]
removeAt selected =
  fmap snd . filter ((/= selected) . fst) . zip [0 :: Int ..]

pivotRowAt :: Int -> Int -> DictionaryRow -> Rational -> (Int, DictionaryRow) -> Either LinearFeasibilityError DictionaryRow
pivotRowAt entering leaving pivotRow pivotValue (rowIndex, row)
  | rowIndex == leaving =
      Right
        DictionaryRow
          { dictionaryBasicVariable = entering,
            dictionaryRowRhs = dictionaryRowRhs pivotRow / pivotValue,
            dictionaryRowCoefficients =
              fmap
                (pivotEnteringRowCoefficient entering (dictionaryBasicVariable pivotRow) pivotValue)
                (zip [0 ..] (dictionaryRowCoefficients pivotRow)),
            dictionaryRowTransform = fmap (/ pivotValue) (dictionaryRowTransform pivotRow)
          }
  | otherwise = do
      enteringCoefficient <- maybe (Left FeasibilityDictionaryMalformed) Right (valueAt entering (dictionaryRowCoefficients row))
      let scaleValue = enteringCoefficient / pivotValue
      Right
        row
          { dictionaryRowRhs = dictionaryRowRhs row - scaleValue * dictionaryRowRhs pivotRow,
            dictionaryRowCoefficients =
              zipWith3
                (pivotOtherRowCoefficient entering (dictionaryBasicVariable pivotRow) scaleValue)
                [0 ..]
                (dictionaryRowCoefficients row)
                (dictionaryRowCoefficients pivotRow),
            dictionaryRowTransform = zipWith (-) (dictionaryRowTransform row) (fmap (scaleValue *) (dictionaryRowTransform pivotRow))
          }

pivotEnteringRowCoefficient :: Int -> Int -> Rational -> (Int, Rational) -> Rational
pivotEnteringRowCoefficient entering oldBasic pivotValue (columnIndex, coefficient)
  | columnIndex == entering = 0
  | columnIndex == oldBasic = 1 / pivotValue
  | otherwise = coefficient / pivotValue

pivotOtherRowCoefficient :: Int -> Int -> Rational -> Int -> Rational -> Rational -> Rational
pivotOtherRowCoefficient entering oldBasic scaleValue columnIndex coefficient pivotCoefficient
  | columnIndex == entering = 0
  | columnIndex == oldBasic = negate scaleValue
  | otherwise = coefficient - scaleValue * pivotCoefficient

pivotReducedCost :: Int -> Int -> Rational -> [Rational] -> (Int, Rational) -> Rational
pivotReducedCost entering oldBasic scaleValue pivotCoefficients (columnIndex, reducedCost)
  | columnIndex == entering = 0
  | columnIndex == oldBasic = negate scaleValue
  | otherwise = reducedCost - scaleValue * valueAtOrZero columnIndex pivotCoefficients

primalSolution :: StandardFeasibility -> SimplexDictionary -> Either LinearFeasibilityError NonNegativeSolution
primalSolution problem dictionary = do
  let (_, variableCount) = dynMatrixShape (standardCoefficientMatrix problem)
      basicValues = fmap (\row -> (dictionaryBasicVariable row, dictionaryRowRhs row)) (dictionaryRows dictionary)
      values = fmap (\variableIndex -> lookupOrZero variableIndex basicValues) [0 .. variableCount - 1]
  vectorValue <- first FeasibilityCarrierConstructionFailed (mkDynVector variableCount values)
  Right (NonNegativeSolution vectorValue)

dualCertificate :: StandardFeasibility -> SimplexDictionary -> Either LinearFeasibilityError FarkasCertificate
dualCertificate problem dictionary = do
  let objectiveValue = dictionaryObjective dictionary
      transportedDual = zipWith (*) (dictionaryEquationSigns dictionary) (dictionaryDual dictionary)
      normalization = negate (recip objectiveValue)
      values = fmap (normalization *) transportedDual
      (equationCount, _) = dynMatrixShape (standardCoefficientMatrix problem)
  vectorValue <- first FeasibilityCarrierConstructionFailed (mkDynVector equationCount values)
  Right (FarkasCertificate vectorValue)

lookupOrZero :: Int -> [(Int, Rational)] -> Rational
lookupOrZero selected =
  foldr
    (\(candidate, value) remaining -> if candidate == selected then value else remaining)
    0

columnProducts :: Int -> [[Rational]] -> [Rational] -> [Rational]
columnProducts columnCount rows values =
  fmap
    (\columnIndex -> sum (zipWith (\row multiplier -> valueAtOrZero columnIndex row * multiplier) rows values))
    [0 .. columnCount - 1]

dot :: [Rational] -> [Rational] -> Rational
dot left right = sum (zipWith (*) left right)

valueAt :: Int -> [a] -> Maybe a
valueAt selected =
  foldr
    (\(candidate, value) remaining -> if candidate == selected then Just value else remaining)
    Nothing
    . zip [0 :: Int ..]

valueAtOrZero :: Num a => Int -> [a] -> a
valueAtOrZero selected = maybe 0 id . valueAt selected

vectorsEqual :: DynVector Rational -> DynVector Rational -> Bool
vectorsEqual left right =
  dynVectorLength left == dynVectorLength right
    && dynVectorToList left == dynVectorToList right
