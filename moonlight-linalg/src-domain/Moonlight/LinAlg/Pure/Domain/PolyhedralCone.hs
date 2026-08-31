{-# LANGUAGE DerivingStrategies #-}

-- | Finitely generated exact cones derived from the certified linear solver.
--
-- Nontrivial intersection is observed on a common positive-gauge section, so
-- the cones' universal zero intersection is excluded by construction.  The
-- section-distance operation names both that normalization and the L-infinity
-- norm rather than pretending that an unqualified cone margin exists.
module Moonlight.LinAlg.Pure.Domain.PolyhedralCone
  ( GeneratedCone,
    ConeError (..),
    ConeSide (..),
    ConeMembership (..),
    NonNegativeCombination,
    ConeSeparationCertificate,
    CommonPositiveGauge,
    NontrivialConeIntersection (..),
    CommonRayWitness,
    ProjectiveSeparationCertificate,
    mkGeneratedCone,
    generatedConeDimension,
    generatedConeGenerators,
    coneMember,
    nonNegativeCombinationCoefficients,
    coneSeparationFunctional,
    mkCommonPositiveGauge,
    commonPositiveGaugeFunctional,
    intersectGeneratedCones,
    commonRayPoint,
    commonRayLeftCoefficients,
    commonRayRightCoefficients,
    verifyCommonRayWitness,
    projectiveSeparationFunctional,
    LInfSectionDistance,
    linfSectionDistanceValue,
    coneSectionDistanceLInf,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Moonlight.Core (MoonlightError, checkedNonNegativeSum)
import Moonlight.LinAlg.Pure.Dense.Dynamic
  ( DynMatrix,
    DynVector,
    dynVectorLength,
    dynVectorToList,
    mkDynMatrix,
    mkDynVector,
  )
import Moonlight.LinAlg.Pure.Domain.LinearFeasibility
  ( FarkasCertificate,
    FeasibilityDecision (..),
    LinearOptimizationDecision (..),
    LinearFeasibilityError,
    NonNegativeSolution,
    StandardLinearProgram,
    decideLinearFeasibility,
    exactLinearOptimumValue,
    farkasCertificateVector,
    mkStandardFeasibility,
    mkStandardLinearProgram,
    nonNegativeSolutionVector,
    optimizeLinearProgram,
  )
import Prelude

type GeneratedCone :: Type
-- | A finitely generated cone in a checked ambient dimension.
data GeneratedCone = GeneratedCone !Int ![DynVector Rational]

type ConeSide :: Type
data ConeSide = LeftCone | RightCone
  deriving stock (Eq, Show)

type ConeError :: Type
data ConeError
  = ConeNegativeAmbientDimension !Int
  | ConeGeneratorDimensionMismatch !Int !Int !Int
  | ConeZeroGenerator !Int
  | ConeTargetDimensionMismatch !Int !Int
  | ConeAmbientDimensionMismatch !Int !Int
  | ConeGaugeDimensionMismatch !Int !Int
  | ConeGaugeNotStrictlyPositive !ConeSide !Int !Rational
  | ConeCardinalityOverflow
  | ConeCarrierConstructionFailed !MoonlightError
  | ConeFeasibilityFailed !LinearFeasibilityError
  | ConeDerivedCertificateRejected
  | ConeProjectiveSectionEmpty !ConeSide
  | ConeDistanceOptimizationInfeasible
  | ConeDistanceOptimizationUnbounded
  deriving stock (Eq, Show)

type NonNegativeCombination :: Type
newtype NonNegativeCombination = NonNegativeCombination (DynVector Rational)

type ConeSeparationCertificate :: Type
newtype ConeSeparationCertificate = ConeSeparationCertificate (DynVector Rational)

type ConeMembership :: Type
-- | Either exact nonnegative coefficients or an exact separating functional.
data ConeMembership
  = ConeMember !NonNegativeCombination
  | ConeSeparated !ConeSeparationCertificate

type CommonPositiveGauge :: Type
-- | A functional strictly positive on every generator of both cones.
data CommonPositiveGauge = CommonPositiveGauge !GeneratedCone !GeneratedCone !(DynVector Rational)

type CommonRayWitness :: Type
data CommonRayWitness = CommonRayWitness !(DynVector Rational) !(DynVector Rational) !(DynVector Rational)

type ProjectiveSeparationCertificate :: Type
newtype ProjectiveSeparationCertificate = ProjectiveSeparationCertificate (DynVector Rational)

type NontrivialConeIntersection :: Type
-- | Intersection or separation after descent to the common unit-gauge section.
data NontrivialConeIntersection
  = ConesIntersect !CommonRayWitness
  | ConesProjectivelySeparated !ProjectiveSeparationCertificate

type LInfSectionDistance :: Type
-- | Exact coordinate L-infinity distance between two unit-gauge sections.
newtype LInfSectionDistance = LInfSectionDistance Rational
  deriving stock (Eq, Ord, Show)

generatedConeDimension :: GeneratedCone -> Int
generatedConeDimension (GeneratedCone dimension _) = dimension

generatedConeGenerators :: GeneratedCone -> [DynVector Rational]
generatedConeGenerators (GeneratedCone _ generators) = generators

nonNegativeCombinationCoefficients :: NonNegativeCombination -> DynVector Rational
nonNegativeCombinationCoefficients (NonNegativeCombination coefficients) = coefficients

coneSeparationFunctional :: ConeSeparationCertificate -> DynVector Rational
coneSeparationFunctional (ConeSeparationCertificate functional) = functional

commonPositiveGaugeFunctional :: CommonPositiveGauge -> DynVector Rational
commonPositiveGaugeFunctional (CommonPositiveGauge _ _ functional) = functional

commonPositiveGaugeLeftCone :: CommonPositiveGauge -> GeneratedCone
commonPositiveGaugeLeftCone (CommonPositiveGauge leftCone _ _) = leftCone

commonPositiveGaugeRightCone :: CommonPositiveGauge -> GeneratedCone
commonPositiveGaugeRightCone (CommonPositiveGauge _ rightCone _) = rightCone

commonRayPoint :: CommonRayWitness -> DynVector Rational
commonRayPoint (CommonRayWitness point _ _) = point

commonRayLeftCoefficients :: CommonRayWitness -> DynVector Rational
commonRayLeftCoefficients (CommonRayWitness _ leftCoefficients _) = leftCoefficients

commonRayRightCoefficients :: CommonRayWitness -> DynVector Rational
commonRayRightCoefficients (CommonRayWitness _ _ rightCoefficients) = rightCoefficients

projectiveSeparationFunctional :: ProjectiveSeparationCertificate -> DynVector Rational
projectiveSeparationFunctional (ProjectiveSeparationCertificate functional) = functional

linfSectionDistanceValue :: LInfSectionDistance -> Rational
linfSectionDistanceValue (LInfSectionDistance distanceValue) = distanceValue

mkGeneratedCone :: Int -> [DynVector Rational] -> Either ConeError GeneratedCone
mkGeneratedCone ambientDimension generators
  | ambientDimension < 0 = Left (ConeNegativeAmbientDimension ambientDimension)
  | otherwise = do
      traverse_ (validateGenerator ambientDimension) (zip [0 ..] generators)
      Right (GeneratedCone ambientDimension generators)

validateGenerator :: Int -> (Int, DynVector Rational) -> Either ConeError ()
validateGenerator expectedDimension (generatorIndex, generator)
  | actualDimension /= expectedDimension =
      Left
        (ConeGeneratorDimensionMismatch generatorIndex expectedDimension actualDimension)
  | all (== 0) (dynVectorToList generator) = Left (ConeZeroGenerator generatorIndex)
  | otherwise = Right ()
  where
    actualDimension = dynVectorLength generator

coneMember :: GeneratedCone -> DynVector Rational -> Either ConeError ConeMembership
coneMember cone target
  | dynVectorLength target /= generatedConeDimension cone =
      Left (ConeTargetDimensionMismatch (generatedConeDimension cone) (dynVectorLength target))
  | otherwise = do
      coefficientMatrix <- coneGeneratorMatrix cone
      problem <- first ConeFeasibilityFailed (mkStandardFeasibility coefficientMatrix target)
      decision <- first ConeFeasibilityFailed (decideLinearFeasibility problem)
      Right
        ( case decision of
            Feasible solution -> ConeMember (NonNegativeCombination (solutionVector solution))
            Infeasible certificate -> ConeSeparated (ConeSeparationCertificate (certificateVector certificate))
        )

mkCommonPositiveGauge ::
  GeneratedCone ->
  GeneratedCone ->
  DynVector Rational ->
  Either ConeError CommonPositiveGauge
mkCommonPositiveGauge leftCone rightCone functional
  | generatedConeDimension leftCone /= generatedConeDimension rightCone =
      Left (ConeAmbientDimensionMismatch (generatedConeDimension leftCone) (generatedConeDimension rightCone))
  | dynVectorLength functional /= generatedConeDimension leftCone =
      Left (ConeGaugeDimensionMismatch (generatedConeDimension leftCone) (dynVectorLength functional))
  | otherwise = do
      validateGaugeOnCone LeftCone functional leftCone
      validateGaugeOnCone RightCone functional rightCone
      Right
        (CommonPositiveGauge leftCone rightCone functional)

intersectGeneratedCones :: CommonPositiveGauge -> Either ConeError NontrivialConeIntersection
intersectGeneratedCones commonGauge = do
  leftNormalized <- normalizedGenerators commonGauge (commonPositiveGaugeLeftCone commonGauge)
  rightNormalized <- normalizedGenerators commonGauge (commonPositiveGaugeRightCone commonGauge)
  let ambientDimension = generatedConeDimension (commonPositiveGaugeLeftCone commonGauge)
      leftCount = length leftNormalized
      rightCount = length rightNormalized
  variableCount <- checkedConeSum leftCount rightCount
  equationCount <- checkedConeSum ambientDimension 1
  ambientRows <- columnsToRows ambientDimension (leftNormalized <> fmap (fmapVector negate) rightNormalized)
  let sectionRow = replicate leftCount 1 <> replicate rightCount 0
      rows = ambientRows <> [sectionRow]
      rhsValues = replicate ambientDimension 0 <> [1]
  coefficientMatrix <- first ConeCarrierConstructionFailed (mkDynMatrix equationCount variableCount (concat rows))
  rightHandSide <- first ConeCarrierConstructionFailed (mkDynVector equationCount rhsValues)
  problem <- first ConeFeasibilityFailed (mkStandardFeasibility coefficientMatrix rightHandSide)
  decision <- first ConeFeasibilityFailed (decideLinearFeasibility problem)
  case decision of
    Feasible solution -> do
      let (leftSectionWeights, rightSectionWeights) = splitAt leftCount (dynVectorToList (solutionVector solution))
      leftCoefficients <- originalGeneratorCoefficients commonGauge (commonPositiveGaugeLeftCone commonGauge) leftSectionWeights
      rightCoefficients <- originalGeneratorCoefficients commonGauge (commonPositiveGaugeRightCone commonGauge) rightSectionWeights
      leftVector <- first ConeCarrierConstructionFailed (mkDynVector leftCount leftCoefficients)
      rightVector <- first ConeCarrierConstructionFailed (mkDynVector rightCount rightCoefficients)
      pointValues <- linearCombination ambientDimension leftNormalized leftSectionWeights
      pointVector <- first ConeCarrierConstructionFailed (mkDynVector ambientDimension pointValues)
      let witness = CommonRayWitness pointVector leftVector rightVector
      if verifyCommonRayWitness commonGauge witness
        then Right (ConesIntersect witness)
        else Left ConeDerivedCertificateRejected
    Infeasible certificate -> do
      let certificateValues = dynVectorToList (certificateVector certificate)
          (functionalValues, affineValues) = splitAt ambientDimension certificateValues
      case affineValues of
        [-1] -> do
          functional <- first ConeCarrierConstructionFailed (mkDynVector ambientDimension functionalValues)
          if projectivelySeparates functional leftNormalized rightNormalized
            then Right (ConesProjectivelySeparated (ProjectiveSeparationCertificate functional))
            else Left ConeDerivedCertificateRejected
        _ -> Left ConeDerivedCertificateRejected

-- | Verify that both coefficient vectors reconstruct the unit-gauge point
-- from the original, unnormalized generators.
verifyCommonRayWitness :: CommonPositiveGauge -> CommonRayWitness -> Bool
verifyCommonRayWitness commonGauge witness =
  let leftCone = commonPositiveGaugeLeftCone commonGauge
      rightCone = commonPositiveGaugeRightCone commonGauge
      ambientDimension = generatedConeDimension leftCone
      pointValues = dynVectorToList (commonRayPoint witness)
      leftCoefficients = dynVectorToList (commonRayLeftCoefficients witness)
      rightCoefficients = dynVectorToList (commonRayRightCoefficients witness)
      pointHasUnitGauge = dot (dynVectorToList (commonPositiveGaugeFunctional commonGauge)) pointValues == 1
   in dynVectorLength (commonRayPoint witness) == ambientDimension
        && dynVectorLength (commonRayLeftCoefficients witness) == length (generatedConeGenerators leftCone)
        && dynVectorLength (commonRayRightCoefficients witness) == length (generatedConeGenerators rightCone)
        && all (>= 0) leftCoefficients
        && all (>= 0) rightCoefficients
        && pointHasUnitGauge
        && case
          ( linearCombination ambientDimension (fmap dynVectorToList (generatedConeGenerators leftCone)) leftCoefficients,
            linearCombination ambientDimension (fmap dynVectorToList (generatedConeGenerators rightCone)) rightCoefficients
          ) of
          (Right leftPoint, Right rightPoint) -> leftPoint == pointValues && rightPoint == pointValues
          _ -> False

coneSectionDistanceLInf :: CommonPositiveGauge -> Either ConeError LInfSectionDistance
coneSectionDistanceLInf commonGauge = do
  let leftCone = commonPositiveGaugeLeftCone commonGauge
      rightCone = commonPositiveGaugeRightCone commonGauge
  if null (generatedConeGenerators leftCone)
    then Left (ConeProjectiveSectionEmpty LeftCone)
    else
      if null (generatedConeGenerators rightCone)
        then Left (ConeProjectiveSectionEmpty RightCone)
        else do
          leftNormalized <- normalizedGenerators commonGauge leftCone
          rightNormalized <- normalizedGenerators commonGauge rightCone
          linearProgram <- sectionDistanceLinearProgram (generatedConeDimension leftCone) leftNormalized rightNormalized
          optimization <- first ConeFeasibilityFailed (optimizeLinearProgram linearProgram)
          case optimization of
            LinearProgramInfeasible _ -> Left ConeDistanceOptimizationInfeasible
            LinearProgramUnbounded _ -> Left ConeDistanceOptimizationUnbounded
            LinearProgramOptimal optimum ->
              let distanceValue = negate (exactLinearOptimumValue optimum)
               in if distanceValue >= 0
                    then Right (LInfSectionDistance distanceValue)
                    else Left ConeDerivedCertificateRejected

sectionDistanceLinearProgram ::
  Int ->
  [[Rational]] ->
  [[Rational]] ->
  Either ConeError StandardLinearProgram
sectionDistanceLinearProgram ambientDimension leftGenerators rightGenerators = do
  let leftCount = length leftGenerators
      rightCount = length rightGenerators
  baseCount <- checkedConeSum leftCount rightCount
  withDistanceCount <- checkedConeSum baseCount 1
  slackCount <- checkedConeSum ambientDimension ambientDimension
  variableCount <- checkedConeSum withDistanceCount slackCount
  sectionEquationCount <- checkedConeSum 2 ambientDimension
  equationCount <- checkedConeSum sectionEquationCount ambientDimension
  leftRows <- columnsToRows ambientDimension leftGenerators
  rightRows <- columnsToRows ambientDimension rightGenerators
  coordinateRows <-
    traverse
      (sectionDistanceCoordinateRows leftCount rightCount ambientDimension leftRows rightRows)
      [0 .. ambientDimension - 1]
  let leftSectionRow =
        replicate leftCount 1
          <> replicate rightCount 0
          <> [0]
          <> replicate slackCount 0
      rightSectionRow =
        replicate leftCount 0
          <> replicate rightCount 1
          <> [0]
          <> replicate slackCount 0
      rows = [leftSectionRow, rightSectionRow] <> concat coordinateRows
      rhsValues = [1, 1] <> replicate slackCount 0
      objectiveValues = replicate baseCount 0 <> [-1] <> replicate slackCount 0
  coefficientMatrix <- first ConeCarrierConstructionFailed (mkDynMatrix equationCount variableCount (concat rows))
  rightHandSide <- first ConeCarrierConstructionFailed (mkDynVector equationCount rhsValues)
  objective <- first ConeCarrierConstructionFailed (mkDynVector variableCount objectiveValues)
  feasibility <- first ConeFeasibilityFailed (mkStandardFeasibility coefficientMatrix rightHandSide)
  first ConeFeasibilityFailed (mkStandardLinearProgram feasibility objective)

sectionDistanceCoordinateRows ::
  Int ->
  Int ->
  Int ->
  [[Rational]] ->
  [[Rational]] ->
  Int ->
  Either ConeError [[Rational]]
sectionDistanceCoordinateRows leftCount rightCount ambientDimension leftRows rightRows coordinateIndex = do
  leftRow <- maybe (Left ConeDerivedCertificateRejected) Right (valueAt coordinateIndex leftRows)
  rightRow <- maybe (Left ConeDerivedCertificateRejected) Right (valueAt coordinateIndex rightRows)
  let positiveSlack = fmap (\candidate -> if candidate == coordinateIndex then 1 else 0) [0 .. ambientDimension - 1]
      negativeSlack = fmap (\candidate -> if candidate == coordinateIndex then 1 else 0) [0 .. ambientDimension - 1]
      positiveRow =
        take leftCount leftRow
          <> fmap negate (take rightCount rightRow)
          <> [-1]
          <> positiveSlack
          <> replicate ambientDimension 0
      negativeRow =
        fmap negate (take leftCount leftRow)
          <> take rightCount rightRow
          <> [-1]
          <> replicate ambientDimension 0
          <> negativeSlack
  Right [positiveRow, negativeRow]

validateGaugeOnCone :: ConeSide -> DynVector Rational -> GeneratedCone -> Either ConeError ()
validateGaugeOnCone side functional cone =
  traverse_
    ( \(generatorIndex, generator) ->
        let evaluation = dot (dynVectorToList functional) (dynVectorToList generator)
         in if evaluation > 0
              then Right ()
              else Left (ConeGaugeNotStrictlyPositive side generatorIndex evaluation)
    )
    (zip [0 ..] (generatedConeGenerators cone))

normalizedGenerators :: CommonPositiveGauge -> GeneratedCone -> Either ConeError [[Rational]]
normalizedGenerators commonGauge cone =
  traverse normalizeGenerator (generatedConeGenerators cone)
  where
    functionalValues = dynVectorToList (commonPositiveGaugeFunctional commonGauge)
    normalizeGenerator generator =
      let generatorValues = dynVectorToList generator
          gaugeValue = dot functionalValues generatorValues
       in if gaugeValue > 0
            then Right (fmap (/ gaugeValue) generatorValues)
            else Left ConeDerivedCertificateRejected

originalGeneratorCoefficients :: CommonPositiveGauge -> GeneratedCone -> [Rational] -> Either ConeError [Rational]
originalGeneratorCoefficients commonGauge cone sectionWeights =
  if length sectionWeights /= length (generatedConeGenerators cone)
    then Left ConeDerivedCertificateRejected
    else
      traverse
        ( \(generator, sectionWeight) ->
            let gaugeValue = dot functionalValues (dynVectorToList generator)
             in if gaugeValue > 0
                  then Right (sectionWeight / gaugeValue)
                  else Left ConeDerivedCertificateRejected
        )
        (zip (generatedConeGenerators cone) sectionWeights)
  where
    functionalValues = dynVectorToList (commonPositiveGaugeFunctional commonGauge)

coneGeneratorMatrix :: GeneratedCone -> Either ConeError (DynMatrix Rational)
coneGeneratorMatrix cone = do
  rows <- columnsToRows (generatedConeDimension cone) (fmap dynVectorToList (generatedConeGenerators cone))
  first
    ConeCarrierConstructionFailed
    (mkDynMatrix (generatedConeDimension cone) (length (generatedConeGenerators cone)) (concat rows))

columnsToRows :: Int -> [[Rational]] -> Either ConeError [[Rational]]
columnsToRows rowCount columns =
  traverse
    (\rowIndex -> traverse (maybe (Left ConeDerivedCertificateRejected) Right . valueAt rowIndex) columns)
    [0 .. rowCount - 1]

linearCombination :: Int -> [[Rational]] -> [Rational] -> Either ConeError [Rational]
linearCombination dimension vectors coefficients =
  if length vectors /= length coefficients
    then Left ConeDerivedCertificateRejected
    else
      traverse
        ( \coordinateIndex ->
            fmap sum
              ( traverse
                  ( \(vectorValues, coefficient) ->
                      fmap (coefficient *)
                        (maybe (Left ConeDerivedCertificateRejected) Right (valueAt coordinateIndex vectorValues))
                  )
                  (zip vectors coefficients)
              )
        )
        [0 .. dimension - 1]

projectivelySeparates :: DynVector Rational -> [[Rational]] -> [[Rational]] -> Bool
projectivelySeparates functional leftGenerators rightGenerators =
  let values = dynVectorToList functional
   in all (>= 1) (fmap (dot values) leftGenerators)
        && all (<= 0) (fmap (dot values) rightGenerators)

fmapVector :: (Rational -> Rational) -> [Rational] -> [Rational]
fmapVector = fmap

solutionVector :: NonNegativeSolution -> DynVector Rational
solutionVector = nonNegativeSolutionVector

certificateVector :: FarkasCertificate -> DynVector Rational
certificateVector = farkasCertificateVector

checkedConeSum :: Int -> Int -> Either ConeError Int
checkedConeSum left right =
  first (const ConeCardinalityOverflow) (checkedNonNegativeSum left right)

dot :: [Rational] -> [Rational] -> Rational
dot left right = sum (zipWith (*) left right)

valueAt :: Int -> [a] -> Maybe a
valueAt selected =
  foldr
    (\(candidate, value) remaining -> if candidate == selected then Just value else remaining)
    Nothing
    . zip [0 :: Int ..]
