module Moonlight.Category.Pure.Simplicial.Validation.Internal
  ( SimplicialLawEquality,
    simplicialLawEq,
    SimplicialLawKind (..),
    allSimplicialLawKinds,
    SimplicialLawIndices (..),
    SimplicialLawObstruction (..),
    lawObstructionKind,
    SimplicialLawCarrier (..),
    SimplicialLawCheck,
    checkObstructions,
    dimensionsStrictlyBelow,
    checkFaceFaceLawBy,
    checkDegeneracyDegeneracyLawBy,
    checkFaceDegeneracyLawBy,
    checkSimplicialLawsBy,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Numeric.Natural (Natural)

type SimplicialLawEquality :: Type -> Type
type SimplicialLawEquality simplex = Maybe simplex -> Maybe simplex -> Bool

simplicialLawEq :: Eq simplex => SimplicialLawEquality simplex
simplicialLawEq = (==)

type SimplicialLawKind :: Type
data SimplicialLawKind
  = FaceFaceLaw
  | DegeneracyDegeneracyLaw
  | FaceDegeneracyLaw
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

allSimplicialLawKinds :: [SimplicialLawKind]
allSimplicialLawKinds = [minBound .. maxBound]

type SimplicialLawIndices :: Type
data SimplicialLawIndices
  = FaceFaceIndices Natural Natural
  | DegeneracyDegeneracyIndices Natural Natural
  | FaceDegeneracyIndices Natural Natural
  deriving stock (Eq, Ord, Show)

type SimplicialLawObstruction :: Type -> Type
data SimplicialLawObstruction simplex = SimplicialLawObstruction
  { lawObstructionDimension :: Natural,
    lawObstructionSimplex :: simplex,
    lawObstructionIndices :: SimplicialLawIndices,
    lawObstructionLeftResult :: Maybe simplex,
    lawObstructionRightResult :: Maybe simplex
  }
  deriving stock (Eq, Show)

lawIndicesKind :: SimplicialLawIndices -> SimplicialLawKind
lawIndicesKind indices =
  case indices of
    FaceFaceIndices _ _ -> FaceFaceLaw
    DegeneracyDegeneracyIndices _ _ -> DegeneracyDegeneracyLaw
    FaceDegeneracyIndices _ _ -> FaceDegeneracyLaw

lawObstructionKind :: SimplicialLawObstruction simplex -> SimplicialLawKind
lawObstructionKind =
  lawIndicesKind . lawObstructionIndices

type SimplicialLawCarrier :: Type -> Type
data SimplicialLawCarrier simplex = SimplicialLawCarrier
  { lawCarrierUpperBound :: Natural,
    lawCarrierSimplicesAtDimension :: Natural -> [simplex],
    lawCarrierFaceAtDimension :: Natural -> Natural -> simplex -> Maybe simplex,
    lawCarrierDegeneracyAtDimension :: Natural -> Natural -> simplex -> Maybe simplex
  }

type SimplicialLawCheck :: Type -> Type
type SimplicialLawCheck simplex = Either (NonEmpty (SimplicialLawObstruction simplex)) ()

checkObstructions :: [obstruction] -> Either (NonEmpty obstruction) ()
checkObstructions obstructions =
  case obstructions of
    [] -> Right ()
    firstObstruction : remainingObstructions -> Left (firstObstruction :| remainingObstructions)

dimensionsStrictlyBelow :: Natural -> [Natural]
dimensionsStrictlyBelow upperBound =
  if upperBound == 0
    then []
    else [0 .. upperBound - 1]

dimensionsAtLeastTwoBelowBound :: Natural -> [Natural]
dimensionsAtLeastTwoBelowBound upperBound =
  if upperBound < 2
    then []
    else [0 .. upperBound - 2]

obstructionUnlessEqual ::
  SimplicialLawEquality simplex ->
  Natural ->
  simplex ->
  SimplicialLawIndices ->
  Maybe simplex ->
  Maybe simplex ->
  [SimplicialLawObstruction simplex]
obstructionUnlessEqual areEqual dimensionValue simplexValue indices leftResult rightResult =
  if areEqual leftResult rightResult
    then []
    else
      [ SimplicialLawObstruction
          { lawObstructionDimension = dimensionValue,
            lawObstructionSimplex = simplexValue,
            lawObstructionIndices = indices,
            lawObstructionLeftResult = leftResult,
            lawObstructionRightResult = rightResult
          }
      ]

faceFaceLawObstructionsBy :: SimplicialLawEquality simplex -> SimplicialLawCarrier simplex -> [SimplicialLawObstruction simplex]
faceFaceLawObstructionsBy areEqual carrier =
  [ obstruction
  | dimensionValue <- [2 .. lawCarrierUpperBound carrier],
    simplexValue <- lawCarrierSimplicesAtDimension carrier dimensionValue,
    leftFaceIndex <- [0 .. dimensionValue - 1],
    rightFaceIndex <- [leftFaceIndex + 1 .. dimensionValue],
    let leftResult =
          lawCarrierFaceAtDimension carrier (dimensionValue - 1) leftFaceIndex
            =<< lawCarrierFaceAtDimension carrier dimensionValue rightFaceIndex simplexValue,
    let rightResult =
          lawCarrierFaceAtDimension carrier (dimensionValue - 1) (rightFaceIndex - 1)
            =<< lawCarrierFaceAtDimension carrier dimensionValue leftFaceIndex simplexValue,
    obstruction <-
      obstructionUnlessEqual
        areEqual
        dimensionValue
        simplexValue
        (FaceFaceIndices leftFaceIndex rightFaceIndex)
        leftResult
        rightResult
  ]

degeneracyDegeneracyLawObstructionsBy :: SimplicialLawEquality simplex -> SimplicialLawCarrier simplex -> [SimplicialLawObstruction simplex]
degeneracyDegeneracyLawObstructionsBy areEqual carrier =
  [ obstruction
  | dimensionValue <- dimensionsAtLeastTwoBelowBound (lawCarrierUpperBound carrier),
    simplexValue <- lawCarrierSimplicesAtDimension carrier dimensionValue,
    leftDegeneracyIndex <- [0 .. dimensionValue],
    rightDegeneracyIndex <- [leftDegeneracyIndex .. dimensionValue],
    let leftResult =
          lawCarrierDegeneracyAtDimension carrier (dimensionValue + 1) leftDegeneracyIndex
            =<< lawCarrierDegeneracyAtDimension carrier dimensionValue rightDegeneracyIndex simplexValue,
    let rightResult =
          lawCarrierDegeneracyAtDimension carrier (dimensionValue + 1) (rightDegeneracyIndex + 1)
            =<< lawCarrierDegeneracyAtDimension carrier dimensionValue leftDegeneracyIndex simplexValue,
    obstruction <-
      obstructionUnlessEqual
        areEqual
        dimensionValue
        simplexValue
        (DegeneracyDegeneracyIndices leftDegeneracyIndex rightDegeneracyIndex)
        leftResult
        rightResult
  ]

expectedFaceDegeneracy ::
  SimplicialLawCarrier simplex ->
  Natural ->
  simplex ->
  Natural ->
  Natural ->
  Maybe simplex
expectedFaceDegeneracy carrier dimensionValue simplexValue faceIndex degeneracyIndex
  | faceIndex < degeneracyIndex =
      lawCarrierDegeneracyAtDimension carrier (dimensionValue - 1) (degeneracyIndex - 1)
        =<< lawCarrierFaceAtDimension carrier dimensionValue faceIndex simplexValue
  | faceIndex == degeneracyIndex = Just simplexValue
  | faceIndex == degeneracyIndex + 1 = Just simplexValue
  | otherwise =
      lawCarrierDegeneracyAtDimension carrier (dimensionValue - 1) degeneracyIndex
        =<< lawCarrierFaceAtDimension carrier dimensionValue (faceIndex - 1) simplexValue

faceDegeneracyLawObstructionsBy :: SimplicialLawEquality simplex -> SimplicialLawCarrier simplex -> [SimplicialLawObstruction simplex]
faceDegeneracyLawObstructionsBy areEqual carrier =
  [ obstruction
  | dimensionValue <- dimensionsStrictlyBelow (lawCarrierUpperBound carrier),
    simplexValue <- lawCarrierSimplicesAtDimension carrier dimensionValue,
    degeneracyIndex <- [0 .. dimensionValue],
    faceIndex <- [0 .. dimensionValue + 1],
    let leftResult =
          lawCarrierFaceAtDimension carrier (dimensionValue + 1) faceIndex
            =<< lawCarrierDegeneracyAtDimension carrier dimensionValue degeneracyIndex simplexValue,
    let rightResult = expectedFaceDegeneracy carrier dimensionValue simplexValue faceIndex degeneracyIndex,
    obstruction <-
      obstructionUnlessEqual
        areEqual
        dimensionValue
        simplexValue
        (FaceDegeneracyIndices faceIndex degeneracyIndex)
        leftResult
        rightResult
  ]

checkFaceFaceLawBy :: SimplicialLawEquality simplex -> SimplicialLawCarrier simplex -> SimplicialLawCheck simplex
checkFaceFaceLawBy areEqual =
  checkObstructions . faceFaceLawObstructionsBy areEqual

checkDegeneracyDegeneracyLawBy :: SimplicialLawEquality simplex -> SimplicialLawCarrier simplex -> SimplicialLawCheck simplex
checkDegeneracyDegeneracyLawBy areEqual =
  checkObstructions . degeneracyDegeneracyLawObstructionsBy areEqual

checkFaceDegeneracyLawBy :: SimplicialLawEquality simplex -> SimplicialLawCarrier simplex -> SimplicialLawCheck simplex
checkFaceDegeneracyLawBy areEqual =
  checkObstructions . faceDegeneracyLawObstructionsBy areEqual

checkSimplicialLawsBy :: SimplicialLawEquality simplex -> SimplicialLawCarrier simplex -> SimplicialLawCheck simplex
checkSimplicialLawsBy areEqual carrier =
  checkObstructions
    ( faceFaceLawObstructionsBy areEqual carrier
        <> degeneracyDegeneracyLawObstructionsBy areEqual carrier
        <> faceDegeneracyLawObstructionsBy areEqual carrier
    )
