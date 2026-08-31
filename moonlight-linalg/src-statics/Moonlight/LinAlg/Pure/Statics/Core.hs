{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.LinAlg.Pure.Statics.Core
  ( checkEquilibrium,
    solveGraphicStatics,
  )
where

import Control.Monad (join)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Pure.Dense.Rows (transposeRowsExact)
import Moonlight.LinAlg.Pure.Dense.Dynamic
  ( DynMatrix,
    DynVector,
    dynMatrixToRows,
    dynMatrixShape,
    dynVectorLength,
    dynVectorToList,
    fromDynMatrix,
    fromDynVector,
    mkDynMatrix,
    mkDynVector,
    toDynVector,
    withDynMatrix,
    withDynVector,
  )
import Moonlight.LinAlg.Internal.Primitives (matrixVectorProduct)
import Moonlight.LinAlg.Pure.Dense.Decomposition (qrDecompFullColumnRank)
import Moonlight.LinAlg.Pure.Dense.Solver (solveDirect)
import Moonlight.LinAlg.Pure.Statics.Algebra
  ( addVec3,
    axisVector,
    magnitudeVec3,
    memberEndpoints,
    memberTouchesNode,
    vec3Zero,
  )
import Moonlight.LinAlg.Pure.Statics.Compile (assembleEquilibriumEquations)
import Moonlight.LinAlg.Pure.Statics.Types
  ( CompiledEquilibrium,
    EquationRef (..),
    EquilibriumResult (..),
    EquilibriumSolution (..),
    EquilibriumViolation (..),
    ForceNetwork,
    ForceSign (..),
    MemberRef,
    NodeRef,
    UnknownForce (..),
    Vec3,
    compiledCoefficientMatrix,
    compiledEquationOrder,
    compiledFoundationOrder,
    compiledMemberOrder,
    compiledNodeOrder,
    compiledRightHandSide,
    compiledUnknownOrder,
  )
import Moonlight.LinAlg.Pure.Dense.Types
  ( Matrix,
    Vector,
    fromListVector,
    matrixToRows,
    toListVector,
  )
import Prelude

checkEquilibrium :: ForceNetwork -> Either MoonlightError EquilibriumResult
checkEquilibrium networkValue =
  assembleEquilibriumEquations networkValue >>= solveGraphicStatics

solveGraphicStatics :: CompiledEquilibrium -> Either MoonlightError EquilibriumResult
solveGraphicStatics compiledValue = do
  solvedUnknowns <- solveUnknowns compiledValue
  solutionValues <- pure (dynVectorToList solvedUnknowns)
  residualForces <- solveResiduals compiledValue solutionValues
  let solutionValue = interpretSolution compiledValue solutionValues residualForces
      violations = collectViolations compiledValue solutionValue
  pure
    ( maybe
        (InEquilibrium solutionValue)
        Disequilibrium
        violations
    )

solveUnknowns :: CompiledEquilibrium -> Either MoonlightError (DynVector Double)
solveUnknowns compiledValue =
  case componentNodeSets compiledValue of
    [] -> solveUnknownsDense compiledValue
    [_] -> solveUnknownsDense compiledValue
    components -> solveUnknownsByComponents components compiledValue

solveUnknownsDense :: CompiledEquilibrium -> Either MoonlightError (DynVector Double)
solveUnknownsDense compiledValue =
  solveDenseSystemByShape
    (compiledCoefficientMatrix compiledValue)
    (compiledRightHandSide compiledValue)

solveUnknownsByComponents :: [Set NodeRef] -> CompiledEquilibrium -> Either MoonlightError (DynVector Double)
solveUnknownsByComponents components compiledValue = do
  coefficientRows <- dynMatrixToRows (compiledCoefficientMatrix compiledValue)
  solvedEntries <-
    fmap concat
      ( traverse
          (solveComponentUnknowns compiledValue coefficientRows (dynVectorToList (compiledRightHandSide compiledValue)))
          components
      )
  let solvedMap = Map.fromList solvedEntries
      unknownCount = length (compiledUnknownOrder compiledValue)
  solutionValues <-
    traverse
      ( \unknownIndex ->
          maybe
            (Left (InvariantViolation ("graphic statics component solve omitted unknown index " <> show unknownIndex)))
            Right
            (Map.lookup unknownIndex solvedMap)
      )
      [0 .. unknownCount - 1]
  mkDynVector unknownCount solutionValues

solveComponentUnknowns ::
  CompiledEquilibrium ->
  [[Double]] ->
  [Double] ->
  Set NodeRef ->
  Either MoonlightError [(Int, Double)]
solveComponentUnknowns compiledValue coefficientRows rightHandSideValues componentNodes = do
  let equationEntries =
        filter
          ( \(_, equationRefValue) ->
              Set.member (equationNodeRef equationRefValue) componentNodes
          )
          (indexedValues (compiledEquationOrder compiledValue))
      unknownEntries =
        filter
          (unknownEntryInComponent componentNodes)
          (indexedValues (compiledUnknownOrder compiledValue))
  componentRows <-
    traverse
      ( \(equationIndex, _) -> do
          rowValues <- selectIndex "graphic statics component equation row" equationIndex coefficientRows
          traverse
            (\(unknownIndex, _) -> selectIndex "graphic statics component unknown column" unknownIndex rowValues)
            unknownEntries
      )
      equationEntries
  componentRightHandSide <-
    traverse
      (\(equationIndex, _) -> selectIndex "graphic statics component RHS" equationIndex rightHandSideValues)
      equationEntries
  componentMatrix <- mkDynMatrix (length equationEntries) (length unknownEntries) (concat componentRows)
  componentVector <- mkDynVector (length equationEntries) componentRightHandSide
  componentSolution <- solveComponentDenseSystem componentMatrix componentVector
  let solutionValues = dynVectorToList componentSolution
  if length solutionValues /= length unknownEntries
    then Left (InvariantViolation "graphic statics component solve returned wrong unknown count")
    else Right (zip (fst <$> unknownEntries) solutionValues)

solveComponentDenseSystem :: DynMatrix Double -> DynVector Double -> Either MoonlightError (DynVector Double)
solveComponentDenseSystem =
  solveDenseSystemByShape

solveDenseSystemByShape :: DynMatrix Double -> DynVector Double -> Either MoonlightError (DynVector Double)
solveDenseSystemByShape coefficientMatrix rightHandSide =
  let (rowCount, columnCount) = dynMatrixShape coefficientMatrix
   in if rowCount == columnCount && rowCount == dynVectorLength rightHandSide
        then solveSquareSystem coefficientMatrix rightHandSide
        else solveLeastSquares coefficientMatrix rightHandSide

componentNodeSets :: CompiledEquilibrium -> [Set NodeRef]
componentNodeSets compiledValue =
  Set.fromList . flattenSCC
    <$> stronglyConnComp
      ( (\nodeRefValue -> (nodeRefValue, nodeRefValue, Map.findWithDefault [] nodeRefValue adjacencyMap))
          <$> compiledNodeOrder compiledValue
      )
  where
    adjacencyMap =
      Map.fromListWith
        (<>)
        (componentMemberAdjacency =<< compiledMemberOrder compiledValue)

componentMemberAdjacency :: MemberRef -> [(NodeRef, [NodeRef])]
componentMemberAdjacency memberRefValue =
  case memberEndpoints memberRefValue of
    (leftRef, rightRef) ->
      [ (leftRef, [rightRef]),
        (rightRef, [leftRef])
      ]

flattenSCC :: SCC node -> [node]
flattenSCC component =
  case component of
    AcyclicSCC nodeValue -> [nodeValue]
    CyclicSCC nodeValues -> nodeValues

unknownEntryInComponent :: Set NodeRef -> (Int, UnknownForce) -> Bool
unknownEntryInComponent componentNodes (_, unknownValue) =
  case unknownValue of
    MemberUnknown memberRefValue ->
      case memberEndpoints memberRefValue of
        (leftRef, rightRef) ->
          Set.member leftRef componentNodes || Set.member rightRef componentNodes
    ReactionUnknown nodeRefValue _ ->
      Set.member nodeRefValue componentNodes

indexedValues :: [value] -> [(Int, value)]
indexedValues =
  zip [0 ..]

selectIndex :: String -> Int -> [value] -> Either MoonlightError value
selectIndex context indexValue values
  | indexValue < 0 =
      Left (InvariantViolation (context <> " index must be non-negative: " <> show indexValue))
  | otherwise =
      case drop indexValue values of
        value : _ -> Right value
        [] ->
          Left
            ( InvariantViolation
                ( context
                    <> " index out of bounds: index="
                    <> show indexValue
                    <> ", length="
                    <> show (length values)
                )
            )

solveSquareSystem :: DynMatrix Double -> DynVector Double -> Either MoonlightError (DynVector Double)
solveSquareSystem coefficientMatrix rightHandSide
  | rowCount /= columnCount =
      Left (InvariantViolation "graphic statics direct solve requires a square coefficient matrix")
  | rowCount /= dynVectorLength rightHandSide =
      Left (InvariantViolation "graphic statics direct solve RHS length mismatch")
  | otherwise =
      join
        ( withDynVector rightHandSide
            ( \(staticRightHandSide :: Vector n Double) -> do
                staticMatrix <- (fromDynMatrix coefficientMatrix :: Either MoonlightError (Matrix n n Double))
                toDynVector <$> solveDirect staticMatrix staticRightHandSide
            )
        )
  where
    (rowCount, columnCount) = dynMatrixShape coefficientMatrix

solveLeastSquares :: DynMatrix Double -> DynVector Double -> Either MoonlightError (DynVector Double)
solveLeastSquares coefficientMatrix rightHandSide
  | rowCount /= dynVectorLength rightHandSide =
      Left (InvariantViolation "graphic statics least-squares RHS length mismatch")
  | rowCount < columnCount =
      Left (InvariantViolation "graphic statics QR least-squares requires row count greater than or equal to unknown count")
  | otherwise =
      join
        ( withDynMatrix coefficientMatrix
            ( \(staticMatrix :: Matrix rows columns Double) -> do
                staticRightHandSide <- (fromDynVector rightHandSide :: Either MoonlightError (Vector rows Double))
                (qMatrix, rMatrix) <- qrDecompFullColumnRank staticMatrix
                qRows <- matrixToRows qMatrix
                qTransposeRows <- transposeRowsExact qRows
                projectedRightHandSideValues <- matrixVectorProduct qTransposeRows (toListVector staticRightHandSide)
                projectedRightHandSide <- fromListVector @columns projectedRightHandSideValues
                toDynVector <$> solveDirect rMatrix projectedRightHandSide
            )
        )
  where
    (rowCount, columnCount) = dynMatrixShape coefficientMatrix

solveResiduals :: CompiledEquilibrium -> [Double] -> Either MoonlightError (Map NodeRef Vec3)
solveResiduals compiledValue solvedUnknowns = do
  coefficientRows <- dynMatrixToRows (compiledCoefficientMatrix compiledValue)
  let rightHandSideValues = dynVectorToList (compiledRightHandSide compiledValue)
  predictedValues <- matrixVectorProduct coefficientRows solvedUnknowns
  if length predictedValues /= length rightHandSideValues
    then Left (InvariantViolation "graphic statics residual computation length mismatch")
    else
      pure
        ( foldl'
            accumulateResidual
            Map.empty
            ( zip
                (compiledEquationOrder compiledValue)
                (zipWith (-) predictedValues rightHandSideValues)
            )
        )

interpretSolution :: CompiledEquilibrium -> [Double] -> Map NodeRef Vec3 -> EquilibriumSolution
interpretSolution compiledValue solvedUnknowns residualForces =
  let solutionEntries = zip (compiledUnknownOrder compiledValue) solvedUnknowns
      (memberForces, reactionForces) =
        foldl'
          accumulateUnknown
          (Map.empty, Map.empty)
          solutionEntries
   in EquilibriumSolution
        { equilibriumMemberForces = memberForces,
          equilibriumReactionForces =
            foldl'
              (\reactionMap nodeRefValue -> Map.insertWith addVec3 nodeRefValue vec3Zero reactionMap)
              reactionForces
              (compiledFoundationOrder compiledValue),
          equilibriumResidualForces =
            foldl'
              (\residualMap nodeRefValue -> Map.insertWith addVec3 nodeRefValue vec3Zero residualMap)
              residualForces
              (compiledNodeOrder compiledValue)
        }

collectViolations :: CompiledEquilibrium -> EquilibriumSolution -> Maybe (NonEmpty EquilibriumViolation)
collectViolations compiledValue solutionValue =
  NonEmpty.nonEmpty
    ( foldMap
        (violationAtNode compiledValue solutionValue)
        (compiledNodeOrder compiledValue)
    )

violationAtNode :: CompiledEquilibrium -> EquilibriumSolution -> NodeRef -> [EquilibriumViolation]
violationAtNode compiledValue solutionValue nodeRefValue =
  let residualForce =
        Map.findWithDefault vec3Zero nodeRefValue (equilibriumResidualForces solutionValue)
      residualMagnitude = magnitudeVec3 residualForce
      memberDetails = incidentMembers compiledValue solutionValue nodeRefValue
      worstMember = strongestMember memberDetails
      tensionMember = strongestTension memberDetails
      selectedMember = maybe worstMember Just tensionMember
      selectedSign =
        fmap
          (\(_, forceValue) -> if forceValue < 0.0 then Tension else Compression)
          selectedMember
   in if residualMagnitude > equilibriumTolerance || tensionMember /= Nothing
        then
          [ EquilibriumViolation
              { violationNode = nodeRefValue,
                violationResidualForce = residualForce,
                violationResidualMagnitude = residualMagnitude,
                violationWorstMember = fmap fst selectedMember,
                violationMemberForceSign = selectedSign
              }
          ]
        else []

incidentMembers :: CompiledEquilibrium -> EquilibriumSolution -> NodeRef -> [(MemberRef, Double)]
incidentMembers compiledValue solutionValue nodeRefValue =
  fmap
    (\memberRefValue -> (memberRefValue, Map.findWithDefault 0.0 memberRefValue (equilibriumMemberForces solutionValue)))
    ( filter
        (memberTouchesNode nodeRefValue)
        (compiledMemberOrder compiledValue)
    )

strongestMember :: [(MemberRef, Double)] -> Maybe (MemberRef, Double)
strongestMember =
  foldl'
    ( \currentBest candidate ->
        case currentBest of
          Nothing -> Just candidate
          Just bestCandidate ->
            if abs (snd candidate) > abs (snd bestCandidate)
              then Just candidate
              else currentBest
    )
    Nothing

strongestTension :: [(MemberRef, Double)] -> Maybe (MemberRef, Double)
strongestTension =
  foldl'
    ( \currentBest candidate ->
        if snd candidate < (-equilibriumTolerance)
          then
            case currentBest of
              Nothing -> Just candidate
              Just bestCandidate ->
                if snd candidate < snd bestCandidate
                  then Just candidate
                  else currentBest
          else currentBest
    )
    Nothing

accumulateUnknown ::
  (Map MemberRef Double, Map NodeRef Vec3) ->
  (UnknownForce, Double) ->
  (Map MemberRef Double, Map NodeRef Vec3)
accumulateUnknown (memberForces, reactionForces) (unknownValue, magnitudeValue) =
  case unknownValue of
    MemberUnknown memberRefValue ->
      (Map.insert memberRefValue magnitudeValue memberForces, reactionForces)
    ReactionUnknown nodeRefValue axisValue ->
      ( memberForces,
        Map.insertWith addVec3 nodeRefValue (axisVector axisValue magnitudeValue) reactionForces
      )

accumulateResidual :: Map NodeRef Vec3 -> (EquationRef, Double) -> Map NodeRef Vec3
accumulateResidual residuals (equationRefValue, magnitudeValue) =
  Map.insertWith
    addVec3
    (equationNodeRef equationRefValue)
    (axisVector (equationAxis equationRefValue) magnitudeValue)
    residuals

equilibriumTolerance :: Double
equilibriumTolerance = 1.0e-8
