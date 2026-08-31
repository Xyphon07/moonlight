{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Krylov.Decomposition
  ( ArnoldiDecomposition,
    mkArnoldiDecomposition,
    arnoldiBasisColumns,
    arnoldiHessenbergRows,
    arnoldiStepsCompleted,
    LanczosDecomposition,
    mkLanczosDecomposition,
    lanczosBasisColumns,
    lanczosProjectedTridiagonal,
    lanczosAlphaDiagonal,
    lanczosBetaOffDiagonal,
    lanczosResidualNorm,
    lanczosStepsCompleted,
    BlockLanczosDecomposition,
    mkBlockLanczosDecomposition,
    blockLanczosBasisColumns,
    blockLanczosProjectedBlockTridiagonal,
    blockLanczosBasisCount,
    blockLanczosBlockSteps,
  )
where

import Data.Kind (Type)
import Data.Vector qualified as Box
import Data.Vector.Unboxed qualified as U
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Pure.Structured.BlockTridiagonal
  ( SymmetricBlockTridiagonal,
    symmetricBlockTridiagonalDimension,
  )
import Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    symmetricTridiagonalDiagonalEntries,
    symmetricTridiagonalDimension,
    symmetricTridiagonalOffDiagonalEntries,
  )
import Prelude

type ArnoldiDecomposition :: Type
data ArnoldiDecomposition = ArnoldiDecomposition
  { arnoldiBasisColumns :: !(Box.Vector (U.Vector Double)),
    arnoldiHessenbergRows :: !(Box.Vector (U.Vector Double))
  }
  deriving stock (Eq, Show)

mkArnoldiDecomposition :: Box.Vector (U.Vector Double) -> Box.Vector (U.Vector Double) -> Either MoonlightError ArnoldiDecomposition
mkArnoldiDecomposition basisColumns hessenbergRows = do
  stepCount <- validateBasisColumns "Arnoldi" basisColumns
  if Box.length hessenbergRows /= stepCount + 1
    then
      Left
        ( InvariantViolation
            ( "Arnoldi Hessenberg row count mismatch: expected "
                <> show (stepCount + 1)
                <> " but received "
                <> show (Box.length hessenbergRows)
            )
        )
    else
      if any ((/= stepCount) . U.length) (Box.toList hessenbergRows)
        then Left (InvariantViolation "Arnoldi Hessenberg rows must have equal length matching the step count")
        else Right (ArnoldiDecomposition basisColumns hessenbergRows)

arnoldiStepsCompleted :: ArnoldiDecomposition -> Int
arnoldiStepsCompleted = Box.length . arnoldiBasisColumns

type LanczosDecomposition :: Type
data LanczosDecomposition = LanczosDecomposition
  { lanczosBasisColumns :: !(Box.Vector (U.Vector Double)),
    lanczosProjectedTridiagonal :: !SymmetricTridiagonal,
    lanczosResidualNorm :: !Double
  }
  deriving stock (Eq, Show)

mkLanczosDecomposition ::
  Box.Vector (U.Vector Double) ->
  SymmetricTridiagonal ->
  Double ->
  Either MoonlightError LanczosDecomposition
mkLanczosDecomposition basisColumns projectedTridiagonal residualNorm = do
  basisCount <- validateBasisColumns "Lanczos" basisColumns
  let projectedDimension = symmetricTridiagonalDimension projectedTridiagonal
  if projectedDimension /= basisCount
    then
      Left
        ( InvariantViolation
            ( "Lanczos projected tridiagonal dimension mismatch: expected "
                <> show basisCount
                <> " but received "
                <> show projectedDimension
            )
        )
    else Right (LanczosDecomposition basisColumns projectedTridiagonal residualNorm)

lanczosAlphaDiagonal :: LanczosDecomposition -> [Double]
lanczosAlphaDiagonal = symmetricTridiagonalDiagonalEntries . lanczosProjectedTridiagonal

lanczosBetaOffDiagonal :: LanczosDecomposition -> [Double]
lanczosBetaOffDiagonal = symmetricTridiagonalOffDiagonalEntries . lanczosProjectedTridiagonal

lanczosStepsCompleted :: LanczosDecomposition -> Int
lanczosStepsCompleted =
  symmetricTridiagonalDimension . lanczosProjectedTridiagonal

type BlockLanczosDecomposition :: Type
data BlockLanczosDecomposition = BlockLanczosDecomposition
  { blockLanczosBasisColumns :: !(Box.Vector (U.Vector Double)),
    blockLanczosProjectedBlockTridiagonal :: !SymmetricBlockTridiagonal,
    blockLanczosBlockSteps :: !Int
  }
  deriving stock (Eq, Show)

mkBlockLanczosDecomposition ::
  Box.Vector (U.Vector Double) ->
  SymmetricBlockTridiagonal ->
  Int ->
  Either MoonlightError BlockLanczosDecomposition
mkBlockLanczosDecomposition basisColumns projectedBlockTridiagonal blockStepCount = do
  basisCount <- validateBasisColumns "Block Lanczos" basisColumns
  let projectedDimension = symmetricBlockTridiagonalDimension projectedBlockTridiagonal
  if blockStepCount <= 0
    then Left (InvariantViolation "Block Lanczos step count must be positive")
    else
      if projectedDimension /= basisCount
        then
          Left
            ( InvariantViolation
                ( "Block Lanczos projected operator dimension mismatch: expected "
                    <> show basisCount
                    <> " but received "
                    <> show projectedDimension
                )
            )
        else Right (BlockLanczosDecomposition basisColumns projectedBlockTridiagonal blockStepCount)

blockLanczosBasisCount :: BlockLanczosDecomposition -> Int
blockLanczosBasisCount =
  symmetricBlockTridiagonalDimension . blockLanczosProjectedBlockTridiagonal

validateBasisColumns :: String -> Box.Vector (U.Vector Double) -> Either MoonlightError Int
validateBasisColumns algorithmName basisColumns =
  let basisCount = Box.length basisColumns
      basisDimensions = U.length <$> Box.toList basisColumns
      basisDimension =
        case basisDimensions of
          [] -> 0
          firstDimension : _ -> firstDimension
   in if basisCount <= 0
        then Left (InvariantViolation (algorithmName <> " basis must be non-empty"))
        else
          if any (/= basisDimension) basisDimensions
            then Left (InvariantViolation (algorithmName <> " basis columns must have equal length"))
            else Right basisCount
