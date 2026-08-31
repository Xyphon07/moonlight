{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Krylov.Internal
  ( validateSquareOperator,
    validateIterationCount,
    normalizeSeed,
    normalizeSeedBlock,
    unitVector,
    requireBasisVector,
    orthogonalizeAgainst,
    orthonormalizeBlock,
    blockInnerBlock,
    selfAdjointBlockInnerBlock,
    linearCombinationColumnsU,
    multiplyBasisByBlock,
    subtractBlocks,
    sparseColumnsToDenseRowVectors,
  )
where

import Control.Monad.ST (ST, runST)
import qualified Data.Vector as Box
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.VectorOps (dotU, normU, scaleU, subScaledU, subU)
import Moonlight.LinAlg.Pure.Operator
import Moonlight.LinAlg.Pure.Structured.BlockTridiagonal
  ( RowMajorBlock,
    mkRowMajorBlock,
    rowMajorBlockColumns,
    rowMajorBlockEntry,
    rowMajorBlockRows,
    symmetrizeRowMajorBlockLower,
  )
import Prelude

validateSquareOperator :: String -> LinearOperator symmetry -> Either MoonlightError ()
validateSquareOperator algorithmName op
  | rows <= 0 || cols <= 0 =
      Left (InvariantViolation (algorithmName <> " requires a positive square operator"))
  | rows /= cols =
      Left
        ( InvariantViolation
            ( algorithmName
                <> " requires a square operator, but received "
                <> show (rows, cols)
            )
        )
  | otherwise = Right ()
  where
    (rows, cols) = operatorShape op

validateIterationCount :: String -> Int -> Either MoonlightError Int
validateIterationCount algorithmName iterationCount
  | iterationCount <= 0 =
      Left (InvariantViolation (algorithmName <> " iteration count must be positive"))
  | otherwise = Right iterationCount

normalizeSeed :: String -> Int -> Double -> U.Vector Double -> Either MoonlightError (U.Vector Double)
normalizeSeed algorithmName dimension tolerance seedVector
  | U.null seedVector = Left (InvariantViolation (algorithmName <> " requires a non-empty seed vector"))
  | U.length seedVector /= dimension =
      Left
        ( InvariantViolation
            ( algorithmName
                <> " seed length mismatch: expected "
                <> show dimension
                <> " but received "
                <> show (U.length seedVector)
            )
        )
  | otherwise =
      let seedNorm = normU seedVector
       in if seedNorm <= tolerance
            then Left (InvariantViolation (algorithmName <> " seed vector is near-zero; provide a non-trivial seed"))
            else Right (scaleU (1.0 / seedNorm) seedVector)

normalizeSeedBlock ::
  String ->
  Int ->
  Double ->
  Int ->
  Box.Vector (U.Vector Double) ->
  Either MoonlightError (Box.Vector (U.Vector Double))
normalizeSeedBlock algorithmName dimension tolerance blockSize seedBlock
  | blockSize <= 0 =
      Left (InvariantViolation (algorithmName <> " block size must be positive"))
  | otherwise = do
      let candidateSeeds = Box.take blockSize (seedBlock <> canonicalSeeds dimension)
      acceptedSeeds <-
        U.foldM' (acceptSeed candidateSeeds) Box.empty (U.enumFromN 0 (Box.length candidateSeeds))
      if Box.null acceptedSeeds
        then Left (InvariantViolation (algorithmName <> " could not construct a non-zero orthogonal seed block"))
        else Right acceptedSeeds
  where
    acceptSeed candidateSeeds acceptedVectors seedIndex =
      case candidateSeeds Box.!? seedIndex of
        Nothing -> Left (InvariantViolation "block seed lookup failed")
        Just seedVector -> do
          normalizedSeed <- normalizeSeed algorithmName dimension tolerance seedVector
          (reducedSeed, _) <- orthogonalizeAgainst True acceptedVectors normalizedSeed
          let reducedNorm = normU reducedSeed
          pure
            ( if reducedNorm <= tolerance
                then acceptedVectors
                else acceptedVectors `Box.snoc` scaleU (1.0 / reducedNorm) reducedSeed
            )

canonicalSeeds :: Int -> Box.Vector (U.Vector Double)
canonicalSeeds dimension =
  Box.generate dimension (unitVector dimension)

unitVector :: Int -> Int -> U.Vector Double
unitVector dimension selectedIndex =
  U.generate dimension
    (\indexValue -> if indexValue == selectedIndex then 1.0 else 0.0)

requireBasisVector :: Int -> Box.Vector (U.Vector Double) -> Either MoonlightError (U.Vector Double)
requireBasisVector indexValue basisVectors =
  maybe
    (Left (InvariantViolation ("Krylov basis lookup failed at index " <> show indexValue)))
    Right
    (basisVectors Box.!? indexValue)

orthogonalizeAgainst ::
  Bool ->
  Box.Vector (U.Vector Double) ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double, U.Vector Double)
orthogonalizeAgainst reorthogonalize basisVectors inputVector =
  do
    (reducedOnce, coefficientsOnce) <- projectOnce basisVectors inputVector
    if reorthogonalize
      then do
        (reducedTwice, coefficientsTwice) <- projectOnce basisVectors reducedOnce
        coefficients <- addCoefficientVectors coefficientsOnce coefficientsTwice
        Right (reducedTwice, coefficients)
      else Right (reducedOnce, coefficientsOnce)

projectOnce ::
  Box.Vector (U.Vector Double) ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double, U.Vector Double)
projectOnce basisVectors inputVector =
  runST $ do
    coefficientValues <- MU.replicate (Box.length basisVectors) 0.0
    projectedVector <-
      U.foldM'
        (projectBasisIndex basisVectors coefficientValues)
        (Right inputVector)
        (U.enumFromN 0 (Box.length basisVectors))
    case projectedVector of
      Left err -> pure (Left err)
      Right reducedVector -> do
        frozenCoefficients <- U.freeze coefficientValues
        pure (Right (reducedVector, frozenCoefficients))

projectBasisIndex ::
  Box.Vector (U.Vector Double) ->
  MU.MVector s Double ->
  Either MoonlightError (U.Vector Double) ->
  Int ->
  ST s (Either MoonlightError (U.Vector Double))
projectBasisIndex basisVectors coefficientValues projectedVector basisIndex =
  case projectedVector of
    Left err -> pure (Left err)
    Right workingVector ->
      case basisVectors Box.!? basisIndex of
        Nothing -> pure (Left (InvariantViolation ("Krylov basis lookup failed at index " <> show basisIndex)))
        Just basisVector ->
          case dotU basisVector workingVector of
            Left err -> pure (Left err)
            Right coefficient ->
              case subScaledU workingVector coefficient basisVector of
                Left err -> pure (Left err)
                Right nextVector -> do
                  MU.unsafeWrite coefficientValues basisIndex coefficient
                  pure (Right nextVector)

addCoefficientVectors ::
  U.Vector Double ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double)
addCoefficientVectors left right =
  if U.length left == U.length right
    then Right (U.zipWith (+) left right)
    else
      Left
        ( InvariantViolation
            ( "coefficient vector length mismatch: left "
                <> show (U.length left)
                <> " right "
                <> show (U.length right)
            )
        )

orthonormalizeBlock ::
  Bool ->
  Double ->
  Box.Vector (U.Vector Double) ->
  Box.Vector (U.Vector Double) ->
  Either MoonlightError (Box.Vector (U.Vector Double))
orthonormalizeBlock reorthogonalize tolerance existingBasis candidateVectors =
  U.foldM' acceptCandidateIndex Box.empty (U.enumFromN 0 (Box.length candidateVectors))
  where
    acceptCandidateIndex acceptedVectors candidateIndex =
      case candidateVectors Box.!? candidateIndex of
        Nothing -> Left (InvariantViolation "block candidate lookup failed")
        Just candidateVector -> do
          let combinedBasis = existingBasis <> acceptedVectors
          (reducedVector, _) <- orthogonalizeAgainst reorthogonalize combinedBasis candidateVector
          let reducedNorm = normU reducedVector
          Right
            ( if reducedNorm <= tolerance
                then acceptedVectors
                else acceptedVectors `Box.snoc` scaleU (1.0 / reducedNorm) reducedVector
            )

blockInnerBlock ::
  Box.Vector (U.Vector Double) ->
  Box.Vector (U.Vector Double) ->
  Either MoonlightError RowMajorBlock
blockInnerBlock leftBasis rightBasis = do
  payload <-
    U.generateM
      (Box.length leftBasis * Box.length rightBasis)
      ( \payloadIndex ->
          let rightCount = Box.length rightBasis
              leftIndex = payloadIndex `quot` rightCount
              rightIndex = payloadIndex `rem` rightCount
           in case (leftBasis Box.!? leftIndex, rightBasis Box.!? rightIndex) of
                (Just leftVector, Just rightVector) -> dotU leftVector rightVector
                _ -> Left (InvariantViolation "block inner-product index out of bounds")
      )
  mkRowMajorBlock
    (Box.length leftBasis)
    (Box.length rightBasis)
    payload

selfAdjointBlockInnerBlock ::
  Box.Vector (U.Vector Double) ->
  Box.Vector (U.Vector Double) ->
  Either MoonlightError RowMajorBlock
selfAdjointBlockInnerBlock leftBasis rightBasis =
  blockInnerBlock leftBasis rightBasis >>= symmetrizeRowMajorBlockLower

linearCombinationColumnsU ::
  Box.Vector (U.Vector Double) ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double)
linearCombinationColumnsU basisColumns coefficients =
  case basisColumns Box.!? 0 of
    Nothing ->
      Left (InvariantViolation "basis linear combination requires a non-empty basis")
    Just firstColumn ->
      let basisCount = Box.length basisColumns
          coefficientCount = U.length coefficients
          ambientDimension = U.length firstColumn
       in if coefficientCount /= basisCount
            then
              Left
                ( InvariantViolation
                    ( "basis linear combination coefficient count mismatch: expected "
                        <> show basisCount
                        <> " but received "
                        <> show coefficientCount
                    )
                )
            else do
              _ <- Box.ifoldM' (validateLinearCombinationColumn ambientDimension) () basisColumns
              Right
                ( U.generate
                    ambientDimension
                    ( \entryIndex ->
                        Box.ifoldl'
                          (accumulateLinearCombinationEntry coefficients entryIndex)
                          0.0
                          basisColumns
                    )
                )

validateLinearCombinationColumn ::
  Int ->
  () ->
  Int ->
  U.Vector Double ->
  Either MoonlightError ()
validateLinearCombinationColumn ambientDimension () columnIndex columnValue =
  let actualDimension = U.length columnValue
   in if actualDimension == ambientDimension
        then Right ()
        else
          Left
            ( InvariantViolation
                ( "basis linear combination column "
                    <> show columnIndex
                    <> " has dimension "
                    <> show actualDimension
                    <> " but expected "
                    <> show ambientDimension
                )
            )

accumulateLinearCombinationEntry ::
  U.Vector Double ->
  Int ->
  Double ->
  Int ->
  U.Vector Double ->
  Double
accumulateLinearCombinationEntry coefficients entryIndex accumulator columnIndex columnVector =
  accumulator
    + coefficients `U.unsafeIndex` columnIndex
      * columnVector `U.unsafeIndex` entryIndex

multiplyBasisByBlock ::
  Box.Vector (U.Vector Double) ->
  RowMajorBlock ->
  Either MoonlightError (Box.Vector (U.Vector Double))
multiplyBasisByBlock basisVectors coefficientBlock
  | Box.length basisVectors /= rowMajorBlockRows coefficientBlock =
      Left (InvariantViolation "basis/vector coefficient block row count mismatch")
  | Box.null basisVectors =
      Left (InvariantViolation "basis/vector coefficient block requires a non-empty basis")
  | otherwise =
      Right
        ( Box.generate
            (rowMajorBlockColumns coefficientBlock)
            (basisCombinationColumn basisVectors coefficientBlock)
        )

basisCombinationColumn :: Box.Vector (U.Vector Double) -> RowMajorBlock -> Int -> U.Vector Double
basisCombinationColumn basisVectors coefficientBlock outputColumn =
  let ambientDimension =
        maybe 0 U.length (basisVectors Box.!? 0)
   in U.generate
        ambientDimension
        ( \entryIndex ->
            Box.ifoldl'
              ( \accumulator basisIndex basisVector ->
                  accumulator
                    + rowMajorBlockEntry coefficientBlock basisIndex outputColumn
                      * maybe 0.0 id (basisVector U.!? entryIndex)
              )
              0.0
              basisVectors
        )

subtractBlocks ::
  Box.Vector (U.Vector Double) ->
  Box.Vector (U.Vector Double) ->
  Either MoonlightError (Box.Vector (U.Vector Double))
subtractBlocks leftBlocks rightBlocks
  | Box.length leftBlocks /= Box.length rightBlocks =
      Left
        ( InvariantViolation
            ( "block vector count mismatch: left "
                <> show (Box.length leftBlocks)
                <> " right "
                <> show (Box.length rightBlocks)
            )
        )
  | otherwise =
      Box.generateM
        (Box.length leftBlocks)
        ( \blockIndex ->
            case (leftBlocks Box.!? blockIndex, rightBlocks Box.!? blockIndex) of
              (Just leftBlock, Just rightBlock) -> subU leftBlock rightBlock
              _ -> Left (InvariantViolation "block vector lookup failed")
        )

sparseColumnsToDenseRowVectors :: Int -> Int -> Box.Vector (U.Vector Double) -> Box.Vector (U.Vector Double)
sparseColumnsToDenseRowVectors rowCount columnCount columnValues =
  Box.generate
    rowCount
    ( \rowIndex ->
        U.generate
          columnCount
          (sparseEntryAt rowIndex)
    )
  where
    sparseEntryAt rowIndex columnIndex =
      maybe
        0.0
        (\columnValue -> maybe 0.0 id (columnValue U.!? rowIndex))
        (columnValues Box.!? columnIndex)
