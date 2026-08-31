{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Internal.VectorOps
  ( dotU,
    normU,
    scaleU,
    subU,
    subScaledU,
    csrMatVecU,
    csrMatVecValidatedU,
    csrContiguousBandMatVecValidatedU,
    csrMatVecBoxedDouble,
    csrMatVecBoxedDoubleValidated,
  )
where

import qualified Data.Vector as Box
import Control.Monad.ST (runST)
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray
  ( indexByteArray,
    newByteArray,
    unsafeFreezeByteArray,
    writeByteArray,
  )
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed.Base as UB
import Moonlight.Core (MoonlightError (..))
import Prelude

dotU :: U.Vector Double -> U.Vector Double -> Either MoonlightError Double
dotU left right =
  if U.length left == U.length right
    then Right (U.sum (U.zipWith (*) left right))
    else
      Left
        ( InvariantViolation
            ( "unboxed vector dot length mismatch: left "
                <> show (U.length left)
                <> " right "
                <> show (U.length right)
            )
        )

normU :: U.Vector Double -> Double
normU vectorValue =
  sqrt (U.sum (U.map (\entry -> entry * entry) vectorValue))

scaleU :: Double -> U.Vector Double -> U.Vector Double
scaleU factor = U.map (factor *)

subU :: U.Vector Double -> U.Vector Double -> Either MoonlightError (U.Vector Double)
subU left = subScaledU left 1.0

subScaledU :: U.Vector Double -> Double -> U.Vector Double -> Either MoonlightError (U.Vector Double)
subScaledU left factor right =
  if U.length left == U.length right
    then Right (U.zipWith (\leftEntry rightEntry -> leftEntry - factor * rightEntry) left right)
    else
      Left
        ( InvariantViolation
            ( "unboxed vector subtraction length mismatch: left "
                <> show (U.length left)
                <> " right "
                <> show (U.length right)
            )
        )

csrMatVecU ::
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Double ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double)
csrMatVecU rowCount rowOffsets columnIndices values inputVector =
  validateCSRKernelVectors rowCount rowOffsets columnIndices values inputVector
    *> Right (csrMatVecValidatedU rowCount rowOffsets columnIndices values inputVector)

csrMatVecValidatedU ::
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double
csrMatVecValidatedU
  rowCount
  (UB.V_Int (P.Vector rowOffsetBase _ rowOffsetArray))
  (UB.V_Int (P.Vector columnBase _ columnArray))
  (UB.V_Double (P.Vector coefficientBase _ coefficientArray))
  (UB.V_Double (P.Vector inputBase _ inputArray)) =
    UB.V_Double
      ( P.Vector
          0
          rowCount
          ( runST $ do
              targetArray <-
                newByteArray
                  (rowCount * sizeOf (0.0 :: Double))

              let writeRows !rowIndex
                    | rowIndex >= rowCount =
                        unsafeFreezeByteArray targetArray
                    | otherwise = do
                        let !startIndex =
                              indexByteArray
                                rowOffsetArray
                                (rowOffsetBase + rowIndex)
                            !stopIndex =
                              indexByteArray
                                rowOffsetArray
                                (rowOffsetBase + rowIndex + 1)
                            !rowValue =
                              accumulateRow startIndex stopIndex (0.0 :: Double)
                        writeByteArray targetArray rowIndex rowValue
                        writeRows (rowIndex + 1)

                  accumulateRow :: Int -> Int -> Double -> Double
                  accumulateRow !entryIndex !stopIndex !accumulator
                    | entryIndex >= stopIndex = accumulator
                    | otherwise =
                        let !columnIndex =
                              indexByteArray
                                columnArray
                                (columnBase + entryIndex)
                            !coefficient =
                              ( indexByteArray
                                  coefficientArray
                                  (coefficientBase + entryIndex)
                                  :: Double
                              )
                            !inputValue =
                              ( indexByteArray
                                  inputArray
                                  (inputBase + columnIndex)
                                  :: Double
                              )
                         in accumulateRow
                              (entryIndex + 1)
                              stopIndex
                              (accumulator + coefficient * inputValue)

              writeRows 0
          )
      )
{-# INLINE csrMatVecValidatedU #-}

csrContiguousBandMatVecValidatedU ::
  Int ->
  Int ->
  Int ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double
csrContiguousBandMatVecValidatedU
  rowCount
  lowerBandwidth
  upperBandwidth
  coefficients
  inputVector
    | lowerBandwidth == 2
        && upperBandwidth == 2
        && rowCount >= 5 =
        csrPentadiagonalMatVecValidatedU
          rowCount
          coefficients
          inputVector
    | lowerBandwidth == 1
        && upperBandwidth == 1
        && rowCount >= 3 =
        csrTridiagonalMatVecValidatedU
          rowCount
          coefficients
          inputVector
    | otherwise =
        csrContiguousBandMatVecGeneralValidatedU
          rowCount
          lowerBandwidth
          upperBandwidth
          coefficients
          inputVector
{-# INLINE csrContiguousBandMatVecValidatedU #-}

csrPentadiagonalMatVecValidatedU ::
  Int ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double
csrPentadiagonalMatVecValidatedU
  rowCount
  (UB.V_Double (P.Vector coefficientBase _ coefficientArray))
  (UB.V_Double (P.Vector inputBase _ inputArray)) =
    UB.V_Double
      ( P.Vector
          0
          rowCount
          ( runST $ do
              targetArray <-
                newByteArray
                  (rowCount * sizeOf (0.0 :: Double))

              let coefficientAt !indexValue =
                    ( indexByteArray
                        coefficientArray
                        (coefficientBase + indexValue)
                        :: Double
                    )
                  inputAt !indexValue =
                    ( indexByteArray
                        inputArray
                        (inputBase + indexValue)
                        :: Double
                    )
                  !row0 =
                    coefficientAt 0 * inputAt 0
                      + coefficientAt 1 * inputAt 1
                      + coefficientAt 2 * inputAt 2
                  !row1 =
                    coefficientAt 3 * inputAt 0
                      + coefficientAt 4 * inputAt 1
                      + coefficientAt 5 * inputAt 2
                      + coefficientAt 6 * inputAt 3

              writeByteArray targetArray 0 row0
              writeByteArray targetArray 1 row1

              let writeInterior !rowIndex
                    | rowIndex + 2 >= rowCount = pure ()
                    | otherwise = do
                        let !entryIndex = 5 * rowIndex - 3
                            !rowValue =
                              coefficientAt entryIndex
                                * inputAt (rowIndex - 2)
                                + coefficientAt (entryIndex + 1)
                                  * inputAt (rowIndex - 1)
                                + coefficientAt (entryIndex + 2)
                                  * inputAt rowIndex
                                + coefficientAt (entryIndex + 3)
                                  * inputAt (rowIndex + 1)
                                + coefficientAt (entryIndex + 4)
                                  * inputAt (rowIndex + 2)
                        writeByteArray targetArray rowIndex rowValue
                        writeInterior (rowIndex + 1)

              writeInterior 2

              let !penultimateRow = rowCount - 2
                  !penultimateEntry = 5 * rowCount - 13
                  !penultimateValue =
                    coefficientAt penultimateEntry
                      * inputAt (rowCount - 4)
                      + coefficientAt (penultimateEntry + 1)
                        * inputAt (rowCount - 3)
                      + coefficientAt (penultimateEntry + 2)
                        * inputAt (rowCount - 2)
                      + coefficientAt (penultimateEntry + 3)
                        * inputAt (rowCount - 1)
                  !lastRow = rowCount - 1
                  !lastEntry = 5 * rowCount - 9
                  !lastValue =
                    coefficientAt lastEntry
                      * inputAt (rowCount - 3)
                      + coefficientAt (lastEntry + 1)
                        * inputAt (rowCount - 2)
                      + coefficientAt (lastEntry + 2)
                        * inputAt (rowCount - 1)

              writeByteArray targetArray penultimateRow penultimateValue
              writeByteArray targetArray lastRow lastValue
              unsafeFreezeByteArray targetArray
          )
      )
{-# INLINE csrPentadiagonalMatVecValidatedU #-}

csrTridiagonalMatVecValidatedU ::
  Int ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double
csrTridiagonalMatVecValidatedU
  rowCount
  (UB.V_Double (P.Vector coefficientBase _ coefficientArray))
  (UB.V_Double (P.Vector inputBase _ inputArray)) =
    UB.V_Double
      ( P.Vector
          0
          rowCount
          ( runST $ do
              targetArray <-
                newByteArray
                  (rowCount * sizeOf (0.0 :: Double))

              let coefficientAt !indexValue =
                    ( indexByteArray
                        coefficientArray
                        (coefficientBase + indexValue)
                        :: Double
                    )
                  inputAt !indexValue =
                    ( indexByteArray
                        inputArray
                        (inputBase + indexValue)
                        :: Double
                    )
                  !firstValue =
                    coefficientAt 0 * inputAt 0
                      + coefficientAt 1 * inputAt 1

              writeByteArray targetArray 0 firstValue

              let writeInterior !rowIndex
                    | rowIndex + 1 >= rowCount = pure ()
                    | otherwise = do
                        let !entryIndex = 3 * rowIndex - 1
                            !rowValue =
                              coefficientAt entryIndex
                                * inputAt (rowIndex - 1)
                                + coefficientAt (entryIndex + 1)
                                  * inputAt rowIndex
                                + coefficientAt (entryIndex + 2)
                                  * inputAt (rowIndex + 1)
                        writeByteArray targetArray rowIndex rowValue
                        writeInterior (rowIndex + 1)

              writeInterior 1

              let !lastRow = rowCount - 1
                  !lastEntry = 3 * rowCount - 4
                  !lastValue =
                    coefficientAt lastEntry
                      * inputAt (rowCount - 2)
                      + coefficientAt (lastEntry + 1)
                        * inputAt (rowCount - 1)
              writeByteArray targetArray lastRow lastValue
              unsafeFreezeByteArray targetArray
          )
      )
{-# INLINE csrTridiagonalMatVecValidatedU #-}

csrContiguousBandMatVecGeneralValidatedU ::
  Int ->
  Int ->
  Int ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double
csrContiguousBandMatVecGeneralValidatedU
  rowCount
  lowerBandwidth
  upperBandwidth
  (UB.V_Double (P.Vector coefficientBase _ coefficientArray))
  (UB.V_Double (P.Vector inputBase _ inputArray)) =
    UB.V_Double
      ( P.Vector
          0
          rowCount
          ( runST $ do
              targetArray <-
                newByteArray
                  (rowCount * sizeOf (0.0 :: Double))

              let writeRows !rowIndex !entryIndex
                    | rowIndex >= rowCount =
                        unsafeFreezeByteArray targetArray
                    | otherwise = do
                        let !firstColumn =
                              max 0 (rowIndex - lowerBandwidth)
                            !lastColumn =
                              min
                                (rowCount - 1)
                                (rowIndex + upperBandwidth)
                            !entryCount =
                              lastColumn - firstColumn + 1
                            !rowValue =
                              accumulateBand
                                entryIndex
                                firstColumn
                                entryCount
                                0.0
                        writeByteArray targetArray rowIndex rowValue
                        writeRows
                          (rowIndex + 1)
                          (entryIndex + entryCount)

                  accumulateBand :: Int -> Int -> Int -> Double -> Double
                  accumulateBand
                    !entryIndex
                    !columnIndex
                    !remaining
                    !accumulator
                      | remaining <= 0 = accumulator
                      | otherwise =
                          let !coefficient =
                                ( indexByteArray
                                    coefficientArray
                                    (coefficientBase + entryIndex)
                                    :: Double
                                )
                              !inputValue =
                                ( indexByteArray
                                    inputArray
                                    (inputBase + columnIndex)
                                    :: Double
                                )
                           in accumulateBand
                                (entryIndex + 1)
                                (columnIndex + 1)
                                (remaining - 1)
                                (accumulator + coefficient * inputValue)

              writeRows 0 0
          )
      )
{-# INLINE csrContiguousBandMatVecGeneralValidatedU #-}

csrMatVecBoxedDouble ::
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  Box.Vector Double ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double)
csrMatVecBoxedDouble rowCount rowOffsets columnIndices values inputVector =
  validateCSRBoxedDoubleKernelVectors rowCount rowOffsets columnIndices values inputVector
    *> Right (csrMatVecBoxedDoubleValidated rowCount rowOffsets columnIndices values inputVector)

csrMatVecBoxedDoubleValidated ::
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  Box.Vector Double ->
  U.Vector Double ->
  U.Vector Double
csrMatVecBoxedDoubleValidated rowCount rowOffsets columnIndices values inputVector =
  U.create $ do
    targetVector <- MU.unsafeNew rowCount
    let writeRows !rowIndex
          | rowIndex >= rowCount = pure targetVector
          | otherwise = do
              let !startIndex = rowOffsets `U.unsafeIndex` rowIndex
                  !stopIndex = rowOffsets `U.unsafeIndex` (rowIndex + 1)
                  !rowValue = accumulateRow startIndex stopIndex 0.0
              MU.unsafeWrite targetVector rowIndex rowValue
              writeRows (rowIndex + 1)

        accumulateRow !entryIndex !stopIndex !accumulator
          | entryIndex >= stopIndex = accumulator
          | otherwise =
              let !columnIndex = columnIndices `U.unsafeIndex` entryIndex
                  !coefficient = values `Box.unsafeIndex` entryIndex
                  !inputValue = inputVector `U.unsafeIndex` columnIndex
               in accumulateRow
                    (entryIndex + 1)
                    stopIndex
                    (accumulator + coefficient * inputValue)

    writeRows 0
{-# INLINE csrMatVecBoxedDoubleValidated #-}

validateCSRKernelVectors ::
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Double ->
  U.Vector Double ->
  Either MoonlightError ()
validateCSRKernelVectors rowCount rowOffsets columnIndices values inputVector
  = validateCSRKernelShape rowCount rowOffsets columnIndices (U.length values) inputVector

validateCSRBoxedDoubleKernelVectors ::
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  Box.Vector Double ->
  U.Vector Double ->
  Either MoonlightError ()
validateCSRBoxedDoubleKernelVectors rowCount rowOffsets columnIndices values inputVector =
  validateCSRKernelShape rowCount rowOffsets columnIndices (Box.length values) inputVector

validateCSRKernelShape ::
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  Int ->
  U.Vector Double ->
  Either MoonlightError ()
validateCSRKernelShape rowCount rowOffsets columnIndices entryCount inputVector
  | rowCount < 0 = Left (InvariantViolation "CSR matvec row count must be non-negative")
  | U.length rowOffsets /= rowCount + 1 =
      Left
        ( InvariantViolation
            ( "CSR row offset length mismatch: expected "
                <> show (rowCount + 1)
                <> " but received "
                <> show (U.length rowOffsets)
            )
        )
  | U.length columnIndices /= entryCount =
      Left
        ( InvariantViolation
            ( "CSR column/value length mismatch: "
                <> show (U.length columnIndices)
                <> " columns but "
                <> show entryCount
                <> " values"
            )
        )
  | not offsetsValid = Left (InvariantViolation "CSR row offsets are not a valid nondecreasing range")
  | not columnsValid = Left (InvariantViolation "CSR column index out of input-vector bounds")
  | otherwise = Right ()
  where
    offsetsValid =
      maybe False (== 0) (rowOffsets U.!? 0)
        && maybe False (== entryCount) (rowOffsets U.!? rowCount)
        && U.and (U.zipWith (<=) rowOffsets (U.drop 1 rowOffsets))
        && U.all (\offsetValue -> offsetValue >= 0 && offsetValue <= entryCount) rowOffsets
    inputLength = U.length inputVector
    columnsValid = U.all (\columnIndex -> columnIndex >= 0 && columnIndex < inputLength) columnIndices
