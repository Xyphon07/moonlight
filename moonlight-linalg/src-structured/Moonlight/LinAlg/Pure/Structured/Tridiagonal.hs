{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    mkSymmetricTridiagonal,
    mkSymmetricTridiagonalVectors,
    pathLaplacianBands,
    symmetricTridiagonalDimension,
    symmetricTridiagonalDiagonalEntries,
    symmetricTridiagonalOffDiagonalEntries,
    symmetricTridiagonalDiagonalVector,
    symmetricTridiagonalOffDiagonalVector,
    applyPathLaplacianValidatedU,
    applySymmetricTridiagonalU,
    applySymmetricTridiagonalValidatedU,
    isPathLaplacianTridiagonal,
    symmetricTridiagonalUpperBound,
  )
where

import Control.Monad.ST (runST)
import Data.Kind (Type)
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
import Moonlight.Core (MoonlightError (..), fieldValueValid)
import Prelude

type SymmetricTridiagonal :: Type
data SymmetricTridiagonal = SymmetricTridiagonal
  { symmetricTridiagonalDiagonalVector :: !(U.Vector Double),
    symmetricTridiagonalOffDiagonalVector :: !(U.Vector Double)
  }
  deriving stock (Eq, Show)

mkSymmetricTridiagonal :: [Double] -> [Double] -> Either MoonlightError SymmetricTridiagonal
mkSymmetricTridiagonal diagonalValues offDiagonalValues =
  mkSymmetricTridiagonalVectors
    (U.fromList diagonalValues)
    (U.fromList offDiagonalValues)

mkSymmetricTridiagonalVectors ::
  U.Vector Double ->
  U.Vector Double ->
  Either MoonlightError SymmetricTridiagonal
mkSymmetricTridiagonalVectors diagonalValues offDiagonalValues =
  let matrixSize = U.length diagonalValues
      expectedOffDiagonalCount = max 0 (matrixSize - 1)
   in if U.length offDiagonalValues /= expectedOffDiagonalCount
        then
          Left
            ( InvariantViolation
                ( "Symmetric tridiagonal off-diagonal length mismatch: expected "
                    <> show expectedOffDiagonalCount
                    <> " but received "
                    <> show (U.length offDiagonalValues)
                )
            )
        else
          if U.any (not . fieldValueValid) diagonalValues || U.any (not . fieldValueValid) offDiagonalValues
            then Left (InvariantViolation "Symmetric tridiagonal entries must be finite")
            else
              Right
                SymmetricTridiagonal
                  { symmetricTridiagonalDiagonalVector = diagonalValues,
                    symmetricTridiagonalOffDiagonalVector = offDiagonalValues
                  }

pathLaplacianBands :: Int -> Either MoonlightError ([Double], [Double])
pathLaplacianBands dimension
  | dimension < 0 =
      Left
        ( InvariantViolation
            ( "path Laplacian dimension must be non-negative, received "
                <> show dimension
            )
        )
  | dimension == 0 = Right ([], [])
  | dimension == 1 = Right ([0.0], [])
  | otherwise =
      Right
        ( 1.0 : (replicate (dimension - 2) 2.0 <> [1.0]),
          replicate (dimension - 1) (-1.0)
        )

symmetricTridiagonalDimension :: SymmetricTridiagonal -> Int
symmetricTridiagonalDimension =
  U.length . symmetricTridiagonalDiagonalVector

symmetricTridiagonalDiagonalEntries :: SymmetricTridiagonal -> [Double]
symmetricTridiagonalDiagonalEntries =
  U.toList . symmetricTridiagonalDiagonalVector

symmetricTridiagonalOffDiagonalEntries :: SymmetricTridiagonal -> [Double]
symmetricTridiagonalOffDiagonalEntries =
  U.toList . symmetricTridiagonalOffDiagonalVector

applyPathLaplacianValidatedU ::
  Int ->
  U.Vector Double ->
  U.Vector Double
applyPathLaplacianValidatedU dimension inputVector
  | dimension <= 0 = U.empty
  | dimension == 1 = U.singleton 0.0
  | otherwise =
      U.create $ do
        targetVector <- MU.unsafeNew dimension
        let !firstValue = inputVector `U.unsafeIndex` 0
            !secondValue = inputVector `U.unsafeIndex` 1
        MU.unsafeWrite targetVector 0 (firstValue - secondValue)

        let writeInterior !rowIndex
              | rowIndex + 1 >= dimension = pure ()
              | otherwise = do
                  let !leftValue = inputVector `U.unsafeIndex` (rowIndex - 1)
                      !centerValue = inputVector `U.unsafeIndex` rowIndex
                      !rightValue = inputVector `U.unsafeIndex` (rowIndex + 1)
                  MU.unsafeWrite
                    targetVector
                    rowIndex
                    (2.0 * centerValue - leftValue - rightValue)
                  writeInterior (rowIndex + 1)

        writeInterior 1

        let !lastIndex = dimension - 1
            !lastValue = inputVector `U.unsafeIndex` lastIndex
            !penultimateValue = inputVector `U.unsafeIndex` (lastIndex - 1)
        MU.unsafeWrite targetVector lastIndex (lastValue - penultimateValue)
        pure targetVector
{-# INLINE applyPathLaplacianValidatedU #-}

applySymmetricTridiagonalU ::
  SymmetricTridiagonal ->
  U.Vector Double ->
  U.Vector Double
applySymmetricTridiagonalU = applySymmetricTridiagonalValidatedU
{-# INLINE applySymmetricTridiagonalU #-}

applySymmetricTridiagonalValidatedU ::
  SymmetricTridiagonal ->
  U.Vector Double ->
  U.Vector Double
applySymmetricTridiagonalValidatedU
  ( SymmetricTridiagonal
      (UB.V_Double (P.Vector diagonalBase matrixSize diagonalArray))
      (UB.V_Double (P.Vector offDiagonalBase _ offDiagonalArray))
    )
  (UB.V_Double (P.Vector inputBase _ inputArray))
  | matrixSize <= 0 = U.empty
  | matrixSize == 1 =
      U.singleton
        ( (indexByteArray diagonalArray diagonalBase :: Double)
            * (indexByteArray inputArray inputBase :: Double)
        )
  | otherwise =
      UB.V_Double
        ( P.Vector
            0
            matrixSize
            ( runST $ do
                targetArray <-
                  newByteArray
                    (matrixSize * sizeOf (0.0 :: Double))

                let !firstInput =
                      ( indexByteArray inputArray inputBase
                          :: Double
                      )
                    !secondInput =
                      ( indexByteArray inputArray (inputBase + 1)
                          :: Double
                      )
                    !firstValue =
                      ( indexByteArray diagonalArray diagonalBase
                          :: Double
                      )
                        * firstInput
                        + ( indexByteArray
                              offDiagonalArray
                              offDiagonalBase
                              :: Double
                          )
                          * secondInput
                writeByteArray targetArray 0 firstValue

                let writeRows !rowIndex !previousInput !currentInput
                      | rowIndex + 1 >= matrixSize = do
                          let !lastValue =
                                ( indexByteArray
                                    offDiagonalArray
                                    (offDiagonalBase + rowIndex - 1)
                                    :: Double
                                )
                                  * previousInput
                                  + ( indexByteArray
                                        diagonalArray
                                        (diagonalBase + rowIndex)
                                        :: Double
                                    )
                                    * currentInput
                          writeByteArray
                            targetArray
                            rowIndex
                            lastValue
                          unsafeFreezeByteArray targetArray
                      | otherwise = do
                          let !nextInput =
                                ( indexByteArray
                                    inputArray
                                    (inputBase + rowIndex + 1)
                                    :: Double
                                )
                              !rowValue =
                                ( indexByteArray
                                    offDiagonalArray
                                    (offDiagonalBase + rowIndex - 1)
                                    :: Double
                                )
                                  * previousInput
                                  + ( indexByteArray
                                        diagonalArray
                                        (diagonalBase + rowIndex)
                                        :: Double
                                    )
                                    * currentInput
                                  + ( indexByteArray
                                        offDiagonalArray
                                        (offDiagonalBase + rowIndex)
                                        :: Double
                                    )
                                    * nextInput
                          writeByteArray
                            targetArray
                            rowIndex
                            rowValue
                          writeRows
                            (rowIndex + 1)
                            currentInput
                            nextInput

                writeRows 1 firstInput secondInput
            )
        )
{-# INLINE applySymmetricTridiagonalValidatedU #-}

isPathLaplacianTridiagonal :: SymmetricTridiagonal -> Bool
isPathLaplacianTridiagonal tridiagonalValue =
  diagonalLoop 0 && U.all (== (-1.0)) offDiagonalValues
  where
    !diagonalValues = symmetricTridiagonalDiagonalVector tridiagonalValue
    !offDiagonalValues = symmetricTridiagonalOffDiagonalVector tridiagonalValue
    !matrixSize = U.length diagonalValues

    expectedDiagonal !rowIndex
      | matrixSize == 1 = 0.0
      | rowIndex == 0 || rowIndex + 1 == matrixSize = 1.0
      | otherwise = 2.0

    diagonalLoop !rowIndex
      | rowIndex >= matrixSize = True
      | diagonalValues `U.unsafeIndex` rowIndex == expectedDiagonal rowIndex =
          diagonalLoop (rowIndex + 1)
      | otherwise = False
{-# INLINE isPathLaplacianTridiagonal #-}

symmetricTridiagonalUpperBound :: SymmetricTridiagonal -> Double
symmetricTridiagonalUpperBound tridiagonalValue =
  let matrixSize = symmetricTridiagonalDimension tridiagonalValue
   in if matrixSize <= 0
        then 0.0
        else U.maximum (U.generate matrixSize (rowUpperBound tridiagonalValue))

rowUpperBound :: SymmetricTridiagonal -> Int -> Double
rowUpperBound tridiagonalValue rowIndex =
  let diagonalValues = symmetricTridiagonalDiagonalVector tridiagonalValue
      offDiagonalValues = symmetricTridiagonalOffDiagonalVector tridiagonalValue
      matrixSize = U.length diagonalValues
      leftRadius =
        if rowIndex <= 0
          then 0.0
          else abs (vectorEntryOrZero offDiagonalValues (rowIndex - 1))
      rightRadius =
        if rowIndex + 1 >= matrixSize
          then 0.0
          else abs (vectorEntryOrZero offDiagonalValues rowIndex)
   in vectorEntryOrZero diagonalValues rowIndex + leftRadius + rightRadius
{-# INLINE rowUpperBound #-}

vectorEntryOrZero :: U.Vector Double -> Int -> Double
vectorEntryOrZero values indexValue =
  case values U.!? indexValue of
    Nothing -> 0.0
    Just value -> value
{-# INLINE vectorEntryOrZero #-}
