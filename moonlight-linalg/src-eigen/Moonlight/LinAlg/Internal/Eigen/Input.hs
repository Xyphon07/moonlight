module Moonlight.LinAlg.Internal.Eigen.Input
  ( validateSymmetricEigenInput,
    isSymmetricWithin,
  )
where

import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.Eigen.Kernels (finiteDouble)
import Moonlight.LinAlg.Internal.Primitives (epsilon)
import Prelude

validateSymmetricEigenInput :: String -> Int -> [[Double]] -> Either MoonlightError ()
validateSymmetricEigenInput context matrixSize matrixRows
  | matrixSize < 0 =
      Left (InvariantViolation (context <> " requires a non-negative matrix size"))
  | length matrixRows /= matrixSize =
      Left (InvariantViolation (context <> " row count does not match declared matrix size"))
  | not (all ((== matrixSize) . length) matrixRows) =
      Left (InvariantViolation (context <> " requires a square dense matrix"))
  | not (all (all finiteDouble) matrixRows) =
      Left (InvariantViolation (context <> " requires finite matrix entries"))
  | not (isSymmetricWithin (sqrt epsilon) matrixRows) =
      Left (InvariantViolation (context <> " requires a symmetric matrix"))
  | otherwise = Right ()

isSymmetricWithin :: Double -> [[Double]] -> Bool
isSymmetricWithin tolerance matrixRows =
  and
    [ abs (leftEntry - rightEntry) <= tolerance
      | (rowIndex, rowValues) <- zip [0 :: Int ..] matrixRows,
        (columnIndex, leftEntry) <- zip [0 :: Int ..] rowValues,
        rowIndex < columnIndex,
        Just rightEntry <- [matrixEntryMaybe columnIndex rowIndex matrixRows]
    ]

matrixEntryMaybe :: Int -> Int -> [[a]] -> Maybe a
matrixEntryMaybe rowIndex columnIndex matrixRows =
  listEntryMaybe rowIndex matrixRows >>= listEntryMaybe columnIndex

listEntryMaybe :: Int -> [a] -> Maybe a
listEntryMaybe indexValue values
  | indexValue < 0 = Nothing
  | otherwise =
      case drop indexValue values of
        [] -> Nothing
        entryValue : _ -> Just entryValue
