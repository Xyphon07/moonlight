module Moonlight.LinAlg.Internal.DenseList
  ( dotProductWith,
    matrixVectorProductWith,
    zipMatrixWith,
    scaleMatrixWith,
    outerProductWith,
  )
where

import Data.Function ((&))
import Prelude

dotProductWith :: (left -> right -> product) -> (product -> accumulator -> accumulator) -> accumulator -> [left] -> [right] -> Either String accumulator
dotProductWith multiply append zeroValue left right =
  if leftLength == rightLength
    then Right (foldr append zeroValue (zipWith multiply left right))
    else Left ("length mismatch (left=" <> show leftLength <> ", right=" <> show rightLength <> ")")
  where
    leftLength = length left
    rightLength = length right

matrixVectorProductWith :: (entry -> value -> product) -> (product -> accumulator -> accumulator) -> accumulator -> [[entry]] -> [value] -> Either String [accumulator]
matrixVectorProductWith multiply append zeroValue matrixRows vectorValue =
  traverse
    (\(rowIndex, rowValues) ->
      dotProductWith multiply append zeroValue rowValues vectorValue
        & either
          (Left . (\message -> "row " <> show rowIndex <> ": " <> message))
          Right
    )
    (zip [0 :: Int ..] matrixRows)

zipMatrixWith :: (left -> right -> result) -> [[left]] -> [[right]] -> [[result]]
zipMatrixWith combine = zipWith (zipWith combine)

scaleMatrixWith :: (scalar -> value -> result) -> scalar -> [[value]] -> [[result]]
scaleMatrixWith multiply scalarValue =
  map (map (multiply scalarValue))

outerProductWith :: (left -> right -> result) -> [left] -> [right] -> [[result]]
outerProductWith multiply left right =
  map (\leftEntry -> map (multiply leftEntry) right) left
