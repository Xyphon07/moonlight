
module Moonlight.LinAlg.Pure.Dense.Basic
  ( mapMatrix,
    add,
    mult,
    transpose,
  )
where

import GHC.TypeNats (KnownNat)
import Moonlight.Core (AdditiveGroup, MoonlightError, Semiring)
import qualified Moonlight.Core as Core
import Moonlight.LinAlg.Internal.Storage
  ( matrixMultiplyList,
    matrixTransposeList,
    matrixZipList,
  )
import Moonlight.LinAlg.Pure.Dense.Types
  ( Matrix,
    fromListMatrix,
    matrixShape,
    toListMatrix,
  )
import Prelude

mapMatrix ::
  forall r c a b.
  (KnownNat r, KnownNat c) =>
  (a -> b) ->
  Matrix r c a ->
  Either MoonlightError (Matrix r c b)
mapMatrix fn matrixValue =
  fromListMatrix @r @c (map fn (toListMatrix matrixValue))

add ::
  forall r c a.
  (KnownNat r, KnownNat c, AdditiveGroup a) =>
  Matrix r c a ->
  Matrix r c a ->
  Either MoonlightError (Matrix r c a)
add left right = do
  let (rowCount, columnCount) = matrixShape left
      (rightRows, rightCols) = matrixShape right
  values <- matrixZipList rowCount columnCount rightRows rightCols Core.add (toListMatrix left) (toListMatrix right)
  fromListMatrix @r @c values

mult ::
  forall r m c a.
  (KnownNat r, KnownNat m, KnownNat c, Semiring a) =>
  Matrix r m a ->
  Matrix m c a ->
  Either MoonlightError (Matrix r c a)
mult left right = do
  let (leftRows, leftCols) = matrixShape left
      (rightRows, rightCols) = matrixShape right
  values <- matrixMultiplyList leftRows leftCols rightRows rightCols (toListMatrix left) (toListMatrix right)
  fromListMatrix @r @c values

transpose ::
  forall r c a.
  (KnownNat r, KnownNat c) =>
  Matrix r c a ->
  Either MoonlightError (Matrix c r a)
transpose matrixValue = do
  let (rowCount, columnCount) = matrixShape matrixValue
  values <- matrixTransposeList rowCount columnCount (toListMatrix matrixValue)
  fromListMatrix @c @r values
