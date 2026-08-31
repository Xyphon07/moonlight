module Moonlight.LinAlg.Pure.Dense.Field
  ( DenseRankBackend,
    PLU (..),
    KernelBasis (..),
    pluDecompFullRank,
    rank,
    kernel,
  )
where

import GHC.TypeNats (KnownNat)
import Moonlight.Core (Field, MoonlightError)
import Moonlight.LinAlg.Internal.Backend.Core (DenseRankBackend, runKernel, runPluDecomp, runRank)
import Moonlight.LinAlg.Internal.Backend.PLU (PLU (..))
import Moonlight.LinAlg.Internal.Backend.RREF (KernelBasis (..))
import Moonlight.LinAlg.Pure.Dense.Types (Matrix)

pluDecompFullRank ::
  forall r c a.
  (KnownNat r, KnownNat c, Field a) =>
  Matrix r c a ->
  Either MoonlightError (PLU r c a)
pluDecompFullRank = runPluDecomp

rank ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, Field a, DenseRankBackend a) =>
  Matrix r c a ->
  Either MoonlightError Int
rank = runRank

kernel ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, Field a) =>
  Matrix r c a ->
  Either MoonlightError (KernelBasis c a)
kernel = runKernel
