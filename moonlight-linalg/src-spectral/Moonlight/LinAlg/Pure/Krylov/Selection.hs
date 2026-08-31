{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Krylov.Selection
  ( SpectrumEnd (..),
    sortForSpectrumBy,
    sortRawPairsForSpectrum,
  )
where

import Data.Kind (Type)
import Data.List (sortBy)
import Data.Ord (comparing)
import Prelude

type SpectrumEnd :: Type
data SpectrumEnd = SmallestEigenvalues | LargestEigenvalues
  deriving stock (Eq, Show)

sortForSpectrumBy :: Ord keyValue => SpectrumEnd -> (value -> keyValue) -> [value] -> [value]
sortForSpectrumBy spectrumEnd projectValue =
  sortBy
    ( case spectrumEnd of
        SmallestEigenvalues -> comparing projectValue
        LargestEigenvalues -> flip (comparing projectValue)
    )

sortRawPairsForSpectrum :: SpectrumEnd -> [(Double, vector)] -> [(Double, vector)]
sortRawPairsForSpectrum spectrumEnd = sortForSpectrumBy spectrumEnd fst
