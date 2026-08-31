module Moonlight.Homology.Pure.Sequence.Spectral
  ( FiltrationFunction,
    Bidegree,
    mkBidegree,
    bidegreeFromTotalDegree,
    bidegreeCoordinates,
    bidegreeFiltrationDegree,
    bidegreeComplementaryDegree,
    bidegreeTotalDegree,
    targetBidegreeAfterDifferential,
    FormalMap (..),
    RationalFormalMap,
    SpectralEntry (..),
    RationalSpectralEntry,
    SpectralPage (..),
    RationalSpectralPage,
    SpectralChain,
    SpectralFamily (..),
    RationalSpectralFamily,
    SpectralAdvance (..),
    SpectralCapability,
    nextPage,
    computeRationalSpectralFamily,
    computeRationalSpectralPages,
    stableSpectralPage,
    convergenceDepth,
    isKPassParseable,
  )
where

import Data.Function ((&))
import qualified Data.List as List
import Moonlight.Core (withCapability)
import Moonlight.Homology.Boundary.Finite (FiniteChainComplex)
import Moonlight.Homology.Pure.Failure (HomologyFailure)
import Moonlight.Homology.Pure.Sequence.Spectral.Bidegree
  ( Bidegree,
    bidegreeComplementaryDegree,
    bidegreeCoordinates,
    bidegreeFiltrationDegree,
    bidegreeFromTotalDegree,
    bidegreeTotalDegree,
    mkBidegree,
    targetBidegreeAfterDifferential,
  )
import Moonlight.Homology.Pure.Sequence.Spectral.Build
  ( buildSpectralFamily,
    mkRationalSpectralSource,
  )
import Moonlight.Homology.Pure.Sequence.Spectral.Types
  ( FiltrationFunction,
    FormalMap (..),
    RationalFormalMap,
    RationalSpectralEntry,
    RationalSpectralFamily,
    RationalSpectralPage,
    SpectralAdvance (..),
    SpectralCapability,
    SpectralChain,
    SpectralEntry (..),
    SpectralFamily (..),
    SpectralPage (..),
    SpectralSource,
  )

nextPage :: SpectralCapability phase r -> SpectralPage r -> Either HomologyFailure (SpectralPage r)
nextPage capability page =
  withCapability capability
    (\advance -> runSpectralAdvance advance page)

computeRationalSpectralFamily ::
  FiniteChainComplex Rational ->
  FiltrationFunction ->
  Either HomologyFailure RationalSpectralFamily
computeRationalSpectralFamily finite filtration =
  mkRationalSpectralSource finite filtration >>= computeFamilyFromSource

computeFamilyFromSource ::
  SpectralSource ->
  Either HomologyFailure RationalSpectralFamily
computeFamilyFromSource =
  buildSpectralFamily

computeRationalSpectralPages ::
  FiniteChainComplex Rational ->
  FiltrationFunction ->
  Either HomologyFailure [RationalSpectralPage]
computeRationalSpectralPages finite filtration =
  spectralFamilyPages <$> computeRationalSpectralFamily finite filtration

stableSpectralPage :: [SpectralPage Rational] -> Maybe (SpectralPage Rational)
stableSpectralPage pages =
  let stableIndex = convergenceDepth pages
   in List.find ((== stableIndex) . pageIndex) pages

convergenceDepth :: [SpectralPage Rational] -> Int
convergenceDepth = stabilizationIndex

isKPassParseable :: Int -> [SpectralPage Rational] -> Bool
isKPassParseable passBudget pages =
  convergenceDepth pages <= passBudget

stabilizationIndex :: [SpectralPage Rational] -> Int
stabilizationIndex pages =
  case List.reverse pages of
    [] -> 0
    limitPage : _ ->
      List.reverse pages
        & List.takeWhile (pageSemanticallyEqual limitPage)
        & List.reverse
        & \stableSuffix ->
          case stableSuffix of
            earliestStablePage : _ -> pageIndex earliestStablePage
            [] -> 0

pageSemanticallyEqual :: SpectralPage Rational -> SpectralPage Rational -> Bool
pageSemanticallyEqual leftPage rightPage =
  fmap entryGroupValue (pageEntryMap leftPage) == fmap entryGroupValue (pageEntryMap rightPage)
    && fmap formalMapVanishes (pageDifferentialMap leftPage) == fmap formalMapVanishes (pageDifferentialMap rightPage)

formalMapVanishes :: FormalMap Rational -> Bool
formalMapVanishes =
  all (all (== 0)) . formalMatrix
