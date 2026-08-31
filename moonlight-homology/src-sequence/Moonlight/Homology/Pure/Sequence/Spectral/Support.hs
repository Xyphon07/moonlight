module Moonlight.Homology.Pure.Sequence.Spectral.Support
  ( SpectralWindow (..),
    SpectralRestrictionKind (..),
    SpectralRestriction (..),
    SpectralWindowCover (..),
    SpectralSupportRegistry,
    mkSpectralSupportRegistry,
    supportWindows,
    supportWindowBidegree,
    supportLookupWindow,
    supportIncomingRestrictions,
    supportWindowCover,
    supportDifferentialTargetWindow,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Homology.Pure.Degree (HomologicalDegree)
import Moonlight.Homology.Pure.Sequence.Spectral.Bidegree
  ( Bidegree,
    bidegreeFromTotalDegree,
    targetBidegreeAfterDifferential,
  )

type SpectralWindow :: Type
data SpectralWindow = SpectralWindow
  { spectralWindowDegree :: HomologicalDegree,
    spectralWindowFiltration :: Int
  }
  deriving stock (Eq, Ord, Show)

type SpectralRestrictionKind :: Type
data SpectralRestrictionKind
  = FiltrationRestriction
  deriving stock (Eq, Ord, Show)

type SpectralRestriction :: Type
data SpectralRestriction = SpectralRestriction
  { spectralRestrictionKind :: SpectralRestrictionKind,
    spectralRestrictionSourceWindow :: SpectralWindow,
    spectralRestrictionTargetWindow :: SpectralWindow
  }
  deriving stock (Eq, Ord, Show)

type SpectralWindowCover :: Type
data SpectralWindowCover = SpectralWindowCover
  { spectralCoverWindow :: SpectralWindow,
    spectralCoverIncomingRestrictions :: [SpectralRestriction]
  }
  deriving stock (Eq, Show)

type SpectralSupportRegistry :: Type
data SpectralSupportRegistry = SpectralSupportRegistry
  { registryWindows :: [SpectralWindow],
    registryByBidegree :: Map.Map Bidegree SpectralWindow,
    registryRestrictionsByTarget :: Map.Map SpectralWindow [SpectralRestriction]
  }

mkSpectralSupportRegistry :: Map.Map HomologicalDegree [Int] -> SpectralSupportRegistry
mkSpectralSupportRegistry levelsByDegree =
  let windows =
        Map.toAscList levelsByDegree
          >>= \(degreeValue, levelsAtDegree) ->
            distinctLevels levelsAtDegree
              & fmap
                ( \filtrationDegreeValue ->
                    SpectralWindow
                      { spectralWindowDegree = degreeValue,
                        spectralWindowFiltration = filtrationDegreeValue
                      }
                )
      byBidegree =
        windows
          & fmap (\windowValue -> (supportWindowBidegree windowValue, windowValue))
          & Map.fromList
      restrictions =
        windows
          >>= \targetWindow ->
            shiftedWindow byBidegree 1 targetWindow
              & maybe
                []
                ( \sourceWindow ->
                    [ SpectralRestriction
                        { spectralRestrictionKind = FiltrationRestriction,
                          spectralRestrictionSourceWindow = sourceWindow,
                          spectralRestrictionTargetWindow = targetWindow
                        }
                    ]
                )
      restrictionsByTarget =
        restrictions
          & fmap (\restrictionValue -> (spectralRestrictionTargetWindow restrictionValue, [restrictionValue]))
          & Map.fromListWith (<>)
   in SpectralSupportRegistry
        { registryWindows = windows,
          registryByBidegree = byBidegree,
          registryRestrictionsByTarget = restrictionsByTarget
        }

supportWindows :: SpectralSupportRegistry -> [SpectralWindow]
supportWindows =
  registryWindows

supportWindowBidegree :: SpectralWindow -> Bidegree
supportWindowBidegree windowValue =
  bidegreeFromTotalDegree
    (spectralWindowFiltration windowValue)
    (spectralWindowDegree windowValue)

supportLookupWindow :: SpectralSupportRegistry -> Bidegree -> Maybe SpectralWindow
supportLookupWindow registry =
  (`Map.lookup` registryByBidegree registry)

supportIncomingRestrictions :: SpectralSupportRegistry -> SpectralWindow -> [SpectralRestriction]
supportIncomingRestrictions registry windowValue =
  Map.findWithDefault [] windowValue (registryRestrictionsByTarget registry)

supportWindowCover :: SpectralSupportRegistry -> SpectralWindow -> SpectralWindowCover
supportWindowCover registry windowValue =
  SpectralWindowCover
    { spectralCoverWindow = windowValue,
      spectralCoverIncomingRestrictions = supportIncomingRestrictions registry windowValue
    }

supportDifferentialTargetWindow :: SpectralSupportRegistry -> Int -> SpectralWindow -> Maybe SpectralWindow
supportDifferentialTargetWindow registry pageNumber windowValue =
  supportLookupWindow
    registry
    (targetBidegreeAfterDifferential pageNumber (supportWindowBidegree windowValue))

shiftedWindow :: Map.Map Bidegree SpectralWindow -> Int -> SpectralWindow -> Maybe SpectralWindow
shiftedWindow byBidegree filtrationDelta windowValue =
  Map.lookup
    ( bidegreeFromTotalDegree
        (spectralWindowFiltration windowValue + filtrationDelta)
        (spectralWindowDegree windowValue)
    )
    byBidegree

distinctLevels :: [Int] -> [Int]
distinctLevels =
  Set.toAscList . Set.fromList
