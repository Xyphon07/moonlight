module Moonlight.Homology.Pure.Sequence.Spectral.Bidegree
  ( Bidegree,
    mkBidegree,
    bidegreeFromTotalDegree,
    bidegreeCoordinates,
    bidegreeFiltrationDegree,
    bidegreeComplementaryDegree,
    bidegreeTotalDegree,
    targetBidegreeAfterDifferential,
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))

type Bidegree :: Type
newtype Bidegree = Bidegree
  { bidegreeCoordinates :: (Int, Int)
  }
  deriving stock (Eq, Ord, Show)

mkBidegree :: Int -> Int -> Bidegree
mkBidegree filtrationDegreeValue complementaryDegreeValue =
  Bidegree (filtrationDegreeValue, complementaryDegreeValue)

bidegreeFromTotalDegree :: Int -> HomologicalDegree -> Bidegree
bidegreeFromTotalDegree filtrationDegreeValue (HomologicalDegree totalDegreeValue) =
  mkBidegree filtrationDegreeValue (totalDegreeValue - filtrationDegreeValue)

bidegreeFiltrationDegree :: Bidegree -> Int
bidegreeFiltrationDegree = fst . bidegreeCoordinates

bidegreeComplementaryDegree :: Bidegree -> Int
bidegreeComplementaryDegree = snd . bidegreeCoordinates

bidegreeTotalDegree :: Bidegree -> HomologicalDegree
bidegreeTotalDegree bidegreeValue =
  HomologicalDegree (bidegreeFiltrationDegree bidegreeValue + bidegreeComplementaryDegree bidegreeValue)

targetBidegreeAfterDifferential :: Int -> Bidegree -> Bidegree
targetBidegreeAfterDifferential pageNumber bidegreeValue =
  mkBidegree
    (bidegreeFiltrationDegree bidegreeValue + pageNumber)
    (bidegreeComplementaryDegree bidegreeValue - pageNumber + 1)
