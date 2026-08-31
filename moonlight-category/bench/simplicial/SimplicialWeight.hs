module SimplicialWeight
  ( naturalListWeight,
    naturalSSetWeight,
    naturalSimplicesWeight,
    naturalWeight,
    obstructionWeight,
  )
where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Category.Simplicial
  ( TruncatedNormalizedSSet,
    simplicesAtDimension,
    truncationBound,
  )
import Numeric.Natural (Natural)

naturalSSetWeight :: TruncatedNormalizedSSet [Natural] -> Int
naturalSSetWeight simplicialSet =
  [0 .. truncationBound simplicialSet]
    & fmap (naturalSimplicesWeight . simplicesAtDimension simplicialSet)
    & sum

naturalSimplicesWeight :: [[Natural]] -> Int
naturalSimplicesWeight =
  sum . fmap naturalSimplexWeight

naturalSimplexWeight :: [Natural] -> Int
naturalSimplexWeight values =
  length values + naturalListWeight values

naturalListWeight :: [Natural] -> Int
naturalListWeight =
  sum . fmap naturalWeight

naturalWeight :: Natural -> Int
naturalWeight =
  fromIntegral

obstructionWeight :: NonEmpty obstruction -> Int
obstructionWeight =
  length . NonEmpty.toList
