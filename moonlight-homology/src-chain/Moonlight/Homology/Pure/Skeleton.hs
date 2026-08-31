module Moonlight.Homology.Pure.Skeleton
  ( SkeletonSignature (..),
    skeletonSignatureWithinTolerance,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Homology.Pure.Filtration (CriticalKind)

type SkeletonSignature :: Type
data SkeletonSignature = SkeletonSignature
  { signatureCriticalCounts :: Map.Map CriticalKind Int,
    signatureArcCount :: Int
  }
  deriving stock (Eq, Show, Read)

skeletonSignatureWithinTolerance :: Int -> SkeletonSignature -> SkeletonSignature -> Bool
skeletonSignatureWithinTolerance toleranceValue targetSignature observedSignature =
  let criticalKinds = [minBound .. maxBound] :: [CriticalKind]
      criticalMatches =
        criticalKinds
          & all
            ( \criticalKindValue ->
                let observedCount = Map.findWithDefault 0 criticalKindValue (signatureCriticalCounts observedSignature)
                    targetCount = Map.findWithDefault 0 criticalKindValue (signatureCriticalCounts targetSignature)
                 in abs (observedCount - targetCount) <= toleranceValue
            )
      arcMatch = abs (signatureArcCount observedSignature - signatureArcCount targetSignature) <= toleranceValue
   in criticalMatches && arcMatch
