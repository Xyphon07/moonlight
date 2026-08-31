module Moonlight.Homology.Effect.Determinism
  ( DeterminismFingerprint (..),
    fingerprintBasis,
    fingerprintBoundaryIncidence,
    fingerprintReductionImage,
    fingerprintFiniteChainComplex,
    verifyDeterministicFingerprints,
  )
where

import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Function ((&))
import Data.Kind (Type)
import Data.List (sort, sortOn)
import Moonlight.Core (StableHashDigest, stableHashByteStrings)
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    incidenceMatrixAt,
    maxHomologicalDegree,
  )
import Moonlight.Homology.Boundary.LinAlg (BoundaryEntry, boundaryCoefficient, sourceIndex, targetIndex, BoundaryIncidence, boundaryEntries, sourceCardinality, targetCardinality)
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))
import Moonlight.Homology.Pure.Failure (HomologyFailure (..), HomologyLaw (..))

type DeterminismFingerprint :: Type
newtype DeterminismFingerprint = DeterminismFingerprint
  { unFingerprint :: StableHashDigest
  }
  deriving stock (Eq, Show)

fingerprintTokens :: [String] -> DeterminismFingerprint
fingerprintTokens =
  DeterminismFingerprint . stableHashByteStrings . fmap ByteString.Char8.pack

fingerprintBasis :: Show basis => [basis] -> DeterminismFingerprint
fingerprintBasis values =
  values
    & fmap show
    & sort
    & fingerprintTokens

canonicalEntryToken :: Show r => BoundaryEntry r -> String
canonicalEntryToken entry =
  show (sourceIndex entry, targetIndex entry, boundaryCoefficient entry)

fingerprintBoundaryIncidence :: Show r => BoundaryIncidence r -> DeterminismFingerprint
fingerprintBoundaryIncidence incidence =
  let headerToken = show (sourceCardinality incidence, targetCardinality incidence)
      entryTokens =
        boundaryEntries incidence
          & sortOn (\entry -> (sourceIndex entry, targetIndex entry, show (boundaryCoefficient entry)))
          & fmap canonicalEntryToken
   in fingerprintTokens (headerToken : entryTokens)

fingerprintReductionImage :: (Show r, Show basis) => [(r, basis)] -> DeterminismFingerprint
fingerprintReductionImage terms =
  terms
    & fmap (\(coefficientValue, basisValue) -> (show basisValue, show coefficientValue))
    & sortOn id
    & fmap (\(basisToken, coefficientToken) -> basisToken <> ":" <> coefficientToken)
    & fingerprintTokens

fingerprintFiniteChainComplex :: Show r => FiniteChainComplex r -> DeterminismFingerprint
fingerprintFiniteChainComplex chainComplex =
  let HomologicalDegree maxDegree = maxHomologicalDegree chainComplex
      dimensionTokens =
        [0 .. maxDegree]
          & fmap
            ( \dimensionValue ->
                let boundaryFingerprint =
                      incidenceMatrixAt chainComplex (HomologicalDegree dimensionValue)
                        & fingerprintBoundaryIncidence
                        & unFingerprint
                 in show (dimensionValue, boundaryFingerprint)
            )
   in fingerprintTokens dimensionTokens

verifyDeterministicFingerprints :: [DeterminismFingerprint] -> Either HomologyFailure DeterminismFingerprint
verifyDeterministicFingerprints fingerprints =
  case fingerprints of
    [] -> Left (LawViolation DeterminismLaw)
    expectedFingerprint : remainder ->
      if all (== expectedFingerprint) remainder
        then Right expectedFingerprint
        else Left (LawViolation DeterminismLaw)
