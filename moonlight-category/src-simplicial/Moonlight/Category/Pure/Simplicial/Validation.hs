-- | Typed obstructions for the simplicial identities: law kinds, indices, and
-- checks reported as values rather than exceptions.
module Moonlight.Category.Pure.Simplicial.Validation
  ( SimplicialLawEquality,
    simplicialLawEq,
    SimplicialLawKind (..),
    allSimplicialLawKinds,
    SimplicialLawIndices (..),
    SimplicialLawObstruction (..),
    lawObstructionKind,
    SimplicialLawCheck,
    checkFaceFaceLawBy,
    checkDegeneracyDegeneracyLawBy,
    checkFaceDegeneracyLawBy,
    checkSimplicialLawsBy,
    checkFaceFaceLaw,
    checkDegeneracyDegeneracyLaw,
    checkFaceDegeneracyLaw,
    checkSimplicialLaws,
  )
where

import Moonlight.Category.Pure.Simplicial.Validation.Internal
  ( SimplicialLawCarrier (..),
    SimplicialLawCheck,
    SimplicialLawEquality,
    SimplicialLawIndices (..),
    SimplicialLawKind (..),
    SimplicialLawObstruction (..),
    allSimplicialLawKinds,
    lawObstructionKind,
    simplicialLawEq,
  )
import qualified Moonlight.Category.Pure.Simplicial.Validation.Internal as ValidationInternal
import Moonlight.Category.Pure.Simplicial.Set
  ( TruncatedNormalizedSSet,
    applyDegeneracyAtDimension,
    applyFaceAtDimension,
    simplicesAtDimension,
    truncationBound,
  )

truncatedLawCarrier :: TruncatedNormalizedSSet simplex -> SimplicialLawCarrier simplex
truncatedLawCarrier simplicialSet =
  SimplicialLawCarrier
    { lawCarrierUpperBound = truncationBound simplicialSet,
      lawCarrierSimplicesAtDimension = simplicesAtDimension simplicialSet,
      lawCarrierFaceAtDimension = applyFaceAtDimension simplicialSet,
      lawCarrierDegeneracyAtDimension = applyDegeneracyAtDimension simplicialSet
    }

checkFaceFaceLawBy :: SimplicialLawEquality simplex -> TruncatedNormalizedSSet simplex -> SimplicialLawCheck simplex
checkFaceFaceLawBy areEqual =
  ValidationInternal.checkFaceFaceLawBy areEqual . truncatedLawCarrier

checkDegeneracyDegeneracyLawBy :: SimplicialLawEquality simplex -> TruncatedNormalizedSSet simplex -> SimplicialLawCheck simplex
checkDegeneracyDegeneracyLawBy areEqual =
  ValidationInternal.checkDegeneracyDegeneracyLawBy areEqual . truncatedLawCarrier

checkFaceDegeneracyLawBy :: SimplicialLawEquality simplex -> TruncatedNormalizedSSet simplex -> SimplicialLawCheck simplex
checkFaceDegeneracyLawBy areEqual =
  ValidationInternal.checkFaceDegeneracyLawBy areEqual . truncatedLawCarrier

checkSimplicialLawsBy :: SimplicialLawEquality simplex -> TruncatedNormalizedSSet simplex -> SimplicialLawCheck simplex
checkSimplicialLawsBy areEqual =
  ValidationInternal.checkSimplicialLawsBy areEqual . truncatedLawCarrier

checkFaceFaceLaw :: Eq simplex => TruncatedNormalizedSSet simplex -> SimplicialLawCheck simplex
checkFaceFaceLaw =
  checkFaceFaceLawBy simplicialLawEq

checkDegeneracyDegeneracyLaw :: Eq simplex => TruncatedNormalizedSSet simplex -> SimplicialLawCheck simplex
checkDegeneracyDegeneracyLaw =
  checkDegeneracyDegeneracyLawBy simplicialLawEq

checkFaceDegeneracyLaw :: Eq simplex => TruncatedNormalizedSSet simplex -> SimplicialLawCheck simplex
checkFaceDegeneracyLaw =
  checkFaceDegeneracyLawBy simplicialLawEq

checkSimplicialLaws :: Eq simplex => TruncatedNormalizedSSet simplex -> SimplicialLawCheck simplex
checkSimplicialLaws =
  checkSimplicialLawsBy simplicialLawEq
