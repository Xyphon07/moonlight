module Moonlight.Homology.Effect.Laws
  ( BlockSchurHomologyAgreement (..),
    BlockSchurHomologyAgreementFailure (..),
    checkBlockSchurHomologyAgreement,
    checkBoundaryNilpotence,
    checkReductionLeftInverse,
    checkReductionHomotopy,
    checkReductionProjectionChainMap,
    checkReductionInclusionChainMap,
    mkReductionChecksFromSamples,
    normalizeCombination,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Kind (Type)
import Moonlight.Core (Ring)
import Moonlight.Homology.Pure.Backend
  ( HomologyBackend,
    HomologyBackendTag,
    homologyBackendTag,
    runHomologyBackend,
  )
import Moonlight.Homology.Pure.Failure (HomologyFailure (..), HomologyLaw (..))
import Moonlight.Homology.Pure.Group (HomologyGroup)
import Moonlight.Homology.Pure.LinearCombination qualified as LC
import Moonlight.Homology.Pure.Reductions
  ( ChainHomotopy (..),
    ChainMap (..),
    Reduction (..),
    ReductionChecks (..),
    ReductionLawContext (..),
    ReductionViolation (..),
  )
import Moonlight.Homology.Pure.Topology.BlockSchur
  ( BlockSchurReduction (..),
  )

type BlockSchurHomologyAgreement :: Type -> Type
data BlockSchurHomologyAgreement groupCoefficient = BlockSchurHomologyAgreement
  { bshaBackend :: !HomologyBackendTag,
    bshaGroupsByDegree :: ![(Int, HomologyGroup groupCoefficient)]
  }
  deriving stock (Eq, Show)

type BlockSchurHomologyAgreementFailure :: Type -> Type
data BlockSchurHomologyAgreementFailure groupCoefficient
  = BlockSchurOriginalBackendFailed !HomologyBackendTag !HomologyFailure
  | BlockSchurReducedBackendFailed !HomologyBackendTag !HomologyFailure
  | BlockSchurHomologyMismatch
      !HomologyBackendTag
      ![(Int, HomologyGroup groupCoefficient)]
      ![(Int, HomologyGroup groupCoefficient)]
  deriving stock (Eq, Show)

checkBlockSchurHomologyAgreement ::
  Eq groupCoefficient =>
  HomologyBackend coefficient groupCoefficient ->
  BlockSchurReduction coefficient ->
  Either (BlockSchurHomologyAgreementFailure groupCoefficient) (BlockSchurHomologyAgreement groupCoefficient)
checkBlockSchurHomologyAgreement backend reduction = do
  let backendTag = homologyBackendTag backend
  originalGroups <-
    first (BlockSchurOriginalBackendFailed backendTag) $
      homologyGroupsByDegree <$> runHomologyBackend backend (bsrOriginalComplex reduction)
  reducedGroups <-
    first (BlockSchurReducedBackendFailed backendTag) $
      homologyGroupsByDegree <$> runHomologyBackend backend (bsrReducedComplex reduction)
  if originalGroups == reducedGroups
    then
      Right
        BlockSchurHomologyAgreement
          { bshaBackend = backendTag,
            bshaGroupsByDegree = originalGroups
          }
    else Left (BlockSchurHomologyMismatch backendTag originalGroups reducedGroups)

normalizeCombination :: (Eq r, Ring r, Ord basis) => [(r, basis)] -> [(r, basis)]
normalizeCombination =
  LC.normalizeWith LC.ringArithmetic

composeCombination ::
  (Eq r, Ring r, Ord targetBasis) =>
  (sourceBasis -> [(r, targetBasis)]) ->
  [(r, sourceBasis)] ->
  [(r, targetBasis)]
composeCombination mapping combination =
  LC.composeWith LC.ringArithmetic mapping combination

checkBoundaryNilpotence ::
  (Eq r, Ring r, Ord basis) =>
  (basis -> [(r, basis)]) ->
  [basis] ->
  Either HomologyFailure ()
checkBoundaryNilpotence boundaryOf basisElements =
  LC.checkLawWith
    LC.ringArithmetic
    ChainNilpotenceLaw
    basisElements
    (const [])
    (composeCombination boundaryOf . boundaryOf)

checkReductionLeftInverse ::
  (Eq r, Ring r, Ord smallBasis) =>
  Reduction large small r largeBasis smallBasis ->
  [smallBasis] ->
  Either HomologyFailure ()
checkReductionLeftInverse reduction smallBasisElements =
  LC.checkLawWith
    LC.ringArithmetic
    ReductionLeftInverseLaw
    smallBasisElements
    (LC.identityWith LC.ringArithmetic)
    ( \smallBasisValue ->
        runChainMap (inclusion reduction) smallBasisValue
          & composeCombination (runChainMap (projection reduction))
    )

checkReductionHomotopy ::
  (Eq r, Ring r, Ord largeBasis) =>
  (largeBasis -> [(r, largeBasis)]) ->
  Reduction large small r largeBasis smallBasis ->
  [largeBasis] ->
  Either HomologyFailure ()
checkReductionHomotopy boundaryOf reduction largeBasisElements =
  LC.checkLawWith
    LC.ringArithmetic
    ReductionHomotopyLaw
    largeBasisElements
    ( \largeBasisValue ->
        LC.subtractWith
          LC.ringArithmetic
          (LC.identityWith LC.ringArithmetic largeBasisValue)
          ( runChainMap (projection reduction) largeBasisValue
              & composeCombination (runChainMap (inclusion reduction))
          )
    )
    ( \largeBasisValue ->
        LC.addWith
          LC.ringArithmetic
          (runChainHomotopy (homotopy reduction) largeBasisValue & composeCombination boundaryOf)
          (boundaryOf largeBasisValue & composeCombination (runChainHomotopy (homotopy reduction)))
    )

checkReductionProjectionChainMap ::
  (Eq r, Ring r, Ord smallBasis) =>
  (largeBasis -> [(r, largeBasis)]) ->
  (smallBasis -> [(r, smallBasis)]) ->
  Reduction large small r largeBasis smallBasis ->
  [largeBasis] ->
  Either HomologyFailure ()
checkReductionProjectionChainMap largeBoundaryOf smallBoundaryOf reduction largeBasisElements =
  LC.checkLawWith
    LC.ringArithmetic
    ReductionProjectionChainMapLaw
    largeBasisElements
    ( \largeBasisValue ->
        runChainMap (projection reduction) largeBasisValue
          & composeCombination smallBoundaryOf
    )
    ( \largeBasisValue ->
        largeBoundaryOf largeBasisValue
          & composeCombination (runChainMap (projection reduction))
    )

checkReductionInclusionChainMap ::
  (Eq r, Ring r, Ord largeBasis) =>
  (largeBasis -> [(r, largeBasis)]) ->
  (smallBasis -> [(r, smallBasis)]) ->
  Reduction large small r largeBasis smallBasis ->
  [smallBasis] ->
  Either HomologyFailure ()
checkReductionInclusionChainMap largeBoundaryOf smallBoundaryOf reduction smallBasisElements =
  LC.checkLawWith
    LC.ringArithmetic
    ReductionInclusionChainMapLaw
    smallBasisElements
    ( \smallBasisValue ->
        runChainMap (inclusion reduction) smallBasisValue
          & composeCombination largeBoundaryOf
    )
    ( \smallBasisValue ->
        smallBoundaryOf smallBasisValue
          & composeCombination (runChainMap (inclusion reduction))
    )

mkReductionChecksFromSamples ::
  (Eq r, Ring r, Ord largeBasis, Ord smallBasis) =>
  ReductionLawContext largeBasis smallBasis r ->
  ReductionChecks largeBasis smallBasis r
mkReductionChecksFromSamples lawContext =
  ReductionChecks
    { checkProjectionInclusionIdentity = \projectionMap inclusionMap ->
        Reduction
          { projection = projectionMap,
            inclusion = inclusionMap,
            homotopy = ChainHomotopy (const [])
          }
          & (\reduction -> checkReductionLeftInverse reduction (sampledSmallBasis lawContext))
          & first ProjectionInclusionIdentityViolation,
      checkInclusionProjectionHomotopy = \projectionMap inclusionMap homotopyMap ->
        Reduction
          { projection = projectionMap,
            inclusion = inclusionMap,
            homotopy = homotopyMap
          }
          & (\reduction -> checkReductionHomotopy (largeBoundary lawContext) reduction (sampledLargeBasis lawContext))
          & first InclusionProjectionHomotopyViolation,
      checkProjectionChainMap = \projectionMap ->
        Reduction
          { projection = projectionMap,
            inclusion = ChainMap (const []),
            homotopy = ChainHomotopy (const [])
          }
          & (\reduction -> checkReductionProjectionChainMap (largeBoundary lawContext) (smallBoundary lawContext) reduction (sampledLargeBasis lawContext))
          & first ProjectionChainMapViolation,
      checkInclusionChainMap = \inclusionMap ->
        Reduction
          { projection = ChainMap (const []),
            inclusion = inclusionMap,
            homotopy = ChainHomotopy (const [])
          }
          & (\reduction -> checkReductionInclusionChainMap (largeBoundary lawContext) (smallBoundary lawContext) reduction (sampledSmallBasis lawContext))
          & first InclusionChainMapViolation
    }

homologyGroupsByDegree :: [HomologyGroup groupCoefficient] -> [(Int, HomologyGroup groupCoefficient)]
homologyGroupsByDegree =
  zip [0 ..]
