-- | Site manifest construction and validation on the shared dense reachability
-- kernel. Full-site and import-category compilation consume these same validated
-- sections; diagnostics therefore have one owner rather than two approximate ones.
module Moonlight.Category.Pure.Site.Manifest
  ( ValidatedSiteManifest,
    validatedSiteObjectVector,
    validatedSiteReachabilityRows,
    mkSiteManifest,
    validateSiteManifest,
    validateSiteManifestDetailed,
    validateSiteImportManifest,
  )
where

import Data.Bits ((.|.))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Moonlight.Category.Pure.Site.Core (SiteManifest (..), SiteViolation (..))
import Moonlight.Category.Pure.Finite.DenseReachability
  ( bitsDifference,
    bitsToAscList,
    denseClosureCycleComponents,
    denseClosureReachabilityRows,
    denseReachabilityWithCycles,
    intListBits,
    objectComponentsFromIndices,
    objectIndexOf,
    objectSetFromBits,
  )

data ValidatedSiteManifest obj = ValidatedSiteManifest
  { validatedSiteObjectVector :: !(Vector obj),
    validatedSiteReachabilityRows :: !(Vector Integer)
  }
  deriving stock (Eq, Show)

data DenseRelationRows obj = DenseRelationRows
  { denseRelationRowVector :: !(Vector Integer),
    denseRelationUnknownTargets :: ![obj],
    denseRelationUnknownMembers :: ![(obj, obj)]
  }
  deriving stock (Eq, Show)

mkSiteManifest :: Ord obj => Set obj -> Map obj (Set obj) -> Map obj (Set obj) -> Either [SiteViolation obj] (SiteManifest obj)
mkSiteManifest objects imports covers =
  let manifest = SiteManifest objects imports covers
   in case validateSiteManifestDetailed manifest of
        Left errors -> Left (NonEmpty.toList errors)
        Right _ -> Right manifest

validateSiteManifest :: Ord obj => SiteManifest obj -> [SiteViolation obj]
validateSiteManifest =
  either NonEmpty.toList (const []) . validateSiteManifestDetailed

validateSiteManifestDetailed :: Ord obj => SiteManifest obj -> Either (NonEmpty (SiteViolation obj)) (ValidatedSiteManifest obj)
validateSiteManifestDetailed =
  validateSiteManifestWith denseCoverErrors

validateSiteImportManifest :: Ord obj => SiteManifest obj -> Either (NonEmpty (SiteViolation obj)) (ValidatedSiteManifest obj)
validateSiteImportManifest manifest =
  let objectVector = Vector.fromList (Set.toAscList (siteObjects manifest))
      objectIndex = objectIndexOf objectVector
      importRows = denseRelationRows objectIndex objectVector (siteImports manifest)
      importClosure = denseReachabilityWithCycles (denseRelationRowVector importRows)
      validationErrors =
        denseImportRelationErrors importRows
          <> denseImportCycleViolations objectVector (denseClosureCycleComponents importClosure)
   in validatedSiteFromErrors objectVector (denseClosureReachabilityRows importClosure) validationErrors

validateSiteManifestWith ::
  Ord obj =>
  (Vector obj -> Vector Integer -> Vector Integer -> [SiteViolation obj]) ->
  SiteManifest obj ->
  Either (NonEmpty (SiteViolation obj)) (ValidatedSiteManifest obj)
validateSiteManifestWith coverErrorsForRows manifest =
  let objectVector = Vector.fromList (Set.toAscList (siteObjects manifest))
      objectIndex = objectIndexOf objectVector
      importRows = denseRelationRows objectIndex objectVector (siteImports manifest)
      coverRows = denseRelationRows objectIndex objectVector (siteCovers manifest)
      importClosure = denseReachabilityWithCycles (denseRelationRowVector importRows)
      reachabilityRows = denseClosureReachabilityRows importClosure
      validationErrors =
        denseImportRelationErrors importRows
          <> denseCoverRelationErrors coverRows
          <> missingCoverErrors objectVector (siteCovers manifest)
          <> denseImportCycleViolations objectVector (denseClosureCycleComponents importClosure)
          <> coverErrorsForRows objectVector reachabilityRows (denseRelationRowVector coverRows)
   in validatedSiteFromErrors objectVector reachabilityRows validationErrors

validatedSiteFromErrors :: Vector obj -> Vector Integer -> [SiteViolation obj] -> Either (NonEmpty (SiteViolation obj)) (ValidatedSiteManifest obj)
validatedSiteFromErrors objectVector reachabilityRows validationErrors =
  case NonEmpty.nonEmpty validationErrors of
    Nothing -> Right (ValidatedSiteManifest objectVector reachabilityRows)
    Just errors -> Left errors

denseImportRelationErrors :: DenseRelationRows obj -> [SiteViolation obj]
denseImportRelationErrors relationRows =
  fmap UnknownImportTarget (denseRelationUnknownTargets relationRows)
    <> fmap (uncurry UnknownImportedObject) (denseRelationUnknownMembers relationRows)

denseCoverRelationErrors :: DenseRelationRows obj -> [SiteViolation obj]
denseCoverRelationErrors relationRows =
  fmap UnknownCoverTarget (denseRelationUnknownTargets relationRows)
    <> fmap (uncurry UnknownCoveredObject) (denseRelationUnknownMembers relationRows)

missingCoverErrors :: Ord obj => Vector obj -> Map obj (Set obj) -> [SiteViolation obj]
missingCoverErrors objectVector covers =
  objectVector
    & Vector.toList
    & filter (`Map.notMember` covers)
    & fmap MissingCover

denseRelationRows :: Ord obj => Map obj Int -> Vector obj -> Map obj (Set obj) -> DenseRelationRows obj
denseRelationRows objectIndex objectVector relation =
  DenseRelationRows
    { denseRelationRowVector =
        objectVector
          & Vector.map
            ( \objectValue ->
                Map.findWithDefault Set.empty objectValue relation
                  & Set.toAscList
                  & mapMaybe (`Map.lookup` objectIndex)
                  & intListBits
            ),
      denseRelationUnknownTargets =
        relation
          & Map.keys
          & filter (`Map.notMember` objectIndex),
      denseRelationUnknownMembers =
        relation
          & Map.toAscList
          >>= ( \(targetObject, sources) ->
                  sources
                    & Set.toAscList
                    & filter (`Map.notMember` objectIndex)
                    & fmap (\sourceObject -> (targetObject, sourceObject))
              )
    }

denseImportCycleViolations :: Ord obj => Vector obj -> [NonEmpty Int] -> [SiteViolation obj]
denseImportCycleViolations objectVector components =
  objectComponentsFromIndices objectVector components
    & fmap ImportCycleDetected

denseCoverErrors ::
  Ord obj =>
  Vector obj ->
  Vector Integer ->
  Vector Integer ->
  [SiteViolation obj]
denseCoverErrors objectVector reachabilityRows coverRows
  | coverRows == reachabilityRows = []
  | otherwise = coverOutsideReachable <> coverClosureViolations
  where
    objectCount = Vector.length objectVector

    coverOutsideReachable =
      Vector.zip3 objectVector reachabilityRows coverRows
        & Vector.toList
        >>= ( \(targetObject, reachableBits, coverBits) ->
                let outsideBits = bitsDifference coverBits reachableBits
                 in if outsideBits == 0
                      then []
                      else [CoverOutsideReachable targetObject (objectSetFromBits objectVector outsideBits)]
            )

    coverClosureViolations =
      Vector.zip objectVector coverRows
        & Vector.toList
        >>= ( \(targetObject, coverBits) ->
                let closureMissingBits = bitsDifference (rowsUnionForBits objectCount coverRows coverBits) coverBits
                 in if closureMissingBits == 0
                      then []
                      else denseCoverClosureViolationsForTarget objectVector coverRows targetObject coverBits
            )

denseCoverClosureViolationsForTarget :: Ord obj => Vector obj -> Vector Integer -> obj -> Integer -> [SiteViolation obj]
denseCoverClosureViolationsForTarget objectVector coverRows targetObject coverBits =
  bitsToAscList (Vector.length objectVector) coverBits
    >>= ( \coveredIndex ->
            case (objectVector Vector.!? coveredIndex, coverRows Vector.!? coveredIndex) of
              (Just covered, Just coveredCoverBits) ->
                let
                    missingBits = bitsDifference coveredCoverBits coverBits
                 in if missingBits == 0
                      then []
                      else [CoverNotClosed targetObject covered (objectSetFromBits objectVector missingBits)]
              _ -> []
        )

rowsUnionForBits :: Int -> Vector Integer -> Integer -> Integer
rowsUnionForBits objectCount rows bits =
  bitsToAscList objectCount bits
    & mapMaybe (rows Vector.!?)
    & foldr (.|.) 0
