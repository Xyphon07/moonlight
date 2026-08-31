module SiteManifest
  ( siteManifestBenchmarks,
  )
where

import BenchSupport (BenchSetup (..), prepareBenchValue)
import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Set (Set)
import Data.Set qualified as Set
import FinCat
  ( compositionMapWeight,
    finCatExplicitCompositionMapViewWeight,
    finCatExplicitMorphismMapViewWeight,
    finMorphismIdWeight,
    finMorphismWeight,
    finObjectIdWeight,
    morphismMapWeight,
    objectSetWeight,
    representativeCompositionPair,
    rnfFinCat,
    rnfMaybeFinMorphismPair,
  )
import Moonlight.Category.Pure.Category (composeMor)
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinMorphismId,
    FinObjectId,
    FinMor,
    finCatHandle,
    finCatMorphismCountFrom,
    finCatMorphismCountTo,
    finCatMorphismIdByEndpoints,
    foldMapFinMorphisms,
  )
import Moonlight.Category.Pure.Site.Compile
  ( ThinSitePresentation (..),
    thinSiteImportKernel,
    thinSiteKernel,
    thinSiteKernelCodomain,
    thinSitePresentation,
  )
import Moonlight.Category.Pure.Site.Core
  ( SiteFinCatError,
    SiteManifest (..),
    SiteViolation,
  )
import Moonlight.Category.Pure.Site.Graph
  ( importCycles,
    reachableClosure,
  )
import Moonlight.Category.Pure.Site.Manifest (validateSiteManifest)
import SiteCases
  ( SiteCase,
    siteCaseLabel,
    siteCases,
    siteEndpointObjectIds,
    siteManifestFromCase,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

siteManifestBenchmarks :: Benchmark
siteManifestBenchmarks =
  bgroup
    "manifest graph and compilation"
    (siteCases & fmap siteManifestBenchmark)

siteManifestBenchmark :: SiteCase -> Benchmark
siteManifestBenchmark siteCase =
  let manifest = siteManifestFromCase siteCase
   in bgroup
        (siteCaseLabel siteCase)
        [ bench "validateSiteManifest" (nf validateSiteManifestWeight manifest),
          bench "reachableClosure" (nf reachableClosureWeight (siteImports manifest)),
          bench "importCycles" (nf importCyclesWeight manifest),
          bench "thinSiteKernel + explicit presentation" (nf thinSitePresentationWeight manifest),
          bench "thinSiteImportKernel constructor" (nf thinSiteImportKernelConstructorWeight manifest),
          env (prepareBenchValue (preparedSiteFinCatCase siteCase manifest)) $ \prepared ->
            bgroup
              "prepared thinSiteImportKernel"
              [ bench "resident endpoint lookup" (nf preparedSiteFinCatEndpointLookupWeight prepared),
                bench "resident source incident count" (nf preparedSiteFinCatSourceIncidentCountWeight prepared),
                bench "resident target incident count" (nf preparedSiteFinCatTargetIncidentCountWeight prepared),
                bench "resident composition" (nf preparedSiteFinCatCompositionWeight prepared),
                bench "full morphism enumeration" (nf preparedSiteFinCatFullMorphismEnumerationWeight prepared),
                bench "explicit morphism map view" (nf preparedSiteFinCatExplicitMorphismMapViewWeight prepared),
                bench "explicit composition map view" (nf preparedSiteFinCatExplicitCompositionMapViewWeight prepared)
              ]
        ]

data PreparedSiteFinCatCase = PreparedSiteFinCatCase
  { preparedSiteFinCatCategory :: !FinCat,
    preparedSiteFinCatSourceId :: !FinObjectId,
    preparedSiteFinCatTargetId :: !FinObjectId,
    preparedSiteFinCatCompositionPair :: Maybe (FinMor, FinMor)
  }

instance NFData PreparedSiteFinCatCase where
  rnf prepared =
    rnfFinCat (preparedSiteFinCatCategory prepared)
      `seq` finObjectIdWeight (preparedSiteFinCatSourceId prepared)
      `seq` finObjectIdWeight (preparedSiteFinCatTargetId prepared)
      `seq` rnfMaybeFinMorphismPair (preparedSiteFinCatCompositionPair prepared)
      `seq` ()

preparedSiteFinCatCase :: SiteCase -> SiteManifest Int -> BenchSetup PreparedSiteFinCatCase
preparedSiteFinCatCase siteCase manifest =
  BenchSetup $ do
    importKernel <- first show (thinSiteImportKernel manifest)
    let categoryValue = thinSiteKernelCodomain importKernel
    (sourceId, targetId) <- siteEndpointObjectIds siteCase importKernel
    pure
      PreparedSiteFinCatCase
        { preparedSiteFinCatCategory = categoryValue,
          preparedSiteFinCatSourceId = sourceId,
          preparedSiteFinCatTargetId = targetId,
          preparedSiteFinCatCompositionPair = representativeCompositionPair categoryValue
        }

preparedSiteFinCatEndpointLookupWeight :: PreparedSiteFinCatCase -> Int
preparedSiteFinCatEndpointLookupWeight prepared =
  finCatMorphismIdByEndpoints
    (preparedSiteFinCatCategory prepared)
    (preparedSiteFinCatSourceId prepared)
    (preparedSiteFinCatTargetId prepared)
    & maybe 0 finMorphismIdWeight

preparedSiteFinCatSourceIncidentCountWeight :: PreparedSiteFinCatCase -> Int
preparedSiteFinCatSourceIncidentCountWeight prepared =
  finCatMorphismCountFrom
    (preparedSiteFinCatCategory prepared)
    (preparedSiteFinCatSourceId prepared)

preparedSiteFinCatTargetIncidentCountWeight :: PreparedSiteFinCatCase -> Int
preparedSiteFinCatTargetIncidentCountWeight prepared =
  finCatMorphismCountTo
    (preparedSiteFinCatCategory prepared)
    (preparedSiteFinCatTargetId prepared)

preparedSiteFinCatCompositionWeight :: PreparedSiteFinCatCase -> Int
preparedSiteFinCatCompositionWeight prepared =
  case preparedSiteFinCatCompositionPair prepared of
    Nothing -> 0
    Just (leftMorphism, rightMorphism) ->
      composeMor (preparedSiteFinCatCategory prepared) leftMorphism rightMorphism
        & either (const 0) finMorphismWeight

preparedSiteFinCatFullMorphismEnumerationWeight :: PreparedSiteFinCatCase -> Int
preparedSiteFinCatFullMorphismEnumerationWeight prepared =
  foldMapFinMorphisms (Sum . finMorphismWeight) (preparedSiteFinCatCategory prepared)
    & getSum

preparedSiteFinCatExplicitMorphismMapViewWeight :: PreparedSiteFinCatCase -> Int
preparedSiteFinCatExplicitMorphismMapViewWeight =
  finCatExplicitMorphismMapViewWeight . preparedSiteFinCatCategory

preparedSiteFinCatExplicitCompositionMapViewWeight :: PreparedSiteFinCatCase -> Int
preparedSiteFinCatExplicitCompositionMapViewWeight =
  finCatExplicitCompositionMapViewWeight . preparedSiteFinCatCategory

objectIdMapWeight :: Map Int FinObjectId -> Int
objectIdMapWeight =
  Map.foldlWithKey'
    ( \accumulated objectValue objectId ->
        accumulated + objectValue + finObjectIdWeight objectId
    )
    0

pairIdMapWeight :: Map (Int, Int) FinMorphismId -> Int
pairIdMapWeight =
  Map.foldlWithKey'
    ( \accumulated (sourceValue, targetValue) morphismId ->
        accumulated + sourceValue + targetValue + finMorphismIdWeight morphismId
    )
    0

validateSiteManifestWeight :: SiteManifest Int -> Int
validateSiteManifestWeight =
  sum . fmap siteViolationWeight . validateSiteManifest

reachableClosureWeight :: Map Int (Set Int) -> Int
reachableClosureWeight =
  sum . fmap Set.size . Map.elems . reachableClosure

importCyclesWeight :: SiteManifest Int -> Int
importCyclesWeight manifest =
  importCycles manifest
    & fmap (length . NonEmpty.toList)
    & sum

thinSitePresentationWeight :: SiteManifest Int -> Int
thinSitePresentationWeight manifest =
  case thinSiteKernel manifest of
    Left siteError -> siteFinCatErrorWeight siteError
    Right kernel ->
      let presentation = thinSitePresentation kernel
       in objectIdMapWeight (thinPresentationObjectIds presentation)
            + pairIdMapWeight (thinPresentationPairIds presentation)
            + objectSetWeight (thinPresentationObjects presentation)
            + morphismMapWeight (thinPresentationMorphisms presentation)
            + compositionMapWeight (thinPresentationComposition presentation)

thinSiteImportKernelConstructorWeight :: SiteManifest Int -> Int
thinSiteImportKernelConstructorWeight manifest =
  either
    siteFinCatErrorWeight
    (\kernel -> finCatHandle (thinSiteKernelCodomain kernel) `seq` 1)
    (thinSiteImportKernel manifest)

siteViolationWeight :: SiteViolation Int -> Int
siteViolationWeight =
  length . show

siteFinCatErrorWeight :: SiteFinCatError Int -> Int
siteFinCatErrorWeight =
  length . show
