module Moonlight.Category.Effect.Laws.Site
  ( lawSuites,
  )
where

import Data.Either (isRight)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Moonlight.Category.Effect.Harness as Harness
import qualified Moonlight.Category.Effect.PathQuotientHarness as PathQuotientHarness
import Moonlight.Category.Effect.Fixture.FinCat (sampleFinCat)
import Moonlight.Category.Effect.LawNames (LawName (..))
import Moonlight.Category.Effect.Laws.Generators (allPairs)
import Moonlight.Category.Effect.SiteGen (diamondManifest)
import Moonlight.Category.Pure.Category (Category (..), Mor, Ob)
import Moonlight.Category.Pure.FinCat
  ( finCatExplicitCompositionMapView,
    finCatExplicitMorphismMapView,
    finCatObjects,
    mkFinCat,
  )
import Moonlight.Category.Pure.Site
  ( SiteManifest (..),
    SitePathCategory,
    mkSitePathObject,
    pathThinCat,
    pathThinCodomainMorphism,
    pathThinCodomainObject,
    quotientPathThinMorphism,
    quotientPathThinObject,
    sitePathCategory,
    sitePathManifest,
    sitePathMorphismsBetween,
    thinPresentationToFinCat,
    thinSiteKernel,
    thinSitePresentation,
  )
import Moonlight.Category.Pure.Site.Compile
  ( thinSiteImportKernel,
    thinSiteKernelCodomain,
  )
import Moonlight.Pale.Test.Laws.Suite (LawSuite, lawGroup, namedQuickCheckLaw)

sampleSiteManifest :: SiteManifest Int
sampleSiteManifest =
  SiteManifest
    { siteObjects = sampleSiteObjects,
      siteImports = sampleSiteImports,
      siteCovers = sampleSiteCovers
    }

sampleDiamondPathCategory :: Either () (SitePathCategory Int)
sampleDiamondPathCategory =
  case thinSiteKernel diamondManifest of
    Left _ -> Left ()
    Right kernel -> Right (sitePathCategory kernel)

sampleSiteObjects :: Set.Set Int
sampleSiteObjects = Set.fromList [0, 1, 2, 3]

sampleSiteImports :: Map.Map Int (Set.Set Int)
sampleSiteImports =
  Map.fromList
    [ (0, Set.empty),
      (1, Set.singleton 0),
      (2, Set.singleton 1),
      (3, Set.fromList [1, 2])
    ]

sampleSiteCovers :: Map.Map Int (Set.Set Int)
sampleSiteCovers =
  Map.fromList
    [ (0, Set.empty),
      (1, Set.singleton 0),
      (2, Set.fromList [0, 1]),
      (3, Set.fromList [0, 1, 2])
    ]

sampleLayerOf :: Int -> Int
sampleLayerOf objectValue =
  case objectValue of
    0 -> 0
    1 -> 1
    2 -> 2
    _ -> 3

sampleLayerPolicy :: Int -> Int -> Bool
sampleLayerPolicy importer imported = imported <= importer

fincatWellFormedLaw :: Bool
fincatWellFormedLaw =
  isRight
    ( mkFinCat
        (finCatObjects sampleFinCat)
        (finCatExplicitMorphismMapView sampleFinCat)
        (finCatExplicitCompositionMapView sampleFinCat)
    )
    && thinSiteFinCatGenericAgreementLaw

thinSiteFinCatGenericAgreementLaw :: Bool
thinSiteFinCatGenericAgreementLaw =
  case (thinSiteImportKernel diamondManifest, thinSiteKernel diamondManifest) of
    (Right importKernel, Right kernel) ->
      case thinPresentationToFinCat (thinSitePresentation kernel) of
        Left _ -> False
        Right genericallyChecked ->
          let thinDerived = thinSiteKernelCodomain importKernel
           in thinDerived == genericallyChecked
                && finCatObjects thinDerived == finCatObjects genericallyChecked
                && finCatExplicitMorphismMapView thinDerived == finCatExplicitMorphismMapView genericallyChecked
                && finCatExplicitCompositionMapView thinDerived == finCatExplicitCompositionMapView genericallyChecked
    _ -> False

siteQuotientIdentityLaw :: SitePathCategory Int -> Bool
siteQuotientIdentityLaw category =
  let thinCategory = pathThinCat category
   in all
        ( \objectValue ->
            case (identity category objectValue, identity thinCategory (quotientPathThinObject objectValue)) of
              (Right domainIdentity, Right thinIdentity) -> quotientPathThinMorphism domainIdentity == thinIdentity
              _ -> False
        )
        (sitePathObjects category)

siteQuotientCompositionLaw :: SitePathCategory Int -> Bool
siteQuotientCompositionLaw category =
  let thinCategory = pathThinCat category
   in all
        ( \(leftValue, rightValue) ->
            case compose category leftValue rightValue of
              Left _ -> True
              Right (composedDomain, _) ->
                case compose thinCategory (quotientPathThinMorphism leftValue) (quotientPathThinMorphism rightValue) of
                  Left _ -> False
                  Right (composedThin, _) -> quotientPathThinMorphism composedDomain == composedThin
        )
        (allPairs (sitePathMorphisms category))

pathThinCodomainIdentityLaw :: SitePathCategory Int -> Bool
pathThinCodomainIdentityLaw category =
  case thinSiteImportKernel (sitePathManifest category) of
    Left _ -> False
    Right importKernel ->
      let finCategory = thinSiteKernelCodomain importKernel
          thinCategory = pathThinCat category
       in all
            ( \sitePathObject ->
                let objectValue = quotientPathThinObject sitePathObject
                 in case (identity thinCategory objectValue, identity finCategory (pathThinCodomainObject objectValue)) of
                      (Right thinIdentity, Right finIdentity) -> pathThinCodomainMorphism thinIdentity == finIdentity
                      _ -> False
            )
            (sitePathObjects category)

pathThinCodomainCompositionLaw :: SitePathCategory Int -> Bool
pathThinCodomainCompositionLaw category =
  case thinSiteImportKernel (sitePathManifest category) of
    Left _ -> False
    Right importKernel ->
      let finCategory = thinSiteKernelCodomain importKernel
          thinCategory = pathThinCat category
       in all
            ( \(leftValue, rightValue) ->
                case compose thinCategory leftValue rightValue of
                  Left _ -> True
                  Right (composedThin, _) ->
                    case compose finCategory (pathThinCodomainMorphism leftValue) (pathThinCodomainMorphism rightValue) of
                      Left _ -> False
                      Right (composedFin, _) -> pathThinCodomainMorphism composedThin == composedFin
            )
            (sitePathMorphisms category & fmap quotientPathThinMorphism & allPairs)

pathQuotientUniquenessLaw :: SitePathCategory Int -> Bool
pathQuotientUniquenessLaw category =
  diamondHasMultiplePathWitnesses category
    && all
    ( \(sourceValue, targetValue) ->
        PathQuotientHarness.quotientUniquenessPerEndpoint @Int category sourceValue targetValue
    )
    (siteObjects (sitePathManifest category) & Set.toList & allPairs)

diamondHasMultiplePathWitnesses :: SitePathCategory Int -> Bool
diamondHasMultiplePathWitnesses category =
  length (sitePathMorphismsBetween category 0 3) >= 2

pathQuotientFaithfulLaw :: SitePathCategory Int -> Bool
pathQuotientFaithfulLaw =
  PathQuotientHarness.pathThinCodomainFaithful @Int

pathQuotientInterpreterCoherenceLaw :: SitePathCategory Int -> Bool
pathQuotientInterpreterCoherenceLaw =
  PathQuotientHarness.quotientInterpreterCoherence @Int

sitePathObjects :: SitePathCategory Int -> [Ob (SitePathCategory Int)]
sitePathObjects category =
  siteObjects (sitePathManifest category)
    & Set.toList
    & mapMaybe (mkSitePathObject category)

sitePathMorphisms :: SitePathCategory Int -> [Mor (SitePathCategory Int)]
sitePathMorphisms category =
  siteObjects (sitePathManifest category)
    & Set.toList
    & allPairs
    >>= (\(sourceValue, targetValue) -> sitePathMorphismsBetween category sourceValue targetValue)

withSampleDiamondPathCategory :: (SitePathCategory Int -> Bool) -> Bool
withSampleDiamondPathCategory predicate =
  case sampleDiamondPathCategory of
    Left () -> False
    Right category -> predicate category

sampleSiteLaws :: Harness.SiteLaws Int Int
sampleSiteLaws = Harness.mkSiteLaws @Int @Int

lawSuites :: [LawSuite]
lawSuites =
  [ lawGroup
      "site"
      [ namedQuickCheckLaw FinCatWellFormed fincatWellFormedLaw,
        namedQuickCheckLaw SiteCoverageClosure (Harness.siteCoverageClosure sampleSiteLaws sampleSiteManifest),
        namedQuickCheckLaw SiteCategoryIdentity (Harness.siteCategoryIdentity sampleSiteLaws sampleSiteManifest),
        namedQuickCheckLaw SiteCategoryAssociativity (Harness.siteCategoryAssociativity sampleSiteLaws sampleSiteManifest),
        namedQuickCheckLaw SiteLayerPolicyConformance (Harness.siteLayerPolicyConformance sampleSiteLaws sampleLayerOf sampleLayerPolicy sampleSiteManifest),
        namedQuickCheckLaw SiteQuotientIdentity (withSampleDiamondPathCategory siteQuotientIdentityLaw),
        namedQuickCheckLaw SiteQuotientComposition (withSampleDiamondPathCategory siteQuotientCompositionLaw),
        namedQuickCheckLaw PathThinCodomainIdentity (withSampleDiamondPathCategory pathThinCodomainIdentityLaw),
        namedQuickCheckLaw PathThinCodomainComposition (withSampleDiamondPathCategory pathThinCodomainCompositionLaw),
        namedQuickCheckLaw PathQuotientUniqueness (withSampleDiamondPathCategory pathQuotientUniquenessLaw),
        namedQuickCheckLaw PathQuotientFaithful (withSampleDiamondPathCategory pathQuotientFaithfulLaw),
        namedQuickCheckLaw PathQuotientInterpreterCoherence (withSampleDiamondPathCategory pathQuotientInterpreterCoherenceLaw)
      ]
  ]
