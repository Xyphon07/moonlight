module SiteSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Category (allObjects)
import Moonlight.Category.Pure.Site.Compile
  ( ThinSiteObjectValueError (..),
    thinSiteFinObject,
    thinSiteImportKernel,
    thinSiteKernel,
    thinSiteKernelCodomain,
    thinSiteObjectValue,
  )
import Moonlight.Category.Pure.Site.Core (SiteFinCatError (..), SiteManifest (..), SiteViolation (..))
import Moonlight.Category.Pure.Site.Graph (importCycles, reachableClosure)
import Moonlight.Category.Pure.Site.Manifest (validateSiteManifest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Site"
    [ testCase
        "reachableClosure computes the transitive imports of an acyclic DAG"
        testReachableClosureClosesAcyclicDag,
      testCase
        "importCycles reports a singleton self-loop"
        testImportCyclesReportsSingletonSelfLoop,
      testCase
        "importCycles reports disjoint SCCs sorted by least object"
        testImportCyclesReportsDisjointComponentsInLeastObjectOrder,
      testCase
        "validateSiteManifest reports import cycles between declared objects"
        testValidateSiteManifestReportsDeclaredObjectCycle,
      testCase
        "thinSiteKernel rejects cyclic manifests before presentation"
        testThinSiteKernelRejectsDeclaredObjectCycle,
      testCase
        "manifest validation and kernel compilation share diagnostics"
        testManifestValidationAndKernelDiagnosticsAgree,
      testCase
        "site kernel round-trips every semantic and codomain object"
        testThinSiteKernelObjectRoundTrips,
      testCase
        "site kernel rejects a finite object from a foreign codomain"
        testThinSiteKernelRejectsForeignCodomainObject,
      testCase
        "import kernel accepts import-valid cover-invalid manifests while the full kernel rejects them"
        testThinSiteImportKernelSeparatesCoverValidation,
      testCase
        "import and full kernels agree for a valid manifest"
        testThinSiteKernelsAgreeOnValidManifest,
      testCase
        "validateSiteManifest reports cover sets that are not closed under covered covers"
        testValidateSiteManifestReportsCoverClosureViolation
    ]

testReachableClosureClosesAcyclicDag :: Assertion
testReachableClosureClosesAcyclicDag =
  reachableClosure imports
    @?= Map.fromList
      [ ("api", set ["core"]),
        ("app", set ["api", "core", "ui"]),
        ("core", Set.empty),
        ("ui", set ["core"])
      ]
  where
    imports :: Map String (Set String)
    imports =
      Map.fromList
        [ ("api", set ["core"]),
          ("app", set ["api", "ui"]),
          ("core", Set.empty),
          ("ui", set ["core"])
        ]

testImportCyclesReportsSingletonSelfLoop :: Assertion
testImportCyclesReportsSingletonSelfLoop =
  importCycles manifest @?= ["root" :| []]
  where
    manifest :: SiteManifest String
    manifest =
      SiteManifest
        { siteObjects = set ["root"],
          siteImports = Map.singleton "root" (set ["root"]),
          siteCovers = Map.empty
        }

testImportCyclesReportsDisjointComponentsInLeastObjectOrder :: Assertion
testImportCyclesReportsDisjointComponentsInLeastObjectOrder =
  importCycles manifest @?= ["a" :| ["b"], "c" :| ["d"]]
  where
    manifest :: SiteManifest String
    manifest =
      SiteManifest
        { siteObjects = set ["a", "b", "c", "d", "x"],
          siteImports =
            Map.fromList
              [ ("c", set ["d"]),
                ("x", Set.empty),
                ("a", set ["b"]),
                ("d", set ["c"]),
                ("b", set ["a"])
              ],
          siteCovers = Map.empty
        }

testValidateSiteManifestReportsDeclaredObjectCycle :: Assertion
testValidateSiteManifestReportsDeclaredObjectCycle =
  validateSiteManifest declaredCycleManifest @?= [ImportCycleDetected ("domain" :| ["service"])]

testThinSiteKernelRejectsDeclaredObjectCycle :: Assertion
testThinSiteKernelRejectsDeclaredObjectCycle =
  case thinSiteKernel declaredCycleManifest of
    Left (SiteManifestInvalid violations) ->
      NonEmpty.toList violations @?= validateSiteManifest declaredCycleManifest
    Right _ -> assertFailure "cyclic manifest produced a validated site kernel"

testManifestValidationAndKernelDiagnosticsAgree :: Assertion
testManifestValidationAndKernelDiagnosticsAgree =
  case thinSiteKernel invalidCoverManifest of
    Left (SiteManifestInvalid violations) ->
      NonEmpty.toList violations @?= validateSiteManifest invalidCoverManifest
    Right _ -> assertFailure "invalid cover produced a validated site kernel"

testThinSiteKernelObjectRoundTrips :: Assertion
testThinSiteKernelObjectRoundTrips =
  case thinSiteKernel roundTripManifest of
    Left siteError ->
      assertFailure ("round-trip manifest failed to compile: " <> show siteError)
    Right kernel -> do
      let manifestObjects = Set.toAscList (siteObjects roundTripManifest)
          codomainObjects = allObjects (thinSiteKernelCodomain kernel)
      case traverse (thinSiteFinObject kernel) manifestObjects of
        Left lookupError ->
          assertFailure ("semantic object failed to compile: " <> show lookupError)
        Right finObjects ->
          case traverse (thinSiteObjectValue kernel) finObjects of
            Left objectValueError ->
              assertFailure ("compiled object failed to recover: " <> show objectValueError)
            Right recoveredObjects ->
              recoveredObjects @?= manifestObjects
      case traverse (thinSiteObjectValue kernel) codomainObjects of
        Left lookupError ->
          assertFailure ("codomain object failed to recover: " <> show lookupError)
        Right recoveredObjects ->
          Set.fromList recoveredObjects @?= siteObjects roundTripManifest

testThinSiteKernelRejectsForeignCodomainObject :: Assertion
testThinSiteKernelRejectsForeignCodomainObject =
  case (thinSiteKernel roundTripManifest, thinSiteKernel foreignCodomainManifest) of
    (Right kernel, Right foreignKernel) ->
      case thinSiteFinObject foreignKernel 0 of
        Left lookupError ->
          assertFailure ("foreign kernel did not produce its declared object: " <> show lookupError)
        Right foreignObject ->
          case thinSiteObjectValue kernel foreignObject of
            Left (ThinSiteForeignCodomainObject _ _ _) -> pure ()
            otherResult ->
              assertFailure ("foreign codomain object was not rejected: " <> show otherResult)
    (leftResult, rightResult) ->
      assertFailure
        ( "foreign-codomain fixtures failed to compile: "
            <> show (leftResult, rightResult)
        )

testThinSiteImportKernelSeparatesCoverValidation :: Assertion
testThinSiteImportKernelSeparatesCoverValidation =
  case (thinSiteImportKernel invalidCoverManifest, thinSiteKernel invalidCoverManifest) of
    (Right _, Left (SiteManifestInvalid _)) -> pure ()
    outcomes ->
      assertFailure ("import and full kernels did not separate cover validation: " <> show outcomes)

testThinSiteKernelsAgreeOnValidManifest :: Assertion
testThinSiteKernelsAgreeOnValidManifest =
  case (thinSiteImportKernel roundTripManifest, thinSiteKernel roundTripManifest) of
    (Right importKernel, Right fullKernel) -> do
      thinSiteKernelCodomain importKernel @?= thinSiteKernelCodomain fullKernel
      let manifestObjects = Set.toAscList (siteObjects roundTripManifest)
      case
          ( traverse (thinSiteFinObject importKernel) manifestObjects,
            traverse (thinSiteFinObject fullKernel) manifestObjects
          ) of
        (Right importObjects, Right fullObjects) ->
          importObjects @?= fullObjects
        outcomes ->
          assertFailure ("valid kernels disagreed about a manifest object: " <> show outcomes)
    outcomes ->
      assertFailure ("valid manifest failed to compile under one kernel scope: " <> show outcomes)

declaredCycleManifest :: SiteManifest String
declaredCycleManifest =
  let objects = set ["domain", "service"]
   in SiteManifest
        { siteObjects = objects,
          siteImports =
            Map.fromList
              [ ("domain", set ["service"]),
                ("service", set ["domain"])
              ],
          siteCovers = Map.fromList [("domain", objects), ("service", objects)]
        }

invalidCoverManifest :: SiteManifest Int
invalidCoverManifest =
  SiteManifest
    { siteObjects = Set.singleton 0,
      siteImports = Map.singleton 0 Set.empty,
      siteCovers = Map.singleton 0 (Set.singleton 1)
    }

roundTripManifest :: SiteManifest Int
roundTripManifest =
  SiteManifest
    { siteObjects = set [0, 1, 2],
      siteImports =
        Map.fromList
          [ (0, set [1]),
            (1, set [2]),
            (2, Set.empty)
          ],
      siteCovers =
        Map.fromList
          [ (0, set [1, 2]),
            (1, set [2]),
            (2, Set.empty)
          ]
    }

foreignCodomainManifest :: SiteManifest Int
foreignCodomainManifest =
  SiteManifest
    { siteObjects = set [0, 1, 2],
      siteImports =
        Map.fromList
          [ (0, set [1]),
            (1, Set.empty),
            (2, Set.empty)
          ],
      siteCovers =
        Map.fromList
          [ (0, set [1]),
            (1, Set.empty),
            (2, Set.empty)
          ]
    }

testValidateSiteManifestReportsCoverClosureViolation :: Assertion
testValidateSiteManifestReportsCoverClosureViolation =
  validateSiteManifest manifest @?= [CoverNotClosed "root" "leaf" (set ["support"])]
  where
    manifest :: SiteManifest String
    manifest =
      SiteManifest
        { siteObjects = set ["root", "leaf", "support"],
          siteImports =
            Map.fromList
              [ ("root", set ["leaf"]),
                ("leaf", set ["support"]),
                ("support", Set.empty)
              ],
          siteCovers =
            Map.fromList
              [ ("root", set ["leaf"]),
                ("leaf", set ["support"]),
                ("support", Set.empty)
              ]
        }

set :: Ord a => [a] -> Set a
set = Set.fromList
