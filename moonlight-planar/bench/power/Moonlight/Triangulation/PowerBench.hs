-- | Tasty-bench lanes for the exact bounded power-diagram owner.  The timed
-- lanes force the public result, while the one-shot report keeps the
-- structural receipt and a shared incremental allocation observation outside
-- the timing sample.  One construction is revealed, its diagram is forced,
-- and only then is the additional receipt-reachable section measured.
module Moonlight.Triangulation.PowerBench
  ( PowerBenchCase
  , prepareCases
  , reportCases
  , runBenchmarks
  ) where

import BenchMeasure (requireRight)
import BenchSupport (randomPoints)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import GHC.Conc.Sync (getAllocationCounter, setAllocationCounter)
import Moonlight.Triangulation
  ( BoundedPowerDiagram
  , ConvexPolygon
  , Point (Point)
  , PowerDiagramError
  , PowerDiagramReceipt
  , PowerSite
  , PowerWeight
  , RegularEditError
  , RegularEditResult
  , RegularTriangulation
  , RegularTriangulationReceipt
  , boundedPowerDiagram
  , boundedPowerDiagramFromRegular
  , convexPolygon
  , exactPoint
  , powerDiagramActiveBoundaries
  , powerDiagramCoincidentDominatedCells
  , powerDiagramCoincidentEquivalentCells
  , powerDiagramDomainVertices
  , powerDiagramBoundaryCompatibilityChecks
  , powerDiagramEmptyCells
  , powerDiagramExactIntersections
  , powerDiagramFinalCoordinateBitGrowth
  , powerDiagramFinalCoordinateBits
  , powerDiagramFinalDenominatorBits
  , powerDiagramInputSites
  , powerDiagramLowerDimensionalCells
  , powerDiagramMaximumAffineCoefficientBits
  , powerDiagramMaximumInputBits
  , powerDiagramPeakIntermediateBitGrowth
  , powerDiagramPeakIntermediateCoordinateBits
  , powerDiagramMaximumCellConstraints
  , powerDiagramPublishedCells
  , powerDiagramOracleCells
  , powerDiagramRegularEdges
  , powerDiagramRegularFaces
  , powerDiagramSubmittedSiteConstraints
  , powerSite
  , powerSiteLabel
  , powerSitePosition
  , powerSiteWeight
  , powerWeight
  , powerWeightExact
  , powerWeightFromExact
  , insertRegularSite
  , removeRegularSite
  , reweightRegularSites
  , regularTriangulation
  , regularEditChangedSites
  , regularEditTriangulation
  , regularEditTransitions
  , regularSiteDisposition
  , regularSites
  , RegularSiteDisposition (RegularSiteHidden)
  )
import System.Mem (performGC)
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf)

data PowerBenchFamily
  = WellConditioned
  | NearParallel
  | CollinearSlopes
  deriving stock (Eq, Ord, Show)

data PowerBenchCase = PowerBenchCase
  { powerBenchLabel :: !String
  , powerBenchDomain :: !ConvexPolygon
  , powerBenchSites :: !(NonEmpty (PowerSite Int))
  , powerBenchRegular :: !(RegularTriangulation Int)
  , powerBenchInsertionBase :: !(RegularTriangulation Int)
  , powerBenchInsertedSite :: !(PowerSite Int)
  , powerBenchReweight :: !PowerWeight
  , powerBenchCommonShift :: !(Map.Map Int PowerWeight)
  , powerBenchCoincidentLabel :: !Int
  , powerBenchCoincidentRegular :: !(RegularTriangulation Int)
  , powerBenchCoincidentReweight :: !PowerWeight
  , powerBenchHiddenEdit :: !(Maybe HiddenEditFixture)
  }

data HiddenEditFixture = HiddenEditFixture
  { hiddenEditLabel :: !Int
  , hiddenEditLowerWeight :: !PowerWeight
  }

type PowerResult =
  Either (PowerDiagramError Int) (BoundedPowerDiagram Int, PowerDiagramReceipt)

type DiagramResult =
  Either (PowerDiagramError Int) (BoundedPowerDiagram Int)

type RegularResult =
  Either
    (PowerDiagramError Int)
    (RegularTriangulation Int, RegularTriangulationReceipt)

type RegularEditBenchmarkResult =
  Either (RegularEditError Int) (RegularEditResult Int)

type RegularEditObservation =
  Either (RegularEditError Int) (Int, Int, Maybe (RegularSiteDisposition Int))

siteCounts :: [Int]
siteCounts = [16, 64, 128, 169, 256, 512, 2048]

families :: [PowerBenchFamily]
families = [WellConditioned, NearParallel, CollinearSlopes]

-- | Deterministic, fixed-domain fixture bands include the exhibit scale and a
-- larger scaling point.  Ordinary and nearly parallel families expose exact
-- clipping cost; the collinear family exposes upper-chain classification.
prepareCases :: IO [PowerBenchCase]
prepareCases =
  traverse prepareCase [(family, count) | family <- families, count <- siteCounts]

prepareCase :: (PowerBenchFamily, Int) -> IO PowerBenchCase
prepareCase (family, siteCount) = do
  domain <-
    requireRight
      ( convexPolygon
          ( exactPoint (-1) (-1)
              :| [exactPoint 1 (-1), exactPoint 1 1, exactPoint (-1) 1]
          )
      )
  sites <-
    traverse (prepareSite family) (zip [0 ..] (fixturePoints family siteCount))
  case NonEmpty.nonEmpty sites of
    Nothing -> fail "power benchmark fixture requires at least one site"
    Just nonEmptySites -> do
      regular <- requireRight (regularTriangulation nonEmptySites)
      insertionBaseSites <-
        case NonEmpty.nonEmpty (NonEmpty.init nonEmptySites) of
          Nothing -> fail "power edit benchmark requires at least two sites"
          Just baseSites -> pure baseSites
      insertionBase <- requireRight (regularTriangulation insertionBaseSites)
      replacementWeight <- requireRight (powerWeight 17)
      subordinateWeight <- requireRight (powerWeight (-1048576))
      subordinateReweight <- requireRight (powerWeight (-2097152))
      let representativeSite = NonEmpty.head nonEmptySites
          commonShift =
            Map.fromList
              [ ( powerSiteLabel site
                , powerWeightFromExact (powerWeightExact (powerSiteWeight site) + 1)
                )
              | site <- NonEmpty.toList nonEmptySites
              ]
      subordinateSite <-
        requireRight
          (powerSite siteCount (powerSitePosition representativeSite) subordinateWeight)
      subordinateInsertion <-
        requireRight (insertRegularSite subordinateSite (fst regular))
      let preparedRegular = fst regular
          hiddenEdit =
            fmap
              (\site ->
                 HiddenEditFixture
                   { hiddenEditLabel = powerSiteLabel site
                   , hiddenEditLowerWeight = subordinateWeight
                   })
              ( List.find
                  (\site ->
                     regularSiteDisposition (powerSiteLabel site) preparedRegular
                       == Just RegularSiteHidden)
                  (regularSites preparedRegular)
              )
      pure
        PowerBenchCase
          { powerBenchLabel = familyLabel family <> "/n=" <> show siteCount
          , powerBenchDomain = domain
          , powerBenchSites = nonEmptySites
          , powerBenchRegular = preparedRegular
          , powerBenchInsertionBase = fst insertionBase
          , powerBenchInsertedSite = NonEmpty.last nonEmptySites
          , powerBenchReweight = replacementWeight
          , powerBenchCommonShift = commonShift
          , powerBenchCoincidentLabel = powerSiteLabel subordinateSite
          , powerBenchCoincidentRegular = regularEditTriangulation subordinateInsertion
          , powerBenchCoincidentReweight = subordinateReweight
          , powerBenchHiddenEdit = hiddenEdit
          }

fixturePoints :: PowerBenchFamily -> Int -> [Point]
fixturePoints family siteCount =
  case family of
    WellConditioned -> randomPoints 0x9e3779b97f4a7c15 siteCount
    NearParallel -> nearParallelPoints siteCount
    CollinearSlopes -> collinearSlopePoints siteCount

nearParallelPoints :: Int -> [Point]
nearParallelPoints siteCount =
  [ Point
      (fromIntegral index / fromIntegral (siteCount + 1) * 1.8 - 0.9)
      (fromIntegral index / fromIntegral (siteCount + 1) * 1.8e-6 - 0.9e-6)
  | index <- [1 .. siteCount]
  ]

collinearSlopePoints :: Int -> [Point]
collinearSlopePoints siteCount =
  [ Point (fromIntegral index / 1024 - 0.5) 0
  | index <- [0 .. siteCount - 1]
  ]

familyLabel :: PowerBenchFamily -> String
familyLabel WellConditioned = "power-well-conditioned"
familyLabel NearParallel = "power-near-parallel"
familyLabel CollinearSlopes = "power-collinear-slopes"

prepareSite :: PowerBenchFamily -> (Int, Point) -> IO (PowerSite Int)
prepareSite family (label, point) = do
  let weightValue =
        case family of
          CollinearSlopes
            | odd label -> -(2 / 1048576)
            | otherwise -> 0
          WellConditioned -> fromIntegral (label `mod` 9 - 4) / 256
          NearParallel -> fromIntegral (label `mod` 9 - 4) / 256
  weight <- requireRight (powerWeight weightValue)
  requireRight (powerSite label point weight)

-- | Print one receipt and one allocation probe per fixture.  Tasty-bench
-- remains the authority for timing; this report is the derived structural
-- view that timing samples cannot carry.
reportCases :: [PowerBenchCase] -> IO ()
reportCases cases = do
  putStrLn "moonlight-triangulation exact bounded power-diagram benchmark"
  putStrLn
    "receipt-overhead-method: reveal one shared construction, force its diagram, then measure only the additional receipt-reachable allocation; strict work incurred while revealing the pair is not attributed"
  traverse_ reportCase cases

reportCase :: PowerBenchCase -> IO ()
reportCase benchmarkCase = do
  (sharedResult, allocation) <- measureSharedAllocation benchmarkCase
  case sharedResult of
    Left failure -> fail (powerBenchLabel benchmarkCase <> ": " <> show failure)
    Right (_, receipt) -> do
      putStrLn (renderReceipt (powerBenchLabel benchmarkCase) receipt)
      putStrLn
        ( powerBenchLabel benchmarkCase
            <> "-allocated-bytes/shared-full: "
            <> show (sharedFullAllocation allocation)
        )
      putStrLn
        ( powerBenchLabel benchmarkCase
            <> "-allocated-bytes/shared-through-diagram: "
            <> show (sharedDiagramAllocation allocation)
        )
      putStrLn
        ( powerBenchLabel benchmarkCase
            <> "-allocated-bytes/incremental-receipt-reachable-overhead: "
            <> show (sharedReceiptReachableAllocation allocation)
        )

runBenchmarks :: [PowerBenchCase] -> IO ()
runBenchmarks cases = defaultMain [bgroup "exact bounded power diagram" (concatMap caseBenchmarks cases)]

caseBenchmarks :: PowerBenchCase -> [Benchmark]
caseBenchmarks benchmarkCase =
  [ bench
      (powerBenchLabel benchmarkCase <> "/regular-topology")
      (nf constructRegularTopology benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/construct-with-receipt")
      (nf construct benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/construct-diagram-only")
      (nf constructDiagramOnly benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/clip-prepared-regular")
      (nf constructFromRegular benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/insert-local-observation")
      (nf observeInsertion benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/insert-and-normalize")
      (nf insertAndNormalize benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/remove-local-observation")
      (nf observeRemoval benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/remove-and-normalize")
      (nf removeAndNormalize benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/reweight-local-observation")
      (nf observeReweight benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/reweight-and-normalize")
      (nf reweightAndNormalize benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/common-weight-shift")
      (nf shiftEveryWeight benchmarkCase)
  ]
    <> topologyPreservingBenchmarks benchmarkCase

topologyPreservingBenchmarks :: PowerBenchCase -> [Benchmark]
topologyPreservingBenchmarks benchmarkCase =
  [ bench
      (powerBenchLabel benchmarkCase <> "/remove-coincident-subordinate")
      (nf removeCoincidentSubordinate benchmarkCase)
  , bench
      (powerBenchLabel benchmarkCase <> "/reweight-coincident-subordinate")
      (nf reweightCoincidentSubordinate benchmarkCase)
  ]
    <> maybe [] hiddenBenchmarks (powerBenchHiddenEdit benchmarkCase)
 where
  hiddenBenchmarks hidden =
    [ bench
        (powerBenchLabel benchmarkCase <> "/remove-hidden")
        (nf (removeHidden benchmarkCase) hidden)
    , bench
        (powerBenchLabel benchmarkCase <> "/reweight-hidden-downward")
        (nf (reweightHiddenDownward benchmarkCase) hidden)
    ]

construct :: PowerBenchCase -> PowerResult
construct benchmarkCase =
  boundedPowerDiagram
    (powerBenchDomain benchmarkCase)
    (powerBenchSites benchmarkCase)

constructDiagramOnly :: PowerBenchCase -> DiagramResult
constructDiagramOnly = fmap fst . construct

constructRegularTopology :: PowerBenchCase -> RegularResult
constructRegularTopology = regularTriangulation . powerBenchSites

constructFromRegular :: PowerBenchCase -> PowerResult
constructFromRegular benchmarkCase =
  boundedPowerDiagramFromRegular
    (powerBenchDomain benchmarkCase)
    (powerBenchRegular benchmarkCase)

insertAndNormalize
  :: PowerBenchCase
  -> RegularEditBenchmarkResult
insertAndNormalize benchmarkCase =
  insertRegularSite
    (powerBenchInsertedSite benchmarkCase)
    (powerBenchInsertionBase benchmarkCase)

observeInsertion
  :: PowerBenchCase
  -> RegularEditObservation
observeInsertion benchmarkCase =
  observeRegularEdit
    (powerSiteLabel (powerBenchInsertedSite benchmarkCase))
    (insertAndNormalize benchmarkCase)

removeAndNormalize
  :: PowerBenchCase
  -> RegularEditBenchmarkResult
removeAndNormalize benchmarkCase =
  removeRegularSite
    (powerSiteLabel (powerBenchInsertedSite benchmarkCase))
    (powerBenchRegular benchmarkCase)

observeRemoval
  :: PowerBenchCase
  -> RegularEditObservation
observeRemoval benchmarkCase =
  observeRegularEdit
    (powerSiteLabel (powerBenchInsertedSite benchmarkCase))
    (removeAndNormalize benchmarkCase)

reweightAndNormalize
  :: PowerBenchCase
  -> RegularEditBenchmarkResult
reweightAndNormalize benchmarkCase =
  reweightRegularSites
    ( Map.singleton
        (powerSiteLabel (powerBenchInsertedSite benchmarkCase))
        (powerBenchReweight benchmarkCase)
    )
    (powerBenchRegular benchmarkCase)

observeReweight
  :: PowerBenchCase
  -> RegularEditObservation
observeReweight benchmarkCase =
  observeRegularEdit
    (powerSiteLabel (powerBenchInsertedSite benchmarkCase))
    (reweightAndNormalize benchmarkCase)

observeRegularEdit
  :: Int
  -> RegularEditBenchmarkResult
  -> RegularEditObservation
observeRegularEdit focus =
  fmap
    (\editResult ->
       ( Set.size (regularEditChangedSites editResult)
       , Vector.length (regularEditTransitions editResult)
       , regularSiteDisposition
           focus
           (regularEditTriangulation editResult)
       ))

shiftEveryWeight
  :: PowerBenchCase
  -> RegularEditBenchmarkResult
shiftEveryWeight benchmarkCase =
  reweightRegularSites
    (powerBenchCommonShift benchmarkCase)
    (powerBenchRegular benchmarkCase)

removeCoincidentSubordinate
  :: PowerBenchCase
  -> RegularEditBenchmarkResult
removeCoincidentSubordinate benchmarkCase =
  removeRegularSite
    (powerBenchCoincidentLabel benchmarkCase)
    (powerBenchCoincidentRegular benchmarkCase)

reweightCoincidentSubordinate
  :: PowerBenchCase
  -> RegularEditBenchmarkResult
reweightCoincidentSubordinate benchmarkCase =
  reweightRegularSites
    ( Map.singleton
        (powerBenchCoincidentLabel benchmarkCase)
        (powerBenchCoincidentReweight benchmarkCase)
    )
    (powerBenchCoincidentRegular benchmarkCase)

removeHidden
  :: PowerBenchCase
  -> HiddenEditFixture
  -> RegularEditBenchmarkResult
removeHidden benchmarkCase fixture =
  removeRegularSite (hiddenEditLabel fixture) (powerBenchRegular benchmarkCase)

reweightHiddenDownward
  :: PowerBenchCase
  -> HiddenEditFixture
  -> RegularEditBenchmarkResult
reweightHiddenDownward benchmarkCase fixture =
  reweightRegularSites
    (Map.singleton (hiddenEditLabel fixture) (hiddenEditLowerWeight fixture))
    (powerBenchRegular benchmarkCase)

data SharedPowerAllocation = SharedPowerAllocation
  { sharedFullAllocation :: !Integer
  , sharedDiagramAllocation :: !Integer
  , sharedReceiptReachableAllocation :: !Integer
  }

measureSharedAllocation
  :: PowerBenchCase
  -> IO (PowerResult, SharedPowerAllocation)
measureSharedAllocation benchmarkCase = do
  performGC
  setAllocationCounter maxBound
  allocationStart <- getAllocationCounter
  sharedResult <- evaluate (construct benchmarkCase)
  case sharedResult of
    Left failure -> do
      forcedFailure <- evaluate (force failure)
      allocationEnd <- getAllocationCounter
      let total = allocationDifference allocationStart allocationEnd
      pure
        ( Left forcedFailure
        , SharedPowerAllocation total total 0
        )
    Right (diagram, receipt) -> do
      forcedDiagram <- evaluate (force diagram)
      allocationAfterDiagram <- getAllocationCounter
      forcedReceipt <- evaluate (force receipt)
      allocationAfterReceipt <- getAllocationCounter
      pure
        ( Right (forcedDiagram, forcedReceipt)
        , SharedPowerAllocation
            { sharedFullAllocation =
                allocationDifference allocationStart allocationAfterReceipt
            , sharedDiagramAllocation =
                allocationDifference allocationStart allocationAfterDiagram
            , sharedReceiptReachableAllocation =
                allocationDifference allocationAfterDiagram allocationAfterReceipt
            }
        )

allocationDifference :: Integral amount => amount -> amount -> Integer
allocationDifference before after = toInteger (max 0 (before - after))

renderReceipt :: String -> PowerDiagramReceipt -> String
renderReceipt label receipt =
  unwords
    [ label <> "-receipt:"
    , field "input-sites" powerDiagramInputSites
    , field "domain-vertices" powerDiagramDomainVertices
    , field "submitted-site-constraints" powerDiagramSubmittedSiteConstraints
    , field "active-boundaries" powerDiagramActiveBoundaries
    , field "boundary-compatibility-checks" powerDiagramBoundaryCompatibilityChecks
    , field "exact-intersections" powerDiagramExactIntersections
    , field "published" powerDiagramPublishedCells
    , field "lower-dimensional" powerDiagramLowerDimensionalCells
    , field "empty" powerDiagramEmptyCells
    , field "coincident-equivalent" powerDiagramCoincidentEquivalentCells
    , field "coincident-dominated" powerDiagramCoincidentDominatedCells
    , field "regular-faces" powerDiagramRegularFaces
    , field "regular-edges" powerDiagramRegularEdges
    , field "oracle-cells" powerDiagramOracleCells
    , field "maximum-cell-constraints" powerDiagramMaximumCellConstraints
    , field "input-bits" powerDiagramMaximumInputBits
    , field "affine-bits" powerDiagramMaximumAffineCoefficientBits
    , field "peak-coordinate-bits" powerDiagramPeakIntermediateCoordinateBits
    , field "final-coordinate-bits" powerDiagramFinalCoordinateBits
    , field "final-denominator-bits" powerDiagramFinalDenominatorBits
    , field "peak-growth-bits" powerDiagramPeakIntermediateBitGrowth
    , field "final-growth-bits" powerDiagramFinalCoordinateBitGrowth
    ]
 where
  field :: Show value => String -> (PowerDiagramReceipt -> value) -> String
  field name project = name <> "=" <> show (project receipt)
