module Main
  ( main,
  )
where

import Control.DeepSeq (NFData (rnf), force)
import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Word (Word64)
import DiagnosticBench
  ( RestrictionCorpus,
    RestrictionDigest (..),
    distinctRestrictionCorpus,
    outcomeSummaryLeftFold,
    outcomeSummaryMconcat,
    repeatedRestrictionCorpus,
    restrictionHotspotDigest,
    restrictionIndexStatsDigest,
  )
import GhcSurfaceBench
  ( PreparedConversionCorpus,
    commonSubsetSemanticManifests,
    conversionBenchmarkDigestHash,
    convertCommonCorpus,
    prepareCommonSubsetCorpus,
  )
import Moonlight.Pale.Bench.Measure
  ( RtsDelta (..),
    RtsMeasurement (..),
    measureSample,
  )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data DiagnosticReceiptSpec = DiagnosticReceiptSpec
  { diagnosticReceiptLabel :: !String,
    diagnosticReceiptCorpus :: Int -> RestrictionCorpus,
    diagnosticReceiptWorkload :: RestrictionCorpus -> RestrictionDigest
  }

data EquivalenceReceipt = EquivalenceReceipt
  { receiptLabel :: !String,
    receiptElapsedNanoseconds :: !Word64,
    receiptAllocatedBytes :: !Word64,
    receiptCopiedBytes :: !Word64,
    receiptDigest :: !Int
  }
  deriving stock (Eq, Show)

main :: IO ()
main =
  case (commonSubsetSemanticManifests, prepareCommonSubsetCorpus 128) of
    (Left obstruction, _) ->
      rejectReceipt "semantic manifest" obstruction
    (_, Left obstruction) ->
      rejectReceipt "conversion input" obstruction
    (Right manifests, Right (_, conversionInput)) -> do
      diagnosticResults <- traverse measureDiagnosticReceipt diagnosticReceiptSpecs
      conversionResult <- measureConversionReceipt conversionInput
      case sequence (diagnosticResults <> [conversionResult]) of
        Left failure -> do
          hPutStrLn stderr ("moonlight-pale equivalence receipt failed: " <> failure)
          exitFailure
        Right receipts -> do
          traverse_
            (putStrLn . ("conversion-semantic-manifest " <>) . show)
            manifests
          traverse_ print receipts

rejectReceipt :: Show obstruction => String -> obstruction -> IO ()
rejectReceipt receiptLabel obstruction = do
  hPutStrLn stderr ("moonlight-pale " <> receiptLabel <> " rejected: " <> show obstruction)
  exitFailure

diagnosticReceiptSpecs :: [DiagnosticReceiptSpec]
diagnosticReceiptSpecs =
  foldMap
    ( \(regimeLabel, corpusFromSize) ->
        fmap
          (\(workloadLabel, workload) -> DiagnosticReceiptSpec (regimeLabel <> "/" <> workloadLabel) corpusFromSize workload)
          diagnosticWorkloads
    )
    [ ("repeated-cardinality", repeatedRestrictionCorpus),
      ("distinct-cardinality", distinctRestrictionCorpus)
    ]

diagnosticWorkloads :: [(String, RestrictionCorpus -> RestrictionDigest)]
diagnosticWorkloads =
  [ ("outcome-summary-mconcat", outcomeSummaryMconcat),
    ("outcome-summary-left-fold", outcomeSummaryLeftFold),
    ("restriction-index-stats", restrictionIndexStatsDigest),
    ("restriction-hotspots-top-16", restrictionHotspotDigest)
  ]

measureDiagnosticReceipt :: DiagnosticReceiptSpec -> IO (Either String EquivalenceReceipt)
measureDiagnosticReceipt specification = do
  measurementResult <-
    measureSample
      1
      (\_ -> evaluate (force (diagnosticReceiptCorpus specification 16384)))
      (\corpus -> pure (Right (diagnosticReceiptWorkload specification corpus) :: Either String RestrictionDigest))
      rnf
      restrictionDigestHash
  pure
    ( fmap
        (measurementReceipt ("diagnostic/" <> diagnosticReceiptLabel specification))
        (first show measurementResult)
    )

measureConversionReceipt :: PreparedConversionCorpus -> IO (Either String EquivalenceReceipt)
measureConversionReceipt preparedInput = do
  measurementResult <-
    measureSample
      1
      (\_ -> evaluate (force preparedInput))
      (pure . convertCommonCorpus)
      rnf
      conversionBenchmarkDigestHash
  pure
    ( fmap
        (measurementReceipt "ghc-surface/common-subset-convert-and-normalize/bindings/128")
        (first show measurementResult)
    )

measurementReceipt :: String -> RtsMeasurement value -> EquivalenceReceipt
measurementReceipt label measurement =
  EquivalenceReceipt
    { receiptLabel = label,
      receiptElapsedNanoseconds = rtsMeasurementElapsedNanoseconds measurement,
      receiptAllocatedBytes = rtsDeltaAllocatedBytes (rtsMeasurementDelta measurement),
      receiptCopiedBytes = rtsDeltaCopiedBytes (rtsMeasurementDelta measurement),
      receiptDigest = rtsMeasurementDigest measurement
    }
