{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import BenchMeasure (requireRight, timedValue)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Binary (Binary)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Moonlight.Planar.Internal.Paged (pagedOverlayPageCount)
import Moonlight.Planar.Internal.Representation
  ( Triangulation (..), mapVertices, mapDirectedEdges, mapUndirectedEdges, mapFaces )
import Moonlight.Planar.Internal.Types (KnownConstraintMode)
import Moonlight.Planar.Internal.Validation (validateTopology)
import Moonlight.Planar.Serialization
  ( DecodingBudget (..), decodeTriangulation, encodeTriangulation, trustedBinaryPayloadDecoders )
import Moonlight.Planar.SerializationFixtures
  ( SerializationFixture, serializationFixtures, serializationGridFixtures )
import Moonlight.Planar.SerializationV6Oracle (encodeV6Gathered)
import System.Mem (performGC)
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf)

main :: IO ()
main = do
  collinearFixtures <- concat <$> traverse prepare [10242, 32770]
  gridFixtures <- requireRight (serializationGridFixtures 10242)
  fixtures <- evaluate (force (collinearFixtures <> [("grid/10242/" <> label, mesh) | (label, mesh) <- gridFixtures]))
  unitFixtures <-
    evaluate . force $
      [ (label <> "/unit", mapVertices (const ()) . mapDirectedEdges (const ()) . mapUndirectedEdges (const ()) . mapFaces (const ()) $ mesh)
      | (label, mesh) <- fixtures
      ]
  traverse_ report fixtures
  traverse_ report unitFixtures
  defaultMain (map benchmarkPair fixtures <> map benchmarkPair unitFixtures)
 where
  prepare :: Int -> IO [(String, SerializationFixture)]
  prepare count = do
    fixtures <- requireRight (serializationFixtures count)
    evaluate (force [(show count <> "/" <> label, mesh) | (label, mesh) <- fixtures])

  benchmarkPair
    :: forall mode vertex directed undirected face.
       ( KnownConstraintMode mode, Binary vertex, Binary directed, Binary undirected, Binary face
       , NFData vertex, NFData directed, NFData undirected, NFData face )
    => (String, Triangulation mode vertex directed undirected face)
    -> Benchmark
  benchmarkPair (label, mesh) =
    bgroup label
      [ bench "gathered-v6-oracle" (nf encodeV6Gathered mesh)
      , bench "pagewise-v6" (nf encodeTriangulation mesh)
      , bench "decode-v6"
          (nf (first show . decodeTriangulation @mode @vertex @directed @undirected @face
            (DecodingBudget 100_000_000 100_000_000) trustedBinaryPayloadDecoders)
            (encodeTriangulation mesh))
      , bench "validate-topology" (nf validateTopology mesh)
      , bench "map-vertices" (nf (mapVertices (\value -> (value, True))) mesh)
      , bench "map-faces" (nf (mapFaces (\value -> (value, True))) mesh)
      ]

  report
    :: (KnownConstraintMode mode, Binary vertex, Binary directed, Binary undirected, Binary face)
    => (String, Triangulation mode vertex directed undirected face)
    -> IO ()
  report (label, mesh) = do
    performGC
    gathered <- timedValue (label <> "/gathered-v6-oracle") (pure (encodeV6Gathered mesh))
    performGC
    pagewise <- timedValue (label <> "/pagewise-v6") (pure (encodeTriangulation mesh))
    unless (gathered == pagewise) (fail (label <> ": V6 bytes differ"))
    putStrLn
      ( label <> " bytes=" <> show (BL.length pagewise)
          <> " shared-topology-pages=" <> show (pagedOverlayPageCount (triHalfTopology mesh))
      )
