{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The serialization slice: the versioned binary envelope and its refusals.
module Moonlight.Triangulation.SerializationSpec (tests) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Binary (Binary)
import Data.Binary.Put (putWord16be, putWord32be, putWord64be, putWord8, runPut)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Vector as V
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Moonlight.Triangulation
import Moonlight.Triangulation.Internal.HandleDefs (DirectedEdgeId (..), FaceId (..))
import Moonlight.Triangulation.Serialization
import Moonlight.Triangulation.Types (KnownConstraintMode)
import Support (assertEqual, assertValid, requireRight)

tests :: IO ()
tests = do
  testRoundTrip
  testDegenerateCardinalityRoundTrips
  testConstrainedRoundTrip
  testIndependentPayloadGeometryRoundTrip
  testPointPayloadRoundTrip
  testCanonicalizesSerializedSignedZero
  testRejectsHostileStructuralPrefixes
  testRejectsOutOfRangeOuterFaceReference
  testRejectsCorruption
  putStrLn "all serialization tests passed"

data SerialVertex = SerialVertex
  { serialPosition :: !(Point)
  , serialLabel :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Binary)

instance HasPosition SerialVertex where
  position = serialPosition

type SerialTriangulation = Triangulation 'Unconstrained SerialVertex Int Bool String

testDecodingBudget :: DecodingBudget
testDecodingBudget =
  DecodingBudget
    { decodingMaximumInputBytes = 10_000_000
    , decodingMaximumSectionElements = 10_000_000
    }

source :: IO SerialTriangulation
source = do
  let defaults = ElementDefaults (3 :: Int) True ("face" :: String)
      payloads =
        V.fromList
          [ SerialVertex (Point 0 0) 10
          , SerialVertex (Point 2 0) 20
          , SerialVertex (Point 0 2) 30
          , SerialVertex (Point 0.5 0.5) 40
          ]
  buildTriangulation <$> requireRight "serialization source" (delaunay defaults payloads)

testRoundTrip :: IO ()
testRoundTrip = do
  original <- source
  let bytes = encodeTriangulation original
  unless (BL.length bytes > 0) $ fail "serialization produced an empty payload"
  assertSerializationRoundTrip "serialization" original

testDegenerateCardinalityRoundTrips :: IO ()
testDegenerateCardinalityRoundTrips =
  traverse_
    roundTripGeometry
    [ ("empty", V.empty)
    , ("singleton", V.singleton (Point 0 0))
    , ("segment", V.fromList [Point 0 0, Point 1 0])
    , ("collinear chain", V.fromList [Point 0 0, Point 1 0, Point 2 0, Point 3 0])
    ]
 where
  roundTripGeometry (label, points) = do
    original <- requireRight (label <> " serialization source") (delaunayGeometry points)
    assertSerializationRoundTrip (label <> " serialization") original

testConstrainedRoundTrip :: IO ()
testConstrainedRoundTrip = do
  built <-
    requireRight
      "constrained serialization source"
      ( constrainedDelaunay
          unitElementDefaults
          (V.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2])
          (V.singleton (0, 2))
      )
  let original = buildTriangulation built
  assertSerializationRoundTrip "constrained serialization" original

-- Vertex payload positions are annotations after ingestion. Serialization must
-- therefore preserve the fixed geometry and the independently edited payload,
-- rather than letting the latter reauthor the former on decode.
testIndependentPayloadGeometryRoundTrip :: IO ()
testIndependentPayloadGeometryRoundTrip = do
  geometry <- source
  vertex <- case vertices geometry of
    (first : _) -> pure first
    [] -> fail "independent payload fixture has no vertices"
  let independentPayload = SerialVertex (Point 91 73) 1010
      original = setVertexData geometry vertex independentPayload
      positionless = mapVertices serialLabel original
  assertEqual "independent payload leaves geometry fixed"
    (vertexPoint geometry vertex) (vertexPoint original vertex)
  assertEqual "independent payload position is stored"
    (Point 91 73) (serialPosition (vertexData original vertex))
  assertSerializationRoundTrip "independent payload serialization" original
  assertSerializationRoundTrip "positionless payload serialization" positionless

testPointPayloadRoundTrip :: IO ()
testPointPayloadRoundTrip = do
  let points = V.fromList [Point 0 0, Point 2 0, Point 0 2, Point 0.5 0.5] :: V.Vector (Point)
  built <- requireRight "point payload source" (delaunay unitElementDefaults points)
  let geometry = buildTriangulation built
  vertex <- case vertices geometry of
    (first : _) -> pure first
    [] -> fail "point payload fixture has no vertices"
  let original = setVertexData geometry vertex (Point 13 17)
  assertEqual "point payload leaves geometry fixed"
    (vertexPoint geometry vertex) (vertexPoint original vertex)
  assertEqual "point payload is stored"
    (Point 13 17) (vertexData original vertex)
  assertSerializationRoundTrip "point payload serialization" original

-- The wire format admits every binary64 bit pattern, but resident point identity
-- admits only canonical zero. Mutate the first singleton coordinate to negative
-- zero and require decode to restore the canonical representation.
testCanonicalizesSerializedSignedZero :: IO ()
testCanonicalizesSerializedSignedZero = do
  original <- requireRight "signed-zero source" (delaunayGeometry (V.singleton (Point 0 0)))
  let bytes = encodeTriangulation original
      structuralPrefixSize = 8 + 2 + 1 + 1 + 4 * 8
      signedZeroBytes =
        BL.concat
          [ BL.take structuralPrefixSize bytes
          , BL.singleton 0x80
          , BL.drop (structuralPrefixSize + 1) bytes
          ]
  decoded <-
    requireRight
      "signed-zero decode"
      ( decodeTriangulation testDecodingBudget trustedBinaryPayloadDecoders signedZeroBytes
          :: Either SerializationError (Triangulation 'Unconstrained () () () ())
      )
  vertex <- case vertices decoded of
    [onlyVertex] -> pure onlyVertex
    unexpected -> fail ("signed-zero fixture produced " <> show (length unexpected) <> " vertices")
  assertEqual "signed-zero coordinate canonicalization" (Point 0 0) (vertexPoint decoded vertex)
  assertEqual "signed-zero canonical re-encoding" bytes (encodeTriangulation decoded)

assertSerializationRoundTrip
  :: ( KnownConstraintMode mode
     , Binary vertex
     , Binary directed
     , Binary undirected
     , Binary face
     , Eq vertex
     , Eq directed
     , Eq undirected
     , Eq face
     , Show vertex
     , Show directed
     , Show undirected
     , Show face
     )
  => String
  -> Triangulation mode vertex directed undirected face
  -> IO ()
assertSerializationRoundTrip label original = do
  decoded <-
    requireRight
      (label <> " round trip")
      (decodeTriangulation testDecodingBudget trustedBinaryPayloadDecoders (encodeTriangulation original))
  assertEqual (label <> " equality") original decoded
  assertValid (label <> " validity") decoded

-- Counts are one prefix precisely so these refusals precede all default and
-- payload decoders. The hostile fixtures use @()@, whose lawful decoder consumes
-- no bytes, to exercise the formerly allocative attack rather than relying on
-- truncation to save the process.
testRejectsHostileStructuralPrefixes :: IO ()
testRejectsHostileStructuralPrefixes =
  traverse_
    (\(label, budget, bytes, failure) ->
       assertDecodeFailure label budget bytes failure)
    [ ( "input byte budget"
      , DecodingBudget 43 10_000
      , structuralPrefix 6 0 0 1 0
      , InputByteBudgetExceeded 44 43
      )
    , ( "section element budget"
      , DecodingBudget 1_000 1_000_000
      , structuralPrefix 6 1_000_000 1_999_998 1 0
      , DecodedSectionBudgetExceeded 15_999_990 1_000_000
      )
    , ("directed edge parity", testDecodingBudget, structuralPrefix 6 0 1 1 0, SerializedDirectedEdgeCountOdd 1)
    , ("missing outer face", testDecodingBudget, structuralPrefix 6 0 0 0 0, SerializedMissingOuterFace)
    , ("constraint count relationship", testDecodingBudget, structuralPrefix 6 0 2 1 2, SerializedConstraintCountExceedsEdges 2 1)
    , ("planar cardinality relationship", testDecodingBudget, structuralPrefix 6 2 0 1 0, SerializedPlanarCardinalityMismatch 2 0 1)
    , ("fixed body lower bound", testDecodingBudget, structuralPrefix 6 1 0 1 0, SerializedFixedBodyTooShort 0 24)
    , ("host Int vertex count", DecodingBudget 1_000 maxBound, structuralPrefix 6 maxBound 0 1 0, EncodedCountExceedsInt SerializedVertexCount maxBound)
    , ( "packed vertex count"
      , DecodingBudget 1_000 maxBound
      , structuralPrefix 6 4_294_967_296 0 1 0
      , EncodedCountExceedsPackedIndex SerializedVertexCount 4_294_967_296 4_294_967_295
      )
    , ("version 5", testDecodingBudget, structuralPrefix 5 0 0 1 0, UnsupportedFormatVersion 5)
    ]

-- Range validation must obstruct before relational validation dereferences the
-- hostile handle. Fully comparing the error forces its complete violation
-- inventory and therefore pins the absence of a lazy unsafe-index crash.
testRejectsOutOfRangeOuterFaceReference :: IO ()
testRejectsOutOfRangeOuterFaceReference =
  assertDecodeFailure
    "out-of-range outer-face reference"
    testDecodingBudget
    (structuralPrefix 6 0 0 1 0 <> runPut (putWord32be 0 >> putWord8 0))
    ( DecodedInvariantViolations
        (FaceAdjacentOutOfRange (FaceId 0) (DirectedEdgeId 0) 0 :| [])
    )

assertDecodeFailure
  :: String
  -> DecodingBudget
  -> BL.ByteString
  -> SerializationError
  -> IO ()
assertDecodeFailure label budget bytes expected =
  assertEqual
    label
    (Left expected)
    ( decodeTriangulation budget trustedBinaryPayloadDecoders bytes
        :: Either SerializationError (Triangulation 'Unconstrained () () () ())
    )

structuralPrefix :: Word16 -> Word64 -> Word64 -> Word64 -> Word64 -> BL.ByteString
structuralPrefix version vertexCount directedEdgeCount faceCount constraintCount =
  runPut $ do
    putWord64be 0x5350414445485307
    putWord16be version
    putWord16be 2
    traverse_
      putWord64be
      [vertexCount, directedEdgeCount, faceCount, constraintCount]

-- The header is the part of the stream that is structurally constrained: magic,
-- version, constraint mode and coordinate encoding each have exactly one admissible
-- byte pattern, so every mutation of them must be refused. Beyond the header
-- the stream carries payload values, and a byte flipped inside an element
-- payload names a different but entirely legal value — the guarantee there is
-- not refusal but soundness: a decoder that rebuilds its indexes rather than
-- trusting them may never surface a triangulation that violates its invariants,
-- whatever it is fed.
testRejectsCorruption :: IO ()
testRejectsCorruption = do
  original <- source
  let bytes = encodeTriangulation original
      size = BL.length bytes
      envelopeSize = 8 + 2 + 1 + 1
      structuralPrefixSize = envelopeSize + 4 * 8
      decode candidate = decodeTriangulation testDecodingBudget trustedBinaryPayloadDecoders candidate :: Either SerializationError SerialTriangulation
      flipAt offset =
        BL.concat [BL.take offset bytes, BL.singleton (BL.index bytes offset + 1), BL.drop (offset + 1) bytes]
      rejects :: String -> BL.ByteString -> IO ()
      rejects label candidate =
        case decode candidate of
          Left _ -> pure ()
          Right _ -> fail ("decoder accepted " <> label)
  assertEqual
    "typed trailing-byte refusal"
    (Left (TrailingBytes 1))
    (decode (bytes <> BL.singleton 0))
  case decode (BL.cons 0 (BL.drop 1 bytes)) of
    Left (InvalidFormatMagic _) -> pure ()
    other -> fail ("magic corruption produced " <> show other)
  rejects "an empty payload" BL.empty
  traverse_
    (\dropped -> rejects ("a payload truncated by " <> show dropped) (BL.take (size - dropped) bytes))
    [1 .. size]
  traverse_
    (\offset -> rejects ("an envelope byte flipped at offset " <> show offset) (flipAt offset))
    [0 .. envelopeSize - 1]
  traverse_
    (\offset -> rejects ("a structural-prefix byte flipped at offset " <> show offset) (flipAt offset))
    [envelopeSize .. structuralPrefixSize - 1]
  traverse_ (\offset ->
    case decode (flipAt offset) of
      Left _ -> pure ()
      Right decoded ->
        assertValid ("a byte flipped at offset " <> show offset <> " decoded to") decoded
    ) [structuralPrefixSize .. size - 1]
