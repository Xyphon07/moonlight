module StableHashSpec (tests) where

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder qualified as Builder
import Data.Foldable (traverse_)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word64, Word8)
import Moonlight.Core
  ( defaultStableHashKey,
    sipHashFinalize,
    sipHash24,
    sipHashDigest,
    sipHashInit,
    sipHashUpdateByteString,
    stableHashBuilder,
    stableHashByteStrings,
    stableHashEncodingByteString,
    stableHashEncodingChunks,
    stableHashEncodingDigest,
    stableHashEncodingTextUtf8,
    stableHashEncodingVersion,
    stableHashEncodingWord64Dec,
    stableHashEncodingWord64LE,
    stableHashEncodingWord8,
    unStableHashDigest,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "stable_hash"
    [ testCase "sipHash24 is deterministic for identical inputs" $
        sipHash24 17 23 (BS8.pack "meridian") @?= sipHash24 17 23 (BS8.pack "meridian"),
      testCase "byte framing encoding is versioned" $
        stableHashEncodingVersion @?= 2,
      testCase "byte-string chunks are deterministically framed" $
        stableHashByteStrings [BS8.pack "moon", BS8.pack "light"]
          @?= stableHashByteStrings [BS8.pack "moon", BS8.pack "light"],
      testCase "builder payloads are deterministically hashed" $
        stableHashBuilder (Builder.string8 "moonlight")
          @?= stableHashBuilder (Builder.string8 "moonlight"),
      testCase "builder streaming matches materialized versioned preimage" $
        unStableHashDigest (stableHashBuilder (Builder.string8 "moonlight" <> Builder.word64LE 42))
          @?= sipHashDigest
            defaultStableHashKey
            ( LazyByteString.toStrict
                ( Builder.toLazyByteString
                    ( Builder.word64LE stableHashEncodingVersion
                        <> Builder.string8 "moonlight"
                        <> Builder.word64LE 42
                    )
                )
            ),
      testCase "encoding writers hash without materialized builder payloads" $
        stableHashEncodingDigest
          defaultStableHashKey
          ( stableHashEncodingWord64LE stableHashEncodingVersion
              <> stableHashEncodingWord8 0x6d
              <> stableHashEncodingWord64LE 42
          )
          @?= stableHashBuilder (Builder.word8 0x6d <> Builder.word64LE 42),
      testCase "text utf8 writer matches encodeUtf8 bytes" $
        let textValue =
              Text.pack "moonλ🌙"
         in stableHashEncodingDigest
              defaultStableHashKey
              (stableHashEncodingWord64LE stableHashEncodingVersion <> stableHashEncodingTextUtf8 textValue)
              @?= stableHashBuilder (Builder.byteString (TextEncoding.encodeUtf8 textValue)),
      testProperty "word64 decimal writer matches materialized decimal bytes" $
        \(wordValue :: Word64) ->
          stableHashEncodingDigest
            defaultStableHashKey
            (stableHashEncodingWord64LE stableHashEncodingVersion <> stableHashEncodingWord64Dec wordValue)
            === stableHashBuilder (Builder.word64Dec wordValue),
      testCase "byte-string block reader preserves partial-state overlaps" $
        let sampleBytes byteLength =
              ByteString.pack (take byteLength (cycle [0 .. 251]))
            prefixes =
              fmap sampleBytes [0 .. 15]
            payloads =
              fmap sampleBytes [0 .. 40]
            assertOverlap prefix payload =
              sipHashFinalize
                ( sipHashUpdateByteString
                    (sipHashUpdateByteString (sipHashInit defaultStableHashKey) prefix)
                    payload
                )
                @?= sipHashDigest defaultStableHashKey (prefix <> payload)
         in traverse_ (\prefix -> traverse_ (assertOverlap prefix) payloads) prefixes,
      testCase "sipHash24 reacts to key changes" $
        assertBool
          "different keys should perturb the digest"
          (sipHash24 17 23 (BS8.pack "meridian") /= sipHash24 17 24 (BS8.pack "meridian")),
      testProperty "framed fold is byte-for-byte identical to materialized one-shot sipHash" $
        \chunkWordLists ->
          let chunks = map ByteString.pack (chunkWordLists :: [[Word8]])
           in unStableHashDigest (stableHashByteStrings chunks)
                === sipHashDigest defaultStableHashKey (referenceFramedBytes chunks),
      testProperty "encoding chunks preserve byte-string framing" $
        \chunkWordLists ->
          let chunks = map ByteString.pack (chunkWordLists :: [[Word8]])
           in stableHashEncodingChunks stableHashEncodingByteString chunks
                === stableHashByteStrings chunks
    ]

-- The exact byte preimage 'stableHashByteStrings' streams into SipHash: version
-- word, compact chunk count, then each compact length-prefixed chunk. Hashing
-- this in one shot with @memory@'s 'sipHashDigest' proves the streamed fold
-- matches the authoritative framed byte sequence for every corpus.
referenceFramedBytes :: [ByteString] -> ByteString
referenceFramedBytes chunks =
  LazyByteString.toStrict
    ( Builder.toLazyByteString
        ( Builder.word64LE stableHashEncodingVersion
            <> compactWord64Builder (fromIntegral (length chunks))
            <> foldMap encodeChunk chunks
        )
    )
  where
    encodeChunk chunk =
      compactWord64Builder (fromIntegral (ByteString.length chunk))
        <> Builder.byteString chunk

compactWord64Builder :: Word64 -> Builder.Builder
compactWord64Builder wordValue =
  let byteValue =
        fromIntegral (wordValue `rem` 128)
      remainingValue =
        wordValue `quot` 128
   in if remainingValue == 0
        then Builder.word8 byteValue
        else Builder.word8 (byteValue + 128) <> compactWord64Builder remainingValue
