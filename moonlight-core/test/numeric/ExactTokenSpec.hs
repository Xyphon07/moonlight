{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}

module ExactTokenSpec (tests) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Short qualified as ShortByteString
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Word (Word8)
import Moonlight.Core
  ( ExactEncoding,
    ExactEncodingAtom (..),
    ExactToken,
    exactAtomEncoding,
    exactEncodingLength,
    exactSequenceEncoding,
    exactTokenBytes,
    exactTokenFromEncoding,
    exactTokenLength,
  )
import Moonlight.Core (IsLawName (..), constructorLawName)
import SourceShape (assertSourceShape)
import Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Gen, Testable, frequency, listOf, resize, sized, testProperty)

data ExactTokenLawName
  = ExactTokenAtomSubtypeSeparation
  | ExactTokenEncodingDeterminism
  | ExactTokenReferenceEncodingAgreement
  | ExactTokenSequenceLengthLaw
  | ExactTokenEmptySequenceSeparation
  | ExactTokenSingletonSequenceSeparation
  | ExactTokenChildBoundarySeparation
  | ExactTokenNestingSeparation
  deriving stock (Eq, Ord, Show)

instance IsLawName ExactTokenLawName where
  lawNameText = constructorLawName . show

lawProperty :: Testable property => ExactTokenLawName -> property -> TestTree
lawProperty lawName =
  testProperty (lawNameText lawName)

lawCase :: ExactTokenLawName -> Assertion -> TestTree
lawCase lawName =
  testCase (lawNameText lawName)

assertExactTokenEncodingBoundaryShape :: IO ()
assertExactTokenEncodingBoundaryShape =
  assertSourceShape
    __FILE__
    "src-numeric/Moonlight/Core/ExactToken.hs"
    [ "exactTokenFromEncoding :: ExactEncoding -> ExactToken",
      "exactTokenBytes :: ExactToken -> Either ExactEncodingError ShortByteString",
      "exactSequenceMapEncoding :: Foldable values => (value -> ExactEncoding) -> values value -> ExactEncoding",
      "type ExactEncodingWriter =",
      "newByteArray#",
      "writeWord8Array#"
    ]
    [ "instance Semigroup ExactEncoding",
      "instance Monoid ExactEncoding",
      "exactConcatEncoding",
      "exactCountedEncoding",
      "Data.ByteString.Builder",
      "Builder.toLazyByteString",
      "LazyByteString.toStrict"
    ]

type SampleEncodingSeed :: Type
data SampleEncodingSeed
  = SampleAtom ExactEncodingAtom
  | SampleSequence [SampleEncodingSeed]
  deriving stock (Eq, Show)

sampleEncoding :: SampleEncodingSeed -> ExactEncoding
sampleEncoding seed =
  case seed of
    SampleAtom atom ->
      exactAtomEncoding atom
    SampleSequence children ->
      exactSequenceEncoding (fmap sampleEncoding children)

sampleExactToken :: SampleEncodingSeed -> ExactToken
sampleExactToken =
  exactTokenFromEncoding . sampleEncoding

sampleEncodingAtomGen :: Gen SampleEncodingSeed
sampleEncodingAtomGen =
  frequency
    [ (1, SampleAtom . ExactWord8 <$> arbitrary),
      (1, SampleAtom . ExactInt <$> arbitrary)
    ]

sampleEncodingGenSized :: Int -> Gen SampleEncodingSeed
sampleEncodingGenSized size =
  case size of
    0 ->
      sampleEncodingAtomGen
    _ ->
      frequency
        [ (2, sampleEncodingAtomGen),
          (1, SampleSequence <$> childSeedListGen)
        ]
  where
    childSeedSize = size `div` 2
    childSeedListGen =
      resize childSeedSize (listOf (sampleEncodingGenSized childSeedSize))

instance Arbitrary SampleEncodingSeed where
  arbitrary =
    sized sampleEncodingGenSized

sequenceLengthLaw :: [SampleEncodingSeed] -> Bool
sequenceLengthLaw children =
  exactEncodingLength (exactSequenceEncoding (fmap sampleEncoding children))
    == fmap (2 +) (sum <$> traverse (exactEncodingLength . sampleEncoding) children)

referenceEncoding :: SampleEncodingSeed -> Builder.Builder
referenceEncoding seed =
  case seed of
    SampleAtom (ExactWord8 byteValue) ->
      Builder.word8 0x01 <> Builder.word8 byteValue
    SampleAtom (ExactInt intValue) ->
      Builder.word8 0x02 <> Builder.int64BE (fromIntegral intValue :: Int64)
    SampleSequence children ->
      Builder.word8 0x03
        <> foldMap referenceEncoding children
        <> Builder.word8 0x04

productionBytes :: SampleEncodingSeed -> Either String LazyByteString.ByteString
productionBytes seed =
  case exactTokenBytes (exactTokenFromEncoding (sampleEncoding seed)) of
    Left encodingError -> Left (show encodingError)
    Right tokenBytes ->
      Right
        ( LazyByteString.fromStrict
            (ShortByteString.fromShort tokenBytes)
        )

referenceEncodingAgreement :: SampleEncodingSeed -> Bool
referenceEncodingAgreement seed =
  productionBytes seed == Right (Builder.toLazyByteString (referenceEncoding seed))

atomA :: SampleEncodingSeed
atomA = SampleAtom (ExactWord8 11)

atomB :: SampleEncodingSeed
atomB = SampleAtom (ExactInt 22)

atomC :: SampleEncodingSeed
atomC = SampleAtom (ExactWord8 33)

tests :: TestTree
tests =
  testGroup
    "exact-token"
    [ testCase "exact token boundary is one structural sequence algebra" assertExactTokenEncodingBoundaryShape,
      lawProperty ExactTokenEncodingDeterminism $
        \seed -> sampleExactToken seed == sampleExactToken seed,
      lawProperty ExactTokenReferenceEncodingAgreement referenceEncodingAgreement,
      lawProperty ExactTokenAtomSubtypeSeparation $
        \(byteValue :: Word8) ->
          sampleExactToken (SampleAtom (ExactWord8 byteValue))
            /= sampleExactToken (SampleAtom (ExactInt (fromIntegral byteValue))),
      lawProperty ExactTokenSingletonSequenceSeparation $
        \child -> sampleExactToken (SampleSequence [child]) /= sampleExactToken child,
      lawCase ExactTokenEmptySequenceSeparation $
        assertBool
          "an empty sequence must differ from a singleton empty sequence"
          ( sampleExactToken (SampleSequence [])
              /= sampleExactToken (SampleSequence [SampleSequence []])
          ),
      lawCase ExactTokenChildBoundarySeparation $
        assertBool
          "child boundaries must survive sequence encoding"
          ( sampleExactToken (SampleSequence [SampleSequence [atomA, atomB], atomC])
              /= sampleExactToken (SampleSequence [atomA, SampleSequence [atomB, atomC]])
          ),
      lawCase ExactTokenNestingSeparation $
        assertBool
          "left and right nesting must remain distinct"
          ( sampleExactToken (SampleSequence [SampleSequence [SampleSequence [atomA], atomB], atomC])
              /= sampleExactToken (SampleSequence [atomA, SampleSequence [atomB, SampleSequence [atomC]]])
          ),
      lawProperty ExactTokenSequenceLengthLaw sequenceLengthLaw,
      testProperty "sealed token length agrees with checked encoding length" $
        \seed ->
          exactTokenLength (exactTokenFromEncoding (sampleEncoding seed))
            == exactEncodingLength (sampleEncoding seed)
    ]
