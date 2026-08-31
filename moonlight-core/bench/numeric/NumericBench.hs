module NumericBench
  ( numericBenchmarks,
  )
where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Scientific qualified as Scientific
import Data.Word
  ( Word32,
  )
import Moonlight.Core
  ( AbsTol,
    ApproxEq (..),
    absTol,
  )
import BenchSupport
  ( caseLabel,
    keys,
    numericSizes,
    showLength,
  )
import Moonlight.Core
  ( canonicalize,
    quantizeForHash,
  )
import Moonlight.Core
  ( CanonicalNumber,
    canonicalNumberToMaybeDouble,
    mkCanonicalFiniteNumber,
  )
import Moonlight.Core
  ( ExactEncoding,
    ExactEncodingAtom (..),
    exactAtomEncoding,
    exactSequenceMapEncoding,
    exactTokenFromEncoding,
    exactTokenLength,
  )
import Moonlight.Core
  ( MoonlightError,
  )
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MultiplicativeMonoid (..),
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    nf,
  )
import Prelude

numericBenchmarks :: Benchmark
numericBenchmarks =
  bgroup
    "numeric"
    (numericSizes >>= numericBenchmarksForSize)

numericBenchmarksForSize :: Int -> [Benchmark]
numericBenchmarksForSize size =
  [ bench (caseLabel "canonicalize doubles" size) (nf canonicalizeWeight size),
    bench (caseLabel "quantize doubles" size) (nf quantizeWeight size),
    bench (caseLabel "canonical finite numbers" size) (nf canonicalNumberWeight size),
    bench (caseLabel "hackage: scientific fromFloatDigits" size) (nf hackageScientificFromFloatDigitsWeight size),
    bench (caseLabel "scalar class operations" size) (nf scalarClassWeight size),
    bench (caseLabel "approximate equality" size) (nf approximateEqualityWeight size),
    bench (caseLabel "structural exact token construction" size) (nf exactTokenWeight size),
    bench (caseLabel "hackage: bytestring Builder counted ints" size) (nf hackageBuilderCountedIntsWeight size)
  ]

canonicalizeWeight :: Int -> Int
canonicalizeWeight size =
  eitherFoldWeight round (traverse canonicalize (benchDoubles size))

quantizeWeight :: Int -> Int
quantizeWeight size =
  eitherFoldWeight fromIntegral (traverse (quantizeForHash benchPrecision) (benchDoubles size))

canonicalNumberWeight :: Int -> Int
canonicalNumberWeight size =
  eitherValueWeight id (fmap canonicalNumberDigest (traverse mkCanonicalFiniteNumber (benchDoubles size)))

hackageScientificFromFloatDigitsWeight :: Int -> Integer
hackageScientificFromFloatDigitsWeight =
  foldl' scientificDigest 146959810
    . fmap (Scientific.normalize . Scientific.fromFloatDigits)
    . benchDoubles

scientificDigest :: Integer -> Scientific.Scientific -> Integer
scientificDigest digest scientificValue =
  digest * 16777619
    + Scientific.coefficient scientificValue
    + fromIntegral (Scientific.base10Exponent scientificValue)

scalarClassWeight :: Int -> Double
scalarClassWeight size =
  let values = benchDoubles size
      additiveTotal = foldl' add zero values
      multiplicativeTotal = foldl' mul one (fmap normalizedFactor values)
   in sub additiveTotal multiplicativeTotal

approximateEqualityWeight :: Int -> Int
approximateEqualityWeight size =
  case benchAbsTol of
    Left err -> showLength (show err)
    Right tolerance ->
      length
        [ ()
        | value <- benchDoubles size,
          approxEq tolerance value (value + 0.000001)
        ]

exactTokenWeight :: Int -> Int
exactTokenWeight =
  eitherValueWeight id . exactTokenLength . exactTokenFromEncoding . exactTokenEncoding

hackageBuilderCountedIntsWeight :: Int -> Int
hackageBuilderCountedIntsWeight =
  fromIntegral
    . LazyByteString.length
    . Builder.toLazyByteString
    . countedIntsBuilder

countedIntsBuilder :: Int -> Builder.Builder
countedIntsBuilder size =
  Builder.int64BE (fromIntegral size)
    <> foldMap (Builder.int64BE . fromIntegral) (keys size)

exactTokenEncoding :: Int -> ExactEncoding
exactTokenEncoding size =
  exactSequenceMapEncoding (exactAtomEncoding . ExactInt) (keys size)

canonicalNumberDigest :: [CanonicalNumber] -> Int
canonicalNumberDigest =
  foldl'
    ( \digest number ->
        digest * 16777619 + maybe 0 round (canonicalNumberToMaybeDouble number)
    )
    146959810

benchDoubles :: Int -> [Double]
benchDoubles size =
  fmap (\key -> fromIntegral key / 10.0) (keys size)

normalizedFactor :: Double -> Double
normalizedFactor value =
  1.0 + value / 1000000.0

benchPrecision :: Word32
benchPrecision = 4

benchAbsTol :: Either MoonlightError AbsTol
benchAbsTol =
  absTol 0.001

eitherFoldWeight :: (Foldable values, Show err) => (value -> Int) -> Either err (values value) -> Int
eitherFoldWeight project eitherValue =
  case eitherValue of
    Left err -> showLength (show err)
    Right values -> foldl' (\total value -> total + project value) 0 values

eitherValueWeight :: Show err => (value -> Int) -> Either err value -> Int
eitherValueWeight project eitherValue =
  case eitherValue of
    Left err -> showLength (show err)
    Right value -> project value
