{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Literal
  ( renderNormalizedLit,
    renderNormalizedOverLit,
    renderExactIntegral,
    renderExactFractional,
    renderExactRational,
    finiteDecimal,
    factorMultiplicity,
    renderScaledDecimal,
    renderExponent,
    renderPrimitiveByte
  )
where

import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.Ratio (denominator, numerator)
import Data.Word (Word8)
import GHC.Types.SourceText (FractionalExponentBase (..))
import Numeric (showHex)
import Moonlight.Pale.Ghc.Expr.Syntax
renderNormalizedLit :: NormalizedLit -> String
renderNormalizedLit = \case
  NormalizedChar value -> show value
  NormalizedCharPrim value -> show value <> "#"
  NormalizedString value -> show value
  NormalizedMultilineString value -> show value
  NormalizedStringPrim value -> "\"" <> foldMap renderPrimitiveByte (ByteString.unpack value) <> "\"#"
  NormalizedInt value -> renderExactIntegral "" value
  NormalizedIntPrim value -> renderExactIntegral "#" value
  NormalizedWordPrim value -> renderExactIntegral "##" value
  NormalizedInt8Prim value -> renderExactIntegral "#Int8" value
  NormalizedInt16Prim value -> renderExactIntegral "#Int16" value
  NormalizedInt32Prim value -> renderExactIntegral "#Int32" value
  NormalizedInt64Prim value -> renderExactIntegral "#Int64" value
  NormalizedWord8Prim value -> renderExactIntegral "#Word8" value
  NormalizedWord16Prim value -> renderExactIntegral "#Word16" value
  NormalizedWord32Prim value -> renderExactIntegral "#Word32" value
  NormalizedWord64Prim value -> renderExactIntegral "#Word64" value
  NormalizedFloatPrim value -> renderExactFractional "#" value
  NormalizedDoublePrim value -> renderExactFractional "##" value

renderNormalizedOverLit :: NormalizedOverLit -> String
renderNormalizedOverLit = \case
  NormalizedIntegralOverLit value -> renderExactIntegral "" value
  NormalizedFractionalOverLit value -> renderExactFractional "" value
  NormalizedStringOverLit value -> show value

renderExactIntegral :: String -> ExactIntegral -> String
renderExactIntegral suffix exactValue =
  maybe
    ( [ '-' | exactIntegralNegative exactValue ]
        <> show (exactIntegralValue exactValue)
        <> suffix
    )
    id
    (exactIntegralSource exactValue)

renderExactFractional :: String -> ExactFractional -> String
renderExactFractional suffix exactValue =
  maybe
    ( [ '-' | exactFractionalNegative exactValue ]
        <> renderExactRational (exactFractionalSignificand exactValue)
        <> renderExponent (exactFractionalBase exactValue) (exactFractionalExponent exactValue)
        <> suffix
    )
    id
    (exactFractionalSource exactValue)

renderExactRational :: Rational -> String
renderExactRational rationalValue =
  maybe
    ("(" <> show (numerator rationalValue) <> " / " <> show (denominator rationalValue) <> ")")
    id
    (finiteDecimal rationalValue)

finiteDecimal :: Rational -> Maybe String
finiteDecimal rationalValue =
  let denominatorValue = denominator rationalValue
      (twoCount, afterTwos) = factorMultiplicity 2 denominatorValue
      (fiveCount, residualDenominator) = factorMultiplicity 5 afterTwos
      decimalPlaces = max twoCount fiveCount
      scaledNumerator =
        numerator rationalValue
          * (2 ^ (decimalPlaces - twoCount))
          * (5 ^ (decimalPlaces - fiveCount))
   in if residualDenominator /= 1
        then Nothing
        else Just (renderScaledDecimal decimalPlaces scaledNumerator)

factorMultiplicity :: Integer -> Integer -> (Int, Integer)
factorMultiplicity factorValue value
  | value `mod` factorValue == 0 =
      let (remainingCount, residualValue) =
            factorMultiplicity factorValue (value `div` factorValue)
       in (remainingCount + 1, residualValue)
  | otherwise =
      (0, value)

renderScaledDecimal :: Int -> Integer -> String
renderScaledDecimal decimalPlaces scaledNumerator
  | decimalPlaces == 0 =
      show scaledNumerator <> ".0"
  | otherwise =
      let signPrefix = ['-' | scaledNumerator < 0]
          unsignedDigits = show (abs scaledNumerator)
          paddedDigits =
            replicate (max 0 (decimalPlaces + 1 - length unsignedDigits)) '0'
              <> unsignedDigits
          splitIndex = length paddedDigits - decimalPlaces
          (wholeDigits, fractionalDigits) = splitAt splitIndex paddedDigits
       in signPrefix <> wholeDigits <> "." <> fractionalDigits

renderExponent :: FractionalExponentBase -> Integer -> String
renderExponent exponentBase exponentValue =
  case (exponentBase, exponentValue) of
    (_, 0) -> ""
    (Base10, _) -> "e" <> show exponentValue
    (Base2, _) -> "p" <> show exponentValue

renderPrimitiveByte :: Word8 -> String
renderPrimitiveByte byteValue =
  case Char.chr (fromIntegral byteValue) of
    '"' -> "\\\""
    '\\' -> "\\\\"
    characterValue
      | byteValue >= 32 && byteValue <= 126 ->
          [characterValue]
      | otherwise ->
          "\\x" <> showHex byteValue "" <> "\\&"
