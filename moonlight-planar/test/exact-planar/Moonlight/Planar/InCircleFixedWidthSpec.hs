{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | The fixed-width in-circle worker against the arbitrary-precision
-- determinant it stands in for. Agreement on every case is the law; the
-- count of cases the worker resolved by itself is the receipt that the law
-- was observed on the worker and not on the fallback it declines into.
module Moonlight.Planar.InCircleFixedWidthSpec (tests) where

import Control.Monad (unless)
import Data.Bits (finiteBitSize, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.List as List
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import Moonlight.Planar.Internal.Dyadic
  ( exactInCircleDet
  , exactInCircleSignFixedWidth
  )

data Quad = Quad !Double !Double !Double !Double !Double !Double !Double !Double
  deriving stock (Show)

-- | What the worker owes a family on a 64-bit word: every case, because the
-- family's exponents always align within the width, or at least one refusal,
-- because the family exists to keep the fallback on the record.
data Expectation = ResolvesEvery | DeclinesSome

data Family = Family
  { familyName :: String
  , familyQuads :: [Quad]
  , familyExpectation :: Expectation
  , familyCocircular :: Bool
  }

data Tally = Tally
  { tallyCases :: !Int
  , tallyResolved :: !Int
  , tallyDeclined :: !Int
  , tallyDisagreement :: Maybe Quad
  , tallyNotCocircular :: Maybe Quad
  }

tests :: IO ()
tests = do
  tallies <- traverse observe families
  let total = sum (map tallyCases tallies)
  unless (total >= 10_000) $
    fail ("in-circle fixed-width law observed on too few cases: " <> show total)

observe :: Family -> IO Tally
observe family = do
  let tally = List.foldl' (record (familyCocircular family)) emptyTally (familyQuads family)
  putStrLn
    ( "in-circle fixed width " <> familyName family
        <> ": cases=" <> show (tallyCases tally)
        <> " resolved=" <> show (tallyResolved tally)
        <> " declined=" <> show (tallyDeclined tally)
    )
  case tallyNotCocircular tally of
    Just quad -> fail (familyName family <> ": fixture is not exactly cocircular: " <> show quad)
    Nothing -> pure ()
  case tallyDisagreement tally of
    Just quad ->
      fail
        ( familyName family <> ": fixed-width sign disagrees with the determinant on "
            <> show quad <> " (determinant " <> show (determinant quad) <> ")"
        )
    Nothing -> pure ()
  case familyExpectation family of
    ResolvesEvery
      | wideWord && tallyDeclined tally /= 0 ->
          fail (familyName family <> ": the worker declined " <> show (tallyDeclined tally) <> " cases it should resolve")
      | not wideWord && tallyResolved tally /= 0 ->
          fail (familyName family <> ": a narrow word resolved cases in fixed width")
    DeclinesSome
      | tallyDeclined tally == 0 ->
          fail (familyName family <> ": the fallback was never taken")
    _ -> pure ()
  pure tally
 where
  wideWord = finiteBitSize (0 :: Word) == 64

emptyTally :: Tally
emptyTally = Tally 0 0 0 Nothing Nothing

record :: Bool -> Tally -> Quad -> Tally
record cocircular tally quad =
  let !exact = determinant quad
      !expected = compare exact 0
      counted = tally{tallyCases = tallyCases tally + 1}
      checked
        | cocircular && exact /= 0 =
            counted{tallyNotCocircular = maybe (Just quad) Just (tallyNotCocircular counted)}
        | otherwise = counted
   in case fixedWidth quad of
        Nothing -> checked{tallyDeclined = tallyDeclined checked + 1}
        Just verdict
          | verdict == expected -> checked{tallyResolved = tallyResolved checked + 1}
          | otherwise ->
              checked
                { tallyResolved = tallyResolved checked + 1
                , tallyDisagreement = maybe (Just quad) Just (tallyDisagreement checked)
                }

determinant :: Quad -> Integer
determinant (Quad ax ay bx by cx cy dx dy) = exactInCircleDet ax ay bx by cx cy dx dy

fixedWidth :: Quad -> Maybe Ordering
fixedWidth (Quad ax ay bx by cx cy dx dy) = exactInCircleSignFixedWidth ax ay bx by cx cy dx dy

families :: [Family]
families =
  [ Family "uniform" (take 3_000 (uniformQuads (stream 0x1ed1_b2ad_5c0f_9e31))) ResolvesEvery False
  , Family "wide exponents" (take 2_000 (wideQuads (stream 0x7a5f_3c21_9b8e_d604))) DeclinesSome False
  , Family "subnormal floor" (take 1_000 (subnormalQuads (stream 0x3f8c_2d17_a9e4_b055))) ResolvesEvery False
  , Family "grid cocircular" (take 1_500 (gridCocircularQuads (stream 0x9c4e_71b3_5d2a_f086))) ResolvesEvery True
  , Family "grid one unit off" (take 1_000 (gridOffQuads (stream 0x2b7d_e8a1_4f63_c917))) ResolvesEvery False
  , Family "integer circle cocircular" (take 1_500 (circleCocircularQuads (stream 0x6e2a_9d5c_1b74_38f0))) ResolvesEvery True
  , Family "integer circle one unit off" (take 1_000 (circleOffQuads (stream 0xd41b_7c8e_2a95_6f13))) ResolvesEvery False
  , Family "hex lattice hexagon" (take 1_500 (hexHexagonQuads (stream 0x58f3_a6d2_c9e1_74b8))) ResolvesEvery False
  , Family "hex lattice random" (take 500 (hexRandomQuads (stream 0xa7c4_2e91_6d3b_f508))) ResolvesEvery False
  , Family "one ulp off cocircular" (take 1_500 (ulpOffQuads (stream 0x14e9_b7d6_3a2c_85f1))) ResolvesEvery False
  , Family "degenerate" (take 500 (degenerateQuads (stream 0xc3a8_5f1d_7e26_49b0))) ResolvesEvery False
  ]

-- SplitMix64: one deterministic word stream per family.
stream :: Word64 -> [Word64]
stream seed =
  let !state = seed + 0x9E37_79B9_7F4A_7C15
      !z1 = (state `xor` (state `shiftR` 30)) * 0xBF58_476D_1CE4_E5B9
      !z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D0_49BB_1331_11EB
   in (z2 `xor` (z2 `shiftR` 31)) : stream state

chunks :: Int -> [a] -> [[a]]
chunks width values =
  case splitAt width values of
    (chunk, rest) | length chunk == width -> chunk : chunks width rest
    _ -> []

quadOf :: [Double] -> Quad
quadOf [ax, ay, bx, by, cx, cy, dx, dy] = Quad ax ay bx by cx cy dx dy
quadOf other = error ("quadOf: " <> show (length other) <> " coordinates")

unit :: Word64 -> Double
unit word = fromIntegral (word `shiftR` 11) / 9_007_199_254_740_992

signed :: Word64 -> Double
signed word = 2 * unit word - 1

modulo :: Word64 -> Int -> Int
modulo word bound = fromIntegral (word `mod` fromIntegral bound)

integerIn :: Int -> Word64 -> Double
integerIn bound word = fromIntegral (modulo word (2 * bound + 1) - bound)

uniformQuads :: [Word64] -> [Quad]
uniformQuads = map (quadOf . map signed) . chunks 8

-- Any finite bit pattern: sign, exponent field in [0, 2046], random mantissa.
wideQuads :: [Word64] -> [Quad]
wideQuads = map (quadOf . map (fromBits 2047)) . chunks 8

-- Exponent fields in [0, 74]: subnormals and the normals within 73 bits of
-- their floor, so every alignment shift fits the width.
subnormalQuads :: [Word64] -> [Quad]
subnormalQuads = map (quadOf . map (fromBits 75)) . chunks 8

fromBits :: Int -> Word64 -> Double
fromBits exponentBound word =
  let signBit = (word `shiftR` 63) `shiftL` 63
      exponentField = fromIntegral (modulo (word `shiftR` 52) exponentBound) `shiftL` 52
      mantissa = word .&. 0x000F_FFFF_FFFF_FFFF
   in castWord64ToDouble (signBit .|. exponentField .|. mantissa)

-- The four corners of a rectangle centred on an integer point: all on one
-- circle, in a random order.
gridCocircularQuads :: [Word64] -> [Quad]
gridCocircularQuads = map corners . chunks 5
 where
  corners [cxWord, cyWord, aWord, bWord, orderWord] =
    let cx = integerIn 1_048_576 cxWord
        cy = integerIn 1_048_576 cyWord
        a = fromIntegral (1 + modulo aWord 1_048_576)
        b = fromIntegral (1 + modulo bWord 1_048_576)
     in permuted orderWord [(cx + a, cy + b), (cx - a, cy + b), (cx - a, cy - b), (cx + a, cy - b)]
  corners _ = error "gridCocircularQuads"

gridOffQuads :: [Word64] -> [Quad]
gridOffQuads source =
  zipWith nudge (gridCocircularQuads source) (stream 0x71c5_2d9e_b384_a6f0)

permuted :: Word64 -> [(Double, Double)] -> Quad
permuted word points =
  quadOf (concatMap (\(x, y) -> [x, y]) (List.permutations points !! modulo word 24))

-- Move one coordinate of the fourth point by one unit; on integer fixtures
-- that is an exact step off the circle.
nudge :: Quad -> Word64 -> Quad
nudge (Quad ax ay bx by cx cy dx dy) word =
  case modulo word 4 of
    0 -> Quad ax ay bx by cx cy (dx + 1) dy
    1 -> Quad ax ay bx by cx cy (dx - 1) dy
    2 -> Quad ax ay bx by cx cy dx (dy + 1)
    _ -> Quad ax ay bx by cx cy dx (dy - 1)

-- Every lattice point on the circle of radius 5525 = 5^2 * 13 * 17, a radius
-- chosen for its many representations as a sum of two squares.
circlePoints :: [(Integer, Integer)]
circlePoints =
  List.nub
    [ (sx * x, sy * y)
    | x <- [0 .. radius]
    , let remaining = radius * radius - x * x
          y = integerSquareRoot remaining
    , y * y == remaining
    , sx <- [1, -1]
    , sy <- [1, -1]
    ]
 where
  radius = 5525 :: Integer

integerSquareRoot :: Integer -> Integer
integerSquareRoot value =
  let guess = floor (sqrt (fromIntegral value :: Double))
   in List.foldl' (\best candidate -> if candidate * candidate <= value then max best candidate else best) 0 [guess - 1, guess, guess + 1]

circleCocircularQuads :: [Word64] -> [Quad]
circleCocircularQuads = map pick . filter distinct . chunks 7
 where
  count = length circlePoints
  distinct [a, b, c, d, _, _, _] =
    let indices = map (`modulo` count) [a, b, c, d]
     in length (List.nub indices) == 4
  distinct _ = False
  pick [a, b, c, d, txWord, tyWord, orderWord] =
    let tx = integerIn 1_073_741_824 txWord
        ty = integerIn 1_073_741_824 tyWord
        point index =
          let (x, y) = circlePoints !! modulo index count
           in (fromIntegral x + tx, fromIntegral y + ty)
     in permuted orderWord (map point [a, b, c, d])
  pick _ = error "circleCocircularQuads"

circleOffQuads :: [Word64] -> [Quad]
circleOffQuads source =
  zipWith nudge (circleCocircularQuads source) (stream 0x5d2e_8a47_c1f9_63b0)

-- The hexagonal lattice as binary64: x exact, y a rounded multiple of
-- sqrt 3 / 2. No four of its points are exactly cocircular in binary64, so
-- these are the near-cocircular quadrilaterals a Delaunay flip meets.
hexPoint :: Int -> Int -> (Double, Double)
hexPoint i j = (fromIntegral i + fromIntegral j / 2, fromIntegral j * (sqrt 3 / 2))

-- Four of the six lattice points around a lattice point: the corners of a
-- lattice hexagon, cocircular up to the rounding of the ordinate.
hexHexagonQuads :: [Word64] -> [Quad]
hexHexagonQuads = map hexagon . chunks 4
 where
  hexagon [iWord, jWord, skipWord, orderWord] =
    let i = modulo iWord 101 - 50
        j = modulo jWord 101 - 50
        ring = [(i + 1, j), (i, j + 1), (i - 1, j + 1), (i - 1, j), (i, j - 1), (i + 1, j - 1)]
        skipped = modulo skipWord 6
        kept = [corner | (position, corner) <- zip [0 :: Int ..] ring, position /= skipped, position /= (skipped + 3) `mod` 6]
     in permuted orderWord (map (uncurry hexPoint) kept)
  hexagon _ = error "hexHexagonQuads"

hexRandomQuads :: [Word64] -> [Quad]
hexRandomQuads = map (quadOf . concatMap coordinate . pairs) . chunks 8
 where
  pairs :: [Word64] -> [(Word64, Word64)]
  pairs (i : j : rest) = (i, j) : pairs rest
  pairs _ = []
  coordinate :: (Word64, Word64) -> [Double]
  coordinate (iWord, jWord) =
    let (x, y) = hexPoint (modulo iWord 101 - 50) (modulo jWord 101 - 50)
     in [x, y]

-- A cocircular grid quadrilateral with one nonzero coordinate moved by a
-- single ulp: the determinant is tiny against the magnitudes it is built
-- from, which is where a sign is hardest to resolve.
ulpOffQuads :: [Word64] -> [Quad]
ulpOffQuads source =
  zipWith ulpNudge (gridCocircularQuads source) (stream 0x8b3f_6e1a_d47c_2905)

ulpNudge :: Quad -> Word64 -> Quad
ulpNudge quad@(Quad ax ay bx by cx cy dx dy) word =
  let coordinates = [ax, ay, bx, by, cx, cy, dx, dy]
      start = modulo word 8
      candidates = [index | offset <- [0 .. 7], let index = (start + offset) `mod` 8, coordinates !! index /= 0]
      step value
        | even (word `shiftR` 3) = castWord64ToDouble (castDoubleToWord64 value + 1)
        | otherwise = castWord64ToDouble (castDoubleToWord64 value - 1)
   in case candidates of
        chosen : _ ->
          quadOf [if index == chosen then step value else value | (index, value) <- zip [0 :: Int ..] coordinates]
        [] -> quad

-- Repeated points, collinear triples, zeros of both signs, and mixtures of
-- those with ordinary coordinates.
degenerateQuads :: [Word64] -> [Quad]
degenerateQuads = map shape . chunks 9
 where
  shape (selector : rest) =
    case quadOf (map signed rest) of
      Quad ax ay bx by cx cy dx dy ->
        case modulo selector 6 of
          0 -> Quad ax ay ax ay cx cy dx dy
          1 -> Quad ax ay bx by cx cy ax ay
          2 -> Quad ax ay (ax + 1) (ay + 1) (ax + 2) (ay + 2) dx dy
          3 -> Quad 0 0 0 negativeZero negativeZero 0 dx dy
          4 -> Quad ax 0 bx 0 cx 0 dx 0
          _ -> Quad ax ay bx by cx cy negativeZero negativeZero
  shape _ = error "degenerateQuads"
  negativeZero = -0.0 :: Double
