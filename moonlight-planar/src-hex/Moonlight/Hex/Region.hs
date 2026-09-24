{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bounded axial layouts and native packed finite-set algebra.
module Moonlight.Hex.Region
  ( HexLayout
  , HexLayoutObstruction (..)
  , hexLayout
  , hexLayoutOrigin
  , hexLayoutWidth
  , hexLayoutHeight
  , hexLayoutCellCount
  , hexLayoutWordCount
  , hexLayoutContains
  , hexLayoutCoordAt
  , HexRegion
  , HexRegionObstruction (..)
  , HexPackedRegionObstruction (..)
  , emptyHexRegion
  , fullHexRegion
  , singletonHexRegion
  , hexRegionFromCoords
  , hexRegionFromPackedWords
  , hexRegionGenerate
  , hexRegionGenerateM
  , HexRowSpan
  , hexRowSpan
  , hexRegionGenerateRowSpans
  , hexRegionLayout
  , hexRegionMember
  , hexRegionCardinality
  , foldHexRegionPackedWords
  , hexRegionCoords
  , foldHexRegionCoords
  , hexRegionUnion
  , hexRegionIntersection
  , hexRegionDifference
  , hexRegionSymmetricDifference
  , complementHexRegion
  , hexRegionSubsetOf
  , restrictHexRegion
  , reframeHexRegion
  , HexGluingObstruction (..)
  , glueCompatibleHexRegions
  , hexNeighbourCoord
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), (.|.), complement, countTrailingZeros, popCount, shiftL, xor)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word64)
import GHC.Generics (Generic)
import Moonlight.Hex.Coordinate
  ( HexCoord (..)
  , HexDirection
  , hexStepCoord
  )
import Moonlight.Hex.Region.Internal
  ( HexLayout (..)
  , HexRegion (..)
  , extractBits
  , finalWordMask
  , firstSameLayoutDifferenceIndex
  , generateHexRegionIndexed
  , generateHexRegionIndexedM
  , hexCoordAtIndex
  , hexCoordIndex
  , hexRegionMemberAtIndex
  , lowBitMask
  , setBitAt
  )

data HexLayoutObstruction
  = HexLayoutWidthNotPositive !Int
  | HexLayoutHeightNotPositive !Int
  | HexLayoutCoordinateRangeOverflow !HexCoord !Int !Int
  | HexLayoutCellCountOverflow !Integer
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

hexLayout :: HexCoord -> Int -> Int -> Either HexLayoutObstruction HexLayout
hexLayout origin width height
  | width <= 0 = Left (HexLayoutWidthNotPositive width)
  | height <= 0 = Left (HexLayoutHeightNotPositive height)
  | not (coordinateRangeFits origin width height) =
      Left (HexLayoutCoordinateRangeOverflow origin width height)
  | cells > toInteger (maxBound :: Int) = Left (HexLayoutCellCountOverflow cells)
  | otherwise =
      let cellCount = fromInteger cells
       in Right
            HexLayout
              { layoutOrigin = origin
              , layoutWidth = width
              , layoutHeight = height
              , layoutCellCount = cellCount
              , layoutWordCount =
                  cellCount `quot` 64
                    + if cellCount `rem` 64 == 0 then 0 else 1
              }
 where
  cells = toInteger width * toInteger height

hexLayoutOrigin :: HexLayout -> HexCoord
hexLayoutOrigin = layoutOrigin
{-# INLINE hexLayoutOrigin #-}

hexLayoutWidth :: HexLayout -> Int
hexLayoutWidth = layoutWidth
{-# INLINE hexLayoutWidth #-}

hexLayoutHeight :: HexLayout -> Int
hexLayoutHeight = layoutHeight
{-# INLINE hexLayoutHeight #-}

hexLayoutCellCount :: HexLayout -> Int
hexLayoutCellCount = layoutCellCount
{-# INLINE hexLayoutCellCount #-}

hexLayoutWordCount :: HexLayout -> Int
hexLayoutWordCount = layoutWordCount
{-# INLINE hexLayoutWordCount #-}

hexLayoutContains :: HexLayout -> HexCoord -> Bool
hexLayoutContains layout coordinate = case hexCoordIndex layout coordinate of
  Nothing -> False
  Just _ -> True
{-# INLINE hexLayoutContains #-}

hexLayoutCoordAt :: HexLayout -> Int -> Maybe HexCoord
hexLayoutCoordAt layout index
  | index < 0 || index >= layoutCellCount layout = Nothing
  | otherwise = Just (hexCoordAtIndex layout index)
{-# INLINE hexLayoutCoordAt #-}

data HexRegionObstruction
  = HexRegionCoordinateOutsideLayout !HexLayout !HexCoord
  | HexRegionLayoutMismatch !HexLayout !HexLayout
  | HexRegionRestrictionOutsideLayout !HexLayout !HexLayout
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A rejected packed representation. Word count and zero padding are the
-- complete representation invariant; admitted vectors are retained without a
-- copy.
data HexPackedRegionObstruction
  -- | Expected word count, then observed word count.
  = HexPackedRegionWordCountMismatch !Int !Int
  | HexPackedRegionNonCanonicalPadding !Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

emptyHexRegion :: HexLayout -> HexRegion
emptyHexRegion layout = HexRegion layout (U.replicate (layoutWordCount layout) 0)

fullHexRegion :: HexLayout -> HexRegion
fullHexRegion layout =
  HexRegion layout
    ( U.generate
        (layoutWordCount layout)
        (\index ->
            if index + 1 == layoutWordCount layout
              then finalWordMask layout
              else maxBound
        )
    )

singletonHexRegion :: HexLayout -> HexCoord -> Either HexRegionObstruction HexRegion
singletonHexRegion layout coordinate = hexRegionFromCoords layout [coordinate]

hexRegionFromCoords
  :: Foldable collection
  => HexLayout
  -> collection HexCoord
  -> Either HexRegionObstruction HexRegion
hexRegionFromCoords layout coordinates = runST $ do
  mutableWords <- MU.replicate (layoutWordCount layout) 0
  admitted <- runExceptT (traverse_ (insertCoordinate mutableWords) coordinates)
  case admitted of
    Left obstruction -> pure (Left obstruction)
    Right () -> Right . HexRegion layout <$> U.unsafeFreeze mutableWords
 where
  insertCoordinate
    :: MU.MVector s Word64
    -> HexCoord
    -> ExceptT HexRegionObstruction (ST s) ()
  insertCoordinate mutableWords coordinate =
    case hexCoordIndex layout coordinate of
      Nothing -> throwE (HexRegionCoordinateOutsideLayout layout coordinate)
      Just cellIndex ->
        let (wordIndex, bitIndex) = cellIndex `quotRem` 64
         in MU.modify mutableWords (`setBitAt` bitIndex) wordIndex

hexRegionFromPackedWords
  :: HexLayout
  -> U.Vector Word64
  -> Either HexPackedRegionObstruction HexRegion
hexRegionFromPackedWords layout wordsValue
  | actualWordCount /= expectedWordCount =
      Left (HexPackedRegionWordCountMismatch expectedWordCount actualWordCount)
  | nonCanonicalPadding /= 0 =
      Left (HexPackedRegionNonCanonicalPadding nonCanonicalPadding)
  | otherwise = Right (HexRegion layout wordsValue)
 where
  expectedWordCount = layoutWordCount layout
  actualWordCount = U.length wordsValue
  nonCanonicalPadding =
    (wordsValue `U.unsafeIndex` (expectedWordCount - 1))
      .&. complement (finalWordMask layout)

-- | Generate membership directly into packed words. This is the total bulk
-- authoring path when membership is already a function rather than a sparse
-- coordinate collection.
hexRegionGenerate :: HexLayout -> (HexCoord -> Bool) -> HexRegion
hexRegionGenerate layout predicate =
  generateHexRegionIndexed layout (predicate . hexCoordAtIndex layout)
{-# INLINE hexRegionGenerate #-}

-- | A nonempty closed interval of global axial q coordinates. Row ownership
-- comes from 'hexRegionGenerateRowSpans'; arbitrary-size endpoints are clipped
-- to its admitted layout before conversion to resident indices.
data HexRowSpan = HexRowSpan !Integer !Integer
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

hexRowSpan :: Integer -> Integer -> Maybe HexRowSpan
hexRowSpan firstQ finalQ
  | firstQ <= finalQ = Just (HexRowSpan firstQ finalQ)
  | otherwise = Nothing

-- | Generate a packed section from closed q spans at each global axial row.
-- Spans may overlap or extend beyond the layout: their clipped union is the
-- result. Each span contributes whole word masks, never one predicate call
-- per interior cell. The existing layout index and mask owners alone lower
-- coordinates to bits, and clipping establishes canonical zero padding.
hexRegionGenerateRowSpans :: HexLayout -> (Int -> [HexRowSpan]) -> HexRegion
hexRegionGenerateRowSpans layout spansAtRow =
  HexRegion layout
    ( U.accum (.|.) (U.replicate (layoutWordCount layout) 0)
        [ (wordIndex, lowBitMask bitCount `shiftL` firstBit)
        | localRow <- [0 .. layoutHeight layout - 1]
        , let row = originR + localRow
        , HexRowSpan firstQ finalQ <- spansAtRow row
        , let clippedFirst = max minimumQ firstQ
        , let clippedFinal = min maximumQ finalQ
        , clippedFirst <= clippedFinal
        , Just firstIndex <- [hexCoordIndex layout (HexCoord (fromInteger clippedFirst) row)]
        , Just finalIndex <- [hexCoordIndex layout (HexCoord (fromInteger clippedFinal) row)]
        , wordIndex <- [firstIndex `quot` 64 .. finalIndex `quot` 64]
        , let wordBase = wordIndex * 64
        , let firstBit = max 0 (firstIndex - wordBase)
        , let finalBit = min 63 (finalIndex - wordBase)
        , let bitCount = finalBit - firstBit + 1
        ]
    )
 where
  HexCoord originQ originR = layoutOrigin layout
  minimumQ = toInteger originQ
  maximumQ = minimumQ + toInteger (layoutWidth layout) - 1

-- | Generate canonical packed membership while accumulating one caller-owned
-- effect. Predicate refusal is preserved directly; packed admission cannot
-- fail because this constructor owns every emitted bit, including padding.
hexRegionGenerateM
  :: Monad effect
  => HexLayout
  -> (HexCoord -> effect Bool)
  -> effect HexRegion
hexRegionGenerateM layout predicate =
  generateHexRegionIndexedM layout (predicate . hexCoordAtIndex layout)
{-# INLINE hexRegionGenerateM #-}

hexRegionLayout :: HexRegion -> HexLayout
hexRegionLayout (HexRegion layout _) = layout
{-# INLINE hexRegionLayout #-}

hexRegionMember :: HexCoord -> HexRegion -> Bool
hexRegionMember coordinate region@(HexRegion layout _) =
  case hexCoordIndex layout coordinate of
    Nothing -> False
    Just cellIndex -> hexRegionMemberAtIndex region cellIndex
{-# INLINE hexRegionMember #-}

hexRegionCardinality :: HexRegion -> Int
hexRegionCardinality (HexRegion _ wordsValue) =
  U.foldl' (\total word -> total + popCount word) 0 wordsValue

-- | Fold the canonical row-major packed words without exposing a constructor
-- that could admit nonzero padding. This is the zero-copy observation used by
-- binary interpreters and digests.
foldHexRegionPackedWords :: (accumulator -> Word64 -> accumulator) -> accumulator -> HexRegion -> accumulator
foldHexRegionPackedWords step initial (HexRegion _ wordsValue) =
  U.foldl' step initial wordsValue
{-# INLINE foldHexRegionPackedWords #-}

hexRegionCoords :: HexRegion -> Vector HexCoord
hexRegionCoords = Vector.fromList . reverse . foldHexRegionCoords (flip (:)) []

foldHexRegionCoords :: (accumulator -> HexCoord -> accumulator) -> accumulator -> HexRegion -> accumulator
foldHexRegionCoords step initial (HexRegion layout wordsValue) =
  U.ifoldl' (foldHexWord layout step) initial wordsValue

foldHexWord
  :: HexLayout
  -> (accumulator -> HexCoord -> accumulator)
  -> accumulator
  -> Int
  -> Word64
  -> accumulator
foldHexWord layout step accumulator wordIndex =
  foldSetHexBits layout step accumulator (wordIndex * 64)

foldSetHexBits
  :: HexLayout
  -> (accumulator -> HexCoord -> accumulator)
  -> accumulator
  -> Int
  -> Word64
  -> accumulator
foldSetHexBits layout step !accumulator !baseIndex !remaining
  | remaining == 0 = accumulator
  | otherwise =
      let bitIndex = countTrailingZeros remaining
          cellIndex = baseIndex + bitIndex
          next = remaining .&. (remaining - 1)
       in foldSetHexBits
            layout
            step
            (step accumulator (hexCoordAtIndex layout cellIndex))
            baseIndex
            next

hexRegionUnion :: HexRegion -> HexRegion -> Either HexRegionObstruction HexRegion
hexRegionUnion = combineHexRegionsWith (.|.)

hexRegionIntersection :: HexRegion -> HexRegion -> Either HexRegionObstruction HexRegion
hexRegionIntersection = combineHexRegionsWith (.&.)

hexRegionDifference :: HexRegion -> HexRegion -> Either HexRegionObstruction HexRegion
hexRegionDifference = combineHexRegionsWith (\left right -> left .&. complement right)

hexRegionSymmetricDifference :: HexRegion -> HexRegion -> Either HexRegionObstruction HexRegion
hexRegionSymmetricDifference = combineHexRegionsWith xor

complementHexRegion :: HexRegion -> HexRegion
complementHexRegion (HexRegion layout wordsValue) =
  HexRegion layout
    ( U.imap
        (\index word ->
            let inverted = complement word
             in if index + 1 == layoutWordCount layout
                  then inverted .&. finalWordMask layout
                  else inverted
        )
        wordsValue
    )

hexRegionSubsetOf :: HexRegion -> HexRegion -> Either HexRegionObstruction Bool
hexRegionSubsetOf (HexRegion leftLayout leftWords) (HexRegion rightLayout rightWords)
  | leftLayout /= rightLayout = Left (HexRegionLayoutMismatch leftLayout rightLayout)
  | otherwise = Right (isNothing (firstSameLayoutDifferenceIndex left right))
 where
  left = HexRegion leftLayout leftWords
  right = HexRegion rightLayout rightWords

restrictHexRegion :: HexLayout -> HexRegion -> Either HexRegionObstruction HexRegion
restrictHexRegion target source
  | layoutContainsLayout (hexRegionLayout source) target = Right (reframeHexRegion target source)
  | otherwise = Left (HexRegionRestrictionOutsideLayout (hexRegionLayout source) target)

-- | Re-express a section in another global axial window. Cells outside the
-- source context are false. Packed row spans, rather than boxed coordinates,
-- are copied into the result.
reframeHexRegion :: HexLayout -> HexRegion -> HexRegion
reframeHexRegion target source =
  HexRegion target
    (U.generate (layoutWordCount target) (reframedWord target source))
{-# INLINE reframeHexRegion #-}

-- | A failed descent or overlap-compatibility obligation.
data HexGluingObstruction
  = HexGluingExtentOverflow !Integer !Integer
  | HexGluingLayoutInvalid !HexLayoutObstruction
  | HexOverlapDisagreement !HexCoord
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Glue local sections only when their truth values agree on every overlap.
-- This is deliberately distinct from set union. Space is mostly unglued;
-- stars are the rare places where enough local sections agreed to make a
-- global one and lit up from the sheer relief of consistency.
glueCompatibleHexRegions :: NonEmpty HexRegion -> Either HexGluingObstruction HexRegion
glueCompatibleHexRegions regions = do
  target <- boundingLayout regions
  let firstRegion :| remainingRegions = regions
  runST $ do
    mutableValues <-
      U.unsafeThaw
        (U.generate (layoutWordCount target) (reframedWord target firstRegion))
    mutableCoverage <-
      U.unsafeThaw
        ( U.generate
            (layoutWordCount target)
            (reframedCoverageWord target (hexRegionLayout firstRegion))
        )
    admitted <-
      runExceptT
        ( traverse_
            (descendHexSection target mutableValues mutableCoverage)
            remainingRegions
        )
    case admitted of
      Left coordinate -> pure (Left (HexOverlapDisagreement coordinate))
      Right () -> Right . HexRegion target <$> U.unsafeFreeze mutableValues

hexNeighbourCoord :: HexLayout -> HexCoord -> HexDirection -> Maybe HexCoord
hexNeighbourCoord layout coordinate direction =
  hexStepCoord coordinate direction >>= admitNeighbour
 where
  admitNeighbour candidate =
    if hexLayoutContains layout candidate then Just candidate else Nothing
{-# INLINE hexNeighbourCoord #-}

combineHexRegionsWith
  :: (Word64 -> Word64 -> Word64)
  -> HexRegion
  -> HexRegion
  -> Either HexRegionObstruction HexRegion
combineHexRegionsWith combine (HexRegion leftLayout leftWords) (HexRegion rightLayout rightWords)
  | leftLayout /= rightLayout = Left (HexRegionLayoutMismatch leftLayout rightLayout)
  | otherwise = Right (HexRegion leftLayout (U.zipWith combine leftWords rightWords))
{-# INLINE combineHexRegionsWith #-}

descendHexSection
  :: forall s. HexLayout
  -> MU.MVector s Word64
  -> MU.MVector s Word64
  -> HexRegion
  -> ExceptT HexCoord (ST s) ()
descendHexSection target mutableValues mutableCoverage local =
  traverse_
    (\(firstWord, finalWord) -> descendHexWordSpan firstWord finalWord)
    (localTargetWordSpans target (hexRegionLayout local))
 where
  descendHexWordSpan :: Int -> Int -> ExceptT HexCoord (ST s) ()
  descendHexWordSpan !wordIndex !finalWord
    | wordIndex > finalWord = pure ()
    | otherwise = do
        values <- lift (MU.unsafeRead mutableValues wordIndex)
        coverage <- lift (MU.unsafeRead mutableCoverage wordIndex)
        let localValues = reframedWord target local wordIndex
            localCoverage = reframedCoverageWord target (hexRegionLayout local) wordIndex
            disagreement = (values `xor` localValues) .&. coverage .&. localCoverage
        if disagreement == 0
          then do
            lift (MU.unsafeWrite mutableValues wordIndex (values .|. localValues))
            lift (MU.unsafeWrite mutableCoverage wordIndex (coverage .|. localCoverage))
            descendHexWordSpan (wordIndex + 1) finalWord
          else
            throwE
              (hexCoordAtIndex target (wordIndex * 64 + countTrailingZeros disagreement))

localTargetWordSpans :: HexLayout -> HexLayout -> [(Int, Int)]
localTargetWordSpans target local =
  foldr mergeWordSpan [] (fmap rowWordSpan [0 .. layoutHeight local - 1])
 where
  HexCoord targetQ targetR = layoutOrigin target
  HexCoord localQ localR = layoutOrigin local
  targetColumn = localQ - targetQ
  firstTargetRow = localR - targetR

  rowWordSpan :: Int -> (Int, Int)
  rowWordSpan localRow =
    let firstCell =
          (firstTargetRow + localRow) * layoutWidth target + targetColumn
        finalCell = firstCell + layoutWidth local - 1
     in (firstCell `quot` 64, finalCell `quot` 64)

  mergeWordSpan :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
  mergeWordSpan wordSpan [] = [wordSpan]
  mergeWordSpan wordSpan@(firstWord, finalWord) ((nextFirstWord, nextFinalWord) : remaining)
    | nextFirstWord <= finalWord + 1 =
        (firstWord, max finalWord nextFinalWord) : remaining
    | otherwise = wordSpan : (nextFirstWord, nextFinalWord) : remaining

boundingLayout :: NonEmpty HexRegion -> Either HexGluingObstruction HexLayout
boundingLayout regions =
  let layouts = fmap hexRegionLayout regions
      minimumQ = minimum (fmap (toInteger . hexQ . layoutOrigin) layouts)
      minimumR = minimum (fmap (toInteger . hexR . layoutOrigin) layouts)
      maximumQ = maximum (fmap layoutMaximumQ layouts)
      maximumR = maximum (fmap layoutMaximumR layouts)
      width = maximumQ - minimumQ + 1
      height = maximumR - minimumR + 1
   in case (integerToInt minimumQ, integerToInt minimumR, integerToInt width, integerToInt height) of
        (Just originQ, Just originR, Just widthValue, Just heightValue) ->
          either (Left . HexGluingLayoutInvalid) Right
            (hexLayout (HexCoord originQ originR) widthValue heightValue)
        _ -> Left (HexGluingExtentOverflow width height)

reframedWord :: HexLayout -> HexRegion -> Int -> Word64
reframedWord target (HexRegion source sourceWords) wordIndex =
  reframedSourceWord target source (extractBits sourceWords) wordIndex
{-# INLINE reframedWord #-}

reframedCoverageWord :: HexLayout -> HexLayout -> Int -> Word64
reframedCoverageWord target source =
  reframedSourceWord target source (const lowBitMask)
{-# INLINE reframedCoverageWord #-}

reframedSourceWord
  :: HexLayout
  -> HexLayout
  -> (Int -> Int -> Word64)
  -> Int
  -> Word64
reframedSourceWord target source extract wordIndex =
  gather 0 (wordIndex * 64) 0
 where
  gather !destinationShift !targetIndex !accumulator
    | destinationShift >= 64 || targetIndex >= layoutCellCount target = accumulator
    | otherwise =
        let (targetRow, targetColumn) = targetIndex `quotRem` layoutWidth target
            rowRun = min (64 - destinationShift) (layoutWidth target - targetColumn)
            targetQ0 = hexQ (layoutOrigin target) + targetColumn
            targetR = hexR (layoutOrigin target) + targetRow
            sourceQ0 = hexQ (layoutOrigin source)
            sourceQMaximum = sourceQ0 + layoutWidth source - 1
            sourceR0 = hexR (layoutOrigin source)
            sourceRMaximum = sourceR0 + layoutHeight source - 1
            runQMaximum = targetQ0 + rowRun - 1
            overlapQ0 = max targetQ0 sourceQ0
            overlapQMaximum = min runQMaximum sourceQMaximum
            hasRow = targetR >= sourceR0 && targetR <= sourceRMaximum
            hasColumns = overlapQ0 <= overlapQMaximum
            copied =
              if hasRow && hasColumns
                then
                  let leading = overlapQ0 - targetQ0
                      copiedCount = overlapQMaximum - overlapQ0 + 1
                      sourceRow = targetR - sourceR0
                      sourceColumn = overlapQ0 - sourceQ0
                      sourceBitIndex = sourceRow * layoutWidth source + sourceColumn
                   in extract sourceBitIndex copiedCount
                        `shiftL` (destinationShift + leading)
                else 0
         in gather
              (destinationShift + rowRun)
              (targetIndex + rowRun)
              (accumulator .|. copied)
{-# INLINE reframedSourceWord #-}

coordinateRangeFits :: HexCoord -> Int -> Int -> Bool
coordinateRangeFits (HexCoord originQ originR) width height =
  let maximumQ = toInteger originQ + toInteger width - 1
      maximumR = toInteger originR + toInteger height - 1
      lower = toInteger (minBound :: Int)
      upper = toInteger (maxBound :: Int)
   in maximumQ >= lower && maximumQ <= upper && maximumR >= lower && maximumR <= upper

layoutContainsLayout :: HexLayout -> HexLayout -> Bool
layoutContainsLayout outer inner =
  let HexCoord outerQ outerR = layoutOrigin outer
      HexCoord innerQ innerR = layoutOrigin inner
   in innerQ >= outerQ
        && innerR >= outerR
        && layoutMaximumQ inner <= layoutMaximumQ outer
        && layoutMaximumR inner <= layoutMaximumR outer

layoutMaximumQ :: HexLayout -> Integer
layoutMaximumQ layout =
  toInteger (hexQ (layoutOrigin layout)) + toInteger (layoutWidth layout) - 1

layoutMaximumR :: HexLayout -> Integer
layoutMaximumR layout =
  toInteger (hexR (layoutOrigin layout)) + toInteger (layoutHeight layout) - 1

integerToInt :: Integer -> Maybe Int
integerToInt value
  | value < toInteger (minBound :: Int) = Nothing
  | value > toInteger (maxBound :: Int) = Nothing
  | otherwise = Just (fromInteger value)
