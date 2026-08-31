{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

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
  , emptyHexRegion
  , fullHexRegion
  , singletonHexRegion
  , hexRegionFromCoords
  , hexRegionGenerate
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
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), (.|.), complement, countTrailingZeros, popCount, shiftL, shiftR, xor)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word64)
import GHC.Generics (Generic)
import Moonlight.Hex.Coordinate (HexCoord (..), HexDirection, hexStepCoord)

-- | A nonempty axial parallelogram with an overflow-safe dense row-major index.
data HexLayout = HexLayout
  { layoutOrigin :: !HexCoord
  , layoutWidth :: !Int
  , layoutHeight :: !Int
  , layoutCellCount :: !Int
  , layoutWordCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

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

-- | Canonical packed membership: one bit per layout cell and zero padding.
data HexRegion = HexRegion !HexLayout !(U.Vector Word64)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data HexRegionObstruction
  = HexRegionCoordinateOutsideLayout !HexLayout !HexCoord
  | HexRegionLayoutMismatch !HexLayout !HexLayout
  | HexRegionRestrictionOutsideLayout !HexLayout !HexLayout
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

-- | Generate membership directly into packed words. This is the total bulk
-- authoring path when membership is already a function rather than a sparse
-- coordinate collection.
hexRegionGenerate :: HexLayout -> (HexCoord -> Bool) -> HexRegion
hexRegionGenerate layout predicate =
  HexRegion layout (U.generate (layoutWordCount layout) generateWord)
 where
  generateWord wordIndex =
    let baseIndex = wordIndex * 64
        bitCount = min 64 (layoutCellCount layout - baseIndex)
     in gatherBits baseIndex bitCount 0 0

  gatherBits !baseIndex !bitCount !bitIndex !word
    | bitIndex >= bitCount = word
    | predicate (hexCoordAtIndex layout (baseIndex + bitIndex)) =
        gatherBits baseIndex bitCount (bitIndex + 1) (setBitAt word bitIndex)
    | otherwise = gatherBits baseIndex bitCount (bitIndex + 1) word
{-# INLINE hexRegionGenerate #-}

hexRegionLayout :: HexRegion -> HexLayout
hexRegionLayout (HexRegion layout _) = layout
{-# INLINE hexRegionLayout #-}

hexRegionMember :: HexCoord -> HexRegion -> Bool
hexRegionMember coordinate (HexRegion layout wordsValue) =
  case hexCoordIndex layout coordinate of
    Nothing -> False
    Just cellIndex ->
      let (wordIndex, bitIndex) = cellIndex `quotRem` 64
       in wordsValue `U.unsafeIndex` wordIndex .&. (1 `shiftL` bitIndex) /= 0
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
  | otherwise =
      Right
        ( U.ifoldl'
            (\isSubset index leftWord ->
                isSubset
                  && leftWord .&. complement (rightWords `U.unsafeIndex` index) == 0
            )
            True
            leftWords
        )

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
-- This is deliberately distinct from set union.
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
    obstruction <-
      descendHexSections
        target
        mutableValues
        mutableCoverage
        remainingRegions
    case obstruction of
      Just coordinate -> pure (Left (HexOverlapDisagreement coordinate))
      Nothing -> Right . HexRegion target <$> U.unsafeFreeze mutableValues

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

descendHexSections
  :: HexLayout
  -> MU.MVector s Word64
  -> MU.MVector s Word64
  -> [HexRegion]
  -> ST s (Maybe HexCoord)
descendHexSections _ _ _ [] = pure Nothing
descendHexSections target mutableValues mutableCoverage (local : remaining) = do
  obstruction <- descendHexSection target mutableValues mutableCoverage local 0
  case obstruction of
    Just coordinate -> pure (Just coordinate)
    Nothing -> descendHexSections target mutableValues mutableCoverage remaining

descendHexSection
  :: HexLayout
  -> MU.MVector s Word64
  -> MU.MVector s Word64
  -> HexRegion
  -> Int
  -> ST s (Maybe HexCoord)
descendHexSection target mutableValues mutableCoverage local !wordIndex
  | wordIndex >= layoutWordCount target = pure Nothing
  | otherwise = do
      values <- MU.unsafeRead mutableValues wordIndex
      coverage <- MU.unsafeRead mutableCoverage wordIndex
      let localValues = reframedWord target local wordIndex
          localCoverage = reframedCoverageWord target (hexRegionLayout local) wordIndex
          disagreement = (values `xor` localValues) .&. coverage .&. localCoverage
      if disagreement == 0
        then do
          MU.unsafeWrite mutableValues wordIndex (values .|. localValues)
          MU.unsafeWrite mutableCoverage wordIndex (coverage .|. localCoverage)
          descendHexSection target mutableValues mutableCoverage local (wordIndex + 1)
        else
          pure
            ( Just
                (hexCoordAtIndex target (wordIndex * 64 + countTrailingZeros disagreement))
            )

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

extractBits :: U.Vector Word64 -> Int -> Int -> Word64
extractBits wordsValue bitIndex count
  | count <= 0 = 0
  | otherwise =
      let (wordIndex, bitOffset) = bitIndex `quotRem` 64
          low = wordsValue `U.unsafeIndex` wordIndex `shiftR` bitOffset
          high =
            if bitOffset == 0 || bitOffset + count <= 64
              then 0
              else wordsValue `U.unsafeIndex` (wordIndex + 1) `shiftL` (64 - bitOffset)
       in (low .|. high) .&. lowBitMask count
{-# INLINE extractBits #-}

hexCoordIndex :: HexLayout -> HexCoord -> Maybe Int
hexCoordIndex layout (HexCoord q r)
  | q < originQ || r < originR = Nothing
  | q > maximumQ || r > maximumR = Nothing
  | otherwise = Just ((r - originR) * layoutWidth layout + (q - originQ))
 where
  HexCoord originQ originR = layoutOrigin layout
  maximumQ = originQ + layoutWidth layout - 1
  maximumR = originR + layoutHeight layout - 1
{-# INLINE hexCoordIndex #-}

hexCoordAtIndex :: HexLayout -> Int -> HexCoord
hexCoordAtIndex layout index =
  let (row, column) = index `quotRem` layoutWidth layout
      HexCoord originQ originR = layoutOrigin layout
   in HexCoord (originQ + column) (originR + row)
{-# INLINE hexCoordAtIndex #-}

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

finalWordMask :: HexLayout -> Word64
finalWordMask layout =
  lowBitMask (((layoutCellCount layout - 1) `rem` 64) + 1)
{-# INLINE finalWordMask #-}

lowBitMask :: Int -> Word64
lowBitMask count
  | count >= 64 = maxBound
  | count <= 0 = 0
  | otherwise = (1 `shiftL` count) - 1
{-# INLINE lowBitMask #-}

setBitAt :: Word64 -> Int -> Word64
setBitAt word bitIndex = word .|. (1 `shiftL` bitIndex)
{-# INLINE setBitAt #-}

integerToInt :: Integer -> Maybe Int
integerToInt value
  | value < toInteger (minBound :: Int) = Nothing
  | value > toInteger (maxBound :: Int) = Nothing
  | otherwise = Just (fromInteger value)
