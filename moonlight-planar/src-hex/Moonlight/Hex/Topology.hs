{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Packed neighbourhood morphology and graph topology over native hex regions.
-- Dilation and erosion form an adjunction on each bounded layout; components
-- and distances are opaque views derived from one admitted selection.
module Moonlight.Hex.Topology
  ( hexRegionDilate
  , hexRegionErode
  , hexRegionOpening
  , hexRegionClosing
  , hexRegionInnerFrontier
  , hexRegionOuterFrontier
  , HexComponentLabels
  , hexRegionComponentLabels
  , hexComponentCount
  , hexComponentIndexAt
  , hexComponentRegion
  , HexTraversalObstruction (..)
  , HexDistanceMap
  , hexRegionDistancesWithin
  , hexDistanceMapLayout
  , hexDistanceAt
  , hexDistanceMaximum
  , hexDistanceReachableRegion
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), (.|.), complement, shiftL)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word64)
import GHC.Generics (Generic)
import Moonlight.Hex.Coordinate
  ( HexCoord (..)
  , HexDirection
  , allHexDirections
  , hexDirectionDelta
  )
import Moonlight.Hex.Region (complementHexRegion)
import Moonlight.Hex.Region.Internal
  ( HexLayout (..)
  , HexRegion (..)
  , extractBits
  , firstSameLayoutDifferenceIndex
  , generateHexRegionIndexed
  , hexCoordAtIndex
  , hexCoordIndex
  , hexRegionMemberAtIndex
  )

-- | One closed-neighbourhood step, clipped to the owning layout.
hexRegionDilate :: HexRegion -> HexRegion
hexRegionDilate region@(HexRegion layout _) =
  HexRegion layout (U.generate (layoutWordCount layout) (dilatedHexWord region))
{-# INLINE hexRegionDilate #-}

-- | Retain exactly the cells whose six neighbours are selected. A neighbour
-- outside the layout imposes no obligation: this is the right adjoint of
-- clipped dilation, so opening and closing remain lawful on bounded layouts.
hexRegionErode :: HexRegion -> HexRegion
hexRegionErode region@(HexRegion layout wordsValue) =
  let absent = complementHexRegion region
   in HexRegion layout
        (U.generate (layoutWordCount layout) (erodedHexWord absent wordsValue))
{-# INLINE hexRegionErode #-}

dilatedHexWord :: HexRegion -> Int -> Word64
dilatedHexWord region@(HexRegion _ wordsValue) wordIndex =
  foldl'
    (\word direction -> word .|. translatedHexWord region direction wordIndex)
    (wordsValue `U.unsafeIndex` wordIndex)
    allHexDirections
{-# INLINE dilatedHexWord #-}

erodedHexWord :: HexRegion -> U.Vector Word64 -> Int -> Word64
erodedHexWord absent selectedWords wordIndex =
  foldl'
    (\word direction ->
       word .&. complement (translatedHexWord absent direction wordIndex))
    (selectedWords `U.unsafeIndex` wordIndex)
    allHexDirections
{-# INLINE erodedHexWord #-}

-- | Erosion followed by dilation in the same bounded layout.
hexRegionOpening :: HexRegion -> HexRegion
hexRegionOpening = hexRegionDilate . hexRegionErode

-- | Dilation followed by erosion in the same bounded layout.
hexRegionClosing :: HexRegion -> HexRegion
hexRegionClosing = hexRegionErode . hexRegionDilate

-- | Selected cells adjacent to absence, including absence beyond the layout.
hexRegionInnerFrontier :: HexRegion -> HexRegion
hexRegionInnerFrontier source@(HexRegion layout sourceWords) =
  let absent = complementHexRegion source
   in HexRegion layout
        ( U.generate
            (layoutWordCount layout)
            (\wordIndex ->
               sourceWords `U.unsafeIndex` wordIndex
                 .&. complement (erodedHexWord absent sourceWords wordIndex))
        )

-- | Unselected cells inside the layout adjacent to a selected cell.
hexRegionOuterFrontier :: HexRegion -> HexRegion
hexRegionOuterFrontier source@(HexRegion layout sourceWords) =
  HexRegion layout
    ( U.generate
        (layoutWordCount layout)
        (\wordIndex ->
           dilatedHexWord source wordIndex
             .&. complement (sourceWords `U.unsafeIndex` wordIndex))
    )

-- | Canonical zero-based component labels in row-major seed order. Unselected
-- cells carry no public label and the view owns no independent membership.
data HexComponentLabels = HexComponentLabels !HexLayout {-# UNPACK #-} !Int !(U.Vector Int)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

hexRegionComponentLabels :: HexRegion -> HexComponentLabels
hexRegionComponentLabels region@(HexRegion layout _) = runST $ do
  labels <- MU.replicate (layoutCellCount layout) (-1)
  queue <- MU.new (layoutCellCount layout)
  count <- labelHexComponents region labels queue 0 0
  HexComponentLabels layout count <$> U.unsafeFreeze labels

hexComponentCount :: HexComponentLabels -> Int
hexComponentCount (HexComponentLabels _ count _) = count
{-# INLINE hexComponentCount #-}

-- | The component of a selected coordinate. Absence means either an
-- unselected coordinate or one outside the labels' layout.
hexComponentIndexAt :: HexCoord -> HexComponentLabels -> Maybe Int
hexComponentIndexAt coordinate (HexComponentLabels layout _ labels) = do
  index <- hexCoordIndex layout coordinate
  let component = labels `U.unsafeIndex` index
  if component < 0 then Nothing else Just component
{-# INLINE hexComponentIndexAt #-}

-- | Materialize one component on demand; invalid component indices are
-- rejected without fabricating an empty component.
hexComponentRegion :: Int -> HexComponentLabels -> Maybe HexRegion
hexComponentRegion requested (HexComponentLabels layout count labels)
  | requested < 0 || requested >= count = Nothing
  | otherwise = Just (generateHexRegionIndexed layout ((== requested) . (labels `U.unsafeIndex`)))

-- | Typed incompatibilities for traversal inside a selected domain.
data HexTraversalObstruction
  = HexTraversalLayoutMismatch !HexLayout !HexLayout
  | HexTraversalSourceOutsideDomain !HexCoord
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Dense shortest-path distances through one hex region. Negative internal
-- entries are unreachable and never escape through the public observation.
data HexDistanceMap = HexDistanceMap !HexLayout !(U.Vector Int)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Multi-source unit distances through selected domain cells.
hexRegionDistancesWithin
  :: HexRegion
  -> HexRegion
  -> Either HexTraversalObstruction HexDistanceMap
hexRegionDistancesWithin domain@(HexRegion domainLayout _) sources@(HexRegion sourceLayout _)
  | domainLayout /= sourceLayout = Left (HexTraversalLayoutMismatch domainLayout sourceLayout)
  | Just sourceIndex <- firstSameLayoutDifferenceIndex sources domain =
      Left (HexTraversalSourceOutsideDomain (hexCoordAtIndex domainLayout sourceIndex))
  | otherwise = Right (runST buildDistances)
 where
  buildDistances :: forall s. ST s HexDistanceMap
  buildDistances = do
    distances <- MU.replicate (layoutCellCount domainLayout) (-1)
    queue <- MU.new (layoutCellCount domainLayout)
    queued <- seedHexDistances sources distances queue 0 0
    descendHexDistances domain distances queue 0 queued
    HexDistanceMap domainLayout <$> U.unsafeFreeze distances

hexDistanceMapLayout :: HexDistanceMap -> HexLayout
hexDistanceMapLayout (HexDistanceMap layout _) = layout
{-# INLINE hexDistanceMapLayout #-}

hexDistanceAt :: HexCoord -> HexDistanceMap -> Maybe Int
hexDistanceAt coordinate (HexDistanceMap layout distances) = do
  index <- hexCoordIndex layout coordinate
  let distance = distances `U.unsafeIndex` index
  if distance < 0 then Nothing else Just distance
{-# INLINE hexDistanceAt #-}

hexDistanceMaximum :: HexDistanceMap -> Maybe Int
hexDistanceMaximum (HexDistanceMap _ distances) =
  let maximumValue = U.foldl' max (-1) distances
   in if maximumValue < 0 then Nothing else Just maximumValue

hexDistanceReachableRegion :: HexDistanceMap -> HexRegion
hexDistanceReachableRegion (HexDistanceMap layout distances) =
  generateHexRegionIndexed layout ((>= 0) . (distances `U.unsafeIndex`))

translatedHexWord :: HexRegion -> HexDirection -> Int -> Word64
translatedHexWord (HexRegion layout wordsValue) direction wordIndex =
  gather 0 (wordIndex * 64) 0
 where
  HexCoord deltaQ deltaR = hexDirectionDelta direction

  gather !destinationShift !targetIndex !accumulator
    | destinationShift >= 64 || targetIndex >= layoutCellCount layout = accumulator
    | otherwise =
        let (targetRow, targetColumn) = targetIndex `quotRem` layoutWidth layout
            rowRun = min (64 - destinationShift) (layoutWidth layout - targetColumn)
            sourceRow = targetRow - deltaR
            finalTargetColumn = targetColumn + rowRun - 1
            copiedTargetColumn = max targetColumn deltaQ
            copiedFinalTargetColumn = min finalTargetColumn (layoutWidth layout - 1 + deltaQ)
            hasRow = sourceRow >= 0 && sourceRow < layoutHeight layout
            hasColumns = copiedTargetColumn <= copiedFinalTargetColumn
            copied =
              if hasRow && hasColumns
                then
                  let leading = copiedTargetColumn - targetColumn
                      copiedCount = copiedFinalTargetColumn - copiedTargetColumn + 1
                      sourceColumn = copiedTargetColumn - deltaQ
                      sourceBitIndex = sourceRow * layoutWidth layout + sourceColumn
                   in extractBits wordsValue sourceBitIndex copiedCount
                        `shiftL` (destinationShift + leading)
                else 0
         in gather
              (destinationShift + rowRun)
              (targetIndex + rowRun)
              (accumulator .|. copied)
{-# INLINE translatedHexWord #-}

labelHexComponents
  :: HexRegion
  -> MU.MVector s Int
  -> MU.MVector s Int
  -> Int
  -> Int
  -> ST s Int
labelHexComponents region@(HexRegion layout _) labels queue index component
  | index >= layoutCellCount layout = pure component
  | not (hexRegionMemberAtIndex region index) =
      labelHexComponents region labels queue (index + 1) component
  | otherwise = do
      existing <- MU.unsafeRead labels index
      if existing >= 0
        then labelHexComponents region labels queue (index + 1) component
        else do
          MU.unsafeWrite labels index component
          MU.unsafeWrite queue 0 index
          descendHexComponent region labels queue component 0 1
          labelHexComponents region labels queue (index + 1) (component + 1)

descendHexComponent
  :: HexRegion
  -> MU.MVector s Int
  -> MU.MVector s Int
  -> Int
  -> Int
  -> Int
  -> ST s ()
descendHexComponent region@(HexRegion layout _) labels queue component readIndex queued
  | readIndex >= queued = pure ()
  | otherwise = do
      cellIndex <- MU.unsafeRead queue readIndex
      nextQueued <-
        foldHexNeighbourIndicesM
          layout
          cellIndex
          (admitComponentCell region labels queue component)
          queued
      descendHexComponent region labels queue component (readIndex + 1) nextQueued

admitComponentCell
  :: HexRegion
  -> MU.MVector s Int
  -> MU.MVector s Int
  -> Int
  -> Int
  -> Int
  -> ST s Int
admitComponentCell region labels queue component queued candidate
  | not (hexRegionMemberAtIndex region candidate) = pure queued
  | otherwise = do
      existing <- MU.unsafeRead labels candidate
      if existing >= 0
        then pure queued
        else do
          MU.unsafeWrite labels candidate component
          MU.unsafeWrite queue queued candidate
          pure (queued + 1)

seedHexDistances
  :: HexRegion
  -> MU.MVector s Int
  -> MU.MVector s Int
  -> Int
  -> Int
  -> ST s Int
seedHexDistances region@(HexRegion layout _) distances queue index queued
  | index >= layoutCellCount layout = pure queued
  | hexRegionMemberAtIndex region index = do
      MU.unsafeWrite distances index 0
      MU.unsafeWrite queue queued index
      seedHexDistances region distances queue (index + 1) (queued + 1)
  | otherwise =
      seedHexDistances region distances queue (index + 1) queued

descendHexDistances
  :: HexRegion
  -> MU.MVector s Int
  -> MU.MVector s Int
  -> Int
  -> Int
  -> ST s ()
descendHexDistances domain@(HexRegion layout _) distances queue readIndex queued
  | readIndex >= queued = pure ()
  | otherwise = do
      cellIndex <- MU.unsafeRead queue readIndex
      distance <- MU.unsafeRead distances cellIndex
      nextQueued <-
        foldHexNeighbourIndicesM
          layout
          cellIndex
          (admitDistanceCell domain distances queue (distance + 1))
          queued
      descendHexDistances domain distances queue (readIndex + 1) nextQueued

admitDistanceCell
  :: HexRegion
  -> MU.MVector s Int
  -> MU.MVector s Int
  -> Int
  -> Int
  -> Int
  -> ST s Int
admitDistanceCell region distances queue distance queued candidate
  | not (hexRegionMemberAtIndex region candidate) = pure queued
  | otherwise = do
      existing <- MU.unsafeRead distances candidate
      if existing >= 0
        then pure queued
        else do
          MU.unsafeWrite distances candidate distance
          MU.unsafeWrite queue queued candidate
          pure (queued + 1)

foldHexNeighbourIndicesM
  :: HexLayout
  -> Int
  -> (accumulator -> Int -> ST s accumulator)
  -> accumulator
  -> ST s accumulator
foldHexNeighbourIndicesM layout index step initial = do
  let (row, column) = index `quotRem` layoutWidth layout
      hasEast = column + 1 < layoutWidth layout
      hasNorth = row > 0
      hasWest = column > 0
      hasSouth = row + 1 < layoutHeight layout
      applyIf condition candidate accumulator =
        if condition then step accumulator candidate else pure accumulator
  east <- applyIf hasEast (index + 1) initial
  northEast <- applyIf (hasNorth && hasEast) (index - layoutWidth layout + 1) east
  northWest <- applyIf hasNorth (index - layoutWidth layout) northEast
  west <- applyIf hasWest (index - 1) northWest
  southWest <- applyIf (hasSouth && hasWest) (index + layoutWidth layout - 1) west
  applyIf hasSouth (index + layoutWidth layout) southWest
{-# INLINE foldHexNeighbourIndicesM #-}
