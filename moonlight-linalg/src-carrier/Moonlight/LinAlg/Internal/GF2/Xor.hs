{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.LinAlg.Internal.GF2.Xor
  ( PackedRow
  , packedRowWidth
  , packedRowNonZeroCount
  , emptyPackedRow
  , unitPackedRow
  , packedRowFromIndices
  , packedRowIndices
  , packedRowMember
  , packedRowIsZero
  , packedRowXor
  , packedRowRemap
  , PackedLinearMap
  , packedLinearMapDomain
  , packedLinearMapCodomain
  , packedLinearMapColumns
  , packedLinearMapFromColumns
  , packedLinearMapFromEntries
  , zeroPackedLinearMap
  , identityPackedLinearMap
  , applyPackedLinearMap
  , composePackedLinearMaps
  , addPackedLinearMaps
  , packedLinearMapIsZero
  , PackedSpan
  , emptyPackedSpan
  , packedSpanFromRows
  , reducePackedRow
  , admitPackedRow
  , ColumnReduction (..)
  , reducePackedColumns
  , PackedCoordinateSolver
  , packedCoordinateSolver
  , coordinatesInPackedBasis
  , inverseFromPackedBasisColumns
  , rankPackedRowsByReduction
  ) where

import Control.Monad (foldM, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits
  ( bit
  , clearBit
  , complement
  , countTrailingZeros
  , popCount
  , testBit
  , xor
  , (.&.)
  )
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word (Word64)
import Moonlight.Core (MoonlightError (..))

wordBits :: Int
wordBits = 64

wordCountForWidth :: Int -> Int
wordCountForWidth widthValue
  | widthValue <= 0 = 0
  | otherwise = (widthValue + wordBits - 1) `div` wordBits

lastWordMask :: Int -> Word64
lastWordMask widthValue
  | widthValue <= 0 = 0
  | remainderValue == 0 = complement 0
  | otherwise = bit remainderValue - 1
  where
    remainderValue = widthValue `mod` wordBits

type PackedRow :: Type
data PackedRow = PackedRow
  { prWidth :: !Int
  , prWords :: !(U.Vector Word64)
  , prNonZeroCount :: !Int
  }
  deriving stock (Eq, Show)

packedRowWidth :: PackedRow -> Int
packedRowWidth = prWidth

packedRowNonZeroCount :: PackedRow -> Int
packedRowNonZeroCount = prNonZeroCount

packedRowFromWords :: Int -> U.Vector Word64 -> PackedRow
packedRowFromWords widthValue rawWords =
  let expectedWords = wordCountForWidth widthValue
      paddedWords =
        U.generate
          expectedWords
          (\wordIndex -> maybe 0 id (rawWords U.!? wordIndex))
      maskedWords
        | U.null paddedWords = U.empty
        | otherwise =
            U.imap
              (\wordIndex wordValue ->
                if wordIndex == U.length paddedWords - 1
                  then wordValue .&. lastWordMask widthValue
                  else wordValue
              )
              paddedWords
   in PackedRow
        { prWidth = widthValue
        , prWords = maskedWords
        , prNonZeroCount = U.foldl' (\countValue wordValue -> countValue + popCount wordValue) 0 maskedWords
        }

emptyPackedRow :: Int -> Either MoonlightError PackedRow
emptyPackedRow widthValue
  | widthValue < 0 =
      Left (InvariantViolation ("emptyPackedRow: negative width " <> show widthValue))
  | otherwise =
      Right (packedRowFromWords widthValue U.empty)

unitPackedRow :: String -> Int -> Int -> Either MoonlightError PackedRow
unitPackedRow context widthValue indexValue =
  packedRowFromIndices context widthValue [indexValue]

packedRowFromIndices :: String -> Int -> [Int] -> Either MoonlightError PackedRow
packedRowFromIndices context widthValue indicesValue = do
  unless (widthValue >= 0)
    (Left (InvariantViolation (context <> ": negative packed-row width " <> show widthValue)))
  traverse_ validateIndex indicesValue
  let wordValues =
        U.create $ do
          mutableWords <- UM.replicate (wordCountForWidth widthValue) 0
          traverse_ (toggleIndex mutableWords) indicesValue
          pure mutableWords
  Right (packedRowFromWords widthValue wordValues)
  where
    validateIndex indexValue
      | indexValue < 0 || indexValue >= widthValue =
          Left
            ( InvariantViolation
                ( context
                    <> ": packed coordinate "
                    <> show indexValue
                    <> " is outside width "
                    <> show widthValue
                )
            )
      | otherwise = Right ()

    toggleIndex :: UM.MVector state Word64 -> Int -> ST state ()
    toggleIndex mutableWords indexValue = do
      let wordIndex = indexValue `div` wordBits
          bitIndex = indexValue `mod` wordBits
      oldWord <- UM.read mutableWords wordIndex
      UM.write mutableWords wordIndex (oldWord `xor` bit bitIndex)

packedRowIndices :: PackedRow -> [Int]
packedRowIndices PackedRow {prWidth, prWords} =
  reverse (U.ifoldl' collectWord [] prWords)
  where
    collectWord accumulated wordIndex wordValue =
      collectBits accumulated (wordIndex * wordBits) wordValue

    collectBits accumulated baseIndex remainingWord
      | remainingWord == 0 = accumulated
      | otherwise =
          let bitIndex = countTrailingZeros remainingWord
              coordinateValue = baseIndex + bitIndex
              nextWord = clearBit remainingWord bitIndex
           in collectBits
                ( if coordinateValue < prWidth
                    then coordinateValue : accumulated
                    else accumulated
                )
                baseIndex
                nextWord

packedRowMember :: Int -> PackedRow -> Bool
packedRowMember indexValue PackedRow {prWidth, prWords}
  | indexValue < 0 || indexValue >= prWidth = False
  | otherwise =
      maybe
        False
        (`testBit` (indexValue `mod` wordBits))
        (prWords U.!? (indexValue `div` wordBits))

packedRowIsZero :: PackedRow -> Bool
packedRowIsZero = (== 0) . prNonZeroCount

xorSameWidth :: PackedRow -> PackedRow -> PackedRow
xorSameWidth leftRow rightRow =
  packedRowFromWords
    (prWidth leftRow)
    (U.zipWith xor (prWords leftRow) (prWords rightRow))

packedRowXor :: String -> PackedRow -> PackedRow -> Either MoonlightError PackedRow
packedRowXor context leftRow rightRow
  | prWidth leftRow /= prWidth rightRow =
      Left
        ( InvariantViolation
            ( context
                <> ": packed-row width mismatch "
                <> show (prWidth leftRow, prWidth rightRow)
            )
        )
  | otherwise = Right (xorSameWidth leftRow rightRow)

packedRowRemap ::
  String ->
  Int ->
  (Int -> Maybe Int) ->
  PackedRow ->
  Either MoonlightError PackedRow
packedRowRemap context targetWidth remapIndex sourceRow = do
  remappedIndices <- traverse remapOne (packedRowIndices sourceRow)
  packedRowFromIndices context targetWidth remappedIndices
  where
    remapOne sourceIndex =
      case remapIndex sourceIndex of
        Nothing ->
          Left
            ( InvariantViolation
                ( context
                    <> ": no target coordinate for source coordinate "
                    <> show sourceIndex
                )
            )
        Just targetIndex -> Right targetIndex

type PackedLinearMap :: Type
data PackedLinearMap = PackedLinearMap
  { plmDomain :: !Int
  , plmCodomain :: !Int
  , plmColumns :: !(Vector PackedRow)
  }
  deriving stock (Eq, Show)

packedLinearMapDomain :: PackedLinearMap -> Int
packedLinearMapDomain = plmDomain

packedLinearMapCodomain :: PackedLinearMap -> Int
packedLinearMapCodomain = plmCodomain

packedLinearMapColumns :: PackedLinearMap -> Vector PackedRow
packedLinearMapColumns = plmColumns

packedLinearMapFromColumns ::
  String ->
  Int ->
  Int ->
  Vector PackedRow ->
  Either MoonlightError PackedLinearMap
packedLinearMapFromColumns context domainValue codomainValue columnValues
  | domainValue < 0 || codomainValue < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative linear-map shape "
                <> show (codomainValue, domainValue)
            )
        )
  | V.length columnValues /= domainValue =
      Left
        ( InvariantViolation
            ( context
                <> ": received "
                <> show (V.length columnValues)
                <> " columns for domain dimension "
                <> show domainValue
            )
        )
  | otherwise = do
      traverse_ validateColumn (V.toList (V.indexed columnValues))
      Right
        PackedLinearMap
          { plmDomain = domainValue
          , plmCodomain = codomainValue
          , plmColumns = columnValues
          }
  where
    validateColumn (columnIndex, columnValue)
      | prWidth columnValue == codomainValue = Right ()
      | otherwise =
          Left
            ( InvariantViolation
                ( context
                    <> ": column "
                    <> show columnIndex
                    <> " has width "
                    <> show (prWidth columnValue)
                    <> ", expected "
                    <> show codomainValue
                )
            )

packedLinearMapFromEntries ::
  String ->
  Int ->
  Int ->
  [(Int, Int)] ->
  Either MoonlightError PackedLinearMap
packedLinearMapFromEntries context domainValue codomainValue entriesValue = do
  unless (domainValue >= 0 && codomainValue >= 0)
    (Left (InvariantViolation (context <> ": negative linear-map shape " <> show (codomainValue, domainValue))))
  traverse_ validateEntry entriesValue
  columnsValue <-
    traverse
      (\columnIndex ->
        packedRowFromIndices
          (context <> ": column " <> show columnIndex)
          codomainValue
          (IntMap.findWithDefault [] columnIndex entriesByColumn)
      )
      [0 .. domainValue - 1]
  packedLinearMapFromColumns context domainValue codomainValue (V.fromList columnsValue)
  where
    entriesByColumn =
      foldl'
        (\entryMap (rowIndex, columnIndex) ->
          IntMap.insertWith (flip (<>)) columnIndex [rowIndex] entryMap
        )
        IntMap.empty
        entriesValue

    validateEntry (rowIndex, columnIndex)
      | rowIndex < 0 || rowIndex >= codomainValue =
          Left
            ( InvariantViolation
                ( context
                    <> ": row index "
                    <> show rowIndex
                    <> " is outside codomain dimension "
                    <> show codomainValue
                )
            )
      | columnIndex < 0 || columnIndex >= domainValue =
          Left
            ( InvariantViolation
                ( context
                    <> ": column index "
                    <> show columnIndex
                    <> " is outside domain dimension "
                    <> show domainValue
                )
            )
      | otherwise = Right ()

zeroPackedLinearMap :: String -> Int -> Int -> Either MoonlightError PackedLinearMap
zeroPackedLinearMap context domainValue codomainValue
  | domainValue < 0 || codomainValue < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative linear-map shape "
                <> show (codomainValue, domainValue)
            )
        )
  | otherwise = do
      zeroColumn <- emptyPackedRow codomainValue
      packedLinearMapFromColumns context domainValue codomainValue (V.replicate domainValue zeroColumn)

identityPackedLinearMap :: String -> Int -> Either MoonlightError PackedLinearMap
identityPackedLinearMap context dimensionValue = do
  columnsValue <- traverse (unitPackedRow context dimensionValue) [0 .. dimensionValue - 1]
  packedLinearMapFromColumns context dimensionValue dimensionValue (V.fromList columnsValue)

xorIntoMutable :: UM.MVector state Word64 -> U.Vector Word64 -> ST state ()
xorIntoMutable mutableTarget sourceWords =
  traverse_ xorWordAt [0 .. UM.length mutableTarget - 1]
  where
    xorWordAt wordIndex = do
      oldWord <- UM.read mutableTarget wordIndex
      let sourceWord = maybe 0 id (sourceWords U.!? wordIndex)
      UM.write mutableTarget wordIndex (oldWord `xor` sourceWord)

applyPackedLinearMap :: String -> PackedLinearMap -> PackedRow -> Either MoonlightError PackedRow
applyPackedLinearMap context linearMap sourceRow
  | prWidth sourceRow /= plmDomain linearMap =
      Left
        ( InvariantViolation
            ( context
                <> ": vector width "
                <> show (prWidth sourceRow)
                <> " does not match map domain "
                <> show (plmDomain linearMap)
            )
        )
  | otherwise = do
      selectedColumns <-
        traverse
          (\columnIndex -> lookupVector (context <> ": source column") columnIndex (plmColumns linearMap))
          (packedRowIndices sourceRow)
      let resultWords =
            runST $ do
              mutableResult <- UM.replicate (wordCountForWidth (plmCodomain linearMap)) 0
              traverse_ (xorIntoMutable mutableResult . prWords) selectedColumns
              U.freeze mutableResult
      Right (packedRowFromWords (plmCodomain linearMap) resultWords)

composePackedLinearMaps ::
  String ->
  PackedLinearMap ->
  PackedLinearMap ->
  Either MoonlightError PackedLinearMap
composePackedLinearMaps context leftMap rightMap
  | plmDomain leftMap /= plmCodomain rightMap =
      Left
        ( InvariantViolation
            ( context
                <> ": incompatible map shapes "
                <> show (plmCodomain leftMap, plmDomain leftMap)
                <> " and "
                <> show (plmCodomain rightMap, plmDomain rightMap)
            )
        )
  | otherwise = do
      productColumns <-
        traverse
          (applyPackedLinearMap (context <> ": product column") leftMap)
          (plmColumns rightMap)
      packedLinearMapFromColumns context (plmDomain rightMap) (plmCodomain leftMap) productColumns

addPackedLinearMaps ::
  String ->
  PackedLinearMap ->
  PackedLinearMap ->
  Either MoonlightError PackedLinearMap
addPackedLinearMaps context leftMap rightMap
  | mapShape leftMap /= mapShape rightMap =
      Left
        ( InvariantViolation
            ( context
                <> ": linear-map shape mismatch "
                <> show (mapShape leftMap, mapShape rightMap)
            )
        )
  | otherwise =
      packedLinearMapFromColumns
        context
        (plmDomain leftMap)
        (plmCodomain leftMap)
        (V.zipWith xorSameWidth (plmColumns leftMap) (plmColumns rightMap))

packedLinearMapIsZero :: PackedLinearMap -> Bool
packedLinearMapIsZero = V.all packedRowIsZero . plmColumns

mapShape :: PackedLinearMap -> (Int, Int)
mapShape mapValue = (plmCodomain mapValue, plmDomain mapValue)

type PackedSpan :: Type
data PackedSpan = PackedSpan
  { psWidth :: !Int
  , psBasis :: !(IntMap PackedRow)
  }
  deriving stock (Eq, Show)

emptyPackedSpan :: Int -> Either MoonlightError PackedSpan
emptyPackedSpan widthValue
  | widthValue < 0 = Left (InvariantViolation ("emptyPackedSpan: negative width " <> show widthValue))
  | otherwise = Right PackedSpan {psWidth = widthValue, psBasis = IntMap.empty}

packedSpanFromRows :: String -> Int -> [PackedRow] -> Either MoonlightError PackedSpan
packedSpanFromRows context widthValue rowsValue = do
  initialSpan <- emptyPackedSpan widthValue
  foldM (\spanValue rowValue -> snd <$> admitPackedRow context rowValue spanValue) initialSpan rowsValue

mutablePivot :: Int -> UM.MVector state Word64 -> ST state (Maybe Int)
mutablePivot widthValue mutableWords = do
  candidates <- traverse pivotAtWord [0 .. UM.length mutableWords - 1]
  pure (foldr firstJust Nothing candidates)
  where
    pivotAtWord wordIndex = do
      wordValue <- UM.read mutableWords wordIndex
      pure
        ( if wordValue == 0
            then Nothing
            else
              let pivotIndex = wordIndex * wordBits + countTrailingZeros wordValue
               in if pivotIndex < widthValue then Just pivotIndex else Nothing
        )

    firstJust :: Maybe Int -> Maybe Int -> Maybe Int
    firstJust left right =
      case left of
        Nothing -> right
        Just _ -> left

reduceMutableWords :: Int -> IntMap PackedRow -> UM.MVector state Word64 -> ST state ()
reduceMutableWords widthValue basisRows mutableWords = do
  maybePivot <- mutablePivot widthValue mutableWords
  case maybePivot of
    Nothing -> pure ()
    Just pivotIndex ->
      case IntMap.lookup pivotIndex basisRows of
        Nothing -> pure ()
        Just basisRow -> do
          xorIntoMutable mutableWords (prWords basisRow)
          reduceMutableWords widthValue basisRows mutableWords

reducePackedRowUnchecked :: PackedSpan -> PackedRow -> PackedRow
reducePackedRowUnchecked PackedSpan {psWidth, psBasis} rowValue =
  let reducedWords = runST $ do
        mutableWords <- U.thaw (prWords rowValue)
        reduceMutableWords psWidth psBasis mutableWords
        U.freeze mutableWords
   in packedRowFromWords psWidth reducedWords

reducePackedRow :: String -> PackedSpan -> PackedRow -> Either MoonlightError PackedRow
reducePackedRow context spanValue@PackedSpan {psWidth} rowValue
  | prWidth rowValue /= psWidth =
      Left
        ( InvariantViolation
            ( context
                <> ": row width "
                <> show (prWidth rowValue)
                <> " does not match span width "
                <> show psWidth
            )
        )
  | otherwise = Right (reducePackedRowUnchecked spanValue rowValue)

packedRowPivot :: PackedRow -> Maybe Int
packedRowPivot PackedRow {prWidth, prWords} =
  U.ifoldl' firstPivot Nothing prWords
  where
    firstPivot (Just pivotIndex) _ _ =
      Just pivotIndex
    firstPivot Nothing wordIndex wordValue
      | wordValue == 0 = Nothing
      | otherwise =
          let pivotIndex = wordIndex * wordBits + countTrailingZeros wordValue
           in if pivotIndex < prWidth then Just pivotIndex else Nothing

admitPackedRow ::
  String ->
  PackedRow ->
  PackedSpan ->
  Either MoonlightError (Maybe PackedRow, PackedSpan)
admitPackedRow context candidateRow spanValue@PackedSpan {psWidth, psBasis} = do
  reducedRow <- reducePackedRow context spanValue candidateRow
  case packedRowPivot reducedRow of
    Nothing -> Right (Nothing, spanValue)
    Just pivotIndex ->
      Right
        ( Just reducedRow
        , PackedSpan
            { psWidth = psWidth
            , psBasis = IntMap.insert pivotIndex reducedRow psBasis
            }
        )

type TrackedBasisRow :: Type
data TrackedBasisRow = TrackedBasisRow
  { tbrData :: !PackedRow
  , tbrWitness :: !PackedRow
  }
  deriving stock (Eq, Show)

type ColumnReduction :: Type
data ColumnReduction = ColumnReduction
  { crIndependentIndices :: !(Vector Int)
  , crKernelBasis :: !(Vector PackedRow)
  }
  deriving stock (Eq, Show)

reduceMutableTracked ::
  Int ->
  IntMap TrackedBasisRow ->
  UM.MVector state Word64 ->
  UM.MVector state Word64 ->
  ST state ()
reduceMutableTracked dataWidth basisRows mutableData mutableWitness = do
  maybePivot <- mutablePivot dataWidth mutableData
  case maybePivot of
    Nothing -> pure ()
    Just pivotIndex ->
      case IntMap.lookup pivotIndex basisRows of
        Nothing -> pure ()
        Just TrackedBasisRow {tbrData, tbrWitness} -> do
          xorIntoMutable mutableData (prWords tbrData)
          xorIntoMutable mutableWitness (prWords tbrWitness)
          reduceMutableTracked dataWidth basisRows mutableData mutableWitness

reduceTrackedRows ::
  String ->
  IntMap TrackedBasisRow ->
  PackedRow ->
  PackedRow ->
  Either MoonlightError (PackedRow, PackedRow)
reduceTrackedRows context basisRows dataRow witnessRow = do
  traverse_ validateBasis (IntMap.toList basisRows)
  let (dataWords, witnessWords) =
        runST $ do
          mutableData <- U.thaw (prWords dataRow)
          mutableWitness <- U.thaw (prWords witnessRow)
          reduceMutableTracked (prWidth dataRow) basisRows mutableData mutableWitness
          frozenData <- U.freeze mutableData
          frozenWitness <- U.freeze mutableWitness
          pure (frozenData, frozenWitness)
  Right
    ( packedRowFromWords (prWidth dataRow) dataWords
    , packedRowFromWords (prWidth witnessRow) witnessWords
    )
  where
    validateBasis (pivotIndex, TrackedBasisRow {tbrData, tbrWitness})
      | pivotIndex < 0 || pivotIndex >= prWidth dataRow =
          Left (InvariantViolation (context <> ": tracked pivot outside data width: " <> show pivotIndex))
      | prWidth tbrData /= prWidth dataRow =
          Left (InvariantViolation (context <> ": tracked data width mismatch"))
      | prWidth tbrWitness /= prWidth witnessRow =
          Left (InvariantViolation (context <> ": tracked witness width mismatch"))
      | otherwise = Right ()

reducePackedColumns ::
  String ->
  Int ->
  Vector PackedRow ->
  Either MoonlightError ColumnReduction
reducePackedColumns context codomainWidth columnsValue = do
  unless (codomainWidth >= 0)
    (Left (InvariantViolation (context <> ": negative codomain width " <> show codomainWidth)))
  traverse_ validateColumn (V.toList (V.indexed columnsValue))
  let domainWidth = V.length columnsValue
  (_, independentReversed, kernelReversed) <-
    foldM
      (reduceColumn domainWidth)
      (IntMap.empty, [], [])
      (V.toList (V.indexed columnsValue))
  Right
    ColumnReduction
      { crIndependentIndices = V.fromList (reverse independentReversed)
      , crKernelBasis = V.fromList (reverse kernelReversed)
      }
  where
    validateColumn (columnIndex, columnValue)
      | prWidth columnValue == codomainWidth = Right ()
      | otherwise =
          Left
            ( InvariantViolation
                ( context
                    <> ": column "
                    <> show columnIndex
                    <> " has width "
                    <> show (prWidth columnValue)
                    <> ", expected "
                    <> show codomainWidth
                )
            )

    reduceColumn domainWidth (basisRows, independentReversed, kernelReversed) (columnIndex, columnValue) = do
      witnessValue <- unitPackedRow (context <> ": witness") domainWidth columnIndex
      (reducedData, reducedWitness) <- reduceTrackedRows context basisRows columnValue witnessValue
      case packedRowPivot reducedData of
        Nothing -> Right (basisRows, independentReversed, reducedWitness : kernelReversed)
        Just pivotIndex ->
          Right
            ( IntMap.insert
                pivotIndex
                TrackedBasisRow {tbrData = reducedData, tbrWitness = reducedWitness}
                basisRows
            , columnIndex : independentReversed
            , kernelReversed
            )

type PackedCoordinateSolver :: Type
data PackedCoordinateSolver = PackedCoordinateSolver
  { pcsAmbientWidth :: !Int
  , pcsBasisCardinality :: !Int
  , pcsBasisRows :: !(IntMap TrackedBasisRow)
  }
  deriving stock (Eq, Show)

packedCoordinateSolver ::
  String ->
  Int ->
  Vector PackedRow ->
  Either MoonlightError PackedCoordinateSolver
packedCoordinateSolver context ambientWidth basisColumns = do
  let basisCardinality = V.length basisColumns
  unless (ambientWidth >= 0)
    (Left (InvariantViolation (context <> ": negative ambient width " <> show ambientWidth)))
  unless (basisCardinality <= ambientWidth)
    ( Left
        ( InvariantViolation
            ( context
                <> ": basis cardinality "
                <> show basisCardinality
                <> " exceeds ambient width "
                <> show ambientWidth
            )
        )
    )
  basisRows <-
    foldM insertBasisColumn IntMap.empty (V.toList (V.indexed basisColumns))
  Right
    PackedCoordinateSolver
      { pcsAmbientWidth = ambientWidth
      , pcsBasisCardinality = basisCardinality
      , pcsBasisRows = basisRows
      }
  where
    insertBasisColumn basisRows (basisIndex, columnValue)
      | prWidth columnValue /= ambientWidth =
          Left
            ( InvariantViolation
                ( context
                    <> ": basis column "
                    <> show basisIndex
                    <> " has width "
                    <> show (prWidth columnValue)
                    <> ", expected "
                    <> show ambientWidth
                )
            )
      | otherwise = do
          witnessValue <- unitPackedRow (context <> ": basis witness") (V.length basisColumns) basisIndex
          (reducedData, reducedWitness) <-
            reduceTrackedRows (context <> ": basis reduction") basisRows columnValue witnessValue
          case packedRowPivot reducedData of
            Nothing ->
              Left
                ( InvariantViolation
                    ( context
                        <> ": supplied basis columns are linearly dependent at column "
                        <> show basisIndex
                    )
                )
            Just pivotIndex ->
              Right
                ( IntMap.insert
                    pivotIndex
                    TrackedBasisRow {tbrData = reducedData, tbrWitness = reducedWitness}
                    basisRows
                )

coordinatesInPackedBasis ::
  String ->
  PackedCoordinateSolver ->
  PackedRow ->
  Either MoonlightError (Maybe PackedRow)
coordinatesInPackedBasis context PackedCoordinateSolver {pcsAmbientWidth, pcsBasisCardinality, pcsBasisRows} vectorValue
  | prWidth vectorValue /= pcsAmbientWidth =
      Left
        ( InvariantViolation
            ( context
                <> ": vector width "
                <> show (prWidth vectorValue)
                <> " does not match ambient width "
                <> show pcsAmbientWidth
            )
        )
  | otherwise = do
      zeroWitness <- emptyPackedRow pcsBasisCardinality
      (reducedData, reducedWitness) <-
        reduceTrackedRows context pcsBasisRows vectorValue zeroWitness
      Right (if packedRowIsZero reducedData then Just reducedWitness else Nothing)

inverseFromPackedBasisColumns ::
  String ->
  Vector PackedRow ->
  Either MoonlightError PackedLinearMap
inverseFromPackedBasisColumns context basisColumns = do
  let dimensionValue = V.length basisColumns
  solver <- packedCoordinateSolver (context <> ": coordinate solver") dimensionValue basisColumns
  inverseColumns <- traverse (inverseColumn solver) [0 .. dimensionValue - 1]
  packedLinearMapFromColumns context dimensionValue dimensionValue (V.fromList inverseColumns)
  where
    inverseColumn solver columnIndex = do
      unitVector <- unitPackedRow (context <> ": inverse unit vector") (V.length basisColumns) columnIndex
      maybeCoordinates <- coordinatesInPackedBasis (context <> ": inverse coordinates") solver unitVector
      case maybeCoordinates of
        Nothing -> Left (InvariantViolation (context <> ": basis columns do not span the ambient space"))
        Just coordinatesValue -> Right coordinatesValue

rankPackedRowsByReduction :: Int -> [U.Vector Word64] -> Int
rankPackedRowsByReduction widthValue rowWords
  | widthValue <= 0 = 0
  | otherwise =
      IntMap.size
        ( psBasis
            ( foldl'
                admitUnchecked
                PackedSpan {psWidth = widthValue, psBasis = IntMap.empty}
                (packedRowFromWords widthValue <$> rowWords)
            )
        )
  where
    admitUnchecked spanValue@PackedSpan {psWidth, psBasis} rowValue =
      let reducedRow = reducePackedRowUnchecked spanValue rowValue
       in case packedRowPivot reducedRow of
            Nothing -> spanValue
            Just pivotIndex ->
              PackedSpan
                { psWidth = psWidth
                , psBasis = IntMap.insert pivotIndex reducedRow psBasis
                }

lookupVector :: String -> Int -> Vector value -> Either MoonlightError value
lookupVector context indexValue vectorValue =
  case vectorValue V.!? indexValue of
    Nothing ->
      Left
        ( InvariantViolation
            ( context
                <> ": index "
                <> show indexValue
                <> " is outside vector length "
                <> show (V.length vectorValue)
            )
        )
    Just value -> Right value

traverse_ :: Applicative f => (a -> f b) -> [a] -> f ()
traverse_ actionValue =
  foldr (\value accumulated -> actionValue value *> accumulated) (pure ())
