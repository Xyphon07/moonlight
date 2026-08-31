{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.Source
  ( SourceSliceIndex (..),
    sourceSliceIndex,
    sourceSliceForRegion,
    sourcePositionOffset,
    sourceColumnOffset,
    normalizedTypeText
  )
where

import Data.Kind (Type)
import Data.Vector.Unboxed qualified as U
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Moonlight.Pale.Ghc.Expr.Syntax
-- The fields deliberately remain lazy: value-only modules never pay to index
-- source text, while every opaque declaration shares the same forced vectors.
type SourceSliceIndex :: Type
data SourceSliceIndex = SourceSliceIndex
  { ssiSourceCharacters :: U.Vector Char,
    ssiLineStartOffsets :: U.Vector Int
  }

sourceSliceIndex :: String -> SourceSliceIndex
sourceSliceIndex sourceText =
  SourceSliceIndex
    { ssiSourceCharacters = sourceCharacters,
      ssiLineStartOffsets =
        U.cons
          0
          (U.map (+ 1) (U.findIndices (== '\n') sourceCharacters))
    }
  where
    sourceCharacters = U.fromList sourceText

sourceSliceForRegion :: SourceSliceIndex -> SourceRegion -> Maybe (SourceRegion, String)
sourceSliceForRegion moduleSourceIndex regionValue = do
  startOffset <-
    sourcePositionOffset
      moduleSourceIndex
      (srStartLine regionValue)
      (srStartCol regionValue)
  endOffset <-
    sourcePositionOffset
      moduleSourceIndex
      (srEndLine regionValue)
      (srEndCol regionValue)
  if startOffset <= endOffset
    then
      Just
        ( regionValue,
          U.toList
            ( U.take
                (endOffset - startOffset)
                (U.drop startOffset (ssiSourceCharacters moduleSourceIndex))
            )
        )
    else Nothing

sourcePositionOffset :: SourceSliceIndex -> Int -> Int -> Maybe Int
sourcePositionOffset moduleSourceIndex targetLine targetColumn
  | targetLine < 1 =
      Nothing
  | otherwise = do
      lineStartOffset <-
        ssiLineStartOffsets moduleSourceIndex U.!? (targetLine - 1)
      sourceColumnOffset
        (ssiSourceCharacters moduleSourceIndex)
        lineStartOffset
        targetColumn

sourceColumnOffset :: U.Vector Char -> Int -> Int -> Maybe Int
sourceColumnOffset sourceCharacters lineStartOffset targetColumn
  | targetColumn < 1 || lineStartOffset < 0 =
      Nothing
  | otherwise =
      go 1 lineStartOffset
  where
    go !currentColumn !currentOffset
      | currentColumn == targetColumn =
          Just currentOffset
      | currentColumn > targetColumn =
          Nothing
      | otherwise =
          case sourceCharacters U.!? currentOffset of
            Nothing ->
              Nothing
            Just sourceCharacter
              | sourceCharacter == '\n' ->
                  Nothing
              | sourceCharacter == '\t' ->
                  go
                    (currentColumn + 8 - ((currentColumn - 1) `mod` 8))
                    (currentOffset + 1)
              | otherwise ->
                  go
                    (currentColumn + 1)
                    (currentOffset + 1)

normalizedTypeText :: Outputable a => a -> NormalizedTypeText
normalizedTypeText =
  NormalizedTypeText . unwords . words . showSDocUnsafe . ppr
