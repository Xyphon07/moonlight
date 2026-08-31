{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Internal.Layout
  ( checkedRelationWordCount,
    checkedPairCellCount,
  )
where

import Data.Word (Word64)
import Foreign.Storable (sizeOf)
import Moonlight.FiniteLattice.Internal.Key
  ( contextKeySetChunkCount,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextCompileLimits (..),
    ContextLatticeCompileError (..),
    ContextRepresentation (..),
  )
import Numeric.Natural (Natural)

data ContextLayoutFailure
  = ContextLayoutCountOverflow !Integer
  | ContextLayoutLimitExceeded !Integer !Natural
  deriving stock (Eq, Ord, Show, Read)

checkedRelationWordCount ::
  ContextCompileLimits ->
  Int ->
  Either (ContextLatticeCompileError c) Int
checkedRelationWordCount limits size =
  case
    checkedRepresentationProduct
      (cclMaximumRelationBytes limits)
      (sizeOf (0 :: Word64))
      2
      [size, contextKeySetChunkCount size]
    of
    Left (ContextLayoutCountOverflow requestedCount) ->
      Left (ContextLatticeRepresentationOverflow ContextRelationWords requestedCount)
    Left (ContextLayoutLimitExceeded requestedBytes maximumBytes) ->
      Left
        ( ContextLatticeRepresentationLimitExceeded
            ContextRelationWords
            requestedBytes
            maximumBytes
        )
    Right wordCount -> Right wordCount

checkedPairCellCount ::
  ContextCompileLimits ->
  Int ->
  Maybe Int
checkedPairCellCount limits size =
  either
    (const Nothing)
    Just
    ( checkedRepresentationProduct
        (cclMaximumBinaryTableBytes limits)
        (sizeOf (0 :: Int))
        2
        [size, size]
    )

checkedRepresentationProduct ::
  Maybe Natural ->
  Int ->
  Integer ->
  [Int] ->
  Either ContextLayoutFailure Int
checkedRepresentationProduct byteLimit bytesPerElement retainedCopies factors =
  case countResult of
    Left overflow ->
      Left overflow
    Right tableEntryCount ->
      case limitResult tableEntryCount of
        Left limitError -> Left limitError
        Right () -> Right (fromInteger tableEntryCount)
  where
    requestedCount =
      product (fmap toInteger factors)

    countResult
      | requestedCount <= toInteger (maxBound :: Int) =
          Right requestedCount
      | otherwise =
          Left (ContextLayoutCountOverflow requestedCount)

    requestedBytes requestedCountValue =
      requestedCountValue
        * toInteger bytesPerElement
        * retainedCopies

    limitResult requestedCountValue =
      case byteLimit of
        Nothing ->
          Right ()
        Just maximumBytes
          | requestedBytes requestedCountValue <= toInteger maximumBytes ->
              Right ()
          | otherwise ->
              Left
                ( ContextLayoutLimitExceeded
                    (requestedBytes requestedCountValue)
                    maximumBytes
                )
