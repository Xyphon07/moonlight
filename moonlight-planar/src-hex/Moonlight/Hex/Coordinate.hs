{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Global axial coordinates and the six translations of the hexagonal grid.
module Moonlight.Hex.Coordinate
  ( HexCoord (..)
  , HexDirection (..)
  , allHexDirections
  , oppositeHexDirection
  , hexDirectionDelta
  , hexStepCoord
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)

-- | A global axial coordinate. Layouts restrict which coordinates are present;
-- the coordinate itself remains stable across restrictions and gluing.
data HexCoord = HexCoord
  { hexQ :: !Int
  , hexR :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The six translations of the axial lattice.
data HexDirection
  = HexEast
  | HexNorthEast
  | HexNorthWest
  | HexWest
  | HexSouthWest
  | HexSouthEast
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

allHexDirections :: NonEmpty HexDirection
allHexDirections =
  HexEast :| [HexNorthEast, HexNorthWest, HexWest, HexSouthWest, HexSouthEast]

oppositeHexDirection :: HexDirection -> HexDirection
oppositeHexDirection direction = case direction of
  HexEast -> HexWest
  HexNorthEast -> HexSouthWest
  HexNorthWest -> HexSouthEast
  HexWest -> HexEast
  HexSouthWest -> HexNorthEast
  HexSouthEast -> HexNorthWest
{-# INLINE oppositeHexDirection #-}

hexDirectionDelta :: HexDirection -> HexCoord
hexDirectionDelta direction = case direction of
  HexEast -> HexCoord 1 0
  HexNorthEast -> HexCoord 1 (-1)
  HexNorthWest -> HexCoord 0 (-1)
  HexWest -> HexCoord (-1) 0
  HexSouthWest -> HexCoord (-1) 1
  HexSouthEast -> HexCoord 0 1
{-# INLINE hexDirectionDelta #-}

-- | Translate once, returning 'Nothing' only when the machine 'Int' boundary
-- would be crossed. Layout membership is deliberately a separate question.
hexStepCoord :: HexCoord -> HexDirection -> Maybe HexCoord
hexStepCoord (HexCoord q r) direction =
  let HexCoord dq dr = hexDirectionDelta direction
   in HexCoord <$> checkedUnitAdd q dq <*> checkedUnitAdd r dr
{-# INLINE hexStepCoord #-}

checkedUnitAdd :: Int -> Int -> Maybe Int
checkedUnitAdd value delta
  | delta > 0 && value == maxBound = Nothing
  | delta < 0 && value == minBound = Nothing
  | otherwise = Just (value + delta)
{-# INLINE checkedUnitAdd #-}
