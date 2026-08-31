{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Canonical vertices and sides of native axial hexagonal cells.
module Moonlight.Hex.Element
  ( HexCorner (..)
  , HexVertex
  , hexVertexCoordinates
  , HexSide
  , hexSideEndpoints
  , HexBoundarySide
  , hexBoundaryDirection
  , hexBoundaryFrom
  , hexBoundaryTo
  , hexBoundaryIdentity
  , hexCellVertex
  , hexCellVertices
  , hexCellSide
  , hexCellSides
  , hexCellBoundarySide
  , hexCellBoundary
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)
import Moonlight.Hex.Coordinate (HexCoord (..), HexDirection (..))

-- | Corners in counter-clockwise boundary order.
data HexCorner
  = HexEastCorner
  | HexNorthEastCorner
  | HexNorthWestCorner
  | HexWestCorner
  | HexSouthWestCorner
  | HexSouthEastCorner
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | A vertex in the integer affine embedding of the global axial lattice.
data HexVertex = HexVertex !Integer !Integer
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

hexVertexCoordinates :: HexVertex -> (Integer, Integer)
hexVertexCoordinates (HexVertex x y) = (x, y)
{-# INLINE hexVertexCoordinates #-}

-- | An unoriented side with endpoints stored in canonical ascending order.
data HexSide = HexSide !HexVertex !HexVertex
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

hexSideEndpoints :: HexSide -> (HexVertex, HexVertex)
hexSideEndpoints (HexSide from to) = (from, to)
{-# INLINE hexSideEndpoints #-}

-- | One counter-clockwise cell-boundary side, together with the neighbouring
-- cell direction across it and its canonical unoriented identity.
data HexBoundarySide = HexBoundarySide
  !HexDirection
  !HexVertex
  !HexVertex
  !HexSide
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

hexBoundaryDirection :: HexBoundarySide -> HexDirection
hexBoundaryDirection (HexBoundarySide direction _ _ _) = direction
{-# INLINE hexBoundaryDirection #-}

hexBoundaryFrom :: HexBoundarySide -> HexVertex
hexBoundaryFrom (HexBoundarySide _ from _ _) = from
{-# INLINE hexBoundaryFrom #-}

hexBoundaryTo :: HexBoundarySide -> HexVertex
hexBoundaryTo (HexBoundarySide _ _ to _) = to
{-# INLINE hexBoundaryTo #-}

hexBoundaryIdentity :: HexBoundarySide -> HexSide
hexBoundaryIdentity (HexBoundarySide _ _ _ side) = side
{-# INLINE hexBoundaryIdentity #-}

hexCellVertex :: HexCoord -> HexCorner -> HexVertex
hexCellVertex (HexCoord q r) corner =
  let centerX = 3 * toInteger q
      centerY = 2 * toInteger r + toInteger q
      (offsetX, offsetY) = cornerOffset corner
   in HexVertex (centerX + offsetX) (centerY + offsetY)
{-# INLINE hexCellVertex #-}

hexCellVertices :: HexCoord -> NonEmpty HexVertex
hexCellVertices coordinate =
  hexCellVertex coordinate HexEastCorner
    :| [ hexCellVertex coordinate HexNorthEastCorner
       , hexCellVertex coordinate HexNorthWestCorner
       , hexCellVertex coordinate HexWestCorner
       , hexCellVertex coordinate HexSouthWestCorner
       , hexCellVertex coordinate HexSouthEastCorner
       ]

-- | The side shared with the cell in the given direction.
hexCellSide :: HexCoord -> HexDirection -> HexSide
hexCellSide coordinate direction =
  let (fromCorner, toCorner) = directionCorners direction
   in canonicalSide
        (hexCellVertex coordinate fromCorner)
        (hexCellVertex coordinate toCorner)
{-# INLINE hexCellSide #-}

hexCellSides :: HexCoord -> NonEmpty HexSide
hexCellSides coordinate =
  hexCellSide coordinate HexEast
    :| [ hexCellSide coordinate HexNorthEast
       , hexCellSide coordinate HexNorthWest
       , hexCellSide coordinate HexWest
       , hexCellSide coordinate HexSouthWest
       , hexCellSide coordinate HexSouthEast
       ]

hexCellBoundarySide :: HexCoord -> HexDirection -> HexBoundarySide
hexCellBoundarySide coordinate direction =
  let (fromCorner, toCorner) = boundaryDirectionCorners direction
      from = hexCellVertex coordinate fromCorner
      to = hexCellVertex coordinate toCorner
   in HexBoundarySide direction from to (canonicalSide from to)
{-# INLINE hexCellBoundarySide #-}

hexCellBoundary :: HexCoord -> NonEmpty HexBoundarySide
hexCellBoundary coordinate =
  hexCellBoundarySide coordinate HexEast
    :| [ hexCellBoundarySide coordinate HexSouthEast
       , hexCellBoundarySide coordinate HexSouthWest
       , hexCellBoundarySide coordinate HexWest
       , hexCellBoundarySide coordinate HexNorthWest
       , hexCellBoundarySide coordinate HexNorthEast
       ]

cornerOffset :: HexCorner -> (Integer, Integer)
cornerOffset corner = case corner of
  HexEastCorner -> (2, 0)
  HexNorthEastCorner -> (1, 1)
  HexNorthWestCorner -> (-1, 1)
  HexWestCorner -> (-2, 0)
  HexSouthWestCorner -> (-1, -1)
  HexSouthEastCorner -> (1, -1)
{-# INLINE cornerOffset #-}

directionCorners :: HexDirection -> (HexCorner, HexCorner)
directionCorners direction = case direction of
  HexEast -> (HexEastCorner, HexNorthEastCorner)
  HexNorthEast -> (HexSouthEastCorner, HexEastCorner)
  HexNorthWest -> (HexSouthWestCorner, HexSouthEastCorner)
  HexWest -> (HexWestCorner, HexSouthWestCorner)
  HexSouthWest -> (HexNorthWestCorner, HexWestCorner)
  HexSouthEast -> (HexNorthEastCorner, HexNorthWestCorner)
{-# INLINE directionCorners #-}

boundaryDirectionCorners :: HexDirection -> (HexCorner, HexCorner)
boundaryDirectionCorners direction = case direction of
  HexEast -> (HexEastCorner, HexNorthEastCorner)
  HexSouthEast -> (HexNorthEastCorner, HexNorthWestCorner)
  HexSouthWest -> (HexNorthWestCorner, HexWestCorner)
  HexWest -> (HexWestCorner, HexSouthWestCorner)
  HexNorthWest -> (HexSouthWestCorner, HexSouthEastCorner)
  HexNorthEast -> (HexSouthEastCorner, HexEastCorner)
{-# INLINE boundaryDirectionCorners #-}

canonicalSide :: HexVertex -> HexVertex -> HexSide
canonicalSide left right =
  if left <= right then HexSide left right else HexSide right left
{-# INLINE canonicalSide #-}
