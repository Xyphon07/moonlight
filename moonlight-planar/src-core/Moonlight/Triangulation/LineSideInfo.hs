-- | Which side of a directed line a point falls on, as a total three-valued
-- answer that carries the collinear case rather than rounding it away.
module Moonlight.Triangulation.LineSideInfo
  ( LineSideInfo
  , fromDeterminant
  , fromOrdering
  , sideOrdering
  , isOnLeftSide
  , isOnRightSide
  , isOnLine
  , isOnLeftSideOrLine
  , isOnRightSideOrLine
  , reverseSide
  ) where

-- | A three-valued side: the collinear case is a value, not a rounding.
newtype LineSideInfo = LineSideInfo Ordering
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | The side an orientation determinant's sign names.
fromDeterminant :: Double -> LineSideInfo
fromDeterminant value = LineSideInfo (compare value 0)
{-# INLINE fromDeterminant #-}

-- | The side an 'Ordering' names.
fromOrdering :: Ordering -> LineSideInfo
fromOrdering = LineSideInfo
{-# INLINE fromOrdering #-}

-- | The underlying 'Ordering'.
sideOrdering :: LineSideInfo -> Ordering
sideOrdering (LineSideInfo ordering) = ordering
{-# INLINE sideOrdering #-}

-- | The five side tests; the @OrLine@ pair admit the collinear case.
isOnLeftSide, isOnRightSide, isOnLine, isOnLeftSideOrLine, isOnRightSideOrLine :: LineSideInfo -> Bool
isOnLeftSide (LineSideInfo ordering) = ordering == GT
isOnRightSide (LineSideInfo ordering) = ordering == LT
isOnLine (LineSideInfo ordering) = ordering == EQ
isOnLeftSideOrLine side = not (isOnRightSide side)
isOnRightSideOrLine side = not (isOnLeftSide side)
{-# INLINE isOnLeftSide #-}
{-# INLINE isOnRightSide #-}
{-# INLINE isOnLine #-}
{-# INLINE isOnLeftSideOrLine #-}
{-# INLINE isOnRightSideOrLine #-}

-- | The same point, seen along the reversed line.
reverseSide :: LineSideInfo -> LineSideInfo
reverseSide (LineSideInfo ordering) = LineSideInfo $ case ordering of
  LT -> GT
  EQ -> EQ
  GT -> LT
{-# INLINE reverseSide #-}
