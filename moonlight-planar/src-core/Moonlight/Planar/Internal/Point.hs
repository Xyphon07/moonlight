{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Coordinate values and finite-query admission, independent of mesh storage.
-- Trusted mesh owners may construct QueryPoint from already-admitted planes.
module Moonlight.Planar.Internal.Point where

import Control.DeepSeq (NFData (..))
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..), peekElemOff, pokeElemOff)
import GHC.Generics (Generic)
import Moonlight.Planar.Scalar
  ( CoordinateError, canonicalScalarZero, minimumAllowedCoordinate, validateCoordinate )

-- | Cartesian binary64 point.
data Point = Point
  { pointX :: !Double
  , pointY :: !Double
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A coordinate pair admitted to the exact-predicate domain and normalized
-- at its construction boundary. Query algorithms consume this phase rather
-- than each inventing a fallback for invalid floating-point input.
newtype QueryPoint = QueryPoint Point
  deriving stock (Eq, Ord, Show)

instance NFData QueryPoint where
  rnf (QueryPoint point) = rnf point
  {-# INLINE rnf #-}

-- | Read the admitted coordinate without exposing a record-update route around
-- finite-coordinate admission and canonical normalization.
queryPointValue :: QueryPoint -> Point
queryPointValue (QueryPoint point) = point
{-# INLINE queryPointValue #-}

-- | Coordinate axis and reason that kept a point outside the query domain.
data PointValidationError
  = InvalidPointX !CoordinateError
  | InvalidPointY !CoordinateError
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Storable (Point) where
  sizeOf _ = 2 * sizeOf (0 :: Double)
  alignment _ = alignment (0 :: Double)
  peek pointer = do
    x <- peekElemOff (castPtr pointer) 0
    y <- peekElemOff (castPtr pointer) 1
    pure (Point x y)
  poke pointer (Point x y) = do
    pokeElemOff (castPtr pointer) 0 x
    pokeElemOff (castPtr pointer) 1 y

-- | Extract a vertex's position once, at the construction boundary.
class HasPosition vertex where
  position :: vertex -> Point

instance HasPosition (Point) where
  position = id

-- | Admit and normalize a finite point for read-only geometric queries.
mkQueryPoint :: Point -> Either PointValidationError (QueryPoint)
mkQueryPoint point@(Point x y) = do
  maybe (Right ()) (Left . InvalidPointX) (validateCoordinate x)
  maybe (Right ()) (Left . InvalidPointY) (validateCoordinate y)
  Right (QueryPoint (canonicalPoint point))

-- | Round coordinates below the robust-predicate input floor toward zero.
-- The operation never changes a coordinate already accepted by
-- 'validateCoordinate'.
mitigateUnderflow :: Point -> Point
mitigateUnderflow (Point x y) = Point (mitigate x) (mitigate y)
 where
  mitigate :: Double -> Double
  mitigate value
    | value /= 0 && abs value < minimumAllowedCoordinate = 0
    | otherwise = value

-- | Canonicalize both coordinate components for point identity.
canonicalPoint :: Point -> Point
canonicalPoint (Point x y) = Point (canonicalScalarZero x) (canonicalScalarZero y)
{-# INLINE canonicalPoint #-}
