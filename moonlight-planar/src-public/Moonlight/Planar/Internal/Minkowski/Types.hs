{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Closed vocabulary and invariant carriers for exact polygonal morphology.
module Moonlight.Planar.Internal.Minkowski.Types
  ( StructuringElement (..)
  , MinkowskiOperation (..)
  , MinkowskiError (..)
  , MinkowskiReceipt (..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Moonlight.Planar.Convex (ConvexError, ConvexPolygon)
import Moonlight.Planar.Exact
  ( ExactClipError
  , ExactHalfPlaneError
  , ExactPoint
  )
import Moonlight.Planar.Internal.HandleDefs (FaceId)
import Moonlight.Planar.Internal.Overlay.Types (OverlayError)
import Moonlight.Planar.Internal.Region.Types
  ( RegionPublicationError
  , RegionPointLocation
  )

newtype StructuringElement = StructuringElement ConvexPolygon
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data MinkowskiOperation
  = MinkowskiAddition
  | MinkowskiErosion
  | MinkowskiOpening
  | MinkowskiClosing
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data MinkowskiError
  = MinkowskiConvexFailed !ConvexError
  | MinkowskiInvalidHalfPlane !ExactHalfPlaneError
  | MinkowskiClipFailed !ExactClipError
  | MinkowskiOriginOutside !RegionPointLocation
  | MinkowskiOverlayFailed !OverlayError
  | MinkowskiPublicationFailed !RegionPublicationError
  | MinkowskiInclusionAmbiguous !FaceId !ExactPoint
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data MinkowskiReceipt = MinkowskiReceipt
  { minkowskiOperation :: !MinkowskiOperation
  , minkowskiInputComponents :: !Int
  , minkowskiConvexPieces :: !Int
  , minkowskiGeneratedPieces :: !Int
  , minkowskiGeneratedConvolutionEdges :: !Int
  , minkowskiOverlayPasses :: !Int
  , minkowskiExactCrossings :: !Int
  , minkowskiOutputCells :: !Int
  , minkowskiExactCoordinateBitGrowth :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)
