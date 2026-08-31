{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Closed vocabulary and invariant carriers for exact polygonal morphology.
module Moonlight.Triangulation.Internal.Minkowski.Types
  ( ConvexPolygon (..)
  , StructuringElement (..)
  , MinkowskiOperation (..)
  , MinkowskiError (..)
  , MinkowskiReceipt (..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Moonlight.Triangulation.Exact
  ( ExactClipError
  , ExactHalfPlaneError
  , ExactPoint
  )
import Moonlight.Triangulation.Internal.HandleDefs (FaceId)
import Moonlight.Triangulation.Internal.ExactRational (ExactArithmeticError)
import Moonlight.Triangulation.Internal.Overlay.Types
  ( OverlayCellId
  , OverlayCellWitness
  , OverlayError
  )
import Moonlight.Triangulation.Internal.Region.Types
  ( ExactLoop
  , RegionPublicationError
  , RegionPointLocation
  , RegionValidationError
  )

newtype ConvexPolygon = ConvexPolygon ExactLoop
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

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
  = MinkowskiInvalidConvexLoop !RegionValidationError
  | MinkowskiInvalidHalfPlane !ExactHalfPlaneError
  | MinkowskiClipFailed !ExactClipError
  | MinkowskiNonConvexTurn !Int !Ordering
  | MinkowskiOriginOutside !RegionPointLocation
  | MinkowskiExactArithmetic !ExactArithmeticError
  | MinkowskiOverlayFailed !(OverlayError Bool Bool)
  | MinkowskiPublicationFailed !RegionPublicationError
  | MinkowskiOverlayCellWitness !OverlayCellWitness
  | MinkowskiFaceArity !FaceId !Int
  | MinkowskiCandidateCellMissing !OverlayCellId
  | MinkowskiInclusionAmbiguous !OverlayCellId !ExactPoint
  | MinkowskiConvexHullDegenerate ![ExactPoint]
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
