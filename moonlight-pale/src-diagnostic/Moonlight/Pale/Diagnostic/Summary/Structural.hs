{-# LANGUAGE DerivingStrategies #-}

{-| Whole-object structural and Grothendieck summaries. -}
module Moonlight.Pale.Diagnostic.Summary.Structural
  ( StructuralSummary (..),
    GrothendieckStructuralSummary (..),
  )
where

import Data.Kind (Type)
import Moonlight.Pale.Diagnostic.Topology.Cohomology (CoboundaryNilpotenceEvidence)
import Moonlight.Pale.Diagnostic.Topology.Homotopy (NerveHomotopyProfile)
import Prelude (Bool, Double, Eq, Int, Maybe, Read, Show)

type StructuralSummary :: Type
data StructuralSummary = StructuralSummary
  { ssConnectedComponents :: Int,
    ssBettiNumbers :: [Int],
    ssCellCount :: Int,
    ssRestrictionCount :: Int,
    ssCoboundaryNilpotent :: Bool,
    ssMicrosupportSize :: Maybe Int,
    ssCriticalCellCount :: Maybe Int,
    ssNoncriticalFraction :: Maybe Double
  }
  deriving stock (Eq, Show, Read)

type GrothendieckStructuralSummary :: Type
data GrothendieckStructuralSummary = GrothendieckStructuralSummary
  { gssHomotopyProfile :: NerveHomotopyProfile,
    gssCellCount :: Int,
    gssFaceCount :: Int,
    gssObjectCount :: Int,
    gssMorphismCount :: Int,
    gssCrossContextMorphismCount :: Int,
    gssVerticalMorphismCount :: Int,
    gssDiagonalMorphismCount :: Int,
    gssCoboundaryNilpotenceEvidence :: CoboundaryNilpotenceEvidence
  }
  deriving stock (Eq, Show, Read)
