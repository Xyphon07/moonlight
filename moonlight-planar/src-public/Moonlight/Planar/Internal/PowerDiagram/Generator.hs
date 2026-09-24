{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact affine generators and their dual geometry. This module owns the
-- algebra shared by batch hull construction, local edits, and bounded
-- projection; it does not choose a construction schedule.
module Moonlight.Planar.Internal.PowerDiagram.Generator
  ( ExactPowerGenerator (..)
  , DistinctSlopeGenerators (..)
  , RegularGeneratorDisposition (..)
  , GeneratorRegularFace (..)
  , PowerDualEdge (..)
  , GeneratorRegularEdge (..)
  , GeneratorRegularTopology (..)
  , GeneratorRegularReceipt (..)
  , RegularTopologyError (..)
  , exactGeneratorAxis
  , exactGeneratorLiftedVolume
  , exactGeneratorLiftedSign
  , exactGeneratorFaceDual
  , exactGeneratorBoundaryDualRay
  , exactGeneratorDualBetween
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Moonlight.Planar.Exact
  ( ExactAffineLine
  , ExactGeometryError
  , ExactHalfPlaneError
  , ExactIntersectionError
  , ExactPoint
  , ExactRay
  , ExactSegment
  , ExactVector (..)
  , exactAffineLine
  , exactAffineLineCoefficients
  , exactAffineLineIntersection
  , exactRay
  , exactSegment
  )
import Moonlight.Planar.Internal.ExactRational
  ( ExactRational
  , exactRationalDenominator
  , exactRationalNumerator
  )
import Moonlight.Planar.Convex (ConvexError)
data ExactPowerGenerator label = ExactPowerGenerator
  { exactPowerGeneratorLabel :: !label
  , exactPowerGeneratorXCoefficient :: !ExactRational
  , exactPowerGeneratorYCoefficient :: !ExactRational
  , exactPowerGeneratorConstant :: !ExactRational
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Package-internal witness that coincident affine slopes were resolved once.
newtype DistinctSlopeGenerators label =
  DistinctSlopeGenerators (NonEmpty (ExactPowerGenerator label))

data RegularGeneratorDisposition
  = RegularGeneratorVisible
  | RegularGeneratorLowerDimensional
  | RegularGeneratorHidden
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data GeneratorRegularFace label = GeneratorRegularFace
  { generatorRegularFaceFirst :: !label
  , generatorRegularFaceSecond :: !label
  , generatorRegularFaceThird :: !label
  , generatorRegularFaceDualPoint :: !ExactPoint
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Exact weighted Voronoi geometry dual to one regular edge. Generator
-- construction and public regular topology share this one carrier.
data PowerDualEdge
  = BoundedPowerDual !ExactSegment
  | UnboundedPowerDual !ExactRay
  | FullLinePowerDual !ExactAffineLine
  | CollapsedPowerDual !ExactPoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data GeneratorRegularEdge label = GeneratorRegularEdge
  { generatorRegularEdgeFirst :: !(ExactPowerGenerator label)
  , generatorRegularEdgeSecond :: !(ExactPowerGenerator label)
  , generatorRegularEdgeDual :: !PowerDualEdge
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data GeneratorRegularTopology label = GeneratorRegularTopology
  { generatorRegularDispositions :: !(NonEmpty (ExactPowerGenerator label, RegularGeneratorDisposition))
  , generatorRegularFaces :: ![GeneratorRegularFace label]
  , generatorRegularEdges :: ![GeneratorRegularEdge label]
  , generatorRegularReceipt :: !GeneratorRegularReceipt
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data GeneratorRegularReceipt = GeneratorRegularReceipt
  { generatorRegularInputSites :: !Int
  , generatorRegularVisibleSites :: !Int
  , generatorRegularLowerDimensionalSites :: !Int
  , generatorRegularHiddenSites :: !Int
  , generatorRegularFaceCount :: !Int
  , generatorRegularEdgeCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Exact slope-hull, incidence, and dual-construction obstructions.
data RegularTopologyError label
  = RegularTopologySlopeHullInvalid !ConvexError
  | RegularTopologySlopeVertexMissing !ExactPoint
  | RegularTopologyAxisInvalid !label !label !ExactHalfPlaneError
  | RegularTopologyDualVertexInvalid !label !label !label !ExactIntersectionError
  | RegularTopologyDualSegmentInvalid !label !label !ExactGeometryError
  | RegularTopologyDualRayInvalid !label !label !ExactGeometryError
  | RegularTopologyFlatBoundary !label !label !label
  | RegularTopologyNonManifoldEdge !label !label !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

exactGeneratorAxis
  :: ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> Either ExactHalfPlaneError ExactAffineLine
exactGeneratorAxis firstGenerator secondGenerator =
  exactAffineLine
    ( exactPowerGeneratorXCoefficient firstGenerator
        - exactPowerGeneratorXCoefficient secondGenerator
    )
    ( exactPowerGeneratorYCoefficient firstGenerator
        - exactPowerGeneratorYCoefficient secondGenerator
    )
    ( exactPowerGeneratorConstant firstGenerator
        - exactPowerGeneratorConstant secondGenerator
    )

-- | Signed exact lifted volume of four affine generators. For a
-- counter-clockwise slope face, a positive result lies above its supporting
-- plane and therefore belongs to the insertion conflict cavity.
exactGeneratorLiftedVolume
  :: ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> ExactRational
exactGeneratorLiftedVolume firstGenerator secondGenerator thirdGenerator fourthGenerator =
  let (firstX, firstY, firstZ) = generatorCoordinates firstGenerator
      (secondX, secondY, secondZ) = generatorCoordinates secondGenerator
      (thirdX, thirdY, thirdZ) = generatorCoordinates thirdGenerator
      (fourthX, fourthY, fourthZ) = generatorCoordinates fourthGenerator
      secondX' = secondX - firstX
      secondY' = secondY - firstY
      secondZ' = secondZ - firstZ
      thirdX' = thirdX - firstX
      thirdY' = thirdY - firstY
      thirdZ' = thirdZ - firstZ
      fourthX' = fourthX - firstX
      fourthY' = fourthY - firstY
      fourthZ' = fourthZ - firstZ
   in secondX' * (thirdY' * fourthZ' - thirdZ' * fourthY')
        - secondY' * (thirdX' * fourthZ' - thirdZ' * fourthX')
        + secondZ' * (thirdX' * fourthY' - thirdY' * fourthX')

-- | Sign of the lifted volume without normalizing rational intermediates.
-- Each homogeneous row @(1,x,y,z)@ is multiplied by its own positive least
-- common denominator. The determinant is therefore scaled by a positive
-- product and keeps the upper-affine-hull sign convention of
-- 'exactGeneratorLiftedVolume'. Magnitude consumers must retain that worker.
exactGeneratorLiftedSign
  :: ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> Ordering
exactGeneratorLiftedSign firstGenerator secondGenerator thirdGenerator fourthGenerator =
  let (firstD, firstX, firstY, firstZ) = integerRow firstGenerator
      (secondD, secondX, secondY, secondZ) = integerRow secondGenerator
      (thirdD, thirdX, thirdY, thirdZ) = integerRow thirdGenerator
      (fourthD, fourthX, fourthY, fourthZ) = integerRow fourthGenerator
      determinant =
        if firstD == 1
          then
            -- A unit leading entry eliminates the homogeneous column with
            -- no division or extra denominator factor. The six homogeneous
            -- cofactor products allocate more on integral input than the
            -- existing rational magnitude schedule.
            let secondX' = secondX - firstX * secondD
                secondY' = secondY - firstY * secondD
                secondZ' = secondZ - firstZ * secondD
                thirdX' = thirdX - firstX * thirdD
                thirdY' = thirdY - firstY * thirdD
                thirdZ' = thirdZ - firstZ * thirdD
                fourthX' = fourthX - firstX * fourthD
                fourthY' = fourthY - firstY * fourthD
                fourthZ' = fourthZ - firstZ * fourthD
             in secondX' * (thirdY' * fourthZ' - thirdZ' * fourthY')
                  - secondY' * (thirdX' * fourthZ' - thirdZ' * fourthX')
                  + secondZ' * (thirdX' * fourthY' - thirdY' * fourthX')
          else
            (firstD * secondX - firstX * secondD) * (thirdY * fourthZ - thirdZ * fourthY)
              - (firstD * secondY - firstY * secondD) * (thirdX * fourthZ - thirdZ * fourthX)
              + (firstD * secondZ - firstZ * secondD) * (thirdX * fourthY - thirdY * fourthX)
              + (firstX * secondY - firstY * secondX) * (thirdD * fourthZ - thirdZ * fourthD)
              - (firstX * secondZ - firstZ * secondX) * (thirdD * fourthY - thirdY * fourthD)
              + (firstY * secondZ - firstZ * secondY) * (thirdD * fourthX - thirdX * fourthD)
   in compare determinant 0
  where
    integerRow
      :: ExactPowerGenerator rowLabel
      -> (Integer, Integer, Integer, Integer)
    integerRow generator =
      let (coordinateX, coordinateY, coordinateZ) = generatorCoordinates generator
          denominatorX = exactRationalDenominator coordinateX
          denominatorY = exactRationalDenominator coordinateY
          denominatorZ = exactRationalDenominator coordinateZ
          denominator = lcm denominatorX (lcm denominatorY denominatorZ)
       in if denominatorX == 1 && denominatorY == 1 && denominatorZ == 1
            then
              ( 1
              , exactRationalNumerator coordinateX
              , exactRationalNumerator coordinateY
              , exactRationalNumerator coordinateZ
              )
            else
              ( denominator
              , exactRationalNumerator coordinateX * (denominator `quot` denominatorX)
              , exactRationalNumerator coordinateY * (denominator `quot` denominatorY)
              , exactRationalNumerator coordinateZ * (denominator `quot` denominatorZ)
              )
    {-# INLINE integerRow #-}

-- | Exact dual vertex shared by three affine generators.
exactGeneratorFaceDual
  :: ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> Either (RegularTopologyError label) ExactPoint
exactGeneratorFaceDual firstGenerator secondGenerator thirdGenerator = do
  firstAxis <-
    first
      (RegularTopologyAxisInvalid (exactPowerGeneratorLabel firstGenerator) (exactPowerGeneratorLabel secondGenerator))
      (exactGeneratorAxis firstGenerator secondGenerator)
  secondAxis <-
    first
      (RegularTopologyAxisInvalid (exactPowerGeneratorLabel firstGenerator) (exactPowerGeneratorLabel thirdGenerator))
      (exactGeneratorAxis firstGenerator thirdGenerator)
  first
    ( RegularTopologyDualVertexInvalid
        (exactPowerGeneratorLabel firstGenerator)
        (exactPowerGeneratorLabel secondGenerator)
        (exactPowerGeneratorLabel thirdGenerator)
    )
    (exactAffineLineIntersection firstAxis secondAxis)

-- | Exact outward dual ray of an oriented boundary edge and its incident
-- counter-clockwise face.
exactGeneratorBoundaryDualRay
  :: ExactPoint
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> Either (RegularTopologyError label) ExactRay
exactGeneratorBoundaryDualRay originPoint firstGenerator secondGenerator thirdGenerator = do
  axis <-
    first
      ( RegularTopologyAxisInvalid
          (exactPowerGeneratorLabel firstGenerator)
          (exactPowerGeneratorLabel secondGenerator)
      )
      (exactGeneratorAxis firstGenerator secondGenerator)
  let (axisX, axisY, _) = exactAffineLineCoefficients axis
      candidate = ExactVector axisY (negate axisX)
      advantageX =
        exactPowerGeneratorXCoefficient firstGenerator
          - exactPowerGeneratorXCoefficient thirdGenerator
      advantageY =
        exactPowerGeneratorYCoefficient firstGenerator
          - exactPowerGeneratorYCoefficient thirdGenerator
      ExactVector candidateX candidateY = candidate
      derivative = advantageX * candidateX + advantageY * candidateY
      direction =
        if derivative > 0
          then candidate
          else ExactVector (negate candidateX) (negate candidateY)
  if derivative == 0
    then
      Left
        ( RegularTopologyFlatBoundary
            (exactPowerGeneratorLabel firstGenerator)
            (exactPowerGeneratorLabel secondGenerator)
            (exactPowerGeneratorLabel thirdGenerator)
        )
    else
      first
        ( RegularTopologyDualRayInvalid
            (exactPowerGeneratorLabel firstGenerator)
            (exactPowerGeneratorLabel secondGenerator)
        )
        (exactRay originPoint direction)

-- | Canonical undirected dual geometry between two incident face vertices.
exactGeneratorDualBetween
  :: label
  -> label
  -> ExactPoint
  -> ExactPoint
  -> Either (RegularTopologyError label) PowerDualEdge
exactGeneratorDualBetween firstLabel secondLabel firstPoint secondPoint
  | firstPoint == secondPoint = Right (CollapsedPowerDual firstPoint)
  | otherwise =
      fmap BoundedPowerDual
        ( first
            (RegularTopologyDualSegmentInvalid firstLabel secondLabel)
            ( uncurry exactSegment
                ( if firstPoint <= secondPoint
                    then (firstPoint, secondPoint)
                    else (secondPoint, firstPoint)
                )
            )
        )

generatorCoordinates
  :: ExactPowerGenerator label
  -> (ExactRational, ExactRational, ExactRational)
generatorCoordinates generator =
  ( exactPowerGeneratorXCoefficient generator
  , exactPowerGeneratorYCoefficient generator
  , exactPowerGeneratorConstant generator
  )
