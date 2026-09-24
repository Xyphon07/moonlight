{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact weighted alpha filtration of a full-dimensional regular
-- subdivision. Births are signed power radii; Homology lowering remains in
-- @moonlight-planar:cell-complex@.
module Moonlight.Planar.RegularAlpha
  ( PowerAlphaBirth
  , powerAlphaBirthExact
  , powerAlphaBirthNumerator
  , powerAlphaBirthDenominator
  , RegularAlphaFiltration
  , RegularAlphaError (..)
  , regularAlphaFiltration
  , regularAlphaComplex
  , regularAlphaBirths
  , regularAlphaSimplexBirth
  , regularAlphaCriticalBirths
  , regularAlphaComplexAtBirth
  )
where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Moonlight.Planar.Exact
  ( ExactAffineLine
  , ExactArithmeticError
  , ExactPoint
  , ExactRational
  , ExactVector (..)
  , exactAffineLineCoefficients
  , exactDivide
  , exactPoint
  , exactPointCoordinates
  , exactRayDirection
  , exactRayOrigin
  , exactRationalDenominator
  , exactRationalNumerator
  , exactSegmentEndpoints
  )
import Moonlight.Planar.PowerDiagram
  ( PowerDualEdge (..)
  , PowerSite
  , RegularEdge
  , RegularFace
  , RegularSiteDisposition (..)
  , RegularTriangulation
  , powerSiteExactPosition
  , powerSiteLabel
  , powerSiteWeight
  , powerWeightExact
  , regularEdgeDual
  , regularEdgeLabels
  , regularEdges
  , regularFaceDualPoint
  , regularFaceLabels
  , regularFaces
  , regularNeighbours
  , regularSite
  , regularSiteDisposition
  , regularSites
  )
import Moonlight.Planar.Simplex
  ( PlanarComplex
  , PlanarComplexError
  , PlanarSimplex
  , PlanarSimplexError
  , planarComplex
  , planarEdge
  , planarFace
  , planarSimplexVertices
  , planarVertex
  )

-- | Signed exact power radius. Unlike ordinary alpha birth, negative values
-- are lawful when a positive site weight already contains the simplex at
-- negative power level.
newtype PowerAlphaBirth = PowerAlphaBirth ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Recover the reduced exact power radius.
powerAlphaBirthExact :: PowerAlphaBirth -> ExactRational
powerAlphaBirthExact (PowerAlphaBirth value) = value

-- | Numerator of the reduced exact power radius.
powerAlphaBirthNumerator :: PowerAlphaBirth -> Integer
powerAlphaBirthNumerator = exactRationalNumerator . powerAlphaBirthExact

-- | Positive denominator of the reduced exact power radius.
powerAlphaBirthDenominator :: PowerAlphaBirth -> Integer
powerAlphaBirthDenominator = exactRationalDenominator . powerAlphaBirthExact

-- | One admitted labelled regular complex and its total exact birth section.
data RegularAlphaFiltration label = RegularAlphaFiltration
  { regularAlphaComplex :: !(PlanarComplex label)
    -- ^ Canonical full-dimensional regular complex.
  , regularAlphaBirths :: !(Map (PlanarSimplex label) PowerAlphaBirth)
    -- ^ Total exact birth section over that complex.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Typed topology, dual-consistency, projection, and sublevel obstructions.
data RegularAlphaError label
  = RegularAlphaSiteMissing !label
  | RegularAlphaSimplexInvalid !(PlanarSimplexError label)
  | RegularAlphaComplexInvalid !(PlanarComplexError label)
  | RegularAlphaDualMismatch
      !(PlanarSimplex label)
      !ExactRational
      !ExactRational
  | RegularAlphaProjectionFailed
      !(PlanarSimplex label)
      !ExactArithmeticError
  | RegularAlphaVertexBirthMissing !label
  | RegularAlphaSublevelInvalid !(PlanarComplexError label)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Derive one exact weighted-alpha birth section from the resident regular dual.
regularAlphaFiltration
  :: Ord label
  => RegularTriangulation label
  -> Either (RegularAlphaError label) (RegularAlphaFiltration label)
regularAlphaFiltration triangulation = do
  let visibleLabels =
        Set.fromAscList
          [ powerSiteLabel site
          | site <- regularSites triangulation
          , regularSiteDisposition (powerSiteLabel site) triangulation
              == Just RegularSiteVisible
          ]
      visibleEdges = filter (edgeIsVisible visibleLabels) (regularEdges triangulation)
      visibleFaces = filter (faceIsVisible visibleLabels) (regularFaces triangulation)
  edgeSimplices <- traverse regularEdgeSimplex visibleEdges
  faceSimplices <- traverse regularFaceSimplex visibleFaces
  complexValue <-
    first RegularAlphaComplexInvalid
      ( planarComplex
          ( Set.unions
              [ Set.map planarVertex visibleLabels
              , Set.fromList edgeSimplices
              , Set.fromList faceSimplices
              ]
          )
      )
  faceBirthSection <-
    Map.fromList <$> traverse (regularFaceBirth triangulation) visibleFaces
  edgeBirthEntries <- traverse (regularEdgeBirth triangulation) visibleEdges
  let edgeBirthSection = Map.fromList edgeBirthEntries
      incidentEdgeBirths =
        Map.fromListWith min (edgeBirthEntries >>= incidentEdgeBirthsFor)
  vertexBirthSection <-
    Map.fromList
      <$> traverse
        (regularVertexBirth triangulation incidentEdgeBirths)
        (Set.toAscList visibleLabels)
  pure
    RegularAlphaFiltration
      { regularAlphaComplex = complexValue
      , regularAlphaBirths =
          vertexBirthSection <> edgeBirthSection <> faceBirthSection
      }

incidentEdgeBirthsFor
  :: (PlanarSimplex label, PowerAlphaBirth)
  -> [(label, PowerAlphaBirth)]
incidentEdgeBirthsFor (simplex, birthValue) =
  case NonEmpty.toList (planarSimplexVertices simplex) of
    [firstLabel, secondLabel] ->
      [ (firstLabel, birthValue)
      , (secondLabel, birthValue)
        ]
    _ -> []

-- | Look up the exact birth of one admitted simplex.
regularAlphaSimplexBirth
  :: Ord label
  => PlanarSimplex label
  -> RegularAlphaFiltration label
  -> Maybe PowerAlphaBirth
regularAlphaSimplexBirth simplex = Map.lookup simplex . regularAlphaBirths

-- | Distinct births in ascending exact order.
regularAlphaCriticalBirths
  :: RegularAlphaFiltration label
  -> [PowerAlphaBirth]
regularAlphaCriticalBirths =
  Set.toAscList . Set.fromList . Map.elems . regularAlphaBirths

-- | Reconstruct the closed subcomplex born no later than the threshold.
regularAlphaComplexAtBirth
  :: Ord label
  => PowerAlphaBirth
  -> RegularAlphaFiltration label
  -> Either (RegularAlphaError label) (PlanarComplex label)
regularAlphaComplexAtBirth threshold filtration =
  first RegularAlphaSublevelInvalid
    ( planarComplex
        ( Map.keysSet
            (Map.filter (<= threshold) (regularAlphaBirths filtration))
        )
    )

edgeIsVisible :: Ord label => Set label -> RegularEdge label -> Bool
edgeIsVisible visible edge =
  let (firstLabel, secondLabel) = regularEdgeLabels edge
   in Set.member firstLabel visible && Set.member secondLabel visible

faceIsVisible :: Ord label => Set label -> RegularFace label -> Bool
faceIsVisible visible face =
  let (firstLabel, secondLabel, thirdLabel) = regularFaceLabels face
   in all (`Set.member` visible) [firstLabel, secondLabel, thirdLabel]

regularEdgeSimplex
  :: Ord label
  => RegularEdge label
  -> Either (RegularAlphaError label) (PlanarSimplex label)
regularEdgeSimplex edge =
  let (firstLabel, secondLabel) = regularEdgeLabels edge
   in first RegularAlphaSimplexInvalid (planarEdge firstLabel secondLabel)

regularFaceSimplex
  :: Ord label
  => RegularFace label
  -> Either (RegularAlphaError label) (PlanarSimplex label)
regularFaceSimplex face =
  let (firstLabel, secondLabel, thirdLabel) = regularFaceLabels face
   in first RegularAlphaSimplexInvalid
        (planarFace firstLabel secondLabel thirdLabel)

regularFaceBirth
  :: Ord label
  => RegularTriangulation label
  -> RegularFace label
  -> Either (RegularAlphaError label) (PlanarSimplex label, PowerAlphaBirth)
regularFaceBirth triangulation face = do
  simplex <- regularFaceSimplex face
  let (firstLabel, secondLabel, thirdLabel) = regularFaceLabels face
      dualPoint = regularFaceDualPoint face
  firstSite <- requireRegularSite triangulation firstLabel
  secondSite <- requireRegularSite triangulation secondLabel
  thirdSite <- requireRegularSite triangulation thirdLabel
  let firstBirth = powerDistance firstSite dualPoint
      secondBirth = powerDistance secondSite dualPoint
      thirdBirth = powerDistance thirdSite dualPoint
  requireEqualDual simplex firstBirth secondBirth
  requireEqualDual simplex firstBirth thirdBirth
  pure (simplex, PowerAlphaBirth firstBirth)

regularEdgeBirth
  :: Ord label
  => RegularTriangulation label
  -> RegularEdge label
  -> Either (RegularAlphaError label) (PlanarSimplex label, PowerAlphaBirth)
regularEdgeBirth triangulation edge = do
  simplex <- regularEdgeSimplex edge
  let (firstLabel, secondLabel) = regularEdgeLabels edge
  firstSite <- requireRegularSite triangulation firstLabel
  secondSite <- requireRegularSite triangulation secondLabel
  minimizingPoint <- minimizePowerOnDual simplex firstSite (regularEdgeDual edge)
  let firstBirth = powerDistance firstSite minimizingPoint
      secondBirth = powerDistance secondSite minimizingPoint
  requireEqualDual simplex firstBirth secondBirth
  pure (simplex, PowerAlphaBirth firstBirth)

regularVertexBirth
  :: Ord label
  => RegularTriangulation label
  -> Map label PowerAlphaBirth
  -> label
  -> Either (RegularAlphaError label) (PlanarSimplex label, PowerAlphaBirth)
regularVertexBirth triangulation incidentBirths label = do
  owner <- requireRegularSite triangulation label
  competitors <-
    traverse (requireRegularSite triangulation) (Set.toAscList (regularNeighbours label triangulation))
  let ownerPoint = powerSiteExactPosition owner
      ownerBirth = powerDistance owner ownerPoint
  if all ((ownerBirth <=) . (`powerDistance` ownerPoint)) competitors
    then Right (planarVertex label, PowerAlphaBirth ownerBirth)
    else
      case Map.lookup label incidentBirths of
        Just birthValue -> Right (planarVertex label, birthValue)
        Nothing -> Left (RegularAlphaVertexBirthMissing label)

minimizePowerOnDual
  :: PlanarSimplex label
  -> PowerSite label
  -> PowerDualEdge
  -> Either (RegularAlphaError label) ExactPoint
minimizePowerOnDual simplex site dual =
  case dual of
    BoundedPowerDual segment -> do
      let (origin, terminal) = exactSegmentEndpoints segment
      projected <- projectAlong simplex (powerSiteExactPosition site) origin (differenceVector origin terminal)
      pure (pointAtClampedParameter 0 1 origin (differenceVector origin terminal) projected)
    UnboundedPowerDual ray -> do
      let origin = exactRayOrigin ray
          direction = exactRayDirection ray
      projected <- projectAlong simplex (powerSiteExactPosition site) origin direction
      pure (pointAtParameter origin direction (max 0 projected))
    FullLinePowerDual line -> projectOntoLine simplex (powerSiteExactPosition site) line
    CollapsedPowerDual point -> Right point

projectAlong
  :: PlanarSimplex label
  -> ExactPoint
  -> ExactPoint
  -> ExactVector
  -> Either (RegularAlphaError label) ExactRational
projectAlong simplex query origin direction =
  let (queryX, queryY) = exactPointCoordinates query
      (originX, originY) = exactPointCoordinates origin
      ExactVector directionX directionY = direction
      numerator = (queryX - originX) * directionX + (queryY - originY) * directionY
      denominator = directionX * directionX + directionY * directionY
   in first (RegularAlphaProjectionFailed simplex)
        (exactDivide numerator denominator)

pointAtClampedParameter
  :: ExactRational
  -> ExactRational
  -> ExactPoint
  -> ExactVector
  -> ExactRational
  -> ExactPoint
pointAtClampedParameter lower upper origin direction parameter =
  pointAtParameter origin direction (max lower (min upper parameter))

pointAtParameter :: ExactPoint -> ExactVector -> ExactRational -> ExactPoint
pointAtParameter origin (ExactVector directionX directionY) parameter =
  let (originX, originY) = exactPointCoordinates origin
   in exactPoint
        (originX + parameter * directionX)
        (originY + parameter * directionY)

projectOntoLine
  :: PlanarSimplex label
  -> ExactPoint
  -> ExactAffineLine
  -> Either (RegularAlphaError label) ExactPoint
projectOntoLine simplex query line = do
  let (normalX, normalY, constant) = exactAffineLineCoefficients line
      (queryX, queryY) = exactPointCoordinates query
      lineValue = normalX * queryX + normalY * queryY + constant
      squaredNormal = normalX * normalX + normalY * normalY
  displacement <-
    first (RegularAlphaProjectionFailed simplex)
      (exactDivide lineValue squaredNormal)
  pure
    ( exactPoint
        (queryX - displacement * normalX)
        (queryY - displacement * normalY)
    )

powerDistance :: PowerSite label -> ExactPoint -> ExactRational
powerDistance site point =
  let (pointX, pointY) = exactPointCoordinates point
      (siteX, siteY) = exactPointCoordinates (powerSiteExactPosition site)
      deltaX = pointX - siteX
      deltaY = pointY - siteY
   in deltaX * deltaX + deltaY * deltaY - powerWeightExact (powerSiteWeight site)

differenceVector :: ExactPoint -> ExactPoint -> ExactVector
differenceVector from to =
  let (fromX, fromY) = exactPointCoordinates from
      (toX, toY) = exactPointCoordinates to
   in ExactVector (toX - fromX) (toY - fromY)

requireRegularSite
  :: Ord label
  => RegularTriangulation label
  -> label
  -> Either (RegularAlphaError label) (PowerSite label)
requireRegularSite triangulation label =
  maybe (Left (RegularAlphaSiteMissing label)) Right (regularSite label triangulation)

requireEqualDual
  :: PlanarSimplex label
  -> ExactRational
  -> ExactRational
  -> Either (RegularAlphaError label) ()
requireEqualDual simplex expected actual
  | expected == actual = Right ()
  | otherwise = Left (RegularAlphaDualMismatch simplex expected actual)
