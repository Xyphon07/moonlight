{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact alpha births over one resident Delaunay triangulation. Geometry
-- remains owned by the DCEL and 'ExactCellSet'; this module supplies the
-- handle-indexed filtration section used by topology consumers.
module Moonlight.Triangulation.Alpha
  ( AlphaBirth
  , alphaBirthNumerator
  , alphaBirthDenominator
  , alphaBirthToDouble
  , alphaBirthFromRadiusSquared
  , AlphaFiltration
  , AlphaFiltrationError (..)
  , alphaFiltration
  , alphaFiltrationCellSet
  , alphaVertexBirth
  , alphaEdgeBirth
  , alphaFaceBirth
  , alphaFiltrationCriticalBirths
  , alphaComplexAtBirth
  , alphaComplexAtRadius
  , alphaShapeContainsFace
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Bits (shiftL)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import GHC.Generics (Generic)
import Moonlight.Triangulation.CellSet
  ( CellSelectionError
  , exactCellSet
  )
import Moonlight.Triangulation.Dcel qualified as Dcel
import Moonlight.Triangulation.Internal.HandleDefs
  ( FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  , directedPair
  , faceIdIndex
  , undirectedEdgeIdIndex
  , vertexIdIndex
  )
import Moonlight.Triangulation.Internal.CellSet
  ( ExactCellSet (..)
  , residentExactCellSet
  )
import Moonlight.Triangulation.Internal.Dyadic
  ( exactCircumradiusSquaredRational
  , exactQuarterSquaredDistanceRational
  , integerBitLength
  , integerRatioToDouble
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactArithmeticError (..)
  , ExactRational
  , exactRationalDenominator
  , exactRationalFromFiniteDouble
  , exactRationalNumerator
  )
import Moonlight.Triangulation.Internal.Representation
  ( DelaunayTriangulation
  , Triangulation
  )
import Moonlight.Triangulation.Internal.Predicates (inDiametralCircle)
import Moonlight.Triangulation.Scalar (circumradiusSquaredWithinCoordinates)
import Moonlight.Triangulation.Types
  ( ConstraintMode (Unconstrained)
  , Point (..)
  , RadiusSquared
  , radiusSquaredValue
  )

-- | One exact squared-radius birth. The constructor is withheld so negative
-- exact rationals cannot masquerade as geometric radii.
data AlphaBirth = AlphaBirth !AlphaOrderKey !ExactRational
  deriving stock (Generic)
  deriving anyclass (NFData)

data AlphaOrderKey
  = NonPositiveAlphaOrder
  | PositiveAlphaOrder !Int !Word64
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Eq AlphaBirth where
  AlphaBirth _ leftValue == AlphaBirth _ rightValue = leftValue == rightValue

instance Ord AlphaBirth where
  compare (AlphaBirth leftKey leftValue) (AlphaBirth rightKey rightValue) =
    case compare leftKey rightKey of
      EQ -> compare leftValue rightValue
      distinctKeyOrder -> distinctKeyOrder

instance Show AlphaBirth where
  showsPrec precedence (AlphaBirth _ value) =
    showParen
      (precedence > 10)
      (showString "AlphaBirth " . showsPrec 11 value)

-- | Numerator of the reduced exact squared-radius birth.
alphaBirthNumerator :: AlphaBirth -> Integer
alphaBirthNumerator (AlphaBirth _ value) = exactRationalNumerator value

-- | Positive denominator of the reduced exact squared-radius birth.
alphaBirthDenominator :: AlphaBirth -> Integer
alphaBirthDenominator (AlphaBirth _ value) = exactRationalDenominator value

-- | Binary64 display projection. Equality and filtration order must use the
-- 'AlphaBirth' itself, not this rounded view.
alphaBirthToDouble :: AlphaBirth -> Double
alphaBirthToDouble birth =
  integerRatioToDouble
    (alphaBirthNumerator birth)
    (alphaBirthDenominator birth)

-- | Embed an admitted binary64 threshold into the exact alpha order without
-- loss. Every finite binary64 value is a dyadic rational.
alphaBirthFromRadiusSquared :: RadiusSquared -> AlphaBirth
alphaBirthFromRadiusSquared =
  alphaBirthFromExact . exactRationalFromFiniteDouble . radiusSquaredValue

-- | The exact handle-indexed filtration of one Delaunay DCEL. The cell set is
-- the only geometry inventory; the maps are derived birth sections over those
-- resident handles.
data AlphaFiltration = AlphaFiltration
  { alphaFiltrationCells :: !ExactCellSet
  , alphaVertexBirths :: !(Vector AlphaBirth)
  , alphaEdgeBirths :: !(Vector AlphaBirth)
  , alphaFaceBirths :: !(Vector AlphaBirth)
  }

data FaceAlphaSection = FaceAlphaSection
  { faceAlphaBirth :: !AlphaBirth
  , faceAlphaVertices :: !(VertexId, VertexId, VertexId)
  }

-- | Typed obstruction to deriving the exact birth section from a resident
-- Delaunay triangulation. Every incidence failure retains the affected handle.
data AlphaFiltrationError
  = AlphaFiltrationCellSelection !CellSelectionError
  | AlphaFaceVerticesUnavailable !FaceId
  | AlphaFaceDegenerate !FaceId
  | AlphaCircumradiusDivisionFailed !FaceId !ExactArithmeticError
  | AlphaFaceEdgeIncidenceInvalid !FaceId !UndirectedEdgeId ![VertexId]
  | AlphaIncidentFaceBirthMissing !UndirectedEdgeId !FaceId
  | AlphaEdgeBirthWitnessMissing !UndirectedEdgeId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Construct every exact alpha birth once. Faces descend first because a
-- non-Gabriel edge is born at the least incident-face circumradius.
alphaFiltration
  :: DelaunayTriangulation vertex
  -> Either AlphaFiltrationError AlphaFiltration
alphaFiltration triangulation = do
  cellSet <-
    first AlphaFiltrationCellSelection
      (residentExactCellSet triangulation)
  let vertexBirths = Vector.replicate (Dcel.numVertices triangulation) zeroAlphaBirth
  faceSections <-
    Vector.imapM
      (\faceIndex -> exactFaceSection triangulation (FaceId (fromIntegral (faceIndex + 1))))
      (Dcel.innerFaceVertexTriples triangulation)
  edgeBirths <-
    Vector.generateM
      (Dcel.numUndirectedEdges triangulation)
      (exactEdgeBirth faceSections triangulation . UndirectedEdgeId . fromIntegral)
  let faceBirths = fmap faceAlphaBirth faceSections
  pure
    AlphaFiltration
      { alphaFiltrationCells = cellSet
      , alphaVertexBirths = vertexBirths
      , alphaEdgeBirths = edgeBirths
      , alphaFaceBirths = faceBirths
      }

-- | The authoritative resident cells underlying this filtration. This is a
-- read-only projection rather than an exported record field, so callers cannot
-- splice births from one triangulation onto the cells of another.
alphaFiltrationCellSet :: AlphaFiltration -> ExactCellSet
alphaFiltrationCellSet = alphaFiltrationCells

-- | Exact birth of a resident vertex, when the handle belongs to this
-- filtration.
alphaVertexBirth :: AlphaFiltration -> VertexId -> Maybe AlphaBirth
alphaVertexBirth filtration vertex =
  alphaVertexBirths filtration Vector.!? vertexIdIndex vertex

-- | Exact birth of a resident undirected edge, when the handle belongs to this
-- filtration.
alphaEdgeBirth :: AlphaFiltration -> UndirectedEdgeId -> Maybe AlphaBirth
alphaEdgeBirth filtration edge =
  alphaEdgeBirths filtration Vector.!? undirectedEdgeIdIndex edge

-- | Exact birth of a resident bounded face, when the handle belongs to this
-- filtration.
alphaFaceBirth :: AlphaFiltration -> FaceId -> Maybe AlphaBirth
alphaFaceBirth filtration =
  faceBirthAt (alphaFaceBirths filtration)

-- | Distinct critical births in exact ascending order.
alphaFiltrationCriticalBirths :: AlphaFiltration -> [AlphaBirth]
alphaFiltrationCriticalBirths filtration =
  Set.toAscList
    ( Set.fromList
        ( Vector.toList (alphaVertexBirths filtration)
            <> Vector.toList (alphaEdgeBirths filtration)
            <> Vector.toList (alphaFaceBirths filtration)
        )
    )

-- | Select the closed alpha subcomplex at one exact birth. The resident DCEL
-- is retained; only its downward-closed cell section changes.
alphaComplexAtBirth
  :: AlphaBirth
  -> AlphaFiltration
  -> Either CellSelectionError ExactCellSet
alphaComplexAtBirth threshold filtration =
  case alphaFiltrationCells filtration of
    ExactCellSet triangulation _ _ _ ->
      exactCellSet
        triangulation
        (selectedHandles (VertexId . fromIntegral) (alphaVertexBirths filtration))
        (selectedHandles (UndirectedEdgeId . fromIntegral) (alphaEdgeBirths filtration))
        (selectedHandles (FaceId . fromIntegral . (+ 1)) (alphaFaceBirths filtration))
 where
  selectedHandles :: (Int -> handle) -> Vector AlphaBirth -> [handle]
  selectedHandles handleAt =
    Vector.ifoldr
      (\indexValue birthValue selected -> if birthValue <= threshold then handleAt indexValue : selected else selected)
      []

alphaComplexAtRadius
  :: RadiusSquared
  -> AlphaFiltration
  -> Either CellSelectionError ExactCellSet
alphaComplexAtRadius radius =
  alphaComplexAtBirth (alphaBirthFromRadiusSquared radius)

-- | Membership of a bounded face in the closed alpha shape. Exact dyadic
-- comparison makes equality independent of circumcenter rounding. This
-- specialized observation does not construct the full filtration.
alphaShapeContainsFace
  :: RadiusSquared
  -> Triangulation 'Unconstrained vertex directed undirected face
  -> FaceId
  -> Bool
alphaShapeContainsFace radius triangulation =
  maybe False withinRadius . Dcel.innerFaceVertices triangulation
 where
  threshold = radiusSquaredValue radius
  withinRadius (firstVertex, secondVertex, thirdVertex) =
    let Point ax ay = Dcel.vertexPoint triangulation firstVertex
        Point bx by = Dcel.vertexPoint triangulation secondVertex
        Point cx cy = Dcel.vertexPoint triangulation thirdVertex
     in circumradiusSquaredWithinCoordinates threshold ax ay bx by cx cy

zeroAlphaBirth :: AlphaBirth
zeroAlphaBirth = alphaBirthFromExact 0

exactFaceSection
  :: DelaunayTriangulation vertex
  -> FaceId
  -> (VertexId, VertexId, VertexId)
  -> Either AlphaFiltrationError FaceAlphaSection
exactFaceSection triangulation face verticesValue@(firstVertex, secondVertex, thirdVertex) =
  let Point ax ay = Dcel.vertexPoint triangulation firstVertex
      Point bx by = Dcel.vertexPoint triangulation secondVertex
      Point cx cy = Dcel.vertexPoint triangulation thirdVertex
   in case exactCircumradiusSquaredRational ax ay bx by cx cy of
        Left ExactZeroDivisor -> Left (AlphaFaceDegenerate face)
        Left arithmeticError -> Left (AlphaCircumradiusDivisionFailed face arithmeticError)
        Right exactBirth ->
          Right
            FaceAlphaSection
              { faceAlphaBirth = alphaBirthFromExact exactBirth
              , faceAlphaVertices = verticesValue
              }

exactEdgeBirth
  :: Vector FaceAlphaSection
  -> DelaunayTriangulation vertex
  -> UndirectedEdgeId
  -> Either AlphaFiltrationError AlphaBirth
exactEdgeBirth faceSections triangulation edge = do
  let (fromVertex, toVertex) = Dcel.undirectedEndpoints triangulation edge
      fromPoint = Dcel.vertexPoint triangulation fromVertex
      toPoint = Dcel.vertexPoint triangulation toVertex
      incidentFaces = innerIncidentFaces triangulation edge
  oppositeVertices <-
    traverse
      (oppositeVertexAcross faceSections triangulation edge fromVertex toVertex)
      incidentFaces
  let oppositePoints = fmap (Dcel.vertexPoint triangulation) oppositeVertices
  if all (diametralWitnessOutside fromPoint toPoint) oppositePoints
    then pure (alphaBirthFromExact (quarterSquaredPointDistance fromPoint toPoint))
    else leastIncidentFaceBirth edge faceSections incidentFaces

innerIncidentFaces
  :: DelaunayTriangulation vertex
  -> UndirectedEdgeId
  -> [FaceId]
innerIncidentFaces triangulation edge =
  let (forward, backward) = directedPair edge
      forwardFace = Dcel.incidentFace triangulation forward
      backwardFace = Dcel.incidentFace triangulation backward
   in case (forwardFace == Dcel.outerFace, backwardFace == Dcel.outerFace) of
        (True, True) -> []
        (False, True) -> [forwardFace]
        (True, False) -> [backwardFace]
        (False, False)
          | forwardFace == backwardFace -> [forwardFace]
          | forwardFace < backwardFace -> [forwardFace, backwardFace]
          | otherwise -> [backwardFace, forwardFace]

oppositeVertexAcross
  :: Vector FaceAlphaSection
  -> DelaunayTriangulation vertex
  -> UndirectedEdgeId
  -> VertexId
  -> VertexId
  -> FaceId
  -> Either AlphaFiltrationError VertexId
oppositeVertexAcross faceSections triangulation edge fromVertex toVertex face =
  case faceSectionAt faceSections face of
    Nothing ->
      Left
        (AlphaFaceEdgeIncidenceInvalid face edge (Dcel.faceVertices triangulation face))
    Just faceSection ->
      let verticesValue@(firstVertex, secondVertex, thirdVertex) = faceAlphaVertices faceSection
          outsideEdge vertex = vertex /= fromVertex && vertex /= toVertex
       in case
            ( outsideEdge firstVertex
            , outsideEdge secondVertex
            , outsideEdge thirdVertex
            )
          of
            (True, False, False) -> Right firstVertex
            (False, True, False) -> Right secondVertex
            (False, False, True) -> Right thirdVertex
            _ ->
              Left
                ( AlphaFaceEdgeIncidenceInvalid
                    face
                    edge
                    (filter outsideEdge (triangleVertices verticesValue))
                )

diametralWitnessOutside :: Point -> Point -> Point -> Bool
diametralWitnessOutside firstPoint secondPoint =
  not . inDiametralCircle firstPoint secondPoint

quarterSquaredPointDistance :: Point -> Point -> ExactRational
quarterSquaredPointDistance (Point ax ay) (Point bx by) =
  exactQuarterSquaredDistanceRational ax ay bx by

leastIncidentFaceBirth
  :: UndirectedEdgeId
  -> Vector FaceAlphaSection
  -> [FaceId]
  -> Either AlphaFiltrationError AlphaBirth
leastIncidentFaceBirth edge faceSections incidentFaces = do
  births <-
    traverse
      (\face -> maybe (Left (AlphaIncidentFaceBirthMissing edge face)) (Right . faceAlphaBirth) (faceSectionAt faceSections face))
      incidentFaces
  case NonEmpty.nonEmpty births of
    Nothing -> Left (AlphaEdgeBirthWitnessMissing edge)
    Just nonEmptyBirths -> Right (minimum nonEmptyBirths)

faceSectionAt :: Vector FaceAlphaSection -> FaceId -> Maybe FaceAlphaSection
faceSectionAt faceSections face
  | face == Dcel.outerFace = Nothing
  | otherwise = faceSections Vector.!? (faceIdIndex face - 1)

faceBirthAt :: Vector AlphaBirth -> FaceId -> Maybe AlphaBirth
faceBirthAt faceBirths face
  | face == Dcel.outerFace = Nothing
  | otherwise = faceBirths Vector.!? (faceIdIndex face - 1)

triangleVertices :: (VertexId, VertexId, VertexId) -> [VertexId]
triangleVertices (firstVertex, secondVertex, thirdVertex) =
  [firstVertex, secondVertex, thirdVertex]

alphaBirthFromExact :: ExactRational -> AlphaBirth
alphaBirthFromExact exactValue =
  AlphaBirth (alphaOrderKey exactValue) exactValue

-- | A monotone exact fixed-point projection of the positive rational. Distinct
-- keys prove order; collisions descend to the authoritative rational. One
-- division at birth replaces repeated cross-products during every downstream
-- ordered-map comparison.
alphaOrderKey :: ExactRational -> AlphaOrderKey
alphaOrderKey exactValue =
  let numerator = exactRationalNumerator exactValue
      denominator = exactRationalDenominator exactValue
   in if numerator <= 0
        then NonPositiveAlphaOrder
        else
          let candidateExponent = integerBitLength numerator - integerBitLength denominator
              binaryExponent =
                if candidateExponent >= 0
                  then
                    if numerator < denominator `shiftL` candidateExponent
                      then candidateExponent - 1
                      else candidateExponent
                else
                  if numerator `shiftL` negate candidateExponent < denominator
                    then candidateExponent - 1
                    else candidateExponent
              scale = alphaOrderFractionBits - binaryExponent
              scaledMantissa =
                if scale >= 0
                  then (numerator `shiftL` scale) `quot` denominator
                  else numerator `quot` (denominator `shiftL` negate scale)
           in PositiveAlphaOrder binaryExponent (fromIntegral scaledMantissa)

alphaOrderFractionBits :: Int
alphaOrderFractionBits = 16
