{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

-- | Canonical labelled simplices and downward-closed planar complexes.
-- Labels, rather than resident DCEL handles, own identity across independently
-- constructed meshes.
module Moonlight.Planar.Simplex
  ( SimplexDimension (..)
  , PlanarSimplex
  , PlanarSimplexError (..)
  , planarVertex
  , planarEdge
  , planarFace
  , planarSimplexDimension
  , planarSimplexVertices
  , planarSimplexBoundary
  , PlanarComplex
  , PlanarComplexError (..)
  , planarComplex
  , planarComplexCells
  , planarComplexBasisAt
  , unionPlanarComplex
  , intersectPlanarComplex
  )
where

import Control.DeepSeq (NFData (..))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)

-- | The closed dimension vocabulary for planar simplicial complexes.
data SimplexDimension
  = SimplexDimension0
  | SimplexDimension1
  | SimplexDimension2
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | One canonical simplex. Edge and face labels are strictly ascending.
data PlanarSimplex label
  = PlanarVertex !label
  | PlanarEdge !label !label
  | PlanarFace !label !label !label
  deriving stock (Eq, Ord, Show)

instance NFData label => NFData (PlanarSimplex label) where
  rnf simplex = case simplex of
    PlanarVertex vertex -> rnf vertex
    PlanarEdge first second -> rnf first `seq` rnf second
    PlanarFace first second third -> rnf first `seq` rnf second `seq` rnf third

-- | A repeated vertex would lower the requested simplex's dimension.
data PlanarSimplexError label
  = PlanarSimplexRepeatedVertex !label
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

planarVertex :: label -> PlanarSimplex label
planarVertex = PlanarVertex

planarEdge
  :: Ord label
  => label
  -> label
  -> Either (PlanarSimplexError label) (PlanarSimplex label)
planarEdge firstLabel secondLabel =
  case compare firstLabel secondLabel of
    LT -> Right (PlanarEdge firstLabel secondLabel)
    EQ -> Left (PlanarSimplexRepeatedVertex firstLabel)
    GT -> Right (PlanarEdge secondLabel firstLabel)

planarFace
  :: Ord label
  => label
  -> label
  -> label
  -> Either (PlanarSimplexError label) (PlanarSimplex label)
planarFace firstLabel secondLabel thirdLabel =
  let (firstLow, firstHigh) = ascendingPair firstLabel secondLabel
      (secondLow, finalHigh) = ascendingPair firstHigh thirdLabel
      (finalLow, finalMiddle) = ascendingPair firstLow secondLow
   in if finalLow == finalMiddle
        then Left (PlanarSimplexRepeatedVertex finalLow)
        else
          if finalMiddle == finalHigh
            then Left (PlanarSimplexRepeatedVertex finalMiddle)
            else Right (PlanarFace finalLow finalMiddle finalHigh)

planarSimplexDimension :: PlanarSimplex label -> SimplexDimension
planarSimplexDimension simplex =
  case simplex of
    PlanarVertex _ -> SimplexDimension0
    PlanarEdge _ _ -> SimplexDimension1
    PlanarFace _ _ _ -> SimplexDimension2

planarSimplexVertices :: PlanarSimplex label -> NonEmpty label
planarSimplexVertices simplex =
  case simplex of
    PlanarVertex label -> label :| []
    PlanarEdge firstLabel secondLabel -> firstLabel :| [secondLabel]
    PlanarFace firstLabel secondLabel thirdLabel ->
      firstLabel :| [secondLabel, thirdLabel]

-- | The canonical oriented simplicial boundary.
planarSimplexBoundary
  :: Num coefficient
  => PlanarSimplex label
  -> [(coefficient, PlanarSimplex label)]
planarSimplexBoundary simplex =
  case simplex of
    PlanarVertex _ -> []
    PlanarEdge firstLabel secondLabel ->
      [ (-1, PlanarVertex firstLabel)
      , (1, PlanarVertex secondLabel)
      ]
    PlanarFace firstLabel secondLabel thirdLabel ->
      [ (1, PlanarEdge secondLabel thirdLabel)
      , (-1, PlanarEdge firstLabel thirdLabel)
      , (1, PlanarEdge firstLabel secondLabel)
      ]

-- | A requested cell whose boundary is absent from the same complex.
data PlanarComplexError label
  = PlanarComplexBoundaryMissing
      !(PlanarSimplex label)
      !(PlanarSimplex label)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | An admitted downward-closed complex and its canonical dense bases.
data PlanarComplex label = PlanarComplex
  { storedPlanarComplexCells :: !(Set (PlanarSimplex label))
  , storedPlanarVertexBasis :: !(Vector (PlanarSimplex label))
  , storedPlanarEdgeBasis :: !(Vector (PlanarSimplex label))
  , storedPlanarFaceBasis :: !(Vector (PlanarSimplex label))
  }
  deriving stock (Show)

instance NFData label => NFData (PlanarComplex label) where
  rnf (PlanarComplex cells vertices edges faces) =
    rnf cells `seq` rnf vertices `seq` rnf edges `seq` rnf faces

instance Eq label => Eq (PlanarComplex label) where
  left == right =
    storedPlanarComplexCells left == storedPlanarComplexCells right

planarComplex
  :: Ord label
  => Set (PlanarSimplex label)
  -> Either (PlanarComplexError label) (PlanarComplex label)
planarComplex cells =
  case List.find (\(_, boundary) -> Set.notMember boundary cells) boundaryPairs of
    Just (simplex, boundary) ->
      Left (PlanarComplexBoundaryMissing simplex boundary)
    Nothing -> Right (admittedPlanarComplex cells)
 where
  boundaryPairs =
    [ (simplex, boundary)
    | simplex <- Set.toAscList cells
    , (_, boundary) <- planarSimplexBoundary @Int simplex
    ]

planarComplexCells :: PlanarComplex label -> Set (PlanarSimplex label)
planarComplexCells = storedPlanarComplexCells

planarComplexBasisAt
  :: SimplexDimension
  -> PlanarComplex label
  -> Vector (PlanarSimplex label)
planarComplexBasisAt dimension complexValue =
  case dimension of
    SimplexDimension0 -> storedPlanarVertexBasis complexValue
    SimplexDimension1 -> storedPlanarEdgeBasis complexValue
    SimplexDimension2 -> storedPlanarFaceBasis complexValue

-- | Union preserves downward closure, so no second validation is needed.
unionPlanarComplex
  :: Ord label
  => PlanarComplex label
  -> PlanarComplex label
  -> PlanarComplex label
unionPlanarComplex left right =
  admittedPlanarComplex
    (Set.union (planarComplexCells left) (planarComplexCells right))

-- | Intersection also preserves downward closure.
intersectPlanarComplex
  :: Ord label
  => PlanarComplex label
  -> PlanarComplex label
  -> PlanarComplex label
intersectPlanarComplex left right =
  admittedPlanarComplex
    (Set.intersection (planarComplexCells left) (planarComplexCells right))

admittedPlanarComplex :: Set (PlanarSimplex label) -> PlanarComplex label
admittedPlanarComplex cells =
  let (vertices, edges, faces) =
        foldr partitionSimplex ([], [], []) (Set.toAscList cells)
   in PlanarComplex
        { storedPlanarComplexCells = cells
        , storedPlanarVertexBasis = Vector.fromList vertices
        , storedPlanarEdgeBasis = Vector.fromList edges
        , storedPlanarFaceBasis = Vector.fromList faces
        }

partitionSimplex
  :: PlanarSimplex label
  -> ( [PlanarSimplex label]
     , [PlanarSimplex label]
     , [PlanarSimplex label]
     )
  -> ( [PlanarSimplex label]
     , [PlanarSimplex label]
     , [PlanarSimplex label]
     )
partitionSimplex simplex (vertices, edges, faces) =
  case simplex of
    PlanarVertex _ -> (simplex : vertices, edges, faces)
    PlanarEdge _ _ -> (vertices, simplex : edges, faces)
    PlanarFace _ _ _ -> (vertices, edges, simplex : faces)

ascendingPair :: Ord value => value -> value -> (value, value)
ascendingPair firstValue secondValue =
  if firstValue <= secondValue
    then (firstValue, secondValue)
    else (secondValue, firstValue)
