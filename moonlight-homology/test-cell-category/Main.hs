module Main (main) where

import Moonlight.Homology.Pure.Topology.CellCategory
  ( ComplexCat,
    ComplexMor (..),
    complexCategory,
  )
import Moonlight.Algebra.Pure.Orientation (Orientation (..))
import Moonlight.Category.Pure.FiniteComposable (FiniteComposableCategory (..))
import Moonlight.Category.Simplicial
  ( generatedSimplicesAtDimension,
    normalizedNerve,
    simplicesAtDimension,
    unnormalizedNerve,
  )
import Moonlight.Homology.Pure.Topology.CellComplex
  ( CellComplex2D (..),
    CellTypes (..),
    OrientedEdge (..),
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "finite incidence category"
    [ testCase "enumerates every cell object and incidence morphism" enumerateTriangleCategory,
      testCase "generates one normalized 2-simplex per face-edge-vertex flag" generateTriangleFlagNerve,
      testCase "retains identity-saturated chains only in the unnormalized nerve" compareTriangleNerves
    ]

enumerateTriangleCategory :: Assertion
enumerateTriangleCategory = do
  length (enumerateObjects triangleCategory) @?= 7
  length morphisms @?= 22
  length (filter isIdentityMorphism morphisms) @?= 7
  length (filter isFaceEdgeMorphism morphisms) @?= 3
  length (filter isEdgeVertexMorphism morphisms) @?= 6
  length (filter isFaceVertexMorphism morphisms) @?= 6
  where
    morphisms = enumerateMorphisms triangleCategory

generateTriangleFlagNerve :: Assertion
generateTriangleFlagNerve =
  length (simplicesAtDimension (normalizedNerve triangleCategory 2) 2) @?= 6

compareTriangleNerves :: Assertion
compareTriangleNerves =
  let normalizedCount = length (simplicesAtDimension (normalizedNerve triangleCategory 2) 2)
      unnormalizedCount = length (generatedSimplicesAtDimension (unnormalizedNerve triangleCategory 2) 2)
   in assertBool
        "the unnormalized nerve retains identity-inserted two-simplices"
        (unnormalizedCount > normalizedCount)

isIdentityMorphism :: ComplexMor TriangleComplex -> Bool
isIdentityMorphism morphism =
  case morphism of
    IdentityMor _ -> True
    _ -> False

isFaceEdgeMorphism :: ComplexMor TriangleComplex -> Bool
isFaceEdgeMorphism morphism =
  case morphism of
    FaceToEdge _ _ _ -> True
    _ -> False

isEdgeVertexMorphism :: ComplexMor TriangleComplex -> Bool
isEdgeVertexMorphism morphism =
  case morphism of
    EdgeToVertex _ _ -> True
    _ -> False

isFaceVertexMorphism :: ComplexMor TriangleComplex -> Bool
isFaceVertexMorphism morphism =
  case morphism of
    FaceToVertex _ _ _ -> True
    _ -> False

data TriangleComplex = TriangleComplex

data TriangleVertex
  = VertexZero
  | VertexOne
  | VertexTwo
  deriving stock (Eq, Ord, Show)

data TriangleEdge
  = EdgeZeroOne
  | EdgeOneTwo
  | EdgeTwoZero
  deriving stock (Eq, Ord, Show)

data TriangleFace = TriangleFace
  deriving stock (Eq, Ord, Show)

instance CellTypes TriangleComplex where
  type Vertex TriangleComplex = TriangleVertex
  type Edge TriangleComplex = TriangleEdge
  type Face TriangleComplex = TriangleFace

instance CellComplex2D TriangleComplex where
  vertices _ = [VertexZero, VertexOne, VertexTwo]
  edges _ = [EdgeZeroOne, EdgeOneTwo, EdgeTwoZero]
  faces _ = [TriangleFace]

  edgeBoundary _ edgeValue =
    case edgeValue of
      EdgeZeroOne -> (VertexZero, VertexOne)
      EdgeOneTwo -> (VertexOne, VertexTwo)
      EdgeTwoZero -> (VertexTwo, VertexZero)

  faceBoundary _ TriangleFace =
    [ OrientedEdge EdgeZeroOne Positive,
      OrientedEdge EdgeOneTwo Positive,
      OrientedEdge EdgeTwoZero Positive
    ]

  edgesAtVertex _ vertexValue =
    case vertexValue of
      VertexZero -> [EdgeZeroOne, EdgeTwoZero]
      VertexOne -> [EdgeZeroOne, EdgeOneTwo]
      VertexTwo -> [EdgeOneTwo, EdgeTwoZero]

  facesAtEdge _ _ = (Just TriangleFace, Nothing)

triangleCategory :: ComplexCat TriangleComplex
triangleCategory = complexCategory TriangleComplex
