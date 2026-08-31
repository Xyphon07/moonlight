module TestFixtures
  ( GenuineGoldenCase (..),
    genuineGoldenCorpus,
    intervalComplex,
    mooreComplex,
    projectivePlaneComplex,
    tetrahedronBoundaryComplex,
    tetrahedronBoundaryMissingFaceComplex,
    triangleCycleComplex,
    widePathComplex,
  )
where

import Moonlight.Core (Semiring)
import qualified Moonlight.Homology as H
import qualified Moonlight.Homology.Boundary.Finite as H (mkFiniteChainComplex)
import Numeric.Natural (Natural)

data GenuineGoldenCase = GenuineGoldenCase
  { genuineGoldenName :: String,
    genuineGoldenCellCounts :: [Int],
    genuineGoldenBetti :: [Int],
    genuineGoldenComplex :: H.FiniteChainComplex Integer
  }

genuineGoldenCorpus :: [GenuineGoldenCase]
genuineGoldenCorpus =
  [ GenuineGoldenCase
      { genuineGoldenName = "interval",
        genuineGoldenCellCounts = [2, 1],
        genuineGoldenBetti = [1, 0],
        genuineGoldenComplex = intervalComplex
      },
    GenuineGoldenCase
      { genuineGoldenName = "triangle cycle",
        genuineGoldenCellCounts = [3, 3],
        genuineGoldenBetti = [1, 1],
        genuineGoldenComplex = triangleCycleComplex
      },
    GenuineGoldenCase
      { genuineGoldenName = "tetrahedron boundary",
        genuineGoldenCellCounts = [4, 6, 4],
        genuineGoldenBetti = [1, 0, 1],
        genuineGoldenComplex = tetrahedronBoundaryComplex
      }
  ]

triangleCycleComplex :: H.FiniteChainComplex Integer
triangleCycleComplex =
  H.mkFiniteChainComplex (H.HomologicalDegree 1) $ \dimensionValue ->
    case dimensionValue of
      H.HomologicalDegree 1 ->
        boundaryIncidence
          3
          3
          [ boundaryEntry 0 0 (-1),
            boundaryEntry 0 1 1,
            boundaryEntry 1 1 (-1),
            boundaryEntry 1 2 1,
            boundaryEntry 2 2 (-1),
            boundaryEntry 2 0 1
          ]
      H.HomologicalDegree 0 -> H.emptyBoundaryIncidenceOf 3 0
      _ -> H.emptyBoundaryIncidence

projectivePlaneComplex :: H.FiniteChainComplex Integer
projectivePlaneComplex =
  H.mkFiniteChainComplex (H.HomologicalDegree 2) $ \dimensionValue ->
    case dimensionValue of
      H.HomologicalDegree 2 ->
        boundaryIncidence 1 1 [boundaryEntry 0 0 2]
      H.HomologicalDegree 1 -> H.emptyBoundaryIncidenceOf 1 1
      H.HomologicalDegree 0 -> H.emptyBoundaryIncidenceOf 1 0
      _ -> H.emptyBoundaryIncidence

mooreComplex :: H.FiniteChainComplex Integer
mooreComplex = projectivePlaneComplex

tetrahedronBoundaryComplex :: H.FiniteChainComplex Integer
tetrahedronBoundaryComplex =
  H.mkFiniteChainComplex (H.HomologicalDegree 2) $ \dimensionValue ->
    case dimensionValue of
      H.HomologicalDegree 2 ->
        boundaryIncidence
          4
          6
          [ boundaryEntry 0 3 1,
            boundaryEntry 0 1 (-1),
            boundaryEntry 0 0 1,
            boundaryEntry 1 4 1,
            boundaryEntry 1 2 (-1),
            boundaryEntry 1 0 1,
            boundaryEntry 2 5 1,
            boundaryEntry 2 2 (-1),
            boundaryEntry 2 1 1,
            boundaryEntry 3 5 1,
            boundaryEntry 3 4 (-1),
            boundaryEntry 3 3 1
          ]
      H.HomologicalDegree 1 ->
        boundaryIncidence
          6
          4
          [ boundaryEntry 0 0 (-1),
            boundaryEntry 0 1 1,
            boundaryEntry 1 0 (-1),
            boundaryEntry 1 2 1,
            boundaryEntry 2 0 (-1),
            boundaryEntry 2 3 1,
            boundaryEntry 3 1 (-1),
            boundaryEntry 3 2 1,
            boundaryEntry 4 1 (-1),
            boundaryEntry 4 3 1,
            boundaryEntry 5 2 (-1),
            boundaryEntry 5 3 1
          ]
      H.HomologicalDegree 0 -> H.emptyBoundaryIncidenceOf 4 0
      _ -> H.emptyBoundaryIncidence

intervalComplex :: H.FiniteChainComplex Integer
intervalComplex =
  H.mkFiniteChainComplex (H.HomologicalDegree 1) $ \dimensionValue ->
    case dimensionValue of
      H.HomologicalDegree 1 ->
        boundaryIncidence
          1
          2
          [ boundaryEntry 0 0 (-1),
            boundaryEntry 0 1 1
          ]
      H.HomologicalDegree 0 -> H.emptyBoundaryIncidenceOf 2 0
      _ -> H.emptyBoundaryIncidence

tetrahedronBoundaryMissingFaceComplex :: H.FiniteChainComplex Integer
tetrahedronBoundaryMissingFaceComplex =
  H.mkFiniteChainComplex (H.HomologicalDegree 2) $ \dimensionValue ->
    case dimensionValue of
      H.HomologicalDegree 2 ->
        boundaryIncidence
          3
          6
          [ boundaryEntry 0 1 (-1),
            boundaryEntry 0 2 1,
            boundaryEntry 0 4 1,
            boundaryEntry 1 0 1,
            boundaryEntry 1 2 1,
            boundaryEntry 1 3 (-1),
            boundaryEntry 2 1 1,
            boundaryEntry 2 3 (-1),
            boundaryEntry 2 5 1
          ]
      H.HomologicalDegree 1 ->
        boundaryIncidence
          6
          4
          [ boundaryEntry 0 1 (-1),
            boundaryEntry 0 3 1,
            boundaryEntry 1 0 (-1),
            boundaryEntry 1 2 1,
            boundaryEntry 2 0 (-1),
            boundaryEntry 2 1 1,
            boundaryEntry 3 0 (-1),
            boundaryEntry 3 3 1,
            boundaryEntry 4 1 (-1),
            boundaryEntry 4 2 1,
            boundaryEntry 5 2 (-1),
            boundaryEntry 5 3 1
          ]
      H.HomologicalDegree 0 -> H.emptyBoundaryIncidenceOf 4 0
      _ -> H.emptyBoundaryIncidence

boundaryEntry :: Natural -> Natural -> coefficient -> H.BoundaryEntry coefficient
boundaryEntry = H.mkBoundaryEntry

boundaryIncidence :: (Eq coefficient, Semiring coefficient) => Natural -> Natural -> [H.BoundaryEntry coefficient] -> H.BoundaryIncidence coefficient
boundaryIncidence sourceDimension targetDimension entries =
  case H.mkBoundaryIncidence sourceDimension targetDimension entries of
    Left shapeError ->
      error ("invalid homology test fixture boundary: " <> show shapeError)
    Right incidence ->
      incidence

widePathComplex :: Natural -> H.FiniteChainComplex Integer
widePathComplex nodeCount =
  let edgeCount = nodeCount - 1
   in H.mkFiniteChainComplex (H.HomologicalDegree 1) $ \dimensionValue ->
        case dimensionValue of
          H.HomologicalDegree 1 ->
            boundaryIncidence
              (fromIntegral edgeCount)
              (fromIntegral nodeCount)
              ( concatMap
                  ( \edgeIndex ->
                      [ H.mkBoundaryEntry edgeIndex edgeIndex (-1),
                        H.mkBoundaryEntry edgeIndex (edgeIndex + 1) 1
                      ]
                  )
                  [0 .. edgeCount - 1]
              )
          H.HomologicalDegree 0 ->
            boundaryIncidence (fromIntegral nodeCount) 0 []
          _ ->
            boundaryIncidence 0 0 []
