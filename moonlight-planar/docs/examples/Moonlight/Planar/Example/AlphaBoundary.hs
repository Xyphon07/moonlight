module Moonlight.Planar.Example.AlphaBoundary
  ( AlphaBoundaryExampleError (..)
  , BoundaryShape (..)
  , alphaBoundaryShapes
  ) where

import Data.Bifunctor (first)
import qualified Data.Vector as Vector
import Moonlight.Planar.Alpha (alphaShapeContainsFace)
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.FloodFillIterator (BoundaryObstruction (..), RegionBoundary, boundaryLoopVertices, componentBoundary, faceComponents, regionBoundaryHoleLoops, regionBoundaryOuterLoop)
import Moonlight.Planar.Types (BuildError (..))
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Scalar (RadiusSquaredError (..), mkRadiusSquared)

data AlphaBoundaryExampleError
  = AlphaBoundaryBuildFailed !BuildError
  | AlphaBoundaryRadiusFailed !RadiusSquaredError
  | AlphaBoundaryDescentFailed !BoundaryObstruction
  deriving stock (Eq, Show)

data BoundaryShape = BoundaryShape
  { boundaryOuterVertexCount :: !Int
  , boundaryHoleCount :: !Int
  }
  deriving stock (Eq, Show)

-- | Descend the closed alpha-shape face section of one square to its boundary.
alphaBoundaryShapes :: Either AlphaBoundaryExampleError [BoundaryShape]
alphaBoundaryShapes = do
  mesh <-
    first AlphaBoundaryBuildFailed
      ( delaunayGeometry
          ( Vector.fromList
              [ Point 0 0
              , Point 2 0
              , Point 2 2
              , Point 0 2
              ]
          )
      )
  threshold <- first AlphaBoundaryRadiusFailed (mkRadiusSquared 2)
  let selectedComponents =
        fmap snd
          (filter fst (faceComponents mesh (alphaShapeContainsFace threshold mesh)))
  traverse
    ( fmap boundaryShape
        . first AlphaBoundaryDescentFailed
        . componentBoundary mesh
    )
    selectedComponents

boundaryShape :: RegionBoundary -> BoundaryShape
boundaryShape boundary =
  BoundaryShape
    { boundaryOuterVertexCount =
        length (boundaryLoopVertices (regionBoundaryOuterLoop boundary))
    , boundaryHoleCount = length (regionBoundaryHoleLoops boundary)
    }
