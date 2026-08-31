module Moonlight.Triangulation.Example.SetAlgebra
  ( siteCounts
  ) where

import qualified Data.Vector as Vector
import Moonlight.Triangulation

-- | Join and meet two overlapping site sets through the public facade.
siteCounts :: Either BuildError (Int, Int)
siteCounts = do
  left <- unitSquareAt 0
  right <- unitSquareAt 1
  joined <- left `union` right
  shared <- intersection left right
  pure (numVertices joined, numVertices shared)

unitSquareAt :: Double -> Either BuildError (DelaunayTriangulation ())
unitSquareAt minimumX =
  delaunayGeometry
    ( Vector.fromList
        [ Point minimumX 0
        , Point (minimumX + 1) 0
        , Point (minimumX + 1) 1
        , Point minimumX 1
        ]
    )
