module Moonlight.Planar.Example.SetAlgebra
  ( siteCounts
  ) where

import qualified Data.Vector as Vector
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.Dcel (numVertices)
import Moonlight.Planar.SetAlgebra (intersection, union)
import Moonlight.Planar.Types (BuildError (..), DelaunayTriangulation)
import Moonlight.Planar.Point (Point (..))

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
