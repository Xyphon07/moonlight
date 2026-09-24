module Moonlight.Planar.Example.ConstrainedRegion
  ( constrainedRegionCounts
  ) where

import qualified Data.Vector as Vector
import Moonlight.Planar.Cdt (CdtError (..), boundedRegionFaces, constrainedDelaunay)
import Moonlight.Planar.Dcel (numFaces)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Types (buildTriangulation, unitElementDefaults)

-- | Count every face and only the bounded odd-depth domain faces of an annulus.
constrainedRegionCounts :: Either CdtError (Int, Int)
constrainedRegionCounts = do
  result <-
    constrainedDelaunay
      unitElementDefaults
      (Vector.fromList annulusPoints)
      (Vector.fromList (closedLoop 0 <> closedLoop 4))
  let annulus = buildTriangulation result
  pure (numFaces annulus, length (boundedRegionFaces annulus))

annulusPoints :: [Point]
annulusPoints =
  [ Point (-2) (-2)
  , Point 2 (-2)
  , Point 2 2
  , Point (-2) 2
  , Point (-1) (-1)
  , Point 1 (-1)
  , Point 1 1
  , Point (-1) 1
  ]

closedLoop :: Int -> [(Int, Int)]
closedLoop offset =
  [ (offset, offset + 1)
  , (offset + 1, offset + 2)
  , (offset + 2, offset + 3)
  , (offset + 3, offset)
  ]
