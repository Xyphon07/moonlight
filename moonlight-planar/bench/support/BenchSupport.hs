{-# LANGUAGE NumericUnderscores #-}

-- | Deterministic triangulation fixtures shared by the native benchmark slices.
module BenchSupport
  ( randomPoints
  , latticePoints
  , latticeFaceBand
  ) where

import Data.List qualified as List
import Data.Word (Word64)
import Data.Vector qualified as V
import Moonlight.Planar.Dcel
  ( faceVertices
  , vertexPoint
  )
import Moonlight.Planar.Internal.HandleDefs (FaceId)
import Moonlight.Planar.Point (Point (Point, pointX))
import Moonlight.Planar.Types (Triangulation)

randomPoints :: Word64 -> Int -> [Point]
randomPoints seed count = take count (go seed)
 where
  go :: Word64 -> [Point]
  go state =
    let state1 = state * 6364136223846793005 + 1442695040888963407
        state2 = state1 * 6364136223846793005 + 1442695040888963407
        unit :: Word64 -> Double
        unit value = fromIntegral (value `div` 2048) / 9_007_199_254_740_992
     in Point (2 * unit state1 - 1) (2 * unit state2 - 1) : go state2

latticePoints :: Int -> Int -> V.Vector Point
latticePoints widthInCells heightInCells =
  V.generate
    ((widthInCells + 1) * (heightInCells + 1))
    (\index ->
        let (row, column) = index `quotRem` (widthInCells + 1)
         in Point (fromIntegral column) (fromIntegral row)
    )

latticeFaceBand
  :: Triangulation mode Point directed undirected face
  -> FaceId
  -> Int
latticeFaceBand triangulation face =
  let xSum =
        List.foldl'
          (\accumulator vertex -> accumulator + pointX (vertexPoint triangulation vertex))
          0
          (faceVertices triangulation face)
   in floor (xSum / 3) `quot` 20
