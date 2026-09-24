-- Expected: GHC-22385, record field queryPointValue not in scope.
module Client (observe, attempt) where

import Moonlight.Planar.Point (Point, QueryPoint, queryPointValue)

observe :: QueryPoint -> Point
observe = queryPointValue

attempt :: QueryPoint -> Point -> QueryPoint
attempt query point = query { queryPointValue = point }
