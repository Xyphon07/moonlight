module Moonlight.LinAlg.Pure.Statics.Algebra
  ( allAxes,
    mkMemberRef,
    memberEndpoints,
    memberTouchesNode,
    addVec3,
    subVec3,
    negateVec3,
    scaleVec3,
    dotVec3,
    magnitudeVec3,
    normalizeVec3,
    normalizeVec3Safe,
    axisComponent,
    axisVector,
    vec3Zero,
  )
where

import Moonlight.LinAlg.Pure.Statics.Types
  ( memberEndpoints,
    memberTouchesNode,
    mkMemberRef,
  )
import Moonlight.LinAlg.Pure.Geometry.Vec3
  ( Axis (..),
    addVec3,
    axisComponent,
    axisVector,
    dotVec3,
    magnitudeVec3,
    negateVec3,
    normalizeVec3,
    normalizeVec3Safe,
    scaleVec3,
    subVec3,
    vec3Zero,
  )

allAxes :: [Axis]
allAxes = [AxisX, AxisY, AxisZ]
