-- | A picture and its semantic attachment frames share one affine action.
-- Port vocabularies belong to authors: lookup is total, with no registration,
-- missing-name case, or global namespace. Initial frame meaning is supplied
-- by the author; this owner preserves coherence under subsequent placement.
module Moonlight.Planar.Illustration.Motif
  ( Motif
  , motif
  , motifPicture
  , motifPort
  , transformMotif
  , attachMotif
  ) where

import Moonlight.Planar.Affine
  ( AffineIso2, affineIsoMap, composeAffineIso2, inverseAffineIso2 )
import Moonlight.Planar.Illustration (Picture, place)

data Motif port part = Motif !(Picture part) (port -> AffineIso2)

-- | Each frame maps attachment-local coordinates into the picture's space.
-- This constructor does not prove that an authored port lies on its geometry.
motif :: Picture part -> (port -> AffineIso2) -> Motif port part
motif = Motif

motifPicture :: Motif port part -> Picture part
motifPicture (Motif picture _) = picture

motifPort :: Motif port part -> port -> AffineIso2
motifPort (Motif _ ports) = ports

-- | Transform geometry and all ports together. Shear, scale, and reflection
-- are allowed: this is an affine action, not a rigid-mounting promise.
transformMotif :: AffineIso2 -> Motif port part -> Motif port part
transformMotif transform (Motif picture ports) =
  Motif (place (affineIsoMap transform) picture) (composeAffineIso2 transform . ports)
{-# INLINE transformMotif #-}

-- | Align the chosen source frame with the target using @target . inverse
-- source@. This guarantees frame matching, not tangent continuity or clearance.
attachMotif :: port -> AffineIso2 -> Motif port part -> Motif port part
attachMotif sourcePort target value =
  transformMotif
    (composeAffineIso2 target (inverseAffineIso2 (motifPort value sourcePort)))
    value
-- Expose the source-port projection to callers so repeated attachments can
-- share its inverse instead of retaining an opaque per-instance calculation.
{-# INLINE attachMotif #-}
