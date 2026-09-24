{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

-- | Test-only V6 wire oracle retained from the gathered encoder. It deliberately
-- does not call the production encoder or the new storage producers.
-- Its unfolding visibility matches the serialize component so the benchmark
-- compares both encoders under the same cross-module specialization policy.
module Moonlight.Planar.SerializationV6Oracle (encodeV6Gathered) where

import Data.Binary (Binary (..))
import Data.Binary.Put
  ( putDoublebe, putWord16be, putWord32be, putWord64be, putWord8, runPut )
import qualified Data.ByteString.Lazy as BL
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word64, Word8)
import Moonlight.Planar.Internal.BoxedPaged (BoxedFill (..), boxedFill, boxedToVector)
import Moonlight.Planar.Internal.Paged (toVector)
import Moonlight.Planar.Internal.Representation (Triangulation (..), authoringElementDefaults)
import Moonlight.Planar.Internal.Types
  ( ConstraintMode (..), ElementDefaults (..), KnownConstraintMode (..) )

formatMagic :: Word64
formatMagic = 0x5350414445485307

serializationVersion :: Word16
serializationVersion = 6

binary64EncodingTag :: Word8
binary64EncodingTag = 2

modeProxy :: Triangulation mode vertex directed undirected face -> Proxy mode
modeProxy _ = Proxy

modeTag :: ConstraintMode -> Word8
modeTag Unconstrained = 0
modeTag Constrained = 1

encodeV6Gathered
  :: forall mode vertex directed undirected face. (KnownConstraintMode mode, Binary vertex, Binary directed, Binary undirected, Binary face)
  => Triangulation mode vertex directed undirected face
  -> BL.ByteString
encodeV6Gathered triangulation = runPut $ do
  putWord64be formatMagic
  putWord16be serializationVersion
  putWord8 (modeTag (constraintModeValue (modeProxy triangulation)))
  putWord8 binary64EncodingTag
  let ElementDefaults directedDefault undirectedDefault faceDefault = authoringElementDefaults triangulation
      pointXs = toVector (triPointX triangulation)
      pointYs = toVector (triPointY triangulation)
      vertexDefault = case boxedFill (triVertexData triangulation) of
        NoFill -> Nothing
        Fill value -> Just value
      vertexDataVector = boxedToVector (triVertexData triangulation)
      vertexOut = toVector (triVertexOut triangulation)
      topology = toVector (triHalfTopology triangulation)
      directedDataVector = boxedToVector (triDirectedData triangulation)
      undirectedDataVector = boxedToVector (triUndirectedData triangulation)
      faceEdge = toVector (triFaceEdge triangulation)
      faceDataVector = boxedToVector (triFaceData triangulation)
      constraints = toVector (triConstraint triangulation)
      vertexCount = U.length pointXs
      directedEdgeCount = U.length topology `quot` 4
      faceCount = U.length faceEdge
  -- Version 6 commits every structural count in one prefix. The decoder can
  -- prove their relationships and resource bounds before defaults, payloads,
  -- or section bodies are evaluated.
  putWord64be (fromIntegral vertexCount)
  putWord64be (fromIntegral directedEdgeCount)
  putWord64be (fromIntegral faceCount)
  putWord64be (fromIntegral (triConstraintCount triangulation))
  put directedDefault
  put undirectedDefault
  put faceDefault
  -- Geometry and payloads are independent components. Persist the authoritative
  -- coordinate pages rather than attempting to recover them from annotations.
  U.mapM_ putDoublebe pointXs
  U.mapM_ putDoublebe pointYs
  put vertexDefault
  V.mapM_ put vertexDataVector
  U.mapM_ putWord32be vertexOut
  -- The wire format stores the four topology planes separately; the interleaved
  -- arena is a resident layout, not a serialization concern.
  let plane field = U.generate directedEdgeCount (\edge -> topology U.! (4 * edge + field))
  U.mapM_ putWord32be (plane 0)
  U.mapM_ putWord32be (plane 1)
  U.mapM_ putWord32be (plane 2)
  U.mapM_ putWord32be (plane 3)
  V.mapM_ put directedDataVector
  V.mapM_ put undirectedDataVector
  U.mapM_ putWord32be faceEdge
  V.mapM_ put faceDataVector
  U.mapM_ putWord8 constraints
