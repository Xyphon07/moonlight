{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The versioned binary surface: encode a triangulation to bytes and read it
-- back. Decoding refuses a payload whose format version or coordinate encoding this
-- build does not own, rather than reinterpreting it.
module Moonlight.Planar.Serialization
  ( DecodingBudget (..)
  , TrustedPayloadDecoders
  , trustedBinaryPayloadDecoders
  , SerializedCountKind (..)
  , SerializationError (..)
  , serializationVersion
  , encodeTriangulation
  , decodeTriangulation
  ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Binary (Binary (..))
import Data.Binary.Get
  ( Get
  , bytesRead
  , getDoublebe
  , getWord16be
  , getWord32be
  , getWord64be
  , getWord8
  , runGetOrFail
  )
import Data.Binary.Put
  ( putDoublebe
  , putWord16be
  , putWord32be
  , putWord64be
  , putWord8
  , runPut
  )
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word64, Word8)
import Moonlight.Planar.Internal.BoxedPaged
  ( BoxedFill (..)
  , boxedFill
  , boxedFoldr
  , boxedFromVector
  )
import Moonlight.Planar.Internal.Validation (canonicalAdmission)
import Moonlight.Planar.Internal.Paged (fromLocalVector, fromVector, pagedFoldr, pagedIFoldr, pagedLength)
import Moonlight.Planar.Internal.PackedIndex (indexLimit)
import Moonlight.Planar.Internal.PointIndex (buildPointIndex)
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Internal.Representation
import Moonlight.Planar.Internal.Types
import Moonlight.Planar.Point (Point (..), PointValidationError (..))
import Moonlight.Planar.Scalar (canonicalScalarZero)
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.Validation (validateTriangulation)

instance Binary (Point) where
  put (Point x y) = putDoublebe x >> putDoublebe y
  get = Point <$> getDoublebe <*> getDoublebe

-- | The finite resource envelope admitted by the canonical decoder. The byte
-- budget bounds the complete input before parsing; the element budget bounds
-- the total number of library-owned serialized section elements before any
-- section is allocated.
data DecodingBudget = DecodingBudget
  { decodingMaximumInputBytes :: !Word64
  , decodingMaximumSectionElements :: !Word64
  }
  deriving stock (Eq, Show)

-- | Evidence that the caller accepts the internal resource behavior of all
-- four payload decoders. The structural budget governs only containers owned
-- by this module; executable payload decoders require this separate trust law.
data TrustedPayloadDecoders vertex directed undirected face where
  TrustedBinaryPayloadDecoders
    :: ( Binary vertex
       , Binary directed
       , Binary undirected
       , Binary face
       )
    => TrustedPayloadDecoders vertex directed undirected face

-- | Explicitly trust the selected 'Binary' payload instances. This witness is
-- required because arbitrary instances may allocate independently of input
-- bytes; constructing it declares that the caller has audited that behavior.
trustedBinaryPayloadDecoders
  :: ( Binary vertex
     , Binary directed
     , Binary undirected
     , Binary face
     )
  => TrustedPayloadDecoders vertex directed undirected face
trustedBinaryPayloadDecoders = TrustedBinaryPayloadDecoders

-- | The structural count whose encoded value was outside the resident index
-- or allocation domain.
data SerializedCountKind
  = SerializedVertexCount
  | SerializedDirectedEdgeCount
  | SerializedFaceCount
  | SerializedConstraintCount
  deriving stock (Eq, Ord, Show)

-- | Every way serialization refuses, each naming its witness.
data SerializationError
  = BinaryDecodeFailure !Int64 !String
  | TrailingBytes !Int64
  | InvalidFormatMagic !Word64
  | UnsupportedFormatVersion !Word16
  | ConstraintModeTagMismatch !Word8 !Word8
  | CoordinateEncodingTagMismatch !Word8 !Word8
  | InputByteBudgetExceeded !Word64 !Word64
  | DecodedSectionBudgetExceeded !Word64 !Word64
  | EncodedCountExceedsInt !SerializedCountKind !Word64
  | EncodedCountExceedsPackedIndex !SerializedCountKind !Word64 !Word64
  | SerializedDirectedEdgeCountOdd !Word64
  | SerializedConstraintCountExceedsEdges !Word64 !Word64
  | SerializedPlanarCardinalityMismatch !Word64 !Word64 !Word64
  | SerializedFixedBodyTooShort !Word64 !Word64
  | SerializedMissingOuterFace
  | InvalidSerializedPoint {-# UNPACK #-} !Int !PointValidationError
  | NonCanonicalSerializedConstraintFlag !UndirectedEdgeId !Word8
  | SerializedConstraintCountMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | UnconstrainedSerializedConstraints {-# UNPACK #-} !Int
  | DuplicateSerializedCoordinates {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | DecodedInvariantViolations !(NonEmpty InvariantViolation)
  deriving stock (Eq, Show)

type Decoder = ExceptT SerializationError Get

-- | The envelope version this module writes.
serializationVersion :: Word16
serializationVersion = 6

formatMagic :: Word64
formatMagic = 0x5350414445485307 -- "SPADEHS" + canonical geometry-owned format family

binary64EncodingTag :: Word8
binary64EncodingTag = 2

-- | Write the versioned binary envelope.
encodeTriangulation
  :: forall mode vertex directed undirected face. (KnownConstraintMode mode, Binary vertex, Binary directed, Binary undirected, Binary face)
  => Triangulation mode vertex directed undirected face
  -> BL.ByteString
encodeTriangulation triangulation = runPut $ do
  putWord64be formatMagic
  putWord16be serializationVersion
  putWord8 (modeTag (constraintModeValue (modeProxy triangulation)))
  putWord8 binary64EncodingTag
  let ElementDefaults directedDefault undirectedDefault faceDefault = authoringElementDefaults triangulation
      vertexDefault = case boxedFill (triVertexData triangulation) of
        NoFill -> Nothing
        Fill value -> Just value
      vertexCount = pagedLength (triPointX triangulation)
      directedEdgeCount = pagedLength (triHalfTopology triangulation) `quot` 4
      faceCount = pagedLength (triFaceEdge triangulation)
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
  pagedFoldr ((>>) . putDoublebe) (pure ()) (triPointX triangulation)
  pagedFoldr ((>>) . putDoublebe) (pure ()) (triPointY triangulation)
  put vertexDefault
  boxedFoldr ((>>) . put) (pure ()) (triVertexData triangulation)
  pagedFoldr ((>>) . putWord32be) (pure ()) (triVertexOut triangulation)
  -- The wire format stores the four topology planes separately; the interleaved
  -- arena is a resident layout, not a serialization concern.
  let plane field =
        pagedIFoldr
          (\index value rest -> if index `rem` 4 == field then putWord32be value >> rest else rest)
          (pure ())
          (triHalfTopology triangulation)
  plane 0
  plane 1
  plane 2
  plane 3
  boxedFoldr ((>>) . put) (pure ()) (triDirectedData triangulation)
  boxedFoldr ((>>) . put) (pure ()) (triUndirectedData triangulation)
  pagedFoldr ((>>) . putWord32be) (pure ()) (triFaceEdge triangulation)
  boxedFoldr ((>>) . put) (pure ()) (triFaceData triangulation)
  pagedFoldr ((>>) . putWord8) (pure ()) (triConstraint triangulation)

-- | Decode one exact, versioned finite DCEL inside an explicit resource
-- envelope. Structural counts are read and reconciled before any default,
-- payload, or section body is decoded. Coordinate uniqueness and the complete
-- topology, geometry, and Delaunay/CDT invariants are checked before the opaque
-- value is returned.
--
-- The budget bounds the input and the containers owned by this module. The
-- required 'TrustedPayloadDecoders' witness separately records the caller's
-- decision that every selected payload decoder is internally resource-safe.
decodeTriangulation
  :: forall mode vertex directed undirected face.
     KnownConstraintMode mode
  => DecodingBudget
  -> TrustedPayloadDecoders vertex directed undirected face
  -> BL.ByteString
  -> Either SerializationError (Triangulation mode vertex directed undirected face)
decodeTriangulation budget TrustedBinaryPayloadDecoders bytes
  | inputByteCount > decodingMaximumInputBytes budget =
      Left
        ( InputByteBudgetExceeded
            inputByteCount
            (decodingMaximumInputBytes budget)
        )
  | otherwise =
      case runGetOrFail (runExceptT getTriangulation) bytes of
        Left (_, offset, message) -> Left (BinaryDecodeFailure offset message)
        Right (_, _, Left failure) -> Left failure
        Right (trailing, _, Right triangulation)
          | not (BL.null trailing) -> Left (TrailingBytes (BL.length trailing))
          | otherwise ->
              case validateTriangulation triangulation of
                [] -> Right triangulation{triCanonical = canonicalAdmission triangulation}
                firstViolation : remainingViolations ->
                  Left (DecodedInvariantViolations (firstViolation :| remainingViolations))
 where
  inputByteCount = fromIntegral (BL.length bytes)

  getTriangulation :: Decoder (Triangulation mode vertex directed undirected face)
  getTriangulation = do
    magic <- lift getWord64be
    unless (magic == formatMagic) (throwE (InvalidFormatMagic magic))
    version <- lift getWord16be
    unless (version == serializationVersion) (throwE (UnsupportedFormatVersion version))
    encodedMode <- lift getWord8
    let expectedMode = modeTag (constraintModeValue (Proxy :: Proxy mode))
    unless (encodedMode == expectedMode) (throwE (ConstraintModeTagMismatch expectedMode encodedMode))
    encodedScalar <- lift getWord8
    let expectedScalar = binary64EncodingTag
    unless (encodedScalar == expectedScalar) (throwE (CoordinateEncodingTagMismatch expectedScalar encodedScalar))
    encodedVertexCount <- lift getWord64be
    encodedDirectedEdgeCount <- lift getWord64be
    encodedFaceCount <- lift getWord64be
    encodedConstraintCount <- lift getWord64be
    validateStructuralPrefix
      budget
      encodedVertexCount
      encodedDirectedEdgeCount
      encodedFaceCount
      encodedConstraintCount
    prefixByteCount <- fromIntegral <$> lift bytesRead
    let bodyByteCount = inputByteCount - prefixByteCount
        minimumBodyByteCount =
          minimumFixedBodyBytes
            encodedVertexCount
            encodedDirectedEdgeCount
            encodedFaceCount
    unless (bodyByteCount >= minimumBodyByteCount) $
      throwE (SerializedFixedBodyTooShort bodyByteCount minimumBodyByteCount)

    let vertexCount = fromIntegral encodedVertexCount
        halfCount = fromIntegral encodedDirectedEdgeCount
        edgeCount = halfCount `quot` 2
        faceCount = fromIntegral encodedFaceCount
        cachedConstraintCount = fromIntegral encodedConstraintCount
    defaults <- ElementDefaults <$> lift get <*> lift get <*> lift get
    pointXs <- U.replicateM vertexCount (canonicalScalarZero <$> lift getDoublebe)
    pointYs <- U.replicateM vertexCount (canonicalScalarZero <$> lift getDoublebe)
    vertexDefault <- lift get
    vertexDataVector <- V.replicateM vertexCount (lift get)
    vertexOut <- U.replicateM vertexCount (lift getWord32be)
    halfOrigin <- U.replicateM halfCount (lift getWord32be)
    halfNext <- U.replicateM halfCount (lift getWord32be)
    halfPrev <- U.replicateM halfCount (lift getWord32be)
    halfFace <- U.replicateM halfCount (lift getWord32be)
    directedDataVector <- V.replicateM halfCount (lift get)
    undirectedDataVector <- V.replicateM edgeCount (lift get)
    faceEdge <- U.replicateM faceCount (lift getWord32be)
    faceDataVector <- V.replicateM faceCount (lift get)
    constraints <- U.replicateM edgeCount (lift getWord8)

    let points = V.generate vertexCount (\index -> Point (pointXs U.! index) (pointYs U.! index))
    traverse_ (uncurry validateStoredPoint) (V.indexed points)

    case U.ifoldr (\index flag found -> if flag /= 0 && flag /= 1 then Just (index, flag) else found) Nothing constraints of
      Nothing -> pure ()
      Just (index, flag) ->
        throwE
          ( NonCanonicalSerializedConstraintFlag
              (UndirectedEdgeId (fromIntegral index))
              flag
          )
    let actualConstraintCount = U.foldl' (\count flag -> if flag == 1 then count + 1 else count) 0 constraints
    unless (cachedConstraintCount == actualConstraintCount) (throwE (SerializedConstraintCountMismatch cachedConstraintCount actualConstraintCount))
    when (expectedMode == 0 && actualConstraintCount /= 0) (throwE (UnconstrainedSerializedConstraints actualConstraintCount))

    let distinctPointCount = Set.size (V.foldl' (flip Set.insert) Set.empty points)
    unless (distinctPointCount == vertexCount) (throwE (DuplicateSerializedCoordinates vertexCount distinctPointCount))

    let pointXStore = fromLocalVector 0 pointXs
        pointYStore = fromLocalVector 0 pointYs
        topologyStore =
          fromVector maxBound $
            U.generate (4 * halfCount) $ \slot ->
              let (edge, field) = slot `quotRem` 4
               in case field of
                    0 -> halfOrigin U.! edge
                    1 -> halfNext U.! edge
                    2 -> halfPrev U.! edge
                    _ -> halfFace U.! edge
        constraintEdgeIndex =
          U.ifoldl'
            (\edges index flag ->
               if flag == 1 then IntSet.insert index edges else edges
            )
            IntSet.empty
            constraints
    -- Canonical admission reads topology through unchecked packed indices.
    -- Keep this value unadmitted until outer validation has discharged them.
    let decoded =
          Triangulation
            { triPointX = pointXStore
            , triPointY = pointYStore
            , triPointIndex = buildPointIndex pointXStore pointYStore
            , triVertexOut = fromLocalVector maxBound vertexOut
            , triVertexData = boxedFromVector (maybe NoFill Fill vertexDefault) vertexDataVector
            , triHalfTopology = topologyStore
            , triDirectedData = boxedFromVector (Fill (defaultDirectedEdgeData defaults)) directedDataVector
            , triUndirectedData = boxedFromVector (Fill (defaultUndirectedEdgeData defaults)) undirectedDataVector
            , triFaceEdge = fromLocalVector maxBound faceEdge
            , triFaceData = boxedFromVector (Fill (defaultFaceData defaults)) faceDataVector
            , triConstraint = fromVector 0 constraints
            , triConstraintCount = cachedConstraintCount
            , triConstraintEdges = constraintEdgeIndex
            , triSeamFrontier = Nothing
            , triCanonical = CanonicalUnknown
            }
    pure decoded

validateStoredPoint :: Int -> Point -> Decoder ()
validateStoredPoint index point =
  case mkQueryPoint point of
    Left failure -> throwE (InvalidSerializedPoint index failure)
    Right _ -> pure ()

modeProxy :: Triangulation mode vertex directed undirected face -> Proxy mode
modeProxy _ = Proxy

modeTag :: ConstraintMode -> Word8
modeTag Unconstrained = 0
modeTag Constrained = 1

validateStructuralPrefix
  :: DecodingBudget
  -> Word64
  -> Word64
  -> Word64
  -> Word64
  -> Decoder ()
validateStructuralPrefix budget vertexCount directedEdgeCount faceCount constraintCount = do
  traverse_
    (uncurry validateEncodedCount)
    [ (SerializedVertexCount, vertexCount)
    , (SerializedDirectedEdgeCount, directedEdgeCount)
    , (SerializedFaceCount, faceCount)
    , (SerializedConstraintCount, constraintCount)
    ]
  unless (even directedEdgeCount) $
    throwE (SerializedDirectedEdgeCountOdd directedEdgeCount)
  unless (faceCount >= 1) (throwE SerializedMissingOuterFace)
  let undirectedEdgeCount = directedEdgeCount `quot` 2
  unless (constraintCount <= undirectedEdgeCount) $
    throwE
      ( SerializedConstraintCountExceedsEdges
          constraintCount
          undirectedEdgeCount
      )
  unless
    (planarCardinalityHolds vertexCount directedEdgeCount faceCount)
    ( throwE
        ( SerializedPlanarCardinalityMismatch
            vertexCount
            directedEdgeCount
            faceCount
        )
    )
  let sectionElements =
        4 * vertexCount
          + 6 * directedEdgeCount
          + 2 * faceCount
      maximumElements = decodingMaximumSectionElements budget
  when (sectionElements > maximumElements) $
    throwE (DecodedSectionBudgetExceeded sectionElements maximumElements)

validateEncodedCount :: SerializedCountKind -> Word64 -> Decoder ()
validateEncodedCount kind count = do
  when (count > fromIntegral (maxBound :: Int)) $
    throwE (EncodedCountExceedsInt kind count)
  when (count > maximumPackedElementCount) $
    throwE
      ( EncodedCountExceedsPackedIndex
          kind
          count
          maximumPackedElementCount
      )

-- Each resident handle is a Word32 with @maxBound@ withheld as the optional
-- no-index marker. A count may include every remaining representable handle.
maximumPackedElementCount :: Word64
maximumPackedElementCount = fromIntegral indexLimit + 1

-- Every resident triangulation is a connected planar straight-line graph.
-- Empty input has only the outer face; a bounded-face-free nonempty graph is the
-- collinear chain; otherwise Euler's law determines the edge count. All
-- arithmetic is safe after 'validateEncodedCount' bounds each term to the
-- packed-index domain.
planarCardinalityHolds :: Word64 -> Word64 -> Word64 -> Bool
planarCardinalityHolds vertexCount directedEdgeCount faceCount
  | vertexCount == 0 = directedEdgeCount == 0 && faceCount == 1
  | faceCount == 1 = directedEdgeCount == 2 * (vertexCount - 1)
  | vertexCount < 3 = False
  | otherwise =
      directedEdgeCount == 2 * (vertexCount + faceCount - 2)

-- The packed-index proof above bounds every term far below Word64 overflow.
-- Payloads and defaults have no format-level lower bound because lawful
-- 'Binary' decoders such as that for @()@ may consume zero bytes.
minimumFixedBodyBytes :: Word64 -> Word64 -> Word64 -> Word64
minimumFixedBodyBytes vertexCount directedEdgeCount faceCount =
  20 * vertexCount
    + 16 * directedEdgeCount
    + 4 * faceCount
    + directedEdgeCount `quot` 2
