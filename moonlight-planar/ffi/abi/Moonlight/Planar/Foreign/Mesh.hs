module Moonlight.Planar.Foreign.Mesh
  ( delaunayF64
  , insertGeometryBatch
  , meshCopyTrianglesU32
  , meshCopyVerticesF64
  , meshFree
  , meshInsertManyF64
  , meshSiteDifference
  , meshSiteIntersection
  , meshSiteSymmetricDifference
  , meshSiteUnion
  , meshTriangleCount
  , meshVertexCount
  ) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Word (Word32)
import Foreign.C.Types (CDouble (..), CSize, CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke, pokeElemOff)
import qualified Data.Vector as V
import Moonlight.Planar.BulkLoad qualified as T (delaunayGeometry)
import Moonlight.Planar.Dcel qualified as T (innerFaceVertexTriples, numVertices, vertexPoints)
import Moonlight.Planar.Handles.HandleDefs qualified as T (unVertexId)
import Moonlight.Planar.SetAlgebra qualified as T (difference, intersection, symmetricDifference, union)
import qualified Moonlight.Planar.Types as T (BuildError (..))
import qualified Moonlight.Planar.Point as T (Point (..))
import Moonlight.Planar.Foreign.Boundary
  ( GeometryMesh
  , HandleValue
  , checkedCount
  , dereferenceHandle
  , freeHandle
  , produceHandle
  , readPoints
  , requireOutputCapacity
  , requirePointer
  , runBoundary
  )
import Moonlight.Planar.Foreign.Contract (CMesh, CObstruction)
import Moonlight.Planar.Foreign.Obstruction (buildFailure)
import Moonlight.Planar.Internal.Types (validatePoint)
import qualified Moonlight.Planar.Internal.Session as Session

delaunayF64 :: Ptr CDouble -> CSize -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
delaunayF64 coordinates rawCount output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case checkedCount 2 rawCount of
      Left failure -> pure (Left failure)
      Right count -> do
        points <- readPoints coordinates count
        pure (points >>= first buildFailure . T.delaunayGeometry)

meshInsertManyF64 :: Ptr CMesh -> Ptr CDouble -> CSize -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
meshInsertManyF64 meshPointer coordinates rawCount output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case requirePointer "mesh" meshPointer >> checkedCount 2 rawCount of
      Left failure -> pure (Left failure)
      Right count -> do
        points <- readPoints coordinates count
        case points of
          Left failure -> pure (Left failure)
          Right admitted -> do
            mesh <- dereferenceHandle meshPointer
            pure (first buildFailure (insertGeometryBatch mesh admitted))

-- | Admit the complete batch before opening the immutable insertion session.
-- The empty batch is the identity and needs no transaction.
insertGeometryBatch :: GeometryMesh -> V.Vector T.Point -> Either T.BuildError GeometryMesh
insertGeometryBatch mesh points
  | V.null points = Right mesh
  | otherwise = do
      admittedPoints <- V.imapM (\index point -> validatePoint (Just index) point) points
      (_, revised, _) <-
        Session.withSession
          mesh
          (V.length points)
          ( V.mapM_
              ( \point ->
                  void
                    ( Session.insertAdmittedVertexAtCombining
                        (\_ replacement -> replacement)
                        point
                        ()
                    )
              )
              admittedPoints
          )
      pure revised

meshSiteUnion, meshSiteIntersection, meshSiteDifference, meshSiteSymmetricDifference :: Ptr CMesh -> Ptr CMesh -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
meshSiteUnion = binaryMeshOperation T.union
meshSiteIntersection = binaryMeshOperation T.intersection
meshSiteDifference = binaryMeshOperation T.difference
meshSiteSymmetricDifference = binaryMeshOperation T.symmetricDifference

binaryMeshOperation
  :: (GeometryMesh -> GeometryMesh -> Either T.BuildError GeometryMesh)
  -> Ptr CMesh
  -> Ptr CMesh
  -> Ptr (Ptr CMesh)
  -> Ptr CObstruction
  -> IO CUInt
binaryMeshOperation operation leftPointer rightPointer output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case requirePointer "left mesh" leftPointer >> requirePointer "right mesh" rightPointer of
      Left failure -> pure (Left failure)
      Right () -> do
        left <- dereferenceHandle leftPointer
        right <- dereferenceHandle rightPointer
        pure (first buildFailure (operation left right))

meshVertexCount, meshTriangleCount :: Ptr CMesh -> Ptr CSize -> Ptr CObstruction -> IO CUInt
meshVertexCount = handleCount "mesh" T.numVertices
meshTriangleCount = handleCount "mesh" (V.length . T.innerFaceVertexTriples)

handleCount
  :: String
  -> (HandleValue carrier -> Int)
  -> Ptr carrier
  -> Ptr CSize
  -> Ptr CObstruction
  -> IO CUInt
handleCount handleLabel observe handlePointer output obstructionPointer =
  runBoundary obstructionPointer $
    case requirePointer handleLabel handlePointer >> requirePointer "count" output of
      Left failure -> pure (Left failure)
      Right () -> do
        value <- dereferenceHandle handlePointer
        poke output (fromIntegral (observe value))
        pure (Right ())

meshCopyVerticesF64 :: Ptr CMesh -> Ptr CDouble -> CSize -> Ptr CSize -> Ptr CObstruction -> IO CUInt
meshCopyVerticesF64 =
  copyHandleProjection
    "mesh"
    "points_written"
    "coordinates"
    T.vertexPoints
    ( \output index (T.Point x y) -> do
        pokeElemOff output (index * 2) (CDouble x)
        pokeElemOff output (index * 2 + 1) (CDouble y)
    )

meshCopyTrianglesU32 :: Ptr CMesh -> Ptr Word32 -> CSize -> Ptr CSize -> Ptr CObstruction -> IO CUInt
meshCopyTrianglesU32 =
  copyHandleProjection
    "mesh"
    "triangles_written"
    "triangles"
    T.innerFaceVertexTriples
    ( \output index (firstVertex, secondVertex, thirdVertex) -> do
        pokeElemOff output (index * 3) (T.unVertexId firstVertex)
        pokeElemOff output (index * 3 + 1) (T.unVertexId secondVertex)
        pokeElemOff output (index * 3 + 2) (T.unVertexId thirdVertex)
    )

copyHandleProjection
  :: String
  -> String
  -> String
  -> (HandleValue carrier -> V.Vector item)
  -> (Ptr element -> Int -> item -> IO ())
  -> Ptr carrier
  -> Ptr element
  -> CSize
  -> Ptr CSize
  -> Ptr CObstruction
  -> IO CUInt
{-# INLINE copyHandleProjection #-}
copyHandleProjection handleLabel writtenLabel outputLabel project writeItem handlePointer output rawCapacity written obstructionPointer =
  runBoundary obstructionPointer $
    case requirePointer handleLabel handlePointer >> requirePointer writtenLabel written >> checkedCount 1 rawCapacity of
      Left failure -> pure (Left failure)
      Right capacity -> do
        value <- dereferenceHandle handlePointer
        let items = project value
            required = V.length items
        poke written (fromIntegral required)
        case requireOutputCapacity outputLabel output required capacity of
          Left failure -> pure (Left failure)
          Right () -> V.imapM_ (writeItem output) items >> pure (Right ())

meshFree :: Ptr CMesh -> IO ()
meshFree = freeHandle
