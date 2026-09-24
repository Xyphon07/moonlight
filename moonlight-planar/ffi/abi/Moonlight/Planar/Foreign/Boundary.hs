{-# LANGUAGE TypeFamilies #-}

module Moonlight.Planar.Foreign.Boundary
  ( GeometryMesh
  , HandleValue
  , checkedCount
  , dereferenceHandle
  , freeHandle
  , prepareHandleOutput
  , produceHandle
  , publishHandle
  , readPoints
  , requireOutputCapacity
  , requirePointer
  , runBoundary
  ) where

import Control.Exception (SomeException, try)
import Foreign.C.Types (CDouble (..), CSize, CUInt)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr
  ( StablePtr
  , castPtrToStablePtr
  , castStablePtrToPtr
  , deRefStablePtr
  , freeStablePtr
  , newStablePtr
  )
import Foreign.Storable (peekElemOff, poke)
import qualified Data.Vector as V
import Moonlight.Planar.Minkowski qualified as T (StructuringElement)
import Moonlight.Planar.Region qualified as T (PlanarRegion)
import qualified Moonlight.Planar.Types as T (DelaunayTriangulation)
import qualified Moonlight.Planar.Point as T (Point (..))
import Moonlight.Planar.Foreign.Contract
  ( CMesh
  , CObstruction
  , CRegion
  , CStructuringElement
  )
import Moonlight.Planar.Foreign.Obstruction
  ( AbiFailure (..)
  , bufferTooSmallFailure
  , countOverflowFailure
  , emptyObstruction
  , nullPointerFailure
  , runtimeFailure
  , statusOk
  )

type GeometryMesh = T.DelaunayTriangulation ()

type family HandleValue carrier where
  HandleValue CMesh = GeometryMesh
  HandleValue CRegion = T.PlanarRegion
  HandleValue CStructuringElement = T.StructuringElement

runBoundary :: Ptr CObstruction -> IO (Either AbiFailure ()) -> IO CUInt
runBoundary obstructionPointer action = do
  writeObstruction obstructionPointer emptyObstruction
  outcome <- try action :: IO (Either SomeException (Either AbiFailure ()))
  case outcome of
    Left exception -> finishFailure obstructionPointer (runtimeFailure exception)
    Right (Left failure) -> finishFailure obstructionPointer failure
    Right (Right ()) -> pure statusOk

finishFailure :: Ptr CObstruction -> AbiFailure -> IO CUInt
finishFailure obstructionPointer (AbiFailure status obstruction) = do
  writeObstruction obstructionPointer obstruction
  pure status

writeObstruction :: Ptr CObstruction -> CObstruction -> IO ()
writeObstruction pointer obstruction
  | pointer == nullPtr = pure ()
  | otherwise = poke pointer obstruction

requirePointer :: String -> Ptr value -> Either AbiFailure ()
requirePointer label pointer
  | pointer == nullPtr = Left (nullPointerFailure label)
  | otherwise = Right ()

checkedCount :: Int -> CSize -> Either AbiFailure Int
checkedCount elementsPerItem rawCount
  | toInteger rawCount * toInteger elementsPerItem > toInteger (maxBound :: Int) =
      Left (countOverflowFailure (fromIntegral rawCount))
  | otherwise = Right (fromIntegral rawCount)

readPoints :: Ptr CDouble -> Int -> IO (Either AbiFailure (V.Vector T.Point))
readPoints pointer count
  | count == 0 = pure (Right V.empty)
  | pointer == nullPtr = pure (Left (nullPointerFailure "coordinates"))
  | otherwise =
      Right
        <$> V.generateM
          count
          ( \index -> do
              CDouble x <- peekElemOff pointer (index * 2)
              CDouble y <- peekElemOff pointer (index * 2 + 1)
              pure (T.Point x y)
          )

prepareHandleOutput :: Ptr (Ptr carrier) -> IO (Either AbiFailure ())
prepareHandleOutput pointer =
  case requirePointer "result" pointer of
    Left failure -> pure (Left failure)
    Right () -> poke pointer nullPtr >> pure (Right ())

publishHandle :: Ptr (Ptr carrier) -> HandleValue carrier -> IO ()
publishHandle output value = do
  stable <- newStablePtr value
  poke output (castPtr (castStablePtrToPtr stable))

produceHandle
  :: Ptr (Ptr carrier)
  -> IO (Either AbiFailure (HandleValue carrier))
  -> IO (Either AbiFailure ())
produceHandle output obtain = do
  prepared <- prepareHandleOutput output
  case prepared of
    Left failure -> pure (Left failure)
    Right () -> do
      outcome <- obtain
      case outcome of
        Left failure -> pure (Left failure)
        Right value -> publishHandle output value >> pure (Right ())

dereferenceHandle :: Ptr carrier -> IO (HandleValue carrier)
dereferenceHandle pointer = deRefStablePtr (castPtrToStablePtr (castPtr pointer))

freeHandle :: Ptr carrier -> IO ()
freeHandle pointer
  | pointer == nullPtr = pure ()
  | otherwise = freeStablePtr (castPtrToStablePtr (castPtr pointer) :: StablePtr ())

requireOutputCapacity :: String -> Ptr value -> Int -> Int -> Either AbiFailure ()
requireOutputCapacity label output required capacity
  | capacity < required = Left (bufferTooSmallFailure required capacity)
  | required > 0 = requirePointer label output
  | otherwise = Right ()
