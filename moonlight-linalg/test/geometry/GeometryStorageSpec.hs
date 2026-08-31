module GeometryStorageSpec
  ( tests,
  )
where

import Data.Vector.Unboxed qualified as U
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable (..))
import Moonlight.LinAlg.Geometry
  ( Vec2 (..),
    Vec3 (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Geometry storage"
    [ testCase "Vec2 unboxed vectors round-trip through fromList/toList" testVec2UnboxRoundTrip,
      testCase "Vec3 unboxed vectors round-trip through fromList/toList" testVec3UnboxRoundTrip,
      testCase "Vec2 storable layout round-trips through peek/poke" testVec2StorableRoundTrip,
      testCase "Vec3 storable layout round-trips through peek/poke" testVec3StorableRoundTrip,
      testCase "Vec2 unboxed indexing agrees with list indexing on generated batches" testVec2UnboxIndexing,
      testCase "Vec3 unboxed indexing agrees with list indexing on generated batches" testVec3UnboxIndexing
    ]

testVec2UnboxRoundTrip :: Assertion
testVec2UnboxRoundTrip =
  assertEqual
    "Vec2 U.fromList/U.toList identity"
    vec2Batch
    (U.toList (U.fromList vec2Batch :: U.Vector Vec2))

testVec3UnboxRoundTrip :: Assertion
testVec3UnboxRoundTrip =
  assertEqual
    "Vec3 U.fromList/U.toList identity"
    vec3Batch
    (U.toList (U.fromList vec3Batch :: U.Vector Vec3))

testVec2StorableRoundTrip :: Assertion
testVec2StorableRoundTrip =
  assertStorableRoundTrip "Vec2 peek/poke identity" (Vec2 3.25 (-8.5))

testVec3StorableRoundTrip :: Assertion
testVec3StorableRoundTrip =
  assertStorableRoundTrip "Vec3 peek/poke identity" (Vec3 3.25 (-8.5) 13.75)

testVec2UnboxIndexing :: Assertion
testVec2UnboxIndexing =
  assertEqual
    "Vec2 indexed unboxed vector agrees with indexed source list"
    (zip [0 :: Int ..] vec2Batch)
    (U.toList (U.indexed (U.fromList vec2Batch :: U.Vector Vec2)))

testVec3UnboxIndexing :: Assertion
testVec3UnboxIndexing =
  assertEqual
    "Vec3 indexed unboxed vector agrees with indexed source list"
    (zip [0 :: Int ..] vec3Batch)
    (U.toList (U.indexed (U.fromList vec3Batch :: U.Vector Vec3)))

assertStorableRoundTrip :: (Eq value, Show value, Storable value) => String -> value -> Assertion
assertStorableRoundTrip label value =
  alloca $ \pointerValue -> do
    poke pointerValue value
    actualValue <- peek pointerValue
    assertEqual label value actualValue

vec2Batch :: [Vec2]
vec2Batch =
  (\indexValue -> Vec2 (coordinateValue 17 indexValue) (coordinateValue 29 indexValue))
    <$> [0 .. 127]

vec3Batch :: [Vec3]
vec3Batch =
  ( \indexValue ->
      Vec3
        (coordinateValue 17 indexValue)
        (coordinateValue 29 indexValue)
        (coordinateValue 43 indexValue)
  )
    <$> [0 .. 127]

coordinateValue :: Int -> Int -> Double
coordinateValue saltValue indexValue =
  (fromIntegral ((indexValue * 1103515245 + saltValue * 12345) `mod` 65521) / 257.0) - 127.0
