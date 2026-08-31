module CodecSpec
  ( codecTests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch
import Moonlight.Delta.Patch.Internal.Builder (toPaged)
import Moonlight.Delta.Patch.Internal.Types (CodecStats (..), debugCodecStats)
import Moonlight.Delta.Patch.Internal.Types qualified as Internal
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, (@?=), testCase)

codecTests :: TestTree
codecTests =
  testGroup
    "codec"
    [ testCase "constructor packs repeated endpoints as constant columns" $
        debugCodecStats repeatedEndpointPatch
          @?=
            CodecStats
              { codecExplicitKeyColumns = 0,
                codecRangeKeyColumns = 1,
                codecAffineKeyColumns = 0,
                codecAbsentColumns = 0,
                codecConstantColumns = 2,
                codecRunColumns = 0,
                codecAffineValueColumns = 0,
                codecDenseColumns = 0
              },
      testCase "constructor packs repeated endpoint runs as run columns" $
        debugCodecStats runEndpointPatch
          @?=
            CodecStats
              { codecExplicitKeyColumns = 0,
                codecRangeKeyColumns = 1,
                codecAffineKeyColumns = 0,
                codecAbsentColumns = 0,
                codecConstantColumns = 0,
                codecRunColumns = 2,
                codecAffineValueColumns = 0,
                codecDenseColumns = 0
              },
      testCase "constructor packs strided Int support as affine key columns" $
        debugCodecStats affineKeyPatch
          @?=
            CodecStats
              { codecExplicitKeyColumns = 0,
                codecRangeKeyColumns = 0,
                codecAffineKeyColumns = 1,
                codecAbsentColumns = 0,
                codecConstantColumns = 2,
                codecRunColumns = 0,
                codecAffineValueColumns = 0,
                codecDenseColumns = 0
              },
      testCase "constructor keeps non-Int support in explicit key columns" $
        debugCodecStats genericKeyPatch
          @?=
            CodecStats
              { codecExplicitKeyColumns = 1,
                codecRangeKeyColumns = 0,
                codecAffineKeyColumns = 0,
                codecAbsentColumns = 0,
                codecConstantColumns = 2,
                codecRunColumns = 0,
                codecAffineValueColumns = 0,
                codecDenseColumns = 0
              },
      testCase "constructor packs strided Int endpoints as affine value columns" $
        debugCodecStats affineValuePatch
          @?=
            CodecStats
              { codecExplicitKeyColumns = 0,
                codecRangeKeyColumns = 1,
                codecAffineKeyColumns = 0,
                codecAbsentColumns = 0,
                codecConstantColumns = 0,
                codecRunColumns = 0,
                codecAffineValueColumns = 2,
                codecDenseColumns = 0
              },
      testCase "normalization downshifts an under-threshold paged patch" $
        case Internal.normalize (toPaged smallPatch) of
          Internal.SmallPatch _cells -> pure ()
          Internal.PagedPatch _count _pages -> assertFailure "under-threshold patch remained paged",
      testCase "diff returns the canonical small representation" $
        case diff (Map.empty :: Map.Map Int String) Map.empty of
          Internal.SmallPatch _cells -> pure ()
          Internal.PagedPatch _count _pages -> assertFailure "empty diff remained paged"
    ]

repeatedEndpointPatch :: Patch Int String
repeatedEndpointPatch =
  fromAscList
    [ (key, replace "before" "after")
      | key <- [0 .. 63]
    ]

runEndpointPatch :: Patch Int String
runEndpointPatch =
  fromAscList
    [ (key, replace (beforeValue key) (afterValue key))
      | key <- [0 .. 63]
    ]
  where
    beforeValue :: Int -> String
    beforeValue key =
      if key < 32 then "before-left" else "before-right"

    afterValue :: Int -> String
    afterValue key =
      if key < 32 then "after-left" else "after-right"

affineKeyPatch :: Patch Int String
affineKeyPatch =
  fromAscList
    [ (key, replace "before" "after")
      | key <- fmap (* 2) [0 .. 63]
    ]

newtype GenericCodecKey = GenericCodecKey Int
  deriving stock (Eq, Ord)

genericKeyPatch :: Patch GenericCodecKey String
genericKeyPatch =
  fromAscList
    [ (GenericCodecKey key, replace "before" "after")
      | key <- [0 .. 63]
    ]

affineValuePatch :: Patch Int Int
affineValuePatch =
  fromAscList
    [ (key, replace (key * 2) (key * 2 + 1))
      | key <- [0 .. 63]
    ]

smallPatch :: Patch Int String
smallPatch =
  fromAscList
    [ (key, replace "before" "after")
      | key <- [0 .. 7]
    ]
