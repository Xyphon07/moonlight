{-# OPTIONS_GHC -Wno-orphans #-}

module Fixtures
  ( Shape (..),
    shapes,
    compileSizes,
    querySizes,
    tablelessTallGridCompileHeights,
    tablelessTallGridQueryHeights,
    compileLatticeEnv,
    compileTablelessLatticeEnv,
    compileTablelessTallGridEnv,
    assertFiniteFixture,
    compileLatticeWeight,
    compileTablelessLatticeWeight,
    compileTablelessTallGridWeight,
    compilePresentationWeight,
    compileBoundedPresentationWeight,
    shapeJoinMeetTable,
    rawRelationRows,
    hackageBooleanCubeElements,
    hackageBooleanCubeElement,
    booleanCubeBits,
    supportSeeds,
    wideSupportSeeds,
    shiftedWideSupportSeeds,
    joinSeedStep,
    meetSeedStep,
    tallGridElementCount,
    shapeLabel,
    caseLabel,
    keys,
    topKey,
    bottomKey,
  )
where

import Control.DeepSeq
  ( NFData (..),
  )
import Data.Bifunctor
  ( first,
  )
import Data.Bits
  ( (.&.),
    (.|.),
    popCount,
  )
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.FiniteLattice.Core
  ( ContextCompileLimits (..),
    ContextLattice,
    ContextOrderDecl,
    clBottom,
    clTop,
    compileContextLattice,
    compileContextLatticeWith,
    contextLatticeElements,
    contextOrderDecl,
    defaultContextCompileLimits,
  )
import Moonlight.FiniteLattice.Cover
  ( coverPairs,
  )
import Moonlight.FiniteLattice.Presentation qualified as Presentation

-- | Finite benchmark presentation families. Each family is a local chart over
-- the same declared carrier @[0..n-1]@; the benchmark groups are glued from
-- these fixtures rather than re-declaring lattice shape folklore inline.
data Shape
  = Chain
  | Fan
  | BooleanCube
  | DenseGrid
  deriving stock (Eq, Ord, Show)

instance NFData c => NFData (ContextLattice c) where
  rnf lattice =
    rnf
      ( clTop lattice,
        clBottom lattice,
        contextLatticeElements lattice,
        coverPairs lattice
      )

compileLatticeEnv :: Shape -> Int -> IO (ContextLattice Int)
compileLatticeEnv shape size =
  case compileLattice shape size of
    Left err -> fail ("invalid finite-lattice benchmark fixture: " <> err)
    Right lattice -> pure lattice

compileTablelessLatticeEnv :: Shape -> Int -> IO (ContextLattice Int)
compileTablelessLatticeEnv shape size =
  case first show (compileContextLatticeWith tablelessLimits (universe size) (decl shape size)) of
    Left err -> fail ("invalid tableless finite-lattice benchmark fixture: " <> err)
    Right lattice -> pure lattice

compileTablelessTallGridEnv :: Int -> IO (ContextLattice Int)
compileTablelessTallGridEnv height =
  case first show (compileContextLatticeWith tablelessLimits (tallGridUniverse height) (tallGridDecl height)) of
    Left err -> fail ("invalid tableless tall-grid benchmark fixture: " <> err)
    Right lattice -> pure lattice

assertFiniteFixture :: String -> Either String () -> IO ()
assertFiniteFixture label outcome =
  case outcome of
    Right () ->
      pure ()
    Left err ->
      fail ("invalid finite-lattice benchmark fixture " <> label <> ": " <> err)

compileLatticeWeight :: Shape -> Int -> Either String Int
compileLatticeWeight shape size =
  first show (compileContextLattice (universe size) (decl shape size)) >>= latticeCompileWeight

compileTablelessLatticeWeight :: Shape -> Int -> Either String Int
compileTablelessLatticeWeight shape size =
  first show (compileContextLatticeWith tablelessLimits (universe size) (decl shape size)) >>= latticeCompileWeight

compileTablelessTallGridWeight :: Int -> Either String Int
compileTablelessTallGridWeight height =
  first show (compileContextLatticeWith tablelessLimits (tallGridUniverse height) (tallGridDecl height)) >>= latticeCompileWeight

compilePresentationWeight :: Shape -> Int -> Either String Int
compilePresentationWeight shape size =
  first show (Presentation.latticeOf (presentation shape size)) >>= latticeCompileWeight

compileBoundedPresentationWeight :: Shape -> Int -> Either String Int
compileBoundedPresentationWeight shape size =
  first show (Presentation.boundedLatticeOf (topKey size) bottomKey (presentation shape size)) >>= latticeCompileWeight

latticeCompileWeight :: ContextLattice Int -> Either String Int
latticeCompileWeight lattice =
  Right
    ( clTop lattice
        + clBottom lattice
        + length (contextLatticeElements lattice)
        + length (coverPairs lattice)
    )

compileLattice :: Shape -> Int -> Either String (ContextLattice Int)
compileLattice shape size =
  first show (compileContextLattice (universe size) (decl shape size))

presentation :: Shape -> Int -> Presentation.LatticeBuilder Int ()
presentation shape size = do
  elementRefs <- Presentation.elements (keys size)
  declarePresentationShapeEdges shape size elementRefs

declarePresentationShapeEdges :: Shape -> Int -> [Presentation.ElemRef Int] -> Presentation.LatticeBuilder Int ()
declarePresentationShapeEdges shape size elementRefs =
  case shape of
    Chain ->
      Presentation.belowAll (zip elementRefs (drop 1 elementRefs))
    Fan ->
      declareFanPresentationEdges elementRefs
    BooleanCube ->
      declareBooleanCubePresentationEdges size elementRefs
    DenseGrid ->
      declareGridPresentationEdges size elementRefs

declareFanPresentationEdges :: [Presentation.ElemRef Int] -> Presentation.LatticeBuilder Int ()
declareFanPresentationEdges elementRefs =
  case elementRefs of
    bottomRef : restRefs ->
      case List.unsnoc restRefs of
        Just (atomRefs, topRef) ->
          Presentation.belowAll
            ( foldMap
                (\atomRef -> [(bottomRef, atomRef), (atomRef, topRef)])
                atomRefs
            )
        Nothing -> pure ()
    [] -> pure ()

declareBooleanCubePresentationEdges :: Int -> [Presentation.ElemRef Int] -> Presentation.LatticeBuilder Int ()
declareBooleanCubePresentationEdges size elementRefs =
  let elementRefByValue = Vector.fromList elementRefs
      dimensionBits = takeWhile (< size) (iterate (* 2) 1)
   in Presentation.belowAll
        [ (lowerRef, upperRef)
        | (keyValue, lowerRef) <- zip (keys size) elementRefs,
          bitValue <- dimensionBits,
          keyValue .&. bitValue == 0,
          let upperValue = keyValue + bitValue,
          upperValue < size,
          Just upperRef <- [elementRefByValue Vector.!? upperValue]
        ]

declareGridPresentationEdges :: Int -> [Presentation.ElemRef Int] -> Presentation.LatticeBuilder Int ()
declareGridPresentationEdges size elementRefs =
  let elementRefByValue = Vector.fromList elementRefs
   in Presentation.belowAll
        [ (lowerRef, upperRef)
        | (keyValue, lowerRef) <- zip (keys size) elementRefs,
          upperValue <- gridUpperCovers size keyValue,
          Just upperRef <- [elementRefByValue Vector.!? upperValue]
        ]

shapeJoinMeetTable :: Shape -> Int -> Map.Map (Int, Int) (Int, Int)
shapeJoinMeetTable shape size =
  Map.fromAscList
    [ ((leftValue, rightValue), (shapeJoin shape size leftValue rightValue, shapeMeet shape size leftValue rightValue))
    | leftValue <- keys size,
      rightValue <- keys size
    ]

shapeJoin :: Shape -> Int -> Int -> Int -> Int
shapeJoin shape size leftValue rightValue =
  case shape of
    Chain -> max leftValue rightValue
    Fan -> fanJoin size leftValue rightValue
    BooleanCube -> leftValue .|. rightValue
    DenseGrid ->
      gridKey
        size
        (max (gridRow size leftValue) (gridRow size rightValue))
        (max (gridColumn size leftValue) (gridColumn size rightValue))

shapeMeet :: Shape -> Int -> Int -> Int -> Int
shapeMeet shape size leftValue rightValue =
  case shape of
    Chain -> min leftValue rightValue
    Fan -> fanMeet size leftValue rightValue
    BooleanCube -> leftValue .&. rightValue
    DenseGrid ->
      gridKey
        size
        (min (gridRow size leftValue) (gridRow size rightValue))
        (min (gridColumn size leftValue) (gridColumn size rightValue))

fanJoin :: Int -> Int -> Int -> Int
fanJoin size leftValue rightValue
  | leftValue == bottomKey = rightValue
  | rightValue == bottomKey = leftValue
  | leftValue == rightValue = leftValue
  | leftValue == topKey size || rightValue == topKey size = topKey size
  | otherwise = topKey size

fanMeet :: Int -> Int -> Int -> Int
fanMeet size leftValue rightValue
  | leftValue == topKey size = rightValue
  | rightValue == topKey size = leftValue
  | leftValue == rightValue = leftValue
  | leftValue == bottomKey || rightValue == bottomKey = bottomKey
  | otherwise = bottomKey

rawRelationRows :: Shape -> Int -> Vector.Vector IntSet.IntSet
rawRelationRows shape size =
  Vector.fromList
    [ IntSet.fromAscList (upperKeys shape size leftKey)
    | leftKey <- keys size
    ]

hackageBooleanCubeElements :: Int -> [IntSet.IntSet]
hackageBooleanCubeElements size =
  fmap (hackageBooleanCubeElement (booleanCubeBits size)) (keys size)

hackageBooleanCubeElement :: [Int] -> Int -> IntSet.IntSet
hackageBooleanCubeElement dimensionBits keyValue =
  IntSet.fromAscList
    [ bitOrdinal
    | (bitOrdinal, bitValue) <- zip [0 ..] dimensionBits,
      keyValue .&. bitValue /= 0
    ]

booleanCubeBits :: Int -> [Int]
booleanCubeBits size =
  takeWhile (< size) (iterate (* 2) 1)

decl :: Shape -> Int -> ContextOrderDecl Int
decl shape size =
  contextOrderDecl (topKey size) bottomKey (coverEdges shape size)

tallGridDecl :: Int -> ContextOrderDecl Int
tallGridDecl height =
  contextOrderDecl (tallGridTopKey height) bottomKey (tallGridCoverEdges height)

tallGridCoverEdges :: Int -> [(Int, Int)]
tallGridCoverEdges height =
  [ (keyValue, upperValue)
  | keyValue <- keys (tallGridElementCount height),
    upperValue <- tallGridUpperCovers height keyValue
  ]

tallGridUniverse :: Int -> Set.Set Int
tallGridUniverse height =
  universe (tallGridElementCount height)

coverEdges :: Shape -> Int -> [(Int, Int)]
coverEdges shape size =
  case shape of
    Chain -> fmap (\keyValue -> (keyValue, keyValue + 1)) [0 .. size - 2]
    Fan ->
      fmap (\keyValue -> (bottomKey, keyValue)) (atomKeys size)
        <> fmap (\keyValue -> (keyValue, topKey size)) (atomKeys size)
    BooleanCube ->
      booleanCubeCoverEdges size
    DenseGrid ->
      [ (keyValue, upperValue)
      | keyValue <- keys size,
        upperValue <- gridUpperCovers size keyValue
      ]

booleanCubeCoverEdges :: Int -> [(Int, Int)]
booleanCubeCoverEdges size =
  [ (keyValue, keyValue + bitValue)
  | keyValue <- keys size,
    bitValue <- takeWhile (< size) (iterate (* 2) 1),
    keyValue .&. bitValue == 0,
    keyValue + bitValue < size
  ]

upperKeys :: Shape -> Int -> Int -> [Int]
upperKeys shape size keyValue =
  case shape of
    Chain -> [keyValue .. topKey size]
    Fan
      | keyValue == bottomKey -> keys size
      | keyValue == topKey size -> [topKey size]
      | otherwise -> [keyValue, topKey size]
    BooleanCube ->
      [candidateKey | candidateKey <- keys size, keyValue .&. candidateKey == keyValue]
    DenseGrid ->
      [candidateKey | candidateKey <- keys size, gridKeyLeq size keyValue candidateKey]

supportSeeds :: Shape -> Int -> [Int]
supportSeeds shape size =
  case shape of
    Chain ->
      [size `div` 3, (2 * size) `div` 3, topKey size]
    Fan ->
      take 8 (atomKeys size)
    BooleanCube ->
      take 16 (booleanCubeMiddleLayer size)
    DenseGrid ->
      take 16 (gridMiddleLayer size)

wideSupportSeeds :: Shape -> Int -> [Int]
wideSupportSeeds shape size =
  case shape of
    Chain ->
      [size `div` 3, (2 * size) `div` 3, topKey size]
    Fan ->
      take 16 (atomKeys size)
    BooleanCube ->
      take 32 (booleanCubeMiddleLayer size)
    DenseGrid ->
      take 32 (gridMiddleLayer size)

shiftedWideSupportSeeds :: Shape -> Int -> [Int]
shiftedWideSupportSeeds shape size =
  case shape of
    Chain ->
      [max bottomKey (size `div` 4), max bottomKey ((3 * size) `div` 4)]
    Fan ->
      take 16 (drop 16 (atomKeys size) <> atomKeys size)
    BooleanCube ->
      take 32 (drop 32 (booleanCubeMiddleLayer size) <> booleanCubeMiddleLayer size)
    DenseGrid ->
      take 32 (drop 32 (gridMiddleLayer size) <> gridMiddleLayer size)

booleanCubeMiddleLayer :: Int -> [Int]
booleanCubeMiddleLayer size =
  [ keyValue
  | keyValue <- keys size,
    popCount keyValue == targetWeight
  ]
  where
    targetWeight =
      length (takeWhile (< size) (iterate (* 2) 1)) `quot` 2

joinSeedStep :: Shape -> Int -> Int -> Int
joinSeedStep shape size =
  case shape of
    Chain ->
      max (fixpointSeed shape size)
    Fan ->
      fanJoinSeed size (fixpointSeed shape size)
    BooleanCube ->
      (.|. fixpointSeed shape size)
    DenseGrid ->
      gridJoinSeed size (fixpointSeed shape size)

meetSeedStep :: Shape -> Int -> Int -> Int
meetSeedStep shape size =
  case shape of
    Chain ->
      min (fixpointSeed shape size)
    Fan ->
      fanMeetSeed size (fixpointSeed shape size)
    BooleanCube ->
      (.&. fixpointSeed shape size)
    DenseGrid ->
      gridMeetSeed size (fixpointSeed shape size)

fixpointSeed :: Shape -> Int -> Int
fixpointSeed shape size =
  case shape of
    Chain -> size `quot` 2
    Fan -> min (topKey size) 1
    BooleanCube ->
      foldl' (.|.) 0 (take everyOtherBit (takeWhile (< size) (iterate (* 2) 1)))
      where
        everyOtherBit =
          max 1 (length (takeWhile (< size) (iterate (* 2) 1)) `quot` 2)
    DenseGrid ->
      gridKey size (gridHeight size `quot` 2) (gridWidth size `quot` 2)

fanJoinSeed :: Int -> Int -> Int -> Int
fanJoinSeed size seed value
  | value == bottomKey = seed
  | value == seed = seed
  | value == topKey size = topKey size
  | otherwise = topKey size

fanMeetSeed :: Int -> Int -> Int -> Int
fanMeetSeed size seed value
  | value == topKey size = seed
  | value == seed = seed
  | value == bottomKey = bottomKey
  | otherwise = bottomKey

gridJoinSeed :: Int -> Int -> Int -> Int
gridJoinSeed size seed value =
  gridKey
    size
    (max (gridRow size seed) (gridRow size value))
    (max (gridColumn size seed) (gridColumn size value))

gridMeetSeed :: Int -> Int -> Int -> Int
gridMeetSeed size seed value =
  gridKey
    size
    (min (gridRow size seed) (gridRow size value))
    (min (gridColumn size seed) (gridColumn size value))

gridUpperCovers :: Int -> Int -> [Int]
gridUpperCovers size keyValue =
  [ gridKey size nextRow column
  | nextRow < gridHeight size
  ]
    <> [ gridKey size row nextColumn
       | nextColumn < width
       ]
  where
    width = gridWidth size
    row = gridRow size keyValue
    column = gridColumn size keyValue
    nextRow = row + 1
    nextColumn = column + 1

gridKeyLeq :: Int -> Int -> Int -> Bool
gridKeyLeq size leftKey rightKey =
  gridRow size leftKey <= gridRow size rightKey
    && gridColumn size leftKey <= gridColumn size rightKey

tallGridUpperCovers :: Int -> Int -> [Int]
tallGridUpperCovers height keyValue =
  [ tallGridKey nextRow column
  | nextRow < height
  ]
    <> [ tallGridKey row nextColumn
       | nextColumn < tallGridWidth
       ]
  where
    row = tallGridRow keyValue
    column = tallGridColumn keyValue
    nextRow = row + 1
    nextColumn = column + 1

tallGridKey :: Int -> Int -> Int
tallGridKey row column =
  row * tallGridWidth + column

tallGridRow :: Int -> Int
tallGridRow keyValue =
  keyValue `quot` tallGridWidth

tallGridColumn :: Int -> Int
tallGridColumn keyValue =
  keyValue `rem` tallGridWidth

tallGridElementCount :: Int -> Int
tallGridElementCount height =
  height * tallGridWidth

tallGridTopKey :: Int -> Int
tallGridTopKey height =
  tallGridElementCount height - 1

tallGridWidth :: Int
tallGridWidth = 2

gridMiddleLayer :: Int -> [Int]
gridMiddleLayer size =
  [ keyValue
  | keyValue <- keys size,
    gridRow size keyValue + gridColumn size keyValue == targetRank
  ]
  where
    targetRank =
      (gridHeight size + gridWidth size - 2) `quot` 2

gridKey :: Int -> Int -> Int -> Int
gridKey size row column =
  row * gridWidth size + column

gridRow :: Int -> Int -> Int
gridRow size keyValue =
  keyValue `quot` gridWidth size

gridColumn :: Int -> Int -> Int
gridColumn size keyValue =
  keyValue `rem` gridWidth size

gridHeight :: Int -> Int
gridHeight size =
  size `quot` gridWidth size

gridWidth :: Int -> Int
gridWidth size =
  foldl' selectFactor 1 [1 .. size]
  where
    selectFactor best candidate
      | candidate * candidate <= size && size `rem` candidate == 0 = candidate
      | otherwise = best

shapes :: [Shape]
shapes = [Chain, Fan, BooleanCube, DenseGrid]

compileSizes :: [Int]
compileSizes = [16, 64, 128]

querySizes :: [Int]
querySizes = [16, 64, 128, 256]

tablelessTallGridCompileHeights :: [Int]
tablelessTallGridCompileHeights = [32, 64, 128]

tablelessTallGridQueryHeights :: [Int]
tablelessTallGridQueryHeights = [32, 64, 128]

tablelessLimits :: ContextCompileLimits
tablelessLimits =
  defaultContextCompileLimits {cclMaximumBinaryTableBytes = Just 0}

universe :: Int -> Set.Set Int
universe = Set.fromAscList . keys

keys :: Int -> [Int]
keys size = [0 .. size - 1]

atomKeys :: Int -> [Int]
atomKeys size = [1 .. size - 2]

topKey :: Int -> Int
topKey size = size - 1

bottomKey :: Int
bottomKey = 0

shapeLabel :: Shape -> String
shapeLabel shape =
  case shape of
    Chain -> "chain-dense"
    Fan -> "fan-sparse"
    BooleanCube -> "boolean-cube"
    DenseGrid -> "dense-grid"

caseLabel :: String -> Int -> String
caseLabel label size =
  label <> " n=" <> show size
