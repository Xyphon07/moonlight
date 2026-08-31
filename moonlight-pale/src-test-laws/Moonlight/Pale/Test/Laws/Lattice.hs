{-| Checked finite-lattice compilation and generated law suites. -}
module Moonlight.Pale.Test.Laws.Lattice
  ( FiniteLattice,
    LatticeBounds (..),
    FiniteLatticeError (..),
    FiniteLatticeLookupError (..),
    compileFiniteLattice,
    finiteLatticeJoin,
    finiteLatticeMeet,
    finiteLatticeLaws,
  )
where

import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Pale.Test.Laws.Suite (LawSuite, hUnitLaw, lawGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure)

type FiniteLattice :: Type -> Type
data FiniteLattice a = FiniteLattice
  { finiteLatticeName :: String,
    finiteLatticeValues :: !(Vector a),
    finiteLatticeIndex :: !(Map a Int),
    finiteLatticeJoinTable :: !(Vector Int),
    finiteLatticeMeetTable :: !(Vector Int),
    finiteLatticeBounds :: !(Maybe DenseLatticeBounds)
  }

type LatticeBounds :: Type -> Type
data LatticeBounds a = LatticeBounds
  { latticeBottom :: a,
    latticeTop :: a
  }
  deriving stock (Eq, Show)

data DenseLatticeBounds = DenseLatticeBounds
  { denseLatticeBottom :: !Int,
    denseLatticeTop :: !Int
  }

type FiniteLatticeError :: Type -> Type
data FiniteLatticeError a
  = DuplicateUniverseElement a !Int !Int
  | BottomOutsideUniverse a
  | TopOutsideUniverse a
  | JoinOutsideUniverse a a a
  | MeetOutsideUniverse a a a
  deriving stock (Eq, Show)

type FiniteLatticeLookupError :: Type -> Type
data FiniteLatticeLookupError a
  = UnknownFiniteLatticeElement a
  | FiniteLatticeTableIndexOutOfBounds !Int
  | FiniteLatticeValueIndexOutOfBounds !Int
  deriving stock (Eq, Show)

data UniverseIndexCompilation a = UniverseIndexCompilation
  { compiledUniverseIndex :: !(Map a Int),
    universeIndexErrorsReversed :: ![FiniteLatticeError a]
  }

data DenseTableCompilation a = DenseTableCompilation
  { joinIndicesReversed :: ![Int],
    meetIndicesReversed :: ![Int],
    tableErrorsReversed :: ![FiniteLatticeError a]
  }

compileFiniteLattice ::
  Ord a =>
  String ->
  NonEmpty a ->
  (a -> a -> a) ->
  (a -> a -> a) ->
  Maybe (LatticeBounds a) ->
  Either (NonEmpty (FiniteLatticeError a)) (FiniteLattice a)
compileFiniteLattice name universe joinOperation meetOperation bounds = do
  let values = Vector.fromList (NonEmpty.toList universe)
  valueIndex <- compileUniverseIndex values
  let (denseBounds, boundsErrors) = compileBounds valueIndex bounds
      tableCompilation =
        compileDenseTables valueIndex values joinOperation meetOperation
      compilationErrors =
        boundsErrors <> reverse (tableErrorsReversed tableCompilation)
  case NonEmpty.nonEmpty compilationErrors of
    Just errors -> Left errors
    Nothing ->
      Right
        FiniteLattice
          { finiteLatticeName = name,
            finiteLatticeValues = values,
            finiteLatticeIndex = valueIndex,
            finiteLatticeJoinTable =
              Vector.fromList (reverse (joinIndicesReversed tableCompilation)),
            finiteLatticeMeetTable =
              Vector.fromList (reverse (meetIndicesReversed tableCompilation)),
            finiteLatticeBounds = denseBounds
          }

compileUniverseIndex ::
  Ord a =>
  Vector a ->
  Either (NonEmpty (FiniteLatticeError a)) (Map a Int)
compileUniverseIndex values =
  let compilation =
        Vector.ifoldl'
          insertUniverseElement
          (UniverseIndexCompilation Map.empty [])
          values
   in case NonEmpty.nonEmpty (reverse (universeIndexErrorsReversed compilation)) of
        Just errors -> Left errors
        Nothing -> Right (compiledUniverseIndex compilation)

insertUniverseElement ::
  Ord a =>
  UniverseIndexCompilation a ->
  Int ->
  a ->
  UniverseIndexCompilation a
insertUniverseElement compilation duplicatePosition value =
  case Map.lookup value (compiledUniverseIndex compilation) of
    Just originalPosition ->
      compilation
        { universeIndexErrorsReversed =
            DuplicateUniverseElement value originalPosition duplicatePosition
              : universeIndexErrorsReversed compilation
        }
    Nothing ->
      compilation
        { compiledUniverseIndex =
            Map.insert value duplicatePosition (compiledUniverseIndex compilation)
        }

compileBounds ::
  Ord a =>
  Map a Int ->
  Maybe (LatticeBounds a) ->
  (Maybe DenseLatticeBounds, [FiniteLatticeError a])
compileBounds _ Nothing = (Nothing, [])
compileBounds valueIndex (Just bounds) =
  case
      ( Map.lookup (latticeBottom bounds) valueIndex,
        Map.lookup (latticeTop bounds) valueIndex
      )
    of
      (Just bottomIndex, Just topIndex) ->
        (Just (DenseLatticeBounds bottomIndex topIndex), [])
      (Nothing, Just _) ->
        (Nothing, [BottomOutsideUniverse (latticeBottom bounds)])
      (Just _, Nothing) ->
        (Nothing, [TopOutsideUniverse (latticeTop bounds)])
      (Nothing, Nothing) ->
        ( Nothing,
          [ BottomOutsideUniverse (latticeBottom bounds),
            TopOutsideUniverse (latticeTop bounds)
          ]
        )

compileDenseTables ::
  Ord a =>
  Map a Int ->
  Vector a ->
  (a -> a -> a) ->
  (a -> a -> a) ->
  DenseTableCompilation a
compileDenseTables valueIndex values joinOperation meetOperation =
  Vector.foldl'
    (\compilation leftValue ->
       Vector.foldl'
         (compileOperationPair valueIndex joinOperation meetOperation leftValue)
         compilation
         values
    )
    (DenseTableCompilation [] [] [])
    values

compileOperationPair ::
  Ord a =>
  Map a Int ->
  (a -> a -> a) ->
  (a -> a -> a) ->
  a ->
  DenseTableCompilation a ->
  a ->
  DenseTableCompilation a
compileOperationPair valueIndex joinOperation meetOperation leftValue compilation rightValue =
  let !joinResult = joinOperation leftValue rightValue
      !meetResult = meetOperation leftValue rightValue
      !joinIndex = Map.lookup joinResult valueIndex
      !meetIndex = Map.lookup meetResult valueIndex
      closureErrors =
        case (joinIndex, meetIndex) of
          (Nothing, Nothing) ->
            [ JoinOutsideUniverse leftValue rightValue joinResult,
              MeetOutsideUniverse leftValue rightValue meetResult
            ]
          (Nothing, Just _) ->
            [JoinOutsideUniverse leftValue rightValue joinResult]
          (Just _, Nothing) ->
            [MeetOutsideUniverse leftValue rightValue meetResult]
          (Just _, Just _) -> []
   in DenseTableCompilation
        { joinIndicesReversed =
            maybe
              (joinIndicesReversed compilation)
              (: joinIndicesReversed compilation)
              joinIndex,
          meetIndicesReversed =
            maybe
              (meetIndicesReversed compilation)
              (: meetIndicesReversed compilation)
              meetIndex,
          tableErrorsReversed =
            reverse closureErrors <> tableErrorsReversed compilation
        }

finiteLatticeJoin ::
  Ord a =>
  FiniteLattice a ->
  a ->
  a ->
  Either (FiniteLatticeLookupError a) a
finiteLatticeJoin lattice =
  evaluateFiniteLatticeOperation (finiteLatticeJoinTable lattice) lattice

finiteLatticeMeet ::
  Ord a =>
  FiniteLattice a ->
  a ->
  a ->
  Either (FiniteLatticeLookupError a) a
finiteLatticeMeet lattice =
  evaluateFiniteLatticeOperation (finiteLatticeMeetTable lattice) lattice

evaluateFiniteLatticeOperation ::
  Ord a =>
  Vector Int ->
  FiniteLattice a ->
  a ->
  a ->
  Either (FiniteLatticeLookupError a) a
evaluateFiniteLatticeOperation table lattice leftValue rightValue = do
  leftIndex <- lookupFiniteLatticeElement lattice leftValue
  rightIndex <- lookupFiniteLatticeElement lattice rightValue
  resultIndex <- denseOperationResult lattice table leftIndex rightIndex
  denseLatticeValue lattice resultIndex

lookupFiniteLatticeElement ::
  Ord a =>
  FiniteLattice a ->
  a ->
  Either (FiniteLatticeLookupError a) Int
lookupFiniteLatticeElement lattice value =
  case Map.lookup value (finiteLatticeIndex lattice) of
    Nothing -> Left (UnknownFiniteLatticeElement value)
    Just denseIndex -> Right denseIndex

denseLatticeValue ::
  FiniteLattice a ->
  Int ->
  Either (FiniteLatticeLookupError a) a
denseLatticeValue lattice denseIndex =
  case finiteLatticeValues lattice Vector.!? denseIndex of
    Nothing -> Left (FiniteLatticeValueIndexOutOfBounds denseIndex)
    Just value -> Right value

denseOperationResult ::
  FiniteLattice a ->
  Vector Int ->
  Int ->
  Int ->
  Either (FiniteLatticeLookupError a) Int
denseOperationResult lattice table leftIndex rightIndex =
  let cardinality = Vector.length (finiteLatticeValues lattice)
      tableIndex = leftIndex * cardinality + rightIndex
   in case table Vector.!? tableIndex of
        Nothing -> Left (FiniteLatticeTableIndexOutOfBounds tableIndex)
        Just resultIndex -> Right resultIndex

finiteLatticeLaws :: Show a => FiniteLattice a -> [LawSuite]
finiteLatticeLaws lattice =
  [ lawGroup
      (finiteLatticeName lattice <> " lattice laws")
      ( [ joinCommutativity lattice,
          meetCommutativity lattice,
          joinAssociativity lattice,
          meetAssociativity lattice,
          joinAbsorption lattice,
          meetAbsorption lattice,
          joinIdempotence lattice,
          meetIdempotence lattice
        ]
          <> boundedLaws lattice
      )
  ]

joinCommutativity :: Show a => FiniteLattice a -> LawSuite
joinCommutativity lattice =
  universalPairLaw lattice "join is commutative" $ \leftIndex leftValue rightIndex rightValue ->
    assertDenseEquation
      ("join operands " <> show (leftValue, rightValue))
      (denseOperationResult lattice (finiteLatticeJoinTable lattice) leftIndex rightIndex)
      (denseOperationResult lattice (finiteLatticeJoinTable lattice) rightIndex leftIndex)

meetCommutativity :: Show a => FiniteLattice a -> LawSuite
meetCommutativity lattice =
  universalPairLaw lattice "meet is commutative" $ \leftIndex leftValue rightIndex rightValue ->
    assertDenseEquation
      ("meet operands " <> show (leftValue, rightValue))
      (denseOperationResult lattice (finiteLatticeMeetTable lattice) leftIndex rightIndex)
      (denseOperationResult lattice (finiteLatticeMeetTable lattice) rightIndex leftIndex)

joinAssociativity :: Show a => FiniteLattice a -> LawSuite
joinAssociativity lattice =
  universalTripleLaw lattice "join is associative" $ \xIndex xValue yIndex yValue zIndex zValue ->
    let joinResult = denseOperationResult lattice (finiteLatticeJoinTable lattice)
     in assertDenseEquation
          ("join operands " <> show (xValue, yValue, zValue))
          (joinResult xIndex yIndex >>= (`joinResult` zIndex))
          (joinResult yIndex zIndex >>= joinResult xIndex)

meetAssociativity :: Show a => FiniteLattice a -> LawSuite
meetAssociativity lattice =
  universalTripleLaw lattice "meet is associative" $ \xIndex xValue yIndex yValue zIndex zValue ->
    let meetResult = denseOperationResult lattice (finiteLatticeMeetTable lattice)
     in assertDenseEquation
          ("meet operands " <> show (xValue, yValue, zValue))
          (meetResult xIndex yIndex >>= (`meetResult` zIndex))
          (meetResult yIndex zIndex >>= meetResult xIndex)

joinAbsorption :: Show a => FiniteLattice a -> LawSuite
joinAbsorption lattice =
  universalPairLaw lattice "absorption: join a (meet a b) = a" $ \xIndex xValue yIndex yValue ->
    let joinResult = denseOperationResult lattice (finiteLatticeJoinTable lattice)
        meetResult = denseOperationResult lattice (finiteLatticeMeetTable lattice)
     in assertDenseEquation
          ("absorption operands " <> show (xValue, yValue))
          (meetResult xIndex yIndex >>= joinResult xIndex)
          (Right xIndex)

meetAbsorption :: Show a => FiniteLattice a -> LawSuite
meetAbsorption lattice =
  universalPairLaw lattice "absorption: meet a (join a b) = a" $ \xIndex xValue yIndex yValue ->
    let joinResult = denseOperationResult lattice (finiteLatticeJoinTable lattice)
        meetResult = denseOperationResult lattice (finiteLatticeMeetTable lattice)
     in assertDenseEquation
          ("absorption operands " <> show (xValue, yValue))
          (joinResult xIndex yIndex >>= meetResult xIndex)
          (Right xIndex)

joinIdempotence :: Show a => FiniteLattice a -> LawSuite
joinIdempotence lattice =
  universeLaw lattice "join is idempotent" $ \denseIndex value ->
    assertDenseEquation
      ("join operand " <> show value)
      (denseOperationResult lattice (finiteLatticeJoinTable lattice) denseIndex denseIndex)
      (Right denseIndex)

meetIdempotence :: Show a => FiniteLattice a -> LawSuite
meetIdempotence lattice =
  universeLaw lattice "meet is idempotent" $ \denseIndex value ->
    assertDenseEquation
      ("meet operand " <> show value)
      (denseOperationResult lattice (finiteLatticeMeetTable lattice) denseIndex denseIndex)
      (Right denseIndex)

boundedLaws :: Show a => FiniteLattice a -> [LawSuite]
boundedLaws lattice =
  case finiteLatticeBounds lattice of
    Nothing -> []
    Just bounds ->
      [ universeLaw lattice "join with bottom is identity" $ \denseIndex value ->
          assertDenseEquation
            ("join bottom with " <> show value)
            ( denseOperationResult
                lattice
                (finiteLatticeJoinTable lattice)
                (denseLatticeBottom bounds)
                denseIndex
            )
            (Right denseIndex),
        universeLaw lattice "meet with top is identity" $ \denseIndex value ->
          assertDenseEquation
            ("meet top with " <> show value)
            ( denseOperationResult
                lattice
                (finiteLatticeMeetTable lattice)
                (denseLatticeTop bounds)
                denseIndex
            )
            (Right denseIndex)
      ]

universeLaw ::
  FiniteLattice a ->
  String ->
  (Int -> a -> Assertion) ->
  LawSuite
universeLaw lattice label check =
  hUnitLaw label $
    traverse_
      (\(denseIndex, value) -> check denseIndex value)
      (Vector.indexed (finiteLatticeValues lattice))

universalPairLaw ::
  FiniteLattice a ->
  String ->
  (Int -> a -> Int -> a -> Assertion) ->
  LawSuite
universalPairLaw lattice label check =
  hUnitLaw label $
    traverse_
      (\(leftIndex, leftValue) ->
         traverse_
           (\(rightIndex, rightValue) ->
              check leftIndex leftValue rightIndex rightValue
           )
           indexedValues
      )
      indexedValues
  where
    indexedValues = Vector.indexed (finiteLatticeValues lattice)

universalTripleLaw ::
  FiniteLattice a ->
  String ->
  (Int -> a -> Int -> a -> Int -> a -> Assertion) ->
  LawSuite
universalTripleLaw lattice label check =
  hUnitLaw label $
    traverse_
      (\(xIndex, xValue) ->
         traverse_
           (\(yIndex, yValue) ->
              traverse_
                (\(zIndex, zValue) ->
                   check xIndex xValue yIndex yValue zIndex zValue
                )
                indexedValues
           )
           indexedValues
      )
      indexedValues
  where
    indexedValues = Vector.indexed (finiteLatticeValues lattice)

assertDenseEquation ::
  Show a =>
  String ->
  Either (FiniteLatticeLookupError a) Int ->
  Either (FiniteLatticeLookupError a) Int ->
  Assertion
assertDenseEquation context leftResult rightResult =
  case (leftResult, rightResult) of
    (Left obstruction, _) ->
      assertFailure (context <> ": left dense evaluation failed: " <> show obstruction)
    (_, Left obstruction) ->
      assertFailure (context <> ": right dense evaluation failed: " <> show obstruction)
    (Right leftIndex, Right rightIndex) ->
      assertEqual context rightIndex leftIndex
