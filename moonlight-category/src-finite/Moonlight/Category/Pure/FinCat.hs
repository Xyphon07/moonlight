{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Runtime-validated finite categories ('FinCat'): object and morphism handles and
-- ids, validation errors, morphism enumeration and folds, and bit-packed thin
-- constructions.
module Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinCatHandle,
    FinObjectId (..),
    FinGeneratorId (..),
    FinMorphismId (..),
    FinObj,
    FinMor,
    FinCompositor (..),
    FinTwoMor,
    FinCatValidationError (..),
    FinCatError (..),
    finCatIsThin,
    finCatObjects,
    objectCount,
    finCatMorphismCount,
    finCatNonIdentityMorphismCount,
    finCatMorphismCountFrom,
    finCatMorphismCountTo,
    finCatExplicitMorphismMapView,
    finCatExplicitCompositionMapView,
    finCatHandle,
    finObjCategoryHandle,
    finObjId,
    finMorCategoryHandle,
    finMorId,
    finMorSourceId,
    finMorTargetId,
    finTwoSource,
    finTwoTarget,
    mkFinCat,
    trustedFinCatWithGeneratorBasis,
    finCatMorphismIdByEndpoints,
    foldMapFinMorphisms,
    foldMapFinMorphismsFrom,
    foldMapFinMorphismsTo,
    trustedThinFinCatFromTransitiveEndpoints,
    trustedDenseThinFinCatFromReachabilityRows,
    denseThinEndpointMorphismsFromCategory,
    mkFinObject,
    mkFinMorphism,
    mkFinTwoMor,
    finMorDomObject,
    finMorCodObject,
    finObjectIdentityMor,
    finCatHomMorphism,
    allObjects,
    allMorphisms,
    allMorphismsFrom,
  )
where

import Data.Bits (bit, clearBit, popCount, shiftR, testBit, (.&.), (.|.))
import Data.Kind (Type)
import Data.List (find, genericTake, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import Data.Word (Word64)
import Data.Foldable (fold)
import Data.Function ((&))
import Moonlight.Category.Pure.Category (Category (..), composeMor)
import Moonlight.Category.Pure.FiniteComposable
  ( ComposableChain,
    FiniteComposableCategory (..),
    SizedComposableChain,
    appendComposableMorphism,
    chainTerminalObject,
    sizedComposableChain,
    singletonComposableChain,
  )
import Moonlight.Category.Pure.Higher (Bicategory (..), HigherCategory (..), TwoCategory (..))
import Moonlight.Core
  ( ExactEncoding,
    ExactEncodingAtom (..),
    ExactToken,
    Validation (..),
    exactAtomEncoding,
    exactSequenceEncoding,
    exactSequenceMapEncoding,
    exactTokenFromEncoding,
    validationToEither,
  )
import Moonlight.Category.Pure.Finite.DenseReachability (bitsToAscList, transposeBitRows)
import Numeric.Natural (Natural)

newtype FinObjectId = FinObjectId {unFinObjectId :: Int}
  deriving stock (Eq, Ord, Show)

newtype FinGeneratorId = FinGeneratorId {unFinGeneratorId :: Int}
  deriving stock (Eq, Ord, Show)

type FinMorphismId :: Type
data FinMorphismId
  = FinIdentityId FinObjectId
  | FinGeneratorMorphismId FinGeneratorId
  deriving stock (Eq, Ord, Show)

type FinCatHandle :: Type
newtype FinCatHandle = FinCatHandle ExactToken
  deriving stock (Eq, Ord, Show)

type FinCat :: Type
data FinCat
  = ExplicitFinCat ExplicitFinCatData
  | ThinFinCat ThinFinCatData
  | DenseThinFinCat DenseThinFinCatData

finCatIsThin :: FinCat -> Bool
finCatIsThin categoryValue =
  case categoryValue of
    ThinFinCat _ -> True
    DenseThinFinCat _ -> True
    ExplicitFinCat explicitData ->
      all ((<= 1) . length) (Map.elems (explicitFinCatMorphismMap explicitData))
        && all
          (\((sourceObject, targetObject), morphisms) -> sourceObject /= targetObject || null morphisms)
          (Map.toAscList (explicitFinCatMorphismMap explicitData))

type ExplicitFinCatData :: Type
data ExplicitFinCatData = ExplicitFinCatData
  { explicitFinCatHandle :: FinCatHandle,
    explicitFinCatObjects :: Set FinObjectId,
    explicitFinCatMorphismMap :: Map (FinObjectId, FinObjectId) [FinMorphismId],
    explicitFinCatCompositionMap :: Map (FinMorphismId, FinMorphismId) FinMorphismId,
    explicitFinCatMorphismIndex :: Map FinMorphismId (FinObjectId, FinObjectId),
    explicitFinCatMorphismsBySource :: Map FinObjectId [(FinObjectId, [FinMorphismId])],
    explicitFinCatMorphismsByTarget :: Map FinObjectId [(FinObjectId, [FinMorphismId])]
  }

type ThinFinCatData :: Type
data ThinFinCatData = ThinFinCatData
  { thinFinCatHandle :: FinCatHandle,
    thinFinCatObjects :: Set FinObjectId,
    thinFinCatEndpointMorphisms :: Map (FinObjectId, FinObjectId) FinMorphismId,
    thinFinCatMorphismIndex :: Map FinMorphismId (FinObjectId, FinObjectId),
    thinFinCatMorphismsBySource :: Map FinObjectId [(FinObjectId, FinMorphismId)],
    thinFinCatMorphismsByTarget :: Map FinObjectId [(FinObjectId, FinMorphismId)]
  }

type DenseThinFinCatData :: Type
data DenseThinFinCatData = DenseThinFinCatData
  { denseThinFinCatHandle :: FinCatHandle,
    denseThinFinCatObjects :: Set FinObjectId,
    denseThinFinCatObjectCount :: !Int,
    denseThinFinCatNonIdentityMorphismCount :: !Int,
    denseThinFinCatReachabilityRows :: Vector Integer,
    denseThinFinCatPredecessorRows :: Vector Integer,
    denseThinFinCatSourceCounts :: UVector.Vector Int,
    denseThinFinCatTargetCounts :: UVector.Vector Int,
    denseThinFinCatPrefixCounts :: UVector.Vector Int,
    denseThinFinCatEndpointIndex :: UVector.Vector Int,
    denseThinFinCatSourceColumn :: UVector.Vector Int,
    denseThinFinCatTargetColumn :: UVector.Vector Int
  }

finCatHandle :: FinCat -> FinCatHandle
finCatHandle category =
  case category of
    ExplicitFinCat explicitData -> explicitFinCatHandle explicitData
    ThinFinCat thinData -> thinFinCatHandle thinData
    DenseThinFinCat denseData -> denseThinFinCatHandle denseData

finCatObjects :: FinCat -> Set FinObjectId
finCatObjects category =
  case category of
    ExplicitFinCat explicitData -> explicitFinCatObjects explicitData
    ThinFinCat thinData -> thinFinCatObjects thinData
    DenseThinFinCat denseData -> denseThinFinCatObjects denseData

objectCount :: FinCat -> Int
objectCount category =
  case category of
    ExplicitFinCat explicitData -> Set.size (explicitFinCatObjects explicitData)
    ThinFinCat thinData -> Set.size (thinFinCatObjects thinData)
    DenseThinFinCat denseData -> denseThinFinCatObjectCount denseData

finCatNonIdentityMorphismCount :: FinCat -> Int
finCatNonIdentityMorphismCount category =
  case category of
    ExplicitFinCat explicitData ->
      explicitFinCatMorphismMap explicitData
        & Map.elems
        & fmap length
        & sum
    ThinFinCat thinData ->
      Map.size (thinFinCatEndpointMorphisms thinData)
    DenseThinFinCat denseData ->
      denseThinFinCatNonIdentityMorphismCount denseData

finCatMorphismCount :: FinCat -> Int
finCatMorphismCount category =
  objectCount category + finCatNonIdentityMorphismCount category

finCatMorphismCountFrom :: FinCat -> FinObjectId -> Int
finCatMorphismCountFrom category sourceId =
  case category of
    DenseThinFinCat denseData ->
      denseMorphismCountFrom denseData sourceId
    _
      | not (finCatHasObject category sourceId) -> 0
      | otherwise -> 1 + nonIdentityCount
  where
    nonIdentityCount =
      case category of
        ExplicitFinCat explicitData ->
          Map.findWithDefault [] sourceId (explicitFinCatMorphismsBySource explicitData)
            & fmap (length . snd)
            & sum
        ThinFinCat thinData ->
          Map.findWithDefault [] sourceId (thinFinCatMorphismsBySource thinData)
            & length
        DenseThinFinCat _ -> 0

finCatMorphismCountTo :: FinCat -> FinObjectId -> Int
finCatMorphismCountTo category targetId =
  case category of
    DenseThinFinCat denseData ->
      denseMorphismCountTo denseData targetId
    _
      | not (finCatHasObject category targetId) -> 0
      | otherwise -> 1 + nonIdentityCount
  where
    nonIdentityCount =
      case category of
        ExplicitFinCat explicitData ->
          Map.findWithDefault [] targetId (explicitFinCatMorphismsByTarget explicitData)
            & fmap (length . snd)
            & sum
        ThinFinCat thinData ->
          Map.findWithDefault [] targetId (thinFinCatMorphismsByTarget thinData)
            & length
        DenseThinFinCat _ -> 0

finCatExplicitMorphismMapView :: FinCat -> Map (FinObjectId, FinObjectId) [FinMorphismId]
finCatExplicitMorphismMapView category =
  case category of
    ExplicitFinCat explicitData -> explicitFinCatMorphismMap explicitData
    ThinFinCat thinData -> thinMorphismMap (thinFinCatEndpointMorphisms thinData)
    DenseThinFinCat denseData -> thinMorphismMap (denseThinEndpointMorphisms denseData)

-- | Materializes the full composition table, so the cost is bound below by the
-- output size: one entry per composable generator pair, which is @Θ(n³)@ for a
-- linear site on @n@ objects. No implementation of this contract can be
-- asymptotically faster. For a thin category the table is redundant — composition
-- is a function of endpoints already answered in @O(1)@ from @Θ(n²/w)@ storage by
-- the dense handle — so consumers that need composition at scale should query the
-- 'FinCat' directly and reserve this view for explicit witnesses (law tests,
-- 'Moonlight.Category.Pure.FinCat.mkFinCat' round-trips).
finCatExplicitCompositionMapView :: FinCat -> Map (FinMorphismId, FinMorphismId) FinMorphismId
finCatExplicitCompositionMapView category =
  case category of
    ExplicitFinCat explicitData -> explicitFinCatCompositionMap explicitData
    ThinFinCat thinData -> thinCompositionMap (thinCompositionEntries (thinFinCatEndpointMorphisms thinData))
    DenseThinFinCat denseData -> denseThinCompositionMap denseData

finCatHasObject :: FinCat -> FinObjectId -> Bool
finCatHasObject category =
  (`Set.member` finCatObjects category)

finCatMorphismIndex :: FinCat -> Map FinMorphismId (FinObjectId, FinObjectId)
finCatMorphismIndex category =
  case category of
    ExplicitFinCat explicitData -> explicitFinCatMorphismIndex explicitData
    ThinFinCat thinData -> thinFinCatMorphismIndex thinData
    DenseThinFinCat denseData -> denseThinMorphismIndex denseData


type FinObj :: Type
data FinObj = FinObj
  { finObjCategoryHandle :: FinCatHandle,
    finObjId :: FinObjectId
  }
  deriving stock (Show)

type FinMor :: Type
data FinMor = FinMor
  { finMorCategoryHandle :: FinCatHandle,
    finMorId :: FinMorphismId,
    finMorSourceId :: FinObjectId,
    finMorTargetId :: FinObjectId
  }
  deriving stock (Show)

type FinCompositor :: Type
data FinCompositor
  = FinStrictCompositor
  | FinAssociator FinMor FinMor FinMor
  | FinLeftUnitor FinMor
  | FinRightUnitor FinMor
  deriving stock (Eq, Ord, Show)

type FinTwoMor :: Type
data FinTwoMor = FinTwoMor
  { finTwoSource :: FinMor,
    finTwoTarget :: FinMor
  }
  deriving stock (Eq, Ord, Show)

type FinCatValidationError :: Type
data FinCatValidationError
  = MorphismEndpointOutsideObjects FinObjectId FinObjectId
  | ReservedIdentityMorphismId FinMorphismId
  | DuplicateMorphismId FinMorphismId
  | CompositionReferencesUnknownMorphism FinMorphismId
  | CompositionPairNotComposable FinMorphismId FinMorphismId
  | CompositionResultUnknownMorphism FinMorphismId
  | CompositionResultEndpointMismatch FinMorphismId FinMorphismId FinMorphismId
  | CompositionTableUsesIdentityKey FinMorphismId FinMorphismId
  | MissingCompositionForPair FinMorphismId FinMorphismId
  | AssociativityViolation FinMorphismId FinMorphismId FinMorphismId (Maybe FinMorphismId) (Maybe FinMorphismId)
  deriving stock (Eq, Show)

type FinCatError :: Type
data FinCatError
  = FinCatObjectNotDeclared FinCatHandle FinObjectId
  | FinCatMorphismNotDeclared FinCatHandle FinMorphismId
  | FinCatObjectWrongCategory FinCatHandle FinCatHandle FinObjectId
  | FinCatMorphismWrongCategory FinCatHandle FinCatHandle FinMorphismId
  | FinCatMorphismNotComposable FinMorphismId FinMorphismId FinObjectId FinObjectId
  | FinCatTwoMorphismBoundaryNotParallel FinMor FinMor
  | FinCatTwoMorphismNotVerticallyComposable FinTwoMor FinTwoMor
  | FinCatCompositionMissing FinMorphismId FinMorphismId
  | FinCatCompositionResultInvalid FinMorphismId FinObjectId FinObjectId
  deriving stock (Eq, Show)

type AssociativityMiddleCover :: Type
data AssociativityMiddleCover
  = GeneratorRestrictedMiddleCover (Set FinMorphismId)
  | ExhaustiveMiddleCover

instance Eq FinCat where
  left == right = finCatHandle left == finCatHandle right

instance Ord FinCat where
  compare left right = compare (finCatHandle left) (finCatHandle right)

instance Show FinCat where
  show category =
    case category of
      ExplicitFinCat explicitData ->
        "FinCat "
          <> show (explicitFinCatObjects explicitData)
          <> " "
          <> show (explicitFinCatMorphismMap explicitData)
          <> " "
          <> show (explicitFinCatCompositionMap explicitData)
      ThinFinCat thinData ->
        "ThinFinCat "
          <> show (thinFinCatObjects thinData)
          <> " "
          <> show (thinMorphismMap (thinFinCatEndpointMorphisms thinData))
      DenseThinFinCat denseData ->
        "DenseThinFinCat "
          <> show (denseThinFinCatObjects denseData)
          <> " morphisms="
          <> show (denseThinFinCatNonIdentityMorphismCount denseData)

instance Eq FinObj where
  left == right =
    finObjCategoryHandle left == finObjCategoryHandle right
      && finObjId left == finObjId right

instance Ord FinObj where
  compare left right =
    compare (finObjCategoryHandle left) (finObjCategoryHandle right)
      <> compare (finObjId left) (finObjId right)

instance Eq FinMor where
  left == right =
    finMorCategoryHandle left == finMorCategoryHandle right
      && finMorId left == finMorId right
      && finMorSourceId left == finMorSourceId right
      && finMorTargetId left == finMorTargetId right

instance Ord FinMor where
  compare left right =
    compare (finMorCategoryHandle left) (finMorCategoryHandle right)
      <> compare (finMorId left) (finMorId right)
      <> compare (finMorSourceId left) (finMorSourceId right)
      <> compare (finMorTargetId left) (finMorTargetId right)

encodingAtom :: ExactEncodingAtom -> ExactEncoding
encodingAtom =
  exactAtomEncoding

encodingInt :: Int -> ExactEncoding
encodingInt =
  encodingAtom . ExactInt

finCatCategoryEncoding :: Set FinObjectId -> Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> ExactEncoding
finCatCategoryEncoding objects morphismMap compositionMap =
  exactSequenceEncoding
    [ encodingAtom (ExactWord8 0),
      exactSequenceMapEncoding finCatObjectIdEncoding objects,
      exactSequenceMapEncoding finCatMorphismBucketEncoding (Map.toAscList morphismMap),
      exactSequenceMapEncoding finCatCompositionEntryEncoding (Map.toAscList compositionMap)
    ]

finCatDenseThinCategoryEncoding :: Set FinObjectId -> Vector Integer -> ExactEncoding
finCatDenseThinCategoryEncoding objects reachabilityRows =
  exactSequenceEncoding
    [ encodingAtom (ExactWord8 5),
      exactSequenceMapEncoding finCatObjectIdEncoding objects,
      exactSequenceMapEncoding (finCatReachabilityRowEncoding (Set.size objects)) reachabilityRows
    ]

finCatReachabilityRowEncoding :: Int -> Integer -> ExactEncoding
finCatReachabilityRowEncoding objectTotal bits =
  exactSequenceEncoding
    [ encodingAtom (ExactWord8 7),
      exactSequenceMapEncoding encodingInt (denseRowWordChunks objectTotal bits)
    ]

denseRowWordChunks :: Int -> Integer -> [Int]
denseRowWordChunks objectTotal bits =
  [0 .. ((objectTotal + 63) `div` 64) - 1]
    & fmap (\chunkIndex -> fromIntegral (fromIntegral (bits `shiftR` (64 * chunkIndex)) :: Word64))

finCatObjectIdEncoding :: FinObjectId -> ExactEncoding
finCatObjectIdEncoding (FinObjectId objectId) =
  exactSequenceEncoding [encodingAtom (ExactWord8 1), encodingInt objectId]

finCatGeneratorIdEncoding :: FinGeneratorId -> ExactEncoding
finCatGeneratorIdEncoding (FinGeneratorId generatorId) =
  exactSequenceEncoding [encodingAtom (ExactWord8 2), encodingInt generatorId]

finCatMorphismIdEncoding :: FinMorphismId -> ExactEncoding
finCatMorphismIdEncoding morphismId =
  case morphismId of
    FinIdentityId objectId ->
      exactSequenceEncoding [encodingAtom (ExactWord8 3), finCatObjectIdEncoding objectId]
    FinGeneratorMorphismId generatorId ->
      exactSequenceEncoding [encodingAtom (ExactWord8 4), finCatGeneratorIdEncoding generatorId]

finCatMorphismBucketEncoding :: ((FinObjectId, FinObjectId), [FinMorphismId]) -> ExactEncoding
finCatMorphismBucketEncoding ((sourceId, targetId), morphismIds) =
  exactSequenceEncoding
    [ finCatObjectIdEncoding sourceId,
      finCatObjectIdEncoding targetId,
      exactSequenceMapEncoding finCatMorphismIdEncoding morphismIds
    ]

finCatCompositionEntryEncoding :: ((FinMorphismId, FinMorphismId), FinMorphismId) -> ExactEncoding
finCatCompositionEntryEncoding ((left, right), result) =
  exactSequenceEncoding
    [ finCatMorphismIdEncoding left,
      finCatMorphismIdEncoding right,
      finCatMorphismIdEncoding result
    ]

mkFinCatHandle :: Set FinObjectId -> Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> FinCatHandle
mkFinCatHandle objects morphismMap compositionMap =
  FinCatHandle
    (exactTokenFromEncoding (finCatCategoryEncoding objects morphismMap compositionMap))

normalizeMorphismMap :: Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map (FinObjectId, FinObjectId) [FinMorphismId]
normalizeMorphismMap =
  Map.filter (not . null) . fmap sort

morphismBucketsBySource :: Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map FinObjectId [(FinObjectId, [FinMorphismId])]
morphismBucketsBySource morphismMap =
  morphismMap
    & Map.toAscList
    & fmap (\((sourceId, targetId), morphismIds) -> (sourceId, [(targetId, morphismIds)]))
    & Map.fromListWith (flip (<>))

morphismBucketsByTarget :: Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map FinObjectId [(FinObjectId, [FinMorphismId])]
morphismBucketsByTarget morphismMap =
  morphismMap
    & Map.toAscList
    & fmap (\((sourceId, targetId), morphismIds) -> (targetId, [(sourceId, morphismIds)]))
    & Map.fromListWith (flip (<>))

thinEndpointMorphismsBySource :: Map (FinObjectId, FinObjectId) FinMorphismId -> Map FinObjectId [(FinObjectId, FinMorphismId)]
thinEndpointMorphismsBySource endpointMorphisms =
  endpointMorphisms
    & Map.toAscList
    & fmap (\((sourceId, targetId), morphismId) -> (sourceId, [(targetId, morphismId)]))
    & Map.fromListWith (flip (<>))

thinEndpointMorphismsByTargetSimple :: Map (FinObjectId, FinObjectId) FinMorphismId -> Map FinObjectId [(FinObjectId, FinMorphismId)]
thinEndpointMorphismsByTargetSimple endpointMorphisms =
  endpointMorphisms
    & Map.toAscList
    & fmap (\((sourceId, targetId), morphismId) -> (targetId, [(sourceId, morphismId)]))
    & Map.fromListWith (flip (<>))

buildFinCat :: Set FinObjectId -> Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> FinCat
buildFinCat objects morphismMap compositionMap =
  let normalizedMorphismMap = normalizeMorphismMap morphismMap
      index = declaredMorphismIndex objects normalizedMorphismMap
   in ExplicitFinCat
        ExplicitFinCatData
          { explicitFinCatHandle = mkFinCatHandle objects normalizedMorphismMap compositionMap,
            explicitFinCatObjects = objects,
            explicitFinCatMorphismMap = normalizedMorphismMap,
            explicitFinCatCompositionMap = compositionMap,
            explicitFinCatMorphismIndex = index,
            explicitFinCatMorphismsBySource = morphismBucketsBySource normalizedMorphismMap,
            explicitFinCatMorphismsByTarget = morphismBucketsByTarget normalizedMorphismMap
          }

buildThinFinCat :: Set FinObjectId -> Map (FinObjectId, FinObjectId) FinMorphismId -> FinCat
buildThinFinCat objects endpointMorphisms =
  let morphismMap = thinMorphismMap endpointMorphisms
      index = declaredMorphismIndex objects morphismMap
   in ThinFinCat
        ThinFinCatData
          { thinFinCatHandle = mkFinCatHandle objects morphismMap Map.empty,
            thinFinCatObjects = objects,
            thinFinCatEndpointMorphisms = endpointMorphisms,
            thinFinCatMorphismIndex = index,
            thinFinCatMorphismsBySource = thinEndpointMorphismsBySource endpointMorphisms,
            thinFinCatMorphismsByTarget = thinEndpointMorphismsByTargetSimple endpointMorphisms
        }

buildDenseThinFinCat :: Set FinObjectId -> Vector Integer -> FinCat
buildDenseThinFinCat objects reachabilityRows =
  let objectTotal = Set.size objects
      canonicalRows = denseCanonicalReachabilityRows objectTotal reachabilityRows
      predecessorRows = transposeBitRows objectTotal canonicalRows
      sourceCounts = denseRowCounts canonicalRows
      targetCounts = denseRowCounts predecessorRows
      prefixCounts = densePrefixCountsFromCounts sourceCounts
      endpointIndex = denseEndpointIndex objectTotal canonicalRows prefixCounts
      pairs = denseReachablePairs objectTotal canonicalRows
      sourceColumn = UVector.fromList (fmap fst pairs)
      targetColumn = UVector.fromList (fmap snd pairs)
   in DenseThinFinCat
        DenseThinFinCatData
          { denseThinFinCatHandle = mkDenseThinFinCatHandle objects canonicalRows,
            denseThinFinCatObjects = objects,
            denseThinFinCatObjectCount = objectTotal,
            denseThinFinCatNonIdentityMorphismCount = UVector.sum sourceCounts,
            denseThinFinCatReachabilityRows = canonicalRows,
            denseThinFinCatPredecessorRows = predecessorRows,
            denseThinFinCatSourceCounts = sourceCounts,
            denseThinFinCatTargetCounts = targetCounts,
            denseThinFinCatPrefixCounts = prefixCounts,
            denseThinFinCatEndpointIndex = endpointIndex,
            denseThinFinCatSourceColumn = sourceColumn,
            denseThinFinCatTargetColumn = targetColumn
          }

denseCanonicalReachabilityRows :: Int -> Vector Integer -> Vector Integer
denseCanonicalReachabilityRows objectTotal reachabilityRows =
  Vector.generate
    objectTotal
    ( \sourceIndex ->
        reachabilityRows
          Vector.!? sourceIndex
          & maybe 0 (denseCanonicalReachabilityRow objectTotal sourceIndex)
    )

denseCanonicalReachabilityRow :: Int -> Int -> Integer -> Integer
denseCanonicalReachabilityRow objectTotal sourceIndex reachableBits =
  (reachableBits .&. denseBitMask objectTotal) `clearBit` sourceIndex

denseBitMask :: Int -> Integer
denseBitMask objectTotal =
  if objectTotal <= 0
    then 0
    else bit objectTotal - 1

mkDenseThinFinCatHandle :: Set FinObjectId -> Vector Integer -> FinCatHandle
mkDenseThinFinCatHandle objects reachabilityRows =
  FinCatHandle (exactTokenFromEncoding (finCatDenseThinCategoryEncoding objects reachabilityRows))

denseRowCounts :: Vector Integer -> UVector.Vector Int
denseRowCounts rows =
  rows
    & Vector.toList
    & fmap popCount
    & UVector.fromList

densePrefixCountsFromCounts :: UVector.Vector Int -> UVector.Vector Int
densePrefixCountsFromCounts =
  UVector.scanl (+) 0

denseEndpointIndex :: Int -> Vector Integer -> UVector.Vector Int -> UVector.Vector Int
denseEndpointIndex objectTotal reachabilityRows prefixCounts =
  UVector.concat
    ( [0 .. objectTotal - 1]
        & fmap
          ( \sourceIndex ->
              denseEndpointIndexRow
                objectTotal
                (fromMaybe 0 (reachabilityRows Vector.!? sourceIndex))
                (fromMaybe 0 (prefixCounts UVector.!? sourceIndex))
          )
    )

denseEndpointIndexRow :: Int -> Integer -> Int -> UVector.Vector Int
denseEndpointIndexRow objectTotal reachableBits prefixCount =
  UVector.fromListN
    objectTotal
    ( zipWith
        (\targetIndex rank -> if testBit reachableBits targetIndex then rank else -1)
        [0 .. objectTotal - 1]
        (scanl (\rank targetIndex -> if testBit reachableBits targetIndex then rank + 1 else rank) prefixCount [0 .. objectTotal - 1])
    )

denseReachablePairs :: Int -> Vector Integer -> [(Int, Int)]
denseReachablePairs objectTotal reachabilityRows =
  reachabilityRows
    & Vector.toList
    & zip [0 ..]
    >>= ( \(sourceIndex, reachableBits) ->
            bitsToAscList objectTotal reachableBits
              & fmap (\targetIndex -> (sourceIndex, targetIndex))
        )

denseThinEndpointMorphisms :: DenseThinFinCatData -> Map (FinObjectId, FinObjectId) FinMorphismId
denseThinEndpointMorphisms denseData =
  zip3
    [0 ..]
    (UVector.toList (denseThinFinCatSourceColumn denseData))
    (UVector.toList (denseThinFinCatTargetColumn denseData))
    & fmap
      ( \(morphismIndex, sourceIndex, targetIndex) ->
          ((FinObjectId sourceIndex, FinObjectId targetIndex), denseMorphismId morphismIndex)
      )
    & Map.fromDistinctAscList

denseThinCompositionMap :: DenseThinFinCatData -> Map (FinMorphismId, FinMorphismId) FinMorphismId
denseThinCompositionMap denseData =
  Map.fromDistinctAscList ([0 .. objectTotal - 1] >>= entriesForLeftSource)
  where
    objectTotal = denseThinFinCatObjectCount denseData
    reachabilityRows = denseThinFinCatReachabilityRows denseData
    predecessorRows = denseThinFinCatPredecessorRows denseData
    endpointIndex = denseThinFinCatEndpointIndex denseData

    morphismIds =
      Vector.generate (denseThinFinCatNonIdentityMorphismCount denseData) denseMorphismId

    indexAt sourceIndex targetIndex =
      UVector.unsafeIndex endpointIndex (sourceIndex * objectTotal + targetIndex)

    composedIdAt rightSourceIndex leftTargetIndex
      | rightSourceIndex == leftTargetIndex =
          Just (identityMorphismId (FinObjectId rightSourceIndex))
      | otherwise =
          let composedIndex = indexAt rightSourceIndex leftTargetIndex
           in if composedIndex >= 0
                then Just (Vector.unsafeIndex morphismIds composedIndex)
                else Nothing

    entriesForLeftSource leftSourceIndex =
      let rightEntries =
            bitsToAscList objectTotal (Vector.unsafeIndex predecessorRows leftSourceIndex)
              & fmap
                ( \rightSourceIndex ->
                    (rightSourceIndex, Vector.unsafeIndex morphismIds (indexAt rightSourceIndex leftSourceIndex))
                )
       in bitsToAscList objectTotal (Vector.unsafeIndex reachabilityRows leftSourceIndex)
            >>= entriesForLeft leftSourceIndex rightEntries

    entriesForLeft leftSourceIndex rightEntries leftTargetIndex =
      let leftId = Vector.unsafeIndex morphismIds (indexAt leftSourceIndex leftTargetIndex)
       in rightEntries
            & mapMaybe
              ( \(rightSourceIndex, rightId) ->
                  fmap
                    (\composedId -> ((leftId, rightId), composedId))
                    (composedIdAt rightSourceIndex leftTargetIndex)
              )

denseThinMorphismIndex :: DenseThinFinCatData -> Map FinMorphismId (FinObjectId, FinObjectId)
denseThinMorphismIndex denseData =
  let identityEntries =
        Set.toAscList (denseThinFinCatObjects denseData)
          & fmap (\objectId -> (identityMorphismId objectId, (objectId, objectId)))
      generatorEntries =
        denseThinEndpointMorphisms denseData
          & Map.toAscList
          & fmap (\(endpoints, morphismId) -> (morphismId, endpoints))
   in Map.fromList (identityEntries <> generatorEntries)

denseMorphismId :: Int -> FinMorphismId
denseMorphismId =
  FinGeneratorMorphismId . FinGeneratorId

denseMorphismIdIndex :: FinMorphismId -> Maybe Int
denseMorphismIdIndex morphismId =
  case morphismId of
    FinGeneratorMorphismId (FinGeneratorId indexValue)
      | indexValue >= 0 -> Just indexValue
    _ -> Nothing

denseMorphismEndpoints :: DenseThinFinCatData -> FinMorphismId -> Maybe (FinObjectId, FinObjectId)
denseMorphismEndpoints denseData morphismId =
  case morphismId of
    FinIdentityId objectId
      | denseObjectIdInBounds denseData objectId -> Just (objectId, objectId)
    _ -> do
      morphismIndex <- denseMorphismIdIndex morphismId
      sourceIndex <- denseThinFinCatSourceColumn denseData UVector.!? morphismIndex
      targetIndex <- denseThinFinCatTargetColumn denseData UVector.!? morphismIndex
      pure (FinObjectId sourceIndex, FinObjectId targetIndex)

denseObjectIdInBounds :: DenseThinFinCatData -> FinObjectId -> Bool
denseObjectIdInBounds denseData (FinObjectId objectIndex) =
  objectIndex >= 0 && objectIndex < denseThinFinCatObjectCount denseData

denseMorphismCountFrom :: DenseThinFinCatData -> FinObjectId -> Int
denseMorphismCountFrom denseData sourceId@(FinObjectId sourceIndex) =
  if denseObjectIdInBounds denseData sourceId
    then 1 + UVector.unsafeIndex (denseThinFinCatSourceCounts denseData) sourceIndex
    else 0

denseMorphismCountTo :: DenseThinFinCatData -> FinObjectId -> Int
denseMorphismCountTo denseData targetId@(FinObjectId targetIndex) =
  if denseObjectIdInBounds denseData targetId
    then 1 + UVector.unsafeIndex (denseThinFinCatTargetCounts denseData) targetIndex
    else 0

denseEndpointMorphism :: DenseThinFinCatData -> FinObjectId -> FinObjectId -> Maybe FinMorphismId
denseEndpointMorphism denseData sourceId targetId =
  denseMorphismId <$> denseEndpointMorphismIndex denseData sourceId targetId

denseEndpointMorphismIndex :: DenseThinFinCatData -> FinObjectId -> FinObjectId -> Maybe Int
denseEndpointMorphismIndex denseData sourceId@(FinObjectId sourceIndex) targetId@(FinObjectId targetIndex) =
  if denseObjectIdInBounds denseData sourceId && denseObjectIdInBounds denseData targetId && sourceId /= targetId
    then
      let endpointOffset = sourceIndex * denseThinFinCatObjectCount denseData + targetIndex
          morphismIndex = UVector.unsafeIndex (denseThinFinCatEndpointIndex denseData) endpointOffset
       in if morphismIndex >= 0 then Just morphismIndex else Nothing
    else Nothing

strictDenseThinFinCat :: Set FinObjectId -> Map (FinObjectId, FinObjectId) FinMorphismId -> Maybe FinCat
strictDenseThinFinCat objects endpointMorphisms = do
  reachabilityRows <- denseRowsFromEndpointMorphisms objects endpointMorphisms
  let denseCategory = buildDenseThinFinCat objects reachabilityRows
  if denseThinEndpointMorphismsFromCategory denseCategory == endpointMorphisms
    then Just denseCategory
    else Nothing

denseRowsFromEndpointMorphisms :: Set FinObjectId -> Map (FinObjectId, FinObjectId) FinMorphismId -> Maybe (Vector Integer)
denseRowsFromEndpointMorphisms objects endpointMorphisms =
  let objectIds = Set.toAscList objects
      expectedObjectIds = FinObjectId <$> [0 .. Set.size objects - 1]
   in if objectIds == expectedObjectIds
        then
          endpointMorphisms
            & Map.toAscList
            & traverse denseEndpointBit
            & fmap
              ( \endpointBits ->
                  endpointBits
                    & fmap (\(sourceIndex, targetBit) -> (sourceIndex, targetBit))
                    & Map.fromListWith (.|.)
                    & (\rowMap -> Vector.generate (Set.size objects) (\sourceIndex -> Map.findWithDefault 0 sourceIndex rowMap))
              )
        else Nothing

denseEndpointBit :: ((FinObjectId, FinObjectId), FinMorphismId) -> Maybe (Int, Integer)
denseEndpointBit ((FinObjectId sourceIndex, FinObjectId targetIndex), _)
  | sourceIndex >= 0 && targetIndex >= 0 && sourceIndex /= targetIndex = Just (sourceIndex, bit targetIndex)
  | otherwise = Nothing

denseThinEndpointMorphismsFromCategory :: FinCat -> Map (FinObjectId, FinObjectId) FinMorphismId
denseThinEndpointMorphismsFromCategory category =
  case category of
    DenseThinFinCat denseData -> denseThinEndpointMorphisms denseData
    _ -> Map.empty

strictThinEndpointMorphisms :: Map (FinObjectId, FinObjectId) [FinMorphismId] -> Maybe (Map (FinObjectId, FinObjectId) FinMorphismId)
strictThinEndpointMorphisms morphismMap =
  morphismMap
    & Map.toAscList
    & traverse strictThinEndpointMorphism
    & fmap Map.fromList

strictThinEndpointMorphism :: ((FinObjectId, FinObjectId), [FinMorphismId]) -> Maybe ((FinObjectId, FinObjectId), FinMorphismId)
strictThinEndpointMorphism ((sourceId, targetId), morphismIds) =
  case morphismIds of
    [morphismId]
      | sourceId /= targetId && not (isIdentityMorphismId morphismId) -> Just ((sourceId, targetId), morphismId)
    _ -> Nothing

identityMorphismId :: FinObjectId -> FinMorphismId
identityMorphismId = FinIdentityId

isIdentityMorphismId :: FinMorphismId -> Bool
isIdentityMorphismId morphismId =
  case morphismId of
    FinIdentityId _ -> True
    FinGeneratorMorphismId _ -> False

morphismEntries :: Map (FinObjectId, FinObjectId) [FinMorphismId] -> [(FinMorphismId, (FinObjectId, FinObjectId))]
morphismEntries morphismMap =
  Map.toList morphismMap
    & concatMap
      (\((sourceId, targetId), morphismIds) ->
         map (\morphismId -> (morphismId, (sourceId, targetId))) morphismIds
      )

duplicates :: Ord a => [a] -> [a]
duplicates values =
  values
    & foldr (\value -> Map.insertWith (+) value (1 :: Int)) Map.empty
    & Map.toAscList
    & foldMap (\(value, count) -> if count > 1 then [value] else [])

declaredMorphismIndex :: Set FinObjectId -> Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map FinMorphismId (FinObjectId, FinObjectId)
declaredMorphismIndex objects morphismMap =
  let identityEntries =
        Set.toAscList objects
          & map (\objectId -> (identityMorphismId objectId, (objectId, objectId)))
   in Map.fromList (identityEntries <> morphismEntries morphismMap)

lookupMorphismEndpoints :: Map FinMorphismId (FinObjectId, FinObjectId) -> FinMorphismId -> Maybe (FinObjectId, FinObjectId)
lookupMorphismEndpoints index morphismId = Map.lookup morphismId index

composeMorphismIds :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> FinMorphismId -> FinMorphismId -> Maybe FinMorphismId
composeMorphismIds index compositionMap left right = do
  (leftSourceId, _) <- lookupMorphismEndpoints index left
  (_, rightTargetId) <- lookupMorphismEndpoints index right
  if rightTargetId == leftSourceId
    then
      if isIdentityMorphismId left
        then pure right
        else
          if isIdentityMorphismId right
            then pure left
            else Map.lookup (left, right) compositionMap
    else Nothing

validationFromErrors :: [FinCatValidationError] -> Validation (NonEmpty FinCatValidationError) ()
validationFromErrors errors =
  case errors of
    [] -> Valid ()
    firstError : restErrors -> Invalid (firstError :| restErrors)

validateMorphismEndpoints :: Set FinObjectId -> Map (FinObjectId, FinObjectId) [FinMorphismId] -> Validation (NonEmpty FinCatValidationError) ()
validateMorphismEndpoints objects morphismMap =
  morphismMap
    & Map.keys
    & foldMap
      (\(sourceId, targetId) ->
         if Set.member sourceId objects && Set.member targetId objects
           then []
           else [MorphismEndpointOutsideObjects sourceId targetId]
      )
    & validationFromErrors

validateReservedIds :: Map (FinObjectId, FinObjectId) [FinMorphismId] -> Validation (NonEmpty FinCatValidationError) ()
validateReservedIds morphismMap =
  morphismEntries morphismMap
    & foldMap ((\morphismId -> if isIdentityMorphismId morphismId then [ReservedIdentityMorphismId morphismId] else []) . fst)
    & validationFromErrors

validateUniqueMorphismIds :: Map (FinObjectId, FinObjectId) [FinMorphismId] -> Validation (NonEmpty FinCatValidationError) ()
validateUniqueMorphismIds morphismMap =
  morphismEntries morphismMap
    & fmap fst
    & duplicates
    & fmap DuplicateMorphismId
    & validationFromErrors

validateCompositionTable :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Validation (NonEmpty FinCatValidationError) ()
validateCompositionTable index compositionMap =
  compositionMap
    & Map.toAscList
    & foldMap (compositionEntryErrors index)
    & validationFromErrors

compositionEntryErrors :: Map FinMorphismId (FinObjectId, FinObjectId) -> ((FinMorphismId, FinMorphismId), FinMorphismId) -> [FinCatValidationError]
compositionEntryErrors index ((left, right), result) =
  identityKeyErrors <> unknownErrors <> composabilityErrors <> endpointErrors
  where
    maybeLeftEndpoints = lookupMorphismEndpoints index left
    maybeRightEndpoints = lookupMorphismEndpoints index right
    maybeResultEndpoints = lookupMorphismEndpoints index result

    identityKeyErrors =
      if isIdentityMorphismId left || isIdentityMorphismId right
        then [CompositionTableUsesIdentityKey left right]
        else []

    unknownErrors =
      fold
        [ maybe [CompositionReferencesUnknownMorphism left] (const []) maybeLeftEndpoints,
          maybe [CompositionReferencesUnknownMorphism right] (const []) maybeRightEndpoints,
          maybe [CompositionResultUnknownMorphism result] (const []) maybeResultEndpoints
        ]

    composabilityErrors =
      case (maybeLeftEndpoints, maybeRightEndpoints) of
        (Just (leftSourceId, _), Just (_, rightTargetId))
          | rightTargetId /= leftSourceId -> [CompositionPairNotComposable left right]
        _ -> []

    endpointErrors =
      case (maybeLeftEndpoints, maybeRightEndpoints, maybeResultEndpoints) of
        (Just (_, leftTargetId), Just (rightSourceId, _), Just (resultSourceId, resultTargetId))
          | resultSourceId /= rightSourceId || resultTargetId /= leftTargetId -> [CompositionResultEndpointMismatch left right result]
        _ -> []

morphismsBySourceId :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map FinObjectId [FinMorphismId]
morphismsBySourceId index =
  index
    & Map.toAscList
    & fmap (\(morphismId, (sourceId, _)) -> (sourceId, [morphismId]))
    & Map.fromListWith (<>)

morphismsByTargetId :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map FinObjectId [FinMorphismId]
morphismsByTargetId index =
  index
    & Map.toAscList
    & fmap (\(morphismId, (_, targetId)) -> (targetId, [morphismId]))
    & Map.fromListWith (<>)

composablePairs :: Map FinMorphismId (FinObjectId, FinObjectId) -> [(FinMorphismId, FinMorphismId)]
composablePairs index =
  index
    & Map.toAscList
    & foldMap
      ( \(rightMorphism, (_, rightTargetId)) ->
          morphismsFrom rightTargetId
            & fmap (\leftMorphism -> (leftMorphism, rightMorphism))
      )
  where
    bySource = morphismsBySourceId index
    morphismsFrom sourceId = Map.findWithDefault [] sourceId bySource

composableTriples :: Map FinMorphismId (FinObjectId, FinObjectId) -> [(FinMorphismId, FinMorphismId, FinMorphismId)]
composableTriples index =
  index
    & Map.toAscList
    & foldMap triplesFromRight
  where
    bySource = morphismsBySourceId index
    morphismsFrom sourceId = Map.findWithDefault [] sourceId bySource
    triplesFromRight (rightMorphism, (_, rightTargetId)) =
      morphismsFrom rightTargetId
        & foldMap (triplesFromMiddle rightMorphism)
    triplesFromMiddle rightMorphism middleMorphism =
      case Map.lookup middleMorphism index of
        Nothing -> []
        Just (_, middleTargetId) ->
          morphismsFrom middleTargetId
            & fmap (\leftMorphism -> (leftMorphism, middleMorphism, rightMorphism))


composableTriplesWithMiddleIn :: Set FinMorphismId -> Map FinMorphismId (FinObjectId, FinObjectId) -> [(FinMorphismId, FinMorphismId, FinMorphismId)]
composableTriplesWithMiddleIn middleIds index =
  index
    & Map.toAscList
    & foldMap triplesFromMiddle
  where
    bySource = morphismsBySourceId index
    byTarget = morphismsByTargetId index
    morphismsFrom sourceId = Map.findWithDefault [] sourceId bySource
    morphismsTo targetId = Map.findWithDefault [] targetId byTarget

    triplesFromMiddle (middleMorphism, (middleSourceId, middleTargetId))
      | Set.member middleMorphism middleIds =
          morphismsFrom middleTargetId
            & foldMap
              ( \leftMorphism ->
                  morphismsTo middleSourceId
                    & fmap
                      ( \rightMorphism ->
                          (leftMorphism, middleMorphism, rightMorphism)
                      )
              )
      | otherwise = []

validateClosure :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Validation (NonEmpty FinCatValidationError) ()
validateClosure index compositionMap =
  composablePairs index
    & foldMap
      (\(left, right) ->
         if composeMorphismIds index compositionMap left right == Nothing
           then [MissingCompositionForPair left right]
           else []
      )
    & validationFromErrors

validateAssociativityExhaustive :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Validation (NonEmpty FinCatValidationError) ()
validateAssociativityExhaustive index compositionMap =
  composableTriples index
    & foldMap (associativityErrors index compositionMap)
    & validationFromErrors

validateAssociativity :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Validation (NonEmpty FinCatValidationError) ()
validateAssociativity index compositionMap =
  case associativityMiddleCover index compositionMap of
    GeneratorRestrictedMiddleCover middleIds ->
      composableTriplesWithMiddleIn middleIds index
        & foldMap (associativityErrors index compositionMap)
        & validationFromErrors
    ExhaustiveMiddleCover ->
      validateAssociativityExhaustive index compositionMap

validateAssociativityAtGenerators :: Set FinMorphismId -> Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Validation (NonEmpty FinCatValidationError) ()
validateAssociativityAtGenerators generatorIds index compositionMap =
  composableTriplesWithMiddleIn (Set.filter (not . isIdentityMorphismId) generatorIds) index
    & foldMap (associativityErrors index compositionMap)
    & validationFromErrors

associativityMiddleCover :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> AssociativityMiddleCover
associativityMiddleCover index compositionMap =
  let nonIdentityMorphisms =
        Map.keysSet index
          & Set.filter (not . isIdentityMorphismId)
      primaryGenerators =
        explicitCompositionGenerators index compositionMap
      generatedFromPrimary =
        generatedMorphisms index compositionMap primaryGenerators
      uncoveredMorphisms =
        Set.difference nonIdentityMorphisms generatedFromPrimary
      verifiedGenerators =
        Set.union primaryGenerators uncoveredMorphisms
      generatedFromVerified =
        generatedMorphisms index compositionMap verifiedGenerators
   in if Set.isSubsetOf nonIdentityMorphisms generatedFromVerified
        then GeneratorRestrictedMiddleCover verifiedGenerators
        else ExhaustiveMiddleCover

explicitCompositionGenerators :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Set FinMorphismId
explicitCompositionGenerators index compositionMap =
  let compositeResults =
        compositionMap
          & Map.elems
          & Set.fromList
          & Set.filter (not . isIdentityMorphismId)
   in Map.keysSet index
        & Set.filter (not . isIdentityMorphismId)
        & (`Set.difference` compositeResults)

generatedMorphisms :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Set FinMorphismId -> Set FinMorphismId
generatedMorphisms index compositionMap generators =
  let initialMorphisms =
        Set.union (identityMorphismSet index) generators
      closeMorphisms =
        closeGeneratedMorphisms index compositionMap
      generatedSequence =
        take (Map.size index + 1) (iterate closeMorphisms initialMorphisms)
      stableMorphisms =
        zip generatedSequence (drop 1 generatedSequence)
          & find (uncurry (==))
          & fmap snd
   in case stableMorphisms of
        Just morphisms -> morphisms
        Nothing ->
          Set.unions generatedSequence

identityMorphismSet :: Map FinMorphismId (FinObjectId, FinObjectId) -> Set FinMorphismId
identityMorphismSet index =
  Map.keysSet index
    & Set.filter isIdentityMorphismId

closeGeneratedMorphisms :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Set FinMorphismId -> Set FinMorphismId
closeGeneratedMorphisms _ compositionMap knownMorphisms =
  let composedMorphisms =
        compositionMap
          & Map.toAscList
          & foldMap
            ( \((leftMorphism, rightMorphism), composedMorphism) ->
                if Set.member leftMorphism knownMorphisms && Set.member rightMorphism knownMorphisms
                  then [composedMorphism]
                  else []
            )
          & Set.fromList
   in Set.union knownMorphisms composedMorphisms

associativityErrors :: Map FinMorphismId (FinObjectId, FinObjectId) -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> (FinMorphismId, FinMorphismId, FinMorphismId) -> [FinCatValidationError]
associativityErrors index compositionMap (left, middle, right) =
  let leftAssociated =
        composeMorphismIds index compositionMap left middle
          >>= (\composed -> composeMorphismIds index compositionMap composed right)
      rightAssociated =
        composeMorphismIds index compositionMap middle right
          >>= composeMorphismIds index compositionMap left
   in if leftAssociated == rightAssociated
        then []
        else [AssociativityViolation left middle right leftAssociated rightAssociated]

thinMorphismMap :: Map (FinObjectId, FinObjectId) FinMorphismId -> Map (FinObjectId, FinObjectId) [FinMorphismId]
thinMorphismMap =
  fmap (: [])

thinEndpointMorphismsByTarget ::
  Map (FinObjectId, FinObjectId) FinMorphismId ->
  Map FinObjectId [((FinObjectId, FinObjectId), FinMorphismId)]
thinEndpointMorphismsByTarget endpointMorphisms =
  endpointMorphisms
    & Map.toAscList
    & fmap (\entry@((_, targetId), _) -> (targetId, [entry]))
    & Map.fromListWith (<>)

thinCompositionEntries :: Map (FinObjectId, FinObjectId) FinMorphismId -> [((FinMorphismId, FinMorphismId), FinMorphismId)]
thinCompositionEntries endpointMorphisms =
  endpointMorphisms
    & thinCompositionExpectations
    & mapMaybe
      ( \(compositionKey, maybeComposedId) ->
          fmap (\composedId -> (compositionKey, composedId)) maybeComposedId
      )

thinCompositionExpectations :: Map (FinObjectId, FinObjectId) FinMorphismId -> [((FinMorphismId, FinMorphismId), Maybe FinMorphismId)]
thinCompositionExpectations endpointMorphisms =
  endpointMorphisms
    & Map.toAscList
    & foldMap entriesForLeft
  where
    rightMorphismsByTarget = thinEndpointMorphismsByTarget endpointMorphisms

    entriesForLeft ((leftSourceId, leftTargetId), leftId) =
      Map.findWithDefault [] leftSourceId rightMorphismsByTarget
        & foldMap (entryForRight leftTargetId leftId)

    entryForRight leftTargetId leftId ((rightSourceId, _), rightId) =
      [((leftId, rightId), thinCompositeMorphismId endpointMorphisms rightSourceId leftTargetId)]

thinCompositeMorphismId :: Map (FinObjectId, FinObjectId) FinMorphismId -> FinObjectId -> FinObjectId -> Maybe FinMorphismId
thinCompositeMorphismId endpointMorphisms sourceId targetId =
  if sourceId == targetId
    then Just (identityMorphismId sourceId)
    else Map.lookup (sourceId, targetId) endpointMorphisms

validateThinCompositionClosure :: Map (FinObjectId, FinObjectId) FinMorphismId -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Validation (NonEmpty FinCatValidationError) ()
validateThinCompositionClosure endpointMorphisms compositionMap =
  endpointMorphisms
    & thinCompositionExpectations
    & foldMap (thinCompositionExpectationErrors compositionMap)
    & validationFromErrors

thinCompositionExpectationErrors :: Map (FinMorphismId, FinMorphismId) FinMorphismId -> ((FinMorphismId, FinMorphismId), Maybe FinMorphismId) -> [FinCatValidationError]
thinCompositionExpectationErrors compositionMap ((left, right), maybeExpected) =
  case maybeExpected of
    Nothing -> [MissingCompositionForPair left right]
    Just expected ->
      case Map.lookup (left, right) compositionMap of
        Nothing -> [MissingCompositionForPair left right]
        Just actual
          | actual == expected -> []
          | otherwise -> [CompositionResultEndpointMismatch left right actual]

thinCompositionMap :: [((FinMorphismId, FinMorphismId), FinMorphismId)] -> Map (FinMorphismId, FinMorphismId) FinMorphismId
thinCompositionMap =
  Map.fromList

checkedThinFinCat :: Set FinObjectId -> Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Maybe (Either (NonEmpty FinCatValidationError) FinCat)
checkedThinFinCat objects morphismMap compositionMap =
  strictThinEndpointMorphisms morphismMap
    & fmap
      ( \endpointMorphisms ->
          trustedThinFinCatFromTransitiveEndpoints objects endpointMorphisms
            <$ validationToEither (validateThinCompositionClosure endpointMorphisms compositionMap)
      )

mkFinCat :: Set FinObjectId -> Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Either (NonEmpty FinCatValidationError) FinCat
mkFinCat =
  mkFinCatWithAssociativityValidation validateAssociativity

trustedFinCatWithGeneratorBasis :: Set FinMorphismId -> Set FinObjectId -> Map (FinObjectId, FinObjectId) [FinMorphismId] -> Map (FinMorphismId, FinMorphismId) FinMorphismId -> Either (NonEmpty FinCatValidationError) FinCat
trustedFinCatWithGeneratorBasis generatorIds =
  mkFinCatWithAssociativityValidation (validateAssociativityAtGenerators generatorIds)

mkFinCatWithAssociativityValidation ::
  ( Map FinMorphismId (FinObjectId, FinObjectId) ->
    Map (FinMorphismId, FinMorphismId) FinMorphismId ->
    Validation (NonEmpty FinCatValidationError) ()
  ) ->
  Set FinObjectId ->
  Map (FinObjectId, FinObjectId) [FinMorphismId] ->
  Map (FinMorphismId, FinMorphismId) FinMorphismId ->
  Either (NonEmpty FinCatValidationError) FinCat
mkFinCatWithAssociativityValidation associativityValidation objects morphismMap compositionMap =
  let normalizedMorphismMap = normalizeMorphismMap morphismMap
      index = declaredMorphismIndex objects normalizedMorphismMap
      basicValidation =
        validateMorphismEndpoints objects morphismMap
          *> validateReservedIds morphismMap
          *> validateUniqueMorphismIds morphismMap
          *> validateCompositionTable index compositionMap
      genericValidation =
        validateClosure index compositionMap
          *> associativityValidation index compositionMap
   in case validationToEither basicValidation of
        Left errors -> Left errors
        Right () ->
          case checkedThinFinCat objects normalizedMorphismMap compositionMap of
            Just checkedThinCategory -> checkedThinCategory
            Nothing -> buildFinCat objects normalizedMorphismMap compositionMap <$ validationToEither genericValidation

trustedThinFinCatFromTransitiveEndpoints :: Set FinObjectId -> Map (FinObjectId, FinObjectId) FinMorphismId -> FinCat
trustedThinFinCatFromTransitiveEndpoints objects endpointMorphisms =
  fromMaybe (buildThinFinCat objects endpointMorphisms) (strictDenseThinFinCat objects endpointMorphisms)

trustedDenseThinFinCatFromReachabilityRows :: Set FinObjectId -> Vector Integer -> FinCat
trustedDenseThinFinCatFromReachabilityRows =
  buildDenseThinFinCat

finCatMorphismIdByEndpoints :: FinCat -> FinObjectId -> FinObjectId -> Maybe FinMorphismId
finCatMorphismIdByEndpoints category sourceId targetId =
  case category of
    DenseThinFinCat denseData
      | sourceId == targetId && denseObjectIdInBounds denseData sourceId ->
          Just (identityMorphismId sourceId)
      | otherwise ->
          denseEndpointMorphism denseData sourceId targetId
    ExplicitFinCat explicitData
      | sourceId == targetId && Set.member sourceId (explicitFinCatObjects explicitData) ->
          Just (identityMorphismId sourceId)
      | otherwise ->
          case Map.lookup (sourceId, targetId) (explicitFinCatMorphismMap explicitData) of
            Just [morphismId] -> Just morphismId
            _ -> Nothing
    ThinFinCat thinData
      | sourceId == targetId && Set.member sourceId (thinFinCatObjects thinData) ->
          Just (identityMorphismId sourceId)
      | otherwise ->
          Map.lookup (sourceId, targetId) (thinFinCatEndpointMorphisms thinData)
{-# INLINABLE finCatMorphismIdByEndpoints #-}

mkFinObject :: FinCat -> FinObjectId -> Either FinCatError FinObj
mkFinObject category objectId =
  case category of
    DenseThinFinCat denseData ->
      if denseObjectIdInBounds denseData objectId
        then Right (mkFinObj category objectId)
        else Left (FinCatObjectNotDeclared (finCatHandle category) objectId)
    _ ->
      if Set.member objectId (finCatObjects category)
        then Right (mkFinObj category objectId)
        else Left (FinCatObjectNotDeclared (finCatHandle category) objectId)

mkFinObj :: FinCat -> FinObjectId -> FinObj
mkFinObj category objectId =
  FinObj
    { finObjCategoryHandle = finCatHandle category,
      finObjId = objectId
    }

mkFinMorphism :: FinCat -> FinMorphismId -> Either FinCatError FinMor
mkFinMorphism category morphismId =
  case category of
    DenseThinFinCat denseData ->
      case denseMorphismEndpoints denseData morphismId of
        Nothing -> Left (FinCatMorphismNotDeclared (finCatHandle category) morphismId)
        Just (sourceId, targetId) -> Right (mkFinMor category morphismId sourceId targetId)
    _ ->
      case Map.lookup morphismId (finCatMorphismIndex category) of
        Nothing -> Left (FinCatMorphismNotDeclared (finCatHandle category) morphismId)
        Just (sourceId, targetId) -> Right (mkFinMor category morphismId sourceId targetId)

mkFinMor :: FinCat -> FinMorphismId -> FinObjectId -> FinObjectId -> FinMor
mkFinMor category morphismId sourceId targetId =
  FinMor
    { finMorCategoryHandle = finCatHandle category,
      finMorId = morphismId,
      finMorSourceId = sourceId,
      finMorTargetId = targetId
    }

-- | Trusted O(1) view of a morphism's source object. Total: every t'FinMor' is built
-- by a validated path (its constructor is unexported), so the recorded source is
-- necessarily a declared object of the morphism's category — no re-validation needed.
finMorDomObject :: FinMor -> FinObj
finMorDomObject morphism =
  FinObj (finMorCategoryHandle morphism) (finMorSourceId morphism)
{-# INLINE finMorDomObject #-}

-- | Trusted O(1) view of a morphism's target object. Total, for the same reason as
-- 'finMorDomObject'.
finMorCodObject :: FinMor -> FinObj
finMorCodObject morphism =
  FinObj (finMorCategoryHandle morphism) (finMorTargetId morphism)
{-# INLINE finMorCodObject #-}

-- | Total O(1) identity morphism at an already validated object handle.
finObjectIdentityMor :: FinObj -> FinMor
finObjectIdentityMor objectValue =
  FinMor
    (finObjCategoryHandle objectValue)
    (identityMorphismId (finObjId objectValue))
    (finObjId objectValue)
    (finObjId objectValue)
{-# INLINE finObjectIdentityMor #-}

-- | The unique morphism between two endpoints when one exists (O(1) on the dense form),
-- including the identity when the endpoints coincide and the object is declared.
finCatHomMorphism :: FinCat -> FinObjectId -> FinObjectId -> Maybe FinMor
finCatHomMorphism category sourceId targetId =
  fmap
    (\morphismId -> FinMor (finCatHandle category) morphismId sourceId targetId)
    (finCatMorphismIdByEndpoints category sourceId targetId)
{-# INLINE finCatHomMorphism #-}

allObjects :: FinCat -> [FinObj]
allObjects category =
  fmap
    (\objectId -> FinObj (finCatHandle category) objectId)
    (Set.toAscList (finCatObjects category))

foldMapFinMorphisms :: Monoid monoid => (FinMor -> monoid) -> FinCat -> monoid
foldMapFinMorphisms morphismValue category =
  let objectIds = Set.toAscList (finCatObjects category)
      identityMorphisms =
        objectIds
          & foldMap (\objectId -> morphismValue (mkFinMor category (identityMorphismId objectId) objectId objectId))
      nonIdentityMorphisms =
        objectIds
          & foldMap (foldMapNonIdentityMorphismsFrom morphismValue category)
   in identityMorphisms <> nonIdentityMorphisms

foldMapFinMorphismsFrom :: Monoid monoid => (FinMor -> monoid) -> FinCat -> FinObjectId -> monoid
foldMapFinMorphismsFrom morphismValue category sourceId
  | not (finCatHasObject category sourceId) = mempty
  | otherwise =
      morphismValue (mkFinMor category (identityMorphismId sourceId) sourceId sourceId)
        <> foldMapNonIdentityMorphismsFrom morphismValue category sourceId

foldMapFinMorphismsTo :: Monoid monoid => (FinMor -> monoid) -> FinCat -> FinObjectId -> monoid
foldMapFinMorphismsTo morphismValue category targetId
  | not (finCatHasObject category targetId) = mempty
  | otherwise =
      morphismValue (mkFinMor category (identityMorphismId targetId) targetId targetId)
        <> foldMapNonIdentityMorphismsTo morphismValue category targetId

foldMapNonIdentityMorphismsFrom :: Monoid monoid => (FinMor -> monoid) -> FinCat -> FinObjectId -> monoid
foldMapNonIdentityMorphismsFrom morphismValue category sourceId =
  case category of
    ExplicitFinCat explicitData ->
      Map.findWithDefault [] sourceId (explicitFinCatMorphismsBySource explicitData)
        & foldMap
          ( \(targetId, morphismIds) ->
              morphismIds
                & foldMap (\morphismId -> morphismValue (mkFinMor category morphismId sourceId targetId))
          )
    ThinFinCat thinData ->
      Map.findWithDefault [] sourceId (thinFinCatMorphismsBySource thinData)
        & foldMap
          ( \(targetId, morphismId) ->
              morphismValue (mkFinMor category morphismId sourceId targetId)
          )
    DenseThinFinCat denseData ->
      denseMorphismsFrom category denseData sourceId
        & foldMap morphismValue

foldMapNonIdentityMorphismsTo :: Monoid monoid => (FinMor -> monoid) -> FinCat -> FinObjectId -> monoid
foldMapNonIdentityMorphismsTo morphismValue category targetId =
  case category of
    ExplicitFinCat explicitData ->
      Map.findWithDefault [] targetId (explicitFinCatMorphismsByTarget explicitData)
        & foldMap
          ( \(sourceId, morphismIds) ->
              morphismIds
                & foldMap (\morphismId -> morphismValue (mkFinMor category morphismId sourceId targetId))
          )
    ThinFinCat thinData ->
      Map.findWithDefault [] targetId (thinFinCatMorphismsByTarget thinData)
        & foldMap
          ( \(sourceId, morphismId) ->
              morphismValue (mkFinMor category morphismId sourceId targetId)
          )
    DenseThinFinCat denseData ->
      denseMorphismsTo category denseData targetId
        & foldMap morphismValue

allMorphisms :: FinCat -> [FinMor]
allMorphisms =
  foldMapFinMorphisms (: [])

allMorphismsFrom :: FinCat -> FinObj -> [FinMor]
allMorphismsFrom category sourceObject
  | finObjCategoryHandle sourceObject /= finCatHandle category = []
  | otherwise = foldMapFinMorphismsFrom (: []) category (finObjId sourceObject)

denseMorphismsFrom :: FinCat -> DenseThinFinCatData -> FinObjectId -> [FinMor]
denseMorphismsFrom category denseData sourceObjectId@(FinObjectId sourceIndex) =
  case (denseThinFinCatPrefixCounts denseData UVector.!? sourceIndex, denseThinFinCatReachabilityRows denseData Vector.!? sourceIndex) of
    (Just startIndex, Just reachableBits) ->
      bitsToAscList (denseThinFinCatObjectCount denseData) reachableBits
        & zip [startIndex ..]
        & fmap
          ( \(morphismIndex, targetIndex) ->
              FinMor (finCatHandle category) (denseMorphismId morphismIndex) sourceObjectId (FinObjectId targetIndex)
          )
    _ -> []

denseMorphismsTo :: FinCat -> DenseThinFinCatData -> FinObjectId -> [FinMor]
denseMorphismsTo category denseData targetObjectId@(FinObjectId targetIndex) =
  case denseThinFinCatPredecessorRows denseData Vector.!? targetIndex of
    Nothing -> []
    Just predecessorBits ->
      bitsToAscList (denseThinFinCatObjectCount denseData) predecessorBits
        >>= ( \sourceIndex ->
                case denseEndpointMorphism denseData (FinObjectId sourceIndex) targetObjectId of
                  Nothing -> []
                  Just morphismId -> [mkFinMor category morphismId (FinObjectId sourceIndex) targetObjectId]
            )

finMorphismsBySource :: FinCat -> Map FinObjectId [FinMor]
finMorphismsBySource category =
  allObjects category
    & fmap (\objectValue -> (finObjId objectValue, allMorphismsFrom category objectValue))
    & Map.fromList

finCatComposableChains :: FinCat -> Natural -> [SizedComposableChain FinCat]
finCatComposableChains category dimensionBound =
  genericTake (dimensionBound + 1) (finCatChainsByDimension category)
    & foldMap (fmap sizedComposableChain)

finCatNonDegenerateChainsByDimension :: FinCat -> Natural -> [[ComposableChain FinCat]]
finCatNonDegenerateChainsByDimension category dimensionBound =
  genericTake
    (dimensionBound + 1)
    (iterate extendNonIdentityChains seedChains)
  where
    nonIdentityBySource =
      allObjects category
        & fmap
          ( \objectValue ->
              ( finObjId objectValue,
                foldMapNonIdentityMorphismsFrom (: []) category (finObjId objectValue)
              )
          )
        & Map.fromList

    seedChains =
      allObjects category
        & fmap singletonComposableChain

    extendNonIdentityChains chains =
      chains
        >>= ( \chainValue ->
                Map.findWithDefault [] (finObjId (chainTerminalObject chainValue)) nonIdentityBySource
                  & mapMaybe
                    (either (const Nothing) Just . appendComposableMorphism category chainValue)
            )

finCatChainsByDimension :: FinCat -> [[ComposableChain FinCat]]
finCatChainsByDimension category =
  iterate
    (extendFinCatChains category (finMorphismsBySource category))
    (fmap singletonComposableChain (allObjects category))

extendFinCatChains :: FinCat -> Map FinObjectId [FinMor] -> [ComposableChain FinCat] -> [ComposableChain FinCat]
extendFinCatChains category morphismsBySource chains =
  chains
    >>= ( \chainValue ->
            mapMaybe
              (either (const Nothing) Just . appendComposableMorphism category chainValue)
              (Map.findWithDefault [] (finObjId (chainTerminalObject chainValue)) morphismsBySource)
        )

morphismIsDeclared :: FinCat -> FinObjectId -> FinObjectId -> FinMorphismId -> Bool
morphismIsDeclared category sourceId targetId morphismId =
  case category of
    DenseThinFinCat denseData ->
      denseMorphismEndpoints denseData morphismId == Just (sourceId, targetId)
    _ ->
      case Map.lookup morphismId (finCatMorphismIndex category) of
        Just endpoints -> endpoints == (sourceId, targetId)
        Nothing -> False

mkFinTwoMor :: FinMor -> FinMor -> Either FinCatError FinTwoMor
mkFinTwoMor sourceMorphism targetMorphism =
  if finMorCategoryHandle sourceMorphism /= finMorCategoryHandle targetMorphism
    then Left (FinCatMorphismWrongCategory (finMorCategoryHandle sourceMorphism) (finMorCategoryHandle targetMorphism) (finMorId targetMorphism))
    else
      if finMorSourceId sourceMorphism == finMorSourceId targetMorphism && finMorTargetId sourceMorphism == finMorTargetId targetMorphism
        then Right (FinTwoMor sourceMorphism targetMorphism)
        else Left (FinCatTwoMorphismBoundaryNotParallel sourceMorphism targetMorphism)

instance Category FinCat where
  type Ob FinCat = FinObj
  type Mor FinCat = FinMor
  type TwoMor FinCat = FinTwoMor
  type Compositor FinCat = FinCompositor
  type CategoryError FinCat = FinCatError

  identity category objectValue
    | finObjCategoryHandle objectValue /= finCatHandle category =
        Left (FinCatObjectWrongCategory (finCatHandle category) (finObjCategoryHandle objectValue) (finObjId objectValue))
    | otherwise =
        Right (FinMor (finCatHandle category) (identityMorphismId (finObjId objectValue)) (finObjId objectValue) (finObjId objectValue))

  compose category left right
    | finMorCategoryHandle left /= finCatHandle category =
        Left (FinCatMorphismWrongCategory (finCatHandle category) (finMorCategoryHandle left) (finMorId left))
    | finMorCategoryHandle right /= finCatHandle category =
        Left (FinCatMorphismWrongCategory (finCatHandle category) (finMorCategoryHandle right) (finMorId right))
    | finMorTargetId right /= finMorSourceId left =
        Left (FinCatMorphismNotComposable (finMorId left) (finMorId right) (finMorSourceId left) (finMorTargetId right))
    | isIdentityMorphismId (finMorId left) = Right (right, FinStrictCompositor)
    | isIdentityMorphismId (finMorId right) = Right (left, FinStrictCompositor)
    | otherwise =
        composeNonIdentityMorphisms category left right

  source category morphism
    | finMorCategoryHandle morphism /= finCatHandle category =
        Left (FinCatMorphismWrongCategory (finCatHandle category) (finMorCategoryHandle morphism) (finMorId morphism))
    | otherwise = mkFinObject category (finMorSourceId morphism)

  target category morphism
    | finMorCategoryHandle morphism /= finCatHandle category =
        Left (FinCatMorphismWrongCategory (finCatHandle category) (finMorCategoryHandle morphism) (finMorId morphism))
    | otherwise = mkFinObject category (finMorTargetId morphism)

composeNonIdentityMorphisms :: FinCat -> FinMor -> FinMor -> Either FinCatError (FinMor, FinCompositor)
composeNonIdentityMorphisms category left right =
  let composedSourceId = finMorSourceId right
      composedTargetId = finMorTargetId left
   in case category of
        ExplicitFinCat explicitData ->
          case Map.lookup (finMorId left, finMorId right) (explicitFinCatCompositionMap explicitData) of
            Nothing -> Left (FinCatCompositionMissing (finMorId left) (finMorId right))
            Just composedId ->
              if morphismIsDeclared category composedSourceId composedTargetId composedId
                then Right (FinMor (finCatHandle category) composedId composedSourceId composedTargetId, FinStrictCompositor)
                else Left (FinCatCompositionResultInvalid composedId composedSourceId composedTargetId)
        ThinFinCat thinData ->
          if composedSourceId == composedTargetId
            then Right (FinMor (finCatHandle category) (identityMorphismId composedSourceId) composedSourceId composedTargetId, FinStrictCompositor)
            else
              case Map.lookup (composedSourceId, composedTargetId) (thinFinCatEndpointMorphisms thinData) of
                Nothing -> Left (FinCatCompositionMissing (finMorId left) (finMorId right))
                Just composedId -> Right (FinMor (finCatHandle category) composedId composedSourceId composedTargetId, FinStrictCompositor)
        DenseThinFinCat denseData ->
          if composedSourceId == composedTargetId && denseObjectIdInBounds denseData composedSourceId
            then Right (FinMor (finCatHandle category) (identityMorphismId composedSourceId) composedSourceId composedTargetId, FinStrictCompositor)
            else
              case denseEndpointMorphismIndex denseData composedSourceId composedTargetId of
                Nothing -> Left (FinCatCompositionMissing (finMorId left) (finMorId right))
                Just composedIndex -> Right (FinMor (finCatHandle category) (denseMorphismId composedIndex) composedSourceId composedTargetId, FinStrictCompositor)

instance HigherCategory FinCat where
  source2 = finTwoSource
  target2 = finTwoTarget
  id2 morphism = FinTwoMor morphism morphism

  hCompose category left right = do
    sourceComposed <- composeMor @FinCat category (finTwoSource left) (finTwoSource right)
    targetComposed <- composeMor @FinCat category (finTwoTarget left) (finTwoTarget right)
    mkFinTwoMor sourceComposed targetComposed

  vCompose category left right
    | finMorCategoryHandle (finTwoSource left) /= finCatHandle category =
        Left (FinCatMorphismWrongCategory (finCatHandle category) (finMorCategoryHandle (finTwoSource left)) (finMorId (finTwoSource left)))
    | finMorCategoryHandle (finTwoTarget left) /= finCatHandle category =
        Left (FinCatMorphismWrongCategory (finCatHandle category) (finMorCategoryHandle (finTwoTarget left)) (finMorId (finTwoTarget left)))
    | finMorCategoryHandle (finTwoSource right) /= finCatHandle category =
        Left (FinCatMorphismWrongCategory (finCatHandle category) (finMorCategoryHandle (finTwoSource right)) (finMorId (finTwoSource right)))
    | finMorCategoryHandle (finTwoTarget right) /= finCatHandle category =
        Left (FinCatMorphismWrongCategory (finCatHandle category) (finMorCategoryHandle (finTwoTarget right)) (finMorId (finTwoTarget right)))
    | finTwoSource left /= finTwoTarget right =
        Left (FinCatTwoMorphismNotVerticallyComposable left right)
    | otherwise =
        mkFinTwoMor (finTwoSource right) (finTwoTarget left)

  compositor _ = FinAssociator

instance TwoCategory FinCat where
  -- FinCat 2-cells are invertible equality witnesses, not directed rewrites.
  inverse2 _ twoMorphism = Right (FinTwoMor (finTwoTarget twoMorphism) (finTwoSource twoMorphism))

instance Bicategory FinCat where
  leftUnitor _ = FinLeftUnitor
  rightUnitor _ = FinRightUnitor
  associator _ = FinAssociator

instance FiniteComposableCategory FinCat where
  enumerateObjects = allObjects
  enumerateMorphisms = allMorphisms
  enumerateMorphismsFrom = allMorphismsFrom
  enumerateComposableChains = finCatComposableChains
  enumerateNonDegenerateChainsByDimension = finCatNonDegenerateChainsByDimension
