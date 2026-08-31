-- | Finite truncated simplicial sets generated from indexed simplices, with
-- typed well-formedness obstructions.
module Moonlight.Category.Pure.Simplicial.Set
  ( GeneratedSSet,
    generationBound,
    GeneratedSSetObstruction (..),
    GeneratedSSetCheck,
    IndexedSimplex,
    unindexSimplex,
    SomeIndexedSimplex (..),
    indexSimplexIn,
    mkGeneratedSSet,
    validateGeneratedSSet,
    generatedSimplicesAt,
    generatedSimplicesAtDimension,
    applyGeneratedFaceAtDimension,
    applyGeneratedDegeneracyAtDimension,
    TruncatedNormalizedSSet,
    truncationBound,
    TruncatedSSetObstruction (..),
    TruncatedSSetCheck,
    normalizeGeneratedSSet,
    mkTruncatedSSet,
    validateTruncatedSSet,
    simplicesAt,
    simplicesAtDimension,
    applyFaceAtDimension,
    applyDegeneracyAtDimension,
    faceIndexed,
    degeneracyIndexed,
  )
where

import Data.Function ((&))
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map
import GHC.TypeNats (KnownNat, Nat, type (+))
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Simplicial.Validation.Internal
  ( SimplicialLawCarrier (..),
    SimplicialLawIndices (..),
    SimplicialLawObstruction (..),
    checkObstructions,
    checkSimplicialLawsBy,
    dimensionsStrictlyBelow,
    simplicialLawEq,
  )
import Moonlight.Category.Pure.Simplicial.Set.Internal
  ( GeneratedSSet (..),
    TruncatedNormalizedSSet (..),
    trustedGeneratedSSet,
    trustedTruncatedNormalizedSSet,
  )
import Moonlight.Category.Pure.Simplicial.TypeLevel (Dimension (..), Fin, dimensionValue, withReifiedFinOffset)

type GeneratedSSetObstruction :: Type -> Type
data GeneratedSSetObstruction simplex
  = GeneratedFaceUndefined Natural Natural simplex
  | GeneratedFaceOutsideCarrier Natural Natural simplex simplex
  | GeneratedDegeneracyUndefined Natural Natural simplex
  | GeneratedDegeneracyOutsideCarrier Natural Natural simplex simplex
  | GeneratedFaceFaceMismatch Natural simplex Natural Natural (Maybe simplex) (Maybe simplex)
  | GeneratedDegeneracyDegeneracyMismatch Natural simplex Natural Natural (Maybe simplex) (Maybe simplex)
  | GeneratedFaceDegeneracyMismatch Natural simplex Natural Natural (Maybe simplex) (Maybe simplex)
  deriving stock (Eq, Show)

type GeneratedSSetCheck :: Type -> Type
type GeneratedSSetCheck simplex = Either (NonEmpty (GeneratedSSetObstruction simplex)) ()

type TruncatedSSetObstruction :: Type -> Type
data TruncatedSSetObstruction simplex
  = TruncatedRowOutsideBound Natural
  | TruncatedFaceUndefined Natural Natural simplex
  | TruncatedDegeneracyUndefined Natural Natural simplex
  | TruncatedLawViolation (SimplicialLawObstruction simplex)
  deriving stock (Eq, Show)

type TruncatedSSetCheck :: Type -> Type
type TruncatedSSetCheck simplex = Either (NonEmpty (TruncatedSSetObstruction simplex)) ()

type IndexedSimplex :: Nat -> Type -> Type
newtype IndexedSimplex (n :: Nat) simplex = IndexedSimplex
  { unindexSimplex :: simplex
  }
  deriving stock (Eq, Ord, Show)

type SomeIndexedSimplex :: Type -> Type
data SomeIndexedSimplex simplex where
  SomeIndexedSimplex :: KnownNat n => Dimension n -> IndexedSimplex n simplex -> SomeIndexedSimplex simplex

indexSimplexIn :: (Eq simplex, KnownNat n) => TruncatedNormalizedSSet simplex -> Dimension n -> simplex -> Maybe (IndexedSimplex n simplex)
indexSimplexIn simplicialSet dimensionWitness simplexValue =
  if simplexValue `elem` simplicesAt simplicialSet dimensionWitness
    then Just (IndexedSimplex simplexValue)
    else Nothing

mkGeneratedSSet ::
  Eq simplex =>
  Natural ->
  (Natural -> [simplex]) ->
  (forall n. KnownNat n => Dimension (n + 1) -> Fin (n + 2) -> simplex -> Maybe simplex) ->
  (forall n. KnownNat n => Dimension n -> Fin (n + 1) -> simplex -> Maybe simplex) ->
  Either (NonEmpty (GeneratedSSetObstruction simplex)) (GeneratedSSet simplex)
mkGeneratedSSet upperBound simplicesFunction faceFunction degeneracyFunction =
  let generatedSet = trustedGeneratedSSet upperBound simplicesFunction faceFunction degeneracyFunction
   in generatedSet <$ validateGeneratedSSet generatedSet

validateGeneratedSSet :: Eq simplex => GeneratedSSet simplex -> GeneratedSSetCheck simplex
validateGeneratedSSet generatedSet =
  checkObstructions
    ( faceClosureObstructions generatedSet
        <> degeneracyClosureObstructions generatedSet
        <> generatedLawObstructions generatedSet
    )

generatedSimplicesAt :: forall n simplex. KnownNat n => GeneratedSSet simplex -> Dimension n -> [simplex]
generatedSimplicesAt generatedSet dimensionWitness =
  generatedSimplicesAtDimension generatedSet (dimensionValue dimensionWitness)

generatedSimplicesAtDimension :: GeneratedSSet simplex -> Natural -> [simplex]
generatedSimplicesAtDimension generatedSet dimensionValue' =
  if dimensionValue' <= generationBound generatedSet
    then generatedSimplicesByDimension generatedSet dimensionValue'
    else []

applyGeneratedFaceAtDimension :: GeneratedSSet simplex -> Natural -> Natural -> simplex -> Maybe simplex
applyGeneratedFaceAtDimension generatedSet simplexDimension faceIndex simplexValue
  | simplexDimension == 0 = Nothing
  | simplexDimension > generationBound generatedSet = Nothing
  | faceIndex > simplexDimension = Nothing
  | otherwise = withReifiedFinOffset @2 (simplexDimension - 1) faceIndex $ \(_ :: Dimension n) finiteIndex ->
      generatedFaceMap generatedSet (Dimension @(n + 1)) finiteIndex simplexValue

applyGeneratedDegeneracyAtDimension :: GeneratedSSet simplex -> Natural -> Natural -> simplex -> Maybe simplex
applyGeneratedDegeneracyAtDimension generatedSet simplexDimension degeneracyIndex simplexValue
  | simplexDimension >= generationBound generatedSet = Nothing
  | degeneracyIndex > simplexDimension = Nothing
  | otherwise = withReifiedFinOffset @1 simplexDimension degeneracyIndex $ \dimensionWitness finiteIndex ->
      generatedDegeneracyMap generatedSet dimensionWitness finiteIndex simplexValue

generatedCarrierContains :: Eq simplex => GeneratedSSet simplex -> Natural -> simplex -> Bool
generatedCarrierContains generatedSet dimensionValue' simplexValue =
  simplexValue `elem` generatedSimplicesAtDimension generatedSet dimensionValue'

faceClosureObstructions :: Eq simplex => GeneratedSSet simplex -> [GeneratedSSetObstruction simplex]
faceClosureObstructions generatedSet =
  [ obstruction
    | simplexDimension <- [1 .. generationBound generatedSet],
      simplexValue <- generatedSimplicesAtDimension generatedSet simplexDimension,
      faceIndex <- [0 .. simplexDimension],
      obstruction <-
        case applyGeneratedFaceAtDimension generatedSet simplexDimension faceIndex simplexValue of
          Nothing -> [GeneratedFaceUndefined simplexDimension faceIndex simplexValue]
          Just faceValue ->
            if generatedCarrierContains generatedSet (simplexDimension - 1) faceValue
              then []
              else [GeneratedFaceOutsideCarrier simplexDimension faceIndex simplexValue faceValue]
  ]

degeneracyClosureObstructions :: Eq simplex => GeneratedSSet simplex -> [GeneratedSSetObstruction simplex]
degeneracyClosureObstructions generatedSet =
  [ obstruction
    | simplexDimension <- dimensionsStrictlyBelow (generationBound generatedSet),
      simplexValue <- generatedSimplicesAtDimension generatedSet simplexDimension,
      degeneracyIndex <- [0 .. simplexDimension],
      obstruction <-
        case applyGeneratedDegeneracyAtDimension generatedSet simplexDimension degeneracyIndex simplexValue of
          Nothing -> [GeneratedDegeneracyUndefined simplexDimension degeneracyIndex simplexValue]
          Just degeneracyValue ->
            if generatedCarrierContains generatedSet (simplexDimension + 1) degeneracyValue
              then []
              else [GeneratedDegeneracyOutsideCarrier simplexDimension degeneracyIndex simplexValue degeneracyValue]
  ]

generatedLawCarrier :: GeneratedSSet simplex -> SimplicialLawCarrier simplex
generatedLawCarrier generatedSet =
  SimplicialLawCarrier
    { lawCarrierUpperBound = generationBound generatedSet,
      lawCarrierSimplicesAtDimension = generatedSimplicesAtDimension generatedSet,
      lawCarrierFaceAtDimension = applyGeneratedFaceAtDimension generatedSet,
      lawCarrierDegeneracyAtDimension = applyGeneratedDegeneracyAtDimension generatedSet
    }

generatedLawObstruction :: SimplicialLawObstruction simplex -> GeneratedSSetObstruction simplex
generatedLawObstruction obstruction =
  case lawObstructionIndices obstruction of
    FaceFaceIndices leftFaceIndex rightFaceIndex ->
      GeneratedFaceFaceMismatch
        (lawObstructionDimension obstruction)
        (lawObstructionSimplex obstruction)
        leftFaceIndex
        rightFaceIndex
        (lawObstructionLeftResult obstruction)
        (lawObstructionRightResult obstruction)
    DegeneracyDegeneracyIndices leftDegeneracyIndex rightDegeneracyIndex ->
      GeneratedDegeneracyDegeneracyMismatch
        (lawObstructionDimension obstruction)
        (lawObstructionSimplex obstruction)
        leftDegeneracyIndex
        rightDegeneracyIndex
        (lawObstructionLeftResult obstruction)
        (lawObstructionRightResult obstruction)
    FaceDegeneracyIndices faceIndex degeneracyIndex ->
      GeneratedFaceDegeneracyMismatch
        (lawObstructionDimension obstruction)
        (lawObstructionSimplex obstruction)
        faceIndex
        degeneracyIndex
        (lawObstructionLeftResult obstruction)
        (lawObstructionRightResult obstruction)

generatedLawObstructions :: Eq simplex => GeneratedSSet simplex -> [GeneratedSSetObstruction simplex]
generatedLawObstructions generatedSet =
  case checkSimplicialLawsBy simplicialLawEq (generatedLawCarrier generatedSet) of
    Right () -> []
    Left obstructions -> generatedLawObstruction <$> toList obstructions

canonicalizeDimension :: (Natural -> simplex -> Bool) -> Natural -> [simplex] -> [simplex]
canonicalizeDimension isDegenerate dimensionValue' simplices =
  simplices
    & filter (not . isDegenerate dimensionValue')

normalizeGeneratedSSet :: GeneratedSSet simplex -> TruncatedNormalizedSSet simplex
normalizeGeneratedSSet generatedSet =
  let upperBound = generationBound generatedSet
      canonicalRows =
        [0 .. upperBound]
          & foldr
            ( \dimensionValue' ->
                let canonicalRow =
                      canonicalizeDimension
                        (generatedDegenerateWitness generatedSet)
                        dimensionValue'
                        (generatedSimplicesByDimension generatedSet dimensionValue')
                 in if null canonicalRow
                      then id
                      else Map.insert dimensionValue' canonicalRow
            )
            Map.empty
   in trustedTruncatedNormalizedSSet
        upperBound
        canonicalRows
        (generatedFaceMap generatedSet)
        (generatedDegeneracyMap generatedSet)

mkTruncatedSSet ::
  Eq simplex =>
  Natural ->
  [(Natural, [simplex])] ->
  (forall n. KnownNat n => Dimension (n + 1) -> Fin (n + 2) -> simplex -> Maybe simplex) ->
  (forall n. KnownNat n => Dimension n -> Fin (n + 1) -> simplex -> Maybe simplex) ->
  (Natural -> simplex -> Bool) ->
  Either (NonEmpty (TruncatedSSetObstruction simplex)) (TruncatedNormalizedSSet simplex)
mkTruncatedSSet upperBound levelRows faceFunction degeneracyFunction degeneracyWitness =
  let checkedRows =
        levelRows
          & foldr
            ( \(dimensionValue', simplices) (outsideBounds, accumulatedLevelMap) ->
                if dimensionValue' <= upperBound
                  then (outsideBounds, Map.insertWith (<>) dimensionValue' simplices accumulatedLevelMap)
                  else (TruncatedRowOutsideBound dimensionValue' : outsideBounds, accumulatedLevelMap)
            )
            ([], Map.empty)
      (rowObstructions, levelMap) = checkedRows
      canonicalRows =
        Map.mapWithKey
          (\dimensionValue' -> filter (not . degeneracyWitness dimensionValue'))
          levelMap
          & Map.filter (not . null)
      simplicialSet = trustedTruncatedNormalizedSSet upperBound canonicalRows faceFunction degeneracyFunction
   in simplicialSet <$ checkObstructions (rowObstructions <> truncatedValidationObstructions simplicialSet)

validateTruncatedSSet :: Eq simplex => TruncatedNormalizedSSet simplex -> TruncatedSSetCheck simplex
validateTruncatedSSet =
  checkObstructions . truncatedValidationObstructions

truncatedLawCarrier :: TruncatedNormalizedSSet simplex -> SimplicialLawCarrier simplex
truncatedLawCarrier simplicialSet =
  SimplicialLawCarrier
    { lawCarrierUpperBound = truncationBound simplicialSet,
      lawCarrierSimplicesAtDimension = simplicesAtDimension simplicialSet,
      lawCarrierFaceAtDimension = applyFaceAtDimension simplicialSet,
      lawCarrierDegeneracyAtDimension = applyDegeneracyAtDimension simplicialSet
    }

truncatedLawObstructions :: Eq simplex => TruncatedNormalizedSSet simplex -> [TruncatedSSetObstruction simplex]
truncatedLawObstructions simplicialSet =
  case checkSimplicialLawsBy simplicialLawEq (truncatedLawCarrier simplicialSet) of
    Right () -> []
    Left obstructions -> TruncatedLawViolation <$> toList obstructions

truncatedFaceTotalityObstructions :: TruncatedNormalizedSSet simplex -> [TruncatedSSetObstruction simplex]
truncatedFaceTotalityObstructions simplicialSet =
  [ TruncatedFaceUndefined simplexDimension faceIndex simplexValue
  | simplexDimension <- [1 .. truncationBound simplicialSet],
    simplexValue <- simplicesAtDimension simplicialSet simplexDimension,
    faceIndex <- [0 .. simplexDimension],
    isNothing (applyFaceAtDimension simplicialSet simplexDimension faceIndex simplexValue)
  ]

truncatedDegeneracyTotalityObstructions :: TruncatedNormalizedSSet simplex -> [TruncatedSSetObstruction simplex]
truncatedDegeneracyTotalityObstructions simplicialSet =
  [ TruncatedDegeneracyUndefined simplexDimension degeneracyIndex simplexValue
  | simplexDimension <- dimensionsStrictlyBelow (truncationBound simplicialSet),
    simplexValue <- simplicesAtDimension simplicialSet simplexDimension,
    degeneracyIndex <- [0 .. simplexDimension],
    isNothing (applyDegeneracyAtDimension simplicialSet simplexDimension degeneracyIndex simplexValue)
  ]

truncatedValidationObstructions :: Eq simplex => TruncatedNormalizedSSet simplex -> [TruncatedSSetObstruction simplex]
truncatedValidationObstructions simplicialSet =
  truncatedFaceTotalityObstructions simplicialSet
    <> truncatedDegeneracyTotalityObstructions simplicialSet
    <> truncatedLawObstructions simplicialSet

simplicesAt :: forall n simplex. KnownNat n => TruncatedNormalizedSSet simplex -> Dimension n -> [simplex]
simplicesAt simplicialSet dimensionWitness =
  Map.findWithDefault [] (dimensionValue dimensionWitness) (nondegenerateSimplicesByDimension simplicialSet)

simplicesAtDimension :: TruncatedNormalizedSSet simplex -> Natural -> [simplex]
simplicesAtDimension simplicialSet dimensionValue' =
  Map.findWithDefault [] dimensionValue' (nondegenerateSimplicesByDimension simplicialSet)

applyFaceAtDimension :: TruncatedNormalizedSSet simplex -> Natural -> Natural -> simplex -> Maybe simplex
applyFaceAtDimension simplicialSet simplexDimension faceIndex simplexValue
  | simplexDimension == 0 = Nothing
  | simplexDimension > truncationBound simplicialSet = Nothing
  | faceIndex > simplexDimension = Nothing
  | otherwise = withReifiedFinOffset @2 (simplexDimension - 1) faceIndex $ \(_ :: Dimension n) finiteIndex ->
      faceMap simplicialSet (Dimension @(n + 1)) finiteIndex simplexValue

applyDegeneracyAtDimension :: TruncatedNormalizedSSet simplex -> Natural -> Natural -> simplex -> Maybe simplex
applyDegeneracyAtDimension simplicialSet simplexDimension degeneracyIndex simplexValue
  | simplexDimension >= truncationBound simplicialSet = Nothing
  | degeneracyIndex > simplexDimension = Nothing
  | otherwise = withReifiedFinOffset @1 simplexDimension degeneracyIndex $ \dimensionWitness finiteIndex ->
      degeneracyMap simplicialSet dimensionWitness finiteIndex simplexValue

faceIndexed ::
  forall n simplex.
  KnownNat n =>
  TruncatedNormalizedSSet simplex ->
  Fin (n + 2) ->
  IndexedSimplex (n + 1) simplex ->
  Maybe (IndexedSimplex n simplex)
faceIndexed simplicialSet faceIndex (IndexedSimplex simplexValue) =
  IndexedSimplex <$> faceMap simplicialSet (Dimension @(n + 1)) faceIndex simplexValue

degeneracyIndexed ::
  forall n simplex.
  KnownNat n =>
  TruncatedNormalizedSSet simplex ->
  Fin (n + 1) ->
  IndexedSimplex n simplex ->
  Maybe (IndexedSimplex (n + 1) simplex)
degeneracyIndexed simplicialSet degeneracyIndex (IndexedSimplex simplexValue) =
  IndexedSimplex <$> degeneracyMap simplicialSet (Dimension @n) degeneracyIndex simplexValue
