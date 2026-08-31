{-# LANGUAGE TypeFamilies #-}

-- | Horns and Kan filling interfaces over generated simplicial sets.
module Moonlight.Category.Pure.Simplicial.Kan
  ( HornFrame,
    hornFrameDimension,
    hornFrameMissingFace,
    hornFrameFaces,
    Horn,
    hornDimension,
    hornMissingFace,
    hornFaces,
    IndexedHornFrame,
    indexedHornFrameMissingFace,
    indexedHornFrameFaces,
    IndexedHorn,
    indexedHornMissingFace,
    indexedHornFaces,
    SomeIndexedHorn (..),
    InnerHorn,
    innerHorn,
    HornFrameError (..),
    HornError (..),
    InnerHornError (..),
    HornIndexError (..),
    mkHornFrame,
    mkHorn,
    mkIndexedHornFrame,
    mkIndexedHorn,
    mkInnerHorn,
    indexedHornDimension,
    indexedHornToHorn,
    hornToIndexedHorn,
    isInnerHorn,
    InnerKan (..),
    KanComplex (..),
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.List (find, sort)
import Data.Maybe (listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, natVal, type (+))
import Moonlight.Category.Pure.Simplicial.TypeLevel (Dimension (..), Fin, finValue, mkFinOffset)
import Numeric.Natural (Natural)

type HornFrame :: Type -> Type
data HornFrame simplex = HornFrame
  { hornFrameDimension :: Natural,
    hornFrameMissingFace :: Natural,
    hornFrameFaces :: Map Natural simplex
  }

type Horn :: Type -> Type
newtype Horn simplex = Horn
  { hornToFrame :: HornFrame simplex
  }

type IndexedHornFrame :: Nat -> Type -> Type
data IndexedHornFrame (n :: Nat) simplex = IndexedHornFrame
  { indexedHornFrameMissingFace :: Fin (n + 2),
    indexedHornFrameFaces :: Map Natural simplex
  }

type IndexedHorn :: Nat -> Type -> Type
newtype IndexedHorn (n :: Nat) simplex = IndexedHorn
  { indexedHornToFrame :: IndexedHornFrame n simplex
  }

type SomeIndexedHorn :: Type -> Type
data SomeIndexedHorn simplex where
  SomeIndexedHorn :: KnownNat n => IndexedHorn n simplex -> SomeIndexedHorn simplex

type InnerHorn :: Type -> Type
newtype InnerHorn simplex = InnerHorn
  { innerHorn :: Horn simplex
  }

data HornFrameError
  = HornDimensionZero
  | HornMissingFaceOutOfBounds Natural Natural
  | HornDuplicateFace Natural
  | HornUnexpectedFace Natural Natural
  | HornSuppliedMissingFace Natural
  | HornMissingRequiredFaces [Natural]
  deriving stock (Eq, Show)

data HornError simplex
  = HornFrameInvalid HornFrameError
  | HornFaceDimensionMismatch Natural Natural Natural
  | HornOverlapUndefined Natural Natural (Maybe simplex) (Maybe simplex)
  | HornOverlapMismatch Natural Natural simplex simplex
  deriving stock (Eq, Show)

data InnerHornError
  = HornNotInner Natural Natural
  deriving stock (Eq, Show)

data HornIndexError
  = HornIndexDimensionMismatch Natural Natural
  | HornIndexFaceOutOfBounds Natural
  deriving stock (Eq, Show)

hornDimension :: Horn simplex -> Natural
hornDimension = hornFrameDimension . hornToFrame

hornMissingFace :: Horn simplex -> Natural
hornMissingFace = hornFrameMissingFace . hornToFrame

hornFaces :: Horn simplex -> Map Natural simplex
hornFaces = hornFrameFaces . hornToFrame

indexedHornMissingFace :: IndexedHorn n simplex -> Fin (n + 2)
indexedHornMissingFace = indexedHornFrameMissingFace . indexedHornToFrame

indexedHornFaces :: IndexedHorn n simplex -> Map Natural simplex
indexedHornFaces = indexedHornFrameFaces . indexedHornToFrame

faceMultiplicity :: [(Natural, simplex)] -> Map Natural Int
faceMultiplicity =
  foldr (Map.alter incrementFaceCount . fst) Map.empty
  where
    incrementFaceCount :: Maybe Int -> Maybe Int
    incrementFaceCount Nothing = Just 1
    incrementFaceCount (Just countValue) = Just (countValue + 1)

firstDuplicateFace :: [(Natural, simplex)] -> Maybe Natural
firstDuplicateFace indexedFaces =
  faceMultiplicity indexedFaces
    & Map.filter (> 1)
    & Map.keys
    & sort
    & listToMaybe

requiredFaceIndices :: Natural -> Natural -> [Natural]
requiredFaceIndices dimensionValue missingFace =
  filter (/= missingFace) [0 .. dimensionValue]

mkHornFrame :: Natural -> Natural -> [(Natural, simplex)] -> Either HornFrameError (HornFrame simplex)
mkHornFrame dimensionValue missingFace indexedFaces
  | dimensionValue == 0 = Left HornDimensionZero
  | missingFace > dimensionValue =
      Left (HornMissingFaceOutOfBounds dimensionValue missingFace)
  | Just duplicateFace <- firstDuplicateFace indexedFaces =
      Left (HornDuplicateFace duplicateFace)
  | otherwise =
      let facesMap = Map.fromList indexedFaces
          suppliedFaces = Map.keys facesMap
          requiredFaces = requiredFaceIndices dimensionValue missingFace
          missingRequiredFaces = filter (`Map.notMember` facesMap) requiredFaces
       in case find (> dimensionValue) suppliedFaces of
            Just unexpectedFace ->
              Left (HornUnexpectedFace dimensionValue unexpectedFace)
            Nothing
              | Map.member missingFace facesMap ->
                  Left (HornSuppliedMissingFace missingFace)
              | not (null missingRequiredFaces) ->
                  Left (HornMissingRequiredFaces missingRequiredFaces)
              | otherwise ->
                  Right
                    HornFrame
                      { hornFrameDimension = dimensionValue,
                        hornFrameMissingFace = missingFace,
                        hornFrameFaces = facesMap
                      }

compatibleFacePairs :: HornFrame simplex -> [(Natural, Natural, simplex, simplex)]
compatibleFacePairs frameValue =
  [ (lowerFace, upperFace, lowerSimplex, upperSimplex)
    | (lowerFace, lowerSimplex) <- Map.toAscList (hornFrameFaces frameValue),
      (upperFace, upperSimplex) <- Map.toAscList (hornFrameFaces frameValue),
      lowerFace < upperFace
  ]

validateHornOverlap :: Eq simplex => (Natural -> Natural -> simplex -> Maybe simplex) -> Natural -> (Natural, Natural, simplex, simplex) -> Either (HornError simplex) ()
validateHornOverlap faceAt faceDimension (lowerFace, upperFace, lowerSimplex, upperSimplex) =
  case (faceAt faceDimension lowerFace upperSimplex, faceAt faceDimension (upperFace - 1) lowerSimplex) of
    (Just leftValue, Just rightValue)
      | leftValue == rightValue -> Right ()
      | otherwise ->
          Left (HornOverlapMismatch lowerFace upperFace leftValue rightValue)
    (leftValue, rightValue) ->
      Left (HornOverlapUndefined lowerFace upperFace leftValue rightValue)

validateHornFaceDimension ::
  (simplex -> Natural) ->
  Natural ->
  (Natural, simplex) ->
  Either (HornError simplex) ()
validateHornFaceDimension simplexDimension expectedDimension (faceIndex, simplexValue) =
  let actualDimension = simplexDimension simplexValue
   in if actualDimension == expectedDimension
        then Right ()
        else Left (HornFaceDimensionMismatch faceIndex expectedDimension actualDimension)

mkHorn :: Eq simplex => (simplex -> Natural) -> (Natural -> Natural -> simplex -> Maybe simplex) -> Natural -> Natural -> [(Natural, simplex)] -> Either (HornError simplex) (Horn simplex)
mkHorn simplexDimension faceAt dimensionValue missingFace indexedFaces = do
  frameValue <- first HornFrameInvalid (mkHornFrame dimensionValue missingFace indexedFaces)
  traverse_ (validateHornFaceDimension simplexDimension (dimensionValue - 1)) (Map.toAscList (hornFrameFaces frameValue))
  traverse_ (validateHornOverlap faceAt (dimensionValue - 1)) (compatibleFacePairs frameValue)
  Right (Horn frameValue)

mkIndexedHornFrame :: forall n simplex. KnownNat n => Fin (n + 2) -> [(Fin (n + 2), simplex)] -> Either HornFrameError (IndexedHornFrame n simplex)
mkIndexedHornFrame missingFace indexedFaces =
  let dimensionValue = natVal (Proxy @n) + 1
      indexedFaceRows = map (\(faceIndex, simplexValue) -> (finValue faceIndex, simplexValue)) indexedFaces
   in do
        checkedFrame <- mkHornFrame dimensionValue (finValue missingFace) indexedFaceRows
        Right
          IndexedHornFrame
            { indexedHornFrameMissingFace = missingFace,
              indexedHornFrameFaces = hornFrameFaces checkedFrame
            }

mkIndexedHorn :: forall n simplex. (KnownNat n, Eq simplex) => (simplex -> Natural) -> (Natural -> Natural -> simplex -> Maybe simplex) -> Fin (n + 2) -> [(Fin (n + 2), simplex)] -> Either (HornError simplex) (IndexedHorn n simplex)
mkIndexedHorn simplexDimension faceAt missingFace indexedFaces = do
  let dimensionValue = natVal (Proxy @n) + 1
      indexedFaceRows = map (\(faceIndex, simplexValue) -> (finValue faceIndex, simplexValue)) indexedFaces
  _ <- mkHorn simplexDimension faceAt dimensionValue (finValue missingFace) indexedFaceRows
  frameValue <- first HornFrameInvalid (mkIndexedHornFrame missingFace indexedFaces)
  Right (IndexedHorn frameValue)

indexedHornDimension :: forall n simplex. KnownNat n => IndexedHorn n simplex -> Natural
indexedHornDimension _ = natVal (Proxy @n) + 1

indexedHornToHorn :: forall n simplex. KnownNat n => IndexedHorn n simplex -> Horn simplex
indexedHornToHorn indexedHornValue =
  Horn
    HornFrame
      { hornFrameDimension = indexedHornDimension indexedHornValue,
        hornFrameMissingFace = finValue (indexedHornMissingFace indexedHornValue),
        hornFrameFaces = indexedHornFaces indexedHornValue
      }

hornToIndexedHorn :: forall n simplex. KnownNat n => Horn simplex -> Either HornIndexError (IndexedHorn n simplex)
hornToIndexedHorn hornValue =
  if hornDimension hornValue == natVal (Proxy @n) + 1
    then do
      missingFace <-
        maybe
          (Left (HornIndexFaceOutOfBounds (hornMissingFace hornValue)))
          Right
          (mkFinOffset @n @2 (Dimension @n) (hornMissingFace hornValue))
      indexedFaces <-
        traverse
          (\(faceIndex, simplexValue) ->
             maybe
               (Left (HornIndexFaceOutOfBounds faceIndex))
               (\finiteFace -> Right (finiteFace, simplexValue))
               (mkFinOffset @n @2 (Dimension @n) faceIndex)
          )
          (Map.toAscList (hornFaces hornValue))
      frameValue <- first (const (HornIndexFaceOutOfBounds (hornMissingFace hornValue))) (mkIndexedHornFrame missingFace indexedFaces)
      Right (IndexedHorn frameValue)
    else
      Left (HornIndexDimensionMismatch (natVal (Proxy @n) + 1) (hornDimension hornValue))

isInnerHorn :: Horn simplex -> Bool
isInnerHorn hornValue =
  hornDimension hornValue > 1
    && hornMissingFace hornValue > 0
    && hornMissingFace hornValue < hornDimension hornValue

mkInnerHorn :: Horn simplex -> Either InnerHornError (InnerHorn simplex)
mkInnerHorn hornValue =
  if isInnerHorn hornValue
    then Right (InnerHorn hornValue)
    else
      Left (HornNotInner (hornDimension hornValue) (hornMissingFace hornValue))

type InnerKan :: Type -> Constraint
class InnerKan k where
  type InnerSimplex k
  fillInnerHorn :: k -> InnerHorn (InnerSimplex k) -> Maybe (InnerSimplex k)

type KanComplex :: Type -> Constraint
class InnerKan k => KanComplex k where
  fillHorn :: k -> Horn (InnerSimplex k) -> Maybe (InnerSimplex k)
