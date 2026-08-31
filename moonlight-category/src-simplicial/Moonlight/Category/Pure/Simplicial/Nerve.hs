{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The nerve of a finite composable category: simplices are composable chains,
-- with face and degeneracy structure.  'normalizedNerve' is the ordinary
-- nonidentity-chain construction.  'unnormalizedNerve' materializes every
-- composable chain, including identity insertions, and therefore has a
-- substantially larger cost surface.
module Moonlight.Category.Pure.Simplicial.Nerve
  ( NerveSimplex,
    nerveSimplexDimension,
    nerveSimplexChain,
    nerveSimplexFromChain,
    mkNerveSimplex,
    Nerve,
    nerveCategory,
    unNerve,
    unnormalizedNerve,
    isNerveSimplexDegenerate,
    nerveSimplexFace,
    nerveSimplexDegeneracy,
    normalizedNerve,
    nerveInnerKan,
    fillNerveInnerHorn,
    fillNerveInnerHornIndexed,
  )
where

import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Function ((&))
import Data.Kind (Type)
import Data.List (genericLength, genericSplitAt, unsnoc)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Map.Strict qualified as Map
import GHC.TypeNats (KnownNat, type (+))
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.FiniteComposable
  ( ComposableChain,
    FiniteComposableCategory (..),
    SizedComposableChain,
    chainDimension,
    chainMorphisms,
    chainStartObject,
    chainVertices,
    mkComposableChain,
    sizedChainDimension,
    sizedChainValue,
  )
import Moonlight.Core (safeIndexNatural)
import Moonlight.Category.Pure.Simplicial.Kan
  ( IndexedHorn,
    InnerHorn,
    InnerHornError,
    InnerKan (..),
    hornDimension,
    hornFaces,
    indexedHornToHorn,
    innerHorn,
    mkInnerHorn,
  )
import Moonlight.Category.Pure.Simplicial.Set
  ( GeneratedSSet,
    TruncatedNormalizedSSet,
    normalizeGeneratedSSet,
  )
import Moonlight.Category.Pure.Simplicial.Set.Internal (trustedGeneratedSSetWithWitness)
import Moonlight.Category.Pure.Simplicial.TypeLevel (Dimension (..), Fin, finValue)
import Numeric.Natural (Natural)

type NerveSimplex :: Type -> Type
data NerveSimplex c = NerveSimplex
  { nerveSimplexDimension :: Natural,
    nerveSimplexChain :: ComposableChain c
  }

instance (Eq (Ob c), Eq (Mor c)) => Eq (NerveSimplex c) where
  (==) = sameSimplex

nerveSimplexFromChain :: ComposableChain c -> NerveSimplex c
nerveSimplexFromChain chainValue =
  NerveSimplex (chainDimension chainValue) chainValue

mkNerveSimplex :: Natural -> ComposableChain c -> Maybe (NerveSimplex c)
mkNerveSimplex dimensionValue chainValue =
  if dimensionValue == chainDimension chainValue
    then Just (NerveSimplex dimensionValue chainValue)
    else Nothing

type Nerve :: Type -> Type
data Nerve c = Nerve
  { nerveCategory :: c,
    unNerve :: TruncatedNormalizedSSet (NerveSimplex c)
  }

morphismIsIdentity :: (Category c, Eq (Mor c)) => c -> Mor c -> Bool
morphismIsIdentity categoryValue morphism =
  case source categoryValue morphism >>= identity categoryValue of
    Right identityMorphism -> morphism == identityMorphism
    Left _ -> False

isNerveSimplexDegenerate :: (Category c, Eq (Mor c)) => c -> Natural -> NerveSimplex c -> Bool
isNerveSimplexDegenerate categoryValue _ simplexValue =
  chainMorphisms (nerveSimplexChain simplexValue)
    & any (morphismIsIdentity categoryValue)

-- | Materialize the unnormalized nerve through the supplied dimension.  This
-- includes every identity insertion, so it is intended only for small
-- diagnostic or law workloads.  The dimension bound is /not/ a resource bound:
-- the number of composable chains can grow combinatorially before that bound.
unnormalizedNerve ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  Natural ->
  GeneratedSSet (NerveSimplex c)
unnormalizedNerve categoryValue upperBound =
  let levelMap = Map.fromAscListWith (<>) (nerveLevels categoryValue upperBound)
   in trustedGeneratedSSetWithWitness
        upperBound
        (\dimensionValue' -> Map.findWithDefault [] dimensionValue' levelMap)
        (nerveFace categoryValue)
        (nerveDegeneracy categoryValue)
        (isNerveSimplexDegenerate categoryValue)

-- | Construct the normalized nerve through the supplied dimension by
-- enumerating nonidentity chains and closing them under faces.  This is the
-- default construction for topological and combinatorial queries.  Its
-- dimension bound selects simplices; it does not promise a fixed time or memory
-- budget for an arbitrary finite category.
normalizedNerve ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  Natural ->
  TruncatedNormalizedSSet (NerveSimplex c)
normalizedNerve categoryValue upperBound =
  normalizeGeneratedSSet
    ( trustedGeneratedSSetWithWitness
        upperBound
        (\dimensionValue' -> Map.findWithDefault [] dimensionValue' closedLevelMap)
        (nerveFace categoryValue)
        (nerveDegeneracy categoryValue)
        (isNerveSimplexDegenerate categoryValue)
    )
  where
    closedLevelMap =
      closeUnderFaces categoryValue upperBound nonDegenerateLevelMap

    nonDegenerateLevelMap =
      Map.fromDistinctAscList
        ( zipWith
            (\dimensionValue' chains -> (dimensionValue', fmap (NerveSimplex dimensionValue') chains))
            [0 ..]
            (enumerateNonDegenerateChainsByDimension categoryValue upperBound)
        )

nerveInnerKan ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  Natural ->
  Nerve c
nerveInnerKan categoryValue upperBound = Nerve categoryValue (normalizedNerve categoryValue upperBound)

nerveLevels :: (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) => c -> Natural -> [(Natural, [NerveSimplex c])]
nerveLevels categoryValue upperBound =
  Map.toAscList
    ( closeUnderFaces
        categoryValue
        upperBound
        (seedNerveLevelMap categoryValue upperBound (\_ _ -> True))
    )

seedNerveLevelMap ::
  forall c.
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  Natural ->
  (Natural -> NerveSimplex c -> Bool) ->
  Map.Map Natural [NerveSimplex c]
seedNerveLevelMap categoryValue upperBound keepSimplex =
  enumerateComposableChains categoryValue upperBound
    & mapMaybe sizedChainEntry
    & foldl' insertChainEntry Map.empty
    & Map.map (dedupeSimplices . reverse)
  where
    insertChainEntry ::
      Map.Map Natural [NerveSimplex c] ->
      (Natural, NerveSimplex c) ->
      Map.Map Natural [NerveSimplex c]
    insertChainEntry levelMap (dimensionValue, simplexValue) =
      Map.insertWith (<>) dimensionValue [simplexValue] levelMap

    sizedChainEntry ::
      SizedComposableChain c ->
      Maybe (Natural, NerveSimplex c)
    sizedChainEntry sizedChain =
      let dimensionValue = sizedChainDimension sizedChain
          simplexValue = NerveSimplex dimensionValue (sizedChainValue sizedChain)
       in if keepSimplex dimensionValue simplexValue
            then Just (dimensionValue, simplexValue)
            else Nothing

closeUnderFaces ::
  forall c.
  (Category c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  Natural ->
  Map.Map Natural [NerveSimplex c] ->
  Map.Map Natural [NerveSimplex c]
closeUnderFaces categoryValue upperBound simplicesByDimension =
  foldr closeDimension
    (Map.map dedupeSimplices simplicesByDimension)
    [1 .. upperBound]
  where
    closeDimension ::
      Natural ->
      Map.Map Natural [NerveSimplex c] ->
      Map.Map Natural [NerveSimplex c]
    closeDimension dimensionValue simplicesAtDimension =
      let simplexFaceRows =
            Map.findWithDefault [] dimensionValue simplicesAtDimension
              & mapMaybe simplexFacesIfClosed
          validSimplices = fmap fst simplexFaceRows
          faceSimplices = foldMap snd simplexFaceRows
          withValidDimension = Map.insert dimensionValue validSimplices simplicesAtDimension
       in Map.insertWith mergeSimplices (dimensionValue - 1) faceSimplices withValidDimension

    simplexFacesIfClosed :: NerveSimplex c -> Maybe (NerveSimplex c, [NerveSimplex c])
    simplexFacesIfClosed simplexValue =
      let dimensionValue = nerveSimplexDimension simplexValue
       in fmap
            (\faceSimplices -> (simplexValue, faceSimplices))
            (traverse (\faceIndex -> faceSimplex dimensionValue faceIndex simplexValue) [0 .. dimensionValue])

    faceSimplex ::
      Natural ->
      Natural ->
      NerveSimplex c ->
      Maybe (NerveSimplex c)
    faceSimplex dimensionValue faceIndex simplexValue =
      NerveSimplex (dimensionValue - 1)
        <$> faceChain categoryValue faceIndex (nerveSimplexChain simplexValue)

mergeSimplices ::
  (Ord (Ob c), Ord (Mor c)) =>
  [NerveSimplex c] ->
  [NerveSimplex c] ->
  [NerveSimplex c]
mergeSimplices newSimplices existingSimplices =
  dedupeSimplices (existingSimplices <> newSimplices)

type NerveSimplexKey :: Type -> Type
type NerveSimplexKey c = (Natural, Ob c, [Mor c])

nerveSimplexKey :: NerveSimplex c -> NerveSimplexKey c
nerveSimplexKey simplexValue =
  ( nerveSimplexDimension simplexValue,
    chainStartObject (nerveSimplexChain simplexValue),
    chainMorphisms (nerveSimplexChain simplexValue)
  )

dedupeSimplices ::
  forall c.
  (Ord (Ob c), Ord (Mor c)) =>
  [NerveSimplex c] ->
  [NerveSimplex c]
dedupeSimplices =
  nubOrdOn nerveSimplexKey

nerveFace ::
  forall c n.
  (Category c, Eq (Ob c)) =>
  c ->
  Dimension (n + 1) ->
  Fin (n + 2) ->
  NerveSimplex c ->
  Maybe (NerveSimplex c)
nerveFace categoryValue _ faceIndex =
  nerveSimplexFace categoryValue (finValue faceIndex)

nerveDegeneracy ::
  forall c n.
  (Category c, Eq (Ob c)) =>
  c ->
  Dimension n ->
  Fin (n + 1) ->
  NerveSimplex c ->
  Maybe (NerveSimplex c)
nerveDegeneracy categoryValue _ degeneracyIndex =
  nerveSimplexDegeneracy categoryValue (finValue degeneracyIndex)

nerveSimplexFace ::
  (Category c, Eq (Ob c)) =>
  c ->
  Natural ->
  NerveSimplex c ->
  Maybe (NerveSimplex c)
nerveSimplexFace categoryValue faceIndex simplexValue = do
  let currentDimension = nerveSimplexDimension simplexValue
  guard (currentDimension > 0)
  faceChainValue <- faceChain categoryValue faceIndex (nerveSimplexChain simplexValue)
  pure (NerveSimplex (currentDimension - 1) faceChainValue)

nerveSimplexDegeneracy ::
  (Category c, Eq (Ob c)) =>
  c ->
  Natural ->
  NerveSimplex c ->
  Maybe (NerveSimplex c)
nerveSimplexDegeneracy categoryValue degeneracyIndex simplexValue = do
  let currentDimension = nerveSimplexDimension simplexValue
  degeneracyChainValue <- degeneracyChain categoryValue degeneracyIndex (nerveSimplexChain simplexValue)
  pure (NerveSimplex (currentDimension + 1) degeneracyChainValue)

splitAtNatural :: Natural -> [a] -> Maybe ([a], [a])
splitAtNatural splitIndex values =
  let (prefix, suffix) = genericSplitAt splitIndex values
   in if genericLength prefix == splitIndex
        then Just (prefix, suffix)
        else Nothing

insertAt :: Natural -> a -> [a] -> Maybe [a]
insertAt indexValue inserted values = do
  (prefix, suffix) <- splitAtNatural indexValue values
  pure (prefix <> (inserted : suffix))

faceChain :: (Category c, Eq (Ob c)) => c -> Natural -> ComposableChain c -> Maybe (ComposableChain c)
faceChain categoryValue faceIndex chainValue
  | faceIndex > dimensionValue' = Nothing
  | otherwise = case morphisms of
      [] -> Nothing
      _ : restMorphisms
        | faceIndex == 0 -> do
            startObject <- safeIndexNatural 1 (NonEmpty.toList (chainVertices chainValue))
            either (const Nothing) Just (mkComposableChain categoryValue startObject restMorphisms)
        | faceIndex == dimensionValue' -> do
            (prefixMorphisms, _) <- unsnoc morphisms
            either (const Nothing) Just (mkComposableChain categoryValue (chainStartObject chainValue) prefixMorphisms)
        | otherwise -> do
            (leftMorphisms, rightMorphisms) <- splitAtNatural faceIndex morphisms
            (leftPrefix, leftMorphism) <- unsnoc leftMorphisms
            case rightMorphisms of
              [] -> Nothing
              rightMorphism : rightSuffix -> do
                (composedMorphism, _) <- either (const Nothing) Just (compose categoryValue rightMorphism leftMorphism)
                either
                  (const Nothing)
                  Just
                  ( mkComposableChain
                      categoryValue
                      (chainStartObject chainValue)
                      (leftPrefix <> (composedMorphism : rightSuffix))
                  )
  where
    morphisms = chainMorphisms chainValue
    dimensionValue' = chainDimension chainValue

degeneracyChain :: (Category c, Eq (Ob c)) => c -> Natural -> ComposableChain c -> Maybe (ComposableChain c)
degeneracyChain categoryValue degeneracyIndex chainValue =
  let dimensionValue' = chainDimension chainValue
   in if degeneracyIndex > dimensionValue'
        then Nothing
        else do
          duplicatedObject <- safeIndexNatural degeneracyIndex (NonEmpty.toList (chainVertices chainValue))
          identityMorphism <- either (const Nothing) Just (identity categoryValue duplicatedObject)
          insertedMorphisms <- insertAt degeneracyIndex identityMorphism (chainMorphisms chainValue)
          either (const Nothing) Just (mkComposableChain categoryValue (chainStartObject chainValue) insertedMorphisms)

sameSimplex :: (Eq (Ob c), Eq (Mor c)) => NerveSimplex c -> NerveSimplex c -> Bool
sameSimplex leftSimplex rightSimplex =
  nerveSimplexDimension leftSimplex == nerveSimplexDimension rightSimplex
    && chainStartObject (nerveSimplexChain leftSimplex) == chainStartObject (nerveSimplexChain rightSimplex)
    && chainMorphisms (nerveSimplexChain leftSimplex) == chainMorphisms (nerveSimplexChain rightSimplex)

fillNerveInnerHorn :: (Category c, Eq (Ob c), Eq (Mor c)) => c -> InnerHorn (NerveSimplex c) -> Maybe (NerveSimplex c)
fillNerveInnerHorn categoryValue innerHornValue = do
      let hornValue = innerHorn innerHornValue
          dimensionValue' = hornDimension hornValue
      leftOuterFace <- Map.lookup 0 (hornFaces hornValue)
      rightOuterFace <- Map.lookup dimensionValue' (hornFaces hornValue)
      guard
        ( nerveSimplexDimension leftOuterFace == dimensionValue' - 1
            && nerveSimplexDimension rightOuterFace == dimensionValue' - 1
        )
      let leftMorphisms = chainMorphisms (nerveSimplexChain leftOuterFace)
          rightMorphisms = chainMorphisms (nerveSimplexChain rightOuterFace)
      firstMorphism <- safeIndexNatural 0 rightMorphisms
      filledChain <-
        either
          (const Nothing)
          Just
          (mkComposableChain categoryValue (chainStartObject (nerveSimplexChain rightOuterFace)) (firstMorphism : leftMorphisms))
      let candidateSimplex = NerveSimplex dimensionValue' filledChain
      checks <-
        traverse
          (\(fi, ef) -> sameSimplex ef . NerveSimplex (dimensionValue' - 1) <$> faceChain categoryValue fi filledChain)
          (Map.toAscList (hornFaces hornValue))
      guard (and checks)
      pure candidateSimplex

fillNerveInnerHornIndexed ::
  (Category c, Eq (Ob c), Eq (Mor c), KnownNat n) =>
  c ->
  IndexedHorn n (NerveSimplex c) ->
  Either InnerHornError (Maybe (NerveSimplex c))
fillNerveInnerHornIndexed categoryValue indexedHornValue =
  fillNerveInnerHorn categoryValue <$> mkInnerHorn (indexedHornToHorn indexedHornValue)

instance (Category c, Eq (Ob c), Eq (Mor c)) => InnerKan (Nerve c) where
  type InnerSimplex (Nerve c) = NerveSimplex c
  fillInnerHorn nerveValue = fillNerveInnerHorn (nerveCategory nerveValue)
