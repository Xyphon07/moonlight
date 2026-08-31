{-# LANGUAGE TypeFamilies #-}

-- | Decorated presentation trees — leaves glued along boundaries — and their
-- fold and compilation to a single decorated composition result.
module Moonlight.Category.Pure.DecoratedPresentation
  ( DecoratedPresentation (..),
    presentationLeaf,
    presentationGlue,
    foldDecoratedPresentation,
    compileDecoratedPresentation,
    compileDecoratedPresentationStructured,
  )
where

import Data.Kind (Type)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Moonlight.Category.Pure.DecoratedComposition
  ( CompositionResult (..),
    DecoratedCompositionError,
    StructuredCompositionAlgebra,
    composeDecorated,
    composeDecoratedStructured,
  )
import Moonlight.Category.Pure.Category (Ob)
import Moonlight.Category.Pure.Limits (HasPushouts)

type DecoratedPresentation :: Type -> Type -> Type -> Type
-- | A free binary gluing tree over decorated leaves.
data DecoratedPresentation boundary ir decoration
  = PresentationLeaf ir decoration
  | PresentationGlue boundary (DecoratedPresentation boundary ir decoration) (DecoratedPresentation boundary ir decoration)
  deriving stock (Eq, Show)

data CompiledPresentation ir obligation decoration = CompiledPresentation
  { compiledPresentationIR :: ir,
    compiledPresentationObligations :: Seq obligation,
    compiledPresentationDecoration :: decoration
  }

-- | Inject one decorated leaf.
presentationLeaf :: ir -> decoration -> DecoratedPresentation boundary ir decoration
presentationLeaf =
  PresentationLeaf

-- | Join two presentations at a declared boundary.
presentationGlue ::
  boundary ->
  DecoratedPresentation boundary ir decoration ->
  DecoratedPresentation boundary ir decoration ->
  DecoratedPresentation boundary ir decoration
presentationGlue =
  PresentationGlue

-- | Eliminate a presentation with leaf and gluing algebras.
foldDecoratedPresentation ::
  (ir -> decoration -> result) ->
  (boundary -> result -> result -> result) ->
  DecoratedPresentation boundary ir decoration ->
  result
foldDecoratedPresentation leafAlgebra glueAlgebra =
  foldPresentation
  where
    foldPresentation presentationValue =
      case presentationValue of
        PresentationLeaf ir decoration ->
          leafAlgebra ir decoration
        PresentationGlue boundary leftPresentation rightPresentation ->
          glueAlgebra boundary (foldPresentation leftPresentation) (foldPresentation rightPresentation)

-- | Compile every local gluing into one result while accumulating obligations.
compileDecoratedPresentation ::
  (decoration -> decoration -> decoration) ->
  (boundary -> (ir, decoration) -> (ir, decoration) -> (ir, [obligation])) ->
  DecoratedPresentation boundary ir decoration ->
  CompositionResult ir obligation decoration
compileDecoratedPresentation combineDecorations glue =
  compositionResultFromCompiled
    . foldDecoratedPresentation
      (\ir decoration -> CompiledPresentation ir Seq.empty decoration)
      (mergeComposedPresentations combineDecorations glue)

-- | Compile by descending through structured-cospan pushouts.
compileDecoratedPresentationStructured ::
  (HasPushouts category, Eq (Ob category)) =>
  category ->
  StructuredCompositionAlgebra boundary category ir decoration obligation ->
  (decoration -> decoration -> decoration) ->
  DecoratedPresentation boundary ir decoration ->
  Either (DecoratedCompositionError category) (CompositionResult ir obligation decoration)
compileDecoratedPresentationStructured categoryValue compositionAlgebra combineDecorations =
  fmap compositionResultFromCompiled
    . foldDecoratedPresentation
      (\ir decoration -> Right (CompiledPresentation ir Seq.empty decoration))
      (mergeStructuredPresentations categoryValue compositionAlgebra combineDecorations)

compositionResultFromCompiled :: CompiledPresentation ir obligation decoration -> CompositionResult ir obligation decoration
compositionResultFromCompiled compiled =
  CompositionResult
    { composedIR = compiledPresentationIR compiled,
      composedObligations = toList (compiledPresentationObligations compiled),
      composedDecoration = compiledPresentationDecoration compiled
    }

mergeComposedPresentations ::
  (decoration -> decoration -> decoration) ->
  (boundary -> (ir, decoration) -> (ir, decoration) -> (ir, [obligation])) ->
  boundary ->
  CompiledPresentation ir obligation decoration ->
  CompiledPresentation ir obligation decoration ->
  CompiledPresentation ir obligation decoration
mergeComposedPresentations combineDecorations glue boundary leftResult rightResult =
  let localResult =
        composeDecorated
          combineDecorations
          glue
          boundary
          (compiledPresentationIR leftResult, compiledPresentationDecoration leftResult)
          (compiledPresentationIR rightResult, compiledPresentationDecoration rightResult)
   in CompiledPresentation
        { compiledPresentationIR = composedIR localResult,
          compiledPresentationObligations =
            compiledPresentationObligations leftResult
              <> compiledPresentationObligations rightResult
              <> Seq.fromList (composedObligations localResult),
          compiledPresentationDecoration = composedDecoration localResult
        }

mergeStructuredPresentations ::
  (HasPushouts category, Eq (Ob category)) =>
  category ->
  StructuredCompositionAlgebra boundary category ir decoration obligation ->
  (decoration -> decoration -> decoration) ->
  boundary ->
  Either (DecoratedCompositionError category) (CompiledPresentation ir obligation decoration) ->
  Either (DecoratedCompositionError category) (CompiledPresentation ir obligation decoration) ->
  Either (DecoratedCompositionError category) (CompiledPresentation ir obligation decoration)
mergeStructuredPresentations categoryValue compositionAlgebra combineDecorations boundary maybeLeft maybeRight = do
  leftResult <- maybeLeft
  rightResult <- maybeRight
  localResult <-
    composeDecoratedStructured
      categoryValue
      compositionAlgebra
      combineDecorations
      boundary
      (compiledPresentationIR leftResult, compiledPresentationDecoration leftResult)
      (compiledPresentationIR rightResult, compiledPresentationDecoration rightResult)
  pure
    CompiledPresentation
      { compiledPresentationIR = composedIR localResult,
        compiledPresentationObligations =
          compiledPresentationObligations leftResult
            <> compiledPresentationObligations rightResult
            <> Seq.fromList (composedObligations localResult),
        compiledPresentationDecoration = composedDecoration localResult
      }
