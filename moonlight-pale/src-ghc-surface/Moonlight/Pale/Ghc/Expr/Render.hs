{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render
  ( LayoutPolicy (..),
    PageWidth,
    defaultPageWidth,
    mkPageWidth,
    shrinkPageWidth,
    ModuleRenderContext (..),
    RenderTarget (..),
    RenderRefusal (..),
    renderSource,
    renderRoundTripEquivalent,
  )
where

import Data.Text qualified as Text
import Moonlight.Pale.Ghc.Expr.Equivalence (renderRoundTripEquivalent)
import Moonlight.Pale.Ghc.Expr.Render.Document
import Moonlight.Pale.Ghc.Expr.Render.Module
import Moonlight.Pale.Ghc.Expr.Render.Refusal

renderSource :: LayoutPolicy -> RenderTarget -> Either RenderRefusal Text.Text
renderSource = \case
  CompactLayout ->
    fmap renderCompactDocument
      . renderSourceWith CompactRender
  PrettyLayout pageWidth ->
    fmap (renderPrettyDocument pageWidth)
      . renderSourceWith GeneratedRender
