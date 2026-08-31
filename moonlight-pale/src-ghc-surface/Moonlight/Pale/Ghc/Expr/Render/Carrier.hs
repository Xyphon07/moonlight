{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Carrier
  ( LocalBindingRows,
    RenderNodeProjection,
    RenderSource (..),
    projectPatternNode
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import GHC.Types.Name.Reader (RdrName)
import Moonlight.Core (Pattern (..))
import Moonlight.Pale.Ghc.Expr.Render.Refusal
import Moonlight.Pale.Ghc.Expr.Syntax

type LocalBindingRows :: Type -> Type
type LocalBindingRows recursive = [(HsPatF, recursive)]

type RenderNodeProjection recursive = recursive -> Either RenderRefusal (HsExprF recursive)

type RenderSource :: Type -> Type
data RenderSource recursive = RenderSource
  { rsProjectNode :: !(RenderNodeProjection recursive),
    rsRenderNames :: !(IntMap RdrName)
  }

projectPatternNode :: RenderNodeProjection (Pattern HsExprF)
projectPatternNode = \case
  PatternVar _ ->
    Left RenderPatternVariable
  PatternNode nodeValue ->
    Right nodeValue
