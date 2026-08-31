{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Refusal
  ( RenderRefusal (..)
  )
where

import Data.Kind (Type)
type RenderRefusal :: Type
data RenderRefusal
  = RenderGuardedExpression
  | RenderPatternVariable
  | RenderNonVarOperator
  | RenderEmptyBindingName
  | RenderClausesShape
  deriving stock (Eq, Ord, Show)
