{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.NameRender
  ( renderRdrName,
    varRefRdrName,
  )
where

import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, isQual_maybe, rdrNameOcc)
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import Moonlight.Pale.Ghc.Expr.Syntax

varRefRdrName :: HsVarRef -> RdrName
varRefRdrName = \case
  GlobalName rdrName -> rdrName
  LocalName binderAnn -> baName binderAnn

renderRdrName :: RdrName -> String
renderRdrName nameValue =
  case isQual_maybe nameValue of
    Just (moduleName, occName) ->
      moduleNameString moduleName <> "." <> occNameString occName
    Nothing ->
      occNameString (rdrNameOcc nameValue)
