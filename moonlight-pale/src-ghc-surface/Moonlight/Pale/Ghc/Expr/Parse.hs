{-| Parsing Haskell source into the scoped expression algebra. -}
module Moonlight.Pale.Ghc.Expr.Parse
  ( parseHsExprSource,
    convertHaskellExprSource,
  )
where

import GHC.Hs (GhcPs, HsExpr, LHsExpr)
import GHC.Parser (parseExpression)
import GHC.Parser.Lexer (P)
import GHC.Parser.PostProcess (PV, runPV, unECP)
import GHC.Types.SrcLoc (unLoc)
import Moonlight.Core (Pattern)
import Moonlight.Pale.Ghc.Expr.Convert.Coalgebra (convertHsExpr)
import Moonlight.Pale.Ghc.Expr.Convert.Obstruction (ConvertObstruction (..))
import Moonlight.Pale.Ghc.Expr.Syntax (HsExprF)
import Moonlight.Pale.Ghc.ModuleSurface (GhcParseFailure, parseWithGhcParser)

parseHsExprSource :: String -> Either GhcParseFailure (HsExpr GhcPs)
parseHsExprSource sourceText =
  unLoc <$> parseWithGhcParser "<haskell-expression>" sourceText parseLocatedHsExpr

convertHaskellExprSource :: String -> Either ConvertObstruction (Pattern HsExprF)
convertHaskellExprSource sourceText =
  either (Left . ConvertParseFailure) convertHsExpr (parseHsExprSource sourceText)

parseLocatedHsExpr :: P (LHsExpr GhcPs)
parseLocatedHsExpr = do
  exprValue <- parseExpression
  runPV (unECP exprValue :: PV (LHsExpr GhcPs))
