module Main
  ( main,
  )
where

import CompileDiagnosticsSpec qualified as CompileDiagnosticsSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain (testGroup "pale-diagnostic-ghc" [CompileDiagnosticsSpec.tests])
