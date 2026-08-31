module Main
  ( main,
  )
where

import Expr.RenderRoundTripSpec qualified as RenderRoundTripSpec
import Expr.SourceCoordinatesSpec qualified as SourceCoordinatesSpec
import Hie.OracleSpec qualified as OracleSpec
import ModuleSurfaceSpec qualified as ModuleSurfaceSpec
import Hie.TypeWordsSpec qualified as TypeWordsSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
      defaultMain
    ( testGroup
        "pale-ghc-surface"
        [ OracleSpec.tests,
          TypeWordsSpec.tests,
          ModuleSurfaceSpec.tests,
          RenderRoundTripSpec.tests,
          SourceCoordinatesSpec.tests
        ]
    )
