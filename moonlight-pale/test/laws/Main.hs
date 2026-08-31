{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wmissing-local-signatures #-}

module Main
  ( main,
  )
where

import AlgebraicSpec qualified as AlgebraicSpec
import LatticeSpec qualified as LatticeSpec
import RestrictionSpec qualified as RestrictionSpec
import SuiteSpec qualified as SuiteSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "pale-test-laws"
        [ AlgebraicSpec.tests,
          LatticeSpec.tests,
          RestrictionSpec.tests,
          SuiteSpec.tests
        ]
    )
