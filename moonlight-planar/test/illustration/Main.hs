module Main (main) where

import qualified Moonlight.Planar.IllustrationSpec as Illustration
import qualified Moonlight.Planar.IllustrationStudySpec as Study
import qualified Moonlight.Planar.EquationalGardenSpec as Garden
import qualified Moonlight.Planar.MotifSpec as Motif
import qualified Moonlight.Planar.LayoutSpec as Layout
import qualified Moonlight.Planar.SpaceSwordSpec as SpaceSword

main :: IO ()
main = () <$ sequenceA
  [Illustration.tests, Motif.tests, Layout.tests, Study.tests, Garden.tests, SpaceSword.tests]
