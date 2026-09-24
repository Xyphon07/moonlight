module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Moonlight.Planar.LiftedSignCases

main :: IO ()
main = do
  cases <- either (fail . show) (evaluate . force) liftedSignCases
  let failures =
        [ (family, index, candidateSign input, magnitudeSign input)
        | (family, inputs) <- cases
        , (index, input) <- zip ([0 ..] :: [Int]) (concatMap permutedInputs inputs)
        , candidateSign input /= magnitudeSign input
        ]
      conventionFailures =
        [ (expected, candidateSign input)
        | (expected, input) <- conventionInputs
        , candidateSign input /= expected || magnitudeSign input /= expected
        ]
      coplanarFailures =
        [ index
        | (ExactlyCoplanar, inputs) <- cases
        , (index, input) <- zip ([0 ..] :: [Int]) inputs
        , candidateSign input /= EQ
        ]
      nearCoplanarFailures =
        [ index
        | (NearlyCoplanar, inputs) <- cases
        , (index, input) <- zip ([1 ..] :: [Int]) inputs
        , candidateSign input /= if even index then GT else LT
        ]
  unless (null failures) (fail ("lifted-sign differential disagreement: " <> show (take 8 failures)))
  unless (null conventionFailures) (fail ("upper-affine sign convention disagreement: " <> show conventionFailures))
  unless (null coplanarFailures) (fail ("coplanar determinant is nonzero: " <> show coplanarFailures))
  unless (null nearCoplanarFailures) (fail ("near-coplanar perturbation sign disagreement: " <> show nearCoplanarFailures))
  putStrLn
    ( "lifted-sign: "
        <> show (sum (map (length . concatMap permutedInputs . snd) cases))
        <> " exact differential permutations; convention, coplanarity and perturbation laws passed"
    )
