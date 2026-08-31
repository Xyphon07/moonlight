{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Category.Test.PolynomialFixture
  ( BranchPosition,
    DemoParameterizedPolynomial,
    DemoPolynomial,
    FullSliceBranchPosition,
    FullSliceRootPosition,
    ParameterizedPosition
      ( FullSliceBranchWitness,
        FullSliceRootWitness,
        TrimmedSliceRootWitness
      ),
    Position (BranchWitness, RootWitness),
    RootPosition,
    TrimmedSliceRootPosition,
  )
where

import Data.Kind (Type)
import Moonlight.Category.Pure.CoveringFamily (Exists (..))
import Moonlight.Category.Pure.PolynomialFunctor
  ( ParameterizedPolynomialFunctor (..),
    PolynomialFunctor (..),
  )

type DemoPolynomial :: Type
data DemoPolynomial

type RootPosition :: Type
data RootPosition

type BranchPosition :: Type
data BranchPosition

type DemoParameterizedPolynomial :: Type
data DemoParameterizedPolynomial

type FullSliceRootPosition :: Type
data FullSliceRootPosition

type FullSliceBranchPosition :: Type
data FullSliceBranchPosition

type TrimmedSliceRootPosition :: Type
data TrimmedSliceRootPosition

instance PolynomialFunctor DemoPolynomial where
  data Position DemoPolynomial position where
    RootWitness :: Position DemoPolynomial RootPosition
    BranchWitness :: Position DemoPolynomial BranchPosition
  type Direction DemoPolynomial RootPosition = Bool
  type Direction DemoPolynomial BranchPosition = Maybe Bool
  allPositions = [Exists RootWitness, Exists BranchWitness]

instance ParameterizedPolynomialFunctor DemoParameterizedPolynomial where
  type PolynomialParameter DemoParameterizedPolynomial = Bool

  data ParameterizedPosition DemoParameterizedPolynomial position where
    FullSliceRootWitness :: ParameterizedPosition DemoParameterizedPolynomial FullSliceRootPosition
    FullSliceBranchWitness :: ParameterizedPosition DemoParameterizedPolynomial FullSliceBranchPosition
    TrimmedSliceRootWitness :: ParameterizedPosition DemoParameterizedPolynomial TrimmedSliceRootPosition

  type ParameterizedDirection DemoParameterizedPolynomial FullSliceRootPosition = Bool
  type ParameterizedDirection DemoParameterizedPolynomial FullSliceBranchPosition = Maybe Bool
  type ParameterizedDirection DemoParameterizedPolynomial TrimmedSliceRootPosition = ()

  positionsAt includeBranch =
    if includeBranch
      then [Exists FullSliceRootWitness, Exists FullSliceBranchWitness]
      else [Exists TrimmedSliceRootWitness]
