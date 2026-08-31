{-| Shared test budgets and validated numeric tolerances. -}
module Moonlight.Pale.Test.Core
  ( TestBudget (..),
    canonicalTestBudget,
    scopedTestBudget,
    stressTestBudget,
    tightNodeBudget,
    tightIterationBudget,
    mediumPressureBudget,
    generousBudget,
    Tolerance,
    ToleranceObstruction (..),
    mkTolerance,
    absoluteTolerance,
    relativeTolerance,
    physicsTolerance,
    exactTolerance,
    solverTolerance,
  )
where

import Data.Kind (Type)

type TestBudget :: Type
data TestBudget = TestBudget
  { testBudgetMaxIterations :: !Int,
    testBudgetMaxNodes :: !Int
  }
  deriving stock (Eq, Show, Read)

canonicalTestBudget :: TestBudget
canonicalTestBudget = TestBudget {testBudgetMaxIterations = 4, testBudgetMaxNodes = 20}

scopedTestBudget :: TestBudget
scopedTestBudget = TestBudget {testBudgetMaxIterations = 3, testBudgetMaxNodes = 20}

stressTestBudget :: TestBudget
stressTestBudget = TestBudget {testBudgetMaxIterations = 30, testBudgetMaxNodes = 1000}

tightNodeBudget :: TestBudget
tightNodeBudget = TestBudget {testBudgetMaxIterations = 30, testBudgetMaxNodes = 15}

tightIterationBudget :: TestBudget
tightIterationBudget = TestBudget {testBudgetMaxIterations = 2, testBudgetMaxNodes = 5000}

mediumPressureBudget :: TestBudget
mediumPressureBudget = TestBudget {testBudgetMaxIterations = 15, testBudgetMaxNodes = 300}

generousBudget :: TestBudget
generousBudget = TestBudget {testBudgetMaxIterations = 30, testBudgetMaxNodes = 1500}

type Tolerance :: Type
data Tolerance = Tolerance
  { absoluteTolerance :: !Double,
    relativeTolerance :: !Double
  }
  deriving stock (Eq, Show)

type ToleranceObstruction :: Type
data ToleranceObstruction
  = ToleranceNotFinite !Double !Double
  | ToleranceNegative !Double !Double
  deriving stock (Eq, Show)

mkTolerance :: Double -> Double -> Either ToleranceObstruction Tolerance
mkTolerance absoluteLimit relativeLimit
  | anyNonFinite =
      Left (ToleranceNotFinite absoluteLimit relativeLimit)
  | absoluteLimit < 0 || relativeLimit < 0 =
      Left (ToleranceNegative absoluteLimit relativeLimit)
  | otherwise =
      Right (Tolerance absoluteLimit relativeLimit)
  where
    anyNonFinite =
      isNaN absoluteLimit
        || isInfinite absoluteLimit
        || isNaN relativeLimit
        || isInfinite relativeLimit

physicsTolerance :: Tolerance
physicsTolerance =
  Tolerance 1.0e-9 1.0e-9

exactTolerance :: Tolerance
exactTolerance =
  Tolerance 1.0e-12 1.0e-12

solverTolerance :: Tolerance
solverTolerance =
  Tolerance 1.0e-5 1.0e-5
