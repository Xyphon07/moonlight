{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Krylov.Config
  ( KrylovConfigError (..),
    krylovConfigErrorMessage,
    PositiveCount,
    mkPositiveCount,
    positiveCountValue,
    NonNegativeConfigTolerance,
    mkNonNegativeConfigTolerance,
    nonNegativeConfigToleranceValue,
    ArnoldiConfig,
    mkArnoldiConfig,
    arnoldiIterations,
    arnoldiTolerance,
    arnoldiReorthogonalize,
    withArnoldiIterations,
    withArnoldiTolerance,
    withArnoldiReorthogonalize,
    defaultArnoldiConfig,
    LanczosConfig,
    mkLanczosConfig,
    lanczosIterations,
    lanczosTolerance,
    withLanczosIterations,
    withLanczosTolerance,
    defaultLanczosConfig,
    BlockLanczosConfig,
    mkBlockLanczosConfig,
    blockLanczosIterations,
    blockLanczosTolerance,
    blockLanczosBlockSize,
    blockLanczosReorthogonalize,
    withBlockLanczosIterations,
    withBlockLanczosTolerance,
    withBlockLanczosBlockSize,
    withBlockLanczosReorthogonalize,
    defaultBlockLanczosConfig,
  )
where

import Data.Kind (Type)
import Moonlight.Core
  ( mkNonNegativeFiniteWith,
    mkPositiveIntWith,
  )
import Prelude

type PositiveCount :: Type
newtype PositiveCount = PositiveCount
  { positiveCountValue :: Int
  }
  deriving stock (Eq, Show)

type KrylovConfigError :: Type
data KrylovConfigError
  = PositiveCountMustBePositive !Int
  | KrylovConfigToleranceMustBeNonNegative !Double
  deriving stock (Eq, Show)

krylovConfigErrorMessage :: KrylovConfigError -> String
krylovConfigErrorMessage configError =
  case configError of
    PositiveCountMustBePositive value ->
      "positive count must be positive, received " <> show value
    KrylovConfigToleranceMustBeNonNegative value ->
      "Krylov config tolerance must be finite and non-negative, received " <> show value

mkPositiveCount :: Int -> Either KrylovConfigError PositiveCount
mkPositiveCount =
  mkPositiveIntWith PositiveCountMustBePositive PositiveCount

type NonNegativeConfigTolerance :: Type
newtype NonNegativeConfigTolerance = NonNegativeConfigTolerance
  { nonNegativeConfigToleranceValue :: Double
  }
  deriving stock (Eq, Show)

mkNonNegativeConfigTolerance :: Double -> Either KrylovConfigError NonNegativeConfigTolerance
mkNonNegativeConfigTolerance =
  mkNonNegativeFiniteWith
    KrylovConfigToleranceMustBeNonNegative
    NonNegativeConfigTolerance

type ArnoldiConfig :: Type
data ArnoldiConfig = ArnoldiConfig
  { arnoldiIterationCount :: !PositiveCount,
    arnoldiToleranceBound :: !NonNegativeConfigTolerance,
    arnoldiReorthogonalize :: !Bool
  }
  deriving stock (Eq)

instance Show ArnoldiConfig where
  showsPrec precedence config =
    showParen (precedence > 10) $
      showString "ArnoldiConfig {arnoldiIterations = "
        . shows (arnoldiIterations config)
        . showString ", arnoldiTolerance = "
        . shows (arnoldiTolerance config)
        . showString ", arnoldiReorthogonalize = "
        . shows (arnoldiReorthogonalize config)
        . showString "}"

arnoldiIterations :: ArnoldiConfig -> Int
arnoldiIterations =
  positiveCountValue . arnoldiIterationCount

arnoldiTolerance :: ArnoldiConfig -> Double
arnoldiTolerance =
  nonNegativeConfigToleranceValue . arnoldiToleranceBound

mkArnoldiConfig :: PositiveCount -> NonNegativeConfigTolerance -> Bool -> ArnoldiConfig
mkArnoldiConfig iterations tolerance reorthogonalize =
  ArnoldiConfig
    { arnoldiIterationCount = iterations,
      arnoldiToleranceBound = tolerance,
      arnoldiReorthogonalize = reorthogonalize
    }

withArnoldiIterations :: PositiveCount -> ArnoldiConfig -> ArnoldiConfig
withArnoldiIterations iterations config =
  mkArnoldiConfig
    iterations
    (arnoldiToleranceBound config)
    (arnoldiReorthogonalize config)

withArnoldiTolerance :: NonNegativeConfigTolerance -> ArnoldiConfig -> ArnoldiConfig
withArnoldiTolerance tolerance config =
  mkArnoldiConfig
    (arnoldiIterationCount config)
    tolerance
    (arnoldiReorthogonalize config)

withArnoldiReorthogonalize :: Bool -> ArnoldiConfig -> ArnoldiConfig
withArnoldiReorthogonalize reorthogonalize config =
  mkArnoldiConfig
    (arnoldiIterationCount config)
    (arnoldiToleranceBound config)
    reorthogonalize

defaultArnoldiConfig :: ArnoldiConfig
defaultArnoldiConfig =
  mkArnoldiConfig (PositiveCount 64) (NonNegativeConfigTolerance 1.0e-10) True

type LanczosConfig :: Type
data LanczosConfig = LanczosConfig
  { lanczosIterationCount :: !PositiveCount,
    lanczosToleranceBound :: !NonNegativeConfigTolerance
  }
  deriving stock (Eq)

instance Show LanczosConfig where
  showsPrec precedence config =
    showParen (precedence > 10) $
      showString "LanczosConfig {lanczosIterations = "
        . shows (lanczosIterations config)
        . showString ", lanczosTolerance = "
        . shows (lanczosTolerance config)
        . showString "}"

lanczosIterations :: LanczosConfig -> Int
lanczosIterations =
  positiveCountValue . lanczosIterationCount

lanczosTolerance :: LanczosConfig -> Double
lanczosTolerance =
  nonNegativeConfigToleranceValue . lanczosToleranceBound

mkLanczosConfig :: PositiveCount -> NonNegativeConfigTolerance -> LanczosConfig
mkLanczosConfig iterations tolerance =
  LanczosConfig
    { lanczosIterationCount = iterations,
      lanczosToleranceBound = tolerance
    }

withLanczosIterations :: PositiveCount -> LanczosConfig -> LanczosConfig
withLanczosIterations iterations config =
  mkLanczosConfig
    iterations
    (lanczosToleranceBound config)

withLanczosTolerance :: NonNegativeConfigTolerance -> LanczosConfig -> LanczosConfig
withLanczosTolerance tolerance config =
  mkLanczosConfig
    (lanczosIterationCount config)
    tolerance

-- | The 24-vector window is the measured minimum-allocation basin for the
-- generic sparse fallback; callers retain an explicit override.
defaultLanczosConfig :: LanczosConfig
defaultLanczosConfig =
  mkLanczosConfig (PositiveCount 24) (NonNegativeConfigTolerance 1.0e-10)

type BlockLanczosConfig :: Type
data BlockLanczosConfig = BlockLanczosConfig
  { blockLanczosIterationCount :: !PositiveCount,
    blockLanczosToleranceBound :: !NonNegativeConfigTolerance,
    blockLanczosConfiguredBlockSize :: !PositiveCount,
    blockLanczosReorthogonalize :: !Bool
  }
  deriving stock (Eq)

instance Show BlockLanczosConfig where
  showsPrec precedence config =
    showParen (precedence > 10) $
      showString "BlockLanczosConfig {blockLanczosIterations = "
        . shows (blockLanczosIterations config)
        . showString ", blockLanczosTolerance = "
        . shows (blockLanczosTolerance config)
        . showString ", blockLanczosBlockSize = "
        . shows (blockLanczosBlockSize config)
        . showString ", blockLanczosReorthogonalize = "
        . shows (blockLanczosReorthogonalize config)
        . showString "}"

blockLanczosIterations :: BlockLanczosConfig -> Int
blockLanczosIterations =
  positiveCountValue . blockLanczosIterationCount

blockLanczosTolerance :: BlockLanczosConfig -> Double
blockLanczosTolerance =
  nonNegativeConfigToleranceValue . blockLanczosToleranceBound

blockLanczosBlockSize :: BlockLanczosConfig -> Int
blockLanczosBlockSize =
  positiveCountValue . blockLanczosConfiguredBlockSize

mkBlockLanczosConfig ::
  PositiveCount ->
  NonNegativeConfigTolerance ->
  PositiveCount ->
  Bool ->
  BlockLanczosConfig
mkBlockLanczosConfig iterations tolerance blockSize reorthogonalize =
  BlockLanczosConfig
    { blockLanczosIterationCount = iterations,
      blockLanczosToleranceBound = tolerance,
      blockLanczosConfiguredBlockSize = blockSize,
      blockLanczosReorthogonalize = reorthogonalize
    }

withBlockLanczosIterations :: PositiveCount -> BlockLanczosConfig -> BlockLanczosConfig
withBlockLanczosIterations iterations config =
  mkBlockLanczosConfig
    iterations
    (blockLanczosToleranceBound config)
    (blockLanczosConfiguredBlockSize config)
    (blockLanczosReorthogonalize config)

withBlockLanczosTolerance :: NonNegativeConfigTolerance -> BlockLanczosConfig -> BlockLanczosConfig
withBlockLanczosTolerance tolerance config =
  mkBlockLanczosConfig
    (blockLanczosIterationCount config)
    tolerance
    (blockLanczosConfiguredBlockSize config)
    (blockLanczosReorthogonalize config)

withBlockLanczosBlockSize :: PositiveCount -> BlockLanczosConfig -> BlockLanczosConfig
withBlockLanczosBlockSize blockSize config =
  mkBlockLanczosConfig
    (blockLanczosIterationCount config)
    (blockLanczosToleranceBound config)
    blockSize
    (blockLanczosReorthogonalize config)

withBlockLanczosReorthogonalize :: Bool -> BlockLanczosConfig -> BlockLanczosConfig
withBlockLanczosReorthogonalize reorthogonalize config =
  mkBlockLanczosConfig
    (blockLanczosIterationCount config)
    (blockLanczosToleranceBound config)
    (blockLanczosConfiguredBlockSize config)
    reorthogonalize

defaultBlockLanczosConfig :: BlockLanczosConfig
defaultBlockLanczosConfig =
  mkBlockLanczosConfig
    (PositiveCount 48)
    (NonNegativeConfigTolerance 1.0e-10)
    (PositiveCount 2)
    True
