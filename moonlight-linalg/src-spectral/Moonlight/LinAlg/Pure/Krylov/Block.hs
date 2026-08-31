{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Krylov.Block
  ( blockLanczosSymmetric
  )
where

import Data.Kind (Type)
import qualified Data.Vector as Box
import qualified Data.Vector.Unboxed as U
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Pure.Krylov.Config
import Moonlight.LinAlg.Pure.Krylov.Decomposition
import Moonlight.LinAlg.Pure.Operator
import Moonlight.LinAlg.Pure.Krylov.Internal
  ( blockInnerBlock,
    multiplyBasisByBlock,
    normalizeSeedBlock,
    orthonormalizeBlock,
    selfAdjointBlockInnerBlock,
    subtractBlocks,
    validateIterationCount,
    validateSquareOperator,
  )
import Moonlight.LinAlg.Pure.Structured.BlockTridiagonal
  ( RowMajorBlock,
    mkSymmetricBlockTridiagonal,
    transposeRowMajorBlock,
  )
import Prelude

type PreviousBlockContext :: Type
data PreviousBlockContext
  = InitialBlockContext
  | PreviousBlockContext !(Box.Vector (U.Vector Double)) !RowMajorBlock

type BlockStepResult :: Type
data BlockStepResult = BlockStepResult
  { stepDiagonalBlock :: !RowMajorBlock,
    stepNextBlock :: !(Box.Vector (U.Vector Double)),
    stepNextCouplingBlock :: !(Maybe RowMajorBlock)
  }

blockLanczosSymmetric ::
  BlockLanczosConfig ->
  LinearOperator 'SelfAdjointOperator ->
  Box.Vector (U.Vector Double) ->
  Either MoonlightError BlockLanczosDecomposition
blockLanczosSymmetric config op seedBlock = do
  validateSquareOperator "Block Lanczos" op
  let (_, dimension) = operatorShape op
      boundedBlockSize = min dimension (blockLanczosBlockSize config)
  boundedIterations <- validateIterationCount "Block Lanczos" (blockLanczosIterations config)
  initialBlock <-
    normalizeSeedBlock
      "Block Lanczos"
      dimension
      (blockLanczosTolerance config)
      boundedBlockSize
      seedBlock
  iterateBlocks boundedIterations initialBlock initialBlock InitialBlockContext [] [] 1
  where
    iterateBlocks boundedIterations accumulatedBasis currentBlock previousContext diagonalBlocksRev couplingBlocksRev blockStepCount = do
      imageBlock <- Box.fromList <$> traverse (runOperatorU op) (Box.toList currentBlock)
      stepResult <-
        blockLanczosStep
          config
          accumulatedBasis
          currentBlock
          previousContext
          imageBlock
      let nextDiagonalBlocksRev = stepDiagonalBlock stepResult : diagonalBlocksRev
          nextCouplingBlocksRev =
            maybe couplingBlocksRev (: couplingBlocksRev) (stepNextCouplingBlock stepResult)
          nextAccumulatedBasis = accumulatedBasis <> stepNextBlock stepResult
       in if blockStepCount >= boundedIterations || Box.length accumulatedBasis >= snd (operatorShape op) || Box.null (stepNextBlock stepResult)
            then
              finalize
                accumulatedBasis
                nextDiagonalBlocksRev
                couplingBlocksRev
                blockStepCount
            else
              iterateBlocks
                boundedIterations
                nextAccumulatedBasis
                (stepNextBlock stepResult)
                (case stepNextCouplingBlock stepResult of
                   Nothing -> InitialBlockContext
                   Just couplingBlock -> PreviousBlockContext currentBlock couplingBlock)
                nextDiagonalBlocksRev
                nextCouplingBlocksRev
                (blockStepCount + 1)

    finalize basisVectors diagonalBlocksRev couplingBlocksRev blockStepCount =
      let diagonalBlocks = Box.fromList (reverse diagonalBlocksRev)
          couplingBlocks = Box.fromList (reverse couplingBlocksRev)
       in do
            projectedBlockTridiagonal <- mkSymmetricBlockTridiagonal diagonalBlocks couplingBlocks
            mkBlockLanczosDecomposition basisVectors projectedBlockTridiagonal blockStepCount

blockLanczosStep ::
  BlockLanczosConfig ->
  Box.Vector (U.Vector Double) ->
  Box.Vector (U.Vector Double) ->
  PreviousBlockContext ->
  Box.Vector (U.Vector Double) ->
  Either MoonlightError BlockStepResult
blockLanczosStep config accumulatedBasis currentBlock previousContext imageBlock = do
  (alphaRows, recurrenceResidual) <- threeTermResidual currentBlock previousContext imageBlock
  let stabilizationBasis =
        if blockLanczosReorthogonalize config
          then accumulatedBasis
          else Box.empty
  nextBlock <-
    orthonormalizeBlock
      (blockLanczosReorthogonalize config)
      (blockLanczosTolerance config)
      stabilizationBasis
      recurrenceResidual
  nextCouplingBlock <-
    if Box.null nextBlock
      then Right Nothing
      else Just <$> blockInnerBlock nextBlock recurrenceResidual
  Right
    BlockStepResult
      { stepDiagonalBlock = alphaRows,
        stepNextBlock = nextBlock,
        stepNextCouplingBlock = nextCouplingBlock
      }

threeTermResidual ::
  Box.Vector (U.Vector Double) ->
  PreviousBlockContext ->
  Box.Vector (U.Vector Double) ->
  Either MoonlightError (RowMajorBlock, Box.Vector (U.Vector Double))
threeTermResidual currentBlock previousContext imageBlock = do
  alphaRows <- selfAdjointBlockInnerBlock currentBlock imageBlock
  currentContribution <- multiplyBasisByBlock currentBlock alphaRows
  residualAfterCurrent <- subtractBlocks imageBlock currentContribution
  recurrenceResidual <-
    case previousContext of
      InitialBlockContext -> Right residualAfterCurrent
      PreviousBlockContext previousBlock couplingRows -> do
        previousContribution <- multiplyBasisByBlock previousBlock (transposeRowMajorBlock couplingRows)
        subtractBlocks residualAfterCurrent previousContribution
  Right (alphaRows, recurrenceResidual)
