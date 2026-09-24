{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -O1 #-}

-- | The allocation law for the optimized filtered-predicate artifact. This
-- module remains at O1 when the surrounding behavioral body is compiled at O0;
-- without the simplifier, the loop does not unbox and the witness is vacuous.
module Moonlight.Planar.FilteredPredicateOptimizationSpec
  ( assertFilteredPredicatesSkipExactOracle
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import GHC.Stats (allocated_bytes, getRTSStats, getRTSStatsEnabled)
import Moonlight.Planar.Math
  ( inCircle
  , inCircleDetApprox
  , orient2d
  , orientDetApprox
  )
import Moonlight.Planar.Internal.Point (QueryPoint (..))
import Moonlight.Planar.Point (Point (..))
import System.Mem (performGC)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)

-- | A filtered predicate must not evaluate its exact oracle when the floating
-- approximation already certifies the sign. The exact path allocates
-- 'Integer's once the operands outgrow its fixed-width worker, so
-- equal-shaped certified and wide-exponent degenerate folds expose whether
-- that fallback remains genuinely conditional; a degenerate fold whose
-- operands fit the worker must then allocate no more than a certified one.
assertFilteredPredicatesSkipExactOracle :: IO ()
assertFilteredPredicatesSkipExactOracle = do
  enabled <- getRTSStatsEnabled
  unless enabled $ fail "allocation counters unavailable: the suite must run with -T"
  let count = 20_000
      bigScale = 2 ^^ (200 :: Int) :: Double
      smallScale = 2 ^^ (-200 :: Int) :: Double
      orientCertified =
        V.generate
          count
          (\index -> QueryPoint (Point (fromIntegral index * bigScale) (if even index then 0 else smallScale)))
      orientDegenerate =
        V.generate
          count
          (\index -> QueryPoint (Point (fromIntegral index * bigScale) (fromIntegral index * smallScale)))
      zigzag = V.generate count (\index -> QueryPoint (Point (fromIntegral index) (if even index then 0 else 1)))
      collinear = V.generate count (\index -> QueryPoint (Point (fromIntegral index) (fromIntegral index)))
      collinearWide =
        V.generate
          count
          (\index -> QueryPoint (Point (fromIntegral index * bigScale) (fromIntegral index * smallScale)))
      orientationIndices = U.enumFromN 0 (max 0 (count - 2))
      circleIndices = U.enumFromN 0 (max 0 (count - 3))
  _ <- evaluate (force orientCertified)
  _ <- evaluate (force orientDegenerate)
  _ <- evaluate (force zigzag)
  _ <- evaluate (force collinear)
  _ <- evaluate (force collinearWide)
  _ <- evaluate (force orientationIndices)
  _ <- evaluate (force circleIndices)
  certifiedSupport <- allocationOf (sumTripleRelations approximateOrientation orientationIndices orientCertified)
  fallbackSupport <- allocationOf (sumTripleRelations approximateOrientation orientationIndices orientDegenerate)
  fixedSupport <- allocationOf (sumTripleRelations approximateOrientation orientationIndices collinear)
  certified <- allocationOf (sumTripleRelations orient2d orientationIndices orientCertified)
  fallback <- allocationOf (sumTripleRelations orient2d orientationIndices orientDegenerate)
  fixed <- allocationOf (sumTripleRelations orient2d orientationIndices collinear)
  let certifiedOracle = allocationBeyondApproximation certified certifiedSupport
      fallbackOracle = allocationBeyondApproximation fallback fallbackSupport
      fixedOracle = allocationBeyondApproximation fixed fixedSupport
  unless (fallbackOracle > 0) $
    fail "degenerate orientations allocated nothing: the measurement is not observing the oracle"
  unless (certifiedOracle * 8 < fallbackOracle) $
    fail
      ( "orient2d evaluates its exact oracle on certified input: "
          <> show certifiedOracle
          <> " excess bytes certified versus "
          <> show fallbackOracle
          <> " excess bytes degenerate (raw "
          <> show certified
          <> " versus "
          <> show fallback
          <> ")"
      )
  unless (fixedOracle * 8 < fallbackOracle) $
    fail
      ( "orient2d allocates on degenerate input its fixed-width worker resolves: "
          <> show fixedOracle
          <> " excess bytes within the width versus "
          <> show fallbackOracle
          <> " excess bytes beyond it"
      )
  certifiedCircleSupport <- allocationOf (sumQuadRelations approximateInCircle circleIndices zigzag)
  fallbackCircleSupport <- allocationOf (sumQuadRelations approximateInCircle circleIndices collinearWide)
  fixedCircleSupport <- allocationOf (sumQuadRelations approximateInCircle circleIndices collinear)
  certifiedCircle <- allocationOf (sumQuadRelations inCircle circleIndices zigzag)
  fallbackCircle <- allocationOf (sumQuadRelations inCircle circleIndices collinearWide)
  fixedCircle <- allocationOf (sumQuadRelations inCircle circleIndices collinear)
  let certifiedCircleOracle = allocationBeyondApproximation certifiedCircle certifiedCircleSupport
      fallbackCircleOracle = allocationBeyondApproximation fallbackCircle fallbackCircleSupport
      fixedCircleOracle = allocationBeyondApproximation fixedCircle fixedCircleSupport
  unless (fallbackCircleOracle > 0) $
    fail "degenerate incircles allocated nothing: the measurement is not observing the oracle"
  unless (certifiedCircleOracle * 8 < fallbackCircleOracle) $
    fail
      ( "inCircle evaluates its exact oracle on certified input: "
          <> show certifiedCircleOracle
          <> " excess bytes certified versus "
          <> show fallbackCircleOracle
          <> " excess bytes degenerate (raw "
          <> show certifiedCircle
          <> " versus "
          <> show fallbackCircle
          <> ")"
      )
  unless (fixedCircleOracle * 8 < fallbackCircleOracle) $
    fail
      ( "inCircle allocates on degenerate input its fixed-width worker resolves: "
          <> show fixedCircleOracle
          <> " excess bytes within the width versus "
          <> show fallbackCircleOracle
          <> " excess bytes beyond it"
      )
  putStrLn
    ( "filtered predicate allocation receipt: orient certified="
        <> show certifiedOracle <> " fixed=" <> show fixedOracle <> " fallback=" <> show fallbackOracle
        <> " incircle certified=" <> show certifiedCircleOracle
        <> " fixed=" <> show fixedCircleOracle <> " fallback=" <> show fallbackCircleOracle
    )

allocationOf :: Int -> IO Word64
allocationOf work = do
  performGC
  before <- allocated_bytes <$> getRTSStats
  _ <- evaluate work
  performGC
  after <- allocated_bytes <$> getRTSStats
  pure (after - before)

allocationBeyondApproximation :: Word64 -> Word64 -> Word64
allocationBeyondApproximation measured support = measured - min measured support

sumTripleRelations
  :: (QueryPoint -> QueryPoint -> QueryPoint -> Ordering)
  -> U.Vector Int
  -> V.Vector QueryPoint
  -> Int
sumTripleRelations relation indices points =
  U.foldl'
    (\accumulated index ->
       accumulated
         + fromEnum
           ( relation
               (V.unsafeIndex points index)
               (V.unsafeIndex points (index + 1))
               (V.unsafeIndex points (index + 2))
           )
    )
    0
    indices
{-# INLINE sumTripleRelations #-}

sumQuadRelations
  :: (QueryPoint -> QueryPoint -> QueryPoint -> QueryPoint -> Ordering)
  -> U.Vector Int
  -> V.Vector QueryPoint
  -> Int
sumQuadRelations relation indices points =
  U.foldl'
    (\accumulated index ->
       accumulated
         + fromEnum
           ( relation
               (V.unsafeIndex points index)
               (V.unsafeIndex points (index + 1))
               (V.unsafeIndex points (index + 2))
               (V.unsafeIndex points (index + 3))
           )
    )
    0
    indices
{-# INLINE sumQuadRelations #-}

approximateOrientation :: QueryPoint -> QueryPoint -> QueryPoint -> Ordering
approximateOrientation (QueryPoint a) (QueryPoint b) (QueryPoint c) =
  compare (orientDetApprox a b c) 0
{-# INLINE approximateOrientation #-}

approximateInCircle :: QueryPoint -> QueryPoint -> QueryPoint -> QueryPoint -> Ordering
approximateInCircle (QueryPoint a) (QueryPoint b) (QueryPoint c) (QueryPoint d) =
  compare (inCircleDetApprox a b c d) 0
{-# INLINE approximateInCircle #-}
