{-# LANGUAGE DerivingStrategies #-}

-- | Bounded exact polygonal approximation. The certificate bounds Hausdorff
-- distance in the declared affine metric, not topology or tangent error.
module Moonlight.Planar.Curve.Lowering
  ( LoweringPolicy
  , loweringPolicy
  , LoweringError (..)
  , LoweredPath
  , LoweredSpan
  , loweredPoints
  , loweredSpans
  , loweredMetric
  , loweredTolerance
  , spanSourceStep
  , spanParameterFrom
  , spanParameterTo
  , spanFrom
  , spanTo
  , spanSquaredBound
  , lowerStep
  , lowerOpenTrail
  , lowerClosedTrail
  , reverseLoweredPath
  ) where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Moonlight.Planar.Affine (Affine2, transformPoint)
import Moonlight.Planar.Curve
  ( CurveStep, OpenTrail, ClosedTrail, Located, location, locatedValue
  , trailSteps, closedTrailSteps, curveStepEnd, stepControlPoints, splitStepHalf )
import Moonlight.Planar.Exact
  ( ExactPoint, ExactVector (..), ExactRational
  , PositiveExact, positiveExactValue, exactHalf, divideByPositive, positiveSumSquares
  , exactVectorFromPoints, translateExactPoint )

data LoweringPolicy = LoweringPolicy !PositiveExact !Affine2 !Int !Int

-- | Samples remain in source coordinates. The affine map is used only to
-- measure error, allowing a renderer to retain its original transform scope.
loweringPolicy
  :: PositiveExact -> Affine2 -> Int -> Int
  -> Either LoweringError LoweringPolicy
loweringPolicy tolerance metric depth leaves
  | depth < 0 = Left (InvalidSubdivisionDepth depth)
  | leaves <= 0 = Left (InvalidLeafBudget leaves)
  | otherwise = Right (LoweringPolicy tolerance metric depth leaves)

data LoweringError
  = InvalidSubdivisionDepth !Int
  | InvalidLeafBudget !Int
  | SubdivisionDepthExhausted !Int !ExactRational !ExactRational !ExactRational
  | LeafBudgetExhausted !Int !ExactRational !ExactRational
  deriving stock (Eq, Show)

-- | Parameter endpoints are in the original source step. Reversal exchanges
-- them rather than fabricating a new sampling of the reversed curve.
data LoweredSpan = LoweredSpan
  !Int !ExactRational !ExactRational !ExactPoint !ExactPoint !ExactRational
  deriving stock (Eq, Show)

spanSourceStep :: LoweredSpan -> Int
spanSourceStep (LoweredSpan index _ _ _ _ _) = index

spanParameterFrom :: LoweredSpan -> ExactRational
spanParameterFrom (LoweredSpan _ from _ _ _ _) = from

spanParameterTo :: LoweredSpan -> ExactRational
spanParameterTo (LoweredSpan _ _ to _ _ _) = to

spanFrom :: LoweredSpan -> ExactPoint
spanFrom (LoweredSpan _ _ _ from _ _) = from

spanTo :: LoweredSpan -> ExactPoint
spanTo (LoweredSpan _ _ _ _ to _) = to

spanSquaredBound :: LoweredSpan -> ExactRational
spanSquaredBound (LoweredSpan _ _ _ _ _ bound) = bound

data LoweredPath = LoweredPath !PositiveExact !Affine2 !ExactPoint !(Seq LoweredSpan)
  deriving stock (Eq, Show)

loweredPoints :: LoweredPath -> NonEmpty ExactPoint
loweredPoints (LoweredPath _ _ origin spans) = origin :| map spanTo (toList spans)

loweredSpans :: LoweredPath -> [LoweredSpan]
loweredSpans (LoweredPath _ _ _ spans) = toList spans

loweredMetric :: LoweredPath -> Affine2
loweredMetric (LoweredPath _ metric _ _) = metric

loweredTolerance :: LoweredPath -> PositiveExact
loweredTolerance (LoweredPath tolerance _ _ _) = tolerance

reverseLoweredPath :: LoweredPath -> LoweredPath
reverseLoweredPath (LoweredPath tolerance metric origin spans) =
  LoweredPath tolerance metric endpoint (fmap reverseSpan (Seq.reverse spans))
 where
  endpoint = case Seq.viewr spans of
    Seq.EmptyR -> origin
    _ Seq.:> finalSpan -> spanTo finalSpan
  reverseSpan (LoweredSpan index t0 t1 from to bound) =
    LoweredSpan index t1 t0 to from bound

lowerStep :: LoweringPolicy -> Located CurveStep -> Either LoweringError LoweredPath
lowerStep policy value = lowerSteps policy (location value) (Seq.singleton (locatedValue value))

lowerOpenTrail :: LoweringPolicy -> Located OpenTrail -> Either LoweringError LoweredPath
lowerOpenTrail policy value = lowerSteps policy (location value) (trailSteps (locatedValue value))

lowerClosedTrail :: LoweringPolicy -> Located ClosedTrail -> Either LoweringError LoweredPath
lowerClosedTrail policy value = lowerSteps policy (location value) (closedTrailSteps (locatedValue value))

data LoweredPrefix = LoweredPrefix !ExactPoint !Int !(Seq LoweredSpan)

lowerSteps :: LoweringPolicy -> ExactPoint -> Seq CurveStep -> Either LoweringError LoweredPath
lowerSteps policy@(LoweringPolicy tolerance metric depth leaves) origin steps = do
  LoweredPrefix _ _ spans <-
    foldM appendStep (LoweredPrefix origin leaves Seq.empty) (Seq.mapWithIndex (,) steps)
  pure (LoweredPath tolerance metric origin spans)
 where
  appendStep (LoweredPrefix from remaining prefix) (index, step) = do
    (remainingAfter, spans) <- descend policy index depth remaining from 0 1 step
    pure (LoweredPrefix (translateExactPoint from (curveStepEnd step)) remainingAfter (prefix <> spans))

-- The binary subdivision tree is consumed directly; its depth and accepted
-- leaf budget bound traversal before any candidate exponential tree exists.
descend
  :: LoweringPolicy -> Int -> Int -> Int -> ExactPoint
  -> ExactRational -> ExactRational -> CurveStep
  -> Either LoweringError (Int, Seq LoweredSpan)
descend policy@(LoweringPolicy tolerance metric _ _) index depth remaining from t0 t1 step
  | remaining <= 0 = Left (LeafBudgetExhausted index t0 t1)
  | otherwise = do
      let bound = chordBound metric from step
      if bound <= epsilon * epsilon
        then Right
          ( remaining - 1
          , Seq.singleton (LoweredSpan index t0 t1 from endpoint bound)
          )
        else if depth == 0
          then Left (SubdivisionDepthExhausted index t0 t1 bound)
          else do
            let (left, right) = splitStepHalf step
                middle = (t0 + t1) * exactHalf
                splitPoint = translateExactPoint from (curveStepEnd left)
            (afterLeft, leftSpans) <- descend policy index (depth - 1) remaining from t0 middle left
            (afterRight, rightSpans) <- descend policy index (depth - 1) afterLeft splitPoint middle t1 right
            pure (afterRight, leftSpans <> rightSpans)
 where
  epsilon = positiveExactValue tolerance
  endpoint = translateExactPoint from (curveStepEnd step)

-- Positive rational Bernstein weights retain the convex-hull property.
-- A finite chord's epsilon capsule is convex; continuous endpoint-connected
-- projection also bounds chord-to-curve distance. A zero chord is a point,
-- not evidence that its source curve is constant.
chordBound :: Affine2 -> ExactPoint -> CurveStep -> ExactRational
chordBound metric from step =
  foldl' (\bound -> max bound . squaredChordDistance start end . project) 0 (stepControlPoints step)
 where
  project = transformPoint metric . translateExactPoint from
  start = transformPoint metric from
  end = project (curveStepEnd step)

squaredChordDistance :: ExactPoint -> ExactPoint -> ExactPoint -> ExactRational
squaredChordDistance start end point =
  case positiveSumSquares dx dy of
    Nothing -> dot relative relative
    Just chordSquared
      | projection <= 0 -> dot relative relative
      | projection >= positiveExactValue chordSquared -> dot beyond beyond
      | otherwise -> divideByPositive (cross * cross) chordSquared
 where
  chord@(ExactVector dx dy) = exactVectorFromPoints start end
  relative@(ExactVector px py) = exactVectorFromPoints start point
  beyond = exactVectorFromPoints end point
  projection = dot relative chord
  cross = dx * py - dy * px

dot :: ExactVector -> ExactVector -> ExactRational
dot (ExactVector ax ay) (ExactVector bx by) = ax * bx + ay * by
