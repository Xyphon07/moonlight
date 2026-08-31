{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.LinAlg.Pure.Geometry.Symmetric
  ( DiagonalizedSymmetric2 (..),
    mapDiagonalizedSymmetric2,
    diagonalizedSymmetric2ToTensor,
    diagonalizedSymmetric2ToVec2,
    eigendecomposeSymmetric2With,
    Symmetric2 (..),
    mapSymmetric2,
    zipSymmetric2With,
    diagonalSymmetric2,
    scaleSymmetric2,
    outerSymmetric2,
    traceSymmetric2,
    applySymmetric2,
    symmetric2Entries,
    symmetric2ToMatrix,
    eigendecomposeSymmetric2,
    DiagonalizedSymmetric3 (..),
    mapDiagonalizedSymmetric3,
    diagonalizedSymmetric3ToTensor,
    diagonalizedSymmetric3ToVec3,
    eigendecomposeSymmetric3With,
    eigendecomposeSymmetric3OrthonormalFrame,
    Symmetric3 (..),
    mapSymmetric3,
    zipSymmetric3With,
    diagonalSymmetric3,
    scaleSymmetric3,
    outerSymmetric3,
    traceSymmetric3,
    applySymmetric3,
    symmetric3Entries,
    symmetric3ToMatrix,
    eigendecomposeSymmetric3,
  )
where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Moonlight.Algebra.Pure.Module (BilinearSpace (..), Module (..), VectorSpace)
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..), Field, Metric (..), MoonlightError (..), MultiplicativeMonoid (..), Ring, fieldValueValid)
import Moonlight.LinAlg.Pure.Geometry.Frame (OrthonormalFrame, identityOrthonormalFrame, orthonormalFrameFromMatrixEntries)
import Moonlight.LinAlg.Pure.Dense.Types (Matrix, Vector, fromListMatrix, fromListVector, toListMatrix, toListVector)
import Moonlight.LinAlg.Pure.Geometry.Vec2 (Vec2 (..))
import Moonlight.LinAlg.Pure.Geometry.Vec3 (Vec3 (..))
import Prelude
  ( Bool,
    Double,
    Either (..),
    Eq,
    Int,
    Maybe (..),
    Monoid (..),
    Ord,
    Read,
    Semigroup (..),
    Show,
    abs,
    acos,
    all,
    cos,
    foldr,
    max,
    min,
    not,
    otherwise,
    pi,
    pure,
    sqrt,
    (&&),
    (||),
    (*),
    (+),
    (-),
    (.),
    (/),
    (<),
    (<=),
    (>=),
    (>),
    (>>=),
  )

type DiagonalizedSymmetric2 :: Type -> Type -> Type
data DiagonalizedSymmetric2 axes a = DiagonalizedSymmetric2
  { diag2XX :: !a,
    diag2YY :: !a,
    diag2Axes :: !axes
  }
  deriving stock (Eq, Ord, Show, Read)

mapDiagonalizedSymmetric2 ::
  (a -> b) ->
  DiagonalizedSymmetric2 axes a ->
  DiagonalizedSymmetric2 axes b
mapDiagonalizedSymmetric2 transform diagonalizedValue =
  DiagonalizedSymmetric2
    { diag2XX = transform (diag2XX diagonalizedValue),
      diag2YY = transform (diag2YY diagonalizedValue),
      diag2Axes = diag2Axes diagonalizedValue
    }

diagonalizedSymmetric2ToTensor ::
  AdditiveGroup a =>
  DiagonalizedSymmetric2 axes a ->
  Symmetric2 a
diagonalizedSymmetric2ToTensor diagonalizedValue =
  diagonalSymmetric2
    (diag2XX diagonalizedValue)
    (diag2YY diagonalizedValue)

diagonalizedSymmetric2ToVec2 ::
  DiagonalizedSymmetric2 axes Double ->
  Vec2
diagonalizedSymmetric2ToVec2 diagonalizedValue =
  Vec2
    (diag2XX diagonalizedValue)
    (diag2YY diagonalizedValue)

eigendecomposeSymmetric2With ::
  ([Double] -> Maybe axes) ->
  axes ->
  Symmetric2 Double ->
  Either MoonlightError (DiagonalizedSymmetric2 axes Double)
eigendecomposeSymmetric2With decodeAxes fallbackAxes tensorValue = do
  (eigenvalues, eigenvectors) <- eigendecomposeSymmetric2 tensorValue
  let resolvedAxes = fromMaybe fallbackAxes (decodeAxes (toListMatrix eigenvectors))
   in pure
        ( case toListVector eigenvalues of
            lambda1 : lambda2 : _ ->
              DiagonalizedSymmetric2
                { diag2XX = lambda1,
                  diag2YY = lambda2,
                  diag2Axes = resolvedAxes
                }
            lambda1 : _ ->
              DiagonalizedSymmetric2
                { diag2XX = lambda1,
                  diag2YY = 0.0,
                  diag2Axes = resolvedAxes
                }
            [] ->
              DiagonalizedSymmetric2
                { diag2XX = 0.0,
                  diag2YY = 0.0,
                  diag2Axes = resolvedAxes
                }
        )

type Symmetric2 :: Type -> Type
data Symmetric2 a = Symmetric2
  { sym2XX :: !a,
    sym2XY :: !a,
    sym2YY :: !a
  }
  deriving stock (Eq, Ord, Show, Read)

instance AdditiveMonoid a => Semigroup (Symmetric2 a) where
  (<>) = zipSymmetric2With add

instance AdditiveMonoid a => Monoid (Symmetric2 a) where
  mempty = diagonalSymmetric2 zero zero

instance AdditiveMonoid a => AdditiveMonoid (Symmetric2 a) where
  zero = mempty
  add = (<>)

instance AdditiveGroup a => AdditiveGroup (Symmetric2 a) where
  neg = mapSymmetric2 neg

instance Ring a => Module a (Symmetric2 a) where
  scale = scaleSymmetric2

instance Field a => VectorSpace a (Symmetric2 a)

instance Field a => BilinearSpace a (Symmetric2 a) where
  bilinearForm leftValue rightValue =
    let doubledUnit = add one one
     in foldr
          add
          zero
          [ mul (sym2XX leftValue) (sym2XX rightValue),
            mul (sym2YY leftValue) (sym2YY rightValue),
            mul doubledUnit (mul (sym2XY leftValue) (sym2XY rightValue))
          ]

instance Metric (Symmetric2 Double) where
  type Magnitude (Symmetric2 Double) = Double
  magnitude tensorValue = sqrt (bilinearForm tensorValue tensorValue)

mapSymmetric2 :: (a -> b) -> Symmetric2 a -> Symmetric2 b
mapSymmetric2 transform tensorValue =
  Symmetric2
    { sym2XX = transform (sym2XX tensorValue),
      sym2XY = transform (sym2XY tensorValue),
      sym2YY = transform (sym2YY tensorValue)
    }

zipSymmetric2With :: (a -> b -> c) -> Symmetric2 a -> Symmetric2 b -> Symmetric2 c
zipSymmetric2With combine leftValue rightValue =
  Symmetric2
    { sym2XX = combine (sym2XX leftValue) (sym2XX rightValue),
      sym2XY = combine (sym2XY leftValue) (sym2XY rightValue),
      sym2YY = combine (sym2YY leftValue) (sym2YY rightValue)
    }

diagonalSymmetric2 :: AdditiveMonoid a => a -> a -> Symmetric2 a
diagonalSymmetric2 diagonalX diagonalY =
  Symmetric2
    { sym2XX = diagonalX,
      sym2XY = zero,
      sym2YY = diagonalY
    }

scaleSymmetric2 :: MultiplicativeMonoid a => a -> Symmetric2 a -> Symmetric2 a
scaleSymmetric2 = mapSymmetric2 . mul

outerSymmetric2 :: Double -> Vec2 -> Symmetric2 Double
outerSymmetric2 weightValue (Vec2 xValue yValue) =
  Symmetric2
    { sym2XX = weightValue * xValue * xValue,
      sym2XY = weightValue * xValue * yValue,
      sym2YY = weightValue * yValue * yValue
    }

traceSymmetric2 :: AdditiveGroup a => Symmetric2 a -> a
traceSymmetric2 tensorValue =
  add (sym2XX tensorValue) (sym2YY tensorValue)

applySymmetric2 :: Symmetric2 Double -> Vec2 -> Vec2
applySymmetric2 tensorValue (Vec2 xValue yValue) =
  Vec2
    (sym2XX tensorValue * xValue + sym2XY tensorValue * yValue)
    (sym2XY tensorValue * xValue + sym2YY tensorValue * yValue)

symmetric2Entries :: Symmetric2 a -> [a]
symmetric2Entries tensorValue =
  [ sym2XX tensorValue,
    sym2XY tensorValue,
    sym2XY tensorValue,
    sym2YY tensorValue
  ]

symmetric2ToMatrix :: Symmetric2 a -> Either MoonlightError (Matrix 2 2 a)
symmetric2ToMatrix = fromListMatrix @2 @2 . symmetric2Entries

eigendecomposeSymmetric2 ::
  Symmetric2 Double ->
  Either MoonlightError (Vector 2 Double, Matrix 2 2 Double)
eigendecomposeSymmetric2 tensorValue =
  if not (all fieldValueValid (symmetric2Entries tensorValue))
    then Left (InvariantViolation "symmetric2 eigendecomposition requires finite entries")
    else
      let scaleValue = symmetric2Scale tensorValue
       in if scaleValue <= 0.0
            then symmetric2Result 1.0 0.0 (Vec2 1.0 0.0) 0.0 (Vec2 0.0 1.0)
            else
              let scaledTensor = scaleSymmetric2 (1.0 / scaleValue) tensorValue
                  aValue = sym2XX scaledTensor
                  bValue = sym2XY scaledTensor
                  dValue = sym2YY scaledTensor
                  meanValue = (aValue + dValue) / 2.0
                  halfDifference = (aValue - dValue) / 2.0
                  radiusValue = sqrt (halfDifference * halfDifference + bValue * bValue)
                  firstEigenvalue = meanValue + radiusValue
                  firstVector = symmetric2Eigenvector scaledTensor firstEigenvalue
                  secondVector = canonicalizeVec2Sign (perpendicularVec2 firstVector)
                  refinedFirst = rayleighQuotient2 tensorValue firstVector
                  refinedSecond = rayleighQuotient2 tensorValue secondVector
               in if refinedFirst >= refinedSecond
                    then symmetric2Result 1.0 refinedFirst firstVector refinedSecond secondVector
                    else symmetric2Result 1.0 refinedSecond secondVector refinedFirst firstVector

perpendicularVec2 :: Vec2 -> Vec2
perpendicularVec2 (Vec2 xValue yValue) =
  Vec2 (0.0 - yValue) xValue

rayleighQuotient2 :: Symmetric2 Double -> Vec2 -> Double
rayleighQuotient2 tensorValue vectorValue@(Vec2 xValue yValue) =
  let Vec2 imageX imageY = applySymmetric2 tensorValue vectorValue
   in (imageX * xValue + imageY * yValue) / (xValue * xValue + yValue * yValue)

type DiagonalizedSymmetric3 :: Type -> Type -> Type
data DiagonalizedSymmetric3 axes a = DiagonalizedSymmetric3
  { diag3XX :: a,
    diag3YY :: a,
    diag3ZZ :: a,
    diag3Axes :: axes
  }
  deriving stock (Eq, Ord, Show, Read)

mapDiagonalizedSymmetric3 ::
  (a -> b) ->
  DiagonalizedSymmetric3 axes a ->
  DiagonalizedSymmetric3 axes b
mapDiagonalizedSymmetric3 transform diagonalizedValue =
  DiagonalizedSymmetric3
    { diag3XX = transform (diag3XX diagonalizedValue),
      diag3YY = transform (diag3YY diagonalizedValue),
      diag3ZZ = transform (diag3ZZ diagonalizedValue),
      diag3Axes = diag3Axes diagonalizedValue
    }

diagonalizedSymmetric3ToTensor ::
  AdditiveGroup a =>
  DiagonalizedSymmetric3 axes a ->
  Symmetric3 a
diagonalizedSymmetric3ToTensor diagonalizedValue =
  diagonalSymmetric3
    (diag3XX diagonalizedValue)
    (diag3YY diagonalizedValue)
    (diag3ZZ diagonalizedValue)

diagonalizedSymmetric3ToVec3 ::
  DiagonalizedSymmetric3 axes Double ->
  Vec3
diagonalizedSymmetric3ToVec3 diagonalizedValue =
  Vec3
    (diag3XX diagonalizedValue)
    (diag3YY diagonalizedValue)
    (diag3ZZ diagonalizedValue)

eigendecomposeSymmetric3With ::
  ([Double] -> Maybe axes) ->
  axes ->
  Symmetric3 Double ->
  Either MoonlightError (DiagonalizedSymmetric3 axes Double)
eigendecomposeSymmetric3With decodeAxes fallbackAxes tensorValue = do
  (eigenvalues, eigenvectors) <- eigendecomposeSymmetric3 tensorValue
  let resolvedAxes = fromMaybe fallbackAxes (decodeAxes (toListMatrix eigenvectors))
   in pure
        ( case toListVector eigenvalues of
            lambda1 : lambda2 : lambda3 : _ ->
              DiagonalizedSymmetric3
                { diag3XX = lambda1,
                  diag3YY = lambda2,
                  diag3ZZ = lambda3,
                  diag3Axes = resolvedAxes
                }
            lambda1 : lambda2 : _ ->
              DiagonalizedSymmetric3
                { diag3XX = lambda1,
                  diag3YY = lambda2,
                  diag3ZZ = 0.0,
                  diag3Axes = resolvedAxes
                }
            lambda1 : _ ->
              DiagonalizedSymmetric3
                { diag3XX = lambda1,
                  diag3YY = 0.0,
                  diag3ZZ = 0.0,
                  diag3Axes = resolvedAxes
                }
            [] ->
              DiagonalizedSymmetric3
                { diag3XX = 0.0,
                  diag3YY = 0.0,
                  diag3ZZ = 0.0,
                  diag3Axes = resolvedAxes
                }
        )

eigendecomposeSymmetric3OrthonormalFrame ::
  Symmetric3 Double ->
  Either MoonlightError (DiagonalizedSymmetric3 OrthonormalFrame Double)
eigendecomposeSymmetric3OrthonormalFrame =
  eigendecomposeSymmetric3With
    orthonormalFrameFromMatrixEntries
    identityOrthonormalFrame

type Symmetric3 :: Type -> Type
data Symmetric3 a = Symmetric3
  { sym3XX :: a,
    sym3XY :: a,
    sym3XZ :: a,
    sym3YY :: a,
    sym3YZ :: a,
    sym3ZZ :: a
  }
  deriving stock (Eq, Ord, Show, Read)

instance AdditiveMonoid a => Semigroup (Symmetric3 a) where
  (<>) = zipSymmetric3With add

instance AdditiveMonoid a => Monoid (Symmetric3 a) where
  mempty = diagonalSymmetric3 zero zero zero

instance AdditiveMonoid a => AdditiveMonoid (Symmetric3 a) where
  zero = mempty
  add = (<>)

instance AdditiveGroup a => AdditiveGroup (Symmetric3 a) where
  neg = mapSymmetric3 neg

instance Ring a => Module a (Symmetric3 a) where
  scale = scaleSymmetric3

instance Field a => VectorSpace a (Symmetric3 a)

instance Field a => BilinearSpace a (Symmetric3 a) where
  bilinearForm leftValue rightValue =
    let doubledUnit = add one one
     in foldr
          add
          zero
          [ mul (sym3XX leftValue) (sym3XX rightValue),
            mul (sym3YY leftValue) (sym3YY rightValue),
            mul (sym3ZZ leftValue) (sym3ZZ rightValue),
            mul
              doubledUnit
              ( foldr
                  add
                  zero
                  [ mul (sym3XY leftValue) (sym3XY rightValue),
                    mul (sym3XZ leftValue) (sym3XZ rightValue),
                    mul (sym3YZ leftValue) (sym3YZ rightValue)
                  ]
              )
          ]

instance Metric (Symmetric3 Double) where
  type Magnitude (Symmetric3 Double) = Double
  magnitude tensorValue = sqrt (bilinearForm tensorValue tensorValue)

mapSymmetric3 :: (a -> b) -> Symmetric3 a -> Symmetric3 b
mapSymmetric3 transform tensorValue =
  Symmetric3
    { sym3XX = transform (sym3XX tensorValue),
      sym3XY = transform (sym3XY tensorValue),
      sym3XZ = transform (sym3XZ tensorValue),
      sym3YY = transform (sym3YY tensorValue),
      sym3YZ = transform (sym3YZ tensorValue),
      sym3ZZ = transform (sym3ZZ tensorValue)
    }

zipSymmetric3With :: (a -> b -> c) -> Symmetric3 a -> Symmetric3 b -> Symmetric3 c
zipSymmetric3With combine leftValue rightValue =
  Symmetric3
    { sym3XX = combine (sym3XX leftValue) (sym3XX rightValue),
      sym3XY = combine (sym3XY leftValue) (sym3XY rightValue),
      sym3XZ = combine (sym3XZ leftValue) (sym3XZ rightValue),
      sym3YY = combine (sym3YY leftValue) (sym3YY rightValue),
      sym3YZ = combine (sym3YZ leftValue) (sym3YZ rightValue),
      sym3ZZ = combine (sym3ZZ leftValue) (sym3ZZ rightValue)
    }

diagonalSymmetric3 :: AdditiveMonoid a => a -> a -> a -> Symmetric3 a
diagonalSymmetric3 diagonalX diagonalY diagonalZ =
  Symmetric3
    { sym3XX = diagonalX,
      sym3XY = zero,
      sym3XZ = zero,
      sym3YY = diagonalY,
      sym3YZ = zero,
      sym3ZZ = diagonalZ
    }

scaleSymmetric3 :: MultiplicativeMonoid a => a -> Symmetric3 a -> Symmetric3 a
scaleSymmetric3 = mapSymmetric3 . mul

outerSymmetric3 :: Double -> Vec3 -> Symmetric3 Double
outerSymmetric3 weightValue (Vec3 xValue yValue zValue) =
  Symmetric3
    { sym3XX = weightValue * xValue * xValue,
      sym3XY = weightValue * xValue * yValue,
      sym3XZ = weightValue * xValue * zValue,
      sym3YY = weightValue * yValue * yValue,
      sym3YZ = weightValue * yValue * zValue,
      sym3ZZ = weightValue * zValue * zValue
    }

traceSymmetric3 :: AdditiveGroup a => Symmetric3 a -> a
traceSymmetric3 tensorValue =
  foldr add zero [sym3XX tensorValue, sym3YY tensorValue, sym3ZZ tensorValue]

applySymmetric3 :: Symmetric3 Double -> Vec3 -> Vec3
applySymmetric3 tensorValue (Vec3 xValue yValue zValue) =
  Vec3
    (sym3XX tensorValue * xValue + sym3XY tensorValue * yValue + sym3XZ tensorValue * zValue)
    (sym3XY tensorValue * xValue + sym3YY tensorValue * yValue + sym3YZ tensorValue * zValue)
    (sym3XZ tensorValue * xValue + sym3YZ tensorValue * yValue + sym3ZZ tensorValue * zValue)

symmetric3Entries :: Symmetric3 a -> [a]
symmetric3Entries tensorValue =
  [ sym3XX tensorValue,
    sym3XY tensorValue,
    sym3XZ tensorValue,
    sym3XY tensorValue,
    sym3YY tensorValue,
    sym3YZ tensorValue,
    sym3XZ tensorValue,
    sym3YZ tensorValue,
    sym3ZZ tensorValue
  ]

symmetric3ToMatrix :: Symmetric3 a -> Either MoonlightError (Matrix 3 3 a)
symmetric3ToMatrix = fromListMatrix @3 @3 . symmetric3Entries

eigendecomposeSymmetric3 ::
  Symmetric3 Double ->
  Either MoonlightError (Vector 3 Double, Matrix 3 3 Double)
eigendecomposeSymmetric3 tensorValue =
  if not (all fieldValueValid (symmetric3Entries tensorValue))
    then Left (InvariantViolation "symmetric3 eigendecomposition requires finite entries")
    else
      let scaleValue = symmetric3Scale tensorValue
       in if scaleValue <= 0.0
            then
              symmetric3Result
                1.0
                ( EigenColumn3 0.0 (Vec3 1.0 0.0 0.0),
                  EigenColumn3 0.0 (Vec3 0.0 1.0 0.0),
                  EigenColumn3 0.0 (Vec3 0.0 0.0 1.0)
                )
            else
              let scaledTensor = scaleSymmetric3 (1.0 / scaleValue) tensorValue
                  analyticEigenvalues = symmetric3AnalyticEigenvalues scaledTensor
                  analyticCandidate = symmetric3AnalyticCandidate scaledTensor analyticEigenvalues
                  (firstColumn, secondColumn, thirdColumn) =
                    case analyticCandidate of
                      Just eigenColumns
                        | symmetric3CandidateAcceptable scaledTensor eigenColumns ->
                            eigenColumns
                      _ -> symmetric3JacobiEigenColumns scaledTensor
               in symmetric3Result
                    1.0
                    ( sortEigenColumns3
                        (rayleighRefineColumn3 tensorValue firstColumn)
                        (rayleighRefineColumn3 tensorValue secondColumn)
                        (rayleighRefineColumn3 tensorValue thirdColumn)
                    )

symmetric2Scale :: Symmetric2 Double -> Double
symmetric2Scale tensorValue =
  max (abs (sym2XX tensorValue)) (max (abs (sym2XY tensorValue)) (abs (sym2YY tensorValue)))

symmetric3Scale :: Symmetric3 Double -> Double
symmetric3Scale tensorValue =
  max
    (abs (sym3XX tensorValue))
    ( max
        (abs (sym3XY tensorValue))
        ( max
            (abs (sym3XZ tensorValue))
            ( max
                (abs (sym3YY tensorValue))
                (max (abs (sym3YZ tensorValue)) (abs (sym3ZZ tensorValue)))
            )
        )
    )

symmetric2Result ::
  Double ->
  Double ->
  Vec2 ->
  Double ->
  Vec2 ->
  Either MoonlightError (Vector 2 Double, Matrix 2 2 Double)
symmetric2Result scaleValue firstEigenvalue firstVector secondEigenvalue secondVector = do
  eigenvalues <- fromListVector @2 [scaleValue * firstEigenvalue, scaleValue * secondEigenvalue]
  eigenvectors <- fromListMatrix @2 @2 (matrix2Columns firstVector secondVector)
  pure (eigenvalues, eigenvectors)

symmetric2Eigenvector :: Symmetric2 Double -> Double -> Vec2
symmetric2Eigenvector tensorValue eigenvalue =
  canonicalizeVec2Sign
    ( if abs (sym2XY tensorValue) <= 1.0e-14
        then
          if abs (sym2XX tensorValue - eigenvalue) <= abs (sym2YY tensorValue - eigenvalue)
            then Vec2 1.0 0.0
            else Vec2 0.0 1.0
        else
          normalizeVec2Or
            (Vec2 1.0 0.0)
            ( largerVec2
                (Vec2 (sym2XY tensorValue) (eigenvalue - sym2XX tensorValue))
                (Vec2 (eigenvalue - sym2YY tensorValue) (sym2XY tensorValue))
            )
    )

largerVec2 :: Vec2 -> Vec2 -> Vec2
largerVec2 leftValue rightValue =
  if vec2NormSquared leftValue >= vec2NormSquared rightValue
    then leftValue
    else rightValue

matrix2Columns :: Vec2 -> Vec2 -> [Double]
matrix2Columns (Vec2 x1 y1) (Vec2 x2 y2) =
  [x1, x2, y1, y2]

normalizeVec2Or :: Vec2 -> Vec2 -> Vec2
normalizeVec2Or fallbackValue vectorValue =
  let normValue = sqrt (vec2NormSquared vectorValue)
   in if normValue <= 1.0e-24
        then fallbackValue
        else scaleVec2Local (1.0 / normValue) vectorValue

vec2NormSquared :: Vec2 -> Double
vec2NormSquared (Vec2 xValue yValue) =
  xValue * xValue + yValue * yValue

scaleVec2Local :: Double -> Vec2 -> Vec2
scaleVec2Local scaleValue (Vec2 xValue yValue) =
  Vec2 (scaleValue * xValue) (scaleValue * yValue)

canonicalizeVec2Sign :: Vec2 -> Vec2
canonicalizeVec2Sign vectorValue@(Vec2 xValue yValue)
  | abs xValue > 1.0e-14 =
      if xValue < 0.0
        then scaleVec2Local (-1.0) vectorValue
        else vectorValue
  | yValue < 0.0 = scaleVec2Local (-1.0) vectorValue
  | otherwise = vectorValue

data EigenColumn3 = EigenColumn3
  { eigenColumn3Value :: !Double,
    eigenColumn3Vector :: !Vec3
  }

data Symmetric3Pivot
  = PivotXY
  | PivotXZ
  | PivotYZ

symmetric3AnalyticEigenvalues :: Symmetric3 Double -> (Double, Double, Double)
symmetric3AnalyticEigenvalues tensorValue =
  let meanValue = (sym3XX tensorValue + sym3YY tensorValue + sym3ZZ tensorValue) / 3.0
      xxCentered = sym3XX tensorValue - meanValue
      yyCentered = sym3YY tensorValue - meanValue
      zzCentered = sym3ZZ tensorValue - meanValue
      secondMoment =
        xxCentered * xxCentered
          + yyCentered * yyCentered
          + zzCentered * zzCentered
          + 2.0 * (sym3XY tensorValue * sym3XY tensorValue + sym3XZ tensorValue * sym3XZ tensorValue + sym3YZ tensorValue * sym3YZ tensorValue)
      scaleMoment = sqrt (secondMoment / 6.0)
   in if scaleMoment <= 1.0e-24
        then (meanValue, meanValue, meanValue)
        else
          let inverseScaleMoment = 1.0 / scaleMoment
              normalized =
                Symmetric3
                  { sym3XX = xxCentered * inverseScaleMoment,
                    sym3XY = sym3XY tensorValue * inverseScaleMoment,
                    sym3XZ = sym3XZ tensorValue * inverseScaleMoment,
                    sym3YY = yyCentered * inverseScaleMoment,
                    sym3YZ = sym3YZ tensorValue * inverseScaleMoment,
                    sym3ZZ = zzCentered * inverseScaleMoment
                  }
              determinantHalf = symmetric3Determinant normalized / 2.0
              angleValue =
                if determinantHalf <= -1.0
                  then pi / 3.0
                  else
                    if determinantHalf >= 1.0
                      then 0.0
                      else acos determinantHalf / 3.0
              largestValue = meanValue + 2.0 * scaleMoment * cos angleValue
              smallestValue = meanValue + 2.0 * scaleMoment * cos (angleValue + 2.0 * pi / 3.0)
              middleValue = 3.0 * meanValue - largestValue - smallestValue
           in sortEigenvalues3 largestValue middleValue smallestValue

symmetric3AnalyticCandidate ::
  Symmetric3 Double ->
  (Double, Double, Double) ->
  Maybe (EigenColumn3, EigenColumn3, EigenColumn3)
symmetric3AnalyticCandidate tensorValue eigenvalues@(firstEigenvalue, secondEigenvalue, thirdEigenvalue)
  | symmetric3EigenvaluesDegenerate eigenvalues = Nothing
  | otherwise =
      case symmetric3Eigenvector tensorValue firstEigenvalue of
        Nothing -> Nothing
        Just firstVector ->
          case symmetric3Eigenvector tensorValue secondEigenvalue >>= orthogonalizeVec3Against firstVector of
            Nothing -> Nothing
            Just secondVector ->
              case normalizeVec3Maybe (crossVec3Local firstVector secondVector) of
                Nothing -> Nothing
                Just thirdVector ->
                  Just
                    ( EigenColumn3 firstEigenvalue (canonicalizeVec3Sign firstVector),
                      EigenColumn3 secondEigenvalue (canonicalizeVec3Sign secondVector),
                      EigenColumn3 thirdEigenvalue (canonicalizeVec3Sign thirdVector)
                    )

symmetric3EigenvaluesDegenerate :: (Double, Double, Double) -> Bool
symmetric3EigenvaluesDegenerate (firstEigenvalue, secondEigenvalue, thirdEigenvalue) =
  min (abs (firstEigenvalue - secondEigenvalue)) (abs (secondEigenvalue - thirdEigenvalue)) <= 1.0e-10

symmetric3Eigenvector :: Symmetric3 Double -> Double -> Maybe Vec3
symmetric3Eigenvector tensorValue eigenvalue =
  let rowX = Vec3 (sym3XX tensorValue - eigenvalue) (sym3XY tensorValue) (sym3XZ tensorValue)
      rowY = Vec3 (sym3XY tensorValue) (sym3YY tensorValue - eigenvalue) (sym3YZ tensorValue)
      rowZ = Vec3 (sym3XZ tensorValue) (sym3YZ tensorValue) (sym3ZZ tensorValue - eigenvalue)
      candidateValue =
        largestVec3
          (crossVec3Local rowX rowY)
          (largestVec3 (crossVec3Local rowX rowZ) (crossVec3Local rowY rowZ))
   in normalizeVec3Maybe candidateValue

symmetric3CandidateAcceptable ::
  Symmetric3 Double ->
  (EigenColumn3, EigenColumn3, EigenColumn3) ->
  Bool
symmetric3CandidateAcceptable tensorValue (firstColumn, secondColumn, thirdColumn) =
  let firstVector = eigenColumn3Vector firstColumn
      secondVector = eigenColumn3Vector secondColumn
      thirdVector = eigenColumn3Vector thirdColumn
      residualBound =
        max
          (symmetric3ResidualNorm tensorValue firstColumn)
          (max (symmetric3ResidualNorm tensorValue secondColumn) (symmetric3ResidualNorm tensorValue thirdColumn))
   in residualBound <= 1.0e-8
        && abs (vec3Norm firstVector - 1.0) <= 1.0e-10
        && abs (vec3Norm secondVector - 1.0) <= 1.0e-10
        && abs (vec3Norm thirdVector - 1.0) <= 1.0e-10
        && abs (dotVec3Local firstVector secondVector) <= 1.0e-9
        && abs (dotVec3Local firstVector thirdVector) <= 1.0e-9
        && abs (dotVec3Local secondVector thirdVector) <= 1.0e-9

rayleighRefineColumn3 :: Symmetric3 Double -> EigenColumn3 -> EigenColumn3
rayleighRefineColumn3 tensorValue eigenColumn =
  let vectorValue = eigenColumn3Vector eigenColumn
      refinedValue =
        dotVec3Local (applySymmetric3 tensorValue vectorValue) vectorValue
          / dotVec3Local vectorValue vectorValue
   in EigenColumn3 refinedValue vectorValue

symmetric3ResidualNorm :: Symmetric3 Double -> EigenColumn3 -> Double
symmetric3ResidualNorm tensorValue eigenColumn =
  vec3Norm
    ( subVec3Local
        (applySymmetric3 tensorValue (eigenColumn3Vector eigenColumn))
        (scaleVec3Local (eigenColumn3Value eigenColumn) (eigenColumn3Vector eigenColumn))
    )

symmetric3JacobiEigenColumns ::
  Symmetric3 Double ->
  (EigenColumn3, EigenColumn3, EigenColumn3)
symmetric3JacobiEigenColumns tensorValue =
  let (diagonalizedTensor, firstVector, secondVector, thirdVector) =
        symmetric3Jacobi 72 tensorValue (Vec3 1.0 0.0 0.0) (Vec3 0.0 1.0 0.0) (Vec3 0.0 0.0 1.0)
   in sortEigenColumns3
        (EigenColumn3 (sym3XX diagonalizedTensor) (canonicalizeVec3Sign firstVector))
        (EigenColumn3 (sym3YY diagonalizedTensor) (canonicalizeVec3Sign secondVector))
        (EigenColumn3 (sym3ZZ diagonalizedTensor) (canonicalizeVec3Sign thirdVector))

symmetric3Jacobi ::
  Int ->
  Symmetric3 Double ->
  Vec3 ->
  Vec3 ->
  Vec3 ->
  (Symmetric3 Double, Vec3, Vec3, Vec3)
symmetric3Jacobi remainingSteps tensorValue firstVector secondVector thirdVector =
  if remainingSteps <= 0 || symmetric3OffDiagonalMax tensorValue <= 1.0e-15
    then (tensorValue, firstVector, secondVector, thirdVector)
    else
      let pivotValue = symmetric3LargestPivot tensorValue
          (cosineValue, sineValue, tangentValue) = symmetric3JacobiRotation tensorValue pivotValue
          (nextTensor, nextFirstVector, nextSecondVector, nextThirdVector) =
            symmetric3ApplyJacobi pivotValue cosineValue sineValue tangentValue tensorValue firstVector secondVector thirdVector
       in symmetric3Jacobi (remainingSteps - 1) nextTensor nextFirstVector nextSecondVector nextThirdVector

symmetric3LargestPivot :: Symmetric3 Double -> Symmetric3Pivot
symmetric3LargestPivot tensorValue =
  let xyMagnitude = abs (sym3XY tensorValue)
      xzMagnitude = abs (sym3XZ tensorValue)
      yzMagnitude = abs (sym3YZ tensorValue)
   in if xyMagnitude >= xzMagnitude && xyMagnitude >= yzMagnitude
        then PivotXY
        else
          if xzMagnitude >= yzMagnitude
            then PivotXZ
            else PivotYZ

symmetric3JacobiRotation ::
  Symmetric3 Double ->
  Symmetric3Pivot ->
  (Double, Double, Double)
symmetric3JacobiRotation tensorValue pivotValue =
  let (leftDiagonal, rightDiagonal, offDiagonal) =
        case pivotValue of
          PivotXY -> (sym3XX tensorValue, sym3YY tensorValue, sym3XY tensorValue)
          PivotXZ -> (sym3XX tensorValue, sym3ZZ tensorValue, sym3XZ tensorValue)
          PivotYZ -> (sym3YY tensorValue, sym3ZZ tensorValue, sym3YZ tensorValue)
   in if abs offDiagonal <= 1.0e-30
        then (1.0, 0.0, 0.0)
        else
          let tauValue = (rightDiagonal - leftDiagonal) / (2.0 * offDiagonal)
              signValue =
                if tauValue < 0.0
                  then -1.0
                  else 1.0
              tangentValue = signValue / (abs tauValue + sqrt (1.0 + tauValue * tauValue))
              cosineValue = 1.0 / sqrt (1.0 + tangentValue * tangentValue)
              sineValue = tangentValue * cosineValue
           in (cosineValue, sineValue, tangentValue)

symmetric3ApplyJacobi ::
  Symmetric3Pivot ->
  Double ->
  Double ->
  Double ->
  Symmetric3 Double ->
  Vec3 ->
  Vec3 ->
  Vec3 ->
  (Symmetric3 Double, Vec3, Vec3, Vec3)
symmetric3ApplyJacobi pivotValue cosineValue sineValue tangentValue tensorValue firstVector secondVector thirdVector =
  case pivotValue of
    PivotXY ->
      ( Symmetric3
          { sym3XX = sym3XX tensorValue - tangentValue * sym3XY tensorValue,
            sym3XY = 0.0,
            sym3XZ = cosineValue * sym3XZ tensorValue - sineValue * sym3YZ tensorValue,
            sym3YY = sym3YY tensorValue + tangentValue * sym3XY tensorValue,
            sym3YZ = sineValue * sym3XZ tensorValue + cosineValue * sym3YZ tensorValue,
            sym3ZZ = sym3ZZ tensorValue
          },
        combineRotatedVec3 cosineValue (-sineValue) firstVector secondVector,
        combineRotatedVec3 sineValue cosineValue firstVector secondVector,
        thirdVector
      )
    PivotXZ ->
      ( Symmetric3
          { sym3XX = sym3XX tensorValue - tangentValue * sym3XZ tensorValue,
            sym3XY = cosineValue * sym3XY tensorValue - sineValue * sym3YZ tensorValue,
            sym3XZ = 0.0,
            sym3YY = sym3YY tensorValue,
            sym3YZ = sineValue * sym3XY tensorValue + cosineValue * sym3YZ tensorValue,
            sym3ZZ = sym3ZZ tensorValue + tangentValue * sym3XZ tensorValue
          },
        combineRotatedVec3 cosineValue (-sineValue) firstVector thirdVector,
        secondVector,
        combineRotatedVec3 sineValue cosineValue firstVector thirdVector
      )
    PivotYZ ->
      ( Symmetric3
          { sym3XX = sym3XX tensorValue,
            sym3XY = cosineValue * sym3XY tensorValue - sineValue * sym3XZ tensorValue,
            sym3XZ = sineValue * sym3XY tensorValue + cosineValue * sym3XZ tensorValue,
            sym3YY = sym3YY tensorValue - tangentValue * sym3YZ tensorValue,
            sym3YZ = 0.0,
            sym3ZZ = sym3ZZ tensorValue + tangentValue * sym3YZ tensorValue
          },
        firstVector,
        combineRotatedVec3 cosineValue (-sineValue) secondVector thirdVector,
        combineRotatedVec3 sineValue cosineValue secondVector thirdVector
      )

combineRotatedVec3 :: Double -> Double -> Vec3 -> Vec3 -> Vec3
combineRotatedVec3 leftScale rightScale leftVector rightVector =
  addVec3Local (scaleVec3Local leftScale leftVector) (scaleVec3Local rightScale rightVector)

symmetric3OffDiagonalMax :: Symmetric3 Double -> Double
symmetric3OffDiagonalMax tensorValue =
  max (abs (sym3XY tensorValue)) (max (abs (sym3XZ tensorValue)) (abs (sym3YZ tensorValue)))

symmetric3Result ::
  Double ->
  (EigenColumn3, EigenColumn3, EigenColumn3) ->
  Either MoonlightError (Vector 3 Double, Matrix 3 3 Double)
symmetric3Result scaleValue (firstColumn, secondColumn, thirdColumn) = do
  eigenvalues <-
    fromListVector
      @3
      [ scaleValue * eigenColumn3Value firstColumn,
        scaleValue * eigenColumn3Value secondColumn,
        scaleValue * eigenColumn3Value thirdColumn
      ]
  eigenvectors <-
    fromListMatrix
      @3
      @3
      ( matrix3Columns
          (eigenColumn3Vector firstColumn)
          (eigenColumn3Vector secondColumn)
          (eigenColumn3Vector thirdColumn)
      )
  pure (eigenvalues, eigenvectors)

matrix3Columns :: Vec3 -> Vec3 -> Vec3 -> [Double]
matrix3Columns (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) (Vec3 x3 y3 z3) =
  [x1, x2, x3, y1, y2, y3, z1, z2, z3]

sortEigenvalues3 :: Double -> Double -> Double -> (Double, Double, Double)
sortEigenvalues3 firstValue secondValue thirdValue =
  let (largestValue, smallerValue) = orderEigenvaluePair firstValue secondValue
      (middleCandidate, smallestValue) = orderEigenvaluePair smallerValue thirdValue
      (finalLargest, finalMiddle) = orderEigenvaluePair largestValue middleCandidate
   in (finalLargest, finalMiddle, smallestValue)

orderEigenvaluePair :: Double -> Double -> (Double, Double)
orderEigenvaluePair leftValue rightValue =
  if leftValue >= rightValue
    then (leftValue, rightValue)
    else (rightValue, leftValue)

sortEigenColumns3 ::
  EigenColumn3 ->
  EigenColumn3 ->
  EigenColumn3 ->
  (EigenColumn3, EigenColumn3, EigenColumn3)
sortEigenColumns3 firstColumn secondColumn thirdColumn =
  let (largestColumn, smallerColumn) = orderEigenColumnPair firstColumn secondColumn
      (middleCandidate, smallestColumn) = orderEigenColumnPair smallerColumn thirdColumn
      (finalLargest, finalMiddle) = orderEigenColumnPair largestColumn middleCandidate
   in (finalLargest, finalMiddle, smallestColumn)

orderEigenColumnPair :: EigenColumn3 -> EigenColumn3 -> (EigenColumn3, EigenColumn3)
orderEigenColumnPair leftColumn rightColumn =
  if eigenColumn3Value leftColumn >= eigenColumn3Value rightColumn
    then (leftColumn, rightColumn)
    else (rightColumn, leftColumn)

symmetric3Determinant :: Symmetric3 Double -> Double
symmetric3Determinant tensorValue =
  sym3XX tensorValue * sym3YY tensorValue * sym3ZZ tensorValue
    + 2.0 * sym3XY tensorValue * sym3XZ tensorValue * sym3YZ tensorValue
    - sym3XX tensorValue * sym3YZ tensorValue * sym3YZ tensorValue
    - sym3YY tensorValue * sym3XZ tensorValue * sym3XZ tensorValue
    - sym3ZZ tensorValue * sym3XY tensorValue * sym3XY tensorValue

orthogonalizeVec3Against :: Vec3 -> Vec3 -> Maybe Vec3
orthogonalizeVec3Against axisValue vectorValue =
  normalizeVec3Maybe (subVec3Local vectorValue (scaleVec3Local (dotVec3Local axisValue vectorValue) axisValue))

largestVec3 :: Vec3 -> Vec3 -> Vec3
largestVec3 leftValue rightValue =
  if vec3NormSquared leftValue >= vec3NormSquared rightValue
    then leftValue
    else rightValue

normalizeVec3Maybe :: Vec3 -> Maybe Vec3
normalizeVec3Maybe vectorValue =
  let normValue = vec3Norm vectorValue
   in if normValue <= 1.0e-12
        then Nothing
        else Just (scaleVec3Local (1.0 / normValue) vectorValue)

vec3Norm :: Vec3 -> Double
vec3Norm vectorValue =
  sqrt (vec3NormSquared vectorValue)

vec3NormSquared :: Vec3 -> Double
vec3NormSquared (Vec3 xValue yValue zValue) =
  xValue * xValue + yValue * yValue + zValue * zValue

dotVec3Local :: Vec3 -> Vec3 -> Double
dotVec3Local (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  x1 * x2 + y1 * y2 + z1 * z2

crossVec3Local :: Vec3 -> Vec3 -> Vec3
crossVec3Local (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)

addVec3Local :: Vec3 -> Vec3 -> Vec3
addVec3Local (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

subVec3Local :: Vec3 -> Vec3 -> Vec3
subVec3Local (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

scaleVec3Local :: Double -> Vec3 -> Vec3
scaleVec3Local scaleValue (Vec3 xValue yValue zValue) =
  Vec3 (scaleValue * xValue) (scaleValue * yValue) (scaleValue * zValue)

canonicalizeVec3Sign :: Vec3 -> Vec3
canonicalizeVec3Sign vectorValue@(Vec3 xValue yValue zValue)
  | abs xValue > 1.0e-14 =
      if xValue < 0.0
        then scaleVec3Local (-1.0) vectorValue
        else vectorValue
  | abs yValue > 1.0e-14 =
      if yValue < 0.0
        then scaleVec3Local (-1.0) vectorValue
        else vectorValue
  | zValue < 0.0 = scaleVec3Local (-1.0) vectorValue
  | otherwise = vectorValue
