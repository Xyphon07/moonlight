module Moonlight.Core.ApproxEq
  ( AbsTol,
    mkAbsTol,
    absTol,
    absTolValue,
    RelTol,
    mkRelTol,
    relTol,
    relTolValue,
    UlpTol,
    mkUlpTol,
    ulpTolValue,
    Tolerance (..),
    normalizeTolerance,
    withinToleranceBy,
    ApproxEq (..),
    withinTol,
  )
where

import Data.Kind (Constraint, Type)
import qualified Data.Set as Set
import Data.Word (Word64)
import Moonlight.Core.Canon (mkNonNegativeFiniteDouble)
import Moonlight.Core.Error (MoonlightError)
import Moonlight.Internal.FloatMath (ulpDistance, ulpDistanceFloat)
import Prelude

type AbsTol :: Type
-- | Non-negative finite absolute tolerance.
newtype AbsTol = AbsTol Double
  deriving stock (Eq, Ord, Show)

mkAbsTol :: Double -> Either MoonlightError AbsTol
mkAbsTol = fmap AbsTol . mkNonNegativeFiniteDouble "absolute tolerance"

absTol :: Double -> Either MoonlightError AbsTol
absTol = mkAbsTol

absTolValue :: AbsTol -> Double
absTolValue (AbsTol value) = value

type RelTol :: Type
-- | Non-negative finite relative tolerance.
newtype RelTol = RelTol Double
  deriving stock (Eq, Ord, Show)

mkRelTol :: Double -> Either MoonlightError RelTol
mkRelTol = fmap RelTol . mkNonNegativeFiniteDouble "relative tolerance"

relTol :: Double -> Either MoonlightError RelTol
relTol = mkRelTol

relTolValue :: RelTol -> Double
relTolValue (RelTol value) = value

type UlpTol :: Type
-- | Unsigned ULP-distance tolerance.
newtype UlpTol = UlpTol Word64
  deriving stock (Eq, Ord, Show)

mkUlpTol :: Word64 -> UlpTol
mkUlpTol = UlpTol

ulpTolValue :: UlpTol -> Word64
ulpTolValue (UlpTol value) = value

type Tolerance :: Type
-- | Composite tolerance expression.
--
-- 'CompositeTol' is conjunction (meet) and 'DisjunctiveTol' is disjunction
-- (join) in the implication order over the instance's valid comparison domain.
-- 'Exact' is the bottom element of that order: @Exact /\ t = Exact@ and
-- @Exact \/ t = t@ whenever @t@ obeys the 'ApproxEq' reflexivity law on the
-- valid comparison domain.
data Tolerance
  = Exact
  | AbsTolBound !AbsTol
  | RelTolBound !RelTol
  | UlpTolBound !UlpTol
  | CompositeTol !Tolerance !Tolerance
  | DisjunctiveTol !Tolerance !Tolerance
  deriving stock (Eq, Ord, Show)

-- | Put a 'Tolerance' expression into canonical algebraic normal form.
--
-- The normal form recursively flattens associative 'CompositeTol' and
-- 'DisjunctiveTol' nests, sorts and deduplicates branches by structural order,
-- rebuilds multi-branch expressions as left-associated chains, collapses
-- @Exact /\ t@ to 'Exact', and drops 'Exact' from disjunctions as the bottom
-- element of the implication order.
normalizeTolerance :: Tolerance -> Tolerance
normalizeTolerance toleranceValue =
  case toleranceValue of
    Exact -> Exact
    AbsTolBound value -> AbsTolBound value
    RelTolBound value -> RelTolBound value
    UlpTolBound value -> UlpTolBound value
    CompositeTol leftTolerance rightTolerance ->
      normalizeCompositeTolerance
        ( compositeToleranceBranches (normalizeTolerance leftTolerance)
            <> compositeToleranceBranches (normalizeTolerance rightTolerance)
        )
    DisjunctiveTol leftTolerance rightTolerance ->
      normalizeDisjunctiveTolerance
        ( disjunctiveToleranceBranches (normalizeTolerance leftTolerance)
            <> disjunctiveToleranceBranches (normalizeTolerance rightTolerance)
        )

normalizeCompositeTolerance :: [Tolerance] -> Tolerance
normalizeCompositeTolerance branches
  | any (== Exact) branches = Exact
  | otherwise = rebuildCompositeTolerance (canonicalToleranceBranches branches)

normalizeDisjunctiveTolerance :: [Tolerance] -> Tolerance
normalizeDisjunctiveTolerance =
  rebuildDisjunctiveTolerance . canonicalToleranceBranches . filter (/= Exact)

compositeToleranceBranches :: Tolerance -> [Tolerance]
compositeToleranceBranches toleranceValue =
  case toleranceValue of
    CompositeTol leftTolerance rightTolerance ->
      compositeToleranceBranches leftTolerance <> compositeToleranceBranches rightTolerance
    branch ->
      [branch]

disjunctiveToleranceBranches :: Tolerance -> [Tolerance]
disjunctiveToleranceBranches toleranceValue =
  case toleranceValue of
    DisjunctiveTol leftTolerance rightTolerance ->
      disjunctiveToleranceBranches leftTolerance <> disjunctiveToleranceBranches rightTolerance
    branch ->
      [branch]

canonicalToleranceBranches :: [Tolerance] -> [Tolerance]
canonicalToleranceBranches =
  Set.toAscList . Set.fromList

rebuildCompositeTolerance :: [Tolerance] -> Tolerance
rebuildCompositeTolerance branches =
  case branches of
    [] -> Exact
    firstBranch : remainingBranches ->
      foldl' CompositeTol firstBranch remainingBranches

rebuildDisjunctiveTolerance :: [Tolerance] -> Tolerance
rebuildDisjunctiveTolerance branches =
  case branches of
    [] -> Exact
    firstBranch : remainingBranches ->
      foldl' DisjunctiveTol firstBranch remainingBranches

withinToleranceBy ::
  Eq a =>
  (a -> a -> Double) ->
  (a -> a -> Double) ->
  (UlpTol -> a -> a -> Bool) ->
  Tolerance ->
  a ->
  a ->
  Bool
withinToleranceBy distance scale withinUlp toleranceValue leftValue rightValue =
  case toleranceValue of
    Exact ->
      leftValue == rightValue
    AbsTolBound absoluteTolerance ->
      distance leftValue rightValue <= absTolValue absoluteTolerance
    RelTolBound relativeTolerance ->
      distance leftValue rightValue <= relTolValue relativeTolerance * scale leftValue rightValue
    UlpTolBound ulpTolerance ->
      withinUlp ulpTolerance leftValue rightValue
    CompositeTol leftTolerance rightTolerance ->
      withinToleranceBy distance scale withinUlp leftTolerance leftValue rightValue
        && withinToleranceBy distance scale withinUlp rightTolerance leftValue rightValue
    DisjunctiveTol leftTolerance rightTolerance ->
      withinToleranceBy distance scale withinUlp leftTolerance leftValue rightValue
        || withinToleranceBy distance scale withinUlp rightTolerance leftValue rightValue

type ApproxEq :: Type -> Type -> Constraint
-- | Approximate equality under a tolerance value.
--
-- Each instance has a /valid comparison domain/: the subset of the carrier
-- on which the laws below are required. For IEEE floating-point carriers
-- ('Double', 'Float') the valid comparison domain is the non-NaN values;
-- comparisons involving NaN are outside the domain and must return 'False'.
-- For exact carriers the domain is the whole type.
--
-- Laws over the instance's valid comparison domain:
--
-- [Reflexivity] @approxEq tol x x@
--
-- [Symmetry] @approxEq tol x y = approxEq tol y x@
--
-- [Tolerance monotonicity] for tolerance families with a widening order, widening a tolerance must not turn a successful comparison into a failure.
class ApproxEq tol a where
  -- | Compare two values under the supplied tolerance.
  approxEq :: tol -> a -> a -> Bool

withinTol :: (ApproxEq tol a) => tol -> a -> a -> Bool
withinTol = approxEq


instance ApproxEq AbsTol Double where
  approxEq tolerance x y
    | x == y = True
    | isNaN x || isNaN y = False
    | isInfinite x || isInfinite y = False
    | otherwise = abs (x - y) <= absTolValue tolerance

instance ApproxEq RelTol Double where
  approxEq tolerance x y
    | x == y = True
    | isNaN x || isNaN y = False
    | isInfinite x || isInfinite y = False
    | otherwise =
        let d = abs (x - y)
            s = max (abs x) (abs y)
         in d <= relTolValue tolerance * s

instance ApproxEq UlpTol Double where
  approxEq tolerance x y
    | x == y = True
    | isNaN x || isNaN y = False
    | otherwise = ulpDistance x y <= ulpTolValue tolerance

instance ApproxEq AbsTol Float where
  approxEq tolerance x y
    | x == y = True
    | isNaN x || isNaN y = False
    | isInfinite x || isInfinite y = False
    | otherwise = abs (realToFrac x - realToFrac y) <= absTolValue tolerance

instance ApproxEq RelTol Float where
  approxEq tolerance x y
    | x == y = True
    | isNaN x || isNaN y = False
    | isInfinite x || isInfinite y = False
    | otherwise =
        let d = abs (realToFrac x - realToFrac y) :: Double
            s = max (abs (realToFrac x)) (abs (realToFrac y)) :: Double
         in d <= relTolValue tolerance * s

instance ApproxEq UlpTol Float where
  approxEq tolerance x y
    | x == y = True
    | isNaN x || isNaN y = False
    | otherwise = ulpDistanceFloat x y <= ulpTolValue tolerance
