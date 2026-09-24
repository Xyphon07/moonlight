-- | Exact rational certificates about located curve pieces, taking no square
-- root. Every witness returned is checked by the inequality that defines it,
-- so a returned witness is a proof; 'Nothing' means only that the stated
-- finite candidate search found none, which is complete where said so.
--
-- A piece of any of the four shapes lies in the convex hull of its controls,
-- and its tangent lies in the cone of its nonzero control-polygon edges: a
-- polynomial derivative is a positive Bernstein combination of those edges,
-- and a positive-weight rational quadratic's is a positive combination of
-- @P1 - P0@, @P2 - P0@ and @P2 - P1@, where @P2 - P0@ is the sum of the other
-- two.
--
-- Completeness of 'halfPlaneWitness'. When the nonzero inputs lie in an open
-- half-plane, the two extreme rays of their cone are inputs @a@ and @b@, and
-- @perpendicular (a - b)@, the normal of the segment between their tips, is
-- equally positive at both tips and so, being linear, on the whole cone. When
-- every input is parallel, one of them is itself a witness.
--
-- Exactness of 'crossingVerdict'. When each piece's edge cone meets the
-- other's and its negation only at zero, the chord between two common points
-- would be a nonzero vector in both, so the pieces, and every straight-line
-- homotopy of them to their chords, meet at most once and transversally.
-- With each endpoint strictly outside the other's control hull, which holds
-- that piece and its chord, no endpoint meets the other along the homotopy,
-- so the parity of the meeting count, and with at most one meeting the count
-- itself, is the chords'.
module Moonlight.Planar.Internal.CurveCertificate
  ( halfPlaneWitness
  , separatingAxis
  , hullGap
  , stationaryPiece
  , monotoneWitness
  , jointWitness
  , hullSeparation
  , CrossingCertificate
  , CrossingVerdict (..)
  , crossingVerdict
  ) where

import Control.DeepSeq (NFData (..))
import Data.Foldable (find, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Moonlight.Planar.Curve (CurveStep, curveStepEnd, stepControlPoints)
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), exactOrient2d, exactPointCoordinates
  , exactRationalBitWidth, exactVectorFromPoints, translateExactPoint )
import Moonlight.Planar.Internal.CurveBudget (BudgetObligation (..), SubdivisionBudget, budgetBits)
import Moonlight.Planar.Internal.ExactRational (divideByPositive, positiveExact)

-- | A direction strictly positive against every nonzero vector, when the
-- nonzero vectors lie in an open half-plane through the origin. Zero vectors
-- constrain nothing and are dropped; with none left no direction is
-- witnessed.
halfPlaneWitness :: [ExactVector] -> Maybe ExactVector
halfPlaneWitness vectors = find positiveOnAll candidates
 where
  nonzero = filter (/= ExactVector 0 0) vectors
  candidates = nonzero <> [perpendicular (subtractVector a b) | a <- nonzero, b <- nonzero, a /= b]
  positiveOnAll direction = not (null nonzero) && all ((> 0) . dot direction) nonzero

-- | A direction along which every point of the first set lies strictly below
-- every point of the second, so their convex hulls are disjoint. The
-- candidates are complete for possibly degenerate hulls: the closest pair of
-- two disjoint hulls is vertex to vertex, separated along their difference,
-- or vertex to edge, separated along the edge's normal.
separatingAxis :: NonEmpty ExactPoint -> NonEmpty ExactPoint -> Maybe ExactVector
separatingAxis lower upper = find ((> 0) . axisGap lower upper) (axisCandidates lower upper)

-- | The displacement between two point sets' convex hulls, whose square is
-- exactly their squared distance: the zero vector when the hulls meet.
--
-- It is the longest of @(g / |a|^2) a@ over the candidate axes @a@ of
-- 'separatingAxis' with positive gap @g@, how far the second set's least
-- projection on @a@ exceeds the first's greatest; its length is @g / |a|@.
-- Every such length is at most the distance, since projection onto a unit
-- axis lengthens no displacement between the hulls. And one attains it: some
-- closest pair @p@, @q@ of disjoint hulls is vertex to vertex or vertex to a
-- point inside an edge, so @q - p@ is a between-set difference or a normal of
-- a within-set difference, a candidate with either sign; the lines through
-- @p@ and @q@ normal to @q - p@ support the two hulls, so along it @g / |a|@
-- is @|q - p|@. When the hulls meet no candidate has a positive gap. The
-- square is rational; the distance itself is not taken.
hullGap :: NonEmpty ExactPoint -> NonEmpty ExactPoint -> ExactVector
hullGap lower upper = foldr longer (ExactVector 0 0) (mapMaybe along (axisCandidates lower upper))
 where
  along axis = case (axisGap lower upper axis, positiveExact (dot axis axis)) of
    (gap, Right norm) | gap > 0 -> Just (scaleVector (divideByPositive gap norm) axis)
    _ -> Nothing
  longer candidate best
    | dot candidate candidate > dot best best = candidate
    | otherwise = best

-- The differences within each set, turned a quarter, and the differences
-- between the sets, each with both signs.
axisCandidates :: NonEmpty ExactPoint -> NonEmpty ExactPoint -> [ExactVector]
axisCandidates lower upper = concatMap (\axis -> [axis, negateVector axis]) axes
 where
  lows = toList lower
  highs = toList upper
  differences points = [exactVectorFromPoints p q | p <- points, q <- points, p /= q]
  axes = map perpendicular (differences lows <> differences highs)
    <> [exactVectorFromPoints p q | p <- lows, q <- highs, p /= q]

-- How far the second set's least projection on the axis exceeds the first's
-- greatest.
axisGap :: NonEmpty ExactPoint -> NonEmpty ExactPoint -> ExactVector -> ExactRational
axisGap lower upper axis = minimum1 (project axis <$> upper) - maximum1 (project axis <$> lower)
 where
  minimum1, maximum1 :: NonEmpty ExactRational -> ExactRational
  minimum1 (x :| xs) = foldr min x xs
  maximum1 (x :| xs) = foldr max x xs

-- | Whether every control of the piece is its start, so it never moves.
stationaryPiece :: CurveStep -> Bool
stationaryPiece = all (== ExactVector 0 0) . controlEdges

-- | A direction along which the piece strictly advances, so the piece is
-- injective and so is its straight-line homotopy to its chord. A stationary
-- piece has no witness.
monotoneWitness :: CurveStep -> Maybe ExactVector
monotoneWitness = halfPlaneWitness . controlEdges

-- | A direction separating two consecutive pieces at their joint @V@: the
-- incoming piece's controls lie strictly ahead of @V@ along it, apart from
-- @V@ itself, and the outgoing piece's strictly behind, so the two meet only
-- at @V@. A cusp, whose tangent reverses, has none. Stationary pieces are
-- contracted by the caller before their joints are tested.
jointWitness :: CurveStep -> CurveStep -> Maybe ExactVector
jointWitness incoming outgoing =
  halfPlaneWitness (fromJoint <> map negateVector (toList (stepControlPoints outgoing)))
 where
  fromJoint = map (`subtractVector` curveStepEnd incoming) (toList (stepControlPoints incoming))

-- | A direction strictly separating two located pieces' control hulls, so the
-- pieces are disjoint.
hullSeparation :: ExactPoint -> CurveStep -> ExactPoint -> CurveStep -> Maybe ExactVector
hullSeparation startA stepA startB stepB = separatingAxis (controls startA stepA) (controls startB stepB)

-- | Why two pieces meet at most once and transversally, and why their chords
-- decide whether they do: a direction positive on both pieces' edges, one
-- positive on the first's and negative on the second's, and an axis
-- separating each endpoint from the other piece's control hull. It carries no
-- crossing point, which is algebraic in general.
data CrossingCertificate = CrossingCertificate
  !ExactVector !ExactVector !ExactVector !ExactVector !ExactVector !ExactVector
  deriving stock (Eq, Show)

-- The widest coordinate a certificate carries.
certificateBits :: CrossingCertificate -> Int
certificateBits (CrossingCertificate same opposite a0 a1 b0 b1) =
  maximum1 (vectorWidth <$> same :| [opposite, a0, a1, b0, b1])
 where
  vectorWidth (ExactVector x y) = max (exactRationalBitWidth x) (exactRationalBitWidth y)
  maximum1 :: NonEmpty Int -> Int
  maximum1 (x :| xs) = foldr max x xs

instance NFData CrossingCertificate where
  rnf (CrossingCertificate same opposite a0 a1 b0 b1) =
    rnf same `seq` rnf opposite `seq` rnf a0 `seq` rnf a1 `seq` rnf b0 `seq` rnf b1

-- | The pieces cross exactly once, transversally, or not at all.
data CrossingVerdict
  = SingleCrossing !CrossingCertificate
  | NoCrossing !CrossingCertificate
  deriving stock (Eq, Show)

instance NFData CrossingVerdict where
  rnf (SingleCrossing certificate) = rnf certificate
  rnf (NoCrossing certificate) = rnf certificate

verdictCertificate :: CrossingVerdict -> CrossingCertificate
verdictCertificate (SingleCrossing certificate) = certificate
verdictCertificate (NoCrossing certificate) = certificate

-- | The exact crossing verdict for two located pieces, when their cones and
-- endpoints admit one. The chords cross exactly when each chord's endpoints
-- lie strictly on opposite sides of the other's line; no endpoint can lie on
-- the other chord, which is inside that piece's control hull. A verdict is
-- returned only with its certificate admitted under the budget's bits, so
-- every caller that retains one retains an admitted one; a wider certificate
-- refuses with its width.
crossingVerdict
  :: SubdivisionBudget -> ExactPoint -> CurveStep -> ExactPoint -> CurveStep
  -> Either BudgetObligation (Maybe CrossingVerdict)
crossingVerdict budget startA stepA startB stepB =
  traverse admitted
    ( verdict
        <$> halfPlaneWitness (edgesA <> edgesB)
        <*> halfPlaneWitness (edgesA <> map negateVector edgesB)
        <*> outside a0 hullB <*> outside a1 hullB <*> outside b0 hullA <*> outside b1 hullA )
 where
  admitted found
    | width > budgetBits budget = Left (BitsExhausted width)
    | otherwise = Right found
   where
    width = certificateBits (verdictCertificate found)
  edgesA = controlEdges stepA
  edgesB = controlEdges stepB
  hullA = controls startA stepA
  hullB = controls startB stepB
  a0 = startA
  a1 = translateExactPoint startA (curveStepEnd stepA)
  b0 = startB
  b1 = translateExactPoint startB (curveStepEnd stepB)
  outside point hull = separatingAxis (point :| []) hull
  chordsCross = opposite (exactOrient2d a0 a1 b0) (exactOrient2d a0 a1 b1)
    && opposite (exactOrient2d b0 b1 a0) (exactOrient2d b0 b1 a1)
  opposite left right = (left, right) `elem` [(LT, GT), (GT, LT)]
  verdict same opposed axisA0 axisA1 axisB0 axisB1
    | chordsCross = SingleCrossing certificate
    | otherwise = NoCrossing certificate
   where
    certificate = CrossingCertificate same opposed axisA0 axisA1 axisB0 axisB1

controls :: ExactPoint -> CurveStep -> NonEmpty ExactPoint
controls start step = translateExactPoint start <$> stepControlPoints step

controlEdges :: CurveStep -> [ExactVector]
controlEdges step = zipWith (flip subtractVector) points (drop 1 points)
 where
  points = toList (stepControlPoints step)

project :: ExactVector -> ExactPoint -> ExactRational
project (ExactVector ax ay) point = let (x, y) = exactPointCoordinates point in ax * x + ay * y

dot :: ExactVector -> ExactVector -> ExactRational
dot (ExactVector ax ay) (ExactVector bx by) = ax * bx + ay * by

perpendicular :: ExactVector -> ExactVector
perpendicular (ExactVector x y) = ExactVector (negate y) x

subtractVector :: ExactVector -> ExactVector -> ExactVector
subtractVector (ExactVector ax ay) (ExactVector bx by) = ExactVector (ax - bx) (ay - by)

scaleVector :: ExactRational -> ExactVector -> ExactVector
scaleVector factor (ExactVector x y) = ExactVector (factor * x) (factor * y)

negateVector :: ExactVector -> ExactVector
negateVector (ExactVector x y) = ExactVector (negate x) (negate y)
