{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Restricted exact observations of fixed-position, linearly reweighted power
-- cells. Preparation descends through the existing all-constraint clipper;
-- active source lines, strict omitted slacks and directed edge lengths glue
-- those local cells into one sufficient open parameter interval. This is not
-- an event-transition engine or a replacement for regular-site dispositions.
module Moonlight.Planar.Internal.PowerDiagram.Mass
  ( PreparedPowerMassSection
  , PowerMassPolynomial (..)
  , PowerMassBoundary (..)
  , PowerMassObligation (..)
  , PowerMassCertificate
  , powerMassCertificateObligation
  , powerMassCertificateConstant
  , powerMassCertificateRate
  , PowerMassBound
  , powerMassBoundParameter
  , powerMassBoundCertificates
  , PowerMassInterval
  , powerMassIntervalLower
  , powerMassIntervalUpper
  , PowerMassError (..)
  , preparePowerMassSection
  , powerMassSectionWindow
  , powerMassSectionSites
  , powerMassSectionDirection
  , powerMassSectionInterval
  , powerMassSectionCertificates
  , powerMassSectionPolynomials
  , evaluatePowerMasses
  ) where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Moonlight.Planar.Convex (ConvexPolygon, convexPolygonPoints, retainConvexPolygon)
import Moonlight.Planar.Exact
  ( ExactAffineLine
  , ExactArithmeticError
  , ExactClipDisposition (..)
  , ExactClipError
  , ExactClosedHalfPlane
  , ExactHalfPlaneError
  , ExactPoint
  , ExactRational
  , ExactRetainedPolygon
  , ExactVector (..)
  , exactAffineLineCoefficients
  , exactClipRetainedPolygon
  , exactClosedHalfPlane
  , exactClosedHalfPlaneFromDirectedEdge
  , exactClosedHalfPlaneLine
  , exactCross
  , exactDivide
  , exactPointCoordinates
  , exactPointCross
  , exactRetainedPolygonPoints
  , exactVectorFromPoints
  )
import Moonlight.Planar.Internal.BoundaryCycle (cyclePairsNonEmpty, unorderedPairs)
import Moonlight.Planar.Internal.ExactRational (exactRationalFromNormalizedRatio)
import Moonlight.Planar.Internal.PowerDiagram.Generator
  ( ExactPowerGenerator
  , exactGeneratorAxis
  , exactPowerGeneratorLabel
  )
import Moonlight.Planar.Internal.PowerDiagram.Model
  ( PowerDiagramError
  , PowerSite
  , powerSiteExactPosition
  , powerSiteLabel
  )
import Moonlight.Planar.Internal.PowerDiagram.Section
  ( prepareExactPowerGenerator
  , validateAndSortSites
  )

-- | Coefficients of @constant + parameter * linear + parameter^2 * quadratic@.
-- The constructor is an observation, not an admission path to a section.
data PowerMassPolynomial = PowerMassPolynomial !ExactRational !ExactRational !ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup PowerMassPolynomial where
  PowerMassPolynomial a b c <> PowerMassPolynomial d e f =
    PowerMassPolynomial (a + d) (b + e) (c + f)

instance Monoid PowerMassPolynomial where
  mempty = PowerMassPolynomial 0 0 0

-- | Stable source identity within one cell's complete inequality family.
data PowerMassBoundary label
  = PowerMassWindowBoundary !Int
  | PowerMassSiteBoundary !label
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data PowerMassObligation label
  = PowerMassVertexSlack !label !ExactPoint !(PowerMassBoundary label)
  | PowerMassDirectedEdge !label !(PowerMassBoundary label)
  | PowerMassEmptyWitness !label !(NonEmpty (PowerMassBoundary label, ExactRational))
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A retained strict affine inequality @constant + t * rate > 0@.
data PowerMassCertificate label =
  PowerMassCertificate !(PowerMassObligation label) !ExactRational !ExactRational
  deriving stock (Eq, Ord, Show)

instance NFData label => NFData (PowerMassCertificate label) where
  rnf (PowerMassCertificate obligation constant rate) = rnf obligation `seq` rnf constant `seq` rnf rate

powerMassCertificateObligation :: PowerMassCertificate label -> PowerMassObligation label
powerMassCertificateObligation (PowerMassCertificate obligation _ _) = obligation

powerMassCertificateConstant :: PowerMassCertificate label -> ExactRational
powerMassCertificateConstant (PowerMassCertificate _ constant _) = constant

powerMassCertificateRate :: PowerMassCertificate label -> ExactRational
powerMassCertificateRate (PowerMassCertificate _ _ rate) = rate

-- | Every retained certificate tied at this extremal root, not an arbitrary
-- event chosen from the simultaneous set.
data PowerMassBound label =
  PowerMassBound !ExactRational !(NonEmpty (PowerMassCertificate label))
  deriving stock (Eq, Ord, Show)

instance NFData label => NFData (PowerMassBound label) where
  rnf (PowerMassBound parameter certificates) = rnf parameter `seq` rnf certificates

powerMassBoundParameter :: PowerMassBound label -> ExactRational
powerMassBoundParameter (PowerMassBound parameter _) = parameter

powerMassBoundCertificates :: PowerMassBound label -> NonEmpty (PowerMassCertificate label)
powerMassBoundCertificates (PowerMassBound _ certificates) = certificates

data PowerMassInterval label =
  PowerMassInterval !(Maybe (PowerMassBound label)) !(Maybe (PowerMassBound label))
  deriving stock (Eq, Ord, Show)

instance NFData label => NFData (PowerMassInterval label) where
  rnf (PowerMassInterval lower upper) = rnf lower `seq` rnf upper

powerMassIntervalLower :: PowerMassInterval label -> Maybe (PowerMassBound label)
powerMassIntervalLower (PowerMassInterval lower _) = lower

powerMassIntervalUpper :: PowerMassInterval label -> Maybe (PowerMassBound label)
powerMassIntervalUpper (PowerMassInterval _ upper) = upper

-- No exported constructor or record-update surface can detach a polynomial
-- from its source, direction, active-line witnesses or validity interval.
data PreparedPowerMassSection label = PreparedPowerMassSection
  !ConvexPolygon
  !(NonEmpty (PowerSite label))
  !(Map label ExactRational)
  !(Map label (CertifiedMassCell label))
  ![PowerMassCertificate label]
  !(PowerMassInterval label)
  deriving stock (Eq, Show)

instance NFData label => NFData (PreparedPowerMassSection label) where
  rnf (PreparedPowerMassSection window sites direction cells certificates interval) =
    rnf window `seq` rnf sites `seq` rnf direction `seq` rnf cells `seq` rnf certificates `seq` rnf interval

data MassConstraint label = MassConstraint !(PowerMassBoundary label) !ExactAffineLine !ExactRational
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data AffineMassVertex label = AffineMassVertex
  !ExactPoint !ExactVector !(MassConstraint label) !(MassConstraint label)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data CertifiedMassCell label
  = FullDimensionalMassCell !PowerMassPolynomial !(NonEmpty (AffineMassVertex label))
  | EmptyMassCell !(PowerMassCertificate label)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Restrictions belong only to this prepared observation. Public regular
-- triangulations continue to support coincident and lower-dimensional sites.
data PowerMassError label
  = PowerMassSiteAdmission !(PowerDiagramError label)
  | PowerMassDirectionMismatch !(Set label) !(Set label)
  | PowerMassCoincidentSites !label !label
  | PowerMassWindowAdmission !ExactHalfPlaneError
  | PowerMassAxisAdmission !label !label !ExactHalfPlaneError
  | PowerMassCellClip !label !ExactClipError
  | PowerMassLowerDimensionalSeed !label !(NonEmpty ExactPoint)
  | PowerMassActiveIncidence !label !ExactPoint ![PowerMassBoundary label]
  | PowerMassParallelActiveBoundaries !label !ExactPoint
  | PowerMassEdgeIncidence !label ![PowerMassBoundary label]
  | PowerMassNonStrictSeed !(PowerMassObligation label) !ExactRational
  | PowerMassEmptyWitnessUnavailable !label
  | PowerMassArithmetic !ExactArithmeticError
  | PowerMassParameterOutsideCertificate !ExactRational !(PowerMassInterval label)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

powerMassSectionWindow :: PreparedPowerMassSection label -> ConvexPolygon
powerMassSectionWindow (PreparedPowerMassSection window _ _ _ _ _) = window

powerMassSectionSites :: PreparedPowerMassSection label -> NonEmpty (PowerSite label)
powerMassSectionSites (PreparedPowerMassSection _ sites _ _ _ _) = sites

powerMassSectionDirection :: PreparedPowerMassSection label -> Map label ExactRational
powerMassSectionDirection (PreparedPowerMassSection _ _ direction _ _ _) = direction

powerMassSectionInterval :: PreparedPowerMassSection label -> PowerMassInterval label
powerMassSectionInterval (PreparedPowerMassSection _ _ _ _ _ interval) = interval

powerMassSectionCertificates :: PreparedPowerMassSection label -> [PowerMassCertificate label]
powerMassSectionCertificates (PreparedPowerMassSection _ _ _ _ certificates _) = certificates

powerMassSectionPolynomials :: PreparedPowerMassSection label -> Map label PowerMassPolynomial
powerMassSectionPolynomials (PreparedPowerMassSection _ _ _ cells _ _) = fmap massCellPolynomial cells

massCellPolynomial :: CertifiedMassCell label -> PowerMassPolynomial
massCellPolynomial (FullDimensionalMassCell coefficients _) = coefficients
massCellPolynomial (EmptyMassCell _) = mempty

-- | Prepare exact area observations for fixed positions, rational affine
-- weights and a fixed bounded convex window. Every label needs an explicit
-- rate, including zero. The expensive small-subset empty-witness search runs
-- only here; evaluation performs one exact interval check and O(n) polynomials.
preparePowerMassSection
  :: Ord label
  => ConvexPolygon
  -> NonEmpty (PowerSite label)
  -> Map label ExactRational
  -> Either (PowerMassError label) (PreparedPowerMassSection label)
preparePowerMassSection window submitted direction = do
  sites <- first PowerMassSiteAdmission (validateAndSortSites submitted)
  let labels = Set.fromList (fmap powerSiteLabel (NonEmpty.toList sites))
      directionLabels = Map.keysSet direction
      missing = labels Set.\\ directionLabels
      extra = directionLabels Set.\\ labels
  if Set.null missing && Set.null extra
    then Right ()
    else Left (PowerMassDirectionMismatch missing extra)
  case List.find samePosition (unorderedPairs (NonEmpty.toList sites)) of
    Just (left, right) -> Left (PowerMassCoincidentSites (powerSiteLabel left) (powerSiteLabel right))
    Nothing -> Right ()
  let retained = retainConvexPolygon window
  windowConstraints <- traverse windowConstraint (zip [0 ..] (NonEmpty.toList (cyclePairsNonEmpty (convexPolygonPoints window))))
  let generators = fmap prepareExactPowerGenerator sites
      -- Exact key equality was discharged above; pairing carries each rate
      -- through all construction rather than inventing a missing-rate default.
      trajectories = zip (NonEmpty.toList generators) (Map.elems direction)
  prepared <- traverse (prepareMassCell retained windowConstraints trajectories) trajectories
  let cells = Map.fromList (fmap (\(label, cell, _) -> (label, cell)) prepared)
      certificates = concatMap (\(_, _, obligations) -> obligations) prepared
  interval <- certificateInterval certificates
  pure (PreparedPowerMassSection window sites direction cells certificates interval)
 where
  samePosition :: (PowerSite label, PowerSite label) -> Bool
  samePosition (left, right) = powerSiteExactPosition left == powerSiteExactPosition right
  windowConstraint
    :: (Int, (ExactPoint, ExactPoint))
    -> Either (PowerMassError label) (MassConstraint label)
  windowConstraint (index, (from, to)) = do
    halfPlane <- first PowerMassWindowAdmission (exactClosedHalfPlaneFromDirectedEdge from to)
    pure (MassConstraint (PowerMassWindowBoundary index) (exactClosedHalfPlaneLine halfPlane) 0)

prepareMassCell
  :: Ord label
  => ExactRetainedPolygon
  -> [MassConstraint label]
  -> [(ExactPowerGenerator label, ExactRational)]
  -> (ExactPowerGenerator label, ExactRational)
  -> Either (PowerMassError label) (label, CertifiedMassCell label, [PowerMassCertificate label])
prepareMassCell retained windowConstraints trajectories (owner, ownerRate) = do
  let label = exactPowerGeneratorLabel owner
  siteConstraints <- traverse (siteConstraint label) (filter ((/= label) . exactPowerGeneratorLabel . fst) trajectories)
  let constraints = windowConstraints <> siteConstraints
  (disposition, _) <- first (PowerMassCellClip label)
    (exactClipRetainedPolygon retained (fmap constraintHalfPlane siteConstraints))
  case disposition of
    ExactClipLowerDimensional points -> Left (PowerMassLowerDimensionalSeed label points)
    ExactClipEmpty -> do
      witness <- maybe (Left (PowerMassEmptyWitnessUnavailable label)) Right (emptyWitness label constraints)
      pure (label, EmptyMassCell witness, [witness])
    ExactClipFullDimensional polygon -> do
      vertices <- traverse (prepareMassVertex label constraints) (exactRetainedPolygonPoints polygon)
      vertexCertificates <- traverse (vertexSlacks label constraints) vertices
      edgeCertificates <- traverse (directedEdgeCertificate label) (cyclePairsNonEmpty vertices)
      pure
        ( label
        , FullDimensionalMassCell (areaPolynomial vertices) vertices
        , concat (NonEmpty.toList vertexCertificates) <> NonEmpty.toList edgeCertificates
        )
 where
  siteConstraint label (competitor, competitorRate) = do
    let competitorLabel = exactPowerGeneratorLabel competitor
    axis <- first (PowerMassAxisAdmission label competitorLabel) (exactGeneratorAxis owner competitor)
    pure (MassConstraint (PowerMassSiteBoundary competitorLabel) axis (ownerRate - competitorRate))
  constraintHalfPlane :: MassConstraint label -> ExactClosedHalfPlane
  constraintHalfPlane (MassConstraint _ line _) = exactClosedHalfPlane line

constraintBoundary :: MassConstraint label -> PowerMassBoundary label
constraintBoundary (MassConstraint boundary _ _) = boundary

constraintCoefficients :: MassConstraint label -> (ExactRational, ExactRational, ExactRational, ExactRational)
constraintCoefficients (MassConstraint _ line rate) =
  let (a, b, c) = exactAffineLineCoefficients line in (a, b, c, rate)

constraintAtPoint :: ExactPoint -> MassConstraint label -> ExactRational
constraintAtPoint point constraint =
  let (x, y) = exactPointCoordinates point
      (a, b, c, _) = constraintCoefficients constraint
   in a * x + b * y + c

prepareMassVertex
  :: label
  -> [MassConstraint label]
  -> ExactPoint
  -> Either (PowerMassError label) (AffineMassVertex label)
prepareMassVertex label constraints point =
  case filter ((== 0) . constraintAtPoint point) constraints of
    [left, right] -> do
      let (a, b, _, leftRate) = constraintCoefficients left
          (c, d, _, rightRate) = constraintCoefficients right
          determinant = a * d - b * c
      if determinant == 0
        then Left (PowerMassParallelActiveBoundaries label point)
        else do
          velocityX <- first PowerMassArithmetic (exactDivide (b * rightRate - leftRate * d) determinant)
          velocityY <- first PowerMassArithmetic (exactDivide (leftRate * c - a * rightRate) determinant)
          pure (AffineMassVertex point (ExactVector velocityX velocityY) left right)
    active -> Left (PowerMassActiveIncidence label point (fmap constraintBoundary active))

strictCertificate
  :: PowerMassObligation label
  -> ExactRational
  -> ExactRational
  -> Either (PowerMassError label) (PowerMassCertificate label)
strictCertificate obligation constant rate
  | constant > 0 = Right (PowerMassCertificate obligation constant rate)
  | otherwise = Left (PowerMassNonStrictSeed obligation constant)

vertexSlacks
  :: Eq label
  => label
  -> [MassConstraint label]
  -> AffineMassVertex label
  -> Either (PowerMassError label) [PowerMassCertificate label]
vertexSlacks label constraints (AffineMassVertex point (ExactVector dx dy) left right) =
  traverse certificate (filter inactive constraints)
 where
  inactive constraint = constraintBoundary constraint /= constraintBoundary left
    && constraintBoundary constraint /= constraintBoundary right
  certificate constraint =
    let (a, b, _, rate) = constraintCoefficients constraint
     in strictCertificate
          (PowerMassVertexSlack label point (constraintBoundary constraint))
          (constraintAtPoint point constraint)
          (a * dx + b * dy + rate)

directedEdgeCertificate
  :: Eq label
  => label
  -> (AffineMassVertex label, AffineMassVertex label)
  -> Either (PowerMassError label) (PowerMassCertificate label)
directedEdgeCertificate label
    ( AffineMassVertex from (ExactVector fromDX fromDY) fromLeft fromRight
    , AffineMassVertex to (ExactVector toDX toDY) toLeft toRight
    ) =
  case filter shared [fromLeft, fromRight] of
    [edge] ->
      let (a, b, _, _) = constraintCoefficients edge
          ExactVector edgeX edgeY = exactVectorFromPoints from to
       in strictCertificate
            (PowerMassDirectedEdge label (constraintBoundary edge))
            (b * edgeX - a * edgeY)
            (b * (toDX - fromDX) - a * (toDY - fromDY))
    edges -> Left (PowerMassEdgeIncidence label (fmap constraintBoundary edges))
 where
  shared constraint = constraintBoundary constraint == constraintBoundary toLeft
    || constraintBoundary constraint == constraintBoundary toRight

areaPolynomial :: NonEmpty (AffineMassVertex label) -> PowerMassPolynomial
areaPolynomial = foldMap edgePolynomial . cyclePairsNonEmpty
 where
  half = exactRationalFromNormalizedRatio (1 % 2)
  edgePolynomial :: (AffineMassVertex label, AffineMassVertex label) -> PowerMassPolynomial
  edgePolynomial (AffineMassVertex from fromVelocity _ _, AffineMassVertex to toVelocity _ _) =
    let (fromX, fromY) = exactPointCoordinates from
        (toX, toY) = exactPointCoordinates to
        fromVector = ExactVector fromX fromY
        toVector = ExactVector toX toY
     in PowerMassPolynomial
          (half * exactPointCross from to)
          (half * (exactCross fromVelocity toVector + exactCross fromVector toVelocity))
          (half * exactCross fromVelocity toVelocity)

-- Small-subset Farkas search is deliberately preparation-only. For source
-- equations a*x+b*y+c(t)>=0, cancelling normals and sum lambda*c(t)<0
-- certifies emptiness, including contacts involving only the window.
emptyWitness :: label -> [MassConstraint label] -> Maybe (PowerMassCertificate label)
emptyWitness label constraints =
  Foldable.asum (fmap witness (pairWeights <> tripleWeights))
 where
  pairWeights = fmap oppositeWeights (unorderedPairs constraints)
  tripleWeights =
    [ cyclicWeights left middle right
    | left : remaining <- List.tails constraints
    , (middle, right) <- unorderedPairs remaining
    ]
  oppositeWeights
    :: (MassConstraint label, MassConstraint label)
    -> [(MassConstraint label, ExactRational)]
  oppositeWeights (left, right) =
    let (a, b, _, _) = constraintCoefficients left
        (c, d, _, _) = constraintCoefficients right
        (leftWeight, rightWeight) = if a /= 0 || c /= 0 then (abs c, abs a) else (abs d, abs b)
     in [(left, leftWeight), (right, rightWeight)]
  cyclicWeights
    :: MassConstraint label
    -> MassConstraint label
    -> MassConstraint label
    -> [(MassConstraint label, ExactRational)]
  cyclicWeights left middle right =
    let crossNormals :: MassConstraint label -> MassConstraint label -> ExactRational
        crossNormals firstConstraint secondConstraint =
          let (a, b, _, _) = constraintCoefficients firstConstraint
              (c, d, _, _) = constraintCoefficients secondConstraint
           in a * d - b * c
        weighted = [(left, crossNormals middle right), (middle, crossNormals right left), (right, crossNormals left middle)]
     in if all ((<= 0) . snd) weighted then fmap (fmap negate) weighted else weighted
  witness weighted =
    let positive = filter ((> 0) . snd) weighted
        normalX = sum (fmap (\(constraint, weight) -> let (a, _, _, _) = constraintCoefficients constraint in weight * a) weighted)
        normalY = sum (fmap (\(constraint, weight) -> let (_, b, _, _) = constraintCoefficients constraint in weight * b) weighted)
        constant = sum (fmap (\(constraint, weight) -> let (_, _, c, _) = constraintCoefficients constraint in weight * c) weighted)
        rate = sum (fmap (\(constraint, weight) -> let (_, _, _, d) = constraintCoefficients constraint in weight * d) weighted)
     in if all ((>= 0) . snd) weighted && normalX == 0 && normalY == 0 && constant < 0
          then fmap
            (\sources -> PowerMassCertificate (PowerMassEmptyWitness label sources) (negate constant) (negate rate))
            (NonEmpty.nonEmpty (fmap (\(constraint, weight) -> (constraintBoundary constraint, weight)) positive))
          else Nothing

certificateInterval
  :: [PowerMassCertificate label]
  -> Either (PowerMassError label) (PowerMassInterval label)
certificateInterval certificates = do
  bounds <- traverse root certificates
  pure (Foldable.foldl' intersectInterval (PowerMassInterval Nothing Nothing) (mapMaybe id bounds))
 where
  root
    :: PowerMassCertificate label
    -> Either (PowerMassError label) (Maybe (PowerMassInterval label))
  root certificate@(PowerMassCertificate _ constant rate)
    | rate == 0 = Right Nothing
    | otherwise = do
        parameter <- first PowerMassArithmetic (exactDivide (negate constant) rate)
        let bound = Just (PowerMassBound parameter (certificate :| []))
        pure (Just (if rate > 0 then PowerMassInterval bound Nothing else PowerMassInterval Nothing bound))
  intersectInterval :: PowerMassInterval label -> PowerMassInterval label -> PowerMassInterval label
  intersectInterval (PowerMassInterval lower upper) (PowerMassInterval otherLower otherUpper) =
    PowerMassInterval (tightest GT lower otherLower) (tightest LT upper otherUpper)
  tightest
    :: Ordering
    -> Maybe (PowerMassBound label)
    -> Maybe (PowerMassBound label)
    -> Maybe (PowerMassBound label)
  tightest _ Nothing right = right
  tightest _ left Nothing = left
  tightest direction left@(Just (PowerMassBound leftRoot leftCertificates)) right@(Just (PowerMassBound rightRoot rightCertificates)) =
    case compare leftRoot rightRoot of
      EQ -> Just (PowerMassBound leftRoot (leftCertificates <> rightCertificates))
      ordering -> if ordering == direction then left else right

-- | Only exact parameters strictly inside the retained sufficient interval
-- are admitted. Endpoint expiry requests recertification; it does not claim a
-- visibility event. No floating epsilon or implicit binary64 conversion exists.
evaluatePowerMasses
  :: ExactRational
  -> PreparedPowerMassSection label
  -> Either (PowerMassError label) (Map label ExactRational)
evaluatePowerMasses parameter section@(PreparedPowerMassSection _ _ _ cells _ _) =
  let interval = powerMassSectionInterval section
      aboveLower = maybe True ((parameter >) . powerMassBoundParameter) (powerMassIntervalLower interval)
      belowUpper = maybe True ((parameter <) . powerMassBoundParameter) (powerMassIntervalUpper interval)
      evaluatePolynomial (PowerMassPolynomial constant linear quadratic) =
        constant + parameter * (linear + parameter * quadratic)
   in if aboveLower && belowUpper
        then Right (fmap (evaluatePolynomial . massCellPolynomial) cells)
        else Left (PowerMassParameterOutsideCertificate parameter interval)
