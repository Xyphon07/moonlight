-- | Exact certificates and reconstruction laws through the public power owner.
module Moonlight.Planar.PowerMassSpec (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Planar.Convex (ConvexPolygon, convexPolygon, convexPolygonPoints)
import Moonlight.Planar.Exact
  ( ExactPoint
  , ExactRational
  , exactDivide
  , exactPointCross
  , exactRational
  )
import Moonlight.Planar.PowerDiagram
  ( PowerCellDisposition (..)
  , PowerMassBoundary (..)
  , PowerMassCertificate
  , PowerMassError (..)
  , PowerMassObligation (..)
  , PowerMassPolynomial (..)
  , PowerSite
  , PreparedPowerMassSection
  , boundedPowerDiagram
  , evaluatePowerMasses
  , powerCellDispositions
  , powerMassBoundCertificates
  , powerMassBoundParameter
  , powerMassCertificateConstant
  , powerMassCertificateObligation
  , powerMassCertificateRate
  , powerMassIntervalLower
  , powerMassIntervalUpper
  , powerMassSectionCertificates
  , powerMassSectionDirection
  , powerMassSectionInterval
  , powerMassSectionPolynomials
  , powerMassSectionSites
  , powerMassSectionWindow
  , powerSite
  , powerSiteLabel
  , powerSitePosition
  , powerSiteWeight
  , powerWeightExact
  , powerWeightFromExact
  , preparePowerMassSection
  )
import Moonlight.Planar.Point (Point (..))
import Support (assertEqual, integerPoint, requireRight)

tests :: IO ()
tests = do
  _ <- sequenceA
    [ testThreeSiteCoefficients
    , testWindowContact
    , testUniformShifts
    , testRestrictions
    , testThreeConstraintEmptyWitness
    , testDifferentialFamilies
    ]
  putStrLn "certified power masses: ok"

testThreeSiteCoefficients :: IO ()
testThreeSiteCoefficients = do
  section <- threeSiteSection
  firstCoefficients <- polynomial (9, 2) (15, 16) (3, 64)
  secondCoefficients <- polynomial (9, 2) (-7, 16) (-1, 64)
  thirdCoefficients <- polynomial (7, 1) (-1, 2) (-1, 32)
  let coefficients = powerMassSectionPolynomials section
      interval = powerMassSectionInterval section
  assertEqual "three-site exact coefficients"
    (Map.fromList [(0, firstCoefficients), (1, secondCoefficients), (2, thirdCoefficients)]) coefficients
  assertEqual "all mass coefficients conserve the window" (PowerMassPolynomial 16 0 0) (foldMap id coefficients)
  assertEqual "three-site lower endpoint" (Just (-8)) (powerMassBoundParameter <$> powerMassIntervalLower interval)
  assertEqual "three-site upper endpoint" (Just 5) (powerMassBoundParameter <$> powerMassIntervalUpper interval)
  assertEqual "all lower simultaneous roots retained"
    (Set.fromList (filter (vanishesAt (-8)) (powerMassSectionCertificates section)))
    (maybe Set.empty (Set.fromList . NonEmpty.toList . powerMassBoundCertificates) (powerMassIntervalLower interval))
  assertEqual "all upper simultaneous roots retained"
    (Set.fromList (filter (vanishesAt 5) (powerMassSectionCertificates section)))
    (maybe Set.empty (Set.fromList . NonEmpty.toList . powerMassBoundCertificates) (powerMassIntervalUpper interval))
  _ <- traverse (compareReconstruction section) [-7, -1, 0, 1, 4]
  _ <- traverse (assertExpired section) [-9, -8, 5, 6]
  pure ()
 where
  vanishesAt :: ExactRational -> PowerMassCertificate Int -> Bool
  vanishesAt parameter certificate =
    powerMassCertificateConstant certificate + parameter * powerMassCertificateRate certificate == 0

testWindowContact :: IO ()
testWindowContact = do
  window <- rectangle (-1) (-1) 1 1
  sites <- traverse admittedSite ((0, Point 0 0, 0) :| [(1, Point 2 0, -4)])
  section <- requireRight "window-contact initial section"
    (preparePowerMassSection window sites (Map.fromList [(0, 0), (1, 4)]))
  assertEqual "window-hidden cell has zero coefficients" (Just mempty) (Map.lookup 1 (powerMassSectionPolynomials section))
  assertEqual "window contact expires at one" (Just 1)
    (powerMassBoundParameter <$> powerMassIntervalUpper (powerMassSectionInterval section))
  assertEqual "empty cell retains window support" True
    (any windowWitness (powerMassSectionCertificates section))
  assertExpired section 1
  threeHalves <- rational 3 2
  assertExpired section threeHalves
  shifted <- reweightedSites section threeHalves
  nextSection <- requireRight "recertify reconstructed section after contact"
    (preparePowerMassSection window shifted (powerMassSectionDirection section))
  masses <- requireRight "post-contact exact masses" (evaluatePowerMasses 0 nextSection)
  assertEqual "window entry occurs without a two-site regular flip" (Map.fromList [(0, 3), (1, 1)]) masses
  compareReconstruction nextSection 0
 where
  windowWitness :: PowerMassCertificate Int -> Bool
  windowWitness certificate = case powerMassCertificateObligation certificate of
    PowerMassEmptyWitness 1 sources -> any isWindow (fmap fst sources)
    _ -> False
  isWindow :: PowerMassBoundary Int -> Bool
  isWindow (PowerMassWindowBoundary _) = True
  isWindow (PowerMassSiteBoundary _) = False

testUniformShifts :: IO ()
testUniformShifts = do
  section <- threeSiteSection
  shifted <- traverse (shiftSiteWeight 23) (powerMassSectionSites section)
  weightShifted <- requireRight "uniform source-weight shift"
    (preparePowerMassSection (powerMassSectionWindow section) shifted (powerMassSectionDirection section))
  rateShifted <- requireRight "uniform weight-direction shift"
    (preparePowerMassSection (powerMassSectionWindow section) (powerMassSectionSites section) (fmap (+ 7) (powerMassSectionDirection section)))
  _ <- traverse
    (\candidate -> do
      assertEqual "uniform shift preserves coefficients" (powerMassSectionPolynomials section) (powerMassSectionPolynomials candidate)
      assertEqual "uniform shift preserves interval and certificates" (powerMassSectionInterval section) (powerMassSectionInterval candidate)
      compareReconstruction candidate 4)
    [weightShifted, rateShifted]
  constant <- requireRight "constant masses under common weight direction"
    (preparePowerMassSection (powerMassSectionWindow section) (powerMassSectionSites section) (Map.fromList [(0, 9), (1, 9), (2, 9)]))
  assertEqual "common weight direction has no lower bound" Nothing (powerMassIntervalLower (powerMassSectionInterval constant))
  assertEqual "common weight direction has no upper bound" Nothing (powerMassIntervalUpper (powerMassSectionInterval constant))
  compareReconstruction constant 1000
 where
  shiftSiteWeight :: ExactRational -> PowerSite Int -> IO (PowerSite Int)
  shiftSiteWeight shift site = requireRight "uniformly shifted admitted site"
    (powerSite (powerSiteLabel site) (powerSitePosition site) (powerWeightFromExact (powerWeightExact (powerSiteWeight site) + shift)))

testRestrictions :: IO ()
testRestrictions = do
  window <- rectangle (-2) (-2) 2 2
  coincident <- traverse admittedSite ((0, Point 0 0, 0) :| [(1, Point 0 0, 0)])
  assertEqual "coincident restriction belongs to observation" True
    (case preparePowerMassSection window coincident (Map.fromList [(0, 0), (1, 0)]) of
      Left (PowerMassCoincidentSites 0 1) -> True
      _ -> False)
  lower <- traverse admittedSite ((0, Point 0 0, 0) :| [(1, Point (-1) 0, 1), (2, Point 1 0, 1)])
  assertEqual "lower-dimensional seed is explicit" True
    (case preparePowerMassSection window lower (Map.fromList [(0, 0), (1, 0), (2, 0)]) of
      Left (PowerMassLowerDimensionalSeed 0 _) -> True
      _ -> False)
  highIncidence <- traverse admittedSite
    ((0, Point (-1) (-1), 0) :| [(1, Point 1 (-1), 0), (2, Point 1 1, 0), (3, Point (-1) 1, 0)])
  assertEqual "fourfold power vertex refuses silent perturbation" True
    (case preparePowerMassSection window highIncidence (Map.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]) of
      Left (PowerMassActiveIncidence _ _ sources) -> length sources > 2
      _ -> False)
  section <- threeSiteSection
  assertEqual "direction must have exactly the source label section" True
    (case preparePowerMassSection window (powerMassSectionSites section) (Map.fromList [(0, 1), (1, 0), (7, 0)]) of
      Left (PowerMassDirectionMismatch missing extra) -> missing == Set.singleton 2 && extra == Set.singleton 7
      _ -> False)

testThreeConstraintEmptyWitness :: IO ()
testThreeConstraintEmptyWitness = do
  window <- rectangle (-10) (-10) 10 10
  sites <- traverse admittedSite
    ((0, Point 0 0, -10) :| [(1, Point (-1) (-1), 0), (2, Point 1 (-1), 0), (3, Point 0 1, 0)])
  section <- requireRight "strict three-constraint empty witness"
    (preparePowerMassSection window sites (Map.fromList [(0, 1), (1, 0), (2, 0), (3, 0)]))
  assertEqual "empty witness needs all three site constraints" True
    (any threeSites (powerMassSectionCertificates section))
  compareReconstruction section 0
 where
  threeSites :: PowerMassCertificate Int -> Bool
  threeSites certificate = case powerMassCertificateObligation certificate of
    PowerMassEmptyWitness 0 sources ->
      Set.fromList (fmap fst (NonEmpty.toList sources)) == Set.fromList (fmap PowerMassSiteBoundary [1, 2, 3])
    _ -> False

-- Deterministic rational families, not sampled floating certificates. Every
-- admitted family is reconstructed at exact interior values on both sides of
-- zero. Restricted seed refusals remain visible rather than coerced to cells.
testDifferentialFamilies :: IO ()
testDifferentialFamilies = do
  window <- rectangle (-30) (-30) 30 30
  admitted <- traverse (checkFamily window) [1 .. 48 :: Int]
  assertEqual "differential corpus admits at least forty families" True (sum admitted >= 40)
  putStrLn
    ( "certified power masses differential: " <> show (sum admitted)
        <> " admitted families, " <> show (length admitted - sum admitted)
        <> " restricted seed refusals, " <> show (18 * sum admitted)
        <> " exact per-cell mass comparisons"
    )
 where
  checkFamily window seed = do
    sites <- traverse (familySite seed) (0 :| [1 .. 5])
    rates <- traverse (\index -> do
      rate <- rational (fromIntegral ((seed * 7 + index * 11) `mod` 13 - 6)) 5
      pure (index, rate)) [0 .. 5]
    case preparePowerMassSection window sites (Map.fromList rates) of
      Right section -> do
        negative <- maybe (pure (-1)) (\bound -> requireRight "interior lower half" (exactDivide (powerMassBoundParameter bound) 2))
          (powerMassIntervalLower (powerMassSectionInterval section))
        positive <- maybe (pure 1) (\bound -> requireRight "interior upper half" (exactDivide (powerMassBoundParameter bound) 2))
          (powerMassIntervalUpper (powerMassSectionInterval section))
        _ <- traverse (compareReconstruction section) [negative, 0, positive]
        assertEqual "differential coefficient conservation" (PowerMassPolynomial 3600 0 0)
          (foldMap id (powerMassSectionPolynomials section))
        pure (1 :: Int)
      Left (PowerMassActiveIncidence _ _ _) -> pure 0
      Left (PowerMassLowerDimensionalSeed _ _) -> pure 0
      Left obstruction -> fail ("differential preparation obstruction: " <> show obstruction)
  familySite seed index = do
    weight <- rational (fromIntegral ((seed * 17 + index * 19) `mod` 31 - 15)) 7
    admittedSite
      ( index
      , Point (fromIntegral (3 * index - 7)) (fromIntegral ((seed * 5 + index * index * 3) `mod` 19 - 9))
      , weight
      )

threeSiteSection :: IO (PreparedPowerMassSection Int)
threeSiteSection = do
  window <- rectangle (-2) (-2) 2 2
  sites <- traverse admittedSite
    ((0, Point (-1) (-1), 0) :| [(1, Point 1 (-1), 0), (2, Point 0 1, 0)])
  requireRight "three-site prepared section" (preparePowerMassSection window sites (Map.fromList [(0, 1), (1, 0), (2, 0)]))

rectangle :: Integer -> Integer -> Integer -> Integer -> IO ConvexPolygon
rectangle minimumX minimumY maximumX maximumY = requireRight "convex mass window"
  (convexPolygon
    (integerPoint minimumX minimumY :|
      [integerPoint maximumX minimumY, integerPoint maximumX maximumY, integerPoint minimumX maximumY]))

admittedSite :: (Int, Point, ExactRational) -> IO (PowerSite Int)
admittedSite (label, position, weight) = requireRight "admitted mass site"
  (powerSite label position (powerWeightFromExact weight))

rational :: Integer -> Integer -> IO ExactRational
rational numerator denominator = requireRight "exact test coefficient" (exactRational numerator denominator)

polynomial :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> IO PowerMassPolynomial
polynomial constant linear quadratic =
  PowerMassPolynomial <$> uncurry rational constant <*> uncurry rational linear <*> uncurry rational quadratic

assertExpired :: PreparedPowerMassSection Int -> ExactRational -> IO ()
assertExpired section parameter = assertEqual "expired parameter is refused" True
  (case evaluatePowerMasses parameter section of
    Left (PowerMassParameterOutsideCertificate rejected interval) ->
      rejected == parameter && interval == powerMassSectionInterval section
    _ -> False)

reweightedSites :: PreparedPowerMassSection Int -> ExactRational -> IO (NonEmpty (PowerSite Int))
reweightedSites section parameter = traverse reweight (powerMassSectionSites section)
 where
  reweight site = do
    rate <- maybe (fail "section direction lost its site") pure (Map.lookup (powerSiteLabel site) (powerMassSectionDirection section))
    requireRight "exact reconstruction weight"
      (powerSite (powerSiteLabel site) (powerSitePosition site)
        (powerWeightFromExact (powerWeightExact (powerSiteWeight site) + parameter * rate)))

compareReconstruction :: PreparedPowerMassSection Int -> ExactRational -> IO ()
compareReconstruction section parameter = do
  observed <- requireRight "evaluate exact mass section" (evaluatePowerMasses parameter section)
  sites <- reweightedSites section parameter
  (diagram, _) <- requireRight "reconstruct public bounded power cells"
    (boundedPowerDiagram (powerMassSectionWindow section) sites)
  reconstructed <- traverse cellArea (powerCellDispositions diagram)
  assertEqual "certificate agrees with exact clipping reconstruction" (Map.fromList reconstructed) observed
 where
  cellArea :: (Int, PowerCellDisposition Int) -> IO (Int, ExactRational)
  cellArea (label, disposition) = case disposition of
    PublishedPowerCell polygon -> do
      area <- polygonArea (convexPolygonPoints polygon)
      pure (label, area)
    EmptyPowerCell -> pure (label, 0)
    other -> fail ("unexpected reconstructed disposition: " <> show other)

polygonArea :: NonEmpty ExactPoint -> IO ExactRational
polygonArea points@(initial :| remaining) =
  requireRight "exact reconstructed area"
    (exactDivide (sum (zipWith exactPointCross (NonEmpty.toList points) (remaining <> [initial]))) 2)
