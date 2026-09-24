module Moonlight.Planar.IllustrationStudySpec (tests) where

import Control.Monad (unless)
import Data.Foldable (toList, traverse_)
import Data.List (nub)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine (Affine2, affine2, affineIsoMap, composeAffine2, identityAffine2, transformPoint)
import Moonlight.Planar.Curve
  ( ClosedTrail, Located, Subpath (..), closedTrailSteps, locate, location, locatedValue, openTrail
  , transformLocatedClosedTrail )
import Moonlight.Planar.Curve.Frame (exactTrailSite)
import Moonlight.Planar.Curve.Lowering (loweringPolicy)
import Moonlight.Planar.Curve.Measure (distance, measurePolicy, radicalPrecision, sitePoint)
import Moonlight.Planar.Curve.Proximity (ClearanceVerdict (..), clearance)
import Moonlight.Planar.Curve.Region (CurveComponent (..), certifiedPointLocation, lowerSimpleRegion, subdivisionBudget)
import Moonlight.Planar.Exhibit.IllustrationStudy
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), exactHalf, exactPoint, exactPointCoordinates, positiveExact
  , positiveOne, unitOne, unitZero )
import Moonlight.Planar.Region (RegionPointLocation (..), planarRegionComponents, polygonHoleLoops)
import Moonlight.Planar.Illustration (PictureAlgebra (..), Paint (..), foldPicture)

-- These are artistic dependency tests, not another curve-equivalence oracle.
tests :: IO ()
tests = do
  let parts = [minBound..maxBound] :: [StudyPart]
  check "part names uniquely encode the author's vocabulary"
    (length (nub (partName <$> parts)) == length parts)
  check "horn pair shares one profile through exact reflection"
    (partContour defaultControls RightHorn == transformLocatedClosedTrail
      (affine2 (ExactVector (-1) 0) (ExactVector 0 1) (ExactVector 820 0))
      (partContour defaultControls LeftHorn))
  check "left horn port is profile root, not contour seam"
    (portPoint defaultControls LeftHorn == exactPoint 336 359
      && portPoint defaultControls LeftHorn /= location (partContour defaultControls LeftHorn))
  check "right horn root follows actual reflection"
    (portPoint defaultControls RightHorn == exactPoint 484 359)
  check "eye port is ellipse center, not seam"
    (portPoint defaultControls LeftEye == exactPoint 353 420
      && portPoint defaultControls LeftEye /= location (partContour defaultControls LeftEye))
  traverse_ (\part -> check ("actual attachment paints canonical contour: " <> show part)
    (any (\(name,contour,_) -> name == Just part && contour == partContour defaultControls part)
      (paintedContours defaultControls))) [LeftHorn,RightHorn,LeftEye,RightEye]
  check "attached horns retain world porcelain gradient coordinates"
    (all (\(_,_,paint) -> case paint of
      LinearGradient start end _ -> start == exactPoint 300 330 && end == exactPoint 520 500
      _ -> False)
      (filter (\(name,_,_) -> name == Just LeftHorn || name == Just RightHorn)
        (paintedContours defaultControls)))
  traverse_ checkEdit
    [(LeftHornSweep,30,[LeftHorn]),(EyeSpacing,66,[LeftEye,RightEye])
    ,(EyeTilt,12,[LeftEye,RightEye]),(CloakFullness,30,[Cloak]),(FullerWidth,20,[Fuller])]
  traverse_ checkRejected
    [(LeftHornSweep,46),(EyeSpacing,39),(EyeTilt,16),(CloakFullness,56),(FullerWidth,0)]
  checkBladeHole defaultControls
  traverse_
    (\width -> either (ioError . userError . show) checkBladeHole (editControl FullerWidth (fromInteger width) defaultControls))
    fullerWidthBounds
  checkHornBases defaultControls
  traverse_
    (\sweep -> either (ioError . userError . show) checkHornBases (editControl LeftHornSweep (fromInteger sweep) defaultControls))
    hornSweepBounds
  putStrLn ("illustration study: semantic edits and stable anchors ok; fuller certified strictly inside the blade at the default and FullerWidth "
    <> show fullerWidthBounds <> "; horn bases certified inside the mask at the default and LeftHornSweep " <> show hornSweepBounds)

checkEdit :: (StudyControl, ExactRational, [StudyPart]) -> IO ()
checkEdit (control,value,affected) = case editControl control value defaultControls of
  Left refusal -> ioError (userError (show refusal))
  Right edited -> do
    traverse_ (checkPart edited) [minBound..maxBound]
    check (show control <> " changes the actual picture")
      (studyPicture defaultControls /= studyPicture edited)
    check (show control <> " eye centers follow only spacing")
      ((partPort defaultControls LeftEye /= partPort edited LeftEye) == (control == EyeSpacing))
    check (show control <> " horn attachment is fixed")
      (partPort defaultControls LeftHorn == partPort edited LeftHorn)
 where
  checkPart :: StudyControls -> StudyPart -> IO ()
  checkPart edited part = do
    let original = partContour defaultControls part
        candidate = partContour edited part
    check (show control <> " geometry dependency: " <> show part)
      ((original /= candidate) == (part `elem` affected))
    -- Sweep/fullness/width edits preserve the authored attachment location;
    -- eye edits intentionally move the ellipse seam (spacing moves its center).
    unless (control `elem` [EyeSpacing,EyeTilt] && part `elem` affected) $
      check (show control <> " retains local anchor: " <> show part)
        (location original == location candidate)

checkRejected :: (StudyControl,ExactRational) -> IO ()
checkRejected (control,value) = check (show control <> " rejects out-of-family values") $
  case editControl control value defaultControls of
    Left (ControlOutsideRange rejected rejectedValue) -> rejected == control && rejectedValue == value
    Right _ -> False

check :: String -> Bool -> IO ()
check label result = unless result (ioError (userError label))

portPoint :: StudyControls -> StudyPart -> ExactPoint
portPoint controls part = transformPoint (affineIsoMap (partPort controls part)) (exactPoint 0 0)

-- Observe world contours and translated paint anchors, not nested Picture Eq.
paintedContours :: StudyControls -> [(Maybe StudyPart, Located ClosedTrail, Paint)]
paintedContours controls = foldPicture algebra (studyPicture controls) identityAffine2 Nothing
 where
  algebra :: PictureAlgebra StudyPart (Affine2 -> Maybe StudyPart -> [(Maybe StudyPart, Located ClosedTrail, Paint)])
  algebra = PictureAlgebra
    { paintSequence = \children frame name -> concatMap (\child -> child frame name) (toList children)
    , paintFill = \_ paint contours frame name ->
        fmap (\contour -> (name,transformLocatedClosedTrail frame contour,worldPaint frame paint)) (toList contours)
    , paintStroke = \_ _ _ _ -> []
    , paintClip = \_ _ child -> child
    , paintPlace = \local child frame -> child (composeAffine2 frame local)
    , paintOpacity = \_ child -> child
    , paintAnnotation = \part child frame _ -> child frame (Just part)
    }
  worldPaint :: Affine2 -> Paint -> Paint
  worldPaint frame paint = case paint of
    LinearGradient a b stops -> LinearGradient (transformPoint frame a) (transformPoint frame b) stops
    other -> other

-- The painting fills the blade with the fuller under EvenOdd, so the fuller
-- must lie strictly inside it: the certificate proves both curves simple,
-- disjoint and wound as outer and hole, and the fuller nested inside the
-- blade, and refuses a lowered polygon that nests otherwise. The tested scope
-- is the default controls and the fuller's editable bounds.
fullerWidthBounds :: [Integer]
fullerWidthBounds = [5, 24]

checkBladeHole :: StudyControls -> IO ()
checkBladeHole controls =
  case (loweringPolicy positiveOne identityAffine2 20 8192, subdivisionBudget 12 4096 4096) of
    (Left refusal, _) -> ioError (userError (show refusal))
    (_, Left refusal) -> ioError (userError (show refusal))
    (Right policy, Right budget) -> case lowerSimpleRegion policy budget
      [CurveComponent (partContour controls Blade) [partContour controls Fuller]] of
        Left refusal -> ioError (userError (show refusal))
        Right (region,_,_) -> check "pierced blade retains exactly one admitted hole"
          (map (length . polygonHoleLoops) (planarRegionComponents region) == [1])

-- The horns are painted beneath the mask and must attach under it: each
-- horn's base, the closing step of its contour across its root, lies inside
-- the mask. A connected segment disjoint from the mask curve with an
-- endpoint inside is wholly inside, since leaving would cross the curve. The
-- base's endpoints are exact sites of the closing step, certified inside by
-- the mask's curve evidence, and strict clearance at zero between the step
-- and the mask curve certifies them disjoint. The tested scope is the
-- default controls and the left horn's editable sweep bounds; the mask is
-- fixed and the sweep moves only the horn's tip.
hornSweepBounds :: [Integer]
hornSweepBounds = [-45, 45]

checkHornBases :: StudyControls -> IO ()
checkHornBases controls = do
  lowering <- orFail (loweringPolicy positiveOne identityAffine2 20 8192)
  budget <- orFail (subdivisionBudget 12 4096 4096)
  tolerance <- orFail (positiveExact 1)
  precision <- orFail (radicalPrecision 64)
  zero <- orFail (distance 0)
  let mask = partContour controls Mask
      measuring = measurePolicy tolerance precision budget
  (_, _, evidence) <- orFail (lowerSimpleRegion lowering budget [CurveComponent mask []])
  traverse_
    (\part -> do
        let contour = partContour controls part
            source = ClosedSubpath contour
            steps = closedTrailSteps (locatedValue contour)
            closing = Seq.length steps - 1
        start <- orFail (exactTrailSite source closing unitZero)
        end <- orFail (exactTrailSite source closing unitOne)
        check (show part <> " base spans its root")
          (midpoint (sitePoint start) (sitePoint end) == portPoint controls part)
        check (show part <> " base endpoints certified inside the mask")
          (map (certifiedPointLocation evidence) [sitePoint start, sitePoint end] == [Just RegionInterior, Just RegionInterior])
        base <- maybe (ioError (userError (show part <> " has no closing step"))) pure (Seq.lookup closing steps)
        verdict <- orFail (clearance measuring zero (OpenSubpath (locate (sitePoint start) (openTrail (Seq.singleton base)))) (ClosedSubpath mask))
        case verdict of
          ClearanceHolds _ -> pure ()
          other -> ioError (userError (show part <> " base meets the mask curve: " <> show other)))
    [LeftHorn, RightHorn]
 where
  orFail :: Show refusal => Either refusal value -> IO value
  orFail = either (ioError . userError . show) pure
  midpoint :: ExactPoint -> ExactPoint -> ExactPoint
  midpoint a b =
    let (ax, ay) = exactPointCoordinates a
        (bx, by) = exactPointCoordinates b
     in exactPoint (exactHalf * (ax + bx)) (exactHalf * (ay + by))
