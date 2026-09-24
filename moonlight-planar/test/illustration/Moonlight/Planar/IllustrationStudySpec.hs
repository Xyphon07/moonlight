module Moonlight.Planar.IllustrationStudySpec (tests) where

import Control.Monad (unless)
import Data.Foldable (toList, traverse_)
import Data.List (nub)
import Moonlight.Planar.Affine (Affine2, affine2, affineIsoMap, composeAffine2, identityAffine2, transformPoint)
import Moonlight.Planar.Curve (ClosedTrail, Located, location, transformLocatedClosedTrail)
import Moonlight.Planar.Curve.Lowering (loweringPolicy)
import Moonlight.Planar.Curve.Region (CurveComponent (..), lowerSimpleRegion)
import Moonlight.Planar.Exhibit.IllustrationStudy
import Moonlight.Planar.Exact (ExactPoint, ExactRational, ExactVector (..), exactPoint, positiveOne)
import Moonlight.Planar.Region (planarRegionComponents, polygonHoleLoops)
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
  case editControl FullerWidth 24 defaultControls of
    Left refusal -> ioError (userError (show refusal))
    Right wide -> checkBladeHole wide
  putStrLn "illustration study: semantic edits, stable anchors and admitted blade hole ok"

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

-- At the study's output scale, the actual outer/hole curves must lower into
-- one admitted polygon with one hole, including the widest allowed fuller.
checkBladeHole :: StudyControls -> IO ()
checkBladeHole controls = case loweringPolicy positiveOne identityAffine2 20 8192 of
  Left refusal -> ioError (userError (show refusal))
  Right policy -> case lowerSimpleRegion policy
    [CurveComponent (partContour controls Blade) [partContour controls Fuller]] of
      Left refusal -> ioError (userError (show refusal))
      Right (region,_) -> check "pierced blade retains exactly one admitted hole"
        (map (length . polygonHoleLoops) (planarRegionComponents region) == [1])
