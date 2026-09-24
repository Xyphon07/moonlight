-- | Exact action laws and witness-preserving polygon transport.
module Moonlight.Planar.AffineSpec (tests) where

import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Moonlight.Planar.Affine
import Moonlight.Planar.Convex (convexPolygon, convexPolygonPoints)
import Moonlight.Planar.Exact
  ( ExactPoint
  , ExactRational
  , ExactVector (..)
  , exactHalf
  , exactPoint
  , exactThird
  , translateExactPoint
  )
import Moonlight.Planar.Region
  ( exactLoop
  , exactLoopPoints
  , planarRegion
  , planarRegionComponents
  , polygonComponent
  , polygonHoleLoops
  , polygonOuterLoop
  , regionPointLocation
  )
import Support (assertEqual, requireRight)

tests :: IO ()
tests = sequence_
  [ testAffineActions
  , testIsoAdmission
  , testIsoInverse
  , testTranslationIso
  , testRegionTransport
  , testConvexTransport
  , putStrLn "affine: ok"
  ]

reflection :: Affine2
reflection = affine2 (ExactVector (-1) 0) (ExactVector 0 1) (ExactVector 7 3)

shear :: Affine2
shear = affine2 (ExactVector 2 1) (ExactVector 1 1) (ExactVector (-4) 5)

collapse :: Affine2
collapse = affine2 (ExactVector 1 2) (ExactVector 2 4) (ExactVector 3 7)

testAffineActions :: IO ()
testAffineActions = do
  let transforms = [identityAffine2, reflection, shear, collapse]
      point = exactPoint 3 (-2)
      vector = ExactVector (-1) 4
  traverse_
    (\transform -> do
       assertEqual "left affine identity" transform (composeAffine2 identityAffine2 transform)
       assertEqual "right affine identity" transform (composeAffine2 transform identityAffine2)
       assertEqual "affine preserves point displacement"
         (transformPoint transform (translateExactPoint point vector))
         (translateExactPoint (transformPoint transform point) (transformVector transform vector)))
    transforms
  traverse_
    (\(outer, inner) -> do
       let composed = composeAffine2 outer inner
       assertEqual "point composition order"
         (transformPoint outer (transformPoint inner point))
         (transformPoint composed point)
       assertEqual "vector composition order"
         (transformVector outer (transformVector inner vector))
         (transformVector composed vector)
       assertEqual "determinants multiply"
         (affineDeterminant outer * affineDeterminant inner)
         (affineDeterminant composed))
    [(outer, inner) | outer <- transforms, inner <- transforms]
  traverse_
    (\(a, b, c) -> assertEqual "affine composition associativity"
      (composeAffine2 (composeAffine2 a b) c)
      (composeAffine2 a (composeAffine2 b c)))
    [(a, b, c) | a <- transforms, b <- transforms, c <- transforms]
  assertEqual "singular point action is still lawful"
    (exactPoint 2 5) (transformPoint collapse point)
  assertEqual "column observation"
    (ExactVector 2 1, ExactVector 1 1, ExactVector (-4) 5)
    (affineColumns shear)

testTranslationIso :: IO ()
testTranslationIso = do
  let offset = ExactVector exactThird (-7)
      other = ExactVector 4 exactHalf
      translation = translationAffineIso2 offset
      point = exactPoint 2 3
      vector = ExactVector 5 6
  assertEqual "zero translation is identity" identityAffineIso2
    (translationAffineIso2 (ExactVector 0 0))
  assertEqual "translation moves points" (translateExactPoint point offset)
    (transformPoint (affineIsoMap translation) point)
  assertEqual "translation leaves vectors unchanged" vector
    (transformVector (affineIsoMap translation) vector)
  assertEqual "translation determinant" 1 (affineDeterminant (affineIsoMap translation))
  assertEqual "translation inverse" (translationAffineIso2 (ExactVector (negate exactThird) 7))
    (inverseAffineIso2 translation)
  assertEqual "translation composition"
    (translationAffineIso2 (ExactVector (4 + exactThird) (exactHalf - 7)))
    (composeAffineIso2 translation (translationAffineIso2 other))

testIsoAdmission :: IO ()
testIsoAdmission = do
  assertEqual "singular map cannot transport topology" Nothing (affineIso2 collapse)
  assertEqual "identity iso admission" (Just identityAffineIso2) (affineIso2 identityAffine2)
  reversing <- requireIso reflection
  preserving <- requireIso shear
  traverse_
    (\(outer, inner) -> assertEqual "iso composition agrees with checked admission"
      (affineIso2 (composeAffine2 (affineIsoMap outer) (affineIsoMap inner)))
      (Just (composeAffineIso2 outer inner)))
    [(outer, inner) | outer <- [reversing, preserving], inner <- [reversing, preserving]]

testIsoInverse :: IO ()
testIsoInverse = do
  let rationalFrame = affine2
        (ExactVector 2 1) (ExactVector exactThird exactHalf) (ExactVector 5 (-3))
      point = exactPoint exactHalf (negate exactThird)
      vector = ExactVector exactThird 7
  frames <- traverse requireIso
    [identityAffine2, reflection, shear, rationalFrame, composeAffine2 reflection rationalFrame]
  traverse_
    (\frame -> do
       let inverse = inverseAffineIso2 frame
       assertEqual "inverse on the left" identityAffineIso2 (composeAffineIso2 inverse frame)
       assertEqual "inverse on the right" identityAffineIso2 (composeAffineIso2 frame inverse)
       assertEqual "inverse involution" frame (inverseAffineIso2 inverse)
       assertEqual "inverse agrees with checked admission"
         (Just inverse) (affineIso2 (affineIsoMap inverse))
       assertEqual "point inverse roundtrip" point
         (transformPoint (affineIsoMap inverse) (transformPoint (affineIsoMap frame) point))
       assertEqual "vector inverse roundtrip" vector
         (transformVector (affineIsoMap inverse) (transformVector (affineIsoMap frame) vector)))
    frames
  traverse_
    (\(outer, inner) -> assertEqual "inverse reverses composition order"
      (composeAffineIso2 (inverseAffineIso2 inner) (inverseAffineIso2 outer))
      (inverseAffineIso2 (composeAffineIso2 outer inner)))
    [(outer, inner) | outer <- frames, inner <- frames]

testRegionTransport :: IO ()
testRegionTransport = do
  outer <- requireRight "outer loop" (exactLoop (rectangle 0 0 8 8))
  holeLeft <- requireRight "left clockwise hole" (exactLoop (NonEmpty.reverse (rectangle 1 1 3 3)))
  holeRight <- requireRight "right clockwise hole" (exactLoop (NonEmpty.reverse (rectangle 5 1 7 3)))
  component <- requireRight "pierced component" (polygonComponent outer [holeLeft, holeRight])
  islandLoop <- requireRight "separate island" (exactLoop (rectangle 10 0 12 2))
  island <- requireRight "island component" (polygonComponent islandLoop [])
  source <- requireRight "disjoint region" (planarRegion [component, island])
  reversing <- requireIso reflection
  preserving <- requireIso shear
  assertEqual "region transport identity" source (transformPlanarRegion identityAffineIso2 source)
  traverse_
    (\iso -> do
       let mapped = transformPlanarRegion iso source
           transform = affineIsoMap iso
           expectedLoop loop = exactLoop
             ((if affineDeterminant transform < 0 then NonEmpty.reverse else id)
               (fmap (transformPoint transform) (exactLoopPoints loop)))
       checkedComponents <- traverse
         (\original -> do
           checkedOuter <- requireRight "mapped outer admission" (expectedLoop (polygonOuterLoop original))
           checkedHoles <- traverse
             (requireRight "mapped hole admission" . expectedLoop)
             (polygonHoleLoops original)
           requireRight "mapped component admission" (polygonComponent checkedOuter checkedHoles))
         (planarRegionComponents source)
       checked <- requireRight "mapped region admission" (planarRegion checkedComponents)
       assertEqual "transport equals checked canonical region" checked mapped
       assertEqual "inverse restores canonical region with sorted holes and components" source
         (transformPlanarRegion (inverseAffineIso2 iso) mapped)
       traverse_
         (\query -> assertEqual "point location commutes with iso transport"
           (regionPointLocation source query)
           (regionPointLocation mapped (transformPoint transform query)))
         [exactPoint 0 0, exactPoint 1 1, exactPoint 1 2, exactPoint 2 2, exactPoint 4 4, exactPoint 9 9, exactPoint 11 1])
    [reversing, preserving]
  assertEqual "canonical region action composition"
    (transformPlanarRegion reversing (transformPlanarRegion preserving source))
    (transformPlanarRegion (composeAffineIso2 reversing preserving) source)

testConvexTransport :: IO ()
testConvexTransport = do
  source <- requireRight "convex rectangle" (convexPolygon (rectangle 0 0 4 3))
  reversing <- requireIso reflection
  let transformed = transformConvexPolygon reversing source
  expected <- requireRight "reflected convex admission"
    (convexPolygon (NonEmpty.reverse (fmap (transformPoint reflection) (convexPolygonPoints source))))
  assertEqual "reflection retains strict counterclockwise convexity" expected transformed
  assertEqual "convex transport identity" source (transformConvexPolygon identityAffineIso2 source)
  assertEqual "convex inverse restores canonical cycle" source
    (transformConvexPolygon (inverseAffineIso2 reversing) transformed)

rectangle :: ExactRational -> ExactRational -> ExactRational -> ExactRational -> NonEmpty ExactPoint
rectangle x0 y0 x1 y1 =
  exactPoint x0 y0 :| [exactPoint x1 y0, exactPoint x1 y1, exactPoint x0 y1]

requireIso :: Affine2 -> IO AffineIso2
requireIso = maybe (fail "fixture affine map is singular") pure . affineIso2
