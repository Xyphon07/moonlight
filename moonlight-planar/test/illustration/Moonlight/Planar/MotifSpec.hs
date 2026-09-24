-- | Coherent placement laws through exact world-geometry observations, not
-- structural equality of nested Picture placement syntax.
module Moonlight.Planar.MotifSpec (tests) where

import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine
import Moonlight.Planar.Curve
  ( ClosedTrail, Located, Path, Subpath (..), circle, path, transformPath )
import Moonlight.Planar.Exact
  ( ExactVector (..), UnitInterval, positiveOne, unitHalf )
import Moonlight.Planar.Illustration
  ( Picture, PictureAlgebra (..), Color (..), Paint (..), FillRule (..)
  , LineCap (..), LineJoin (..), StrokeUnits (..), StrokeStyle (..)
  , annotate, clip, fill, foldPicture, opacity, place, stroke )
import Moonlight.Planar.Illustration.Motif

data Port = Root | Tip
  deriving stock (Eq, Show)

-- Observations retain drawing order, annotations and opacity/clip scopes.
-- Only affine placement syntax is interpreted away.
data Observation
  = Filled !FillRule !Paint !Path
  | Stroked !StrokeStyle !Path
  | Clipped !FillRule !Path ![Observation]
  | Transparent !UnitInterval ![Observation]
  | Annotated !String ![Observation]
  deriving stock (Eq, Show)

observe :: Picture String -> [Observation]
observe picture = foldPicture algebra picture identityAffine2
 where
  algebra :: PictureAlgebra String (Affine2 -> [Observation])
  algebra = PictureAlgebra
    { paintSequence = \children frame -> concatMap ($ frame) (toList children)
    , paintFill = \rule paint contours frame ->
        [Filled rule paint (transformPath frame (contourPath contours))]
    , paintStroke = \style curves frame -> [Stroked style (transformPath frame curves)]
    , paintClip = \rule contours child frame ->
        [Clipped rule (transformPath frame (contourPath contours)) (child frame)]
    , paintPlace = \local child frame -> child (composeAffine2 frame local)
    , paintOpacity = \amount child frame -> [Transparent amount (child frame)]
    , paintAnnotation = \name child frame -> [Annotated name (child frame)]
    }
  contourPath :: NonEmpty (Located ClosedTrail) -> Path
  contourPath = path . Seq.fromList . fmap ClosedSubpath . toList

tests :: IO ()
tests = do
  source <- requireIso (affine2 (ExactVector 2 1) (ExactVector 1 3) (ExactVector 4 (-5)))
  target <- requireIso (affine2 (ExactVector (-2) 1) (ExactVector 0 3) (ExactVector 12 8))
  rebase <- requireIso (affine2 (ExactVector 1 2) (ExactVector 0 1) (ExactVector (-7) 2))
  let child = specimen source
      attached = attachMotif Root target child
      moved = transformMotif rebase child
      final = translationAffineIso2 (ExactVector 30 (-20))
  assertMotif "identity action" child (transformMotif identityAffineIso2 child)
  assertMotif "composition action"
    (transformMotif (composeAffineIso2 target rebase) child)
    (transformMotif target moved)
  assertEqual "nonidentity source frame aligns exactly" target (motifPort attached Root)
  assertMotif "self attachment" child (attachMotif Root source child)
  assertMotif "attachment composition"
    (attachMotif Root final child) (attachMotif Root final attached)
  assertMotif "child rebasing leaves attachment unchanged"
    attached (attachMotif Root target moved)
  assertEqual "other port transported with source" (composeAffineIso2
    (composeAffineIso2 target (inverseAffineIso2 source)) (motifPort child Tip))
    (motifPort attached Tip)
  assertEqual "attachment picture uses same pose"
    (observe (place (affineIsoMap (composeAffineIso2 target (inverseAffineIso2 source)))
      (motifPicture child))) (observe (motifPicture attached))
  assertEqual "singular frame remains refused" Nothing
    (affineIso2 (affine2 (ExactVector 1 2) (ExactVector 2 4) (ExactVector 0 0)))
  putStrLn "motif: ok"

specimen :: AffineIso2 -> Motif Port String
specimen root = motif picture ports
 where
  contour = circle positiveOne
  curves = path (Seq.singleton (ClosedSubpath contour))
  picture = annotate "body" (fill NonZero (Solid (RGB 1 2 3)) (contour :| []))
    <> opacity unitHalf (clip EvenOdd (contour :| [])
      (stroke (StrokeStyle (Solid (RGB 4 5 6)) positiveOne OutputUnits RoundCap RoundJoin) curves))
  ports :: Port -> AffineIso2
  ports Root = root
  ports Tip = translationAffineIso2 (ExactVector 9 7)

assertMotif :: String -> Motif Port String -> Motif Port String -> IO ()
assertMotif label expected actual = do
  assertEqual (label <> " geometry") (observe (motifPicture expected)) (observe (motifPicture actual))
  traverse_ (\port -> assertEqual (label <> " port " <> show port)
    (motifPort expected port) (motifPort actual port)) [Root,Tip]

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
  | expected == actual = pure ()
  | otherwise = fail (label <> ": expected " <> show expected <> ", got " <> show actual)

requireIso :: Affine2 -> IO AffineIso2
requireIso value = case affineIso2 value of
  Nothing -> fail "nonsingular test frame refused"
  Just frame -> pure frame
