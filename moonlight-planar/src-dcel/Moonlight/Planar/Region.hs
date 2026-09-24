-- | Exact polygon authoring and grouped publication through the resident DCEL
-- region traversal. Construction validates topology once; downstream overlay
-- consumes only admitted layers.
module Moonlight.Planar.Region
  ( ExactLoop
  , exactLoop
  , exactLoopPoints
  , PolygonComponent
  , polygonComponent
  , polygonOuterLoop
  , polygonHoleLoops
  , PlanarRegion
  , planarRegion
  , planarRegionComponents
  , emptyPlanarRegion
  , regionBoundaryEdges
  , RegionPointLocation (..)
  , regionPointLocation
  , PlanarLayer
  , planarLayerOutsideLabel
  , planarLayerRegions
  , planarLayer
  , planarLayerLabelAt
  , RegionValidationError (..)
  , RegionPublicationError (..)
  , labelledPlanarLayer
  ) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List (sort)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as V
import Moonlight.Planar.Exact
  ( ExactBounds
  , boundsOverlap
  , pointInBounds
  , ExactPoint
  , ExactSegment
  , SegmentRelation (..)
  , exactOnClosedSegment
  , exactOrient2d
  , exactPointCross
  , exactPointCoordinates
  , exactSegment
  , exactSegmentEndpoints
  )
import Moonlight.Planar.Internal.BoundaryCycle
  ( admitsSimpleCycleEdgeRelation
  , cyclePairs
  , rotateCycleLeast
  , simplifyBoundaryCycle
  )
import Moonlight.Planar.Internal.ExactSegmentEvents
  ( ExactSweepSegmentId (..)
  , exactSegmentEventPlan
  , exactSegmentRelationMap
  )
import Moonlight.Planar.Internal.ExactRational
  ( exactSignum )
import Moonlight.Planar.Internal.Region.Publication (labelledPlanarLayer)
import Moonlight.Planar.Internal.Region.Bounds
  ( componentBounds
  , exactLoopBounds
  , overlappingOptionalPairs
  , overlappingPairs
  , overlappingPairsBetween
  , regionBounds
  )
import Moonlight.Planar.Internal.Region.Types
  ( ExactLoop (..)
  , PlanarLayer (..)
  , PlanarRegion (..)
  , PolygonComponent (..)
  , polygonOuterLoop
  , polygonHoleLoops
  , planarLayerOutsideLabel
  , planarLayerRegions
  , RegionPointLocation (..)
  , RegionPublicationError (..)
  , RegionValidationError (..)
  )

data LoopRelationWitness = LoopRelationWitness
  !Int
  !Int
  !ExactSegment
  !ExactSegment
  !SegmentRelation

-- | One admitted loop with the bounds and segment vector every relation
-- question reads, built once per loop rather than once per pair asked.
data PreparedLoop = PreparedLoop
  !ExactLoop
  !ExactBounds
  !(V.Vector ExactSegment)

prepareLoop :: ExactLoop -> Either RegionValidationError PreparedLoop
prepareLoop loop =
  PreparedLoop loop (exactLoopBounds loop) <$> loopSegments (exactLoopPoints loop)

preparedBounds :: PreparedLoop -> ExactBounds
preparedBounds (PreparedLoop _ bounds _) = bounds

-- | Admit and canonicalize one simple exact cycle.
exactLoop :: NonEmpty ExactPoint -> Either RegionValidationError ExactLoop
exactLoop submitted = do
  let withoutRepeatedTerminal = removeRepeatedTerminal (NonEmpty.toList submitted)
  (_, simplified) <-
    simplifyBoundaryCycle
      RegionLoopDegenerate
      exactRedundant
      exactOrient2d
      id
      withoutRepeatedTerminal
  let canonical = rotateCycleLeast simplified
  validateSimpleLoop canonical
  pure (ExactLoop canonical)

-- | Read the canonical cycle points.
exactLoopPoints :: ExactLoop -> NonEmpty ExactPoint
exactLoopPoints (ExactLoop points) = points

-- | Admit one component after checking orientation, containment, boundary
-- relations, and pairwise-disjoint hole interiors.
polygonComponent
  :: ExactLoop
  -> [ExactLoop]
  -> Either RegionValidationError PolygonComponent
polygonComponent outer holes = do
  let outerWinding = loopWinding outer
  if outerWinding == GT
    then Right ()
    else Left (RegionOuterLoopWinding outerWinding)
  traverse_ validateHoleWinding indexedHoles
  unless (null holes) $ do
    preparedOuter <- prepareLoop outer
    preparedHoles <- traverse (traverse prepareLoop) indexedHoles
    traverse_ (validateHole preparedOuter) preparedHoles
    traverse_
      validateHolePair
      (overlappingPairs (preparedBounds . snd) preparedHoles)
  pure (PolygonComponent outer (sort holes))
 where
  indexedHoles = zip [0 ..] holes
  validateHoleWinding (holeIndex, hole) =
    let winding = loopWinding hole
     in if winding == LT
          then Right ()
          else Left (RegionHoleLoopWinding holeIndex winding)
  validateHole preparedOuter (holeIndex, preparedHole) = do
    relations <- crossLoopRelations preparedOuter preparedHole
    case relations of
      LoopRelationWitness outerEdge holeEdge _ _ relation : _ ->
        Left (RegionBoundaryRelation outerEdge holeEdge relation)
      [] -> Right ()
    let location = preparedPointLocation preparedOuter (firstPreparedPoint preparedHole)
    if location == RegionInterior
      then Right ()
      else Left (RegionHoleLocation holeIndex location)
  validateHolePair ((leftIndex, left), (rightIndex, right)) =
    do
      relations <- crossLoopRelations left right
      case relations of
        LoopRelationWitness _ _ _ _ relation : _ ->
          Left (RegionBoundaryRelation leftIndex rightIndex relation)
        []
          | loopContainsInteriorPoint left right || loopContainsInteriorPoint right left ->
              Left (RegionComponentInteriorOverlap leftIndex rightIndex)
          | otherwise -> Right ()

-- | Admit a finite region after checking that component interiors do not
-- overlap. Boundary-only contact remains lawful.
planarRegion
  :: [PolygonComponent]
  -> Either RegionValidationError PlanarRegion
planarRegion components = do
  traverse_
    validatePair
    (overlappingPairs (componentBounds . snd) (zip [0 ..] components))
  pure (PlanarRegion (sort components))
 where
  validatePair ((leftIndex, left), (rightIndex, right)) = do
    overlaps <- componentInteriorsOverlap left right
    if overlaps
      then Left (RegionComponentInteriorOverlap leftIndex rightIndex)
      else Right ()

-- | Observe the canonically ordered components.
planarRegionComponents :: PlanarRegion -> [PolygonComponent]
planarRegionComponents (PlanarRegion components) = components

-- | The empty finite region.
emptyPlanarRegion :: PlanarRegion
emptyPlanarRegion = PlanarRegion []

-- | Locate an exact point against all components. Boundary membership has
-- priority over interior membership.
regionPointLocation :: PlanarRegion -> ExactPoint -> RegionPointLocation
regionPointLocation (PlanarRegion components) query =
  foldr
    (combineLocation . (`componentPointLocation` query))
    RegionExterior
    components

-- | Admit one labelled layer. Different labels may touch but their interiors
-- may not overlap, and the outside label cannot also name a bounded region.
planarLayer
  :: Ord label
  => label
  -> Map label PlanarRegion
  -> Either RegionValidationError (PlanarLayer label)
planarLayer outside regions
  | Map.member outside regions = Left RegionOutsideLabelUsed
  | otherwise = do
      let indexed = zip [0 ..] (Map.toAscList regions)
      traverse_
        (\((leftIndex, (_, left)), (rightIndex, (_, right))) -> do
           overlaps <- regionsInteriorsOverlap left right
           if overlaps
             then Left (RegionLayerInteriorOverlap leftIndex rightIndex)
             else Right ())
        (overlappingOptionalPairs (regionBounds . snd . snd) indexed)
      pure (PlanarLayer outside regions)

-- | Label an exact point known to lie in a relatively open two-cell. Boundary
-- points conservatively retain the outside label.
planarLayerLabelAt :: PlanarLayer label -> ExactPoint -> label
planarLayerLabelAt layer query =
  case
    [ label
    | (label, region) <- Map.toAscList (planarLayerRegions layer)
    , regionPointLocation region query == RegionInterior
    ] of
    label : _ -> label
    [] -> planarLayerOutsideLabel layer

removeRepeatedTerminal :: Eq value => [value] -> [value]
removeRepeatedTerminal values =
  case values of
    [] -> []
    firstValue : remaining ->
      case reverse remaining of
        finalValue : reversedMiddle
          | firstValue == finalValue -> firstValue : reverse reversedMiddle
        _ -> values

exactRedundant :: ExactPoint -> ExactPoint -> ExactPoint -> Bool
exactRedundant previous current next =
  exactOrient2d previous current next == EQ
    && exactOnClosedSegment previous next current

validateSimpleLoop :: NonEmpty ExactPoint -> Either RegionValidationError ()
validateSimpleLoop points = do
  segments <- loopSegments points
  plan <- first RegionSegmentEventsInvalid (exactSegmentEventPlan segments)
  case
    [ (leftIndex, rightIndex, relation)
    | ((ExactSweepSegmentId leftIndex, ExactSweepSegmentId rightIndex), relation) <-
        Map.toAscList (exactSegmentRelationMap plan)
    , not
        ( admitsSimpleCycleEdgeRelation
            segmentCount
            leftIndex
            rightIndex
            relation
        )
    ] of
    (leftIndex, rightIndex, relation) : _ ->
      Left (RegionLoopSelfRelation leftIndex rightIndex relation)
    [] -> Right ()
 where
  segmentCount = NonEmpty.length points

loopSegments
  :: NonEmpty ExactPoint
  -> Either RegionValidationError (V.Vector ExactSegment)
loopSegments points =
  V.fromList
    <$> traverse
      (\(from, to) ->
         first (const (RegionLoopDegenerate (NonEmpty.toList points)))
           (exactSegment from to))
      (cyclePairs points)

loopWinding :: ExactLoop -> Ordering
loopWinding (ExactLoop points) =
  exactSignum
    ( List.foldl'
        (\signedArea (from, to) ->
           signedArea + exactPointCross from to)
        0
        (cyclePairs points)
    )

pointLocationInLoop :: ExactLoop -> ExactPoint -> RegionPointLocation
pointLocationInLoop loop = pointLocationInLoopWithin (exactLoopBounds loop) loop

preparedPointLocation :: PreparedLoop -> ExactPoint -> RegionPointLocation
preparedPointLocation (PreparedLoop loop bounds _) = pointLocationInLoopWithin bounds loop

pointLocationInLoopWithin :: ExactBounds -> ExactLoop -> ExactPoint -> RegionPointLocation
pointLocationInLoopWithin bounds loop query
  | not (pointInBounds query bounds) = RegionExterior
  | any (\(from, to) -> exactOnClosedSegment from to query) edges = RegionOnBoundary
  | odd (length (filter crossesRay edges)) = RegionInterior
  | otherwise = RegionExterior
 where
  edges = cyclePairs (exactLoopPoints loop)
  (_, py) = exactPointCoordinates query
  crossesRay (from, to) =
    let (_, ay) = exactPointCoordinates from
        (_, by) = exactPointCoordinates to
        orientation = exactOrient2d from to query
     in (ay <= py && py < by && orientation == GT)
          || (by <= py && py < ay && orientation == LT)

componentPointLocation :: PolygonComponent -> ExactPoint -> RegionPointLocation
componentPointLocation component query =
  case pointLocationInLoop (polygonOuterLoop component) query of
    RegionExterior -> RegionExterior
    RegionOnBoundary -> RegionOnBoundary
    RegionInterior -> foldr classifyHole RegionInterior (polygonHoleLoops component)
 where
  classifyHole hole remaining =
    case pointLocationInLoop hole query of
      RegionExterior -> remaining
      RegionOnBoundary -> RegionOnBoundary
      RegionInterior -> RegionExterior

crossLoopRelations
  :: PreparedLoop
  -> PreparedLoop
  -> Either RegionValidationError [LoopRelationWitness]
crossLoopRelations (PreparedLoop _ leftBounds leftSegments) (PreparedLoop _ rightBounds rightSegments)
  | not (boundsOverlap leftBounds rightBounds) = Right []
  | otherwise = do
      let leftCount = V.length leftSegments
      plan <-
        first RegionSegmentEventsInvalid
          (exactSegmentEventPlan (leftSegments <> rightSegments))
      pure
        [ LoopRelationWitness
            leftIndex
            (rightIndex - leftCount)
            (leftSegments V.! leftIndex)
            (rightSegments V.! (rightIndex - leftCount))
            relation
        | ((ExactSweepSegmentId leftIndex, ExactSweepSegmentId rightIndex), relation) <-
            Map.toAscList (exactSegmentRelationMap plan)
        , leftIndex < leftCount
        , rightIndex >= leftCount
        ]

loopContainsInteriorPoint :: PreparedLoop -> PreparedLoop -> Bool
loopContainsInteriorPoint container candidate =
  preparedPointLocation container (firstPreparedPoint candidate) == RegionInterior

componentInteriorsOverlap
  :: PolygonComponent
  -> PolygonComponent
  -> Either RegionValidationError Bool
componentInteriorsOverlap left right =
  boundariesProperlyCross left right
    >>= \crossing ->
      pure
        ( crossing
            || componentContainsInteriorPoint left right
            || componentContainsInteriorPoint right left
        )

boundariesProperlyCross
  :: PolygonComponent
  -> PolygonComponent
  -> Either RegionValidationError Bool
boundariesProperlyCross left right =
  anyEither
    (uncurry loopPairInteriorsOverlap)
    ( overlappingPairsBetween
        exactLoopBounds
        (componentLoops left)
        (componentLoops right)
    )
 where
  componentLoops component =
    polygonOuterLoop component : polygonHoleLoops component

loopPairInteriorsOverlap
  :: ExactLoop
  -> ExactLoop
  -> Either RegionValidationError Bool
loopPairInteriorsOverlap leftLoop rightLoop = do
  left <- prepareLoop leftLoop
  right <- prepareLoop rightLoop
  relations <- crossLoopRelations left right
  pure (any relationOverlapsInteriors relations)
 where
  relationOverlapsInteriors
    (LoopRelationWitness _ _ leftSegment rightSegment relation) =
    case relation of
      SegmentsProperlyCross -> True
      SegmentsDuplicate -> collinearInteriorsCoincide leftSegment rightSegment
      SegmentsCollinearlyOverlap -> collinearInteriorsCoincide leftSegment rightSegment
      _ -> False
  collinearInteriorsCoincide leftSegment rightSegment =
    canonicalDirection leftSegment == canonicalDirection rightSegment
  canonicalDirection segment =
    uncurry (<=) (exactSegmentEndpoints segment)

componentContainsInteriorPoint :: PolygonComponent -> PolygonComponent -> Bool
componentContainsInteriorPoint container candidate =
  any
    ((== RegionInterior) . componentPointLocation container)
    (NonEmpty.toList (exactLoopPoints (polygonOuterLoop candidate)))

firstPreparedPoint :: PreparedLoop -> ExactPoint
firstPreparedPoint (PreparedLoop (ExactLoop (point :| _)) _ _) = point

regionsInteriorsOverlap
  :: PlanarRegion
  -> PlanarRegion
  -> Either RegionValidationError Bool
regionsInteriorsOverlap (PlanarRegion left) (PlanarRegion right) =
  anyEither
    (uncurry componentInteriorsOverlap)
    (overlappingPairsBetween componentBounds left right)

combineLocation :: RegionPointLocation -> RegionPointLocation -> RegionPointLocation
combineLocation RegionOnBoundary _ = RegionOnBoundary
combineLocation RegionExterior accumulated = accumulated
combineLocation RegionInterior RegionOnBoundary = RegionOnBoundary
combineLocation RegionInterior _ = RegionInterior

anyEither
  :: (value -> Either obstruction Bool)
  -> [value]
  -> Either obstruction Bool
anyEither predicate =
  foldr
    (\value remaining -> do
       matches <- predicate value
       if matches then Right True else remaining)
    (Right False)

regionBoundaryEdges :: PlanarRegion -> [(ExactPoint, ExactPoint)]
regionBoundaryEdges region =
  concatMap
    (\component ->
       concatMap
         (cyclePairs . exactLoopPoints)
         (polygonOuterLoop component : polygonHoleLoops component))
    (planarRegionComponents region)
