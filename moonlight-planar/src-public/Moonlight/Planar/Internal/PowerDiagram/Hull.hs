-- | Exact upper-hull descent for affine generators. The public power owner
-- supplies admitted, distinct slopes; this module returns only topology and
-- exact dual geometry.
module Moonlight.Planar.Internal.PowerDiagram.Hull
  ( regularGeneratorTopology
  ) where

import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Planar.Exact
  ( ExactAffineLine
  , ExactPoint
  , ExactRay
  , exactPoint
  , exactPointCoordinates
  )
import Moonlight.Planar.Internal.BoundaryCycle (consecutivePairs)
import Moonlight.Planar.Internal.ExactRational (ExactRational)
import Moonlight.Planar.Convex (convexHullPolygon, convexPolygonPoints)
import Moonlight.Planar.Internal.PowerDiagram.Generator

-- | Construct exact regular topology from distinct affine slopes. Full-rank
-- input uses one conflict graph; collinear slopes use the one-dimensional
-- upper chain; coplanar lifted input reuses the exact planar hull owner.
regularGeneratorTopology
  :: Ord label
  => DistinctSlopeGenerators label
  -> Either (RegularTopologyError label) (GeneratorRegularTopology label)
regularGeneratorTopology (DistinctSlopeGenerators generators) =
  let points = admittedLiftedPoints generators
   in case points of
        singleton :| [] ->
          pure (topologyFromParts points (Set.singleton singleton) Set.empty [] [])
        firstPoint :| secondPoint : remaining ->
          case List.find (not . slopeCollinear firstPoint secondPoint) remaining of
            Nothing -> collinearSlopeTopology points firstPoint secondPoint
            Just thirdPoint ->
              case List.find (not . liftedCoplanar firstPoint secondPoint thirdPoint) remaining of
                Nothing -> coplanarLiftedTopology points
                Just fourthPoint -> fullRankTopology points firstPoint secondPoint thirdPoint fourthPoint

data LiftedPoint label = LiftedPoint
  { liftedPointId :: !Int
  , liftedPointGenerator :: !(ExactPowerGenerator label)
  }

instance Eq (LiftedPoint label) where
  left == right = liftedPointId left == liftedPointId right

instance Ord (LiftedPoint label) where
  compare left right = compare (liftedPointId left) (liftedPointId right)

admittedLiftedPoints
  :: Ord label
  => NonEmpty (ExactPowerGenerator label)
  -> NonEmpty (LiftedPoint label)
admittedLiftedPoints generators =
  let ordered = NonEmpty.sortWith exactPowerGeneratorLabel generators
   in NonEmpty.zipWith
        (\identifier generator -> LiftedPoint identifier generator)
        (0 :| [1 ..])
        ordered

liftedCoordinates
  :: LiftedPoint label
  -> (ExactRational, ExactRational, ExactRational)
liftedCoordinates point =
  let generator = liftedPointGenerator point
   in ( exactPowerGeneratorXCoefficient generator
      , exactPowerGeneratorYCoefficient generator
      , exactPowerGeneratorConstant generator
      )

slopePoint :: LiftedPoint label -> ExactPoint
slopePoint point =
  let (coordinateX, coordinateY, _) = liftedCoordinates point
   in exactPoint coordinateX coordinateY

slopeCollinear
  :: LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> Bool
slopeCollinear firstPoint secondPoint thirdPoint =
  slopeOrientation firstPoint secondPoint thirdPoint == 0

slopeOrientation
  :: LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> ExactRational
slopeOrientation firstPoint secondPoint thirdPoint =
  let (firstX, firstY, _) = liftedCoordinates firstPoint
      (secondX, secondY, _) = liftedCoordinates secondPoint
      (thirdX, thirdY, _) = liftedCoordinates thirdPoint
   in (secondX - firstX) * (thirdY - firstY)
        - (secondY - firstY) * (thirdX - firstX)

liftedCoplanar
  :: LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> Bool
liftedCoplanar firstPoint secondPoint thirdPoint fourthPoint =
  exactGeneratorLiftedSign
    (liftedPointGenerator firstPoint)
    (liftedPointGenerator secondPoint)
    (liftedPointGenerator thirdPoint)
    (liftedPointGenerator fourthPoint)
    == EQ

liftedVolume
  :: LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> ExactRational
liftedVolume firstPoint secondPoint thirdPoint fourthPoint =
  exactGeneratorLiftedVolume
    (liftedPointGenerator firstPoint)
    (liftedPointGenerator secondPoint)
    (liftedPointGenerator thirdPoint)
    (liftedPointGenerator fourthPoint)

data HullFace label = HullFace
  { hullFaceId :: !Int
  , hullFaceFirst :: !(LiftedPoint label)
  , hullFaceSecond :: !(LiftedPoint label)
  , hullFaceThird :: !(LiftedPoint label)
  , hullFaceOutside :: !(Map (LiftedPoint label) ExactRational)
    -- ^ Each outside point with its exact lifted volume against this face.
  , hullFaceCoplanar :: !(Set (LiftedPoint label))
  }

type HullEdge = (Int, Int)

data HullState label = HullState
  { hullFaces :: !(IntMap.IntMap (HullFace label))
  , hullEdgeFaces :: !(Map HullEdge IntSet.IntSet)
  , hullConflictFaces :: !IntSet.IntSet
  , hullNextFaceId :: !Int
  }

fullRankTopology
  :: NonEmpty (LiftedPoint label)
  -> LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> Either (RegularTopologyError label) (GeneratorRegularTopology label)
fullRankTopology points firstPoint secondPoint thirdPoint fourthPoint = do
  let interior = liftedInteriorSum firstPoint secondPoint thirdPoint fourthPoint
      simplex = Set.fromList [firstPoint, secondPoint, thirdPoint, fourthPoint]
      initialFaces =
        zipWith
          (orientedHullFace interior)
          [0 ..]
          [ (firstPoint, secondPoint, thirdPoint)
          , (firstPoint, fourthPoint, secondPoint)
          , (firstPoint, thirdPoint, fourthPoint)
          , (secondPoint, fourthPoint, thirdPoint)
          ]
      initialState =
        List.foldl'
          insertHullFace
          (HullState IntMap.empty Map.empty IntSet.empty 4)
          initialFaces
      seededState =
        Foldable.foldl'
          (flip assignPointToHull)
          initialState
          (filter (`Set.notMember` simplex) (NonEmpty.toList points))
      finalState = descendHull interior seededState
      upperFaces = filter ((> 0) . hullFaceNormalZ) (IntMap.elems (hullFaces finalState))
  (canonicalFaces, visible, lowerDimensional) <- canonicalUpperFaces upperFaces
  (faces, edges) <- topologyFromHullFaces canonicalFaces
  pure
    (topologyFromParts points visible lowerDimensional faces edges)

liftedInteriorSum
  :: LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> (ExactRational, ExactRational, ExactRational)
liftedInteriorSum firstPoint secondPoint thirdPoint fourthPoint =
  let (firstX, firstY, firstZ) = liftedCoordinates firstPoint
      (secondX, secondY, secondZ) = liftedCoordinates secondPoint
      (thirdX, thirdY, thirdZ) = liftedCoordinates thirdPoint
      (fourthX, fourthY, fourthZ) = liftedCoordinates fourthPoint
   in ( firstX + secondX + thirdX + fourthX
      , firstY + secondY + thirdY + fourthY
      , firstZ + secondZ + thirdZ + fourthZ
      )

orientedHullFace
  :: (ExactRational, ExactRational, ExactRational)
  -> Int
  -> (LiftedPoint label, LiftedPoint label, LiftedPoint label)
  -> HullFace label
orientedHullFace interior identifier (firstPoint, secondPoint, thirdPoint) =
  let provisional = HullFace identifier firstPoint secondPoint thirdPoint Map.empty Set.empty
   in if hullFaceVolumeAtInterior interior provisional < 0
        then provisional
        else HullFace identifier firstPoint thirdPoint secondPoint Map.empty Set.empty

hullFaceVolumeAtInterior
  :: (ExactRational, ExactRational, ExactRational)
  -> HullFace label
  -> ExactRational
hullFaceVolumeAtInterior (sumX, sumY, sumZ) face =
  let (firstX, firstY, firstZ) = liftedCoordinates (hullFaceFirst face)
      (secondX, secondY, secondZ) = liftedCoordinates (hullFaceSecond face)
      (thirdX, thirdY, thirdZ) = liftedCoordinates (hullFaceThird face)
      secondX' = secondX - firstX
      secondY' = secondY - firstY
      secondZ' = secondZ - firstZ
      thirdX' = thirdX - firstX
      thirdY' = thirdY - firstY
      thirdZ' = thirdZ - firstZ
      interiorX' = sumX - 4 * firstX
      interiorY' = sumY - 4 * firstY
      interiorZ' = sumZ - 4 * firstZ
   in secondX' * (thirdY' * interiorZ' - thirdZ' * interiorY')
        - secondY' * (thirdX' * interiorZ' - thirdZ' * interiorX')
        + secondZ' * (thirdX' * interiorY' - thirdY' * interiorX')

liftedVolumeAtFace :: HullFace label -> LiftedPoint label -> ExactRational
liftedVolumeAtFace face =
  liftedVolume
    (hullFaceFirst face)
    (hullFaceSecond face)
    (hullFaceThird face)

hullFaceNormalZ :: HullFace label -> ExactRational
hullFaceNormalZ face =
  slopeOrientation
    (hullFaceFirst face)
    (hullFaceSecond face)
    (hullFaceThird face)

hullFacePoints :: HullFace label -> [LiftedPoint label]
hullFacePoints face =
  [hullFaceFirst face, hullFaceSecond face, hullFaceThird face]

hullFaceDirectedEdges
  :: HullFace label
  -> [(LiftedPoint label, LiftedPoint label)]
hullFaceDirectedEdges face =
  [ (hullFaceFirst face, hullFaceSecond face)
  , (hullFaceSecond face, hullFaceThird face)
  , (hullFaceThird face, hullFaceFirst face)
  ]

hullEdgeKey :: LiftedPoint label -> LiftedPoint label -> HullEdge
hullEdgeKey firstPoint secondPoint =
  let firstId = liftedPointId firstPoint
      secondId = liftedPointId secondPoint
   in if firstId <= secondId then (firstId, secondId) else (secondId, firstId)

insertHullFace :: HullState label -> HullFace label -> HullState label
insertHullFace state face =
  state
    { hullFaces = IntMap.insert (hullFaceId face) face (hullFaces state)
    , hullEdgeFaces =
        List.foldl'
          (\incidence (fromPoint, toPoint) ->
             Map.insertWith
               IntSet.union
               (hullEdgeKey fromPoint toPoint)
               (IntSet.singleton (hullFaceId face))
               incidence)
          (hullEdgeFaces state)
          (hullFaceDirectedEdges face)
    }

assignPointToHull :: LiftedPoint label -> HullState label -> HullState label
assignPointToHull point state =
  attachSelectedPoint point state
    (selectHullFace point (IntMap.elems (hullFaces state)))

data HullFaceSelection label
  = HullFaceInterior
  | HullFaceCoplanar !(HullFace label)
  | HullFaceVisible !(HullFace label) !ExactRational

selectHullFace
  :: LiftedPoint label
  -> [HullFace label]
  -> HullFaceSelection label
selectHullFace point = List.foldl' choose HullFaceInterior
 where
  choose selected face =
    let volume = liftedVolumeAtFace face point
     in case compare volume 0 of
          LT -> selected
          EQ ->
            case selected of
              HullFaceInterior -> HullFaceCoplanar face
              _ -> selected
          GT ->
            case selected of
              HullFaceVisible current currentVolume
                | volume < currentVolume
                    || (volume == currentVolume && hullFaceId current < hullFaceId face) ->
                    selected
              _ -> HullFaceVisible face volume

attachSelectedPoint
  :: LiftedPoint label
  -> HullState label
  -> HullFaceSelection label
  -> HullState label
attachSelectedPoint point state selection =
  case selection of
    HullFaceInterior -> state
    HullFaceCoplanar face ->
      state
        { hullFaces =
            IntMap.adjust
              (\selected -> selected{hullFaceCoplanar = Set.insert point (hullFaceCoplanar selected)})
              (hullFaceId face)
              (hullFaces state)
        }
    HullFaceVisible face volume ->
      state
        { hullFaces =
            IntMap.adjust
              (\selected -> selected{hullFaceOutside = Map.insert point volume (hullFaceOutside selected)})
              (hullFaceId face)
              (hullFaces state)
        , hullConflictFaces = IntSet.insert (hullFaceId face) (hullConflictFaces state)
        }

descendHull
  :: (ExactRational, ExactRational, ExactRational)
  -> HullState label
  -> HullState label
descendHull interior state =
  case nextHullExpansion state of
    Nothing -> state
    Just (seedFace, apex) ->
      let visible = visibleHullPatch state apex (IntSet.singleton (hullFaceId seedFace)) [hullFaceId seedFace]
          visibleFaces =
            mapMaybe (`IntMap.lookup` hullFaces state) (IntSet.toAscList visible)
          horizon = hullHorizon state visible visibleFaces
          candidates =
            Set.difference
              ( Set.delete apex
                  ( Set.unions
                      [ Set.unions
                          [ Map.keysSet (hullFaceOutside face)
                          , hullFaceCoplanar face
                          , Set.fromList (hullFacePoints face)
                          ]
                      | face <- visibleFaces
                      ]
                  )
              )
              ( Set.fromList
                  [ point
                  | (firstPoint, secondPoint) <- Map.elems horizon
                  , point <- [firstPoint, secondPoint]
                  ]
              )
          withoutVisible = removeHullFaces visibleFaces state
          (withHorizon, newFaceIds) =
            Map.foldl'
              (insertHorizonFace interior apex)
              (withoutVisible, [])
              horizon
          redistributed =
            Foldable.foldl'
              (assignPointToFaces newFaceIds)
              withHorizon
              candidates
       in descendHull interior redistributed

nextHullExpansion
  :: HullState label
  -> Maybe (HullFace label, LiftedPoint label)
nextHullExpansion state = do
  identifier <- IntSet.lookupMin (hullConflictFaces state)
  face <- IntMap.lookup identifier (hullFaces state)
  apex <- farthestOutsidePoint face
  pure (face, apex)

-- | The lowest-id point among those of maximal stored volume.
farthestOutsidePoint :: HullFace label -> Maybe (LiftedPoint label)
farthestOutsidePoint face =
  fst
    <$> Map.foldlWithKey'
      (\selected candidate candidateVolume ->
         case selected of
           Just (_, selectedVolume)
             | candidateVolume <= selectedVolume -> selected
           _ -> Just (candidate, candidateVolume))
      Nothing
      (hullFaceOutside face)

visibleHullPatch
  :: HullState label
  -> LiftedPoint label
  -> IntSet.IntSet
  -> [Int]
  -> IntSet.IntSet
visibleHullPatch _ _ visited [] = visited
visibleHullPatch state apex visited (identifier : remaining) =
  case IntMap.lookup identifier (hullFaces state) of
    Nothing -> visibleHullPatch state apex visited remaining
    Just face ->
      let neighbours =
            IntSet.unions
              [ Map.findWithDefault IntSet.empty (hullEdgeKey fromPoint toPoint) (hullEdgeFaces state)
              | (fromPoint, toPoint) <- hullFaceDirectedEdges face
              ]
          newlyVisible =
            IntSet.filter
              (\candidateId ->
                 IntSet.notMember candidateId visited
                   && maybe
                     False
                     ((> 0) . (`liftedVolumeAtFace` apex))
                     (IntMap.lookup candidateId (hullFaces state)))
              neighbours
       in visibleHullPatch
            state
            apex
            (IntSet.union visited newlyVisible)
            (IntSet.toList newlyVisible <> remaining)

hullHorizon
  :: HullState label
  -> IntSet.IntSet
  -> [HullFace label]
  -> Map HullEdge (LiftedPoint label, LiftedPoint label)
hullHorizon state visible visibleFaces =
  Map.fromList
    [ (hullEdgeKey fromPoint toPoint, (fromPoint, toPoint))
    | face <- visibleFaces
    , (fromPoint, toPoint) <- hullFaceDirectedEdges face
    , not
        ( IntSet.null
            ( IntSet.difference
                (Map.findWithDefault IntSet.empty (hullEdgeKey fromPoint toPoint) (hullEdgeFaces state))
                visible
            )
        )
    ]

removeHullFaces :: [HullFace label] -> HullState label -> HullState label
removeHullFaces removed state = List.foldl' removeHullFace state removed

removeHullFace :: HullState label -> HullFace label -> HullState label
removeHullFace state face =
  state
    { hullFaces = IntMap.delete identifier (hullFaces state)
    , hullEdgeFaces =
        List.foldl'
          (removeHullFaceFromEdge identifier)
          (hullEdgeFaces state)
          (hullFaceDirectedEdges face)
    , hullConflictFaces = IntSet.delete identifier (hullConflictFaces state)
    }
 where
  identifier = hullFaceId face

removeHullFaceFromEdge
  :: Int
  -> Map HullEdge IntSet.IntSet
  -> (LiftedPoint label, LiftedPoint label)
  -> Map HullEdge IntSet.IntSet
removeHullFaceFromEdge identifier incidence (fromPoint, toPoint) =
  Map.update
    (\incident ->
       let retained = IntSet.delete identifier incident
        in if IntSet.null retained then Nothing else Just retained)
    (hullEdgeKey fromPoint toPoint)
    incidence

insertHorizonFace
  :: (ExactRational, ExactRational, ExactRational)
  -> LiftedPoint label
  -> (HullState label, [Int])
  -> (LiftedPoint label, LiftedPoint label)
  -> (HullState label, [Int])
insertHorizonFace interior apex (state, identifiers) (firstPoint, secondPoint) =
  let identifier = hullNextFaceId state
      face = orientedHullFace interior identifier (firstPoint, secondPoint, apex)
      nextState =
        (insertHullFace state face)
          { hullNextFaceId = identifier + 1 }
   in (nextState, identifier : identifiers)

assignPointToFaces
  :: [Int]
  -> HullState label
  -> LiftedPoint label
  -> HullState label
assignPointToFaces identifiers state point =
  attachSelectedPoint point state
    (selectHullFace point (mapMaybe (`IntMap.lookup` hullFaces state) identifiers))

-- | Glue the triangular implementation facets on each exact supporting plane,
-- then recover only that plane's extreme slope vertices. This removes
-- simplex/horizon diagonals from the visibility decision while retaining a
-- deterministic triangulation for incidence and collapsed duals.
canonicalUpperFaces
  :: [HullFace label]
  -> Either
      (RegularTopologyError label)
      ( [(HullFace label, ExactPoint)]
      , Set (LiftedPoint label)
      , Set (LiftedPoint label)
      )
canonicalUpperFaces faces = do
  associations <- traverse upperFaceAssociation faces
  sections <-
    traverse canonicalUpperFaceGroup
      ( Map.elems
          ( List.foldl'
              (\groups (planeKey, dualFace) ->
                 Map.insertWith (<>) planeKey (dualFace :| []) groups)
              Map.empty
              associations
          )
      )
  let (sectionFaces, visible, lowerDimensional) = mconcat sections
      canonicalFaces =
        zipWith
          (\identifier (face, dualPoint) ->
             (face{hullFaceId = identifier}, dualPoint))
          [0 ..]
          sectionFaces
  pure (canonicalFaces, visible, lowerDimensional)

canonicalUpperFaceGroup
  :: NonEmpty (HullFace label, ExactPoint)
  -> Either
      (RegularTopologyError label)
      ( [(HullFace label, ExactPoint)]
      , Set (LiftedPoint label)
      , Set (LiftedPoint label)
      )
canonicalUpperFaceGroup ((face, dualPoint) :| [])
  | Set.null (hullFaceCoplanar face) =
      Right ([(face, dualPoint)], Set.fromList (hullFacePoints face), Set.empty)
canonicalUpperFaceGroup ((firstFace, dualPoint) :| remainingDualFaces) = do
  let groupFaces = firstFace : fmap fst remainingDualFaces
      groupPoints =
        Set.unions
          [ Set.union
              (Set.fromList (hullFacePoints face))
              (hullFaceCoplanar face)
          | face <- groupFaces
          ]
      initialPoint = hullFaceFirst firstFace
      points = initialPoint :| Set.toAscList (Set.delete initialPoint groupPoints)
      pointsBySlope =
        Map.fromList [(slopePoint point, point) | point <- NonEmpty.toList points]
  hull <-
    first RegularTopologySlopeHullInvalid
      (convexHullPolygon (fmap slopePoint points))
  visiblePoints <-
    traverse
      (\point ->
         maybe
           (Left (RegularTopologySlopeVertexMissing point))
           Right
           (Map.lookup point pointsBySlope))
      (NonEmpty.toList (convexPolygonPoints hull))
  let visible = Set.fromList visiblePoints
  pure
    ( fmap (\fanFace -> (fanFace, dualPoint)) (coplanarFanFaces visiblePoints)
    , visible
    , Set.difference groupPoints visible
    )

hullFacePlaneKey
  :: HullFace label
  -> ExactPoint
  -> (ExactRational, ExactRational, ExactRational)
hullFacePlaneKey face dualPoint =
  let (dualX, dualY) = exactPointCoordinates dualPoint
      generator = liftedPointGenerator (hullFaceFirst face)
      support =
        exactPowerGeneratorXCoefficient generator * dualX
          + exactPowerGeneratorYCoefficient generator * dualY
          + exactPowerGeneratorConstant generator
   in (dualX, dualY, support)

upperFaceAssociation
  :: HullFace label
  -> Either
      (RegularTopologyError label)
      ( (ExactRational, ExactRational, ExactRational)
      , (HullFace label, ExactPoint)
      )
upperFaceAssociation face = do
  (_, dualPoint) <- hullFaceDual face
  pure (hullFacePlaneKey face dualPoint, (face, dualPoint))

coplanarLiftedTopology
  :: NonEmpty (LiftedPoint label)
  -> Either (RegularTopologyError label) (GeneratorRegularTopology label)
coplanarLiftedTopology points = do
  hull <-
    first RegularTopologySlopeHullInvalid
      (convexHullPolygon (fmap slopePoint points))
  let pointsBySlope = Map.fromList [(slopePoint point, point) | point <- NonEmpty.toList points]
  hullPoints <-
    traverse
      (\point -> maybe (Left (RegularTopologySlopeVertexMissing point)) Right (Map.lookup point pointsBySlope))
      (NonEmpty.toList (convexPolygonPoints hull))
  let visible = Set.fromList hullPoints
      lowerDimensional = Set.difference (Set.fromList (NonEmpty.toList points)) visible
      hullFaces' = coplanarFanFaces hullPoints
  dualFaces <- coplanarFanDualFaces hullFaces'
  (faces, edges) <- topologyFromHullFaces dualFaces
  pure
    (topologyFromParts points visible lowerDimensional faces edges)

coplanarFanDualFaces
  :: [HullFace label]
  -> Either
      (RegularTopologyError label)
      [(HullFace label, ExactPoint)]
coplanarFanDualFaces [] = Right []
coplanarFanDualFaces faces@(firstFace : _) = do
  (_, dualPoint) <- hullFaceDual firstFace
  pure (fmap (\face -> (face, dualPoint)) faces)

coplanarFanFaces :: [LiftedPoint label] -> [HullFace label]
coplanarFanFaces (firstPoint : secondPoint : thirdPoint : remaining) =
  zipWith
    (\identifier (leftPoint, rightPoint) ->
       HullFace identifier firstPoint leftPoint rightPoint Map.empty Set.empty)
    [0 ..]
    (consecutivePairs (secondPoint : thirdPoint : remaining))
coplanarFanFaces _ = []

collinearSlopeTopology
  :: NonEmpty (LiftedPoint label)
  -> LiftedPoint label
  -> LiftedPoint label
  -> Either (RegularTopologyError label) (GeneratorRegularTopology label)
collinearSlopeTopology points firstPoint secondPoint = do
  let direction = slopeDifference firstPoint secondPoint
      ordered = List.sortOn (slopeParameter direction) (NonEmpty.toList points)
      visibleList = upperSlopeChain direction ordered
      visible = Set.fromList visibleList
      lowerDimensional =
        Set.difference
          (upperChainPointSet direction visibleList ordered)
          visible
  edges <- traverse fullLineEdge (consecutivePairs visibleList)
  pure (topologyFromParts points visible lowerDimensional [] edges)

slopeDifference
  :: LiftedPoint label
  -> LiftedPoint label
  -> (ExactRational, ExactRational)
slopeDifference firstPoint secondPoint =
  let (firstX, firstY, _) = liftedCoordinates firstPoint
      (secondX, secondY, _) = liftedCoordinates secondPoint
   in (secondX - firstX, secondY - firstY)

slopeParameter
  :: (ExactRational, ExactRational)
  -> LiftedPoint label
  -> ExactRational
slopeParameter (directionX, directionY) point =
  let (coordinateX, coordinateY, _) = liftedCoordinates point
   in directionX * coordinateX + directionY * coordinateY

upperSlopeChain
  :: (ExactRational, ExactRational)
  -> [LiftedPoint label]
  -> [LiftedPoint label]
upperSlopeChain direction = reverse . List.foldl' insertUpper []
 where
  insertUpper
    :: [LiftedPoint label]
    -> LiftedPoint label
    -> [LiftedPoint label]
  insertUpper (secondPoint : firstPoint : remaining) candidate
    | slopeChainTurn direction firstPoint secondPoint candidate >= 0 =
        insertUpper (firstPoint : remaining) candidate
  insertUpper retained candidate = candidate : retained

slopeChainTurn
  :: (ExactRational, ExactRational)
  -> LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> ExactRational
slopeChainTurn direction firstPoint secondPoint thirdPoint =
  let firstT = slopeParameter direction firstPoint
      secondT = slopeParameter direction secondPoint
      thirdT = slopeParameter direction thirdPoint
      (_, _, firstConstant) = liftedCoordinates firstPoint
      (_, _, secondConstant) = liftedCoordinates secondPoint
      (_, _, thirdConstant) = liftedCoordinates thirdPoint
   in (secondT - firstT) * (thirdConstant - firstConstant)
        - (secondConstant - firstConstant) * (thirdT - firstT)

upperChainPointSet
  :: (ExactRational, ExactRational)
  -> [LiftedPoint label]
  -> [LiftedPoint label]
  -> Set (LiftedPoint label)
upperChainPointSet direction chain =
  snd
    . List.foldl'
        classifyPoint
        (consecutivePairs chain, Set.empty)
 where
  classifyPoint
    :: ([(LiftedPoint label, LiftedPoint label)], Set (LiftedPoint label))
    -> LiftedPoint label
    -> ([(LiftedPoint label, LiftedPoint label)], Set (LiftedPoint label))
  classifyPoint (remainingEdges, members) point =
    let pointParameter = slopeParameter direction point
        candidateEdges =
          List.dropWhile
            ((< pointParameter) . slopeParameter direction . snd)
            remainingEdges
        retainedMembers =
          case candidateEdges of
            (firstPoint, secondPoint) : _
              | slopeParameter direction firstPoint <= pointParameter
                  && slopeChainTurn direction firstPoint secondPoint point == 0 ->
                  Set.insert point members
            _ -> members
     in (candidateEdges, retainedMembers)

fullLineEdge
  :: (LiftedPoint label, LiftedPoint label)
  -> Either (RegularTopologyError label) (GeneratorRegularEdge label)
fullLineEdge (firstPoint, secondPoint) = do
  axis <- generatorAxis firstPoint secondPoint
  pure (regularEdge firstPoint secondPoint (FullLinePowerDual axis))

topologyFromHullFaces
  :: [(HullFace label, ExactPoint)]
  -> Either
      (RegularTopologyError label)
      ([GeneratorRegularFace label], [GeneratorRegularEdge label])
topologyFromHullFaces dualFaces = do
  let dualByFace = IntMap.fromList [(hullFaceId face, dual) | (face, dual) <- dualFaces]
      edgeIncidence =
        List.foldl'
          (\incidence (face, _) ->
             List.foldl'
               (\current (firstPoint, secondPoint, thirdPoint) ->
                  Map.insertWith
                    (<>)
                    (hullEdgeKey firstPoint secondPoint)
                    ((face, firstPoint, secondPoint, thirdPoint) :| [])
                    current)
               incidence
               (faceEdgesWithOpposite face))
          Map.empty
          dualFaces
  edges <- traverse (dualEdge dualByFace) (Map.elems edgeIncidence)
  pure
    ( [ GeneratorRegularFace
          (pointLabel (hullFaceFirst face))
          (pointLabel (hullFaceSecond face))
          (pointLabel (hullFaceThird face))
          dual
      | (face, dual) <- dualFaces
      ]
    , edges
    )

hullFaceDual
  :: HullFace label
  -> Either (RegularTopologyError label) (HullFace label, ExactPoint)
hullFaceDual face = do
  let firstPoint = hullFaceFirst face
      secondPoint = hullFaceSecond face
      thirdPoint = hullFaceThird face
  dual <-
    exactGeneratorFaceDual
      (liftedPointGenerator firstPoint)
      (liftedPointGenerator secondPoint)
      (liftedPointGenerator thirdPoint)
  pure (face, dual)

faceEdgesWithOpposite
  :: HullFace label
  -> [(LiftedPoint label, LiftedPoint label, LiftedPoint label)]
faceEdgesWithOpposite face =
  [ (hullFaceFirst face, hullFaceSecond face, hullFaceThird face)
  , (hullFaceSecond face, hullFaceThird face, hullFaceFirst face)
  , (hullFaceThird face, hullFaceFirst face, hullFaceSecond face)
  ]

dualEdge
  :: IntMap.IntMap ExactPoint
  -> NonEmpty (HullFace label, LiftedPoint label, LiftedPoint label, LiftedPoint label)
  -> Either (RegularTopologyError label) (GeneratorRegularEdge label)
dualEdge dualByFace incidences =
  case incidences of
    (face, firstPoint, secondPoint, thirdPoint) :| [] -> do
      dual <-
        maybe
          (Left (RegularTopologyFlatBoundary (pointLabel firstPoint) (pointLabel secondPoint) (pointLabel thirdPoint)))
          Right
          (IntMap.lookup (hullFaceId face) dualByFace)
      ray <- boundaryDualRay dual firstPoint secondPoint thirdPoint
      pure (regularEdge firstPoint secondPoint (UnboundedPowerDual ray))
    (firstFace, firstPoint, secondPoint, _) :| [(secondFace, _, _, _)] -> do
        firstDual <- requiredDual firstPoint secondPoint firstFace
        secondDual <- requiredDual firstPoint secondPoint secondFace
        dual <-
          exactGeneratorDualBetween
            (pointLabel firstPoint)
            (pointLabel secondPoint)
            firstDual
            secondDual
        pure (regularEdge firstPoint secondPoint dual)
    (_, firstPoint, secondPoint, _) :| _ ->
      Left
        ( RegularTopologyNonManifoldEdge
            (pointLabel firstPoint)
            (pointLabel secondPoint)
            (NonEmpty.length incidences)
        )
 where
  requiredDual
    :: LiftedPoint label
    -> LiftedPoint label
    -> HullFace label
    -> Either (RegularTopologyError label) ExactPoint
  requiredDual firstPoint secondPoint face =
    maybe
      (Left (RegularTopologyFlatBoundary (pointLabel firstPoint) (pointLabel secondPoint) (pointLabel (hullFaceThird face))))
      Right
      (IntMap.lookup (hullFaceId face) dualByFace)

boundaryDualRay
  :: ExactPoint
  -> LiftedPoint label
  -> LiftedPoint label
  -> LiftedPoint label
  -> Either (RegularTopologyError label) ExactRay
boundaryDualRay originPoint firstPoint secondPoint thirdPoint = do
  exactGeneratorBoundaryDualRay
    originPoint
    (liftedPointGenerator firstPoint)
    (liftedPointGenerator secondPoint)
    (liftedPointGenerator thirdPoint)

generatorAxis
  :: LiftedPoint label
  -> LiftedPoint label
  -> Either (RegularTopologyError label) ExactAffineLine
generatorAxis firstPoint secondPoint =
  first
    (RegularTopologyAxisInvalid (pointLabel firstPoint) (pointLabel secondPoint))
    (exactGeneratorAxis (liftedPointGenerator firstPoint) (liftedPointGenerator secondPoint))

regularEdge
  :: LiftedPoint label
  -> LiftedPoint label
  -> PowerDualEdge
  -> GeneratorRegularEdge label
regularEdge firstPoint secondPoint dual =
  if liftedPointId firstPoint <= liftedPointId secondPoint
    then GeneratorRegularEdge (liftedPointGenerator firstPoint) (liftedPointGenerator secondPoint) dual
    else GeneratorRegularEdge (liftedPointGenerator secondPoint) (liftedPointGenerator firstPoint) dual

pointLabel :: LiftedPoint label -> label
pointLabel = exactPowerGeneratorLabel . liftedPointGenerator

topologyFromParts
  :: NonEmpty (LiftedPoint label)
  -> Set (LiftedPoint label)
  -> Set (LiftedPoint label)
  -> [GeneratorRegularFace label]
  -> [GeneratorRegularEdge label]
  -> GeneratorRegularTopology label
topologyFromParts points visible lowerDimensional faces edges =
  let dispositions =
        fmap
          (\point ->
             ( liftedPointGenerator point
             , if Set.member point visible
                 then RegularGeneratorVisible
                 else
                   if Set.member point lowerDimensional
                     then RegularGeneratorLowerDimensional
                     else RegularGeneratorHidden
             ))
          points
      visibleCount = Set.size visible
      lowerCount = Set.size lowerDimensional
      inputCount = NonEmpty.length points
   in GeneratorRegularTopology
        { generatorRegularDispositions = dispositions
        , generatorRegularFaces = faces
        , generatorRegularEdges = edges
        , generatorRegularReceipt =
            GeneratorRegularReceipt
              { generatorRegularInputSites = inputCount
              , generatorRegularVisibleSites = visibleCount
              , generatorRegularLowerDimensionalSites = lowerCount
              , generatorRegularHiddenSites = inputCount - visibleCount - lowerCount
              , generatorRegularFaceCount = length faces
              , generatorRegularEdgeCount = length edges
              }
        }
