-- | The closed comparison vocabulary and its boundary projections.
--
-- The driver executes these requests, the report orders and names them, and
-- the Haskell runner interprets their command constructors. None of those
-- boundaries authors another lane universe.
module Moonlight.Triangulation.Bench.SpadeCompare.Lane
  ( LaneClass (..)
  , LaneKind (..)
  , LaneRequest (..)
  , LaneObstruction (..)
  , laneKindName
  , laneRequestLabel
  , parseLaneRequest
  , renderInventoryCsv
  , renderInventoryHuman
  , renderLaneSpecs
  , renderLaneObstruction
  , renderSnapshotSpecs
  ) where

import Data.List (find, intercalate)
import Text.Read (readMaybe)

data LaneClass
  = ParityLane
  | CliffLane
  deriving stock (Eq, Show)

-- | Every benchmark command understood by the Moonlight runner. The board is
-- a closed subset of this vocabulary; the remaining constructors are focused
-- diagnostics rather than Spade comparison rows.
data LaneKind
  = BulkLoadLane
  | IncrementalLane
  | SnapshotInsertLane
  | SweepAngleCollapseLane
  | DegenerateLineLane
  | BatchSweepLane
  | NearestLane
  | CdtRecoveryLane
  | ConstraintIncrementalLane
  | ConstraintSplitLane
  | PublicationFloorLane
  | RemovalLane
  | SnapshotRemovalLane
  | HierarchyIncrementalLane
  | HierarchyDuplicateLane
  | HierarchyRemovalLane
  | HierarchyRemovalOnlyLane
  | HierarchyRebuildOnlyLane
  | InterpolationLane
  | VoronoiSweepLane
  | DcelWalkLane
  | AlphaPersistenceLane
  | IntersectionLane
  | IntersectionOutsideLane
  | RefineLane
  deriving stock (Bounded, Enum, Eq, Show)

data LaneRequest = LaneRequest
  { laneRequestKind :: !LaneKind
  , laneRequestFirst :: !Int
  , laneRequestSecond :: !Int
  }
  deriving stock (Eq, Show)

data LaneObstruction
  = UnknownLane !String
  | UnknownLaneClass !String
  | MalformedWorkSize !String
  | NegativeWorkSize !Int
  | AlphaPersistenceRequiresSpecializedRunner
  deriving stock (Eq, Show)

data LaneSpec = LaneSpec
  { laneSpecClass :: !LaneClass
  , laneSpecRequest :: !LaneRequest
  , laneSpecDisplay :: !String
  }

data SnapshotStressSpec = SnapshotStressSpec
  { snapshotSessionRequest :: !LaneRequest
  , snapshotPublicationRequest :: !LaneRequest
  , snapshotDisplay :: !String
  }

laneKindName :: LaneKind -> String
laneKindName = \case
  BulkLoadLane -> "bulk-load"
  IncrementalLane -> "incremental"
  SnapshotInsertLane -> "snapshot-insert"
  SweepAngleCollapseLane -> "sweep-angle-collapse"
  DegenerateLineLane -> "degenerate-line"
  BatchSweepLane -> "batch-sweep"
  NearestLane -> "nearest"
  CdtRecoveryLane -> "cdt-recovery"
  ConstraintIncrementalLane -> "constraint-incremental"
  ConstraintSplitLane -> "constraint-split"
  PublicationFloorLane -> "publication-floor"
  RemovalLane -> "removal"
  SnapshotRemovalLane -> "snapshot-removal"
  HierarchyIncrementalLane -> "hierarchy-incremental"
  HierarchyDuplicateLane -> "hierarchy-duplicate"
  HierarchyRemovalLane -> "hierarchy-removal"
  HierarchyRemovalOnlyLane -> "hierarchy-removal-only"
  HierarchyRebuildOnlyLane -> "hierarchy-rebuild-only"
  InterpolationLane -> "interpolation"
  VoronoiSweepLane -> "voronoi-sweep"
  DcelWalkLane -> "dcel-walk"
  AlphaPersistenceLane -> "alpha-persistence"
  IntersectionLane -> "intersection"
  IntersectionOutsideLane -> "intersection-outside"
  RefineLane -> "refine"

laneRequestLabel :: LaneRequest -> String
laneRequestLabel request =
  intercalate
    "-"
    [ laneKindName (laneRequestKind request)
    , show (laneRequestFirst request)
    , show (laneRequestSecond request)
    ]

parseLaneRequest :: String -> String -> String -> Either LaneObstruction LaneRequest
parseLaneRequest rawKind rawFirst rawSecond =
  LaneRequest
    <$> parseLaneKind rawKind
    <*> parseWorkSize rawFirst
    <*> parseWorkSize rawSecond

renderLaneObstruction :: LaneObstruction -> String
renderLaneObstruction = \case
  UnknownLane lane -> "unknown benchmark lane: " <> lane
  UnknownLaneClass laneClass -> "unknown benchmark lane class: " <> laneClass
  MalformedWorkSize raw -> "benchmark work size is not an integer: " <> raw
  NegativeWorkSize size -> "benchmark work size is negative: " <> show size
  AlphaPersistenceRequiresSpecializedRunner ->
    "alpha-persistence is owned by moonlight-triangulation-alpha-spade-referent"

renderInventoryCsv :: String
renderInventoryCsv =
  unlines
    ( fmap renderBoardLane boardLaneSpecs
        <> fmap renderSnapshotLane snapshotStressSpecs
    )
 where
  renderBoardLane specification =
    csvRow
      [ "lane"
      , laneClassName (laneSpecClass specification)
      , laneRequestLabel (laneSpecRequest specification)
      , laneSpecDisplay specification
      ]

  renderSnapshotLane specification =
    csvRow
      [ "lane"
      , "snapshot"
      , laneRequestLabel (snapshotPublicationRequest specification)
      , snapshotDisplay specification
      , laneRequestLabel (snapshotSessionRequest specification)
      ]

renderInventoryHuman :: String
renderInventoryHuman =
  concatMap renderClass [ParityLane, CliffLane]
    <> "snapshot lanes ("
    <> show (length snapshotStressSpecs)
    <> ")\n"
    <> concatMap renderSnapshot snapshotStressSpecs
 where
  renderClass laneClass =
    let specifications = laneSpecs laneClass
     in laneClassName laneClass
          <> " lanes ("
          <> show (length specifications)
          <> ")\n"
          <> concatMap
            ((<> "\n") . ("  " <>) . laneRequestLabel . laneSpecRequest)
            specifications

  renderSnapshot specification =
    "  "
      <> laneRequestLabel (snapshotPublicationRequest specification)
      <> " against "
      <> laneRequestLabel (snapshotSessionRequest specification)
      <> "\n"

renderLaneSpecs :: String -> Either LaneObstruction String
renderLaneSpecs rawClass =
  unlines . fmap (renderRequestWords . laneSpecRequest) . laneSpecs
    <$> parseLaneClass rawClass

renderSnapshotSpecs :: String
renderSnapshotSpecs =
  unlines
    ( fmap
        ( \specification ->
            renderRequestWords (snapshotSessionRequest specification)
              <> " "
              <> renderRequestWords (snapshotPublicationRequest specification)
        )
        snapshotStressSpecs
    )

parseLaneKind :: String -> Either LaneObstruction LaneKind
parseLaneKind raw =
  maybe
    (Left (UnknownLane raw))
    Right
    (find ((== raw) . laneKindName) [minBound .. maxBound])

parseLaneClass :: String -> Either LaneObstruction LaneClass
parseLaneClass = \case
  "parity" -> Right ParityLane
  "cliff" -> Right CliffLane
  other -> Left (UnknownLaneClass other)

parseWorkSize :: String -> Either LaneObstruction Int
parseWorkSize raw =
  maybe
    (Left (MalformedWorkSize raw))
    (\size -> if size < 0 then Left (NegativeWorkSize size) else Right size)
    (readMaybe raw)

laneClassName :: LaneClass -> String
laneClassName = \case
  ParityLane -> "parity"
  CliffLane -> "cliff"

laneSpecs :: LaneClass -> [LaneSpec]
laneSpecs laneClass = filter ((== laneClass) . laneSpecClass) boardLaneSpecs

renderRequestWords :: LaneRequest -> String
renderRequestWords request =
  unwords
    [ laneKindName (laneRequestKind request)
    , show (laneRequestFirst request)
    , show (laneRequestSecond request)
    ]

csvRow :: [String] -> String
csvRow = intercalate "," . fmap csvField

csvField :: String -> String
csvField field
  | any (`elem` [',', '"', '\n', '\r']) field =
      '"' : concatMap escapeQuote field <> "\""
  | otherwise = field
 where
  escapeQuote '"' = "\"\""
  escapeQuote character = [character]

boardLaneSpecs :: [LaneSpec]
boardLaneSpecs =
  [ parity BulkLoadLane 1000 0 "Bulk load, 1k points"
  , parity BulkLoadLane 10000 0 "Bulk load, 10k"
  , parity IncrementalLane 1000 0 "Incremental insert, 1k"
  , parity IncrementalLane 10000 0 "Incremental insert, 10k"
  , parity IncrementalLane 500000 0 "Incremental insert, 500k"
  , parity NearestLane 20000 5000 "Nearest, 5k queries / 20k points"
  , parity CdtRecoveryLane 8000 800 "CDT recovery, 800 / 8k"
  , parity RefineLane 625 0 "Refinement, 625 Steiner"
  , parity RefineLane 2500 0 "Refinement, 2,500 Steiner"
  , parity RemovalLane 1000 250 "Removal, 250 / 1k"
  , parity RemovalLane 10000 2500 "Removal, 2.5k / 10k"
  , parity RemovalLane 100000 25000 "Removal, 25k / 100k"
  , parity InterpolationLane 10000 1000 "Interpolation, 1k queries / 10k"
  , parity InterpolationLane 100000 2000 "Interpolation, 2k queries / 100k"
  , parity VoronoiSweepLane 1000 0 "Voronoi dual sweep, 1k sites"
  , parity DcelWalkLane 2000 0 "DCEL traversal, 2k points"
  , parity IntersectionLane 10000 500 "Line intersection, 500 chords / 10k"
  , parity DegenerateLineLane 2000 0 "Exactly collinear bulk chain, 2k"
  , cliff ConstraintIncrementalLane 8000 800 "Constraint one at a time, 800 / 8k"
  , cliff ConstraintSplitLane 1000 0 "Constraint split, 1k crossings"
  -- This deliberately stays at 1k while its parity twin also reaches 10k.
  , cliff HierarchyIncrementalLane 1000 0 "Hierarchy insert, 1k"
  , cliff HierarchyDuplicateLane 10000 500 "Hierarchy duplicate insert, 500 / 10k"
  , cliff HierarchyRemovalLane 10000 250 "Hierarchy removal, 250 / 10k"
  , cliff SweepAngleCollapseLane 2000 0 "Sweep hull index at one angle, 2k"
  , cliff IntersectionOutsideLane 2000 100 "Line intersection from outside the hull, 100 / 2k"
  ]
 where
  parity :: LaneKind -> Int -> Int -> String -> LaneSpec
  parity kind first second display =
    LaneSpec ParityLane (LaneRequest kind first second) display

  cliff :: LaneKind -> Int -> Int -> String -> LaneSpec
  cliff kind first second display =
    LaneSpec CliffLane (LaneRequest kind first second) display

snapshotStressSpecs :: [SnapshotStressSpec]
snapshotStressSpecs =
  [ SnapshotStressSpec
      (LaneRequest IncrementalLane 1000 0)
      (LaneRequest SnapshotInsertLane 1000 0)
      "Insert snapshots, 1k points"
  , SnapshotStressSpec
      (LaneRequest RemovalLane 10000 2500)
      (LaneRequest SnapshotRemovalLane 10000 2500)
      "Removal snapshots, 2.5k / 10k"
  ]
