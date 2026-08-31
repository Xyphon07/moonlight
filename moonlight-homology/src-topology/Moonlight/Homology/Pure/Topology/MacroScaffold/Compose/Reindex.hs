module Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Reindex
  ( traverseShiftedScaffolds,
    scaffoldBasisRefs,
    reebArcCardinality,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Moonlight.Homology.Pure.Chain
  ( HomologicalDegree,
    RepresentativeChain (..),
  )
import Moonlight.Homology.Pure.Carrier
  ( BasisCellRef (..),
    CellCarrier,
    CellCarrierError,
    carrierCells,
    carrierDegree,
    mkCellCarrier,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold
  ( MacroScaffoldIR (..),
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Core
  ( MacroScaffoldCompositionError (..),
    ScaffoldOffsets (..),
    zeroScaffoldOffsets,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Direction
  ( DirectionField,
    DirectionFieldEncoding (..),
    directionFieldCarrier,
    directionFieldEncoding,
    directionFieldSymmetryOrder,
    mkDirectionField,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.HarmonicLoop
  ( HarmonicLoop (..),
    HarmonicLoopId (..),
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Potential
  ( ScalarPotentialField,
    mkScalarPotentialField,
    scalarPotentialCarrier,
    scalarPotentialNormalization,
    scalarPotentialSamples,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Reeb
  ( MorseReebArc (..),
    MorseReebNode (..),
    MorseReebScaffold (..),
    ReebArcId (..),
    ReebNodeId (..),
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Singularity
  ( Singularity (..),
    SingularityId (..),
  )

traverseShiftedScaffolds ::
  NonEmpty (label, MacroScaffoldIR) ->
  Either MacroScaffoldCompositionError (NonEmpty (label, MacroScaffoldIR))
traverseShiftedScaffolds ((firstLabel, firstScaffold) :| remainingScaffolds) = do
  shiftedFirst <- reindexScaffold zeroScaffoldOffsets firstScaffold
  (_, shiftedRest) <-
    foldM
      reindexStep
      (advanceOffsets zeroScaffoldOffsets firstScaffold, [])
      remainingScaffolds
  pure ((firstLabel, shiftedFirst) :| shiftedRest)
  where
    reindexStep ::
      (ScaffoldOffsets, [(label, MacroScaffoldIR)]) ->
      (label, MacroScaffoldIR) ->
      Either MacroScaffoldCompositionError (ScaffoldOffsets, [(label, MacroScaffoldIR)])
    reindexStep (offsets, shiftedScaffolds) (labelValue, scaffoldValue) = do
      shiftedScaffold <- reindexScaffold offsets scaffoldValue
      pure
        ( advanceOffsets offsets scaffoldValue,
          shiftedScaffolds <> [(labelValue, shiftedScaffold)]
        )

scaffoldBasisRefs :: MacroScaffoldIR -> [BasisCellRef]
scaffoldBasisRefs scaffoldValue =
  carrierCells (scalarPotentialCarrier (macroScaffoldScalarPotential scaffoldValue))
    <> Map.keys (scalarPotentialSamples (macroScaffoldScalarPotential scaffoldValue))
    <> carrierCells (directionFieldCarrier (macroScaffoldDirectionField scaffoldValue))
    <> directionEncodingBasisRefs (directionFieldEncoding (macroScaffoldDirectionField scaffoldValue))
    <> fmap morseReebNodeAnchor (morseReebNodes (macroScaffoldReeb scaffoldValue))
    <> (morseReebArcs (macroScaffoldReeb scaffoldValue) >>= morseReebArcSupport)
    <> fmap singularityAnchor (macroScaffoldSingularities scaffoldValue)
    <> (macroScaffoldHarmonicLoops scaffoldValue >>= representativeBasisRefs . harmonicLoopCycle)
    <> (macroScaffoldHarmonicLoops scaffoldValue >>= representativeBasisRefs . harmonicLoopCocycle)

reebArcCardinality :: MacroScaffoldIR -> Int
reebArcCardinality s = maxIdCardinality (unReebArcId . morseReebArcId) (morseReebArcs (macroScaffoldReeb s))

maxIdCardinality :: (a -> Int) -> [a] -> Int
maxIdCardinality extractId = foldr (\v acc -> max acc (extractId v + 1)) 0

reebNodeCardinality :: MacroScaffoldIR -> Int
reebNodeCardinality s = maxIdCardinality (unReebNodeId . morseReebNodeId) (morseReebNodes (macroScaffoldReeb s))

singularityCardinality :: MacroScaffoldIR -> Int
singularityCardinality s = maxIdCardinality (unSingularityId . singularityId) (macroScaffoldSingularities s)

harmonicLoopCardinality :: MacroScaffoldIR -> Int
harmonicLoopCardinality s = maxIdCardinality (unHarmonicLoopId . harmonicLoopId) (macroScaffoldHarmonicLoops s)

reindexScaffold :: ScaffoldOffsets -> MacroScaffoldIR -> Either MacroScaffoldCompositionError MacroScaffoldIR
reindexScaffold offsets scaffoldValue = do
  scalarPotential <- reindexScalarPotential offsets (macroScaffoldScalarPotential scaffoldValue)
  directionField <- reindexDirectionField offsets (macroScaffoldDirectionField scaffoldValue)
  pure
    MacroScaffoldIR
      { macroScaffoldScalarPotential = scalarPotential,
        macroScaffoldReeb = reindexReebScaffold offsets (macroScaffoldReeb scaffoldValue),
        macroScaffoldDirectionField = directionField,
        macroScaffoldSingularities = fmap (reindexSingularity offsets) (macroScaffoldSingularities scaffoldValue),
        macroScaffoldHarmonicLoops = fmap (reindexHarmonicLoop offsets) (macroScaffoldHarmonicLoops scaffoldValue)
      }

reindexScalarPotential :: ScaffoldOffsets -> ScalarPotentialField -> Either MacroScaffoldCompositionError ScalarPotentialField
reindexScalarPotential offsets scalarPotential = do
  carrierValue <- reindexCarrier InvalidComposedScalarPotentialCarrier offsets (scalarPotentialCarrier scalarPotential)
  first
    InvalidComposedScalarPotential
    ( mkScalarPotentialField
        carrierValue
        (scalarPotentialNormalization scalarPotential)
        (Map.fromAscList (reindexAssociation offsets <$> Map.toAscList (scalarPotentialSamples scalarPotential)))
    )

reindexDirectionField :: ScaffoldOffsets -> DirectionField -> Either MacroScaffoldCompositionError DirectionField
reindexDirectionField offsets directionField = do
  carrierValue <- reindexCarrier InvalidComposedDirectionCarrier offsets (directionFieldCarrier directionField)
  first
    InvalidComposedDirectionField
    ( mkDirectionField
        carrierValue
        (directionFieldSymmetryOrder directionField)
        (reindexDirectionEncoding offsets (directionFieldEncoding directionField))
    )

reindexCarrier ::
  (CellCarrierError -> MacroScaffoldCompositionError) ->
  ScaffoldOffsets ->
  CellCarrier ->
  Either MacroScaffoldCompositionError CellCarrier
reindexCarrier liftError offsets carrierValue =
  first
    liftError
    (mkCellCarrier (carrierDegree carrierValue) (reindexBasisCellRef offsets <$> carrierCells carrierValue))

reindexAssociation :: ScaffoldOffsets -> (BasisCellRef, value) -> (BasisCellRef, value)
reindexAssociation offsets (basisCellRef, value) =
  (reindexBasisCellRef offsets basisCellRef, value)

reindexDirectionEncoding :: ScaffoldOffsets -> DirectionFieldEncoding -> DirectionFieldEncoding
reindexDirectionEncoding offsets encodingValue =
  case encodingValue of
    DirectionAngleEncoding phaseMap ->
      DirectionAngleEncoding (Map.fromAscList (reindexAssociation offsets <$> Map.toAscList phaseMap))
    DirectionCochainEncoding coefficientMap ->
      DirectionCochainEncoding (Map.fromAscList (reindexAssociation offsets <$> Map.toAscList coefficientMap))

reindexReebScaffold :: ScaffoldOffsets -> MorseReebScaffold -> MorseReebScaffold
reindexReebScaffold offsets reebValue =
  MorseReebScaffold
    { morseReebNodes = reindexReebNode offsets <$> morseReebNodes reebValue,
      morseReebArcs = reindexReebArc offsets <$> morseReebArcs reebValue
    }

reindexReebNode :: ScaffoldOffsets -> MorseReebNode -> MorseReebNode
reindexReebNode offsets nodeValue =
  nodeValue
    { morseReebNodeId = offsetReebNodeId (soNodeOffset offsets) (morseReebNodeId nodeValue),
      morseReebNodeAnchor = reindexBasisCellRef offsets (morseReebNodeAnchor nodeValue)
    }

reindexReebArc :: ScaffoldOffsets -> MorseReebArc -> MorseReebArc
reindexReebArc offsets arcValue =
  arcValue
    { morseReebArcId = offsetReebArcId (soArcOffset offsets) (morseReebArcId arcValue),
      morseReebArcSource = offsetReebNodeId (soNodeOffset offsets) (morseReebArcSource arcValue),
      morseReebArcTarget = offsetReebNodeId (soNodeOffset offsets) (morseReebArcTarget arcValue),
      morseReebArcSupport = reindexBasisCellRef offsets <$> morseReebArcSupport arcValue
    }

reindexSingularity :: ScaffoldOffsets -> Singularity -> Singularity
reindexSingularity offsets singularityValue =
  singularityValue
    { singularityId = offsetSingularityId (soSingularityOffset offsets) (singularityId singularityValue),
      singularityAnchor = reindexBasisCellRef offsets (singularityAnchor singularityValue),
      singularityReebNode = offsetReebNodeId (soNodeOffset offsets) <$> singularityReebNode singularityValue,
      singularityIncidentArcs = offsetReebArcId (soArcOffset offsets) <$> singularityIncidentArcs singularityValue
    }

reindexHarmonicLoop :: ScaffoldOffsets -> HarmonicLoop -> HarmonicLoop
reindexHarmonicLoop offsets harmonicLoopValue =
  harmonicLoopValue
    { harmonicLoopId = offsetHarmonicLoopId (soLoopOffset offsets) (harmonicLoopId harmonicLoopValue),
      harmonicLoopCycle = reindexRepresentative offsets (harmonicLoopCycle harmonicLoopValue),
      harmonicLoopCocycle = reindexRepresentative offsets (harmonicLoopCocycle harmonicLoopValue),
      harmonicLoopSupport = offsetReebArcId (soArcOffset offsets) <$> harmonicLoopSupport harmonicLoopValue
    }

reindexRepresentative ::
  ScaffoldOffsets ->
  RepresentativeChain coefficient BasisCellRef ->
  RepresentativeChain coefficient BasisCellRef
reindexRepresentative offsets representativeValue =
  representativeValue
    { representativeTerms =
        fmap
          (\(coefficientValue, basisCellRef) -> (coefficientValue, reindexBasisCellRef offsets basisCellRef))
          (representativeTerms representativeValue)
    }

reindexBasisCellRef :: ScaffoldOffsets -> BasisCellRef -> BasisCellRef
reindexBasisCellRef offsets basisCellRef =
  let degreeValue = cellDegree basisCellRef
      indexOffset = Map.findWithDefault 0 degreeValue (soBasisOffsets offsets)
   in basisCellRef
        { cellIndex = cellIndex basisCellRef + indexOffset
        }

advanceOffsets :: ScaffoldOffsets -> MacroScaffoldIR -> ScaffoldOffsets
advanceOffsets offsets scaffoldValue =
  offsets
    { soBasisOffsets = Map.unionWith (+) (soBasisOffsets offsets) (basisCardinalities scaffoldValue),
      soNodeOffset = soNodeOffset offsets + reebNodeCardinality scaffoldValue,
      soArcOffset = soArcOffset offsets + reebArcCardinality scaffoldValue,
      soSingularityOffset = soSingularityOffset offsets + singularityCardinality scaffoldValue,
      soLoopOffset = soLoopOffset offsets + harmonicLoopCardinality scaffoldValue
    }

basisCardinalities :: MacroScaffoldIR -> Map HomologicalDegree Int
basisCardinalities scaffoldValue =
  Map.fromListWith max
    ( fmap
        (\basisCellRef -> (cellDegree basisCellRef, cellIndex basisCellRef + 1))
        (scaffoldBasisRefs scaffoldValue)
    )

directionEncodingBasisRefs :: DirectionFieldEncoding -> [BasisCellRef]
directionEncodingBasisRefs encodingValue =
  case encodingValue of
    DirectionAngleEncoding phaseMap -> Map.keys phaseMap
    DirectionCochainEncoding coefficientMap -> Map.keys coefficientMap

representativeBasisRefs :: RepresentativeChain coefficient BasisCellRef -> [BasisCellRef]
representativeBasisRefs = fmap snd . representativeTerms

offsetReebNodeId :: Int -> ReebNodeId -> ReebNodeId
offsetReebNodeId nodeOffset (ReebNodeId nodeIdValue) =
  ReebNodeId (nodeIdValue + nodeOffset)

offsetReebArcId :: Int -> ReebArcId -> ReebArcId
offsetReebArcId arcOffset (ReebArcId arcIdValue) =
  ReebArcId (arcIdValue + arcOffset)

offsetSingularityId :: Int -> SingularityId -> SingularityId
offsetSingularityId singularityOffset (SingularityId singularityIdValue) =
  SingularityId (singularityIdValue + singularityOffset)

offsetHarmonicLoopId :: Int -> HarmonicLoopId -> HarmonicLoopId
offsetHarmonicLoopId loopOffset (HarmonicLoopId loopIdValue) =
  HarmonicLoopId (loopIdValue + loopOffset)
