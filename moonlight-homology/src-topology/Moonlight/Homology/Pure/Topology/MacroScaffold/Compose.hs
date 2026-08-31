module Moonlight.Homology.Pure.Topology.MacroScaffold.Compose
  ( MacroScaffoldCompositionError (..),
    StitchBoundarySide (..),
    StitchSupportSelection (..),
    StitchSupportRefinement (..),
    StitchSemantics (..),
    StitchRoute (..),
    StitchRouteKey (..),
    MacroScaffoldStitchError (..),
    composeMacroScaffoldsWithScopes,
    composeMacroScaffolds,
    stitchMacroScaffoldRoutes,
  )
where

import Control.Monad (foldM)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Homology.Pure.Carrier
  ( BasisCellRef,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold
  ( MacroScaffoldIR (..),
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Core
  ( MacroScaffoldCompositionError (..),
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Merge
  ( mergeScalarPotentialFields,
    mergeDirectionFields,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Reindex
  ( reebArcCardinality,
    scaffoldBasisRefs,
    traverseShiftedScaffolds,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Stitch
  ( MacroScaffoldStitchError (..),
    StitchBoundarySide (..),
    StitchRoute (..),
    StitchRouteKey (..),
    StitchSemantics (..),
    StitchSupportRefinement (..),
    StitchSupportSelection (..),
    stitchRoutePair,
    uniqueRoutePairs,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Reeb
  ( MorseReebScaffold (..),
  )

composeMacroScaffolds :: NonEmpty MacroScaffoldIR -> Either MacroScaffoldCompositionError MacroScaffoldIR
composeMacroScaffolds scaffolds =
  fmap fst
    ( composeMacroScaffoldsWithScopes
        (fmap (\scaffoldValue -> ((), scaffoldValue)) scaffolds)
    )

composeMacroScaffoldsWithScopes ::
  Ord label =>
  NonEmpty (label, MacroScaffoldIR) ->
  Either MacroScaffoldCompositionError (MacroScaffoldIR, Map label (Set BasisCellRef))
composeMacroScaffoldsWithScopes labeledScaffolds = do
  shiftedScaffolds <- traverseShiftedScaffolds labeledScaffolds
  scalarPotential <-
    mergeScalarPotentialFields
      (macroScaffoldScalarPotential . snd <$> shiftedScaffolds)
  directionField <-
    mergeDirectionFields
      (macroScaffoldDirectionField . snd <$> shiftedScaffolds)
  pure
    ( MacroScaffoldIR
        { macroScaffoldScalarPotential = scalarPotential,
          macroScaffoldReeb =
            MorseReebScaffold
              { morseReebNodes = NonEmpty.toList shiftedScaffolds >>= (morseReebNodes . macroScaffoldReeb . snd),
                morseReebArcs = NonEmpty.toList shiftedScaffolds >>= (morseReebArcs . macroScaffoldReeb . snd)
              },
          macroScaffoldDirectionField = directionField,
          macroScaffoldSingularities = NonEmpty.toList shiftedScaffolds >>= (macroScaffoldSingularities . snd),
          macroScaffoldHarmonicLoops = NonEmpty.toList shiftedScaffolds >>= (macroScaffoldHarmonicLoops . snd)
        },
      regionScopes shiftedScaffolds
    )

stitchMacroScaffoldRoutes ::
  (Ord route, Ord label) =>
  (route -> StitchSemantics) ->
  Map label (Set BasisCellRef) ->
  [StitchRoute route label] ->
  MacroScaffoldIR ->
  Either (MacroScaffoldStitchError label) (MacroScaffoldIR, Map (StitchRouteKey route label) (Set BasisCellRef))
stitchMacroScaffoldRoutes semanticsFor regionScopeMap routes scaffoldValue =
  let stitchedPairs = uniqueRoutePairs routes
      initialArcId = reebArcCardinality scaffoldValue
   in fmap
        (\(_, stitchedArcs, stitchScopes) ->
           ( scaffoldValue
               { macroScaffoldReeb =
                   (macroScaffoldReeb scaffoldValue)
                     { morseReebArcs =
                         morseReebArcs (macroScaffoldReeb scaffoldValue)
                           <> reverse stitchedArcs
                     }
               },
             stitchScopes
           )
        )
        ( foldM
            (stitchRoutePair semanticsFor regionScopeMap scaffoldValue)
            (initialArcId, [], Map.empty)
            stitchedPairs
        )

regionScopes :: Ord label => NonEmpty (label, MacroScaffoldIR) -> Map label (Set BasisCellRef)
regionScopes shiftedScaffolds =
  shiftedScaffolds
    & NonEmpty.toList
    & fmap (\(labelValue, scaffoldValue) -> (labelValue, Set.fromList (scaffoldBasisRefs scaffoldValue)))
    & Map.fromListWith Set.union
