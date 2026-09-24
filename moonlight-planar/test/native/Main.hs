module Main (main) where

import qualified Moonlight.Planar.ConstructionSpec as ConstructionSpec
import qualified Moonlight.Planar.ValidationSpec as ValidationSpec
import qualified Moonlight.Planar.PredicateSpec as PredicateSpec
import qualified Moonlight.Planar.NearestSpec as NearestSpec
import qualified Moonlight.Planar.LocationSpec as LocationSpec
import qualified Moonlight.Planar.EditSpec as EditSpec
import qualified Moonlight.Planar.DcelSpec as DcelSpec
import qualified Moonlight.Planar.PayloadSpec as PayloadSpec
import qualified Moonlight.Planar.ConstraintSpec as ConstraintSpec
import qualified Moonlight.Planar.ConstrainedJoinSpec as ConstrainedJoinSpec
import qualified Moonlight.Planar.ConstrainedSeamSpec as ConstrainedSeamSpec
import qualified Moonlight.Planar.RefinementSpec as RefinementSpec
import qualified Moonlight.Planar.AlphaSpec as AlphaSpec
import qualified Moonlight.Planar.IncidenceSpec as IncidenceSpec
import qualified Moonlight.Planar.ExactEmbeddingSpec as ExactEmbeddingSpec
import qualified Moonlight.Planar.ExactSegmentEventsSpec as ExactSegmentEventsSpec
import qualified Moonlight.Planar.OverlaySpec as OverlaySpec
import qualified Moonlight.Planar.OverlaySelectionSpec as OverlaySelectionSpec
import qualified Moonlight.Planar.OverlayPublicationSpec as OverlayPublicationSpec
import qualified Moonlight.Planar.RegionSpec as RegionSpec
import Moonlight.Planar.FilteredPredicateOptimizationSpec (assertFilteredPredicatesSkipExactOracle)

main :: IO ()
main =
  sequence_
    [ ConstructionSpec.tests
    , ValidationSpec.tests
    , PredicateSpec.tests
    , NearestSpec.tests
    , LocationSpec.tests
    , EditSpec.tests
    , DcelSpec.tests
    , PayloadSpec.tests
    , ConstraintSpec.tests
    , ConstrainedJoinSpec.tests
    , ConstrainedSeamSpec.tests
    , RefinementSpec.tests
    , AlphaSpec.tests
    , assertFilteredPredicatesSkipExactOracle
    , IncidenceSpec.tests
    , ExactEmbeddingSpec.tests
    , ExactSegmentEventsSpec.tests
    , OverlaySpec.tests
    , OverlaySelectionSpec.tests
    , OverlayPublicationSpec.tests
    , RegionSpec.tests
    , putStrLn "all native core tests passed"
    ]
