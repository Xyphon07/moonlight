-- | Import-graph queries over a site manifest: edges, reachable closure, and
-- import-cycle detection. Reachability and cycle reporting both run on the
-- shared dense closure kernel
-- ("Moonlight.Category.Pure.Finite.DenseReachability").
module Moonlight.Category.Pure.Site.Graph
  ( siteImportEdges,
    siteReachable,
    reachableClosure,
    importCycles,
  )
where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Moonlight.Category.Pure.Site.Core (SiteManifest (..))
import Moonlight.Category.Pure.Finite.DenseReachability
  ( denseClosureCycleComponents,
    denseClosureReachabilityRows,
    denseReachabilityWithCycles,
    objectComponentsFromIndices,
    objectIndexOf,
    objectSetFromBits,
    relationBitRows,
    relationUniverse,
  )

siteImportEdges :: Ord obj => SiteManifest obj -> Set (obj, obj)
siteImportEdges manifest =
  siteImports manifest
    & Map.foldMapWithKey
      (\targetObject sources -> Set.map (\sourceObject -> (targetObject, sourceObject)) sources)

siteReachable :: Ord obj => SiteManifest obj -> obj -> Set obj
siteReachable manifest start =
  Map.findWithDefault Set.empty start (reachableClosure (siteImports manifest))

reachableClosure :: Ord obj => Map obj (Set obj) -> Map obj (Set obj)
reachableClosure adjacency =
  let objectVector = Vector.fromList (Set.toAscList (relationUniverse adjacency))
      objectIndex = objectIndexOf objectVector
      closureRows =
        denseClosureReachabilityRows
          (denseReachabilityWithCycles (relationBitRows objectIndex objectVector adjacency))
      reachableSet objectValue =
        maybe
          Set.empty
          (objectSetFromBits objectVector)
          (Map.lookup objectValue objectIndex >>= (closureRows Vector.!?))
   in Map.mapWithKey (\objectValue _ -> reachableSet objectValue) adjacency

importCycles :: Ord obj => SiteManifest obj -> [NonEmpty obj]
importCycles manifest =
  let objectVector = Vector.fromList (Set.toAscList (siteObjects manifest))
      objectIndex = objectIndexOf objectVector
      closure =
        denseReachabilityWithCycles
          (relationBitRows objectIndex objectVector (siteImports manifest))
   in objectComponentsFromIndices objectVector (denseClosureCycleComponents closure)
