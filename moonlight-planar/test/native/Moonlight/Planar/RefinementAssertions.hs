-- | Closure-receipt law shared by local refinement and constrained seams.
module Moonlight.Planar.RefinementAssertions
  ( assertClosureCountsAgree
  ) where

import Moonlight.Planar.Internal.HandleDefs ( FaceId, faceIdIndex, undirectedEdgeIdIndex )
import Moonlight.Planar.Internal.Validation ( topologyClosureSelectionStats )
import Moonlight.Planar.Types ( RefinementDomainResult(..), RefinementReceipt(..),
  RefinementResult(..) )
import Support ( assertEqual )
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Vector as V


-- | The receipt's closure counts, taken without materializing the closure,
-- equal the sizes of the closure selection built from the same initial and
-- final permitted faces and the interface pairs the receipt reports.
assertClosureCountsAgree
  :: String
  -> Set.Set FaceId
  -> RefinementDomainResult mode vertex directed undirected face
  -> IO ()
assertClosureCountsAgree label initialPermitted refined =
  assertEqual
    (label <> ": closure counts equal the closure selection's sizes")
    (topologyClosureSelectionStats (IntSet.union initialFaces finalFaces) interfacePairs after)
    (refinementClosureStats receipt)
 where
  receipt = refinementDomainReceipt refined
  after = refinedTriangulation (refinementDomainResult refined)
  initialFaces = IntSet.fromList (fmap faceIdIndex (Set.toList initialPermitted))
  finalFaces = IntSet.fromList (fmap faceIdIndex (V.toList (refinementFinalPermittedFaces receipt)))
  interfacePairs =
    IntSet.fromList
      [ undirectedEdgeIdIndex edge
      | (edge, _, _) <- V.toList (refinementFinalInterfaceIncidence receipt)
      ]
