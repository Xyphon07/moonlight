module BasisTests
  ( tests,
  )
where

import qualified BoundarySpec as BoundarySpec
import qualified CardinalitySpec as CardinalitySpec
import qualified DenseKeySpec as DenseKeySpec
import qualified FiniteSpec as FiniteSpec
import qualified IsoNormSpec as IsoNormSpec
import qualified MapAccumSpec as MapAccumSpec
import qualified OrdCollectionSpec as OrdCollectionSpec
import qualified OrderSpec as OrderSpec
import qualified ProofManifestSpec as ProofManifestSpec
import qualified QueueSpec as QueueSpec
import qualified RelationalSpec as RelationalSpec
import qualified StableHashSpec as StableHashSpec
import qualified TotalRegistrySpec as TotalRegistrySpec
import qualified TypeLevelSpec as TypeLevelSpec
import qualified ValidationSpec as ValidationSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "moonlight-core-basis"
    [ BoundarySpec.tests,
      CardinalitySpec.tests,
      DenseKeySpec.tests,
      FiniteSpec.tests,
      IsoNormSpec.tests,
      MapAccumSpec.tests,
      OrdCollectionSpec.tests,
      OrderSpec.tests,
      ProofManifestSpec.tests,
      QueueSpec.tests,
      RelationalSpec.tests,
      StableHashSpec.tests,
      TotalRegistrySpec.tests,
      TypeLevelSpec.tests,
      ValidationSpec.tests
    ]
