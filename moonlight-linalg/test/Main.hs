module Main (main) where

import qualified AdvancedSpec as AdvancedSpec
import qualified ArchitectureSpec as ArchitectureSpec
import qualified BasicSpec as BasicSpec
import qualified BlockSpec as BlockSpec
import qualified DenseFlatSpec as DenseFlatSpec
import qualified DenseRowsSpec as DenseRowsSpec
import qualified KrylovSpec as KrylovSpec
import qualified DomainSpec as DomainSpec
import qualified DynamicSpec as DynamicSpec
import qualified FieldSpec as FieldSpec
import qualified ExteriorSpec as ExteriorSpec
import qualified GF2Spec as GF2Spec
import qualified GeometryStorageSpec as GeometryStorageSpec
import qualified SymmetricSpec as SymmetricSpec
import qualified StaticsSpec as StaticsSpec
import qualified SparseSolverSpec as SparseSolverSpec
import qualified SparsePackedSpec as SparsePackedSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-linalg"
        [ ArchitectureSpec.tests,
          BasicSpec.tests,
          BlockSpec.tests,
          DenseFlatSpec.tests,
          DenseRowsSpec.tests,
          FieldSpec.tests,
          DomainSpec.tests,
          DynamicSpec.tests,
          ExteriorSpec.tests,
          GF2Spec.tests,
          GeometryStorageSpec.tests,
          SymmetricSpec.tests,
          AdvancedSpec.tests,
          KrylovSpec.tests,
          SparsePackedSpec.tests,
          SparseSolverSpec.tests,
          StaticsSpec.tests
        ]
    )
