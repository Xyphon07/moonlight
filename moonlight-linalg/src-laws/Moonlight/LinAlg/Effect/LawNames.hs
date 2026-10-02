{-# LANGUAGE DerivingStrategies #-}

module Moonlight.LinAlg.Effect.LawNames
  ( LawName (..),
    allLawNames,
    lawName,
  )
where

import Data.Kind (Type)
import Moonlight.Core (IsLawName (..), constructorLawNameWithOverrides)

type LawName :: Type
data LawName
  = DenseAddAssociative
  | DenseAddCommutative
  | DenseMultiplyAssociative
  | DenseLeftDistributive
  | DenseRightDistributive
  | DenseTransposeInvolution
  | DenseTransposeProductReversal
  | DenseMapComposition
  | QRReconstructsInput
  | QROrthonormalColumns
  | CholeskyReconstructsSPD
  | SymmetricEigenReconstructs
  | SymmetricEigenOrthonormal
  | SymmetricEigenUncheckedPassesCertification
  | ThinSVDReconstructs
  | ThinSVDOrthonormalFactors
  | ThinSVDSingularValuesOrderedNonnegative
  | PLUReconstructsInput
  | RankKernelNullity
  | KernelVectorsAnnihilated
  | PackedLinearMapIdentity
  | PackedLinearMapComposition
  | GF2PackedInverseTwoSided
  | SmithDiagonalReconstructsInput
  | SmithDivisibilityChain
  | SmithWitnessesUnimodular
  | SmithDiagonalOnlyAgreesWithFull
  | BareissRankAgreesWithFieldRank
  | BareissDeterminantAgreesWithExterior
  | COOCSRRoundTrip
  | COOCSCRoundTrip
  | CSRCSCTransposeAgreement
  | CSRMatVecAgreesWithDense
  | CanonicalCSRCombinesDuplicates
  | GraphLaplacianSymmetricRowSumsZero
  | SelfAdjointCSRRejectsAsymmetry
  | ScaledOperatorAction
  | ShiftedIdentityAction
  | IC0FactorSolveRoundTrip
  | IC0RejectsNonpositivePivot
  | PreconditionedCGConvergesOnSPD
  | ArnoldiRelationHolds
  | ArnoldiBasisOrthonormal
  | LanczosProjectionTridiagonal
  | LanczosBasisOrthonormal
  | ThickRestartLockedPairsResidualBounded
  | SelectedPairsResidualBounded
  | SelectedPairsClusterOrthonormal
  | TridiagonalSelectedValuesAgreeWithAllPairs
  | DiagonalSpectralValuesExact
  | PathLaplacianSpectralValuesClosedForm
  | EigenRequestRejectsOversubscription
  | Vec3AddCommutativeAssociative
  | Vec3DotSymmetric
  | Vec3NormalizeUnit
  | AABBUnionCommutativeAssociative
  | AABBUnionContainsOperands
  | AABBIntersectionCommutative
  | SymmetricOuterApplyAgreement
  | GeometrySymmetricEigenReconstructs
  | GeometrySymmetricEigenOrthonormal
  | NetworkDeclarationOrderInvariant
  | RepeatedLoadsAccumulate
  | EquilibriumAssemblyCanonicalOrdering
  | EquilibriumSolutionResidualBounded
  | UnsupportedLoadProducesResidualViolation
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName LawName where
  lawNameText = lawName

allLawNames :: [LawName]
allLawNames =
  [minBound .. maxBound]

lawName :: LawName -> String
lawName =
  constructorLawNameWithOverrides
    [ ("DenseAddAssociative", "linalg_dense_add_associative"),
      ("DenseAddCommutative", "linalg_dense_add_commutative"),
      ("DenseMultiplyAssociative", "linalg_dense_multiply_associative"),
      ("DenseLeftDistributive", "linalg_dense_left_distributive"),
      ("DenseRightDistributive", "linalg_dense_right_distributive"),
      ("DenseTransposeInvolution", "linalg_dense_transpose_involution"),
      ("DenseTransposeProductReversal", "linalg_dense_transpose_product_reversal"),
      ("DenseMapComposition", "linalg_dense_map_composition"),
      ("QRReconstructsInput", "linalg_decomposition_qr_reconstructs_input"),
      ("QROrthonormalColumns", "linalg_decomposition_qr_orthonormal_columns"),
      ("CholeskyReconstructsSPD", "linalg_decomposition_cholesky_reconstructs_spd"),
      ("SymmetricEigenReconstructs", "linalg_decomposition_symmetric_eigen_reconstructs"),
      ("SymmetricEigenOrthonormal", "linalg_decomposition_symmetric_eigen_orthonormal"),
      ("SymmetricEigenUncheckedPassesCertification", "linalg_decomposition_symmetric_eigen_unchecked_passes_certification"),
      ("ThinSVDReconstructs", "linalg_decomposition_thin_svd_reconstructs"),
      ("ThinSVDOrthonormalFactors", "linalg_decomposition_thin_svd_orthonormal_factors"),
      ("ThinSVDSingularValuesOrderedNonnegative", "linalg_decomposition_thin_svd_singular_values_ordered_nonnegative"),
      ("PLUReconstructsInput", "linalg_field_plu_reconstructs_input"),
      ("RankKernelNullity", "linalg_field_rank_kernel_nullity"),
      ("KernelVectorsAnnihilated", "linalg_field_kernel_vectors_annihilated"),
      ("PackedLinearMapIdentity", "linalg_gf2_packed_linear_map_identity"),
      ("PackedLinearMapComposition", "linalg_gf2_packed_linear_map_composition"),
      ("GF2PackedInverseTwoSided", "linalg_gf2_packed_inverse_two_sided"),
      ("SmithDiagonalReconstructsInput", "linalg_domain_smith_diagonal_reconstructs_input"),
      ("SmithDivisibilityChain", "linalg_domain_smith_divisibility_chain"),
      ("SmithWitnessesUnimodular", "linalg_domain_smith_witnesses_unimodular"),
      ("SmithDiagonalOnlyAgreesWithFull", "linalg_domain_smith_diagonal_only_agrees_with_full"),
      ("BareissRankAgreesWithFieldRank", "linalg_domain_bareiss_rank_agrees_with_field_rank"),
      ("BareissDeterminantAgreesWithExterior", "linalg_domain_bareiss_determinant_agrees_with_exterior"),
      ("COOCSRRoundTrip", "linalg_sparse_coo_csr_round_trip"),
      ("COOCSCRoundTrip", "linalg_sparse_coo_csc_round_trip"),
      ("CSRCSCTransposeAgreement", "linalg_sparse_csr_csc_transpose_agreement"),
      ("CSRMatVecAgreesWithDense", "linalg_sparse_csr_matvec_agrees_with_dense"),
      ("CanonicalCSRCombinesDuplicates", "linalg_sparse_canonical_csr_combines_duplicates"),
      ("GraphLaplacianSymmetricRowSumsZero", "linalg_sparse_graph_laplacian_symmetric_row_sums_zero"),
      ("SelfAdjointCSRRejectsAsymmetry", "linalg_sparse_self_adjoint_csr_rejects_asymmetry"),
      ("ScaledOperatorAction", "linalg_operator_scaled_operator_action"),
      ("ShiftedIdentityAction", "linalg_operator_shifted_identity_action"),
      ("IC0FactorSolveRoundTrip", "linalg_preconditioner_ic0_factor_solve_round_trip"),
      ("IC0RejectsNonpositivePivot", "linalg_preconditioner_ic0_rejects_nonpositive_pivot"),
      ("PreconditionedCGConvergesOnSPD", "linalg_preconditioner_preconditioned_cg_converges_on_spd"),
      ("ArnoldiRelationHolds", "linalg_krylov_spectral_arnoldi_relation_holds"),
      ("ArnoldiBasisOrthonormal", "linalg_krylov_spectral_arnoldi_basis_orthonormal"),
      ("LanczosProjectionTridiagonal", "linalg_krylov_spectral_lanczos_projection_tridiagonal"),
      ("LanczosBasisOrthonormal", "linalg_krylov_spectral_lanczos_basis_orthonormal"),
      ("ThickRestartLockedPairsResidualBounded", "linalg_krylov_spectral_thick_restart_locked_pairs_residual_bounded"),
      ("SelectedPairsResidualBounded", "linalg_krylov_spectral_selected_pairs_residual_bounded"),
      ("SelectedPairsClusterOrthonormal", "linalg_krylov_spectral_selected_pairs_cluster_orthonormal"),
      ("TridiagonalSelectedValuesAgreeWithAllPairs", "linalg_krylov_spectral_tridiagonal_selected_values_agree_with_all_pairs"),
      ("DiagonalSpectralValuesExact", "linalg_krylov_spectral_diagonal_spectral_values_exact"),
      ("PathLaplacianSpectralValuesClosedForm", "linalg_krylov_spectral_path_laplacian_spectral_values_closed_form"),
      ("EigenRequestRejectsOversubscription", "linalg_krylov_spectral_eigen_request_rejects_oversubscription"),
      ("Vec3AddCommutativeAssociative", "linalg_geometry_vec3_add_commutative_associative"),
      ("Vec3DotSymmetric", "linalg_geometry_vec3_dot_symmetric"),
      ("Vec3NormalizeUnit", "linalg_geometry_vec3_normalize_unit"),
      ("AABBUnionCommutativeAssociative", "linalg_geometry_aabb_union_commutative_associative"),
      ("AABBUnionContainsOperands", "linalg_geometry_aabb_union_contains_operands"),
      ("AABBIntersectionCommutative", "linalg_geometry_aabb_intersection_commutative"),
      ("SymmetricOuterApplyAgreement", "linalg_geometry_symmetric_outer_apply_agreement"),
      ("GeometrySymmetricEigenReconstructs", "linalg_geometry_symmetric_eigen_reconstructs"),
      ("GeometrySymmetricEigenOrthonormal", "linalg_geometry_symmetric_eigen_orthonormal"),
      ("NetworkDeclarationOrderInvariant", "linalg_statics_network_declaration_order_invariant"),
      ("RepeatedLoadsAccumulate", "linalg_statics_repeated_loads_accumulate"),
      ("EquilibriumAssemblyCanonicalOrdering", "linalg_statics_equilibrium_assembly_canonical_ordering"),
      ("EquilibriumSolutionResidualBounded", "linalg_statics_equilibrium_solution_residual_bounded"),
      ("UnsupportedLoadProducesResidualViolation", "linalg_statics_unsupported_load_produces_residual_violation")
    ]
    . show
