module Moonlight.LinAlg.Effect.Laws
  ( linalgLawSuites,
    registeredLawNames,
    tests,
  )
where

import Data.List (nub, (\\))
import Moonlight.LinAlg.Effect.Harness qualified as Harness
import Moonlight.LinAlg.Effect.LawNames (LawName (..), allLawNames)
import Moonlight.Pale.Test.Laws.Suite
  ( LawSuite,
    hUnitLaw,
    lawGroup,
    namedQuickCheckLaw,
    renderLawSuite,
  )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  renderLawSuite (lawGroup "moonlight-linalg laws" linalgLawSuites)

data RegisteredLaw = RegisteredLaw !LawName !QC.Property

data LawSection = LawSection !String ![RegisteredLaw]

linalgLawSuites :: [LawSuite]
linalgLawSuites =
  lawGroup
    "manifest"
    [hUnitLaw "linalg_law_manifest_totality" manifestTotalityAssertion]
    : (lawSectionSuite <$> registeredLawSections)

registeredLawNames :: [LawName]
registeredLawNames =
  concatMap
    (\(LawSection _ registrations) -> registeredLawName <$> registrations)
    registeredLawSections

lawSectionSuite :: LawSection -> LawSuite
lawSectionSuite (LawSection sectionName registrations) =
  lawGroup sectionName (registeredLawSuite <$> registrations)

registeredLawSuite :: RegisteredLaw -> LawSuite
registeredLawSuite (RegisteredLaw lawName lawProperty) =
  namedQuickCheckLaw lawName lawProperty

registeredLawName :: RegisteredLaw -> LawName
registeredLawName (RegisteredLaw lawName _) = lawName

registeredLawSections :: [LawSection]
registeredLawSections =
  [ LawSection
      "dense algebra"
      [ RegisteredLaw DenseAddAssociative Harness.denseAddAssociativeLaw,
        RegisteredLaw DenseAddCommutative Harness.denseAddCommutativeLaw,
        RegisteredLaw DenseMultiplyAssociative Harness.denseMultiplyAssociativeLaw,
        RegisteredLaw DenseLeftDistributive Harness.denseLeftDistributiveLaw,
        RegisteredLaw DenseRightDistributive Harness.denseRightDistributiveLaw,
        RegisteredLaw DenseTransposeInvolution Harness.denseTransposeInvolutionLaw,
        RegisteredLaw DenseTransposeProductReversal Harness.denseTransposeProductReversalLaw,
        RegisteredLaw DenseMapComposition Harness.denseMapCompositionLaw
      ],
    LawSection
      "decompositions"
      [ RegisteredLaw QRReconstructsInput Harness.qrReconstructsInputLaw,
        RegisteredLaw QROrthonormalColumns Harness.qrOrthonormalColumnsLaw,
        RegisteredLaw CholeskyReconstructsSPD Harness.choleskyReconstructsSpdLaw,
        RegisteredLaw SymmetricEigenReconstructs Harness.symmetricEigenReconstructsLaw,
        RegisteredLaw SymmetricEigenOrthonormal Harness.symmetricEigenOrthonormalLaw,
        RegisteredLaw SymmetricEigenUncheckedPassesCertification Harness.symmetricEigenUncheckedPassesCertificationLaw,
        RegisteredLaw ThinSVDReconstructs Harness.thinSvdReconstructsLaw,
        RegisteredLaw ThinSVDOrthonormalFactors Harness.thinSvdOrthonormalFactorsLaw,
        RegisteredLaw ThinSVDSingularValuesOrderedNonnegative Harness.thinSvdSingularValuesOrderedNonnegativeLaw
      ],
    LawSection
      "field and gf2"
      [ RegisteredLaw PLUReconstructsInput Harness.pluReconstructsInputLaw,
        RegisteredLaw RankKernelNullity Harness.rankKernelNullityLaw,
        RegisteredLaw KernelVectorsAnnihilated Harness.kernelVectorsAnnihilatedLaw,
        RegisteredLaw PackedLinearMapIdentity Harness.packedLinearMapIdentityLaw,
        RegisteredLaw PackedLinearMapComposition Harness.packedLinearMapCompositionLaw,
        RegisteredLaw GF2PackedInverseTwoSided Harness.gf2PackedInverseTwoSidedLaw
      ],
    LawSection
      "domain"
      [ RegisteredLaw SmithDiagonalReconstructsInput Harness.smithDiagonalReconstructsInputLaw,
        RegisteredLaw SmithDivisibilityChain Harness.smithDivisibilityChainLaw,
        RegisteredLaw SmithWitnessesUnimodular Harness.smithWitnessesUnimodularLaw,
        RegisteredLaw SmithDiagonalOnlyAgreesWithFull Harness.smithDiagonalOnlyAgreesWithFullLaw,
        RegisteredLaw BareissRankAgreesWithFieldRank Harness.bareissRankAgreesWithFieldRankLaw,
        RegisteredLaw BareissDeterminantAgreesWithExterior Harness.bareissDeterminantAgreesWithExteriorLaw
      ],
    LawSection
      "sparse storage"
      [ RegisteredLaw COOCSRRoundTrip Harness.cooCsrRoundTripLaw,
        RegisteredLaw COOCSCRoundTrip Harness.cooCscRoundTripLaw,
        RegisteredLaw CSRCSCTransposeAgreement Harness.csrCscTransposeAgreementLaw,
        RegisteredLaw CSRMatVecAgreesWithDense Harness.csrMatVecAgreesWithDenseLaw,
        RegisteredLaw CanonicalCSRCombinesDuplicates Harness.canonicalCsrCombinesDuplicatesLaw,
        RegisteredLaw GraphLaplacianSymmetricRowSumsZero Harness.graphLaplacianSymmetricRowSumsZeroLaw,
        RegisteredLaw SelfAdjointCSRRejectsAsymmetry Harness.selfAdjointCsrRejectsAsymmetryLaw
      ],
    LawSection
      "operator"
      [ RegisteredLaw ScaledOperatorAction Harness.scaledOperatorActionLaw,
        RegisteredLaw ShiftedIdentityAction Harness.shiftedIdentityActionLaw
      ],
    LawSection
      "preconditioner"
      [ RegisteredLaw IC0FactorSolveRoundTrip Harness.ic0FactorSolveRoundTripLaw,
        RegisteredLaw IC0RejectsNonpositivePivot Harness.ic0RejectsNonpositivePivotLaw,
        RegisteredLaw PreconditionedCGConvergesOnSPD Harness.preconditionedCgConvergesOnSpdLaw
      ],
    LawSection
      "krylov spectral"
      [ RegisteredLaw ArnoldiRelationHolds Harness.arnoldiRelationHoldsLaw,
        RegisteredLaw ArnoldiBasisOrthonormal Harness.arnoldiBasisOrthonormalLaw,
        RegisteredLaw LanczosProjectionTridiagonal Harness.lanczosProjectionTridiagonalLaw,
        RegisteredLaw LanczosBasisOrthonormal Harness.lanczosBasisOrthonormalLaw,
        RegisteredLaw ThickRestartLockedPairsResidualBounded Harness.thickRestartLockedPairsResidualBoundedLaw,
        RegisteredLaw SelectedPairsResidualBounded Harness.selectedPairsResidualBoundedLaw,
        RegisteredLaw SelectedPairsClusterOrthonormal Harness.selectedPairsClusterOrthonormalLaw,
        RegisteredLaw TridiagonalSelectedValuesAgreeWithAllPairs Harness.tridiagonalSelectedValuesAgreeWithAllPairsLaw,
        RegisteredLaw DiagonalSpectralValuesExact Harness.diagonalSpectralValuesExactLaw,
        RegisteredLaw PathLaplacianSpectralValuesClosedForm Harness.pathLaplacianSpectralValuesClosedFormLaw,
        RegisteredLaw EigenRequestRejectsOversubscription Harness.eigenRequestRejectsOversubscriptionLaw
      ],
    LawSection
      "geometry"
      [ RegisteredLaw Vec3AddCommutativeAssociative Harness.vec3AddCommutativeAssociativeLaw,
        RegisteredLaw Vec3DotSymmetric Harness.vec3DotSymmetricLaw,
        RegisteredLaw Vec3NormalizeUnit Harness.vec3NormalizeUnitLaw,
        RegisteredLaw AABBUnionCommutativeAssociative Harness.aabbUnionCommutativeAssociativeLaw,
        RegisteredLaw AABBUnionContainsOperands Harness.aabbUnionContainsOperandsLaw,
        RegisteredLaw AABBIntersectionCommutative Harness.aabbIntersectionCommutativeLaw,
        RegisteredLaw SymmetricOuterApplyAgreement Harness.symmetricOuterApplyAgreementLaw,
        RegisteredLaw GeometrySymmetricEigenReconstructs Harness.geometrySymmetricEigenReconstructsLaw,
        RegisteredLaw GeometrySymmetricEigenOrthonormal Harness.geometrySymmetricEigenOrthonormalLaw
      ],
    LawSection
      "statics"
      [ RegisteredLaw NetworkDeclarationOrderInvariant Harness.networkDeclarationOrderInvariantLaw,
        RegisteredLaw RepeatedLoadsAccumulate Harness.repeatedLoadsAccumulateLaw,
        RegisteredLaw EquilibriumAssemblyCanonicalOrdering Harness.equilibriumAssemblyCanonicalOrderingLaw,
        RegisteredLaw EquilibriumSolutionResidualBounded Harness.equilibriumSolutionResidualBoundedLaw,
        RegisteredLaw UnsupportedLoadProducesResidualViolation Harness.unsupportedLoadProducesResidualViolationLaw
      ]
  ]

manifestTotalityAssertion :: IO ()
manifestTotalityAssertion = do
  let missing = allLawNames \\ registeredLawNames
      unrecognized = registeredLawNames \\ allLawNames
      duplicateCount = length registeredLawNames - length (nub registeredLawNames)
  assertBool ("missing law registrations: " <> show missing) (null missing)
  assertBool ("unrecognized law registrations: " <> show unrecognized) (null unrecognized)
  assertBool ("duplicate law registrations: " <> show duplicateCount) (duplicateCount == 0)
