module Moonlight.LinAlg.Effect.Harness.Geometry
  ( vec3AddCommutativeAssociativeLaw,
    vec3DotSymmetricLaw,
    vec3NormalizeUnitLaw,
    aabbUnionCommutativeAssociativeLaw,
    aabbUnionContainsOperandsLaw,
    aabbIntersectionCommutativeLaw,
    symmetricOuterApplyAgreementLaw,
    geometrySymmetricEigenReconstructsLaw,
    geometrySymmetricEigenOrthonormalLaw,
  )
where

import Moonlight.LinAlg
  ( AABB,
    Symmetric3 (..),
    Vec3 (..),
    aabbMax,
    aabbMin,
    addVec3,
    applySymmetric3,
    diagonalSymmetric3,
    dotVec3,
    eigendecomposeSymmetric3,
    magnitudeVec3,
    mkAabb,
    normalizeVec3,
    outerSymmetric3,
    scaleVec3,
    symmetric3ToMatrix,
    toListMatrix,
    toListVector,
    unionAabb,
  )
import Moonlight.LinAlg.Effect.Harness.Core (assertApproxEqual, assertApproxList, assertRightProperty, matrix3Product)
import Test.Tasty.QuickCheck qualified as QC

newtype GeneratedVec3 = GeneratedVec3 Vec3
  deriving stock (Eq, Show)

newtype NonZeroVec3 = NonZeroVec3 Vec3
  deriving stock (Eq, Show)

newtype GeneratedAABB = GeneratedAABB AABB
  deriving stock (Eq, Show)

newtype GeneratedSymmetric3 = GeneratedSymmetric3 (Symmetric3 Double)
  deriving stock (Eq, Show)

newtype GeneratedWeight = GeneratedWeight Double
  deriving stock (Eq, Show)

instance QC.Arbitrary GeneratedVec3 where
  arbitrary =
    GeneratedVec3
      <$> (Vec3 <$> component <*> component <*> component)

instance QC.Arbitrary NonZeroVec3 where
  arbitrary =
    NonZeroVec3
      <$> QC.suchThat
        (Vec3 <$> component <*> component <*> component)
        (\value -> magnitudeVec3 value > 1.0e-6)

instance QC.Arbitrary GeneratedAABB where
  arbitrary = do
    minX <- component
    minY <- component
    minZ <- component
    sizeX <- nonNegativeComponent
    sizeY <- nonNegativeComponent
    sizeZ <- nonNegativeComponent
    case mkAabb (Vec3 minX minY minZ) (Vec3 (minX + sizeX) (minY + sizeY) (minZ + sizeZ)) of
      Nothing -> QC.discard
      Just boxValue -> pure (GeneratedAABB boxValue)

instance QC.Arbitrary GeneratedSymmetric3 where
  arbitrary =
    GeneratedSymmetric3 <$> QC.oneof [repeatedSpectrum, tinyGapSpectrum, badlyScaledSpectrum, coupledSpectrum]

instance QC.Arbitrary GeneratedWeight where
  arbitrary =
    GeneratedWeight <$> component

vec3AddCommutativeAssociativeLaw :: QC.Property
vec3AddCommutativeAssociativeLaw =
  QC.property vec3AddCommutativeAssociativeLawProperty

vec3DotSymmetricLaw :: QC.Property
vec3DotSymmetricLaw =
  QC.property vec3DotSymmetricLawProperty

vec3NormalizeUnitLaw :: QC.Property
vec3NormalizeUnitLaw =
  QC.property vec3NormalizeUnitLawProperty

aabbUnionCommutativeAssociativeLaw :: QC.Property
aabbUnionCommutativeAssociativeLaw =
  QC.property aabbUnionCommutativeAssociativeLawProperty

aabbUnionContainsOperandsLaw :: QC.Property
aabbUnionContainsOperandsLaw =
  QC.property aabbUnionContainsOperandsLawProperty

aabbIntersectionCommutativeLaw :: QC.Property
aabbIntersectionCommutativeLaw =
  QC.property aabbIntersectionCommutativeLawProperty

symmetricOuterApplyAgreementLaw :: QC.Property
symmetricOuterApplyAgreementLaw =
  QC.property symmetricOuterApplyAgreementLawProperty

geometrySymmetricEigenReconstructsLaw :: QC.Property
geometrySymmetricEigenReconstructsLaw =
  QC.property geometrySymmetricEigenReconstructsLawProperty

geometrySymmetricEigenOrthonormalLaw :: QC.Property
geometrySymmetricEigenOrthonormalLaw =
  QC.property geometrySymmetricEigenOrthonormalLawProperty

component :: QC.Gen Double
component =
  fromIntegral <$> QC.chooseInt (-8, 8)

nonNegativeComponent :: QC.Gen Double
nonNegativeComponent =
  fromIntegral <$> QC.chooseInt (0, 8)

vec3AddCommutativeAssociativeLawProperty :: GeneratedVec3 -> GeneratedVec3 -> GeneratedVec3 -> QC.Property
vec3AddCommutativeAssociativeLawProperty (GeneratedVec3 leftValue) (GeneratedVec3 middleValue) (GeneratedVec3 rightValue) =
  QC.property
    ( addVec3 leftValue middleValue == addVec3 middleValue leftValue
        && addVec3 (addVec3 leftValue middleValue) rightValue == addVec3 leftValue (addVec3 middleValue rightValue)
    )

vec3DotSymmetricLawProperty :: GeneratedVec3 -> GeneratedVec3 -> QC.Property
vec3DotSymmetricLawProperty (GeneratedVec3 leftValue) (GeneratedVec3 rightValue) =
  QC.property (assertApproxEqual (dotVec3 leftValue rightValue) (dotVec3 rightValue leftValue))

vec3NormalizeUnitLawProperty :: NonZeroVec3 -> QC.Property
vec3NormalizeUnitLawProperty (NonZeroVec3 value) =
  assertRightProperty $ do
    normalized <- normalizeVec3 value
    pure (assertApproxEqual 1.0 (magnitudeVec3 normalized))

aabbUnionCommutativeAssociativeLawProperty :: GeneratedAABB -> GeneratedAABB -> GeneratedAABB -> QC.Property
aabbUnionCommutativeAssociativeLawProperty (GeneratedAABB leftValue) (GeneratedAABB middleValue) (GeneratedAABB rightValue) =
  QC.property
    ( unionAabb leftValue middleValue == unionAabb middleValue leftValue
        && unionAabb (unionAabb leftValue middleValue) rightValue == unionAabb leftValue (unionAabb middleValue rightValue)
    )

aabbUnionContainsOperandsLawProperty :: GeneratedAABB -> GeneratedAABB -> QC.Property
aabbUnionContainsOperandsLawProperty (GeneratedAABB leftValue) (GeneratedAABB rightValue) =
  let unionValue = unionAabb leftValue rightValue
   in QC.property (aabbContains unionValue leftValue && aabbContains unionValue rightValue)

aabbIntersectionCommutativeLawProperty :: GeneratedAABB -> GeneratedAABB -> QC.Property
aabbIntersectionCommutativeLawProperty (GeneratedAABB leftValue) (GeneratedAABB rightValue) =
  QC.property (intersectAabb leftValue rightValue == intersectAabb rightValue leftValue)

symmetricOuterApplyAgreementLawProperty :: GeneratedWeight -> GeneratedVec3 -> GeneratedVec3 -> QC.Property
symmetricOuterApplyAgreementLawProperty (GeneratedWeight weightValue) (GeneratedVec3 basisValue) (GeneratedVec3 inputValue) =
  let tensorValue = outerSymmetric3 weightValue basisValue
      expectedValue = scaleVec3 (weightValue * dotVec3 basisValue inputValue) basisValue
   in QC.property (vec3Approx expectedValue (applySymmetric3 tensorValue inputValue))

geometrySymmetricEigenReconstructsLawProperty :: GeneratedSymmetric3 -> QC.Property
geometrySymmetricEigenReconstructsLawProperty (GeneratedSymmetric3 tensorValue) =
  assertRightProperty $ do
    matrixValue <- symmetric3ToMatrix tensorValue
    (eigenvalues, eigenvectors) <- eigendecomposeSymmetric3 tensorValue
    let originalRows = rows3 (toListMatrix matrixValue)
        eigenvectorRows = rows3 (toListMatrix eigenvectors)
        diagonalRows = diagonal3 (toListVector eigenvalues)
        reconstructedRows = matrix3Product (matrix3Product eigenvectorRows diagonalRows) (transposeRows3 eigenvectorRows)
    pure (and (zipWith assertApproxList originalRows reconstructedRows))

geometrySymmetricEigenOrthonormalLawProperty :: GeneratedSymmetric3 -> QC.Property
geometrySymmetricEigenOrthonormalLawProperty (GeneratedSymmetric3 tensorValue) =
  assertRightProperty $ do
    (_, eigenvectors) <- eigendecomposeSymmetric3 tensorValue
    let eigenvectorRows = rows3 (toListMatrix eigenvectors)
        gramRows = matrix3Product (transposeRows3 eigenvectorRows) eigenvectorRows
    pure (and (zipWith assertApproxList identityRows3 gramRows))

repeatedSpectrum :: QC.Gen (Symmetric3 Double)
repeatedSpectrum =
  pure (diagonalSymmetric3 2.0 2.0 5.0)

tinyGapSpectrum :: QC.Gen (Symmetric3 Double)
tinyGapSpectrum =
  pure (diagonalSymmetric3 1.0 (1.0 + 1.0e-12) 3.0)

badlyScaledSpectrum :: QC.Gen (Symmetric3 Double)
badlyScaledSpectrum =
  pure (diagonalSymmetric3 1.0e-12 1.0 1.0e12)

coupledSpectrum :: QC.Gen (Symmetric3 Double)
coupledSpectrum =
  pure
    Symmetric3
      { sym3XX = 3.0,
        sym3XY = 1.0e-6,
        sym3XZ = 2.0e-6,
        sym3YY = 3.0 + 1.0e-12,
        sym3YZ = -1.0e-6,
        sym3ZZ = 7.0
      }

intersectAabb :: AABB -> AABB -> Maybe AABB
intersectAabb leftValue rightValue =
  mkAabb
    (Vec3 (max lx rx) (max ly ry) (max lz rz))
    (Vec3 (min lxx rxx) (min lyy ryy) (min lzz rzz))
  where
    Vec3 lx ly lz = aabbMin leftValue
    Vec3 lxx lyy lzz = aabbMax leftValue
    Vec3 rx ry rz = aabbMin rightValue
    Vec3 rxx ryy rzz = aabbMax rightValue

aabbContains :: AABB -> AABB -> Bool
aabbContains outerValue innerValue =
  let Vec3 ox oy oz = aabbMin outerValue
      Vec3 oxx oyy ozz = aabbMax outerValue
      Vec3 ix iy iz = aabbMin innerValue
      Vec3 ixx iyy izz = aabbMax innerValue
   in ox <= ix && oy <= iy && oz <= iz && ixx <= oxx && iyy <= oyy && izz <= ozz

vec3Approx :: Vec3 -> Vec3 -> Bool
vec3Approx (Vec3 ex ey ez) (Vec3 ax ay az) =
  assertApproxList [ex, ey, ez] [ax, ay, az]

rows3 :: [a] -> [[a]]
rows3 values =
  case values of
    [a00, a01, a02, a10, a11, a12, a20, a21, a22] ->
      [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]]
    _ -> []

transposeRows3 :: [[a]] -> [[a]]
transposeRows3 rowsValue =
  case rowsValue of
    [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]] ->
      [[a00, a10, a20], [a01, a11, a21], [a02, a12, a22]]
    _ -> []

diagonal3 :: [Double] -> [[Double]]
diagonal3 values =
  case values of
    [xValue, yValue, zValue] ->
      [[xValue, 0.0, 0.0], [0.0, yValue, 0.0], [0.0, 0.0, zValue]]
    _ -> []

identityRows3 :: [[Double]]
identityRows3 =
  [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]
