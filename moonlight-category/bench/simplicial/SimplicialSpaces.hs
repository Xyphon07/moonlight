module SimplicialSpaces
  ( generatedSpaceBenchmarks,
  )
where

import Data.Function ((&))
import Moonlight.Category.Simplicial
  ( GeneratedSSet,
    boundarySimplex,
    boundarySimplexGenerated,
    generatedSimplicesAtDimension,
    hornSimplex,
    hornSimplexGenerated,
    normalizeGeneratedSSet,
    standardSimplex,
    standardSimplexGenerated,
    validateGeneratedSSet,
  )
import Numeric.Natural (Natural)
import SimplicialWeight
  ( naturalSSetWeight,
    naturalSimplicesWeight,
    obstructionWeight,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

generatedSpaceBenchmarks :: Benchmark
generatedSpaceBenchmarks =
  bgroup
    "simplicial space API"
    [ bgroup
        "normalized constructors"
        [ bgroup
            "standardSimplex"
            (spaceCases & fmap (\spaceCase -> bench (spaceCaseLabel spaceCase) (nf standardSimplexWeight spaceCase))),
          bgroup
            "boundarySimplex"
            (spaceCases & fmap (\spaceCase -> bench (spaceCaseLabel spaceCase) (nf boundarySimplexWeight spaceCase))),
          bgroup
            "hornSimplex"
            (hornSpaceCases & fmap (\spaceCase -> bench (hornSpaceCaseLabel spaceCase) (nf hornSimplexWeight spaceCase)))
        ],
      bgroup
        "normalizeGeneratedSSet . standardSimplexGenerated"
        (spaceCases & fmap (\spaceCase -> bench (spaceCaseLabel spaceCase) (nf standardNormalizationWeight spaceCase))),
      bgroup
        "normalizeGeneratedSSet . boundarySimplexGenerated"
        (spaceCases & fmap (\spaceCase -> bench (spaceCaseLabel spaceCase) (nf boundaryNormalizationWeight spaceCase))),
      bgroup
        "validateGeneratedSSet"
        (generatedValidationCases & fmap validationBenchmark)
    ]

validationBenchmark :: GeneratedValidationCase -> Benchmark
validationBenchmark validationCase =
  bench (generatedValidationCaseLabel validationCase) (nf generatedValidationWeight validationCase)

data SpaceCase = SpaceCase
  { spaceCaseSimplexDimension :: !Natural,
    spaceCaseTruncationBound :: !Natural
  }
  deriving stock (Eq, Ord, Show)

spaceCases :: [SpaceCase]
spaceCases =
  [ SpaceCase 3 3,
    SpaceCase 4 4,
    SpaceCase 5 4,
    SpaceCase 6 4
  ]

spaceCaseLabel :: SpaceCase -> String
spaceCaseLabel spaceCase =
  "simplex=" <> show (spaceCaseSimplexDimension spaceCase) <> " bound=" <> show (spaceCaseTruncationBound spaceCase)

standardSimplexWeight :: SpaceCase -> Int
standardSimplexWeight spaceCase =
  standardSimplex (spaceCaseSimplexDimension spaceCase) (spaceCaseTruncationBound spaceCase)
    & naturalSSetWeight

boundarySimplexWeight :: SpaceCase -> Int
boundarySimplexWeight spaceCase =
  boundarySimplex (spaceCaseSimplexDimension spaceCase) (spaceCaseTruncationBound spaceCase)
    & naturalSSetWeight

standardNormalizationWeight :: SpaceCase -> Int
standardNormalizationWeight spaceCase =
  standardSimplexGenerated (spaceCaseSimplexDimension spaceCase) (spaceCaseTruncationBound spaceCase)
    & normalizeGeneratedSSet
    & naturalSSetWeight

boundaryNormalizationWeight :: SpaceCase -> Int
boundaryNormalizationWeight spaceCase =
  boundarySimplexGenerated (spaceCaseSimplexDimension spaceCase) (spaceCaseTruncationBound spaceCase)
    & normalizeGeneratedSSet
    & naturalSSetWeight

data HornSpaceCase = HornSpaceCase
  { hornSpaceCaseSimplexDimension :: !Natural,
    hornSpaceCaseMissingFaceIndex :: !Natural,
    hornSpaceCaseTruncationBound :: !Natural
  }
  deriving stock (Eq, Ord, Show)

hornSpaceCases :: [HornSpaceCase]
hornSpaceCases =
  [ HornSpaceCase 3 1 3,
    HornSpaceCase 4 1 4,
    HornSpaceCase 5 2 4,
    HornSpaceCase 6 2 4
  ]

hornSpaceCaseLabel :: HornSpaceCase -> String
hornSpaceCaseLabel spaceCase =
  "simplex="
    <> show (hornSpaceCaseSimplexDimension spaceCase)
    <> " missing="
    <> show (hornSpaceCaseMissingFaceIndex spaceCase)
    <> " bound="
    <> show (hornSpaceCaseTruncationBound spaceCase)

hornSimplexWeight :: HornSpaceCase -> Int
hornSimplexWeight spaceCase =
  maybe
    0
    naturalSSetWeight
    ( hornSimplex
        (hornSpaceCaseSimplexDimension spaceCase)
        (hornSpaceCaseMissingFaceIndex spaceCase)
        (hornSpaceCaseTruncationBound spaceCase)
    )

data GeneratedValidationKind
  = ValidateStandard
  | ValidateBoundary
  | ValidateHorn !Natural
  deriving stock (Eq, Ord, Show)

data GeneratedValidationCase = GeneratedValidationCase
  { generatedValidationKind :: !GeneratedValidationKind,
    generatedValidationSimplexDimension :: !Natural,
    generatedValidationTruncationBound :: !Natural
  }
  deriving stock (Eq, Ord, Show)

generatedValidationCases :: [GeneratedValidationCase]
generatedValidationCases =
  [ GeneratedValidationCase ValidateStandard 4 4,
    GeneratedValidationCase ValidateBoundary 4 4,
    GeneratedValidationCase (ValidateHorn 1) 4 4,
    GeneratedValidationCase (ValidateHorn 2) 5 4
  ]

generatedValidationCaseLabel :: GeneratedValidationCase -> String
generatedValidationCaseLabel validationCase =
  case generatedValidationKind validationCase of
    ValidateStandard -> baseLabel "standardSimplexGenerated"
    ValidateBoundary -> baseLabel "boundarySimplexGenerated"
    ValidateHorn missingFace -> baseLabel ("hornSimplexGenerated missing=" <> show missingFace)
  where
    baseLabel prefix =
      prefix
        <> " simplex="
        <> show (generatedValidationSimplexDimension validationCase)
        <> " bound="
        <> show (generatedValidationTruncationBound validationCase)

generatedFromValidationCase :: GeneratedValidationCase -> Maybe (GeneratedSSet [Natural])
generatedFromValidationCase validationCase =
  case generatedValidationKind validationCase of
    ValidateStandard -> Just (standardGenerated validationCase)
    ValidateBoundary -> Just (boundaryGenerated validationCase)
    ValidateHorn missingFace ->
      hornSimplexGenerated (generatedValidationSimplexDimension validationCase) missingFace (generatedValidationTruncationBound validationCase)

standardGenerated :: GeneratedValidationCase -> GeneratedSSet [Natural]
standardGenerated validationCase =
  standardSimplexGenerated
    (generatedValidationSimplexDimension validationCase)
    (generatedValidationTruncationBound validationCase)

boundaryGenerated :: GeneratedValidationCase -> GeneratedSSet [Natural]
boundaryGenerated validationCase =
  boundarySimplexGenerated
    (generatedValidationSimplexDimension validationCase)
    (generatedValidationTruncationBound validationCase)

validateGeneratedWeight :: GeneratedSSet [Natural] -> Int
validateGeneratedWeight generatedSet =
  case validateGeneratedSSet generatedSet of
    Left obstructions -> obstructionWeight obstructions
    Right () ->
      [0 .. 4]
        & fmap (naturalSimplicesWeight . generatedSimplicesAtDimension generatedSet)
        & sum

generatedValidationWeight :: GeneratedValidationCase -> Int
generatedValidationWeight validationCase =
  maybe 0 validateGeneratedWeight (generatedFromValidationCase validationCase)
