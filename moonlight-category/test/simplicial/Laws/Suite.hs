module Laws.Suite
  ( LawSuiteConfig (..),
    mkLawfulCarrierSpec,
    simplicialLawSuite,
    lawfulCarrierSuite,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Pale.Test.Laws.Suite
  ( LawSuite,
    lawGroup,
    quickCheckLaw,
    renderLawSuite,
  )
import Moonlight.Category.Simplicial
  ( SimplicialLawCheck,
    SimplicialLawEquality,
    SimplicialLawIndices (..),
    SimplicialLawObstruction (..),
    checkDegeneracyDegeneracyLawBy,
    checkFaceDegeneracyLawBy,
    checkFaceFaceLawBy,
    checkSimplicialLawsBy,
    lawObstructionKind,
  )
import Moonlight.Category.Simplicial (TruncatedNormalizedSSet)
import Test.Tasty (TestTree)
import qualified Test.Tasty.QuickCheck as QC

type LawSuiteConfig :: Type -> Type -> Type
data LawSuiteConfig carrier simplex = LawSuiteConfig
  { lawSuiteName :: String,
    lawSuiteMaxSuccess :: Int,
    lawSuiteCarrierToSSet :: carrier -> TruncatedNormalizedSSet simplex,
    lawSuiteEquality :: SimplicialLawEquality simplex,
    lawSuiteRenderSimplex :: simplex -> String
  }

mkLawfulCarrierSpec ::
  (QC.Arbitrary carrier, Show carrier) =>
  String ->
  LawSuiteConfig carrier simplex ->
  LawSuite
mkLawfulCarrierSpec carrierName config =
  lawGroup carrierName [simplicialLawSuite config]

renderMaybeSimplex :: (simplex -> String) -> Maybe simplex -> String
renderMaybeSimplex renderSimplex maybeSimplex =
  case maybeSimplex of
    Nothing -> "Nothing"
    Just simplexValue -> "Just " <> renderSimplex simplexValue

renderIndices :: SimplicialLawIndices -> String
renderIndices indices =
  case indices of
    FaceFaceIndices leftFaceIndex rightFaceIndex ->
      "leftFace=" <> show leftFaceIndex <> ", rightFace=" <> show rightFaceIndex
    DegeneracyDegeneracyIndices leftDegeneracyIndex rightDegeneracyIndex ->
      "leftDegeneracy=" <> show leftDegeneracyIndex <> ", rightDegeneracy=" <> show rightDegeneracyIndex
    FaceDegeneracyIndices faceIndex degeneracyIndex ->
      "face=" <> show faceIndex <> ", degeneracy=" <> show degeneracyIndex

renderObstruction :: (simplex -> String) -> SimplicialLawObstruction simplex -> String
renderObstruction renderSimplex obstruction =
  unlines
    [ "law=" <> show (lawObstructionKind obstruction),
      "dimension=" <> show (lawObstructionDimension obstruction),
      "indices=" <> renderIndices (lawObstructionIndices obstruction),
      "source=" <> renderSimplex (lawObstructionSimplex obstruction),
      "left=" <> renderMaybeSimplex renderSimplex (lawObstructionLeftResult obstruction),
      "right=" <> renderMaybeSimplex renderSimplex (lawObstructionRightResult obstruction)
    ]

renderCheckFailure :: (simplex -> String) -> NonEmpty.NonEmpty (SimplicialLawObstruction simplex) -> String
renderCheckFailure renderSimplex obstructions =
  unlines
    [ "simplicial law obstruction count=" <> show (length (NonEmpty.toList obstructions)),
      "first obstruction:",
      renderObstruction renderSimplex (NonEmpty.head obstructions)
    ]

lawCheckProperty ::
  (simplex -> String) ->
  SimplicialLawCheck simplex ->
  QC.Property
lawCheckProperty renderSimplex lawCheck =
  case lawCheck of
    Right () -> QC.property True
    Left obstructions -> QC.counterexample (renderCheckFailure renderSimplex obstructions) False

simplicialLawSuite ::
  (QC.Arbitrary carrier, Show carrier) =>
  LawSuiteConfig carrier simplex ->
  LawSuite
simplicialLawSuite config =
  let runLaw lawCheck carrierValue =
        lawCheckProperty
          (lawSuiteRenderSimplex config)
          ( lawCheck
              (lawSuiteEquality config)
              (lawSuiteCarrierToSSet config carrierValue)
          )
      lawProperty propertyName lawCheck =
        quickCheckLaw
          propertyName
          (QC.withNumTests (lawSuiteMaxSuccess config) (runLaw lawCheck))
      bundledLaws :: [(String, SimplicialLawEquality simplex -> TruncatedNormalizedSSet simplex -> SimplicialLawCheck simplex)]
      bundledLaws =
        [ ("face-face: d_i d_j = d_{j-1} d_i (i < j)", checkFaceFaceLawBy),
          ("degeneracy-degeneracy: s_i s_j = s_{j+1} s_i (i <= j)", checkDegeneracyDegeneracyLawBy),
          ("mixed: d_i s_j cases", checkFaceDegeneracyLawBy),
          ("all simplicial identities", checkSimplicialLawsBy)
        ]
   in lawGroup
        (lawSuiteName config)
        (map (uncurry lawProperty) bundledLaws)

lawfulCarrierSuite :: [LawSuite] -> TestTree
lawfulCarrierSuite carrierSpecs =
  renderLawSuite (lawGroup "lawful carriers" carrierSpecs)
