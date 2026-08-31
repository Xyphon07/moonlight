{-# LANGUAGE LambdaCase #-}

-- | Pure, compact SVG projection of the checked category-observatory exhibit.
module Moonlight.Triangulation.Exhibit.CategoryObservatory.Picture
  ( PictureObstruction (..),
    categoryObservatoryPictureFileName,
    renderCategoryObservatoryPicture,
    renderPictureObstruction,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Model
  ( CategoryObject (..),
    CategoryObjectKind (..),
    CategoryProjection (..),
    Exhibit (..),
    MeshEdge (..),
    MeshFace (..),
    MeshProjection (..),
    MeshVertex (..),
    NerveProjection (..),
    NerveSimplexCount (..),
    NerveTwoSimplex (..),
    Position (..),
  )
import Numeric (showFFloat)

data PictureObstruction
  = PictureHasNoMeshVertices
  | PictureHasNoNerveTwoSimplex
  | PictureMissingMeshVertex !String
  | PictureMissingCategoryObject !String
  | PictureMalformedMeshEdge !String ![String]
  | PictureMalformedMeshFace !String ![String]
  | PictureMalformedNerveTwoSimplex !String ![String]
  | PictureMalformedFlagKinds ![CategoryObjectKind]
  deriving stock (Eq, Show)

data PictureBounds = PictureBounds
  { minimumX :: !Double,
    minimumY :: !Double,
    maximumX :: !Double,
    maximumY :: !Double
  }

data PictureFrame = PictureFrame
  { frameLeft :: !Double,
    frameTop :: !Double,
    frameWidth :: !Double,
    frameHeight :: !Double
  }

data ResolvedMeshEdge = ResolvedMeshEdge
  { resolvedEdge :: !MeshEdge,
    resolvedEdgeSource :: !Position,
    resolvedEdgeTarget :: !Position
  }

data ResolvedMeshFace = ResolvedMeshFace
  { resolvedFace :: !MeshFace,
    resolvedFaceVertices :: ![Position]
  }

data ResolvedNerveSimplex = ResolvedNerveSimplex
  { resolvedSimplex :: !NerveTwoSimplex,
    resolvedSimplexVertices :: ![Position]
  }

data FlagSelection = FlagSelection
  { selectedSimplexIdentifier :: !String,
    selectedFace :: !CategoryObject,
    selectedEdge :: !CategoryObject,
    selectedVertex :: !CategoryObject,
    selectedObjectIdentifiers :: !(Set.Set String)
  }

data PictureContext = PictureContext
  { contextExhibit :: !Exhibit,
    contextBounds :: !PictureBounds,
    contextEdges :: ![ResolvedMeshEdge],
    contextFaces :: ![ResolvedMeshFace],
    contextNerveSimplices :: ![ResolvedNerveSimplex],
    contextFlag :: !FlagSelection
  }

categoryObservatoryPictureFileName :: FilePath
categoryObservatoryPictureFileName =
  "moonlight-triangulation-category-observatory.svg"

renderCategoryObservatoryPicture :: Exhibit -> Either PictureObstruction String
renderCategoryObservatoryPicture exhibit =
  renderPicture <$> preparePicture exhibit

renderPictureObstruction :: PictureObstruction -> String
renderPictureObstruction = \case
  PictureHasNoMeshVertices ->
    "category-observatory picture requires at least one mesh vertex"
  PictureHasNoNerveTwoSimplex ->
    "category-observatory picture requires at least one admitted two-simplex"
  PictureMissingMeshVertex identifier ->
    "category-observatory picture cannot resolve mesh vertex " <> show identifier
  PictureMissingCategoryObject identifier ->
    "category-observatory picture cannot resolve category object " <> show identifier
  PictureMalformedMeshEdge identifier vertices ->
    "category-observatory mesh edge "
      <> show identifier
      <> " does not name exactly two vertices: "
      <> show vertices
  PictureMalformedMeshFace identifier vertices ->
    "category-observatory mesh face "
      <> show identifier
      <> " does not name exactly three vertices: "
      <> show vertices
  PictureMalformedNerveTwoSimplex identifier objects ->
    "category-observatory nerve simplex "
      <> show identifier
      <> " does not name one face, edge, and vertex: "
      <> show objects
  PictureMalformedFlagKinds kinds ->
    "category-observatory selected flag has incompatible object kinds: " <> show kinds

preparePicture :: Exhibit -> Either PictureObstruction PictureContext
preparePicture exhibit = do
  let meshProjection = mesh exhibit
      categoryProjection = category exhibit
      nerveProjection = nerve exhibit
      meshVerticesValue = meshVertices meshProjection
      vertexPositions =
        Map.fromList
          ( fmap
              (\vertex -> (meshVertexIdentifier vertex, meshVertexPosition vertex))
              meshVerticesValue
          )
      categoryObjectsByIdentifier =
        Map.fromList
          ( fmap
              (\objectValue -> (categoryObjectIdentifier objectValue, objectValue))
              (categoryObjects categoryProjection)
          )
  pictureBounds <- boundsFromPositions (meshVertexPosition <$> meshVerticesValue)
  resolvedEdges <- traverse (resolveMeshEdge vertexPositions) (meshEdges meshProjection)
  resolvedFaces <- traverse (resolveMeshFace vertexPositions) (meshFaces meshProjection)
  resolvedSimplices <-
    traverse
      (resolveNerveSimplex categoryObjectsByIdentifier)
      (nerveTwoSimplices nerveProjection)
  flagSelection <- selectFlag categoryObjectsByIdentifier (nerveTwoSimplices nerveProjection)
  pure
    PictureContext
      { contextExhibit = exhibit,
        contextBounds = pictureBounds,
        contextEdges = resolvedEdges,
        contextFaces = resolvedFaces,
        contextNerveSimplices = resolvedSimplices,
        contextFlag = flagSelection
      }

boundsFromPositions :: [Position] -> Either PictureObstruction PictureBounds
boundsFromPositions positions =
  case positions of
    [] -> Left PictureHasNoMeshVertices
    firstPosition : remainingPositions ->
      Right
        ( foldl'
            extendBounds
            ( PictureBounds
                (positionX firstPosition)
                (positionY firstPosition)
                (positionX firstPosition)
                (positionY firstPosition)
            )
            remainingPositions
        )

extendBounds :: PictureBounds -> Position -> PictureBounds
extendBounds bounds positionValue =
  PictureBounds
    { minimumX = min (minimumX bounds) (positionX positionValue),
      minimumY = min (minimumY bounds) (positionY positionValue),
      maximumX = max (maximumX bounds) (positionX positionValue),
      maximumY = max (maximumY bounds) (positionY positionValue)
    }

resolveMeshEdge ::
  Map.Map String Position ->
  MeshEdge ->
  Either PictureObstruction ResolvedMeshEdge
resolveMeshEdge vertexPositions edgeValue =
  case meshEdgeVertexIdentifiers edgeValue of
    [sourceIdentifier, targetIdentifier] ->
      ResolvedMeshEdge edgeValue
        <$> lookupMeshVertex vertexPositions sourceIdentifier
        <*> lookupMeshVertex vertexPositions targetIdentifier
    identifiers ->
      Left (PictureMalformedMeshEdge (meshEdgeIdentifier edgeValue) identifiers)

resolveMeshFace ::
  Map.Map String Position ->
  MeshFace ->
  Either PictureObstruction ResolvedMeshFace
resolveMeshFace vertexPositions faceValue =
  case meshFaceVertexIdentifiers faceValue of
    [firstIdentifier, secondIdentifier, thirdIdentifier] ->
      ResolvedMeshFace faceValue
        <$> traverse
          (lookupMeshVertex vertexPositions)
          [firstIdentifier, secondIdentifier, thirdIdentifier]
    identifiers ->
      Left (PictureMalformedMeshFace (meshFaceIdentifier faceValue) identifiers)

lookupMeshVertex ::
  Map.Map String Position ->
  String ->
  Either PictureObstruction Position
lookupMeshVertex vertexPositions identifier =
  maybe
    (Left (PictureMissingMeshVertex identifier))
    Right
    (Map.lookup identifier vertexPositions)

resolveNerveSimplex ::
  Map.Map String CategoryObject ->
  NerveTwoSimplex ->
  Either PictureObstruction ResolvedNerveSimplex
resolveNerveSimplex categoryObjectsByIdentifier simplexValue =
  case nerveSimplexObjectIdentifiers simplexValue of
    [faceIdentifier, edgeIdentifier, vertexIdentifier] ->
      ResolvedNerveSimplex simplexValue
        . fmap categoryObjectPosition
        <$> traverse
          (lookupCategoryObject categoryObjectsByIdentifier)
          [faceIdentifier, edgeIdentifier, vertexIdentifier]
    identifiers ->
      Left
        ( PictureMalformedNerveTwoSimplex
            (nerveSimplexIdentifier simplexValue)
            identifiers
        )

selectFlag ::
  Map.Map String CategoryObject ->
  [NerveTwoSimplex] ->
  Either PictureObstruction FlagSelection
selectFlag categoryObjectsByIdentifier simplices =
  case simplices of
    [] -> Left PictureHasNoNerveTwoSimplex
    simplexValue : _ ->
      case nerveSimplexObjectIdentifiers simplexValue of
        identifiers@[faceIdentifier, edgeIdentifier, vertexIdentifier] -> do
          faceObject <- lookupCategoryObject categoryObjectsByIdentifier faceIdentifier
          edgeObject <- lookupCategoryObject categoryObjectsByIdentifier edgeIdentifier
          vertexObject <- lookupCategoryObject categoryObjectsByIdentifier vertexIdentifier
          let kinds = categoryObjectKind <$> [faceObject, edgeObject, vertexObject]
          case kinds of
            [FaceObject, EdgeObject, VertexObject] ->
              Right
                FlagSelection
                  { selectedSimplexIdentifier = nerveSimplexIdentifier simplexValue,
                    selectedFace = faceObject,
                    selectedEdge = edgeObject,
                    selectedVertex = vertexObject,
                    selectedObjectIdentifiers = Set.fromList identifiers
                  }
            _ -> Left (PictureMalformedFlagKinds kinds)
        identifiers ->
          Left
            ( PictureMalformedNerveTwoSimplex
                (nerveSimplexIdentifier simplexValue)
                identifiers
            )

lookupCategoryObject ::
  Map.Map String CategoryObject ->
  String ->
  Either PictureObstruction CategoryObject
lookupCategoryObject categoryObjectsByIdentifier identifier =
  maybe
    (Left (PictureMissingCategoryObject identifier))
    Right
    (Map.lookup identifier categoryObjectsByIdentifier)

renderPicture :: PictureContext -> String
renderPicture context =
  unlines
    ( renderDocumentOpening
        <> renderHeader context
        <> renderPanels context
        <> renderNerveProfile context
        <> ["</svg>"]
    )

renderDocumentOpening :: [String]
renderDocumentOpening =
  [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1440 920\" role=\"img\" aria-labelledby=\"title description\">",
    "<title id=\"title\">Moonlight category observatory</title>",
    "<desc id=\"description\">One checked Delaunay section shown as geometry, a local face-edge-vertex incidence flag, and the full path-sensitive flag nerve.</desc>",
    "<defs>",
    "  <linearGradient id=\"background\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"1\"><stop stop-color=\"#10262a\"/><stop offset=\"0.48\" stop-color=\"#071217\"/><stop offset=\"1\" stop-color=\"#120f20\"/></linearGradient>",
    "  <radialGradient id=\"cyan-glow\"><stop stop-color=\"#72d6c2\" stop-opacity=\"0.22\"/><stop offset=\"1\" stop-color=\"#72d6c2\" stop-opacity=\"0\"/></radialGradient>",
    "  <radialGradient id=\"violet-glow\"><stop stop-color=\"#bc8cff\" stop-opacity=\"0.22\"/><stop offset=\"1\" stop-color=\"#bc8cff\" stop-opacity=\"0\"/></radialGradient>",
    "  <pattern id=\"grid\" width=\"28\" height=\"28\" patternUnits=\"userSpaceOnUse\"><path d=\"M28 0H0V28\" fill=\"none\" stroke=\"#9bc7bd\" stroke-opacity=\"0.07\" stroke-width=\"1\"/></pattern>",
    "  <marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"8\" refY=\"5\" markerWidth=\"7\" markerHeight=\"7\" orient=\"auto-start-reverse\"><path d=\"M0 0L10 5L0 10Z\" fill=\"#72d6c2\"/></marker>",
    "  <filter id=\"soft-shadow\" x=\"-20%\" y=\"-20%\" width=\"140%\" height=\"140%\"><feDropShadow dx=\"0\" dy=\"8\" stdDeviation=\"12\" flood-color=\"#000\" flood-opacity=\"0.28\"/></filter>",
    "  <style><![CDATA[",
    "    .sans{font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,sans-serif}",
    "    .serif{font-family:Georgia,'Times New Roman',serif}",
    "    .mono{font-family:ui-monospace,SFMono-Regular,Menlo,monospace}",
    "    .panel{fill:#0a181d;fill-opacity:.93;stroke:#8bbeb2;stroke-opacity:.22;stroke-width:1.2}",
    "    .kicker{fill:#72d6c2;font-size:12px;font-weight:700;letter-spacing:2.8px}",
    "    .title{fill:#f1eee7;font-size:48px;font-weight:400;letter-spacing:-1.4px}",
    "    .subtitle{fill:#9ab0aa;font-size:16px}",
    "    .panel-number{fill:#72d6c2;font-size:11px;font-weight:700;letter-spacing:1.6px}",
    "    .panel-title{fill:#f1eee7;font-size:25px}",
    "    .callout-title{fill:#f1eee7;font-size:21px}",
    "    .panel-note{fill:#8ca49f;font-size:12px}",
    "    .metric-value{fill:#f1eee7;font-size:18px;font-weight:700}",
    "    .metric-label{fill:#8ca49f;font-size:9px;font-weight:700;letter-spacing:1.2px}",
    "    .mesh-face{fill:#72d6c2;fill-opacity:.035;stroke:none}",
    "    .mesh-face.selected{fill:#ffad66;fill-opacity:.28}",
    "    .mesh-edge{stroke:#72d6c2;stroke-opacity:.58;stroke-width:1.7}",
    "    .mesh-edge.selected{stroke:#ffad66;stroke-opacity:1;stroke-width:5}",
    "    .mesh-vertex{fill:#071217;stroke:#f2e37e;stroke-width:2.2}",
    "    .mesh-vertex.selected{fill:#f2e37e;stroke:#fff7b0;stroke-width:4}",
    "    .nerve-simplex{fill:#bc8cff;fill-opacity:.065;stroke:#bc8cff;stroke-opacity:.42;stroke-width:1.1}",
    "    .nerve-simplex.selected{fill:#ffad66;fill-opacity:.48;stroke:#ffd1a8;stroke-opacity:1;stroke-width:3}",
    "    .object-face{fill:#ffad66}.object-edge{fill:#72d6c2}.object-vertex{fill:#f2e37e}",
    "    .node-label{fill:#f1eee7;font-size:23px;font-weight:700}.node-kind{fill:#8ca49f;font-size:10px;font-weight:700;letter-spacing:1.4px}",
    "    .profile-label{fill:#8ca49f;font-size:11px;font-weight:700;letter-spacing:1px}.profile-value{fill:#f1eee7;font-size:12px;font-weight:700}",
    "  ]]></style>",
    "</defs>",
    "<rect width=\"1440\" height=\"920\" fill=\"url(#background)\"/>",
    "<circle cx=\"180\" cy=\"110\" r=\"290\" fill=\"url(#cyan-glow)\"/>",
    "<circle cx=\"1290\" cy=\"520\" r=\"360\" fill=\"url(#violet-glow)\"/>"
  ]

renderHeader :: PictureContext -> [String]
renderHeader context =
  [ svgText "mono kicker" 56 38 "start" "MOONLIGHT / FINITE DESCENT",
    svgText "serif title" 56 94 "start" "One section, three faithful projections",
    svgText
      "sans subtitle"
      57
      127
      "start"
      "Delaunay geometry → incidence category → path-sensitive flag nerve"
  ]
    <> concat
      ( zipWith
          renderMetric
          [56, 212, 368, 524, 680, 856]
          (pictureMetrics context)
      )
    <> [ "<path d=\"M1040 163H1378\" stroke=\"#72d6c2\" stroke-opacity=\".24\"/>",
         svgText "mono panel-note" 1378 168 "end" "ONE HIGHLIGHTED FLAG GLUES ALL THREE PANELS"
       ]

data PictureMetric = PictureMetric
  { metricValue :: !Int,
    metricLabel :: !String
  }

pictureMetrics :: PictureContext -> [PictureMetric]
pictureMetrics context =
  let exhibit = contextExhibit context
      meshProjection = mesh exhibit
      categoryProjection = category exhibit
      nerveProjection = nerve exhibit
   in [ PictureMetric (length (meshVertices meshProjection)) "VERTICES",
        PictureMetric (length (meshEdges meshProjection)) "EDGES",
        PictureMetric (length (meshFaces meshProjection)) "FACES",
        PictureMetric (length (categoryObjects categoryProjection)) "OBJECTS",
        PictureMetric (length (categoryMorphisms categoryProjection)) "MORPHISMS",
        PictureMetric (length (nerveTwoSimplices nerveProjection)) "FLAGS"
      ]

renderMetric :: Double -> PictureMetric -> [String]
renderMetric x metric =
  [ "<g transform=\"translate(" <> coordinate x <> " 151)\">",
    "  <rect width=\"136\" height=\"48\" rx=\"13\" fill=\"#0b1b20\" stroke=\"#8bbeb2\" stroke-opacity=\".17\"/>",
    "  " <> svgText "sans metric-value" 16 22 "start" (show (metricValue metric)),
    "  " <> svgText "mono metric-label" 16 38 "start" (metricLabel metric),
    "</g>"
  ]

renderPanels :: PictureContext -> [String]
renderPanels context =
  renderPanelBackgrounds
    <> renderGeometryPanel context
    <> renderCategoryPanel (contextFlag context)
    <> renderNervePanel context

renderPanelBackgrounds :: [String]
renderPanelBackgrounds =
  concatMap
    (\x -> ["<rect class=\"panel\" x=\"" <> coordinate x <> "\" y=\"222\" width=\"410\" height=\"470\" rx=\"22\" filter=\"url(#soft-shadow)\"/>"])
    [50, 515, 980]

renderGeometryPanel :: PictureContext -> [String]
renderGeometryPanel context =
  renderPanelHeader 50 "01" "Geometric section" "the admitted Delaunay cells"
    <> ["<rect x=\"70\" y=\"326\" width=\"370\" height=\"330\" rx=\"15\" fill=\"url(#grid)\"/>"]
    <> fmap (renderMeshFace context geometryFrame) (contextFaces context)
    <> fmap (renderMeshEdge context geometryFrame) (contextEdges context)
    <> fmap (renderMeshVertex context geometryFrame) (meshVertices (mesh (contextExhibit context)))
    <> renderGeometrySelectionLabels context

geometryFrame :: PictureFrame
geometryFrame = PictureFrame 83 341 344 290

renderMeshFace :: PictureContext -> PictureFrame -> ResolvedMeshFace -> String
renderMeshFace context frame faceValue =
  let selected =
        Set.member
          (meshFaceObjectIdentifier (resolvedFace faceValue))
          (selectedObjectIdentifiers (contextFlag context))
   in "<polygon class=\"mesh-face"
        <> selectedClass selected
        <> "\" points=\""
        <> renderPointList (contextBounds context) frame (resolvedFaceVertices faceValue)
        <> "\"/>"

renderMeshEdge :: PictureContext -> PictureFrame -> ResolvedMeshEdge -> String
renderMeshEdge context frame edgeValue =
  let selected =
        Set.member
          (meshEdgeObjectIdentifier (resolvedEdge edgeValue))
          (selectedObjectIdentifiers (contextFlag context))
      (sourceX, sourceY) = projectPosition (contextBounds context) frame (resolvedEdgeSource edgeValue)
      (targetX, targetY) = projectPosition (contextBounds context) frame (resolvedEdgeTarget edgeValue)
   in "<line class=\"mesh-edge"
        <> selectedClass selected
        <> "\" x1=\""
        <> coordinate sourceX
        <> "\" y1=\""
        <> coordinate sourceY
        <> "\" x2=\""
        <> coordinate targetX
        <> "\" y2=\""
        <> coordinate targetY
        <> "\"/>"

renderMeshVertex :: PictureContext -> PictureFrame -> MeshVertex -> String
renderMeshVertex context frame vertexValue =
  let selected =
        Set.member
          (meshVertexObjectIdentifier vertexValue)
          (selectedObjectIdentifiers (contextFlag context))
      (x, y) = projectPosition (contextBounds context) frame (meshVertexPosition vertexValue)
   in "<circle class=\"mesh-vertex"
        <> selectedClass selected
        <> "\" cx=\""
        <> coordinate x
        <> "\" cy=\""
        <> coordinate y
        <> "\" r=\""
        <> if selected then "7\"/>" else "4.5\"/>"

renderGeometrySelectionLabels :: PictureContext -> [String]
renderGeometrySelectionLabels context =
  let flag = contextFlag context
      labelledObjects = [selectedFace flag, selectedEdge flag, selectedVertex flag]
   in fmap
        (\objectValue ->
           let (x, y) = projectPosition (contextBounds context) geometryFrame (categoryObjectPosition objectValue)
            in svgText
                 "mono profile-value"
                 (x + 10)
                 (y - 10)
                 "start"
                 (categoryObjectLabel objectValue)
        )
        labelledObjects

renderCategoryPanel :: FlagSelection -> [String]
renderCategoryPanel flag =
  renderPanelHeader 515 "02" "Incidence category" "one local section; no arrow thicket"
    <> [ svgText "mono panel-note" 720 334 "middle" "SELECTED NONDEGENERATE CHAIN",
         "<path d=\"M621 463H686\" stroke=\"#72d6c2\" stroke-width=\"2.5\" marker-end=\"url(#arrow)\"/>",
         "<path d=\"M754 463H817\" stroke=\"#72d6c2\" stroke-width=\"2.5\" marker-end=\"url(#arrow)\"/>",
         "<path d=\"M609 421C665 365 794 365 850 421\" fill=\"none\" stroke=\"#bc8cff\" stroke-opacity=\".68\" stroke-width=\"1.6\" stroke-dasharray=\"6 7\" marker-end=\"url(#arrow)\"/>",
         "<rect x=\"556\" y=\"430\" width=\"66\" height=\"66\" rx=\"14\" fill=\"#ffad66\" fill-opacity=\".18\" stroke=\"#ffad66\" stroke-width=\"2\"/>",
         "<rect x=\"696\" y=\"439\" width=\"48\" height=\"48\" transform=\"rotate(45 720 463)\" fill=\"#72d6c2\" fill-opacity=\".16\" stroke=\"#72d6c2\" stroke-width=\"2\"/>",
         "<circle cx=\"850\" cy=\"463\" r=\"31\" fill=\"#f2e37e\" fill-opacity=\".14\" stroke=\"#f2e37e\" stroke-width=\"2\"/>",
         svgText "mono node-kind" 589 407 "middle" "FACE OBJECT",
         svgText "sans node-label" 589 471 "middle" (categoryObjectLabel (selectedFace flag)),
         svgText "mono node-kind" 720 407 "middle" "EDGE OBJECT",
         svgText "sans node-label" 720 471 "middle" (categoryObjectLabel (selectedEdge flag)),
         svgText "mono node-kind" 850 407 "middle" "VERTEX OBJECT",
         svgText "sans node-label" 850 471 "middle" (categoryObjectLabel (selectedVertex flag)),
         svgText "mono panel-note" 720 535 "middle" "face → edge → vertex",
         "<rect x=\"552\" y=\"570\" width=\"336\" height=\"78\" rx=\"14\" fill=\"#071217\" stroke=\"#bc8cff\" stroke-opacity=\".28\"/>",
         svgText "serif callout-title" 720 602 "middle" "The intermediate edge survives.",
         svgText "sans panel-note" 720 628 "middle" "Same endpoints, different paths, distinct flags."
       ]

renderNervePanel :: PictureContext -> [String]
renderNervePanel context =
  renderPanelHeader 980 "03" "Flag nerve" "all 66 admitted two-simplices"
    <> ["<rect x=\"1000\" y=\"326\" width=\"370\" height=\"330\" rx=\"15\" fill=\"url(#grid)\"/>"]
    <> fmap (renderNerveSimplex context nerveFrame) (contextNerveSimplices context)
    <> fmap (renderNerveObject context nerveFrame) (categoryObjects (category (contextExhibit context)))
    <> renderNerveSelectionLabel context

nerveFrame :: PictureFrame
nerveFrame = PictureFrame 1013 341 344 290

renderNerveSimplex :: PictureContext -> PictureFrame -> ResolvedNerveSimplex -> String
renderNerveSimplex context frame simplexValue =
  let selected =
        nerveSimplexIdentifier (resolvedSimplex simplexValue)
          == selectedSimplexIdentifier (contextFlag context)
   in "<polygon class=\"nerve-simplex"
        <> selectedClass selected
        <> "\" points=\""
        <> renderPointList (contextBounds context) frame (resolvedSimplexVertices simplexValue)
        <> "\"/>"

renderNerveObject :: PictureContext -> PictureFrame -> CategoryObject -> String
renderNerveObject context frame objectValue =
  let selected =
        Set.member
          (categoryObjectIdentifier objectValue)
          (selectedObjectIdentifiers (contextFlag context))
      (x, y) = projectPosition (contextBounds context) frame (categoryObjectPosition objectValue)
      radius = if selected then 6 else 3.4
   in "<circle class=\""
        <> objectKindClass (categoryObjectKind objectValue)
        <> "\" cx=\""
        <> coordinate x
        <> "\" cy=\""
        <> coordinate y
        <> "\" r=\""
        <> coordinate radius
        <> "\" opacity=\""
        <> if selected then "1\" stroke=\"#fff\" stroke-width=\"2\"/>" else ".78\"/>"

renderNerveSelectionLabel :: PictureContext -> [String]
renderNerveSelectionLabel context =
  let flag = contextFlag context
      faceObject = selectedFace flag
      (x, y) = projectPosition (contextBounds context) nerveFrame (categoryObjectPosition faceObject)
   in [ svgText "mono profile-value" (x + 12) (y - 12) "start" "selected flag",
        svgText "mono panel-note" 1352 642 "end" (selectedSimplexIdentifier flag)
      ]

renderPanelHeader :: Double -> String -> String -> String -> [String]
renderPanelHeader x number titleText note =
  [ svgText "mono panel-number" (x + 22) 254 "start" number,
    svgText "serif panel-title" (x + 22) 282 "start" titleText,
    svgText "sans panel-note" (x + 22) 304 "start" note,
    "<path d=\"M"
      <> coordinate (x + 20)
      <> " 314H"
      <> coordinate (x + 390)
      <> "\" stroke=\"#8bbeb2\" stroke-opacity=\".14\"/>"
  ]

renderNerveProfile :: PictureContext -> [String]
renderNerveProfile context =
  let counts = nerveSimplexCounts (nerve (contextExhibit context))
      maximumCount =
        foldl'
          (\current count -> max current (unnormalizedSimplexCount count))
          1
          counts
   in [ "<rect x=\"50\" y=\"720\" width=\"1340\" height=\"156\" rx=\"22\" fill=\"#09171c\" stroke=\"#8bbeb2\" stroke-opacity=\".20\"/>",
        svgText "mono kicker" 76 752 "start" "NORMALIZATION PROFILE",
        svgText "serif panel-title" 76 789 "start" "Identity insertions are cost, not geometry.",
        svgText "sans panel-note" 76 817 "start" "Identity insertions swell dimension two to 385.",
        "<rect x=\"76\" y=\"838\" width=\"12\" height=\"4\" rx=\"2\" fill=\"#72d6c2\"/>",
        svgText "mono profile-label" 96 845 "start" "NORMALIZED",
        "<rect x=\"196\" y=\"838\" width=\"12\" height=\"4\" rx=\"2\" fill=\"#bc8cff\"/>",
        svgText "mono profile-label" 216 845 "start" "UNNORMALIZED"
      ]
        <> concat
          ( zipWith
              (renderProfileRow maximumCount)
              [770, 814, 858]
              counts
          )

renderProfileRow :: Int -> Double -> NerveSimplexCount -> [String]
renderProfileRow maximumCount y count =
  let normalizedWidth = profileWidth maximumCount (normalizedSimplexCount count)
      unnormalizedWidth = profileWidth maximumCount (unnormalizedSimplexCount count)
   in [ svgText "mono profile-label" 590 (y + 5) "end" ("DIM " <> show (nerveDimension count)),
        "<rect x=\"615\" y=\"" <> coordinate (y - 8) <> "\" width=\"230\" height=\"11\" rx=\"5.5\" fill=\"#13272d\"/>",
        "<rect x=\"615\" y=\"" <> coordinate (y - 8) <> "\" width=\"" <> coordinate normalizedWidth <> "\" height=\"11\" rx=\"5.5\" fill=\"#72d6c2\"/>",
        svgText "mono profile-value" 858 (y + 3) "start" (show (normalizedSimplexCount count)),
        "<rect x=\"965\" y=\"" <> coordinate (y - 8) <> "\" width=\"310\" height=\"11\" rx=\"5.5\" fill=\"#1d1830\"/>",
        "<rect x=\"965\" y=\"" <> coordinate (y - 8) <> "\" width=\"" <> coordinate (unnormalizedWidth * 310 / 230) <> "\" height=\"11\" rx=\"5.5\" fill=\"#bc8cff\"/>",
        svgText "mono profile-value" 1288 (y + 3) "start" (show (unnormalizedSimplexCount count))
      ]

profileWidth :: Int -> Int -> Double
profileWidth maximumCount count =
  230 * fromIntegral count / fromIntegral maximumCount

projectPosition :: PictureBounds -> PictureFrame -> Position -> (Double, Double)
projectPosition bounds frame positionValue =
  ( frameLeft frame
      + frameWidth frame
        * normalizedCoordinate (positionX positionValue) (minimumX bounds) (maximumX bounds),
    frameTop frame
      + frameHeight frame
        * normalizedCoordinate (positionY positionValue) (minimumY bounds) (maximumY bounds)
  )

normalizedCoordinate :: Double -> Double -> Double -> Double
normalizedCoordinate value minimumValue maximumValue =
  if maximumValue == minimumValue
    then 0.5
    else (value - minimumValue) / (maximumValue - minimumValue)

renderPointList :: PictureBounds -> PictureFrame -> [Position] -> String
renderPointList bounds frame =
  unwords
    . fmap
      (\positionValue ->
         let (x, y) = projectPosition bounds frame positionValue
          in coordinate x <> "," <> coordinate y
      )

selectedClass :: Bool -> String
selectedClass selected = if selected then " selected" else ""

objectKindClass :: CategoryObjectKind -> String
objectKindClass = \case
  FaceObject -> "object-face"
  EdgeObject -> "object-edge"
  VertexObject -> "object-vertex"

svgText :: String -> Double -> Double -> String -> String -> String
svgText classes x y anchor contents =
  "<text class=\""
    <> classes
    <> "\" x=\""
    <> coordinate x
    <> "\" y=\""
    <> coordinate y
    <> "\" text-anchor=\""
    <> anchor
    <> "\">"
    <> escapeXml contents
    <> "</text>"

coordinate :: Double -> String
coordinate value = showFFloat (Just 1) value ""

escapeXml :: String -> String
escapeXml = concatMap escapeCharacter

escapeCharacter :: Char -> String
escapeCharacter = \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '\"' -> "&quot;"
  '\'' -> "&apos;"
  character -> [character]
