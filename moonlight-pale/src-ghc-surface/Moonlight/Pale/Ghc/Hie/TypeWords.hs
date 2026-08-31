{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-| Canonical word encodings of HIE type graphs. -}
module Moonlight.Pale.Ghc.Hie.TypeWords
  ( TypeWords,
    TypeWord (..),
    TypeWordOpcode (..),
    TypeArgumentVisibility (..),
    TypeVariableFlavor (..),
    TypeGraphObstruction (..),
    TypeWireFailure (..),
    typeWords,
    typeWordsList,
    tyConTypeWords,
    hieTypeIndexTypeWords,
    hieTypeRootsTypeWords,
  )
where

import Control.Monad (foldM)
import Data.Array (Array, bounds, elems, inRange)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import GHC.Iface.Ext.Types (HieArgs (..), HieType (..), HieTypeFlat, TypeIndex)
import GHC.Iface.Type (IfaceTyCon, IfaceTyLit (..), ifaceTyConInfo, ifaceTyConName)
import GHC.Types.Name (Name, nameUnique)
import GHC.Types.Unique (getKey)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Numeric.Natural (Natural)
import Moonlight.Pale.Ghc.Hie.TypeWords.Internal

type TypeGraphObstruction :: Type
data TypeGraphObstruction
  = MissingTypeIndex !TypeIndex
  | CyclicTypeIndex !TypeIndex
  | MissingCompiledTypeNode !Natural
  | MissingTypeVariableScopeSection !Natural
  | EscapedBoundTypeVariable !TypeIndex !Word64
  deriving stock (Eq, Ord, Show)

newtype TypeNodeId = TypeNodeId Natural
  deriving stock (Eq, Ord, Show)

newtype TypeBinderId = TypeBinderId Natural
  deriving stock (Eq, Ord, Show)

newtype BinderScopeId = BinderScopeId Natural
  deriving stock (Eq, Ord, Show)

data TypeArgument = TypeArgument
  { taVisibility :: !TypeArgumentVisibility,
    taNode :: !TypeNodeId
  }
  deriving stock (Eq, Ord, Show)

data TypeVariableReference
  = BoundTypeVariable !TypeBinderId
  | FreeTypeVariable !Name
  deriving stock (Eq, Ord)

instance Show TypeVariableReference where
  showsPrec precedence = \case
    BoundTypeVariable binderId ->
      showParen (precedence > 10) (showString "BoundTypeVariable " . showsPrec 11 binderId)
    FreeTypeVariable nameValue ->
      showParen (precedence > 10) (showString "FreeTypeVariable " . showsPrec 11 (getKey (nameUnique nameValue)))

data TypeNode
  = TypeApplication !TypeNodeId ![TypeArgument]
  | TypeFunction !TypeNodeId !TypeNodeId !TypeNodeId
  | TypeQualified !TypeNodeId !TypeNodeId
  | TypeForAll !TypeBinderId !TypeNodeId !String !TypeNodeId
  | TypeVariable !TypeVariableReference
  | TypeCast !TypeNodeId
  | TypeCoercion
  | TypeConstructor !IfaceTyCon ![TypeArgument]
  | TypeLiteral !IfaceTyLit
  deriving stock (Eq, Ord)

data FlatTypeGraph = FlatTypeGraph
  { ftBounds :: !(TypeIndex, TypeIndex),
    ftNodes :: !(Vector HieTypeFlat)
  }

data BinderScope = BinderScope
  { bsId :: !BinderScopeId,
    bsNames :: !(Map Name TypeBinderId),
    bsDepth :: !Natural
  }

data ScopedTypeKey = ScopedTypeKey !TypeIndex !BinderScopeId
  deriving stock (Eq, Ord)

data TypeVariableScopeEvidence
  = ObservedFreeTypeVariable !Word64
  | ObservedBoundTypeVariable !Word64
  deriving stock (Eq, Ord)

newtype TypeVariableScopeSectionId =
  TypeVariableScopeSectionId Natural
  deriving stock (Eq, Ord, Show)

data TypeVariableScopeSection
  = TypeVariableScopeLeaf !TypeIndex !TypeVariableScopeEvidence
  | TypeVariableScopeUnion
      !TypeVariableScopeSectionId
      !TypeVariableScopeSectionId
  deriving stock (Eq, Ord)

data TypeNodeSection node = TypeNodeSection
  { tnsNode :: !node,
    tnsVariableScopes :: !TypeVariableScopeSectionId
  }

-- Evidence-free entries remain absent from the sparse evidence map.  This
-- preserves the allocation profile of variable-free type graphs while keeping
-- node identity and its contextual evidence under one memo owner.
data ScopedTypeMemo = ScopedTypeMemo
  { stmNodes :: !(Map ScopedTypeKey TypeNodeId),
    stmVariableScopes :: !(Map ScopedTypeKey TypeVariableScopeSectionId),
    stmNextVariableScopeSection :: !Natural,
    stmVariableScopeSections ::
      !(Map TypeVariableScopeSectionId TypeVariableScopeSection),
    stmVariableScopeSectionIntern ::
      !(Map TypeVariableScopeSection TypeVariableScopeSectionId)
  }

data GraphBuild = GraphBuild
  { gbNextNode :: !Natural,
    gbNextScope :: !Natural,
    gbScopeIntern :: !(Map (BinderScopeId, Name, TypeBinderId) BinderScopeId),
    gbMemo :: {-# UNPACK #-} !ScopedTypeMemo,
    gbActive :: !(Set TypeIndex),
    gbNodes :: !(Map TypeNodeId TypeNode),
    gbIntern :: !(Map TypeNode TypeNodeId),
    gbVariableScopes :: !(Map TypeIndex TypeVariableScopeEvidence),
    gbReplayedVariableScopeSections :: !(Set TypeVariableScopeSectionId)
  }

tyConTypeWords :: String -> TypeWords
tyConTypeWords tyConName =
  trustedTypeWords
    [ TypeOpcode TypeGraphOpcode,
      TypeRootReference 0,
      TypeDefinitionCount 1,
      TypeDefinitionId 0,
      TypeOpcode TypeTyConAppOpcode,
      TypeOutputText tyConName,
      TypeArgumentCount 0
    ]

hieTypeIndexTypeWords :: Array TypeIndex HieTypeFlat -> TypeIndex -> Either TypeGraphObstruction TypeWords
hieTypeIndexTypeWords typeTable rootIndex =
  maybe
    (Left (MissingTypeIndex rootIndex))
    id
    (Map.lookup rootIndex (hieTypeRootsTypeWords typeTable (Set.singleton rootIndex)))

hieTypeRootsTypeWords ::
  Array TypeIndex HieTypeFlat ->
  Set TypeIndex ->
  Map TypeIndex (Either TypeGraphObstruction TypeWords)
hieTypeRootsTypeWords typeTable rootIndices =
  let flatTypeGraph =
        FlatTypeGraph
          { ftBounds = bounds typeTable,
            ftNodes = Vector.fromList (elems typeTable)
          }
      (compiledRoots, completedBuild) =
        Set.foldl'
          (compileRoot flatTypeGraph)
          (Map.empty, emptyGraphBuild)
          rootIndices
   in fmap
        (>>= renderCompiledRoot (gbNodes completedBuild))
        compiledRoots

emptyScopedTypeMemo :: ScopedTypeMemo
emptyScopedTypeMemo =
  ScopedTypeMemo
    { stmNodes = Map.empty,
      stmVariableScopes = Map.empty,
      stmNextVariableScopeSection = 1,
      stmVariableScopeSections = Map.empty,
      stmVariableScopeSectionIntern = Map.empty
    }

emptyTypeVariableScopeSectionId :: TypeVariableScopeSectionId
emptyTypeVariableScopeSectionId =
  TypeVariableScopeSectionId 0

lookupScopedTypeMemo ::
  ScopedTypeKey ->
  ScopedTypeMemo ->
  Maybe (TypeNodeSection TypeNodeId)
lookupScopedTypeMemo scopedKey scopedMemo =
  ( \nodeId ->
      TypeNodeSection
        { tnsNode = nodeId,
          tnsVariableScopes =
            Map.findWithDefault
              emptyTypeVariableScopeSectionId
              scopedKey
              (stmVariableScopes scopedMemo)
        }
  )
    <$> Map.lookup scopedKey (stmNodes scopedMemo)

insertScopedTypeMemo ::
  ScopedTypeKey ->
  TypeNodeSection TypeNodeId ->
  ScopedTypeMemo ->
  ScopedTypeMemo
insertScopedTypeMemo scopedKey nodeSection scopedMemo =
  ScopedTypeMemo
    { stmNodes =
        Map.insert scopedKey (tnsNode nodeSection) (stmNodes scopedMemo),
      stmVariableScopes =
        if tnsVariableScopes nodeSection == emptyTypeVariableScopeSectionId
          then Map.delete scopedKey (stmVariableScopes scopedMemo)
          else
            Map.insert
              scopedKey
              (tnsVariableScopes nodeSection)
              (stmVariableScopes scopedMemo),
      stmNextVariableScopeSection =
        stmNextVariableScopeSection scopedMemo,
      stmVariableScopeSections =
        stmVariableScopeSections scopedMemo,
      stmVariableScopeSectionIntern =
        stmVariableScopeSectionIntern scopedMemo
    }

internTypeVariableScopeSection ::
  TypeVariableScopeSection ->
  ScopedTypeMemo ->
  (TypeVariableScopeSectionId, ScopedTypeMemo)
internTypeVariableScopeSection variableScopeSection scopedMemo =
  case
      Map.lookup
        variableScopeSection
        (stmVariableScopeSectionIntern scopedMemo)
    of
    Just knownSectionId ->
      (knownSectionId, scopedMemo)
    Nothing ->
      let sectionId =
            TypeVariableScopeSectionId
              (stmNextVariableScopeSection scopedMemo)
       in ( sectionId,
            scopedMemo
              { stmNextVariableScopeSection =
                  stmNextVariableScopeSection scopedMemo + 1,
                stmVariableScopeSections =
                  Map.insert
                    sectionId
                    variableScopeSection
                    (stmVariableScopeSections scopedMemo),
                stmVariableScopeSectionIntern =
                  Map.insert
                    variableScopeSection
                    sectionId
                    (stmVariableScopeSectionIntern scopedMemo)
              }
          )

emptyGraphBuild :: GraphBuild
emptyGraphBuild =
  GraphBuild
        { gbNextNode = 0,
          gbNextScope = 1,
          gbScopeIntern = Map.empty,
          gbMemo = emptyScopedTypeMemo,
          gbActive = Set.empty,
          gbNodes = Map.empty,
          gbIntern = Map.empty,
          gbVariableScopes = Map.empty,
          gbReplayedVariableScopeSections = Set.empty
        }

compileRoot ::
  FlatTypeGraph ->
  (Map TypeIndex (Either TypeGraphObstruction TypeNodeId), GraphBuild) ->
  TypeIndex ->
  (Map TypeIndex (Either TypeGraphObstruction TypeNodeId), GraphBuild)
compileRoot typeGraph (compiledRoots, graphBuild) rootIndex =
  case
      buildTypeNode
        typeGraph
        BinderScope
          { bsId = BinderScopeId 0,
            bsNames = Map.empty,
            bsDepth = 0
          }
        rootIndex
        graphBuild
          { gbActive = Set.empty,
            gbVariableScopes = Map.empty,
            gbReplayedVariableScopeSections = Set.empty
          }
    of
    Left obstruction ->
      (Map.insert rootIndex (Left obstruction) compiledRoots, graphBuild)
    Right (rootSection, nextBuild) ->
      (Map.insert rootIndex (Right (tnsNode rootSection)) compiledRoots, nextBuild)

buildTypeNode ::
  FlatTypeGraph ->
  BinderScope ->
  TypeIndex ->
  GraphBuild ->
  Either TypeGraphObstruction (TypeNodeSection TypeNodeId, GraphBuild)
buildTypeNode typeGraph binderScope typeIndex graphBuild =
  case flatTypeAt typeGraph typeIndex of
    Nothing ->
      Left (MissingTypeIndex typeIndex)
    Just flatType
      | Set.member typeIndex (gbActive graphBuild) ->
          Left (CyclicTypeIndex typeIndex)
      | Just knownSection <- lookupScopedTypeMemo scopedKey (gbMemo graphBuild) -> do
          scopedBuild <-
            replayTypeVariableScopeSection
              (tnsVariableScopes knownSection)
              graphBuild
          pure (knownSection, scopedBuild)
      | otherwise -> do
          let activeBuild =
                graphBuild
                  { gbActive = Set.insert typeIndex (gbActive graphBuild)
                  }
          (nodeSection, descendantBuild) <-
            buildFlatType typeGraph binderScope typeIndex flatType activeBuild
          let inactiveBuild =
                descendantBuild
                  { gbActive = Set.delete typeIndex (gbActive descendantBuild)
                  }
              nodeValue =
                tnsNode nodeSection
          case Map.lookup nodeValue (gbIntern inactiveBuild) of
            Just internedNode ->
              let internedSection =
                    nodeSection {tnsNode = internedNode}
               in pure
                    ( internedSection,
                      inactiveBuild
                        { gbMemo =
                            insertScopedTypeMemo
                              scopedKey
                              internedSection
                              (gbMemo inactiveBuild)
                        }
                    )
            Nothing ->
              let nodeId =
                    TypeNodeId (gbNextNode inactiveBuild)
                  compiledSection =
                    nodeSection {tnsNode = nodeId}
               in pure
                    ( compiledSection,
                      inactiveBuild
                        { gbNextNode = gbNextNode inactiveBuild + 1,
                          gbMemo =
                            insertScopedTypeMemo
                              scopedKey
                              compiledSection
                              (gbMemo inactiveBuild),
                          gbNodes = Map.insert nodeId nodeValue (gbNodes inactiveBuild),
                          gbIntern = Map.insert nodeValue nodeId (gbIntern inactiveBuild)
                        }
                    )
  where
    scopedKey =
      ScopedTypeKey typeIndex (bsId binderScope)

flatTypeAt :: FlatTypeGraph -> TypeIndex -> Maybe HieTypeFlat
flatTypeAt typeGraph typeIndex
  | inRange (ftBounds typeGraph) typeIndex =
      let (lowerBound, _) = ftBounds typeGraph
       in ftNodes typeGraph Vector.!? fromIntegral (typeIndex - lowerBound)
  | otherwise =
      Nothing

buildFlatType ::
  FlatTypeGraph ->
  BinderScope ->
  TypeIndex ->
  HieTypeFlat ->
  GraphBuild ->
  Either TypeGraphObstruction (TypeNodeSection TypeNode, GraphBuild)
buildFlatType typeGraph binderScope typeIndex flatType =
  case flatType of
    HAppTy functionType argumentTypes ->
      buildNodeThenArguments TypeApplication functionType argumentTypes
    HFunTy multiplicityType argumentType resultType ->
      buildThree TypeFunction multiplicityType argumentType resultType
    HQualTy predicateType bodyType ->
      buildTwo TypeQualified predicateType bodyType
    HForAllTy ((binderName, binderKind), flagValue) bodyType ->
      buildForAll binderName binderKind flagValue bodyType
    HTyVarTy nameValue ->
      \graphBuild -> do
        let binderReference =
              Map.lookup nameValue (bsNames binderScope)
        (variableScopes, scopedBuild) <-
          observeTypeVariable
            typeIndex
            nameValue
            binderReference
            graphBuild
        pure
          ( TypeNodeSection
              { tnsNode =
                  TypeVariable
                    ( maybe
                        (FreeTypeVariable nameValue)
                        BoundTypeVariable
                        binderReference
                    ),
                tnsVariableScopes = variableScopes
              },
            scopedBuild
          )
    HCastTy castType ->
      buildOne TypeCast castType
    HCoercionTy ->
      \graphBuild ->
        Right
          ( TypeNodeSection
              { tnsNode = TypeCoercion,
                tnsVariableScopes = emptyTypeVariableScopeSectionId
              },
            graphBuild
          )
    HTyConApp tyCon argumentTypes ->
      buildArguments (TypeConstructor tyCon) argumentTypes
    HLitTy literalType ->
      \graphBuild ->
        Right
          ( TypeNodeSection
              { tnsNode = TypeLiteral literalType,
                tnsVariableScopes = emptyTypeVariableScopeSectionId
              },
            graphBuild
          )
  where
    buildOne constructor childIndex build = do
      (childSection, nextBuild) <-
        buildTypeNode typeGraph binderScope childIndex build
      pure
        ( TypeNodeSection
            { tnsNode = constructor (tnsNode childSection),
              tnsVariableScopes = tnsVariableScopes childSection
            },
          nextBuild
        )

    buildTwo constructor firstIndex secondIndex build = do
      (firstSection, afterFirst) <-
        buildTypeNode typeGraph binderScope firstIndex build
      (secondSection, afterSecond) <-
        buildTypeNode typeGraph binderScope secondIndex afterFirst
      let (variableScopes, gluedBuild) =
            glueTypeVariableScopeSections
              (tnsVariableScopes firstSection)
              (tnsVariableScopes secondSection)
              afterSecond
      pure
        ( TypeNodeSection
            { tnsNode =
                constructor
                  (tnsNode firstSection)
                  (tnsNode secondSection),
              tnsVariableScopes = variableScopes
            },
          gluedBuild
        )

    buildThree constructor firstIndex secondIndex thirdIndex build = do
      (firstSection, afterFirst) <-
        buildTypeNode typeGraph binderScope firstIndex build
      (secondSection, afterSecond) <-
        buildTypeNode typeGraph binderScope secondIndex afterFirst
      (thirdSection, afterThird) <-
        buildTypeNode typeGraph binderScope thirdIndex afterSecond
      let (firstAndSecondScopes, afterFirstGlue) =
            glueTypeVariableScopeSections
              (tnsVariableScopes firstSection)
              (tnsVariableScopes secondSection)
              afterThird
          (variableScopes, gluedBuild) =
            glueTypeVariableScopeSections
              firstAndSecondScopes
              (tnsVariableScopes thirdSection)
              afterFirstGlue
      pure
        ( TypeNodeSection
            { tnsNode =
                constructor
                  (tnsNode firstSection)
                  (tnsNode secondSection)
                  (tnsNode thirdSection),
              tnsVariableScopes = variableScopes
            },
          gluedBuild
        )

    buildArguments constructor arguments build = do
      (argumentSection, nextBuild) <-
        buildHieArguments typeGraph binderScope arguments build
      pure
        ( TypeNodeSection
            { tnsNode = constructor (tnsNode argumentSection),
              tnsVariableScopes = tnsVariableScopes argumentSection
            },
          nextBuild
        )

    buildNodeThenArguments constructor functionIndex arguments build = do
      (functionSection, afterFunction) <-
        buildTypeNode typeGraph binderScope functionIndex build
      (argumentSection, afterArguments) <-
        buildHieArguments typeGraph binderScope arguments afterFunction
      let (variableScopes, gluedBuild) =
            glueTypeVariableScopeSections
              (tnsVariableScopes functionSection)
              (tnsVariableScopes argumentSection)
              afterArguments
      pure
        ( TypeNodeSection
            { tnsNode =
                constructor
                  (tnsNode functionSection)
                  (tnsNode argumentSection),
              tnsVariableScopes = variableScopes
            },
          gluedBuild
        )

    buildForAll binderName binderKind flagValue bodyType build = do
      let binderId = TypeBinderId (bsDepth binderScope)
      (kindSection, afterKind) <-
        buildTypeNode typeGraph binderScope binderKind build
      let (bodyScope, scopedBuild) =
            extendBinderScope binderScope binderName binderId afterKind
      (bodySection, afterBody) <-
        buildTypeNode
          typeGraph
          bodyScope
          bodyType
          scopedBuild
      let (variableScopes, gluedBuild) =
            glueTypeVariableScopeSections
              (tnsVariableScopes kindSection)
              (tnsVariableScopes bodySection)
              afterBody
      pure
        ( TypeNodeSection
            { tnsNode =
                TypeForAll
                  binderId
                  (tnsNode kindSection)
                  (outputString flagValue)
                  (tnsNode bodySection),
              tnsVariableScopes = variableScopes
            },
          gluedBuild
        )

extendBinderScope ::
  BinderScope ->
  Name ->
  TypeBinderId ->
  GraphBuild ->
  (BinderScope, GraphBuild)
extendBinderScope parentScope binderName binderId graphBuild =
  case Map.lookup transitionKey (gbScopeIntern graphBuild) of
    Just knownScopeId ->
      (bodyScope knownScopeId, graphBuild)
    Nothing ->
      let scopeId = BinderScopeId (gbNextScope graphBuild)
       in ( bodyScope scopeId,
            graphBuild
              { gbNextScope = gbNextScope graphBuild + 1,
                gbScopeIntern =
                  Map.insert transitionKey scopeId (gbScopeIntern graphBuild)
              }
          )
  where
    transitionKey =
      (bsId parentScope, binderName, binderId)

    bodyScope scopeId =
      BinderScope
        { bsId = scopeId,
          bsNames = Map.insert binderName binderId (bsNames parentScope),
          bsDepth = bsDepth parentScope + 1
        }

observeTypeVariable ::
  TypeIndex ->
  Name ->
  Maybe TypeBinderId ->
  GraphBuild ->
  Either TypeGraphObstruction (TypeVariableScopeSectionId, GraphBuild)
observeTypeVariable typeIndex nameValue binderReference graphBuild = do
  let binderIdentity =
        getKey (nameUnique nameValue)
      scopeEvidence =
        maybe
          (ObservedFreeTypeVariable binderIdentity)
          (const (ObservedBoundTypeVariable binderIdentity))
          binderReference
      (variableScopeSection, sectionedMemo) =
        internTypeVariableScopeSection
          (TypeVariableScopeLeaf typeIndex scopeEvidence)
          (gbMemo graphBuild)
      sectionedBuild =
        graphBuild {gbMemo = sectionedMemo}
  scopedBuild <-
    replayTypeVariableScopeSection variableScopeSection sectionedBuild
  pure (variableScopeSection, scopedBuild)

replayTypeVariableScopeSection ::
  TypeVariableScopeSectionId ->
  GraphBuild ->
  Either TypeGraphObstruction GraphBuild
replayTypeVariableScopeSection sectionId graphBuild
  | sectionId == emptyTypeVariableScopeSectionId =
      Right graphBuild
  | Set.member sectionId (gbReplayedVariableScopeSections graphBuild) =
      Right graphBuild
  | otherwise =
      case
          Map.lookup
            sectionId
            (stmVariableScopeSections (gbMemo graphBuild))
        of
        Nothing ->
          Left
            ( MissingTypeVariableScopeSection
                (typeVariableScopeSectionIdNatural sectionId)
            )
        Just variableScopeSection ->
          let markedBuild =
                graphBuild
                  { gbReplayedVariableScopeSections =
                      Set.insert
                        sectionId
                        (gbReplayedVariableScopeSections graphBuild)
                  }
           in case variableScopeSection of
                TypeVariableScopeLeaf typeIndex scopeEvidence ->
                  observeTypeVariableScope
                    typeIndex
                    scopeEvidence
                    markedBuild
                TypeVariableScopeUnion leftSection rightSection ->
                  replayTypeVariableScopeSection leftSection markedBuild
                    >>= replayTypeVariableScopeSection rightSection

observeTypeVariableScope ::
  TypeIndex ->
  TypeVariableScopeEvidence ->
  GraphBuild ->
  Either TypeGraphObstruction GraphBuild
observeTypeVariableScope typeIndex scopeEvidence graphBuild =
  case Map.lookup typeIndex (gbVariableScopes graphBuild) of
    Nothing ->
      Right
        graphBuild
          { gbVariableScopes =
              Map.insert
                typeIndex
                scopeEvidence
                (gbVariableScopes graphBuild)
          }
    Just knownEvidence
      | compatibleTypeVariableScopes knownEvidence scopeEvidence ->
          Right graphBuild
      | otherwise ->
          Left
            ( EscapedBoundTypeVariable
                typeIndex
                (typeVariableScopeIdentity scopeEvidence)
            )

glueTypeVariableScopeSections ::
  TypeVariableScopeSectionId ->
  TypeVariableScopeSectionId ->
  GraphBuild ->
  (TypeVariableScopeSectionId, GraphBuild)
-- Both children have already replayed successfully into the current root.
-- Gluing therefore composes canonical section ids without rescanning leaves.
glueTypeVariableScopeSections leftSection rightSection graphBuild
  | leftSection == emptyTypeVariableScopeSectionId =
      (rightSection, graphBuild)
  | rightSection == emptyTypeVariableScopeSectionId =
      (leftSection, graphBuild)
  | leftSection == rightSection =
      (leftSection, graphBuild)
  | otherwise =
      let (lowerSection, upperSection) =
            if leftSection <= rightSection
              then (leftSection, rightSection)
              else (rightSection, leftSection)
          (gluedSection, gluedMemo) =
            internTypeVariableScopeSection
              (TypeVariableScopeUnion lowerSection upperSection)
              (gbMemo graphBuild)
       in (gluedSection, graphBuild {gbMemo = gluedMemo})

compatibleTypeVariableScopes ::
  TypeVariableScopeEvidence ->
  TypeVariableScopeEvidence ->
  Bool
compatibleTypeVariableScopes leftEvidence rightEvidence =
  case (leftEvidence, rightEvidence) of
    (ObservedFreeTypeVariable _, ObservedFreeTypeVariable _) ->
      True
    (ObservedBoundTypeVariable _, ObservedBoundTypeVariable _) ->
      True
    _ ->
      False

typeVariableScopeIdentity :: TypeVariableScopeEvidence -> Word64
typeVariableScopeIdentity = \case
  ObservedFreeTypeVariable binderIdentity ->
    binderIdentity
  ObservedBoundTypeVariable binderIdentity ->
    binderIdentity

typeVariableScopeSectionIdNatural ::
  TypeVariableScopeSectionId ->
  Natural
typeVariableScopeSectionIdNatural (TypeVariableScopeSectionId sectionId) =
  sectionId

buildHieArguments ::
  FlatTypeGraph ->
  BinderScope ->
  HieArgs TypeIndex ->
  GraphBuild ->
  Either TypeGraphObstruction (TypeNodeSection [TypeArgument], GraphBuild)
buildHieArguments typeGraph binderScope (HieArgs arguments) =
  buildArguments arguments
  where
    buildArguments [] graphBuild =
      pure
        ( TypeNodeSection
            { tnsNode = [],
              tnsVariableScopes = emptyTypeVariableScopeSectionId
            },
          graphBuild
        )
    buildArguments ((visible, typeIndex) : remaining) graphBuild = do
      (argumentSection, afterArgument) <-
        buildTypeNode typeGraph binderScope typeIndex graphBuild
      (remainingSection, afterRemaining) <-
        buildArguments remaining afterArgument
      let (variableScopes, gluedBuild) =
            glueTypeVariableScopeSections
              (tnsVariableScopes argumentSection)
              (tnsVariableScopes remainingSection)
              afterRemaining
      pure
        ( TypeNodeSection
            { tnsNode =
                TypeArgument
                  { taVisibility =
                      if visible
                        then TypeArgumentVisible
                        else TypeArgumentHidden,
                    taNode = tnsNode argumentSection
                  }
                  : tnsNode remainingSection,
              tnsVariableScopes = variableScopes
            },
          gluedBuild
        )

data LocalNumbering = LocalNumbering
  { lnNextNode :: !Natural,
    lnNodeIds :: !(Map TypeNodeId TypeNodeId),
    lnTraversalOrder :: !(Seq TypeNodeId)
  }

renderCompiledRoot ::
  Map TypeNodeId TypeNode ->
  TypeNodeId ->
  Either TypeGraphObstruction TypeWords
renderCompiledRoot compiledNodes rootNode = do
  numbering <-
    numberReachableNode
      compiledNodes
      rootNode
      LocalNumbering
        { lnNextNode = 0,
          lnNodeIds = Map.empty,
          lnTraversalOrder = Seq.empty
        }
  localRoot <- localNodeId numbering rootNode
  definitions <-
    foldMap id
      <$> traverse
        (renderDefinition numbering compiledNodes)
        (toList (lnTraversalOrder numbering))
  pure
    ( trustedTypeWords
        ( [ TypeOpcode TypeGraphOpcode,
            TypeRootReference (nodeIdNatural localRoot),
            TypeDefinitionCount (lnNextNode numbering)
          ]
            <> definitions
        )
    )

numberReachableNode ::
  Map TypeNodeId TypeNode ->
  TypeNodeId ->
  LocalNumbering ->
  Either TypeGraphObstruction LocalNumbering
numberReachableNode compiledNodes globalNode numbering =
  case Map.lookup globalNode (lnNodeIds numbering) of
    Just _ ->
      Right numbering
    Nothing ->
      case Map.lookup globalNode compiledNodes of
        Nothing ->
          Left (MissingCompiledTypeNode (nodeIdNatural globalNode))
        Just nodeValue ->
          let localNode = TypeNodeId (lnNextNode numbering)
              numbered =
                numbering
                  { lnNextNode = lnNextNode numbering + 1,
                    lnNodeIds = Map.insert globalNode localNode (lnNodeIds numbering),
                    lnTraversalOrder = lnTraversalOrder numbering |> globalNode
                  }
           in foldM
                (flip (numberReachableNode compiledNodes))
                numbered
                (typeNodeChildren nodeValue)

typeNodeChildren :: TypeNode -> [TypeNodeId]
typeNodeChildren = \case
  TypeApplication functionNode arguments ->
    functionNode : fmap taNode arguments
  TypeFunction multiplicityNode argumentNode resultNode ->
    [multiplicityNode, argumentNode, resultNode]
  TypeQualified predicateNode bodyNode ->
    [predicateNode, bodyNode]
  TypeForAll _ kindNode _ bodyNode ->
    [kindNode, bodyNode]
  TypeVariable _ ->
    []
  TypeCast castNode ->
    [castNode]
  TypeCoercion ->
    []
  TypeConstructor _ arguments ->
    fmap taNode arguments
  TypeLiteral _ ->
    []

renderDefinition ::
  LocalNumbering ->
  Map TypeNodeId TypeNode ->
  TypeNodeId ->
  Either TypeGraphObstruction [TypeWord]
renderDefinition numbering compiledNodes globalNode = do
  localNode <- localNodeId numbering globalNode
  nodeValue <-
    maybe
      (Left (MissingCompiledTypeNode (nodeIdNatural globalNode)))
      Right
      (Map.lookup globalNode compiledNodes)
  nodeWords <- renderTypeNode numbering nodeValue
  pure (TypeDefinitionId (nodeIdNatural localNode) : nodeWords)

renderTypeNode :: LocalNumbering -> TypeNode -> Either TypeGraphObstruction [TypeWord]
renderTypeNode numbering = \case
  TypeApplication functionNode arguments ->
    ( \functionReference argumentWords ->
        TypeOpcode TypeAppOpcode : functionReference : argumentWords
    )
      <$> localNodeReference numbering functionNode
      <*> renderArguments numbering arguments
  TypeFunction multiplicityNode argumentNode resultNode -> do
    references <- traverse (localNodeReference numbering) [multiplicityNode, argumentNode, resultNode]
    pure (TypeOpcode TypeFunOpcode : references)
  TypeQualified predicateNode bodyNode -> do
    references <- traverse (localNodeReference numbering) [predicateNode, bodyNode]
    pure (TypeOpcode TypeQualOpcode : references)
  TypeForAll binderId kindNode specificity bodyNode -> do
    kindReference <- localNodeReference numbering kindNode
    bodyReference <- localNodeReference numbering bodyNode
    pure
      [ TypeOpcode TypeForAllOpcode,
        TypeBinderReference (binderIdNatural binderId),
        kindReference,
        TypeOutputText specificity,
        bodyReference
      ]
  TypeVariable variableReference ->
    pure (TypeOpcode TypeVariableOpcode : renderVariableReference variableReference)
  TypeCast castNode -> do
    castReference <- localNodeReference numbering castNode
    pure [TypeOpcode TypeCastOpcode, castReference]
  TypeCoercion ->
    pure [TypeOpcode TypeCoercionOpcode]
  TypeConstructor tyCon arguments ->
    ( \argumentWords ->
        TypeOpcode TypeTyConAppOpcode
          : TypeNameIdentity (getKey (nameUnique (ifaceTyConName tyCon)))
          : TypeOutputText (outputString (ifaceTyConInfo tyCon))
          : TypeOutputText (outputString tyCon)
          : argumentWords
    )
      <$> renderArguments numbering arguments
  TypeLiteral literalValue ->
    pure
      [ TypeOpcode TypeLiteralOpcode,
        TypeOutputText (typeLiteralConstructorName literalValue),
        TypeOutputText (outputString literalValue)
      ]

typeLiteralConstructorName :: IfaceTyLit -> String
typeLiteralConstructorName = \case
  IfaceNumTyLit {} -> "number"
  IfaceStrTyLit {} -> "string"
  IfaceCharTyLit {} -> "character"

renderArguments :: LocalNumbering -> [TypeArgument] -> Either TypeGraphObstruction [TypeWord]
renderArguments numbering arguments =
  (TypeArgumentCount (naturalFromInt (length arguments)) :)
    . foldMap id
    <$> traverse renderArgument arguments
  where
    renderArgument argumentValue =
      ( \reference ->
          [ TypeArgumentVisibilityWord (taVisibility argumentValue),
            reference
          ]
      )
        <$> localNodeReference numbering (taNode argumentValue)

renderVariableReference :: TypeVariableReference -> [TypeWord]
renderVariableReference = \case
  BoundTypeVariable binderId ->
    [ TypeVariableFlavorWord TypeBoundVariableFlavor,
      TypeBinderReference (binderIdNatural binderId)
    ]
  FreeTypeVariable nameValue ->
    TypeVariableFlavorWord TypeFreeVariableFlavor
      : TypeNameIdentity (getKey (nameUnique nameValue))
      : outputTypeWords nameValue

localNodeReference :: LocalNumbering -> TypeNodeId -> Either TypeGraphObstruction TypeWord
localNodeReference numbering =
  fmap (TypeNodeReference . nodeIdNatural) . localNodeId numbering

localNodeId :: LocalNumbering -> TypeNodeId -> Either TypeGraphObstruction TypeNodeId
localNodeId numbering globalNode =
  maybe
    (Left (MissingCompiledTypeNode (nodeIdNatural globalNode)))
    Right
    (Map.lookup globalNode (lnNodeIds numbering))

nodeIdNatural :: TypeNodeId -> Natural
nodeIdNatural (TypeNodeId nodeId) =
  nodeId

binderIdNatural :: TypeBinderId -> Natural
binderIdNatural (TypeBinderId binderId) =
  binderId

naturalFromInt :: Int -> Natural
naturalFromInt =
  fromIntegral

outputString :: Outputable value => value -> String
outputString =
  showSDocUnsafe . ppr
