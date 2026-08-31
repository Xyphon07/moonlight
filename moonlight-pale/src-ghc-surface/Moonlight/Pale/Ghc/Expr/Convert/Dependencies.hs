{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.Dependencies
  ( BindingDependencyFailure (..),
    inferBindingComponents,
    singletonBindingComponent,
    bindingComponentsRecursion,
  )
where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (BinderId)
import Moonlight.Pale.Ghc.Expr.Syntax

type BindingDependencyFailure :: Type
data BindingDependencyFailure
  = EmptyRecursiveBindingComponent
  | EmptyBindingComponentPartition
  deriving stock (Eq, Ord, Show)

inferBindingComponents ::
  NonEmpty.NonEmpty HsPatF ->
  IntMap (Set BinderId) ->
  Either BindingDependencyFailure (NonEmpty.NonEmpty BindingComponent)
inferBindingComponents (bindingPattern NonEmpty.:| []) dependenciesByRow =
  let rowDependencies =
        IntMap.findWithDefault Set.empty 0 dependenciesByRow
      referencesOwnBinder =
        case bindingPattern of
          PVarP binderAnn ->
            Set.member (baId binderAnn) rowDependencies
          _ ->
            not
              ( Set.disjoint
                  (Set.fromList (fmap baId (patBinders bindingPattern)))
                  rowDependencies
              )
   in Right (singletonBindingComponent bindingPattern referencesOwnBinder)
inferBindingComponents bindingPatterns dependenciesByRow =
  let indexedPatterns =
        zip [0 :: Int ..] (NonEmpty.toList bindingPatterns)
      binderOwnerRows =
        Map.fromList
          [ (baId binderAnn, rowIndex)
          | (rowIndex, bindingPattern) <- indexedPatterns,
            binderAnn <- patBinders bindingPattern
          ]
      groupBinderIds =
        Map.keysSet binderOwnerRows
   in if
        Foldable.all
          (Set.disjoint groupBinderIds)
          dependenciesByRow
        then
          maybe
            (Left EmptyBindingComponentPartition)
            Right
            ( NonEmpty.nonEmpty
                ( fmap
                    independentComponent
                    (reverse indexedPatterns)
                )
            )
        else
          let dependencyNodes =
                fmap
                  (bindingDependencyNode groupBinderIds binderOwnerRows)
                  indexedPatterns
              dependencyComponents =
                stronglyConnComp dependencyNodes
           in do
                components <- traverse componentFromScc dependencyComponents
                maybe
                  (Left EmptyBindingComponentPartition)
                  Right
                  (NonEmpty.nonEmpty components)
  where
    independentComponent :: (Int, HsPatF) -> BindingComponent
    independentComponent (rowIndex, bindingPattern) =
      BindingComponent
        { bindingComponentRows = rowIndex NonEmpty.:| [],
          bindingComponentBinders =
            Set.toList (Set.fromList (fmap baId (patBinders bindingPattern))),
          bindingComponentDependencies = [],
          bindingComponentRecursion = AcyclicBindingComponent
        }

    bindingDependencyNode ::
      Set BinderId ->
      Map.Map BinderId Int ->
      (Int, HsPatF) ->
      ((Int, [BinderId], [BinderId]), Int, [Int])
    bindingDependencyNode groupBinderIds binderOwnerRows (rowIndex, bindingPattern) =
      let rhsDependencies =
            Set.toList
              ( Set.intersection
                  groupBinderIds
                  (IntMap.findWithDefault Set.empty rowIndex dependenciesByRow)
              )
          dependencyRows =
            Set.toList
              ( Set.fromList
                  ( foldMap
                      (\binderId -> maybe [] (: []) (Map.lookup binderId binderOwnerRows))
                      rhsDependencies
                  )
              )
       in ( ( rowIndex,
              fmap baId (patBinders bindingPattern),
              rhsDependencies
            ),
            rowIndex,
            dependencyRows
          )

    componentFromScc = \case
      AcyclicSCC rowPayload ->
        Right
          (mkComponent (rowPayload NonEmpty.:| []) AcyclicBindingComponent)
      CyclicSCC rowPayloads ->
        case NonEmpty.nonEmpty rowPayloads of
          Nothing ->
            Left EmptyRecursiveBindingComponent
          Just nonEmptyRowPayloads ->
            Right
              (mkComponent nonEmptyRowPayloads RecursiveBindingComponent)

    mkComponent rowPayloads recursionValue =
      let componentRows =
            fmap (\(rowIndex, _, _) -> rowIndex) rowPayloads
          componentBinders =
            Set.toList
              ( foldMap
                  (Set.fromList . (\(_, binderIds, _) -> binderIds))
                  rowPayloads
              )
          binderSet =
            Set.fromList componentBinders
          externalDependencies =
            foldMap
              (Set.fromList . (\(_, _, dependencyIds) -> dependencyIds))
              rowPayloads
              `Set.difference` binderSet
       in BindingComponent
            { bindingComponentRows = componentRows,
              bindingComponentBinders = componentBinders,
              bindingComponentDependencies = Set.toList externalDependencies,
              bindingComponentRecursion = recursionValue
            }

singletonBindingComponent :: HsPatF -> Bool -> NonEmpty.NonEmpty BindingComponent
singletonBindingComponent bindingPattern referencesOwnBinder =
  BindingComponent
    { bindingComponentRows = 0 NonEmpty.:| [],
      bindingComponentBinders =
        case bindingPattern of
          PVarP binderAnn ->
            [baId binderAnn]
          _ ->
            Set.toList (Set.fromList (fmap baId (patBinders bindingPattern))),
      bindingComponentDependencies = [],
      bindingComponentRecursion =
        if referencesOwnBinder
          then RecursiveBindingComponent
          else AcyclicBindingComponent
    }
    NonEmpty.:| []

bindingComponentsRecursion :: NonEmpty.NonEmpty BindingComponent -> LetRecursion
bindingComponentsRecursion bindingComponents
  | any ((== RecursiveBindingComponent) . bindingComponentRecursion) bindingComponents =
      RecursiveBinds
  | any (not . null . bindingComponentDependencies) bindingComponents =
      AcyclicDependentBinds
  | otherwise =
      NonRecursiveBinds
