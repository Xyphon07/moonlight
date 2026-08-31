-- | Newtype identifier atoms of the e-graph vocabulary (classes, e-nodes, rules, proof steps, pattern variables): identity only, no arithmetic.
module Moonlight.Core.Identifier.EGraph
  ( ClassId (..),
    classIdKey,
    ENodeId (..),
    RegionNodeId (..),
    RewriteRuleId (..),
    ProofStepId (..),
    PatternVar,
    mkPatternVar,
    patternVarKey,
    BinderId (..),
    binderIdKey,
    rewriteRuleIdKey,
  )
where

import Data.Kind (Type)
import Moonlight.Core.DenseKey (DenseKey (..))
import Prelude (Enum, Eq, Int, Ord, Read, Show)

type ClassId :: Type
newtype ClassId = ClassId Int
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type ENodeId :: Type
newtype ENodeId = ENodeId Int
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type RegionNodeId :: Type
newtype RegionNodeId = RegionNodeId Int
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type RewriteRuleId :: Type
newtype RewriteRuleId = RewriteRuleId Int
  deriving stock (Eq, Ord, Show, Read)

type ProofStepId :: Type
newtype ProofStepId = ProofStepId Int
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

type PatternVar :: Type
newtype PatternVar = PatternVar Int
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

mkPatternVar :: Int -> PatternVar
mkPatternVar =
  PatternVar

type BinderId :: Type
newtype BinderId = BinderId Int
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Enum)

classIdKey :: ClassId -> Int
classIdKey (ClassId key) = key

patternVarKey :: PatternVar -> Int
patternVarKey (PatternVar key) = key

binderIdKey :: BinderId -> Int
binderIdKey (BinderId key) = key

rewriteRuleIdKey :: RewriteRuleId -> Int
rewriteRuleIdKey (RewriteRuleId key) = key

instance DenseKey ClassId where
  encodeDenseKey = classIdKey
  decodeDenseKey = ClassId
