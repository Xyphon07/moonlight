module Moonlight.Pale.Ghc.Expr.Opaque
  ( HsOpaqueTag (..),
    HsPatOpaqueTag (..),
    hsOpaqueTagName,
    hsPatOpaqueTagName,
  )
where

import Data.Kind (Type)

type HsOpaqueTag :: Type
data HsOpaqueTag
  = OpaqueOverLabel
  | OpaqueIPVar
  | OpaqueExplicitSum
  | OpaqueOverloadedRecordUpdate
  | OpaqueGetField
  | OpaqueProjection
  | OpaqueTypedBracket
  | OpaqueUntypedBracket
  | OpaqueTypedSplice
  | OpaqueUntypedSplice
  | OpaqueProc
  | OpaqueStatic
  | OpaquePragE
  | OpaqueEmbTy
  | OpaqueHole
  | OpaqueForAll
  | OpaqueQual
  | OpaqueFunArr
  | OpaqueCaseAlternative
  | OpaqueEmptyLocalBinds
  | OpaqueImplicitParameterBinds
  | OpaquePatternSynonymBind
  | OpaqueExtensionValBinds
  | OpaqueParallelStatement
  | OpaqueTransformStatement
  | OpaqueRecursiveStatement
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type HsPatOpaqueTag :: Type
data HsPatOpaqueTag
  = PatOpaqueOr
  | PatOpaqueSum
  | PatOpaqueView
  | PatOpaqueSplice
  | PatOpaqueNPlusK
  | PatOpaqueSig
  | PatOpaqueEmbTy
  | PatOpaqueInvis
  | PatOpaqueNegativeLit
  | PatOpaqueExtension
  deriving stock (Eq, Ord, Show, Enum, Bounded)

hsOpaqueTagName :: HsOpaqueTag -> String
hsOpaqueTagName =
  show

hsPatOpaqueTagName :: HsPatOpaqueTag -> String
hsPatOpaqueTagName =
  show
