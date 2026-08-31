{-| The focused authoring surface for finite categories.

The semantic result is always 'FinCat'; this module contributes syntax and
compilation, not a second category representation.

Two dialects are supported:

* finite posets, declared by objects and strict generating inequalities with
  'below';
* fully enumerated finite categories, declared by every nonidentity morphism and
  enough equations to determine every nonidentity composable pair.

Identities are implicit in 'FinCat' and may be referenced in equations with
'identityAt'. Longer paths are accepted when their proper intermediate composites
are determined by the same presentation. Arbitrary quotients of free categories by
path congruences are intentionally outside this surface.

For querying a compiled category with mathematical names, import
"Moonlight.Category.Notation" separately.
-}
module Moonlight.Category.Presentation
  ( FinCat,
    FinBuilder,
    ObjRef,
    ArrowExpr,
    FinCatBuildError (..),
    object,
    objects,
    arrow,
    identityAt,
    below,
    after,
    equate,
    finCategory,
  )
where

import Moonlight.Category.Pure.FinCat (FinCat)
import Moonlight.Category.Pure.FinPresentation
  ( ArrowExpr,
    FinBuilder,
    FinCatBuildError (..),
    ObjRef,
    after,
    arrow,
    below,
    equate,
    finCategory,
    identityAt,
    object,
    objects,
  )
