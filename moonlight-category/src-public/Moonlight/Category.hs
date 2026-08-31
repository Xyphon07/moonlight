{-| The primary entry point to @moonlight-category@, re-exporting the categorical
layer as a single convenience surface.

The base abstraction is "Moonlight.Category.Pure.Category": a totalised,
explicit-error @Category@ class whose objects, morphisms, 2-morphisms, compositors
and errors are associated types and whose operations return @Either@. On top of it
this module gathers the limit and colimit class tower, the higher-category tower
(2-categories, bicategories, monoidal and enriched categories), runtime-validated
finite categories (@FinCat@) with their thin and composable-chain variants,
core/automorphism groupoid extraction, the adhesive and PBPO rewriting witnesses,
structured cospans, double categories, decorated composition and presentation,
Galois connections, polynomial functors, covering families, and the site/path
presentation layer.

@UnitCat@ is the terminal one-object category, useful as a base case and in tests.

For direct finite-category authoring, import
"Moonlight.Category.Presentation". For scoped mathematical operations on an
already-compiled finite category, import "Moonlight.Category.Notation".

The indexed, typed-arrow layer is exposed separately as "Moonlight.Category.Indexed".

== Ordinary consumer path

This facade owns ordinary site compilation and finite-category provenance.
'thinSiteImportKernel' validates and compiles just the import category, while
'thinSiteKernel' additionally establishes the cover axioms required by the full
site/path layer. Keep the resulting 'ThinSiteKernel' with its codomain: a
site-derived 'FinObjectId' is a kernel-relative representation token, /not/ a
semantic name or a stable numeric ordering. Recover source objects through
'thinSiteFinObject' and 'thinSiteObjectValue', never by rebuilding an identifier
map from the manifest's object set.

'ComposableChain' is opaque and validated at construction. Its 'chainVertices'
projection is therefore total: the established endpoint evidence is retained rather
than queried again. For a nerve, add the separate
"Moonlight.Category.Simplicial" component and import its facade. Its
@normalizedNerve@ is the ordinary nonidentity-chain construction;
@unnormalizedNerve@ intentionally materializes identity insertions as well and can
grow combinatorially even for a modest truncation bound.
-}
module Moonlight.Category
  ( UnitCat (..),
    UnitMor (..),
    UnitObj (..),
    module X,
  )
where

import Moonlight.Category.Pure.Category as X
import Moonlight.Category.Pure.Adhesive as X
import Moonlight.Category.Pure.DoubleCategory as X
import Moonlight.Category.Pure.CoveringFamily as X
import Moonlight.Category.Pure.CoveringProduct as X
import Moonlight.Category.Pure.Thin as X
import Moonlight.Category.Pure.DecoratedComposition as X
import Moonlight.Category.Pure.DecoratedPresentation as X
import Moonlight.Category.Pure.FiniteComposable as X
import Moonlight.Category.Pure.FinCat as X hiding (denseThinEndpointMorphismsFromCategory, trustedDenseThinFinCatFromReachabilityRows, trustedFinCatWithGeneratorBasis, trustedThinFinCatFromTransitiveEndpoints)
import Moonlight.Category.Pure.FinCat.Functor as X
import Moonlight.Category.Pure.Galois as X
import Moonlight.Category.Pure.Higher as X
import Moonlight.Category.Pure.Invertibility as X
import Moonlight.Category.Pure.PolynomialFunctor as X
import Moonlight.Category.Pure.Limits as X
import Moonlight.Category.Pure.StructuredCospan as X
import Moonlight.Category.Pure.Site as X hiding (ThinSitePresentation (..), thinPresentationToFinCat, thinSitePresentation)
import Moonlight.Category.Pure.Unit
  ( UnitCat (..),
    UnitMor (..),
    UnitObj (..),
  )
