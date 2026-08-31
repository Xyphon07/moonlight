{-# LANGUAGE GHC2024 #-}

{-|
The finite, validated, queryable realization of a lattice over a declared closed
universe — the public @finite-lattice@ face of @moonlight-algebra@. Where the
main library is the abstract algebra class tower, this sublibrary is checked
finite-order compilation and the dense runtime views: @ContextLattice@, dense
join\/meet\/@<=@ plans, resident branded keys, Heyting implication, least and
greatest fixpoints, cover edges, presentation builders, and generator support.

/Quick start./ Declare a finite order by name binding; @latticeOf@ infers the
unique top and bottom, transitively closes the order, derives join and meet, and
proves lattice-hood, returning a compiled @ContextLattice@ or the first
obstruction it finds.

> import Moonlight.FiniteLattice
>
> data Context = Bottom | West | East | Top
>   deriving (Eq, Ord, Show)
>
> diamond :: Either (LatticeBuildError Context) (ContextLattice Context)
> diamond =
>   latticeOf $ do
>     [bottom, west, east, top] <- elements [Bottom, West, East, Top]
>     below bottom west
>     below bottom east
>     below west top
>     below east top

@West@ and @East@ are incomparable; their least upper bound and greatest lower
bound are derived from the order alone. On the compiled lattice, order, join and
meet are total, checked lookups into the dense plan:

>>> Right lattice = diamond
>>> joinContext lattice West East
Right Top
>>> meetContext lattice West East
Right Bottom
>>> leqContext lattice Bottom Top
Right True

/Heyting implication./ The diamond is distributive, so it carries a Heyting
structure. @compileContextHeyting@ builds the residual plan once; @impliesContext@
is then the relative pseudocomplement: the largest @x@ with
@antecedent ∧ x ≤ consequent@.

>>> Right heyting = compileContextHeyting lattice
>>> impliesContext heyting West East
Right East
>>> impliesContext heyting West West
Right Top

/Least and greatest fixpoints./ Every monotone endomap has a least and a greatest
fixpoint (Knaster–Tarski). @leastContextFixpoint@ climbs from the bottom,
@greatestContextFixpoint@ descends from the top; a non-monotone step is rejected
as a typed @ContextMonotoneMapError@.

> settle :: Context -> Context
> settle Bottom = West
> settle West   = West
> settle East   = Top
> settle Top    = Top

>>> leastContextFixpoint lattice settle
Right West
>>> greatestContextFixpoint lattice settle
Right Top

/The resident fast path./ @joinContext@, @meetContext@ and @impliesContext@
resolve each domain value through a map on every call. To sweep many queries,
resolve once: @withResidentContext@ hands you branded keys whose order, join and
meet are pure array indexing with no per-query lookup inside the loop.

> leqRelationSize :: ContextLattice Context -> Int
> leqRelationSize lattice =
>   withResidentContext lattice $ \ctx ->
>     let keys = residentContextKeys ctx
>      in length [() | a <- keys, b <- keys, residentContextKeyLeq ctx a b]

>>> leqRelationSize lattice
9

The count is the size of the @≤@ relation: four reflexive pairs, plus @Bottom@
under @West@, @East@ and @Top@, plus @West@ and @East@ each under @Top@.
-}
module Moonlight.FiniteLattice
  ( module Moonlight.FiniteLattice.Core,
    module Moonlight.FiniteLattice.Resident,
    module Moonlight.FiniteLattice.Cover,
    module Moonlight.FiniteLattice.Fixpoint,
    module Moonlight.FiniteLattice.Heyting,
    module Moonlight.FiniteLattice.Presentation,
    module Moonlight.FiniteLattice.Support,
  )
where

import Moonlight.FiniteLattice.Core
import Moonlight.FiniteLattice.Cover
import Moonlight.FiniteLattice.Fixpoint
import Moonlight.FiniteLattice.Heyting
import Moonlight.FiniteLattice.Presentation
import Moonlight.FiniteLattice.Resident
import Moonlight.FiniteLattice.Support
