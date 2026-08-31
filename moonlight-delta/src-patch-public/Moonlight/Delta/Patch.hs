{-|
Checked keyed transitions. A @CellPatch value@ records both endpoints of one
keyed change — absent→absent, absent→present, present→absent, or
present→present. Build cells with @assertAbsent@, @insert@, @delete@, @replace@,
or @cellFromEndpoints@; @apply@ checks the before endpoint and @compose@ checks
that adjacent endpoints connect. Use patches for concurrent updates, retries,
synchronization, authorization-sensitive changes, and reconciliation.

/Apply a patch./

> import Data.Map.Strict (Map)
> import Data.Map.Strict qualified as Map
> import Moonlight.Delta.Patch qualified as Patch
>
> changeTitle :: Patch.Patch Int String
> changeTitle = Patch.singleton 7 (Patch.replace "Draft" "Published")
>
> committed :: Either (Patch.ApplyError Int String) (Map Int String)
> committed = Patch.apply changeTitle (Map.singleton 7 "Draft")

/Compose and replay./ @compose newer older@ collapses two connected transitions;
@replay@ applies a chronological sequence and reports the failing index. @invert@
reverses a patch. Repeated temporal keys belong in @recordMany@; @fromList@ means
authoritative, last-wins rows.

> draft, reviewed, published :: Map Int String
> draft = Map.singleton 7 "Draft"
> reviewed = Map.singleton 7 "Review"
> published = Map.singleton 7 "Published"
>
> draftToReview, reviewToPublished :: Patch.Patch Int String
> draftToReview = Patch.diff draft reviewed
> reviewToPublished = Patch.diff reviewed published
>
> publication :: Either (Patch.ComposeError Int String) (Patch.Patch Int String)
> publication = Patch.compose reviewToPublished draftToReview
>
> replayed :: Either (Patch.ReplayError Int String) (Map Int String)
> replayed = Patch.replay [draftToReview, reviewToPublished] draft
-}
module Moonlight.Delta.Patch
  ( CellPatch,
    PatchKey,
    PatchValue,
    Patch,
    ApplyError (..),
    ComposeError (..),
    ReplayError (..),
    MerkleDeltaHash,
    MultisetDeltaHash,
    DeltaHashStrategy (..),
    DeltaHashEncoderVersion (..),
    DeltaHashDigest,
    DeltaHashDigestIdentity,
    DeltaHashDigestLanes,
    MerkleDeltaHashDigest,
    MultisetDeltaHashDigest,
    deltaHashDigestIdentity,
    DeltaHashDigestDecodeError (..),
    encodeDeltaHashDigest,
    decodeMerkleDeltaHashDigest,
    decodeMultisetDeltaHashDigest,
    DeltaHashBuildError (..),
    DeltaHashApplyError (..),
    DeltaHashComposeError (..),
    assertAbsent,
    insert,
    delete,
    replace,
    matchCell,
    cellBefore,
    cellAfter,
    -- | Build a cell patch from observable before and after endpoints.
    --
    -- @Nothing@ means the key is absent on that side; @Just value@ means the
    -- key is present with that value.
    cellFromEndpoints,
    mapCell,
    traverseCell,
    empty,
    singleton,
    -- | Build a patch from an authoritative final map of logical rows.
    --
    -- Duplicate keys use the same last-wins semantics as 'Data.Map.Strict.fromList'.
    -- Use 'recordMany' for a temporal edit log whose repeated keys must stitch
    -- through matching before/after boundaries.
    fromList,
    -- | Build from an ascending logical-row list when possible, falling back
    -- to 'fromList' for non-ascending input. Repeated adjacent keys are treated
    -- as authoritative replacement rows, not as a temporal edit sequence.
    fromAscList,
    toAscList,
    lookup,
    mapMaybeWithKey,
    foldWithKey,
    foldWithKey',
    traverseWithKey,
    normalize,
    null,
    size,
    support,
    compose,
    recordApplied,
    -- | Record a batch of applied edits as a temporal edit log.
    --
    -- Repeated keys must form a valid chain: each later before endpoint must
    -- equal the previous after endpoint. Boundary mismatches are reported as
    -- 'ComposeBoundaryMismatch'.
    recordMany,
    invert,
    diff,
    apply,
    replay,
    buildMerkleDeltaHash,
    merkleDeltaHashState,
    merkleDeltaHashDigest,
    applyMerkleDeltaHash,
    composeMerkleDeltaHash,
    buildMultisetDeltaHash,
    multisetDeltaHashState,
    multisetDeltaHashDigest,
    applyMultisetDeltaHash,
    composeMultisetDeltaHash,
  )
where

import Moonlight.Delta.Patch.Internal.Apply
  ( apply,
  )
import Moonlight.Delta.Patch.Internal.Cell
  ( assertAbsent,
    cellAfter,
    cellBefore,
    cellFromEndpoints,
    delete,
    insert,
    mapCell,
    matchCell,
    replace,
    traverseCell,
  )
import Moonlight.Delta.Patch.Internal.Compose.Core
  ( compose,
  )
import Moonlight.Delta.Patch.Internal.Compose.Record
  ( recordApplied,
    recordMany,
  )
import Moonlight.Delta.Patch.Internal.Construction
  ( diff,
    empty,
    foldWithKey,
    foldWithKey',
    fromAscList,
    fromList,
    invert,
    lookup,
    mapMaybeWithKey,
    singleton,
    toAscList,
    traverseWithKey,
  )
import Moonlight.Delta.Patch.Internal.Replay
  ( replay,
  )
import Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( DeltaHashApplyError (..),
    DeltaHashBuildError (..),
    DeltaHashComposeError (..),
    DeltaHashDigest,
    DeltaHashDigestDecodeError (..),
    DeltaHashDigestIdentity,
    DeltaHashDigestLanes,
    DeltaHashEncoderVersion (..),
    DeltaHashStrategy (..),
    MerkleDeltaHashDigest,
    MultisetDeltaHashDigest,
    decodeMerkleDeltaHashDigest,
    decodeMultisetDeltaHashDigest,
    deltaHashDigestIdentity,
    encodeDeltaHashDigest,
  )
import Moonlight.Delta.Patch.Internal.MerkleDeltaHash
  ( MerkleDeltaHash,
    applyMerkleDeltaHash,
    buildMerkleDeltaHash,
    composeMerkleDeltaHash,
    merkleDeltaHashDigest,
    merkleDeltaHashState,
  )
import Moonlight.Delta.Patch.Internal.MultisetDeltaHash
  ( MultisetDeltaHash,
    applyMultisetDeltaHash,
    buildMultisetDeltaHash,
    composeMultisetDeltaHash,
    multisetDeltaHashDigest,
    multisetDeltaHashState,
  )
import Moonlight.Delta.Patch.Internal.Types
  ( ApplyError (..),
    CellPatch,
    ComposeError (..),
    PatchKey,
    PatchValue,
    Patch,
    ReplayError (..),
    normalize,
    null,
    size,
    support,
  )
