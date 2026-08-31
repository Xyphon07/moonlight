-- | The opt-in trust boundary: unsafe constructors that mint refined values,
-- identifier tokens and canonical numbers from a 'TrustJustification' rather than
-- a runtime check. Import deliberately.
module Moonlight.Core.Unsound
  ( TrustJustification (..),
    unsafelyTrustRefined,
    unsafelyTrustIdentifierToken,
    unsafeTrustDomainId,
    unsafeTrustStressorId,
    unsafeCanonicalFiniteLiteral,
    unsafeCanonicalFiniteAssumeCanonical,
  )
where

import Moonlight.Core.CanonicalNumber.Internal (unsafeCanonicalFiniteLiteral, unsafeCanonicalFiniteAssumeCanonical)
import Moonlight.Core.DomainId.Internal (unsafeTrustDomainId)
import Moonlight.Core.Niche.Internal (unsafeTrustStressorId)
import Moonlight.Internal.Unsound (TrustJustification (..), unsafelyTrustIdentifierToken, unsafelyTrustRefined)
