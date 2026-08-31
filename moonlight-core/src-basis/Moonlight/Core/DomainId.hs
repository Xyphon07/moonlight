-- | Checked textual identifier for value domains: 'mkDomainId' is the only public door, rendering is total.
module Moonlight.Core.DomainId
  ( DomainId,
    mkDomainId,
    renderDomainId,
  )
where

import Moonlight.Core.DomainId.Internal
