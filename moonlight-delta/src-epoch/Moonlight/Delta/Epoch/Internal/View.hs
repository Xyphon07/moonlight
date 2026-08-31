{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Version-stamped snapshots.  A 'ContextView' carries the claim under which
-- it was taken; 'contextViewIsCurrent' compares claims, nothing more.  The
-- with-helpers are the curated vocabulary for restamping — consumers should
-- not reach for record update syntax.
module Moonlight.Delta.Epoch.Internal.View
  ( ContextView (..),
    viewAt,
    viewWithVersion,
    viewWithSupport,
    viewWithSection,
    mapContextViewKeys,
    contextViewIsCurrent,
    contextViewIsStale,
  )
where

import Data.Kind (Type)
import Moonlight.Core (OrdSet (..))
import Moonlight.Delta.Epoch.Internal.Version (Version)
import Prelude (Bool, Eq ((==)), Ord, Show, fmap, not, (.))

type ContextView :: Type -> Type -> Type
data ContextView observed section = ContextView
  { cvVersion :: !Version,
    cvObservedKeys :: !observed,
    cvSection :: !section
  }
  deriving stock (Eq, Ord, Show)

viewAt :: Version -> observed -> section -> ContextView observed section
viewAt =
  ContextView

viewWithVersion :: Version -> ContextView observed section -> ContextView observed section
viewWithVersion epochVersion contextView =
  contextView
    { cvVersion = epochVersion
    }

viewWithSupport :: observed -> ContextView observed section -> ContextView observed section
viewWithSupport observedKeys contextView =
  contextView
    { cvObservedKeys = observedKeys
    }

viewWithSection :: section -> ContextView observed section -> ContextView observed section
viewWithSection sectionValue contextView =
  contextView
    { cvSection = sectionValue
    }

mapContextViewKeys ::
  (OrdSet observed1, OrdSet observed2) =>
  (SetKey observed1 -> SetKey observed2) ->
  ContextView observed1 section ->
  ContextView observed2 section
mapContextViewKeys rekey contextView =
  ContextView
    { cvVersion = cvVersion contextView,
      cvObservedKeys = fromListSet (fmap rekey (toAscListSet (cvObservedKeys contextView))),
      cvSection = cvSection contextView
    }

contextViewIsCurrent :: Version -> ContextView observed section -> Bool
contextViewIsCurrent epochVersion contextView =
  cvVersion contextView == epochVersion

contextViewIsStale :: Version -> ContextView observed section -> Bool
contextViewIsStale epochVersion =
  not . contextViewIsCurrent epochVersion
