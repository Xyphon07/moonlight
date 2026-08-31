module EpochSupport.Expected
  ( viewProjection,
  )
where

import Moonlight.Delta.Epoch (ContextView (..), Version)

viewProjection :: ContextView observed section -> (Version, observed, section)
viewProjection contextView =
  (cvVersion contextView, cvObservedKeys contextView, cvSection contextView)
