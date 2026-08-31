{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Delta.Patch.Internal.Compose.Aligned
  ( tryAlignedTree,
    tryAlignedPage,
  )
where

import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch.Internal.Cell
  ( endpointToMaybe,
  )
import Moonlight.Delta.Patch.Internal.Page
import Moonlight.Delta.Patch.Internal.Types
import Prelude

tryAlignedTree ::
  forall key value error.
  (Ord key, Eq value) =>
  (key -> Maybe value -> Maybe value -> error) ->
  Map.Map key (Page key value) ->
  Map.Map key (Page key value) ->
  Either error (Maybe (Map.Map key (Page key value)))
tryAlignedTree makeBoundaryError =
  go
  where
    go MapInternal.Tip MapInternal.Tip =
      Right (Just MapInternal.Tip)
    go
      (MapInternal.Bin olderSize olderMaximum olderPage olderLeft olderRight)
      (MapInternal.Bin newerSize newerMaximum newerPage newerLeft newerRight)
        | olderSize /= newerSize =
            Right Nothing
        | compare olderMaximum newerMaximum /= EQ =
            Right Nothing
        | otherwise = do
            maybeLeft <- go olderLeft newerLeft
            case maybeLeft of
              Nothing ->
                Right Nothing
              Just resultLeft -> do
                maybePage <- tryAlignedPage makeBoundaryError olderMaximum olderPage newerMaximum newerPage
                case maybePage of
                  Nothing ->
                    Right Nothing
                  Just (_, resultPage) -> do
                    maybeRight <- go olderRight newerRight
                    pure
                      ( fmap
                          (\resultRight -> MapInternal.Bin newerSize newerMaximum resultPage resultLeft resultRight)
                          maybeRight
                      )
    go _ _ =
      Right Nothing
{-# INLINABLE tryAlignedTree #-}

tryAlignedPage ::
  forall key value error.
  (Ord key, Eq value) =>
  (key -> Maybe value -> Maybe value -> error) ->
  key ->
  Page key value ->
  key ->
  Page key value ->
  Either error (Maybe (key, Page key value))
tryAlignedPage makeBoundaryError olderMaximum olderPage newerMaximum newerPage =
  case
      validateAlignedPageBoundary
        pageBoundaryError
        olderMaximum
        olderPage
        (pageAfterColumn olderPage)
        newerMaximum
        newerPage
        (pageBeforeColumn newerPage)
    of
      PageBoundaryMatched ->
        Right
          ( Just
              ( newerMaximum,
                newerPage {pageBeforeColumn = pageBeforeColumn olderPage}
              )
          )
      PageBoundaryDiverged ->
        Right Nothing
      PageBoundaryRejected failure ->
        Left failure
  where
    pageBoundaryError key olderAfter newerBefore =
      makeBoundaryError key (endpointToMaybe olderAfter) (endpointToMaybe newerBefore)
{-# INLINABLE tryAlignedPage #-}
