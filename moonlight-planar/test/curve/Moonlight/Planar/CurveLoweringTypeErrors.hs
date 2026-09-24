{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Only this deliberately ill-typed public client defers errors. A lawful
-- observer must not also supply the record-field capability that enables updates.
module Moonlight.Planar.CurveLoweringTypeErrors (recordSpanBound) where

import GHC.Records (getField)
import Moonlight.Planar.Curve.Lowering (LoweredSpan, spanSquaredBound)
import Moonlight.Planar.Exact (ExactRational)

recordSpanBound :: LoweredSpan -> (ExactRational, ExactRational)
recordSpanBound sample = (spanSquaredBound sample, getField @"spanSquaredBound" sample)
