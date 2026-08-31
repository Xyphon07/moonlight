-- | The adaptive push/pull reachability policy: validated ratio thresholds and
-- the small-frontier cutoff that decide when to switch traversal direction.
module Moonlight.Core.Fixpoint.Dense.Internal.Policy
  ( ReachabilityPolicy (..),
    ReachabilityPolicyValidationError (..),
    mkReachabilityPolicy,
    defaultReachabilityPolicy,
  )
where

import Prelude

data ReachabilityPolicy = ReachabilityPolicy
  { pushToPullRatio :: !Double,
    pullToPushRatio :: !Double,
    smallFrontierLimit :: !Int
  }
  deriving stock (Eq, Show)

data ReachabilityPolicyValidationError
  = PushToPullRatioIsNaN
  | PullToPushRatioIsNaN
  | NegativePushToPullRatio !Double
  | NegativePullToPushRatio !Double
  | NegativeSmallFrontierLimit !Int
  deriving stock (Eq, Show)

mkReachabilityPolicy :: Double -> Double -> Int -> Either ReachabilityPolicyValidationError ReachabilityPolicy
mkReachabilityPolicy pushRatio pullRatio frontierLimit
  | isNaN pushRatio =
      Left PushToPullRatioIsNaN
  | isNaN pullRatio =
      Left PullToPushRatioIsNaN
  | pushRatio < 0 =
      Left (NegativePushToPullRatio pushRatio)
  | pullRatio < 0 =
      Left (NegativePullToPushRatio pullRatio)
  | frontierLimit < 0 =
      Left (NegativeSmallFrontierLimit frontierLimit)
  | otherwise =
      Right
        ReachabilityPolicy
          { pushToPullRatio = pushRatio,
            pullToPushRatio = pullRatio,
            smallFrontierLimit = frontierLimit
          }

defaultReachabilityPolicy :: ReachabilityPolicy
defaultReachabilityPolicy =
  ReachabilityPolicy
    { pushToPullRatio = 0.05,
      pullToPushRatio = 0.20,
      smallFrontierLimit = 64
    }
