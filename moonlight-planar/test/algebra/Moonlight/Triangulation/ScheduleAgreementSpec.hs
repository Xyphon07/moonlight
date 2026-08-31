-- | The schedule-agreement slice: the seam merge publishes what the reference
-- rebuild publishes. It travels and dies with the schedule it names.
module Moonlight.Triangulation.ScheduleAgreementSpec (tests) where

import Control.Monad (forM_, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Moonlight.Triangulation (Point (Point), canonicalize, union)
import Moonlight.Triangulation.AlgebraFixtures
  ( assertMesh
  , meshOf
  , operands
  , separatedOperands
  , siteList
  )
import Moonlight.Triangulation.Internal.Join.Seam (executeSeam, planSeam)
import Support (requireRight)

tests :: IO ()
tests = do
  testSeamAgreesWithReference
  putStrLn "schedule agreement: ok"

-- | The seam schedule and the reference schedule are the same function.
--
-- A join has one meaning and several internal schedules, so this is the law
-- that lets a second one exist at all: for every input where the seam applies,
-- it must publish the value the rebuild publishes — not an isomorphic mesh, the
-- same value, once both are canonically numbered.
--
-- The guard matters more than the equality. 'executeSeam' can only be reached
-- with the opaque proof returned by 'planSeam'; the run fails if too few cases
-- obtain that proof, so the reference cannot quietly be compared with itself.
testSeamAgreesWithReference :: IO ()
testSeamAgreesWithReference = do
  separable <- separatedOperands
  overlapping <- operands
  taken <- newIORef (0 :: Int)
  forM_ separable $ \((leftName, left), (rightName, right)) -> do
    let label = "seam " <> leftName <> " / " <> rightName
    seamPlan <- case planSeam left right of
      Nothing ->
        fail (label <> ": the operands are separated but planSeam refused them")
      Just admitted -> do
        modifyIORef' taken (+ 1)
        pure admitted
    rawRebuild <- meshOf label [Point x y | (x, y) <- siteList left <> siteList right]
    rebuilt <- requireRight (label <> ", canonical rebuild") (canonicalize rawRebuild)
    seamed <- requireRight (label <> ", direct seam") (executeSeam seamPlan left right)
    canonicalSeam <- requireRight (label <> ", canonical seam") (canonicalize seamed)
    assertMesh (label <> ", against a rebuild") rebuilt canonicalSeam
    joined <- requireRight (label <> ", through union") (union left right)
    canonicalJoined <- requireRight (label <> ", canonical union") (canonicalize joined)
    assertMesh (label <> ", through the operator") rebuilt canonicalJoined
  forM_ [(l, r) | l <- overlapping, r <- overlapping] $ \((leftName, left), (rightName, right)) ->
    case planSeam left right of
      Nothing -> pure ()
      Just seamPlan -> do
        rawRebuild <-
          meshOf
            ("seam admission " <> leftName <> " / " <> rightName)
            [Point x y | (x, y) <- siteList left <> siteList right]
        rebuilt <- requireRight "seam admission canonical rebuild" (canonicalize rawRebuild)
        seamed <- requireRight "seam admission" (executeSeam seamPlan left right)
        canonicalSeam <- requireRight "seam admission canonical seam" (canonicalize seamed)
        assertMesh
          ("seam admission " <> leftName <> " / " <> rightName)
          rebuilt
          canonicalSeam
        modifyIORef' taken (+ 1)
  count <- readIORef taken
  when (count < 3) $
    fail ("seam agreement: only " <> show count <> " cases entered the seam, so the law asserted nothing")
