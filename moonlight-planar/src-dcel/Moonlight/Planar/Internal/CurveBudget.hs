-- | The finite budget of an exact subdivision, admitted once, and the
-- obligation a subdivision reports when it spends it. Arc measurement,
-- proximity and topology certification share the one budget and its
-- refusals; what each does beyond subdivision stays with its own owner.
module Moonlight.Planar.Internal.CurveBudget
  ( SubdivisionBudget
  , SubdivisionBudgetError (..)
  , subdivisionBudget
  , budgetDepth
  , budgetLeaves
  , budgetBits
  , BudgetObligation (..)
  ) where

import Control.DeepSeq (NFData (..))

-- | Subdivision depth below any source step, the number of leaves in all,
-- and the largest admitted bit width of any retained exact value.
data SubdivisionBudget = SubdivisionBudget !Int !Int !Int
  deriving stock (Eq, Show)

instance NFData SubdivisionBudget where
  rnf (SubdivisionBudget depth leaves bits) = rnf depth `seq` rnf leaves `seq` rnf bits

data SubdivisionBudgetError
  = InvalidBudgetDepth !Int
  | InvalidBudgetLeaves !Int
  | InvalidBudgetBits !Int
  deriving stock (Eq, Show)

subdivisionBudget :: Int -> Int -> Int -> Either SubdivisionBudgetError SubdivisionBudget
subdivisionBudget depth leaves bits
  | depth < 0 = Left (InvalidBudgetDepth depth)
  | leaves <= 0 = Left (InvalidBudgetLeaves leaves)
  | bits <= 0 = Left (InvalidBudgetBits bits)
  | otherwise = Right (SubdivisionBudget depth leaves bits)

budgetDepth :: SubdivisionBudget -> Int
budgetDepth (SubdivisionBudget depth _ _) = depth

budgetLeaves :: SubdivisionBudget -> Int
budgetLeaves (SubdivisionBudget _ leaves _) = leaves

budgetBits :: SubdivisionBudget -> Int
budgetBits (SubdivisionBudget _ _ bits) = bits

-- | The budget a subdivision could not stay within. A bit refusal carries the
-- offending width; it is raised before the value is compared or retained.
data BudgetObligation
  = LeavesExhausted
  | DepthExhausted
  | BitsExhausted !Int
  deriving stock (Eq, Show)

instance NFData BudgetObligation where
  rnf obligation = obligation `seq` ()
