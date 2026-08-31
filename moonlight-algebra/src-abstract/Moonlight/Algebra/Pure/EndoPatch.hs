module Moonlight.Algebra.Pure.EndoPatch
  ( EndoPatch,
    endoPatch,
    endoPatchAssignments,
    endoPatchAdds,
    endoPatchRemoves,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)

type EndoPatch :: Type -> Type
newtype EndoPatch key = EndoPatch
  { endoPatchAssignments :: Map key Bool
  }
  deriving stock (Eq, Show)

endoPatch :: Ord key => Set key -> Set key -> EndoPatch key
endoPatch adds removes =
  EndoPatch
    ( Map.union
        (Map.fromSet (const True) adds)
        (Map.fromSet (const False) removes)
    )

endoPatchAdds :: EndoPatch key -> Set key
endoPatchAdds =
  Map.keysSet . Map.filter id . endoPatchAssignments

endoPatchRemoves :: EndoPatch key -> Set key
endoPatchRemoves =
  Map.keysSet . Map.filter not . endoPatchAssignments

instance Ord key => Semigroup (EndoPatch key) where
  leftPatch <> rightPatch =
    EndoPatch
      (Map.union (endoPatchAssignments rightPatch) (endoPatchAssignments leftPatch))

instance Ord key => Monoid (EndoPatch key) where
  mempty = EndoPatch Map.empty
