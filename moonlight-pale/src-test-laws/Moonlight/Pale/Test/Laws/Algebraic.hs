{-| Predicates for algebraic, lattice, module, and action laws. -}
module Moonlight.Pale.Test.Laws.Algebraic
  ( monoidAssociativity,
    monoidLeftIdentity,
    monoidRightIdentity,
    groupLeftInverse,
    groupRightInverse,
    abelianCommutativity,
    semigroupAssociativity,
    ringAdditiveAssociativity,
    ringAdditiveCommutativity,
    ringAdditiveLeftIdentity,
    ringAdditiveRightIdentity,
    ringAdditiveLeftInverse,
    ringAdditiveRightInverse,
    ringMultiplicativeAssociativity,
    ringMultiplicativeLeftIdentity,
    ringMultiplicativeRightIdentity,
    ringDistributivityLeft,
    ringDistributivityRight,
    ringMultiplicativeCommutativity,
    latticeAbsorptionJoin,
    latticeAbsorptionMeet,
    latticeIdempotenceJoin,
    latticeIdempotenceMeet,
    latticeAssociativityJoin,
    latticeAssociativityMeet,
    latticeCommutativityJoin,
    latticeCommutativityMeet,
    distributiveLatticeJoinOverMeet,
    distributiveLatticeMeetOverJoin,
    booleanAlgebraComplementJoin,
    booleanAlgebraComplementMeet,
    idempotentLaw,
    moduleDistributivityScalar,
    moduleDistributivityVector,
    moduleCompatibility,
    moduleIdentity,
    actionAssociativity,
    actionIdentity,
  )
where

import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..), MultiplicativeMonoid (..), Ring)

monoidAssociativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
monoidAssociativity op x y z = op (op x y) z == op x (op y z)

monoidLeftIdentity :: Eq a => (a -> a -> a) -> a -> a -> Bool
monoidLeftIdentity op e x = op e x == x

monoidRightIdentity :: Eq a => (a -> a -> a) -> a -> a -> Bool
monoidRightIdentity op e x = op x e == x

groupLeftInverse :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
groupLeftInverse op inv e x = op (inv x) x == e

groupRightInverse :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
groupRightInverse op inv e x = op x (inv x) == e

abelianCommutativity :: Eq a => (a -> a -> a) -> a -> a -> Bool
abelianCommutativity op x y = op x y == op y x

semigroupAssociativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
semigroupAssociativity = monoidAssociativity

ringAdditiveAssociativity :: (Eq a, AdditiveGroup a) => a -> a -> a -> Bool
ringAdditiveAssociativity x y z = add (add x y) z == add x (add y z)

ringAdditiveCommutativity :: (Eq a, AdditiveGroup a) => a -> a -> Bool
ringAdditiveCommutativity x y = add x y == add y x

ringAdditiveLeftIdentity :: (Eq a, AdditiveGroup a) => a -> Bool
ringAdditiveLeftIdentity x = add zero x == x

ringAdditiveRightIdentity :: (Eq a, AdditiveGroup a) => a -> Bool
ringAdditiveRightIdentity x = add x zero == x

ringAdditiveLeftInverse :: (Eq a, AdditiveGroup a) => a -> Bool
ringAdditiveLeftInverse x = add (neg x) x == zero

ringAdditiveRightInverse :: (Eq a, AdditiveGroup a) => a -> Bool
ringAdditiveRightInverse x = add x (neg x) == zero

ringMultiplicativeAssociativity :: (Eq a, MultiplicativeMonoid a) => a -> a -> a -> Bool
ringMultiplicativeAssociativity x y z = mul (mul x y) z == mul x (mul y z)

ringMultiplicativeLeftIdentity :: (Eq a, MultiplicativeMonoid a) => a -> Bool
ringMultiplicativeLeftIdentity x = mul one x == x

ringMultiplicativeRightIdentity :: (Eq a, MultiplicativeMonoid a) => a -> Bool
ringMultiplicativeRightIdentity x = mul x one == x

ringDistributivityLeft :: (Eq a, Ring a) => a -> a -> a -> Bool
ringDistributivityLeft x y z = mul x (add y z) == add (mul x y) (mul x z)

ringDistributivityRight :: (Eq a, Ring a) => a -> a -> a -> Bool
ringDistributivityRight x y z = mul (add x y) z == add (mul x z) (mul y z)

ringMultiplicativeCommutativity :: (Eq a, MultiplicativeMonoid a) => a -> a -> Bool
ringMultiplicativeCommutativity x y = mul x y == mul y x

latticeAbsorptionJoin :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> Bool
latticeAbsorptionJoin ljoin lmeet x y = ljoin x (lmeet x y) == x

latticeAbsorptionMeet :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> Bool
latticeAbsorptionMeet ljoin lmeet x y = lmeet x (ljoin x y) == x

latticeIdempotenceJoin :: Eq a => (a -> a -> a) -> a -> Bool
latticeIdempotenceJoin ljoin x = ljoin x x == x

latticeIdempotenceMeet :: Eq a => (a -> a -> a) -> a -> Bool
latticeIdempotenceMeet lmeet x = lmeet x x == x

latticeAssociativityJoin :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
latticeAssociativityJoin ljoin x y z = ljoin (ljoin x y) z == ljoin x (ljoin y z)

latticeAssociativityMeet :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
latticeAssociativityMeet lmeet x y z = lmeet (lmeet x y) z == lmeet x (lmeet y z)

latticeCommutativityJoin :: Eq a => (a -> a -> a) -> a -> a -> Bool
latticeCommutativityJoin ljoin x y = ljoin x y == ljoin y x

latticeCommutativityMeet :: Eq a => (a -> a -> a) -> a -> a -> Bool
latticeCommutativityMeet lmeet x y = lmeet x y == lmeet y x

distributiveLatticeJoinOverMeet :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributiveLatticeJoinOverMeet ljoin lmeet x y z =
  ljoin x (lmeet y z) == lmeet (ljoin x y) (ljoin x z)

distributiveLatticeMeetOverJoin :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributiveLatticeMeetOverJoin ljoin lmeet x y z =
  lmeet x (ljoin y z) == ljoin (lmeet x y) (lmeet x z)

booleanAlgebraComplementJoin :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
booleanAlgebraComplementJoin ljoin compl topElement x =
  ljoin x (compl x) == topElement

booleanAlgebraComplementMeet :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
booleanAlgebraComplementMeet lmeet compl bottomElement x =
  lmeet x (compl x) == bottomElement

idempotentLaw :: Eq a => (a -> a) -> a -> Bool
idempotentLaw f x = f (f x) == f x

moduleDistributivityScalar ::
  (Eq m) =>
  (r -> r -> r) ->
  (m -> m -> m) ->
  (r -> m -> m) ->
  r ->
  r ->
  m ->
  Bool
moduleDistributivityScalar rAdd mAdd mScale r s x =
  mScale (rAdd r s) x == mAdd (mScale r x) (mScale s x)

moduleDistributivityVector ::
  (Eq m) =>
  (m -> m -> m) ->
  (r -> m -> m) ->
  r ->
  m ->
  m ->
  Bool
moduleDistributivityVector mAdd mScale r x y =
  mScale r (mAdd x y) == mAdd (mScale r x) (mScale r y)

moduleCompatibility ::
  (Eq m) =>
  (r -> r -> r) ->
  (r -> m -> m) ->
  r ->
  r ->
  m ->
  Bool
moduleCompatibility rMul mScale r s x =
  mScale (rMul r s) x == mScale r (mScale s x)

moduleIdentity :: (Eq m) => r -> (r -> m -> m) -> m -> Bool
moduleIdentity rOne mScale x = mScale rOne x == x

actionAssociativity ::
  (Eq s) =>
  (m -> m -> m) ->
  (m -> s -> s) ->
  m ->
  m ->
  s ->
  Bool
actionAssociativity mOp mAct g h x =
  mAct (mOp g h) x == mAct g (mAct h x)

actionIdentity :: (Eq s) => m -> (m -> s -> s) -> s -> Bool
actionIdentity e mAct x = mAct e x == x
