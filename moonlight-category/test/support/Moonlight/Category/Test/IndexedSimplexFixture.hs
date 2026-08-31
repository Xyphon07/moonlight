module Moonlight.Category.Test.IndexedSimplexFixture
  ( Zero,
    One,
    Two,
    Three,
    zero,
    one,
    two,
    coface0At0,
    coface1At0,
    coface0At1,
    coface1At1,
    coface2At1,
    coface0At2,
    coface1At2,
    coface2At2,
    coface3At2,
    codegeneracy0At0,
    codegeneracy0At1,
    codegeneracy1At1,
    codegeneracy0At2,
    codegeneracy1At2,
    codegeneracy2At2,
  )
where

import Moonlight.Category.Indexed qualified as Indexed

type Zero = Indexed.Z

type One = Indexed.S Zero

type Two = Indexed.S One

type Three = Indexed.S Two

zero :: Indexed.Simplex Zero Zero
zero = Indexed.simplexZero

one :: Indexed.Simplex One One
one = Indexed.simplexSucc zero

two :: Indexed.Simplex Two Two
two = Indexed.simplexSucc one

coface0At0 :: Indexed.Simplex Zero One
coface0At0 = Indexed.cofaceFirst zero

coface1At0 :: Indexed.Simplex Zero One
coface1At0 = Indexed.cofaceLast zero

coface0At1 :: Indexed.Simplex One Two
coface0At1 = Indexed.cofaceFirst one

coface1At1 :: Indexed.Simplex One Two
coface1At1 = Indexed.cofaceSucc coface0At0

coface2At1 :: Indexed.Simplex One Two
coface2At1 = Indexed.cofaceLast one

coface0At2 :: Indexed.Simplex Two Three
coface0At2 = Indexed.cofaceFirst two

coface1At2 :: Indexed.Simplex Two Three
coface1At2 = Indexed.cofaceSucc coface0At1

coface2At2 :: Indexed.Simplex Two Three
coface2At2 = Indexed.cofaceSucc coface1At1

coface3At2 :: Indexed.Simplex Two Three
coface3At2 = Indexed.cofaceLast two

codegeneracy0At0 :: Indexed.Simplex One Zero
codegeneracy0At0 = Indexed.codegeneracyFirst zero

codegeneracy0At1 :: Indexed.Simplex Two One
codegeneracy0At1 = Indexed.codegeneracyFirst one

codegeneracy1At1 :: Indexed.Simplex Two One
codegeneracy1At1 = Indexed.codegeneracyLast one

codegeneracy0At2 :: Indexed.Simplex Three Two
codegeneracy0At2 = Indexed.codegeneracyFirst two

codegeneracy1At2 :: Indexed.Simplex Three Two
codegeneracy1At2 = Indexed.codegeneracySucc codegeneracy0At1

codegeneracy2At2 :: Indexed.Simplex Three Two
codegeneracy2At2 = Indexed.codegeneracyLast two
