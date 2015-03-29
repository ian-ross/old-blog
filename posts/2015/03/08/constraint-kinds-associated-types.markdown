---
author: Ian
title: Constraint kinds and associated types
tags: haskell
published: 2015-03-08 18:32:26
---

This is going to be the oldest of old hat for the cool Haskell kids
who invent existential higher-kinded polymorphic whatsits before
breakfast, but it amused me, and it's the first time I've used some of
these more interesting language extensions for something "real".

<!--MORE-->

### Background

I have a Haskell library called [`hnetcdf`][hnetcdf] for reading and
writing NetCDF files.  NetCDF is a format for gridded data that's very
widely used in climate science, meteorology and oceanography.  A
NetCDF file contains a number of gridded data sets, along with
associated information describing the coordinate axes for the data.
For example, in a climate application, you might have air temperature
or humidity on a latitude/longitude/height grid.

So far, so simple.  There are C and Fortran libraries for reading and
writing NetCDF files and the interfaces are pretty straightforward.
Writing a basic Haskell binding for this stuff isn't very complicated,
but one thing is a little tricky, which is the choice of Haskell type
to represent the gridded data.

In Haskell, we have a number of different array abstractions that are
in common use -- you can think of flattening your array data into a
vector, using a Repa array, using a `hmatrix` matrix, or a number of
other possibilities.  I wanted to support a sort of "store
polymorphism" over these different options, so you'd be able to use
the same approach to read data directly into a Repa array or a
`hmatrix` matrix.

### NcStore: first try

To do this, I wrote an `NcStore` class, whose first version looked
something like this:

~~~~ {.haskell}
class NcStore s where
  toForeignPtr :: Storable e => s e -> ForeignPtr e
  fromForeignPtr :: Storable e => ForeignPtr e -> [Int] -> s e
  smap :: (Storable a, Storable b) => (a -> b) -> s a -> s b
~~~~

It's basically just a way of getting data in and out of a "store", in
the form of a foreign pointer that can be used to pass data to the
NetCDF C functions, plus a mapping method.  This thing can't be a
functor because of the `Storable` constraints on the types to be
stored (which we need so that we can pass these things to C
functions).

That works fine for vectors from `Data.Vector.Storable`:

~~~~ {.haskell}
instance NcStore Vector where
  toForeignPtr = fst . unsafeToForeignPtr0
  fromForeignPtr p s = unsafeFromForeignPtr0 p (Prelude.product s)
  smap = map
~~~~

and for Repa foreign arrays:

~~~~ {.haskell}
import Data.Array.Repa
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Data.Array.Repa.Repr.ForeignPtr (F)

instance Shape sh => NcStore (Array F sh) where
  toForeignPtr = RF.toForeignPtr
  fromForeignPtr p s = RF.fromForeignPtr (shapeOfList $ reverse s) p
  smap f s = computeS $ R.map f s
~~~~

### Additional element type constraints

However, there's a problem if we try to write an instance of `NcStore`
for `hmatrix` matrices.  Most `hmatrix` functions require that the
values stored in a `hmatrix` matrix are instances of the `hmatrix`
`Element` class.  While it's completely trivial to make types
instances of this class (you just write `instance Element Blah` and
you're good), you still need to propagate the `Element` constraint
through your code.  In particular, I needed to use the `hmatrix`
`flatten` function to turn a matrix into a vector of values in
row-major order for passing to the NetCDF C API.  The `flatten`
function has type signature

~~~~ {.haskell}
flatten :: Element t => Matrix t -> Vector t
~~~~

so that `Element` constraint somehow has to get into `NcStore`, but
only for cases when the "store" is a `hmatrix` matrix.

### Constraint kinds to the rescue

At this point, all the real Haskell programmers are asking what the
big deal is.  You just switch on the `ConstraintKinds` and
`TypeFamilies` extensions and rewrite `NcStore` like this:

~~~~ {.haskell}
class NcStore s where
  type NcStoreExtraCon s a :: Constraint
  type NcStoreExtraCon s a = ()
  toForeignPtr :: (Storable e, NcStoreExtraCon s e) =>
                  s e -> ForeignPtr e
  fromForeignPtr :: (Storable e, NcStoreExtraCon s e) =>
                    ForeignPtr e -> [Int] -> s e
  smap :: (Storable a, Storable b, NcStoreExtraCon s a, NcStoreExtraCon s b) =>
          (a -> b) -> s a -> s b
~~~~

Here, I've added an associated type called `NcStoreExtraCon s a`,
which is a *constraint*, I've given a default for this (of `()`, which
is a "do nothing" empty constraint), and I've added the relevant
constraint to each of the methods of `NcStore`.  The `NcStore`
instances for storable `Vector`s and Repa arrays look the same as
before, but the instance for `hmatrix` matrices now looks like this:

~~~~ {.haskell}
instance NcStore HMatrix where
  type NcStoreExtraCon HMatrix a = C.Element a
  toForeignPtr (HMatrix m) = fst3 $ unsafeToForeignPtr $ C.flatten m
  fromForeignPtr p s =
    let c = last s
        d = product s
    in HMatrix $ matrixFromVector RowMajor (d `div` c) (last s) $
       unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HMatrix m) = HMatrix $ C.mapMatrix f m
~~~~

I've just added the `Element` constraint on the type of values
contained in the "store" to the instance, and I can then use any
`hmatrix` function that requires this constraint without any trouble:
you can see the use of `flatten` in the `toForeignPtr` method
definition.

### Conclusions

The problem I had here is really just an instance of what's come to be
called the "restricted monad" problem.  This is where you have a type
class, possibly with constraints, and you want to write instances of
the class where you impose additional constraints.  The classic case
is making `Set` a monad: `Set` requires its elements to be members of
`Ord`, but `Monad` is fully polymorphic, and so there's no way to make
an instance of something like `Ord a => Monad (Set a)`.

There's even a package on Hackage called [`rmonad`][rmonad] that uses
just this "constraint kinds + associated types" approach to allow you
to write "restricted monads" of this kind.  So this appears to be a
well-known method, but it was fun to rediscover it.  The ability to
combine these two language extensions in this (to me) quite unexpected
way is really rather satisfying!


[hnetcdf]: https://hackage.haskell.org/package/hnetcdf
[rmonad]: https://hackage.haskell.org/package/rmonad
