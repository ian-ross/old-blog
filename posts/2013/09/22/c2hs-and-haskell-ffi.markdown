---
author: Ian
tags: haskell
title: Learning the Haskell FFI with C2HS
published: 2013-09-22 22:02:34
---

I've recently started helping with the maintenance of [C2HS][c2hs], a
tool for generating Haskell foreign function interface (FFI) bindings
from C header files.  I started using C2HS because of a
[Haskell library][hnetcdf] I was writing to read
[Unidata NetCDF][netcdf] files.  I didn't fancy writing all of the
bindings to the hundreds of functions in the C NetCDF library by hand
and C2HS seemed like a good way to get started with the Haskell FFI.

The Haskell FFI is well-documented in the [Language Report][ffi] and
the Haddock documentation for the `Foreign` modules that define
various helper types and marshalling functions ([`Foreign`][foreign]
and [`Foreign.C`][foreignc] are good starting points).  However, even
with the documentation, the FFI is pretty complicated, and there are
lots of choices for marshalling and for allocating memory for
communication between Haskell code and C code: there are `Ptr`s and
`ForeignPtr`s, there are various functions for allocating memory with
different lifetimes, there are lots of types floating around, and it's
quite confusing when you're getting started.

This is where I found starting with C2HS really useful.  It's very
easy to write the C2HS specification for a C function, then you can
run the C2HS tool over your code and look at the Haskell marshalling
code it produces.  For instance, for the C function

~~~~ {.c}
double foo(int n, int *status);
~~~~

where `n` is an input parameter and `status` is used as an output
parameter, the C2HS specification looks like:

~~~~ {.haskell}
{#fun foo { `Int', alloca- `Int' peekIntConv* } -> `Double' #}
~~~~

The annotations around the second parameter to `foo` are used to
indicate that some memory needs to be allocated for the `status`
pointer parameter and that the value of this pointer needs to be
accessed and returned monadically (the [C2HS documentation][c2hsdoc]
is pretty good about explaining these things).  After running C2HS on
this definition, you get some Haskell code that looks like this (after
cleaning up a little and renaming some things):

~~~~ {.haskell}
foo :: Int -> IO (Double, Int)
foo n = let n1 = fromIntegral n
        in alloca $ \ptr ->
        foo'_ n1 ptr >>= \res ->
        let resout = realToFrac res
        in peekIntConv pre >>= \ptrout ->
        return (resout, ptrout)

foreign import ccall safe "Eg1.chs.h foo"
  foo'_ :: CInt -> Ptr CInt -> IO CDouble

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM fromIntegral . peek
~~~~

As well as the `foreign import` of the `foo` function from its C
header file, this has a `foo` function that deals with all of the type
conversion and argument and result marshalling between Haskell and C:
conversion between Haskell `Int` and `Double` types and `CInt`s and
`CDouble`s for communicating with C functions is done using
`fromIntegral` and `realToFrac`, allocation of memory and result
extraction from the integer pointer `status` parameter is done, with
the sequence of allocation, C function call and result extraction
being ordered monadically.

As a more complicated example, consider this function from the NetCDF
C library:

~~~~ {.c}
int nc_def_var(int ncid, const char *name, nc_type xtype, int ndims,
               const int *dimidsp, int *varidp);
~~~~

A suitable C2HS specification for this function is:

~~~~ {.haskell}
{#fun nc_def_var { `Int', `String', `Int', `Int',
                   withIntArray* `[Int]',
                   alloca- `Int' peekIntConv* } -> `Int' #}
~~~~

This function has a C string parameter as well as an integer array
passed as an input parameter (which we're going to represent as a
Haskell list) and an integer pointer used as an output parameter.  The
`withIntArray` function is a marshalling helper.  From this
specification, C2HS produces the following Haskell code:

~~~~ {.haskell}
nc_def_var :: Int -> String -> Int -> Int -> [Int] -> IO (Int, Int)
nc_def_var a1 a2 a3 a4 a5 =
  let a1' = fromIntegral a1 in
  withCString a2 $ \a2' ->
  let a3' = fromIntegral a3 in
  let a4' = fromIntegral a4 in
  withIntArray a5 $ \a5' ->
  alloca $ \a6' ->
  nc_def_var'_ a1' a2' a3' a4' a5' a6' >>= \res ->
  let res' = fromIntegral res in
  peekIntConv  a6'>>= \a6'' ->
  return (res', a6'')

foreign import ccall safe "Data/NetCDF/Raw/Base.chs.h nc_def_var"
  nc_def_var'_ :: CInt -> Ptr CChar -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

withIntArray :: (Storable a, Integral a) => [a] -> (Ptr CInt -> IO b) -> IO b
withIntArray = withArray . liftM fromIntegral
~~~~

This gives a pretty good idea of how to deal with marshalling of input
and output arguments of different types, including C strings (it uses
the `withCString` function from `Foreign.C.String`) and arrays (the
`withIntArray` function is just a specialisation of `withArray` from
`Foreign.Marshal.Array`).

Once you've looked at a few examples like this (and more complex
ones), it becomes much easier to figure out how to abstract these
patterns for more complex cases.  For most of the functions in my
NetCDF library, I don't use C2HS specifications directly, but use
parameterised functions I wrote based on experiments done with C2HS.
In the beginning, I would definitely have had a hard time writing this
sort of parameterised FFI function without the examples provided by
C2HS.  From that perspective, C2HS is really a pretty neat tool for
learning how to use the Haskell FFI (you could probably do just the
same kind of thing with `hsc2hs` or similar tools).


[c2hs]: https://github.com/haskell/c2hs
[hnetcdf]: http://hackage.haskell.org/package/hnetcdf
[netcdf]: http://www.unidata.ucar.edu/software/netcdf/
[ffi]: http://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1490008
[foreign]: http://hackage.haskell.org/packages/archive/base/4.5.0.0/doc/html/Foreign.html
[foreignc]: http://hackage.haskell.org/packages/archive/base/4.5.0.0/doc/html/Foreign-C.html
[c2hsdoc]: https://github.com/haskell/c2hs/wiki/User-Guide
