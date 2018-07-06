---
author: Ian
title: C2HS 0.25.1 "Snowmelt"
tags: haskell
published: 2015-03-30 09:43:39
---

I took over the day-to-day support for [C2HS][c2hs] about 18 months
ago and have now finally cleaned up all the issues on the
[GitHub issue tracker][gh-issues].  It took a *lot* longer than I was
expecting, mostly due to pesky "real work" getting in the way.  Now
seems like a good time to announce the 0.25.1 "Snowmelt" release of
C2HS and to summarise some of the more interesting new C2HS features.

<!--MORE-->

##### Regression suite and Travis testing

When I first started working on C2HS, I kept breaking things and
getting emails letting me know that such-and-such a package no longer
worked.  That got boring pretty quickly, so I wrote a
[Shelly][shelly]-driven regression suite to build a range of packages
that use C2HS to check for breakages.  This now runs on
[Travis CI][travis] so that whenever a C2HS change is pushed to
GitHub, as well as the main C2HS test suite, a bunch of C2HS-dependent
packages are built.  This has been pretty handy for avoiding some
stupid mistakes.

##### Enum handling

Thanks to work contributed by [Philipp Balzarek][philonous], the
treatment of the mapping between C `enum` values and Haskell `Enum`
types is now *much* better than it was.  The C `enum`/Haskell `Enum`
association is kind of an awkward fit, since the C and Haskell worlds
make really quite different assumptions about what an "enumerated"
type is, and the coincidence of names is less meaningful than you
might hope.  We might have to do some more work on that in the future:
I've been thinking about whether it would be good to have a `CEnum`
class in `Foreign.C.Types` to capture just the features of C `enums`
that can be mapped to Haskell types in a sensible way.

##### Finalizers for foreign pointers

You can now say things like:

~~~~ {.haskell}
#include <stdio.h>

{#pointer *FILE as File foreign finalizer fclose newtype#}

{#fun fopen as ^ {`String', `String'} -> `File'#}
{#fun fileno as ^ {`File'} -> `Int'#}

main :: IO ()
main = do
  f <- fopen "tst.txt" "w"
  ...
~~~~

and the file descriptor `f` will be cleaned up by a call to `fclose`
via the Haskell garbage collector.  This encapsulates a very common
use case for handling pointers to C structures allocated by library
functions.  Previously there was no direct way to associate finalizers
with foreign pointers in C2HS, but now it's easy.

##### Easy access to preprocessor constants

C2HS has a new `const` hook for directly accessing the value of C
preprocessor constants -- you can just say `{#const FOO#}` to use the
value of a constant `FOO` defined in a C header in Haskell code.

##### Special case argument marshalling

I've implemented a couple of special mechanisms for argument
marshalling that were requested.  The first of these is a little
esoteric, but an example should make it clear.  A common pattern in
some C libraries is to have code that looks like this:

~~~~ {.c}
typedef struct {
  int a;
  float b;
  char dummy;
} oid;

void func(oid *obj, int aval, float bval);
int oid_a(oid *obj);
float oid_b(oid *obj);
~~~~

Here the function `func` takes a pointer to an `oid` structure and
fills in the values in the structure and the other functions take
`oid` pointers and do various things with them.  Dealing with
functions like `func` through the Haskell FFI is a tedious because you
need to allocate space for an `oid` structure, marshall a pointer to
the allocated space and so on.  Now though, the C2HS code

~~~~ {.haskell}
{#pointer *oid as Oid foreign newtype#}

{#fun func as ^ {+, `Int', `Float'} -> `Oid'#}
~~~~

generates Haskell code like this:

~~~~ {.haskell}
newtype Oid = Oid (ForeignPtr Oid)
withOid :: Oid -> (Ptr Oid -> IO b) -> IO b
withOid (Oid fptr) = withForeignPtr fptr

func :: Int -> Float -> IO Oid
func a2 a3 =
  mallocForeignPtrBytes 12 >>= \a1'' -> withForeignPtr a1'' $ \a1' ->
  let {a2' = fromIntegral a2} in
  let {a3' = realToFrac a3} in
  func'_ a1' a2' a3' >>
  return (Oid a1'')
~~~~

This allocates the right amount of space using the fast
`mallocForeignPtrBytes` function and deals with all the marshalling
for you.  The special `+` parameter in the C2HS function hook
definition triggers this (admittedly rather specialised) case.

The second kind of "special" argument marshalling is more general.  A
lot of C libraries include functions where small structures are passed
"bare", i.e. not as pointers.  The Haskell FFI doesn't include a means
to marshal arguments of this type, which makes using libraries of this
kind painful, with a lot of boilerplate marshalling code needed (just
the kind of thing C2HS is supposed to eliminate!).  The solution I
came up with for C2HS is to add an argument annotation for function
hooks that says that a structure pointer should really be passed as a
bare structure.  In such cases, C2HS then generates an additional C
wrapper function to marshal between structure pointer and bare
structure arguments.  An example will make this clear.  Suppose you
have some code in a C header:

~~~~ {.c}
typedef struct {
  int x;
  int y;
} coord_t;

coord_t *make_coord(int x, int y);
void free_coord(coord_t *coord);
int coord_x(coord_t c, int dummy);
~~~~

Here, the `coord_x` function takes a bare `coord_t` structure as a
parameter.  To bind to these functions in C2HS code, we write this:

~~~~ {.haskell}
{#pointer *coord_t as CoordPtr foreign finalizer free_coord newtype#}

{#fun pure make_coord as makeCoord {`Int', `Int'} -> `CoordPtr'#}
{#fun pure coord_x as coordX {%`CoordPtr', `Int'} -> `Int'#}
~~~~

Here, the `%` annotation on the `CoordPtr` argument to the `coordX`
function hook tells C2HS that this argument needs to be marshalled as
a bare structure.  C2HS then generates Haskell code as usual, but also
an extra `.chs.c` file containing wrapper functions.  This C code
needs to be compiled and linked to the Haskell code.

This is kind of new and isn't yet really supported by released
versions of Cabal.  I've made some Cabal changes to support this,
which have been merged and will hopefully go into the next or next but
one Cabal release.  When that's done, the handling of the C wrapper
code will be transparent -- Cabal will know that C2HS has generated
these extra C files and will add them to the "C sources" list for
whatever it's building.

##### Binding to variadic C functions

Previously, variadic C functions weren't supported in C2HS at all.
Now though, you can do fun things like this:

~~~~ {.haskell}
#include <stdio.h>

{#fun variadic printf[int] as printi {`String', `Int'} -> `()'#}
{#fun variadic printf[int, int] as printi2 {`String', `Int', `Int'} -> `()'#}
{#fun variadic printf[const char *] as prints {`String', `String'} -> `()'#}
~~~~

You need to give distinct names for the Haskell functions to be bound
to different calling sequences of the underlying C function, and
because there's no other way of finding them out, you need to specify
explicit types for the arguments you want to pass in the place of C's
`...` variadic argument container (that's what the C types in the
square brackets are).  Once you do that, you can call `printf` and
friends to your heart's content.  (The user who wanted this feature
wanted to use it for calling Unix `ioctl`...)

##### User-defined default marshallers

A big benefit of C2HS is that it tries quite hard to manage the
associations between C and Haskell types and the marshalling of
arguments between C and Haskell.  To that end, we have a lot of
default marshallers that allow you very quickly to write FFI bindings.
However, we can't cover every case.  There were a few long-standing
issues (imported from the original Trac issue tracker when I moved the
project to GitHub) asking for default marshalling for various C
standard or "standardish" `typedefs`.  I held off on trying to fix
those problems for a long time, mostly because I thought that fixing
them one at a time as special cases would be a little futile and would
just devolve into endless additions of "just one more" case.

In the end, I implemented a general scheme to allow users to
explicitly associate C `typedef` names with Haskell types and to
define default marshallers between them.  As an example, using this
facility, you can write code to marshal Haskell `String` values to and
from C wide character strings like this:

~~~~ {.haskell}
#include <wchar.h>

{#typedef wchar_t CWchar#}
{#default in `String' [wchar_t *] withCWString* #}
{#default out `String' [wchar_t *] peekCWString* #}
{#fun wcscmp {`String', `String'} -> `Int'#}
{#fun wcscat {`String', `String'} -> `String'#}
~~~~

I think that's kind of fun...

##### Miscellany

As well as the features described above, there's a lot more that's
been done over the last 18 months: better handling of structure tags
and `typedefs`; better cross-platform support (OS X, FreeBSD and
Windows); lots more default marshallers; support for parameterised
pointer types; some vague gestures in the direction of "backwards
compatibility" (basically just a `C2HS_MIN_VERSION` macro); and just
in the last couple of days, some changes to deal with marshalling of C
`bool` values (really C99 `_Bool`) which aren't supported directly by
the Haskell FFI (so again require some wrapper code and some other
tricks).


### Contributors

As well as myself and Manuel Chakravarty, the original author of C2HS,
the following people have contributed to C2HS development over the
last 18 months (real names where known, GitHub handles otherwise):

 * Anton Dessiatov
 * Boyun Tang
 * Cindy Wang
 * Dimitri Sabadie
 * Facundo Dominguez
 * Index Int
 * j-keck
 * Kai Harries
 * Merijn Verstraaten
 * Michael Steele
 * Philipp Balzarek
 * RyanGlScott
 * Sivert Berg
 * watashi
 * Zejun Wu

Many thanks to all of them, and many thanks also to Benedikt Huber,
who maintains the [`language-c`][language-c] package on which C2HS is
critically dependent!


### What next?

All of the work I've done on C2HS has been driven purely by user
demand, based on issues I imported from the original Trac issue
tracker and then on things that people have asked for on GitHub.
(Think of it as a sort of call-by-need exploration of the C2HS design
space.)  I'm now anticipating that since I've raised my head above the
parapet by touting all these shiney new features, I can expect a new
stream of bug reports to come in...

One potential remaining large task is to "sort out" the Haskell C
language libraries, of which there are now at least three, all with
different pros and cons.  The `language-c` library used in C2HS has
some analysis capabilities that aren't present in the other libraries,
but the other libraries (notably Geoffrey Mainland's
[`language-c-quote`][language-c-quote] and Manuel's
[`language-c-inline`][language-c-inline]) support more recent dialects
of C.  Many of the issues with C2HS on OS X stem from modern C
features that occur in some of the OS X headers that the `language-c`
package just doesn't recognise.  Using one of the other C language
packages might alleviate some of those problems.  To do that though,
some unholy mushing-together of `language-c` and one of these other
packages has to happen, in order to bring the analysis capabilities of
`language-c` to the other package.  That doesn't look like much fun at
all, so I might ignore the problem and hope it goes away.

I guess longer term the question is whether tools like C2HS really
have a future.  There are better approaches to FFI programming being
developed by research groups (Manuel's is one of them:
[this talk][manuel-talk] is pretty interesting) so maybe we should
just wait until they're ready for prime time.  On the other hand,
quite a lot of people seem to use C2HS, and it is pretty convenient.

One C2HS design decision I've recently had to modify a little is that
C2HS tries to use only information available via the "official"
Haskell FFI.  Unfortunately, there are situations where that just
isn't enough.  The recent changes to marshal C99 `_Bool` values are a
case in point.  In order to determine offsets into structures
containing `_Bool` members, you need to know how big a `_Bool` is.
Types that are marshalled by the Haskell FFI are all instances of
`Storable`, so you can just use the `size` method from `Storable` for
this.  However, the Haskell FFI doesn't know anything about `_Bool`,
so you end up having to "query" the C compiler for the information by
generating a little C test program that you compile and run.  (You can
find out which C compiler to use from the output of `ghc --info`,
which C2HS thus needs to run first.)  This is all pretty nasty, but
there's no obvious other way to do it.

This makes me think, since I'm having to do this anyway, that it might
be worth reorganising some of C2HS's structure member offset
calculation code to use the same sort of "query the C compiler"
approach.  There are some cases
(e.g. [structures within structures][issue-129]) where it's just not
possible to reliably calculate structure member offsets from the size
and alignment information available through the Haskell FFI -- the C
compiler is free to insert padding between structure members, and you
can't work out just by looking when a particular compiler is going to
do that.  Generating little C test programs and compiling and running
them allows you to get the relevant information "straight from the
horse's mouth"...  (I don't know whether this idea really has legs,
but it's one thing I'm thinking about.)


[c2hs]: http://hackage.haskell.org/package/c2hs
[gh-issues]: https://github.com/haskell/c2hs/issues
[shelly]: http://hackage.haskell.org/package/shelly
[travis]: https://travis-ci.org/haskell/c2hs
[philonous]: https://github.com/Philonous
[language-c]: http://hackage.haskell.org/package/language-c
[language-c-quote]: http://hackage.haskell.org/package/language-c-quote
[language-c-inline]: http://hackage.haskell.org/package/language-c-inline
[manuel-talk]: https://speakerdeck.com/mchakravarty/foreign-inline-code-in-haskell-yow-lambda-jam-2014
[issue-129]: https://github.com/haskell/c2hs/issues/129
