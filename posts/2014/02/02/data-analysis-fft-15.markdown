---
author: Ian
tags: data-analysis,haskell
title: "Haskell FFT 15: Some small Haskell things"
published: 2014-02-02 17:33:45
---

After releasing the `arb-fft` package last week, a couple of issues
were pointed out by Daniel Díaz, Pedro Magalhães and Carter
Schonwald.  I've made another couple of releases to fix these things.
They were mostly little Haskell things that I didn't know about, so
I'm going to quickly describe them here.  (I'm sure these are things
that most people know about already...)

<!--MORE-->

### Generic vectors

The first thing was the API.  I had given the main `fft` function the
following type:

~~~~ {.haskell}
fft :: Vector (Complex Double) -> IO (Vector (Complex Double))
~~~~

where `Vector` was the usual boxed vector type from `Data.Vector`.
This was obviously not so convenient if you already had an unboxed
vector you wanted to transform -- all of the `arb-fft` code uses
unboxed vectors internally, so you would have to do some useless
conversions between vector types.

The main `fft` function now has the type:

~~~~ {.haskell}
fft :: Vector v (Complex Double) =>
       v (Complex Double) -> IO (v (Complex Double))
~~~~

Here, `Vector` is a typeclass imported from `Data.Vector.Generic`,
which allows you to use write functions that work with both boxed and
unboxed vectors transparently.  The `fftWith` function now looks like
this:

~~~~ {.haskell}
fftWith :: Vector v (Complex Double) =>
           Plan -> v (Complex Double) -> v (Complex Double)
fftWith p = convert . execute p Forward . convert
~~~~

The `execute` function uses *unboxed* vectors for both its parameter
and return types, so if you call `fftWith` with an unboxed vector, the
calls to `convert` are no-ops and should get optimised away.  If you
have a boxed vector, then the calls to `convert` are needed anyway, to
get things into the form used inside `execute`.

This generic approach is nice and I hadn't seen it before.  After
Daniel asked about using unboxed vectors, the word "generic" in the
documentation for the `vector` package sounded kind of intriguing and
it became obvious pretty quickly that this was the way to do things.

### Cabal build

Because I saw significantly better performance using the LLVM backend
for GHC in my benchmarking, I'd set the `-fllvm` flag in the
`ghc-options` field in the Cabal file for `arb-fft`.  That was silly,
since it meant that the package wouldn't build if you didn't have the
LLVM tools installed.

The obvious solution was to add a Cabal flag to control whether or not
the LLVM backend was used.  Then I got ambitious, and decided that I
wanted to have some slightly intelligent behaviour for managing this
flag: if the LLVM tools are installed, you should be able to specify
whatever value you want for the flag; if the LLVM tools aren't
installed, then the flag shouldn't be set, whatever the user says, and
there ought to be some sort of message warning that the native backend
is being used and that the code is likely to be slower.

That's a bit more than you can specify in the Cabal file.  However,
because Cabal is a Haskell library that you can do more or less
anything with if you want, all you need to do is change the
`build-type` field in the Cabal file to `Custom`, and write a custom
`Setup.hs` program to do the relevant checking and flag twiddling.

I've played a little with the Cabal API for some other tasks in the
past, and I'm always pleasantly surprised how easy it is to do things
with it.  The same was true this time out: the default build process
in Cabal has hooks that you can use to perform custom actions at any
point in the build process.  In my case, a simple function to check
for the presence of LLVM during configuration was enough.

### Images in Haddocks

Because I'd written all those blog articles, I rather neglected the
Haddock documentation for the package, to the extent that there wasn't
any way of telling from the documentation what the main FFT and
inverse FFT functions actually calculated...  Oops.

Fixing the documentation for the main APIs was pretty trivial, of
course, except that I wanted formulae in my Haddock pages!  There were
some [good suggestions][formulae] on the Haskell Cafe a few weeks ago,
and I ended up using LaTeX and the `dvisvgm` program to generate SVG
images that I could include in the Haddock pages using the
`<<image-url>>` syntax.

The only slight annoyance with this was that Cabal 1.18 has a new
`extra-doc-files` field that allows you to specify things like image
files that need to be made available within the generated HTML
documentation, but this isn't yet supported on Hackage.  I just
dropped the SVGs on another server and made links to point there, but
once Hackage has this feature set up, including images in Haddocks
will become really easy.

### License

The last thing was licensing.  I'd originally released `arb-fft` under
the GPL-3 license, but after a little discussion with Carter Schonwald
and others, I've now re-released things under the BSD3 license.
Carter's idea is that releasing code under permissive licenses is a
way to funnel businesses into using things, which hopefully leads, one
way or another, to requirements for further development and possibly
even the Holy Grail of paid work.

I've been contracting for just over a year now, and it's not been
going too badly, but I'm still trying to figure out the best way to be
able to do interesting work and get paid for it.  I know I'm not the
only one in this situation!

[formulae]: http://www.haskell.org/pipermail/haskell-cafe/2014-January/112063.html
