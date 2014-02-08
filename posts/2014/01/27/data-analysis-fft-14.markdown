---
author: Ian
tags: data-analysis,haskell
title: Haskell FFT 14: Wrap-up
published: 2014-01-26 09:26:43
---

After thirteen blog articles and about two-and-a-half weeks'
equivalent work time spread out over three months, I think it's time
to stop with the FFT stuff for a while.  I told myself when I started
all this that if I could get within a factor of 20 of the performance
of FFTW, I would be "quite obscenely pleased with myself".  For most
$N$, the performance of our FFT code is within a factor of 5 or so of
the performance of the `vector-fftw` package.  For a pure Haskell
implementation with some but not lots of optimisation work, that's not
too shabby.

But now, my notes are up to nearly 90 pages and, while there are
definitely things left to do, I'd like to move on for a bit and do
some other data analysis tasks.  This post is just to give a quick
round-up of what we've done, and to lay out the things remaining to be
done (some of which I plan to do at some point and some others that
I'd be very happy for other people to do!).  I've released the latest
version of the code to Hackage.  The package is called `arb-fft`.
There is also an [index][index] of all the posts in this series.

## Lessons learned

The lessons I've taken from this exercise are all things that more
experienced Haskellers have known for some time, but it's interesting
to see how things play out in a realistic example:

 * Haskell is a great language for writing clear code.  The earlier
   versions of the FFT code were, in many cases, more or less direct
   transcriptions of my mental models of the way these algorithms
   should work.  This isn't such a big deal for a relatively simple
   example like the FFT, but when you're dealing with more complicated
   problems, that clarity of expression is really valuable.

 * [QuickCheck][quickcheck] is a wonderful testing tool and
   [Criterion][criterion] is a wonderful benchmarking tool.  But
   *everyone* knows that!

 * Clear code isn't necessarily fast code, but when it comes time to
   optimise, Haskell allows you to isolate any "nastiness" you
   introduce.  By this, I mean for example, that where I've fallen
   back on using stateful calculations and mutable vectors, the `ST`
   monad allows this stateful computation to be "locked away" so that
   no state leaks out into the surrounding pure code.  This isolation
   makes reasoning about code much easier than in a comparable piece
   of procedural code &mdash; you know, because the type system guarantees
   it, that no references can leak from your stateful code: compare
   the situation in C++, where you need to be very careful to manage
   aliasing yourself because the language definition provides only
   limited scope to the compiler to help you.

 * Having a language that allows you to switch seamlessly between
   symbolic and numeric computation is nice.  Quite a lot of the
   planning code is classic combinatorial code (multiset permutations
   and compositions of integers), and this can be expressed very
   clearly and easily in a functional language.  The same goes for the
   "toy algebra system" code in the earlier articles where we used a
   limited set of algebraic structures to help understand the
   Cooley-Tukey decomposition of the Fourier matrix.  And although
   these things are quick and easy to write, they can then be carried
   forward into more efficient representations.

Another lesson, not confined to Haskell or functional programming:
good performance comes first from good algorithms, not from tweaking
your code.  There's no point in optimising an $O(N^2)$ algorithm if
it's known that there's an $O(N \log N)$ algorithm out there.  If you
do that, you're just generating waste heat in your CPU.

A related point is that good performance quite often requires you to
look beyond "textbook" algorithms.  I've yet to find a textbook
reference to Rader's algorithm or any of the other prime-length FFT
algorithms.  I don't know what the more recent editions of *Numerical
Recipes* say, but my copy of the second edition, in reference to using
the fast Fourier transform for anything other than power-of-two
lengths, basically just says "Don't do that."  If you want fast, you
need to look around a bit, read some literature, and implement good
algorithms.  Code tweaking comes later!

## Remaining tasks

These are rated from 1-5 for **D**ifficulty, **E**ffort, **V**alue and
**F**un:

#### Haskell `genfft` for "bottomlets" (**D**4/**E**4/**V**5/**F**4)

Writing a Haskell version of FFTW's `genfft` utility for producing the
optimised straight-line "codelets" used for the base transforms would
be interesting, and it would remove all the embarrassing things in
`Numeric.FFT.Special`, which are copied more or less wholesale from
FFTW[^1].  And once there's a Haskell `genfft`, it should be possible
to generate truly optimal plans at compile-time by running the
`genfft` code as Template Haskell.

#### Haskell `genfft` for "twiddlets" (**D**4/**E**3/**V**5/**F**3)

I've only written specialised straight-line code for what I've been
calling "bottomlets", i.e. the base transforms used at the bottom of
the recursive decomposition of the input vector.  The intermediate
Danielson-Lanczos steps all use generic unspecialised code.  It ought
to be possible to produce specialised versions of the
Danielson-Lanczos steps for specific sub-vector lengths, which might
give good speed-ups for some input lengths.  Once there's a `genfft`
for "bottomlets", it ought to be relatively straightforward to extend
that to "twiddlets" as well.

#### Truly optimal planning (**D**3/**E**3/**V**3/**F**3)

Having a Haskell `genfft` for both "bottomlets" and "twiddlets" opens
up the possibility of doing what I'd call *truly optimal planning*, in
the sense that, as well as testing different plans as is done now, it
would also be possible to construct specialised straight-line codelets
for custom input sizes.  Need a custom FFT of length 437?  That's $19
\times 23$, so we could construct (at compile-time) bottomlets and
twiddlets of sizes 19 and 23 and benchmark to see which ordering is
quicker, avoiding the use of Rader's algorithm for most prime factors.
Similarly, for more composite input lengths, we could try various
combinations of custom bottomlet and twiddlet sizes, instead of
relying only on a pre-selected list of sizes for specialisation.

#### Low-level optimisation (**D**4/**E**2/**V**5/**F**2)

I've really done relatively little optimisation on the `arb-fft` code:
using unboxed mutable vectors is about as far as it goes.  The
empirical plan generation also helps quite a bit, but there is
definitely more that could be done in terms of lower-level
optimisation.  I've not even looked at strictness, and haven't even
bothered with more profiling after optimisation, so there are probably
a few things that could be done to squeeze some more performance out.
Low-level optimisation of Haskell isn't really something that I know a
whole lot about, so I've left it to one side for now.

(There's another optimisation task that really ought to be done too:
for large problem sizes, planning can take a *long* time, particularly
for problem sizes with large prime factors.  I've added some code to
filter out ridiculous plans where possible, i.e. ones that are
guaranteed to be slow because of huge Danielson-Lanczos steps, but
it's not always possible to avoid looking at all of these, and they're
unavoidably slow.  Better heuristics for planning and switching to a
simplified timing approach instead of using Criterion would speed this
up a lot.)

#### Real-to-real, etc. FFTs (**D**3/**E**4/**V**4/**F**0)

So far, I've only implemented complex-to-complex FFTs.  For a
production library, I'd also need to implement real-to-complex
transforms along with the various flavours of real-to-real transforms
(cosine, sine and Hartley).  None of these things are terribly
difficult to do, but they are really pretty tedious exercises in data
management.  No fun at all!

#### Parallelisation (**D**3/**E**2/**V**3/**F**2)

There are a lot of cheap opportunities for parallelisation in the
divide-and-conquer approach of the Cooley-Tukey algorithm, and this is
a place where it might be quite easy to get performance that exceeds
that of FFTW (by cheating, of course &mdash; getting single processor
performance that's competitive with FFTW would take some doing!).

<br>

Of these tasks, I'll probably have a go at the Haskell `genfft` stuff
at some point and I might have a bit of a poke at some more
optimisation efforts.  I don't think I'll do the real-to-real FFTs
unless someone finds themselves in desperate need of a pure Haskell
FFT library with real-to-real transforms and offers to pay me for it!
Parallelisation probably isn't worth the effort unless
single-processor performance can be made more competitive with FFTW.

Now though, it's time for a break from FFT endeavours.  Although
today, I just learnt about
[this very interesting looking paper...][ffts]


[index]: /haskell-fft-index.html
[quickcheck]: http://hackage.haskell.org/package/QuickCheck
[criterion]: http://hackage.haskell.org/package/criterion
[ffts]: http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=6557535

[^1]: Extra embarrassing because I made a mistake in translating the
      biggest and most complicated of these things to Haskell and it
      took *ages* to track down the (one-character) typo.  This was in
      a single function with around 800 local variables, all called
      things like `t1Y` or `t2t` or `t2T`.  A Haskell `genfft` would
      be *nice*.
