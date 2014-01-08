---
author: Ian
tags: data-analysis,haskell
specials: angular(myApp,['radian'])
title: Haskell FFT 10: Building a package
published: 2014-01-04 20:36:00
published: 2014-01-07 20:06:57
---

So far in this series, we've been looking at code piecemeal, writing
little test modules to explore the algorithms we've been developing.
Before we start trying to optimise this code, it makes sense to put it
into a Cabal package with a more organised module structure, to
provide a sensible API and to make a few other small changes.

From now on, the code we're going to be talking about will be the
`arb-fft` package, which is
[hosted on GitHub](https://github.com/ian-ross/arb-fft).  In this
article, we'll be talking about the version of the code tagged
`pre-release-1`.  (We'll be uploading a version of the code to Hackage
once it's ready, but there will be a few pre-release versions that
we'll look at before we get to that point.)

<!--MORE-->

## "Cabalisation" and module structure

Setting up a new package with Cabal, Haskell's package and build
manager, is pretty easy: go to an empty directory, type `cabal init`
and answer some questions about your project -- the project name, a
one-line synopsis of what it does, author details, license details and
a couple of other things.  Cabal generates an initial Cabal package
description file to which you add the details of libraries,
executables, test suites and so on.  Here's the Cabal file for the
`arb-fft` package.

~~~~
name:               arb-fft
version:            0.1.0.0
synopsis:           Pure Haskell arbitrary length FFT library
homepage:           https://github.com/ian-ross/arb-fft
license:            GPL-3
license-file:       LICENSE
author:             Ian Ross
maintainer:         ian@skybluetrades.net
copyright:          Copyright (2013) Ian Ross
category:           Math
build-type:         Simple
extra-source-files: README.md
cabal-version:      >=1.10

Library
  exposed-modules:  Numeric.FFT
  other-modules:    Numeric.FFT.Plan
                    Numeric.FFT.Execute
                    Numeric.FFT.Special
                    Numeric.FFT.Types
                    Numeric.FFT.Utils
  ghc-options:      -O2
  build-depends:    base                        >= 4.6      && < 5,
                    containers                  >= 0.5.0.0  && < 0.6,
                    vector                      >= 0.10.9.1 && < 0.11
  default-language: Haskell2010

Test-Suite basic-test
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          basic-test.hs
  build-depends:    arb-fft,
                    base                        >= 4.6      && < 5,
                    containers                  >= 0.5.0.0  && < 0.6,
                    vector                      >= 0.10.9.1 && < 0.11,
                    QuickCheck                  >= 2.6      && < 2.7,
                    tasty                       >= 0.3,
                    tasty-quickcheck            >= 0.3
  default-language: Haskell2010
~~~~

We define a single library and one test suite.  The modules in the
library are:

 * `Numeric.FFT`: The main "exposed" module implementing the public
    API.  Exports functions for performing FFT and inverse FFT
    transforms, along with re-exporting some data types and additional
    API from a couple of the hidden modules.
 * `Numeric.FFT.Plan: Functions for pre-planning of FFT calculations.
 * `Numeric.FFT.Execute`: Functions to execute pre-planned FFTs.
 * `Numeric.FFT.Types`: Type definitions, the most important of which
   are `Plan` and `BaseTransform`.
 * `Numeric.FFT.Utils`: Utility functions, including prime
    factorisation, primitive root calculation for
    $\mathbb{Z}_p^{\times}$ and some other small things.
 * `Numeric.FFT.Special`: Base transform implementations specialised
   to small fixed input lengths.

## Code changes for packaging

#### Pre-planning

The `plan` function in the `Numeric.FFT.Plan` module (also re-exported
from `Numeric.FFT`) takes a problem size as input and returns a value
of the `Plan` abstract data type that includes all of the information
required to execute an FFT that can be calculated in advance: input
vector permutation, size and factor information for the
Danielson-Lanczos steps used to build up the final results, powers of
$\omega_N$ for all the $N$ values required, and a "base transform"
used at the "bottom" of the Cooley-Tukey decomposition, which is
either a reference to a specialised hard-coded transform for small
input lengths (defined in `Numeric.FFT.Special`), or (for prime
lengths) all the information needed to perform a prime-length
transform using Rader's algorithm, or (for any other lengths) an
indicator that a simple DFT should be used for the base transform.

At the current stage of development of the code, this pre-planning is
relatively lightweight, since we always use the same prime factor
decomposition of the input size.  Later on, we'll add some
benchmarking code to the planner to select the best decomposition of
the input size from a list of likely good plans.

The plans generated by the `plan` function are executed by code in the
`Numeric.FFT.Execute` module.  This is where most of the work is done,
and is where we'll be concentrating our optimisation effort.  As well
as the main Cooley-Tukey driver function (`execute`) and a function to
perform a single Danielson-Lanczos step, this module also has code to
apply the relevant "base transforms".

#### Special base transforms

Up to now, we've been using a simple DFT for the small prime-length
factors at the "bottom" of the FFT decomposition.  For arbitrary prime
lengths, we can use Rader's algorithm, but this will be much less
efficient than a "straight-line" hand-coded transform.  The idea here
is that for small input lengths that occur at the "bottom" of many
transform decompositions, we can write optimal or near-optimal
hand-coded transforms once and for all.

Here such a specialised base transform for input vectors of length 5:

~~~~ {.haskell}
-- | Length 5 hard-coded FFT.
kp951056516, kp559016994, kp250000000, kp618033988 :: Double
kp951056516 = 0.951056516295153572116439333379382143405698634
kp559016994 = 0.559016994374947424102293417182819058860154590
kp250000000 = 0.250000000000000000000000000000000000000000000
kp618033988 = 0.618033988749894848204586834365638117720309180
special5 :: Int -> VCD -> VCD
special5 sign xs =
  let ar:+ai=xs!0 ; br:+bi=xs!1 ; cr:+ci=xs!2 ; dr:+di=xs!3 ; er:+ei=xs!4
      ts = br - er ; t4 = br + er ; tt = cr - dr ; t7 = cr + dr
      t8 = t4 + t7 ; ta = t4 - t7 ; te = bi - ei ; tm = bi + ei
      tn = ci + di ; th = ci - di ; to = tm + tn ; tq = tm - tn
      ti = te + kp618033988 * th ; tk = th - kp618033988 * te
      t9 = ar - kp250000000 * t8 ; tu = ts + kp618033988 * tt
      tw = tt - kp618033988 * ts ; tp = ai - kp250000000 * to
      tb = t9 + kp559016994 * ta ; tj = t9 - kp559016994 * ta
      tr = tp + kp559016994 * tq ; tv = tp - kp559016994 * tq
      r4 = (tb + kp951056516 * ti) :+ (tr - kp951056516 * tu)
      r3 = (tj - kp951056516 * tk) :+ (tv + kp951056516 * tw)
      r2 = (tj + kp951056516 * tk) :+ (tv - kp951056516 * tw)
      r1 = (tb - kp951056516 * ti) :+ (tr + kp951056516 * tu)
  in generate 5 $ \i -> case i of
    0 -> (ar + t8) :+ (ai + to)
    1 -> if sign == 1 then r1 else r4
    2 -> if sign == 1 then r2 else r3
    3 -> if sign == 1 then r3 else r2
    4 -> if sign == 1 then r4 else r1
~~~~

This code (which is actually a bit of a cheat, as I'll explain)
demonstrates the principal problem with these hand-coded transforms:
they are very tedious to write, and doing them properly is very
error-prone.  The code above is adapted from equivalent code in FFTW
(translated from C to Haskell), and I still made a couple of confusing
errors with the translation that took some time to track down.

The right way to do this (and the way that FFTW does it) is to
*generate* the code for these small transforms.  FTTW comes with an
OCaml program called `genfft` that generates C "codelets" for fixed
transform sizes.  It does this the way it should be done, which is to
build a directed acyclic graph of the expressions relating the FFT
output vector elements to the input vector elements, then recursively
partitioning the expression DAG to find an optimal schedule of
sub-expressions to calculate, exploiting sharing in the expression
DAG.  It performs a range of other optimisations and produces code
with information about variable lifetimes to help a C compiler
allocate variables to registers.  It's quite a complicated piece of
code and while it would be quite possible to implement something
similar here in Haskell, it's not something I want to get into.

Instead, I'm just going to translate pre-computed codelets from FFTW
into Haskell as needed.  For the moment, there are just codelets for
sizes 2, 3 and 5.  I know that this is a bit of a cop-out!


## Benchmarking of full FFT with new prime length algorithm

We can benchmark the `pre-release-1` version of our code as we did
[before][benchmark-1].  The only real difference in the benchmarking
code is that we now split out the FFT pre-planning and execution
steps, just as we do for the FFTW comparison:

~~~~ {.haskell}
doit :: Environment
        -- ^ Criterion timing environment.
     -> Int
        -- ^ Problem size to benchmark.
     -> Criterion (Int, Double, Double)
        -- ^ (Problem size, DFT, my FFT, FFTW) result.
doit env sz = do
  let v = tstvec sz
  let myplan = FFT.plan sz
  myffts <- runBenchmark env $ nf (FFT.fftWith myplan) v
  let fftwPlan = FFTW.plan FFTW.dft sz
  fftws <- runBenchmark env $ nf (FFTW.execute fftwPlan) v
  let mean xs = VU.sum xs / fromIntegral (VU.length xs)
  return (sz, mean myffts, mean fftws)
~~~~

Here are some results (compare with the plot [here][benchmark-1]):

<plot-data name="dat" format="csv" separator=" "
           src="/blog/posts/2014/01/04/data-analysis-fft-10/benchmark-2.dat">
</plot-data>

<plot width=800 aspect=1.6 font-size=16 range-x="8,1100"
      axis-x-label="Input length" axis-y-label="Time"
      axis-x-transform="log" ui-axis-x-transform
      axis-y-transform="log" ui-axis-y-transform
      axis-x-ticks="[[[[8,'8'],[16,'16'],[32,'32'],[64,'64'],[128,'128'],[256,'256'],[512,'512'],[1024,'1024']]]]"
      axis-y-ticks="[[[[1,'1&thinsp;&mu;s'],[10,'10&thinsp;&mu;s'],[100,'100&thinsp;&mu;s'],[1000,'1&thinsp;ms'],[10000,'10&thinsp;ms']]]]"
      axis-x-end-tick-size=0 oxs="[[seqStep(8,1024,4)]]">
  <legend order="FFT|FFTW|O(N log N)">
  </legend>
  <plot-options stroke-width=1.5 stroke="black">
    <lines label="O(N log N)" x="[[oxs]]" y="[[x*log(x)]]"></lines>
    <lines x="[[oxs]]" y="[[0.01*x*log(x)]]"></lines>
  </plot-options>
  <plot-options stroke-width=2>
    <lines label="FFT" stroke="green" x="[[dat.Size]]" y="[[dat.FFT]]"></lines>
    <lines label="FFTW" stroke="blue" x="[[dat.Size]]" y="[[dat.FFTW]]"></lines>
  <plot-options>
</plot>

Here, we show only results from our FFT code and FFTW, along with a
couple of $O(N \log N)$ scaling lines.  There are four things to
observe in this plot:

1. The scaling behaviour of our FFT implementation has changed:
   instead of the upper bound to execution times scaling as $O(N^2)$
   seen in [the earlier benchmarking plot][benchmark-1], we now have
   something like $O(N \log N)$ scaling for all input vector lengths.
   This is due to replacing simple DFTs for prime lengths with
   applications of Rader's algorithm.

2. Because of the use of the next highest power of two size for the
   convolution in our implementation of Rader's algorithm, the upper
   range of execution times display a series of plateaus -- these all
   result from prime input sizes, where we need to perform a forward
   FFT and an inverse FFT both of a size given by the smallest power
   of two greater or equal than the input vector length (we only need
   to do one forward FFT because we pre-compute the FFT of the padded
   $b_q$ series in the Rader algorithm as part of the FFT pre-planning
   phase).

3. Although the overall scaling of the new code is $O(N \log N)$ as
   opposed to the $O(N^2)$ scaling of the original code without
   Rader's algorithm, it is often the case that the new code is
   slightly *slower* than the old code for a given input vector size.
   The main reason for this is that, for the base transforms for a
   given input vector length, we now use either a specialised
   hard-coded transform (which I've so far only implemented for vector
   lengths 2, 3 and 5), or we use Rader's algorithm.  The prior
   version of the code used a simple DFT for the base transforms in
   all cases.  Although the asymptotic scaling of Rader's algorithm is
   better than the simple DFT ($O(N \log N)$ compared to $O(N^2)$),
   Rader's algorithm is more complex and has comparatively
   unfavourable constant factors compared to the simple DFT.  The
   result is that the simple DFT can be faster for short vector
   lengths making the overall FFT execution times for the code using
   Rader's algorithm slower.  (Rader's algorithm *is* good for larger
   prime input vector lengths.)  There are two things we can do to fix
   this.  First, we can implement more hard-coded straight-line base
   transforms.  As described above, this is kind of a hassle, but we
   can cheat by translating FFTW's codelets into Haskell.  Second, we
   can use empirical measurements of FFT execution times to decide on
   whether to use Rader's algorithm or the simple DFT for small
   factors.  We'll explore both of these issues in the next article
   when we look at various optimisation approaches.

4. There's still a lot of variability in the performance of our
   algorithm for different input vector sizes, particularly compared
   to the range of variability in performance for FFTW.  Although both
   the bottom and top range of execution times appear to scale as $O(N
   \log N)$, there's quite a wide band of variability between those
   limits.  To some extent, this wide variability may just an artefact
   of the overall worse performance of our code, so we'll come back
   and explore it later.

So, preamble over, we'll get to optimisation in the next article!

[benchmark-1]: /blog/posts/2013/12/07/data-analysis-fft-8/index.html