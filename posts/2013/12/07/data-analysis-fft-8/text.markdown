---
author: Ian
tags: data-analysis,haskell
specials: angular(myApp,['radian'])
title: Haskell FFT 8: Benchmarking Experiments
published: 2013-12-03 12:47:23
published: 2013-12-07 14:19:39
---

We're going to do a number of different things to optimise the code
developed in the previous section, but before we do that, we need to
do some baseline performance measurements, so that we know that our
"improvements" really are improving things.  The performance results
we see will also help to guide us a little in the optimisations that
we need to do.

Benchmarking is a fundamental activity when working with numerical
code, but it can be quite difficult to do it correctly in a lazy
language like Haskell.  It's very easy to get into a situation where a
benchmarking loop (perhaps to run a computation 1000 times to collect
accurate timing information) performs the computation on the first
time round the loop, then reuses the computed result for all the other
999 times round.

We can bypass these worries quite effectively using Bryan O'Sullivan's
Criterion library.  As well as providing a clean framework for
ensuring that pure calculations really do get rerun for timing
purposes, Criterion gives us a nice API for setting up benchmarks,
running them and collecting results.

Here's an example of the most basic use of Criterion:

~~~~ {.haskell}
module Main where

import Criterion.Main
import Data.Complex
import Data.Vector
import qualified DFT
import qualified FFT
import qualified Numeric.FFT.Vector.Invertible as FFTW

main :: IO ()
main = do
  let v = generate 256 (\i -> realToFrac $ sin (2*pi*fromIntegral i / 256)) ::
        Vector (Complex Double)
      fftwPlan = FFTW.plan FFTW.dft 256
  defaultMain [ bench "DFT" $ nf DFT.dft v
              , bench "FFT" $ nf FFT.fft v
              , bench "FFTW" $ nf (FFTW.execute fftwPlan) v ]
~~~~

Here, we're comparing the performance for three FFT implementations on
a single input vector length.  The `defaultMain` function from
Criterion runs a set of benchmarks one after another after collecting
some information about the system clock for the machine you're using.
The `bench` function is a simple way of defining a benchmark for a
pure function -- you give a name for the benchmark and a function to
call.  The tricky bit here is the use of the `nf` function.  This
takes two arguments, a function applied to all but its last argument,
and the last argument, and it ensures that the result of applying the
function to the argument is evaluated to normal form (which basically
means it gets evaluated as far as possible, so for our FFT algorithms,
we perform the whole of the algorithm to get a final result).  Note
that for the FFTW library, we perform the "plan creation" step outside
the benchmark so that we only measure the time taken by the FFT
computation itself.  Criterion also has facilities for benchmarking
monadic code, but we won't be using those here.

If we compile and run this code, we get output that looks something
like this (the numbers and probably also some of the messages will be
different on another machine):

~~~~
[seneca:benchmark-demo] $ ./benchmark-demo
warming up
estimating clock resolution...
mean is 1.170747 us (640001 iterations)
found 96871 outliers among 639999 samples (15.1%)
  4 (6.3e-4%) low severe
  96867 (15.1%) high severe
estimating cost of a clock call...
mean is 31.18633 ns (12 iterations)
found 2 outliers among 12 samples (16.7%)
  2 (16.7%) high mild

benchmarking DFT
mean: 21.87155 ms, lb 21.85268 ms, ub 21.90904 ms, ci 0.950
std dev: 131.1261 us, lb 76.22067 us, ub 212.3907 us, ci 0.950

benchmarking FFT
mean: 878.7273 us, lb 876.9702 us, ub 884.2682 us, ci 0.950
std dev: 14.47592 us, lb 5.361621 us, ub 32.03430 us, ci 0.950
found 12 outliers among 100 samples (12.0%)
  5 (5.0%) high mild
  7 (7.0%) high severe
variance introduced by outliers: 9.424%
variance is slightly inflated by outliers

benchmarking FFTW
mean: 6.682681 us, lb 6.582545 us, ub 6.768512 us, ci 0.950
std dev: 470.4364 ns, lb 421.4242 ns, ub 508.7335 ns, ci 0.950
variance introduced by outliers: 64.627%
variance is severely inflated by outliers
~~~~

Before running any benchmarks, Criterion measures the timing
resolution of the system clock, printing some statistical information.
It then runs enough repetitions of each of the benchmarks to give a
good estimate of the mean time consumed by the computation.  The
`defaultMain` function causes some statistics to be printed for each
of the benchmarks.

What we really want to do is to collect timing information for each of
the three algorithms for a range of input vector lengths.  Here's how
we do this:

~~~~ {.haskell}
module Main where

import Prelude hiding (enumFromTo, length, sum)
import Control.Monad
import Control.Monad.IO.Class
import Criterion
import Criterion.Config
import Criterion.Monad
import Criterion.Environment
import Data.Complex
import Data.Vector hiding (forM, forM_, map, (++))
import qualified Data.Vector.Unboxed as VU
import qualified DFT
import qualified FFT
import qualified Numeric.FFT.Vector.Invertible as FFTW
import System.IO

tstvec :: Int -> Vector (Complex Double)
tstvec sz = generate sz (\i -> let ii = fromIntegral i
                               in sin (2*pi*ii/1024) + sin (2*pi*ii/511))

doit :: Environment
        -- ^ Criterion timing environment.
     -> Int
        -- ^ Problem size to benchmark.
     -> Criterion (Int, Double, Double, Double)
        -- ^ (Problem size, DFT, my FFT, FFTW) result.
doit env sz = do
  let v = tstvec sz
  dfts <- runBenchmark env $ nf DFT.dft v
  myffts <- runBenchmark env $ nf FFT.fft v
  let fftwPlan = FFTW.plan FFTW.dft sz
  fftws <- runBenchmark env $ nf (FFTW.execute fftwPlan) v
  let mean xs = VU.sum xs / fromIntegral (VU.length xs)
  return (sz, mean dfts, mean myffts, mean fftws)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Size DFT FFT FFTW"
  withConfig (defaultConfig { cfgVerbosity = ljust Quiet }) $ do
    env <- measureEnvironment
    forM_ [8..1024] $ \sz -> do
      (_, dft, fft, fftw) <- doit env sz
      liftIO $ putStrLn $ show sz ++
        " " ++ show (1.0E6 * dft) ++
        " " ++ show (1.0E6 * fft) ++
        " " ++ show (1.0E6 * fftw)
~~~~

Here, we're not using Criterion's `defaultMain`, but instead we're
setting some configuration options and using the `withConfig` function
to run some Criterion actions.  Criterion has a monadic interface
(with a monad called, naturally enough, `Criterion`) that allows us to
sequence benchmarking actions ourselves.  Before running any
benchmarks, we use the `measureEnvironment` function to measure the
system clock resolution.  This information is encapsulated in a value
of type `Environment` that we need to pass into the benchmarking
functions.  We use the usual monadic sequencing operators to loop over
the range of input sizes we want to test and, because the `Criterion`
monad is an instance of `MonadIO`, we can output results from within
the `Criterion` code.

The benchmarks themselves are run by the function `doit`, which
creates a test vector then uses the `runBenchmark` function to measure
the performance of each of the three FFT algorithms in turn.  The
`runBenchmark` function takes a timing environment and a
`Benchmarkable` value -- pure functions are instances of
`Benchmarkable`, so we can pass a call to `nf` here, set up
appropriately to trigger execution of our FFT functions.  Criterion
runs each of our functions enough times to get a reasonable idea of
the distribution of the time the function takes to complete.  The
results of the benchmarks are returned as values of type `Sample`,
which is basically just an unboxed vector of double values, from which
we can extract statistics for plotting.  (For detailed comparison
later on, we'll calculate some uncertainty bounds on these
measurements as well, but for a first look, we're just taking the mean
of each sample.)

And here are some results:

<plot-data name="dat" format="csv" separator=" "
           src="/blog/posts/2013/12/07/data-analysis-fft-8/benchmark-1.dat">
</plot-data>

<plot width=800 aspect=1.6 font-size=16 range-x="8,1100"
      axis-x-label="Input length" axis-y-label="Time"
      axis-x-transform="log" ui-axis-x-transform
      axis-y-transform="log" ui-axis-y-transform
      axis-x-ticks="[[[[8,'8'],[16,'16'],[32,'32'],[64,'64'],[128,'128'],[256,'256'],[512,'512'],[1024,'1024']]]]"
      axis-y-ticks="[[[[1,'1&thinsp;&mu;s'],[10,'10&thinsp;&mu;s'],[100,'100&thinsp;&mu;s'],[1000,'1&thinsp;ms'],[10000,'10&thinsp;ms'],[100000,'100&thinsp;ms'],[1000000,'1&thinsp;s'],[10000000,'10&thinsp;s']]]]"
      axis-x-end-tick-size=0 oxs="[[seqStep(8,1024,4)]]">
  <legend order="DFT|FFT|FFTW|O(N&sup2;)|O(N log N)">
  </legend>
  <plot-options stroke-width=1.5 stroke="grey">
    <lines label="O(N&sup2;)" x="[[oxs]]" y="[[x**2]]"></lines>
    <lines x="[[oxs]]" y="[[0.1*x**2]]"></lines>
    <lines x="[[oxs]]" y="[[0.01*x**2]]"></lines>
  </plot-options>
  <plot-options stroke-width=1.5 stroke="black">
    <lines label="O(N log N)" x="[[oxs]]" y="[[x*log(x)]]"></lines>
    <lines x="[[oxs]]" y="[[0.01*x*log(x)]]"></lines>
  </plot-options>
  <plot-options stroke-width=2>
    <lines label="DFT" stroke="red" x="[[dat.Size]]" y="[[dat.DFT]]"></lines>
    <lines label="FFT" stroke="green" x="[[dat.Size]]" y="[[dat.FFT]]"></lines>
    <lines label="FFTW" stroke="blue" x="[[dat.Size]]" y="[[dat.FFTW]]"></lines>
  <plot-options>
</plot>

This plot shows results (as time for a single transform for a given
input vector length) for input vector lengths in the range
$[8, 1024]$.  Note that the plot is on a log-log scale.  The thin
black lines show $C N \log N$ (solid) and $C N^2$ (dotted) for a few
choices of constant $C$ to give some idea of the scaling of the
different results.  Let's look at the DFT results first (the red
line).  After a slightly faster than $O(N^2)$ rise for small values of
$N$, the scaling seems to settle down to something like an $O(N^2)$
increase with input vector length, which is what we expect.  Next, the
results for FFTW are show in blue.  The times here scale more or less
as $O(N \log N)$ (and perhaps even a little better than that!) with
relatively little variability.  It's definitely the case that some
input vector lengths do better and some worse in terms of scaling, but
there are no gross discrepancies in performance as $N$ varies.

The results for our mixed-radix Haskell FFT code are shown in green.
There are two main things we can draw from this:

1. The timing results for our FFT code give a sort of "wedge" shape.
   The bottom edge of the wedge seems to scale as $O(N \log N)$ while
   the top edge scales as $O(N^2)$, but there are results for
   different input vector lengths lying all over the space in between
   these two extremes.  The reason for this is fairly obvious: we fall
   back on the simple $O(N^2)$ DFT algorithm for the prime factors at
   the "bottom" of our Fourier matrix decomposition.  For prime input
   vector lengths, we just do a normal DFT, which gives us the
   $O(N^2)$ top edge of our wedge.  For cases where we have a "good"
   prime factorisation, i.e. the last prime factor is small, we get
   close to $O(N \log N)$ scaling, which gives us the bottom edge of
   the wedge.

2. Even in the best cases, our code is something like 100 times slower
   than FFTW.  That sounds bad, I know, but we're not really comparing
   like with like -- FFTW is probably one of the best optimised pieces
   of code in existence; our code is totally unoptimised.  Of course,
   I use GHC's `-O2` flag to enable optimisation in the compiler, but
   we've not made any effort to optimise the algorithm itself (apart
   from the basic Cooley-Tukey divide and conquer approach to the FFT)
   and, in particular, we've not made any effort to look at
   allocation, unnecessary data copying, inefficiencies due to
   laziness, and so on.

Most of the rest of this series of articles is going to be about how
we can optimise our code.  We're going to take an empirical
measurement-driven approach, making modifications to the code based on
some obvious first things to try, then on profiling information, and
measuring performance at each step along the way to make sure that
we're actually making things better.

What's a reasonable goal?  I don't yet know.  I'd be very happy if we
can get within a factor of ten of the performance of FFTW with a pure
Haskell implementation.

What should we do first?  It seems pretty obvious that those prime
factors are something we need to deal with.  No matter what other
kinds of optimisation we do, as long as we have $O(N^2)$ behaviour in
our code, we're not going to get very far.  There are a number of FFT
algorithms for prime-length vectors that have $O(N \log N)$ scaling
behaviour, and in the next article we'll implement one of these, after
spending a little bit of time thinking about how an efficient
prime-length FFT might work.

After that, we'll take a closer look at our code and do some profiling
to get an idea of what we should look at next.  Before doing any
profiling at all, I can say that there is too much allocation and
copying going on, and that we'll probably end up rewriting things to
use a mutable vector to do the Danielson-Lanczos steps in place, but
there are probably other things that we can do as well.
