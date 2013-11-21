---
author: Ian
tags: data-analysis,haskell
title: Haskell FFT 1: The Discrete Fourier Transform
published: 2013-11-11 20:57:04
published: 2013-11-13 21:00:07
---

This is the first of a series of posts about implementing the Fast
Fourier Transform in Haskell (and trying to make it go *fast*).  This
part is going to be pretty pedestrian, but we need to lay some
groundwork before the interesting stuff.

Fourier transforms turn up everywhere.  They're important in pure
mathematics, but they're also used in pretty much any application that
deals with time series: filtering, signal analysis, numerical solution
of differential equations and many others.  While the classical theory
of Fourier analysis is based on functions, in applications we most
often deal with discrete series of values sampled from a function.
The *discrete Fourier transform* is how we do Fourier analysis in this
setting.

In this series of posts, we're not going to look very much at
applications (well, maybe we'll have one little exception a bit later)
and we're not going to talk a lot about the theory behind the Fourier
transform.  Instead, after briefly introducing the discrete Fourier
transform, we're going to use the *fast Fourier transform* algorithm
and some variations to explore some aspects of Haskell programming.
In particular, we'll look at how Haskell deals with complex numbers,
vectors, profiling and benchmarking, meta-programming and finally, as
our *pièce de résistance*, we'll use all of this to build a Haskell
fast Fourier transform package that does compile-time empirical
optimisation for arbitrary-sized transforms.

In practice, if you want to do Fourier transforms of discrete data,
you use something called FFTW ("the Fastest Fourier Transform in the
West").  Our code has no chance of competing with FFTW, which is
incredibly clever[^1], but we can demonstrate some of the methods that
they use and learn some techniques along the way that are applicable
to more complicated problems.  (There are Haskell bindings for FFTW --
the [`vector-fftw`](http://hackage.haskell.org/package/vector-fftw)
package is a good choice.)

<!--MORE-->

## The Discrete Fourier Transform

According to the
[Fourier theorem](https://en.wikipedia.org/wiki/Fourier_series), any
piecewise-continuous complex-valued function of a real variable,
$h(t)$, periodic on the interval $[0, T]$, can be written as a Fourier
series

$$h(t) = \sum_{n=-\infty}^{\infty} a_n e^{2\pi i n t / T}$$

where the orthogonality of the functions $\exp (2\pi i n t / T), n \in
\mathbb{Z}$ on the interval $[0, T]$ allows us to calculate the
complex-valued Fourier coefficients $a_n$ as

$$a_n = \frac{1}{T} \int_0^T h(t) e^{-2\pi i n t / T} \, dt.$$

In data analysis applications, we most commonly don't have
continuous functions, but discretely *sampled* data.  Suppose
that we have $N$ data values from a function $h(t)$ sampled at a
regular interval $\Delta$:

$$h_n = h(n \Delta) \qquad n = 0, 1, 2, \dots, N-1.$$

The discrete Fourier transform of this set of data values is defined
as

$$H_n = \sum_{k=0}^{N-1} h_k e^{2\pi i k n/N} \qquad n = 0, 1, 2,
\dots, N-1. \qquad (*)$$

There are a few things to note here.  First, like the classical
Fourier transform, the discrete Fourier transform is a *linear*
operation.  That will prove to be important!  Second, we have $N$
input values and $N$ output values, which is as many independent
values as we can generate from $N$ inputs with a linear operation.

If we think of our input data as sampling a continuous function $h(t)$
at $N$ discrete times, $n\Delta$ with $n = 0, 1, 2, \dots, N-1$, then
we can think of the discrete Fourier transform as sampling the
"full" Fourier transform at a finite set of frequencies

$$f_n = n/(N\Delta) \qquad n = -N/2, \dots, N/2.$$

Because we our sampling at an interval $\Delta$, we cannot represent
any functions containing frequency components above the *Nyquist
frequency*

$$f_c = \frac{1}{2\Delta},$$

which is just the frequency at the limits of the frequency range given
above.  (There are $N+1$ values listed in the range above because the
Fourier components for $\pm f_c$ are identical.)

From the $H_n$, we can recover the original samples $h_k$ using the
inverse discrete Fourier transform

$$h_k = \frac{1}{N} \sum_{n=0}^{N-1} H_n e^{-2\pi i k n/N} \qquad n =
0, 1, 2,$$

which is identical to the forward transform except for the sign in the
complex exponential and the factor of $1/N$.  We'll get a better
understanding of this in the next article when we think about the DFT
as a matrix multiplying our data vector.

## A simple implementation

The transform in $(*)$ is easy to implement directly in Haskell.
Here's a very naïve and inefficient implementation (we're going to be
spending a lot of time thinking about optimisation and efficiency
later, so let's not worry about efficiency right now):

~~~~ {.haskell}
import Prelude hiding (length, sum, map, zipWith, (++))
import Data.Complex
import Data.Vector

i :: Complex Double
i = 0 :+ 1

omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)

dft, idft :: Vector (Complex Double) -> Vector (Complex Double)
dft = dft' 1 1
idft v = dft' (-1) (1.0 / (fromIntegral $ length v)) v

dft' :: Int -> Double -> Vector (Complex Double) -> Vector (Complex Double)
dft' sign scale h = generate bigN (((scale :+ 0) *) . doone)
  where bigN = length h
        w = omega bigN
        doone n = sum $
                  zipWith (*) h $ generate bigN (\k -> w^^(sign*n*k))

defuzz :: Vector (Complex Double) -> Vector (Complex Double)
defuzz = map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x
~~~~

We're going to be manipulating complex numbers throughout this set of
articles, so we import the `Data.Complex` module, part of the Haskell
2010 standard, to make this easy.  The `Complex` data type is
parameterised over the type of the real and imaginary parts of the
number and has a constructor `:+` to create a complex number from its
real and imaginary parts.  `Data.Complex` provides instances of all
the expected numeric classes so that you can do arithmetic with
complex values just as for other numeric values.  Definitions are also
provided for transcendental functions ($\sin$, $\cos$, $\log$, etc.),
and the standard explicitly mandates definitions of these so you can
see how they deal with branch cuts and other issues.  Finally,
`Data.Complex` provides some convenience functions for working with
complex numbers, in particular for converting between Cartesian and
polar representations.  It also provides the `cis` function:
$\mathtt{cis}\, \theta = \cos \theta + i \sin \theta$.

We're also going to be dealing with vectors of numbers a lot.  For
that, we use the `vector` package, which provides arrays indexed by
`Int`s with $O(1)$ indexing and powerful fusion rules for loop
optimisation.  It also has vector equivalents of a lot of the standard
Prelude list functions (`length`, `take`, `drop`, `map`, `zipWith`,
etc.).  To avoid name clashes, we'll mostly hide the Prelude versions
of these functions, although you can also use a qualified import of
`Data.Vector` if you prefer.

Take a look at the type of `dft` in the listing above.  It both takes
and returns a `Vector (Complex Double)`.  Throughout this series of
posts, we're only going to be dealing with complex-to-complex DFTs and
we're going to do everything in double precision.  In
production-quality library code, we would parameterise over the
floating point type that we use, and we would also implement the full
range of real-to-complex and real-to-real versions of the DFT.  The
data management issues involved in that aren't too interesting for us
here though, so we'll stick with complex-to-complex transforms.

The algorithm used above is basically just a
direct transcription of the sum in $(*)$, using

~~~ {.haskell}
Data.Vector.generate :: Int -> (Int -> a) -> Vector a
~~~~

to calculate vector entries for each integer vector index.  It's very
inefficient, not only because it does an $O(N^2)$ matrix
multiplication (which the FFT aims to fix!), but also because of
repeated computation of the complex exponential factors.  Anyway,
efficiency doesn't matter here: we just want a simple and clear
implementation to compare our more complicated FFT implementations to
later on.  We use the `dft'` function as a helper to allow us to
implement the forward (`dft`) and inverse (`idft`) transforms using
the same code.

We can do a couple of quick tests to see if it works.  In GHCi (the
`defuzz` function just sets any small components of numbers to zero
for display):

~~~~
> :load DFT.hs
[1 of 1] Compiling DFT              ( DFT.hs, interpreted )
Ok, modules loaded: DFT.
> dft $ fromList [1,0,0,0,0,0,0,0]
fromList [1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,
  1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0]
> defuzz $ idft $ dft $ fromList [1,0,0,0]
fromList [1.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0]
> defuzz $ dft $ generate 8 (\i ->
    sin (2 * pi * fromIntegral i / 8))
fromList [0.0 :+ 0.0,0.0 :+ 4.0,0.0 :+ 0.0,0.0 :+ 0.0,
  0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ (-3.9999999999999987)]
~~~~

First, we look at the DFT of a single "spike", which corresponds
(more or less) to a Dirac $\delta$-function in classical Fourier
analysis.  The Fourier transform of a $\delta$-function is a constant,
and we see that the result of the DFT is constant for our spike, as we
would hope.  Applying the inverse DFT to the transform gets us back
our original data (although as a vector of `Complex Double`).  Next,
we look at a sine curve.  Recall that

$$\sin (n\theta) = \frac{e^{in\theta} - e^{-in\theta}}{2i} =
-\frac{i}{2} e^{in\theta} + \frac{i}{2} e^{-in\theta}$$

so, for a complex Fourier transform, we would expect to see a negative
imaginary component of magnitude $\frac12$ at a positive frequency $n$
and a positive imaginary component of magnitude $\frac12$ at negative
frequency $n$.  Here, $n = 1$ (we're fitting the biggest whole sine
curve into our data array that we can).  Because of the data layout
conventionally used in the DFT, that means that our positive frequency
component is at index 1 of the output array, and the negative
frequency component is at index 7.  The magnitudes of the Fourier
components are scaled with respect to those of classical Fourier
analysis by a factor of $N$, the size of the input array, so we expect
our entries to have magnitude 4.  This is indeed what we see, except
that the signs appear to be wrong -- but remember that the sign in the
complex exponential in the inverse DFT is the opposite of the forward
transform, and it's the inverse transform that we'd be plugging these
values into.  So all is well, and the code above appears to work.

## What next?

So, that wasn't very exciting, but it sets the scene for what comes
next, which is more fun.  In the next article, we'll examine how the
fast Fourier transform algorithm (specifically, the *Cooley-Tukey
decimation-in-time* algorithm) allows us to replace the $O(N^2)$ DFT
calculation with one that scales as $O(N \log N)$ for vectors whose
lengths are powers of two.  We'll use a clever matrix decomposition to
represent the key step in the FFT algorithm, and we'll write some "toy
algebra" code that will enable us to experiment, check some
assumptions and generally get a better idea of just what's going on.


[^1]: Matteo Frigo and Steven G. Johnson, "The Design and
      Implementation of FFTW3," *Proceedings of the IEEE* **93** (2),
      216-231 (2005). Invited paper, Special Issue on Program
      Generation, Optimization, and Platform Adaptation.
      [PDF here](http://www.fftw.org/fftw-paper-ieee.pdf)
