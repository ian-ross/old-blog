---
author: Ian
tags: data-analysis,haskell
title: "Haskell FFT 7: Where We've Got To, Where We're Going"
published: 2013-12-03 13:35:29
---

Let's summarise what we've done so far, as well as pointing out some
properties of our general mixed-radix FFT algorithm that will be
important going forward.  We'll also think a little bit about just
what we are going to do next.  Here's what we've done:

1. Written down the basic expression for the DFT and implemented it
   directly in Haskell.
   [POST](/blog/posts/2013/11/13/data-analysis-fft-1.html)

2. Built some "toy algebra" tools to help us understand the Fourier
   matrix decomposition at the heart of the Cooley-Tukey FFT
   algorithm.  [POST](/blog/posts/2013/11/15/data-analysis-fft-2.html)

3. Implemented a basic powers-of-two FFT.
   [POST](/blog/posts/2013/11/18/data-analysis-fft-3.html)

4. Extended the "toy algebra" code to the case of general input vector
   lengths.  [POST](/blog/posts/2013/11/23/data-analysis-fft-5.html)

5. Implemented a full mixed-radix FFT in Haskell for general input
   vector lengths.
   [POST](/blog/posts/2013/11/27/data-analysis-fft-6.html)


We say "**a** full mixed-radix FFT" because there are lots of choices
available for implementing this calculation, and we've chosen just one
representative approach.  There are two properties of our
implementation that are important for what we're going to do next:

 * We're using the basic naïve DFT for the final prime-length
   transformations at the "bottom" of our input vector decomposition.
   One thing we can do to improve the efficiency of our overall FFT
   algorithm is to explore better ways of dealing with these
   prime-length vectors that rely on the special algebraic properties
   of the Fourier matrix $F_p$ for prime $p$.

 * Our implementation of the mixed-radix FFT algorithm uses a "sorted
   prime factor" decomposition of the input vector length to decide
   how to break down the input vector for transformation.  However,
   nowhere in any part of the algorithm is it required that the
   factors be sorted, or be prime -- none of the elements of the
   algorithm (permutation calculations, sub-factor Fourier matrices,
   and generalised Danielson-Lanczos step) are dependent on factor
   ordering or primality.

This second point is the one that's really critical going forward: for
a given length of input vector, we can choose any order of factors of
the length to drive the Fourier matrix decomposition.  For example, if
we have a 1024-element input vector, we could use the "standard"
powers-of-two decomposition, splitting the input hierarchically ten
times to get to single-element subvectors; or, we could split the
input into 32 vectors of length 32, perform direct $O(N^2)$ DFTs on
those 32-element vectors, then combine the results to give a final
answer in one "32-way" step; or we could do the decomposition based on
any other combination of factors whose overall product is 1024.

For any particular length of input vector, there's no *a priori* way
to know what the most efficient decomposition will be.  It's very
dependent on machine architecture and cache size.  However, the FFT is
a deterministic algorithm -- once you decide on a scheme to factorise
the input length, the whole of the rest of the computation always
proceeds in exactly the same order: the same input vector elements are
always accessed in the same order and are used to build the same
intermediate results in the same order.  This means that once you know
the best factorisation for a given input vector length, it doesn't
change depending on the input data values.

It might be an interesting exercise to work out the best factorisation
for a given input vector length, machine architecture and cache size
by hand, but it's really a rather futile task.  As well as the
"theoretical" cache coherency calculations you would need to do, you
would also need detailed information about the machine code that your
compiler produces for different factorisations.  Any future compiler
improvements would invalidate your calculations: for example, if a
later version of your compiler can make better use of SIMD
instructions, this will change the balance between memory access and
potential pipeline stalls, and you would have to take account of that.

The approach to this problem taken by FFTW (and by the ATLAS low-level
linear algebra libraries) is to use *empirical* information to select
the factorisation to use.  Instead of trying to determine exactly
which factorisation is the best from first principles, we choose a
number of likely good factorisations and *measure* which one is best.

In the next article in this series, we'll introduction the Haskell
Criterion benchmarking framework and use it to compare our unoptimised
mixed-radix FFT algorithm with some other examples, in order to get
some idea of how far we have to go in terms of optimising our code.
