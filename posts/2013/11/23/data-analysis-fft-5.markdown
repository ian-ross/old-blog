---
author: Ian
tags: data-analysis,haskell
title: Haskell FFT 5: Transforming Vectors of Arbitrary Lengths
published: 2013-11-23 13:38:29
---

[The code from this article is available as a [Gist](https://gist.github.com/ian-ross/7614186)]

So far, we've been dealing only with input vectors whose lengths are
powers of two.  This means that we can use the Danielson-Lanczos
result repeatedly to divide our input data into smaller and smaller
vectors, eventually reaching vectors of length one, for which the
discrete Fourier transform is just the identity.  Then, as long as we
take account of the bit reversal ordering due to the repeated even/odd
splitting we've done, we can use the Danielson-Lanczos lemma or its
matrix equivalent to efficiently reassemble the entries in our data
vector to form the DFT (at this point, you may want to review the
[second article](/blog/posts/2013/11/15/data-analysis-fft-2.html) to
remind yourself how this matrix decomposition works).

This is all standard stuff that you'll find in a lot of textbooks.
Starting in this article though, we're going to veer "off piste" and
start doing something a bit more interesting.  It used to be that, if
you wanted to do FFTs of vectors whose lengths weren't powers of two,
the professional advice was "don't do that" (that's what my copy of
*Numerical Recipes* says!).  However, if you're prepared to brave a
little bit of algebra, extending the Cooley-Tukey approach to vectors
of arbitrary lengths is not conceptually difficult.  It *is* a bit
fiddly to get everything just right, so in this article, we'll start
exploring the problem with some more "toy algebra".

<!--MORE-->

So then, what about vectors whose lengths aren't powers of two?
Consider the case when $N=6$.  There's nothing in the
Danielson-Lanczos lemma that requires the input vector length to be a
power of two: it just needs to be even.  In matrix terms, we have the
decomposition

$$F_6 =
\begin{pmatrix}
  I_3 & D_3 \\
  I_3 & -D_3
\end{pmatrix}
\begin{pmatrix}
  F_3 & \\
      & F_3
\end{pmatrix}
P_6,$$

where $D_3 = \mathrm{diag}(1, \omega_6, \omega_6^2)$ and $P_6$ is the
$6 \times 6$ even/odd permutation matrix.  We can't split the $3
\times 3$ Fourier matrices $F_3$ any further, but they're small enough
that we can probably pay the price of using the naïve $O(N^2)$ DFT
algorithm here.

So, it looks as though it should be possible to pull out factors of
two using the Danielson-Lanczos decomposition until we reach whatever
other factors in the input vector length are left over -- a vector of
length 40, for example, would involve three even/odd splits, ending up
with length-5 discrete Fourier transforms to be computed and
reassembled to give the final result.

What about pulling out other factors?  Consider the $N=6$ case again.
Can we split this into three length-2 DFTs?  Let's try to generalise
the Danielson-Lanczos lemma to this case -- consider the situation
where $N$ is divisible by three.  Then:

$$\begin{aligned}
    H_n &= \sum_{k=0}^{N-1} h_k e^{2\pi i k n/N} \\
    &= \sum_{k=0}^{N/3-1} h_{3k} e^{2\pi i (3k) n/N} +
    \sum_{k=0}^{N/3-1} h_{3k+1} e^{2\pi i (3k+1) n/N} +
    \sum_{k=0}^{N/3-1} h_{3k+2} e^{2\pi i (3k+2) n/N} \\
    &= \sum_{k=0}^{N/3-1} h_{2k} e^{2\pi i k n/(N/3)} +
    \omega^n \sum_{k=0}^{N/3-1} h_{3k+1} e^{2\pi i k n/(N/3)} +
    \omega^{2n} \sum_{k=0}^{N/3-1} h_{3k+2} e^{2\pi i k n/(N/3)} \\
    &= H^0_n + \omega_N^n H^1_n + \omega_N^{2n} H^2_n,
  \end{aligned}$$

where $H^0_n$, $H^1_n$ and $H^2_n$ are the "modulo 3" equivalents of
the even/odd $H^e_n$ and $H^o_n$ in the Danielson-Lanczos lemma.  For
$N=6$, in matrix form, this looks as follows:

$$F_6 =
\begin{pmatrix}
  I_2 & D_2^{(1)} & D_2^{(2)} \\
  I_2 & \omega_6^2 D_2^{(1)} & \omega_6^4 D_2^{(2)} \\
  I_2 & \omega_6^4 D_2^{(1)} & \omega_6^8 D_2^{(2)}
\end{pmatrix}
\begin{pmatrix}
  F_2 & & \\
  & F_2 & \\
  & & F_2
\end{pmatrix}
P_6^{(3)}$$

and for $N=9$, like this:

$$F_9 =
\begin{pmatrix}
  I_3 & D_3^{(1)} & D_3^{(2)} \\
  I_3 & \omega_9^3 D_3^{(1)} & \omega_9^6 D_3^{(2)} \\
  I_3 & \omega_9^6 D_3^{(1)} & \omega_9^{12} D_3^{(2)}
\end{pmatrix}
\begin{pmatrix}
  F_3 & & \\
  & F_3 & \\
  & & F_3
\end{pmatrix}
P_9^{(3)}.$$

Here, $P_6^{(3)}$ and $P_9^{(3)}$ are "modulo 3" permutation matrices:
$P_6^{(3)}$ is the $6 \times 6$ matrix that picks out elements 0, 3,
1, 4, 2, 5 in order and $P_9^{(3)}$ is the $9 \times 9$ matrix that
picks out elements 0, 3, 6, 1, 4, 7, 2, 5, 8 in order.  The matrices
$D_2^{(i)}$ and $D_3^{(i)}$ are diagonal matrices whose entries are
all powers of $\omega_6$ and $\omega_9$ respectively, the exact powers
depending on the "block column" $i$ of the matrix -- in these two
cases, $D_2^{(i)} = \mathrm{diag}(1, \omega_6^i)$ and $D_3^{(i)} =
\mathrm{diag}(1, \omega_9^i, \omega_9^{2i})$.

This is all obviously a bit quite a lot more complicated than the
powers-of-two case: in particular, getting the "twiddle factors" in
the first matrix right takes some care.  We can update the "toy
algebra system" code that we wrote for the powers-of-two case to
experiment with this and to convince ourselves that we're doing things
right:

~~~~
> idMatrix 9 3 %*% (fourierMultiple 9 3 %*% perm 9 3) == fourier 9
True
~~~~

Here, `idMatrix`, `fourierMultiple` and `perm` all take two argument,
the first being the input data size ($N$) and the second being the
number of parts to split the calculation into (for the powers-of-two
cases, this was always 2).  The code for `fourierMultiple` and `perm`
is is pretty much as you'd expect, except that we've added some
wrappers to generate the matrices for the inverse FFT as well as the
forward FFT:

~~~~ {.haskell}
-- Multiplied-up resized Fourier matrices.
fourierMultiple :: Int -> Int -> Mat
fourierMultiple = fourierMultiple' 1
fourierMultiple' :: Int -> Int -> Int -> Mat
fourierMultiple' sign np p =
  let n = np `div` p
      zs = replicate n Z
      zs0 = concat $ replicate (p-1) zs
      shift = map (take np . (zs ++))
      f = unMat $ fourier' sign n
  in Mat $ concat $ take p $ iterate shift $ map (++ zs0) f

-- Modulo permutation matrix.
perm :: Int -> Int -> Mat
perm np p =
  let n = np `div` p
      zz = (O P) : replicate (np-1) Z
      zs = take n $ iterate (take np . ((replicate p Z) ++)) zz
      shift = map (take np . (Z :))
  in Mat $ concat $ take p $ iterate shift zs
~~~~

The more difficult case is the new `idMatrix` function:

~~~~ {.haskell}
-- Generalised I + D matrix.
idMatrix :: Int -> Int -> Mat
idMatrix = idMatrix' 1
idMatrix' :: Int -> Int -> Int -> Mat
idMatrix' sign wfac split =
  let ns = wfac `div` split
      w = W P wfac sign
      i = unMat $ unit ns
      ds j = (O P) : [w^(i*j) | i <- [1..ns-1]]
      d r c = unMat $ diag $ map (w^(ns*r*c) *) (ds c)
      row r = L.foldl' (zipWith (++)) i $ map (d r) [1..split-1]
  in Mat $ concat $ map row [0..split-1]
~~~~

The tricky part is keeping the powers of $\omega_N$ straight
everywhere (the lines that define the local functions `ds` and `d` are
the critical ones).

Using this result, we can iteratively decompose the input data vector
length $N$ by splitting by any factors of $N$.  We eventually end up
with sub-vectors of prime length, which we can't split any more.  If
you want to try to understand the code in the next article, I'd
suggest you play with some examples in GHCi to get a feeling for how
this decomposition works.  It is a natural generalisation of the
powers-of-two decomposition, but the algebra is has just enough extra
complexity to make a bit of experimentation worthwhile.

Anyway, here's how we're going to proceed (we'll generalise this
approach later in an attempt to optimise things):

* We'll determine all the prime factors of $N$, ordered in increasing
  size;
* We'll then split our input data based on those factors until we
  reach a final collection of prime-length sub-vectors;
* We'll use a simple DFT calculation for the small prime-length
  vectors, then will use an appropriate generalisation of the
  powers-of-two FFT algorithm to combine everything to give the final
  FFT result.

For example, if $N=30$, we will do a normal even/odd split (for the
factor of 2), then a three-way split, to end up with six length-5
sub-vectors which we process using the simple DFT algorithm.  Of
course, we don't know that this "primes only, smallest first"
decomposition will give the most efficient FFT algorithm -- that
depends on the input vector size, the processor in the machine we're
using, the cache size and any number of other imponderables.  We'll
see later on how we can deal with this to figure out the most
efficient decomposition.  For the moment, we'll stick with the sorted
prime decomposition and try to write code that will work for the more
general case later on as well.

In the next article, we'll present the code that does all this, along
with some tests that give us some confidence that it works!