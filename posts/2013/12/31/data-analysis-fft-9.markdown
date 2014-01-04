---
author: Ian
tags: data-analysis,haskell
title: Haskell FFT 9: Prime-Length FFT With Rader's Algorithm
published: 2013-12-31 13:49:36
---

[The code from this article is available as a [Gist](https://gist.github.com/ian-ross/8197357)]

In the [last article in this series][last], benchmarking results
gave a pretty good indication that we needed to do *something* about
prime-length FFTs if we want to get good scaling for all input sizes.
Falling back on the $O(N^2)$ DFT algorithm just isn't good enough.

In this article, we're going to look at an implementation of
[Rader's FFT algorithm for prime-length inputs][rader].  Some aspects
of this algorithm are not completely straightforward to understand
since they rely on a couple of results from number theory and group
theory.  We'll try to use small examples to motivate what's going on.

<!--MORE-->

Before we begin exploring Rader's algorithm, it's probably worth
taking a minute to look at why we might expect there to be a better
algorithm for prime-length FFTs than the naïve $O(N^2)$ DFT algorithm
we've been using so far.  Throughout this article, we're going to use
$F_5$ and $F_7$ as running examples (you'll see why later).  Here's
$F_5$:

$$F_5 = \begin{pmatrix}
    1 & 1 & 1 & 1 & 1 \\
    1 & \omega_{5}^{1} & \omega_{5}^{2} & \omega_{5}^{3} & \omega_{5}^{4} \\
    1 & \omega_{5}^{2} & \omega_{5}^{4} & \omega_{5}^{6} & \omega_{5}^{8} \\
    1 & \omega_{5}^{3} & \omega_{5}^{6} & \omega_{5}^{9} & \omega_{5}^{12} \\
    1 & \omega_{5}^{4} & \omega_{5}^{8} & \omega_{5}^{12} & \omega_{5}^{16} \\
  \end{pmatrix} =
  \begin{pmatrix}
    1 & 1 & 1 & 1 & 1 \\
    1 & \omega_{5}^{1} & \omega_{5}^{2} & \omega_{5}^{3} & \omega_{5}^{4} \\
    1 & \omega_{5}^{2} & \omega_{5}^{4} & \omega_{5}^{1} & \omega_{5}^{3} \\
    1 & \omega_{5}^{3} & \omega_{5}^{1} & \omega_{5}^{4} & \omega_{5}^{2} \\
    1 & \omega_{5}^{4} & \omega_{5}^{3} & \omega_{5}^{2} & \omega_{5}^{1} \\
  \end{pmatrix}$$

and here's $F_7$:

$$F_7 = \begin{pmatrix}
    1 & 1 & 1 & 1 & 1 & 1 & 1 \\
    1 & \omega_{7}^{1} & \omega_{7}^{2} & \omega_{7}^{3} & \omega_{7}^{4} & \omega_{7}^{5} & \omega_{7}^{6} \\
    1 & \omega_{7}^{2} & \omega_{7}^{4} & \omega_{7}^{6} & \omega_{7}^{8} & \omega_{7}^{10} & \omega_{7}^{12} \\
    1 & \omega_{7}^{3} & \omega_{7}^{6} & \omega_{7}^{9} & \omega_{7}^{12} & \omega_{7}^{15} & \omega_{7}^{18} \\
    1 & \omega_{7}^{4} & \omega_{7}^{8} & \omega_{7}^{12} & \omega_{7}^{16} & \omega_{7}^{20} & \omega_{7}^{24} \\
    1 & \omega_{7}^{5} & \omega_{7}^{10} & \omega_{7}^{15} & \omega_{7}^{20} & \omega_{7}^{25} & \omega_{7}^{30} \\
    1 & \omega_{7}^{6} & \omega_{7}^{12} & \omega_{7}^{18} & \omega_{7}^{24} & \omega_{7}^{30} & \omega_{7}^{36} \\
  \end{pmatrix} =
  \begin{pmatrix}
    1 & 1 & 1 & 1 & 1 & 1 & 1 \\
    1 & \omega_{7}^{1} & \omega_{7}^{2} & \omega_{7}^{3} & \omega_{7}^{4} & \omega_{7}^{5} & \omega_{7}^{6} \\
    1 & \omega_{7}^{2} & \omega_{7}^{4} & \omega_{7}^{6} & \omega_{7}^{1} & \omega_{7}^{3} & \omega_{7}^{5} \\
    1 & \omega_{7}^{3} & \omega_{7}^{6} & \omega_{7}^{2} & \omega_{7}^{5} & \omega_{7}^{1} & \omega_{7}^{4} \\
    1 & \omega_{7}^{4} & \omega_{7}^{8} & \omega_{7}^{5} & \omega_{7}^{2} & \omega_{7}^{6} & \omega_{7}^{3} \\
    1 & \omega_{7}^{5} & \omega_{7}^{3} & \omega_{7}^{1} & \omega_{7}^{6} & \omega_{7}^{4} & \omega_{7}^{2} \\
    1 & \omega_{7}^{6} & \omega_{7}^{5} & \omega_{7}^{4} & \omega_{7}^{3} & \omega_{7}^{2} & \omega_{7}^{1} \\
  \end{pmatrix}$$

The main thing to notice is that if you look at the sub-matrix that
excludes the top row and left column (both of which have entries that
are all ones) from $F_5$ and $F_7$, you'll see a matrix each of whose
rows and columns is a permutation of the values $\omega_N^1,
\omega_N^2, \dots, \omega_N^{N-1}$.  This is because each row of $F_N$
is of the form $(\omega_N^{0k}, \omega_N^{1k}, \omega_N^{2k}, \dots,
\omega_N^{(N-1)k})$ for $1 \leq k \leq N-1$.  For prime $N$, none of
these $k$ values divides $N$ exactly, so that there are no "repeats"
in the powers of $\omega_N$.

So, there's definitely something a little bit special about $F_N$ for
prime $N$.  To make effective use of this to produce a prime-length
FFT requires some non-obvious insight.

Throughout the remainder of this article, we'll use $p$ to denote some
prime number, just to avoid repeatedly writing "for some prime $N$".

## Rader's algorithm

The basic idea of Rader's algorithm is to make use of the special
permutation structure of the powers of $\omega_p$ in $F_p$ to write
the DFT summation

$$H_n = \sum_{k=0}^{N-1} h_k e^{2\pi i k n/N} \qquad n = 0, 1, 2,
  \dots, N-1. \qquad (1)$$

as a *convolution*, which can then be calculated via FFTs of non-prime
lengths.  We'll step through how this works, leaving a couple of
details aside until we come to the implementation.

#### Separation of zero-index values

The first thing we do is to split out the special zero-index parts of
the Fourier matrix calculation, so that $(1)$ becomes

$$\begin{gathered}
    H_0 = \sum_{k=0}^{p-1} h_p \\
    H_n = h_0 + \sum_{k=1}^{p-1} h_k \, \omega_p^{nk} \qquad n = 1, 2,
    \dots, p-1.
  \end{gathered} \qquad (2)$$

We thus need to work out an efficient way of calculating the sum in
the second expression.

#### Multiplicative group modulo $n$

The most common approach to thinking of a group structure for integers
modulo some number $n$ is to use addition as the group operation,
giving the group $\mathbb{Z}/n$ with group elements $0, 1, 2, \dots,
n-1$.  A less common approach is to think of the integers in $1, 2,
\dots, n-1$ that are prime relative to $n$ with the group operation
being *multiplication* modulo $n$.  This group is denoted by a range
of different notations, but we'll call it $\mathbb{Z}_n^{\times}$.
For prime $n$, all of the numbers $1, 2, \dots, n-1$ are elements of
$\mathbb{Z}_n^{\times}$.  For example, for $n=5$ and $n=7$, we have
the following group multiplication tables:

<div style="display: inline-block; width: 48%;">
  <table class="group-multiplication" style="margin-left: auto; margin-right: auto;">
    <col class="left-col">
    <tr class="top-row">
      <td></td> <td>**1**</td> <td>**2**</td> <td>**3**</td> <td>**4**</td>
    </tr>
    <tr><td>**1**</td> <td>1</td> <td>2</td> <td>3</td> <td>4</td></tr>
    <tr><td>**2**</td> <td>2</td> <td>4</td> <td>1</td> <td>3</td></tr>
    <tr><td>**3**</td> <td>3</td> <td>1</td> <td>4</td> <td>2</td></tr>
    <tr><td>**4**</td> <td>4</td> <td>3</td> <td>2</td> <td>1</td></tr>
  </table>
  <br>
  <br>
</div>
<div style="display: inline-block; width: 48%;">
  <table class="group-multiplication" style="margin-left: auto; margin-right: auto;">
    <col class="left-col">
    <tr class="top-row">
      <td></td> <td>**1**</td> <td>**2**</td> <td>**3**</td>
                <td>**4**</td> <td>**5**</td> <td>**6**</td>
    </tr>
    <tr><td>**1**</td> <td>1</td> <td>2</td> <td>3</td> <td>4</td> <td>5</td> <td>6</td></tr>
    <tr><td>**2**</td> <td>2</td> <td>4</td> <td>6</td> <td>1</td> <td>3</td> <td>5</td></tr>
    <tr><td>**3**</td> <td>3</td> <td>6</td> <td>2</td> <td>5</td> <td>1</td> <td>4</td></tr>
    <tr><td>**4**</td> <td>4</td> <td>1</td> <td>5</td> <td>2</td> <td>6</td> <td>3</td></tr>
    <tr><td>**5**</td> <td>5</td> <td>3</td> <td>1</td> <td>6</td> <td>4</td> <td>2</td></tr>
    <tr><td>**6**</td> <td>6</td> <td>5</td> <td>4</td> <td>3</td> <td>2</td> <td>1</td></tr>
  </table>
</div>

This group turns out to be the key to writing the sum in $(2)$ as a
convolution, since both $n$ and $k$ range over the values $1, 2,
\dots, p-1$ and the multiplication in the power of $\omega_p$ is of
necessity calculated modulo $p$ (since $\omega_p^p = 1$).

#### Group generators

The group $\mathbb{Z}_p^{\times}$ is *cyclic*, which means that any
element of the group $a$ can be written as an integer power of a
single element of the group, the group *generator* $g$, i.e. $a = g^i$
for some unique positive integer $i$ with $0 \leq i \leq p-2$, and
equivalently $a = g^{-j}$ for some unique positive integer $j$ with $0
\leq j \leq p-2$, where negative powers of $g$ denote powers of the
multiplicative inverse (modulo $p$) of $g$.  For example, for
$\mathbb{Z}_5^{\times}$, either 2 or 3 is a generator:

$$\begin{gathered}
  2^0 = 1 = 1 \,\mathrm{mod}\, 5 \qquad
  2^1 = 2 = 2 \,\mathrm{mod}\, 5 \qquad
  2^2 = 4 = 4 \,\mathrm{mod}\, 5 \qquad
  2^3 = 8 = 3 \,\mathrm{mod}\, 5 \\
  2^{-0} = 1 = 1 \,\mathrm{mod}\, 5 \qquad
  2^{-1} = 3 = 3 \,\mathrm{mod}\, 5 \qquad
  2^{-2} = 9 = 4 \,\mathrm{mod}\, 5 \qquad
  2^{-3} = 27 = 2 \,\mathrm{mod}\, 5 \\
  \\
  3^0 = 1 = 1 \,\mathrm{mod}\, 5 \qquad
  3^1 = 3 = 3 \,\mathrm{mod}\, 5 \qquad
  3^2 = 9 = 4 \,\mathrm{mod}\, 5 \qquad
  3^3 = 27 = 2 \,\mathrm{mod}\, 5 \\
  3^{-0} = 1 = 1 \,\mathrm{mod}\, 5 \qquad
  3^{-1} = 2 = 2 \,\mathrm{mod}\, 5 \qquad
  3^{-2} = 4 = 4 \,\mathrm{mod}\, 5 \qquad
  3^{-3} = 8 = 3 \,\mathrm{mod}\, 5
\end{gathered}$$

In this case, $2 = 3^{-1}$ and $3 = 2^{-1}$.  For
$\mathbb{Z}_7^{\times}$, a suitable generator is 3, and

$$\begin{gathered}
  3^0 = 1 = 1 \,\mathrm{mod}\, 7 \qquad
  3^1 = 3 = 3 \,\mathrm{mod}\, 7 \qquad
  3^2 = 9 = 2 \,\mathrm{mod}\, 7 \\
  3^3 = 27 = 6 \,\mathrm{mod}\, 7 \qquad
  3^4 = 81 = 4 \,\mathrm{mod}\, 7 \qquad
  3^5 = 243 = 5 \,\mathrm{mod}\, 7 \\
  \\
  3^{-0} = 1 = 1 \,\mathrm{mod}\, 7 \qquad
  3^{-1} = 5 = 5 \,\mathrm{mod}\, 7 \qquad
  3^{-2} = 25 = 4 \,\mathrm{mod}\, 7 \\
  3^{-3} = 125 = 6 \,\mathrm{mod}\, 7 \qquad
  3^{-4} = 625 = 2 \,\mathrm{mod}\, 7 \qquad
  3^{-5} = 3125 = 3 \,\mathrm{mod}\, 7
\end{gathered}$$

In this case, $5 = 3^{-1}$.

We'll talk about how we determine the group generator $g$ later.  For
the moment, assume that we know a suitable generator.

#### Representation as convolution

Given a generator $g$ for the group $\mathbb{Z}_p^{\times}$, we can
write the second equation in $(2)$ as

$$H_{g^{-r}} = h_0 + \sum_{q=0}^{p-2} h_{g^q} \, \omega_p^{g^{q-r}}
  = h_0 + \sum_{q=0}^{p-2} h_{g^q} \, \omega_p^{g^{-(r-q)}}
  \qquad r = 0, 1, \dots, p-2. \qquad (3)$$

(This relies on the fact that $h_k$ and $H_k$ are both periodic in $p$
and $\omega_p^p = 1$.)

For example, if $p=5$, taking $g=2$, we have:

$$\begin{gathered}
  r = 0, \; g^{-r} = 1 \Rightarrow
  H_1 = h_0 + \sum_{q=0}^{p-2} h_{g^q} \omega_5^{g^q} =
  h_0 + h_1 \omega_5 + h_2 \omega_5^2 + h_4 \omega_5^4 + h_3 \omega_5^3 \\
  r = 1, \; g^{-r} = 3 \Rightarrow
  H_3 = h_0 + \sum_{q=0}^{p-2} h_{g^q} \omega_5^{g^{q-1}} =
  h_0 + h_1 \omega_5^3 + h_2 \omega_5^1 + h_4 \omega_5^2 + h_3 \omega_5^4 \\
  r = 2, \; g^{-r} = 4 \Rightarrow
  H_4 = h_0 + \sum_{q=0}^{p-2} h_{g^q} \omega_5^{g^{q-2}} =
  h_0 + h_1 \omega_5^4 + h_2 \omega_5^3 + h_4 \omega_5^1 + h_3 \omega_5^2 \\
  r = 3, \; g^{-r} = 2 \Rightarrow
  H_2 = h_0 + \sum_{q=0}^{p-2} h_{g^q} \omega_5^{g^{q-3}} =
  h_0 + h_1 \omega_5^2 + h_2 \omega_5^4 + h_4 \omega_5^3 + h_3 \omega_5^1
\end{gathered}$$

and comparison of the right hand sides of each of these equations with
the rows of $F_5$ shows that this generator-based approach replicates
the rows of the Fourier matrix, albeit in a different order to the
"normal" order.

The summation in the expression $(3)$ is in the form of a cyclic
convolution of the two sequences $a_q = h_{g^q}$ and $b_q =
\omega_p^{g^{-q}}$, both of length $p-1$ with $0 \leq q \leq p-2$.
For example, for $p=5$, taking $g=2$:

<div style="width: 100%;">
  <table class="lines" style="margin-left: auto; margin-right: auto;">
    <col class="left-col">
    <tr class="top-row"><td>$q$</td> <td>&nbsp;&nbsp;0&nbsp;&nbsp;</td> <td>&nbsp;&nbsp;1&nbsp;&nbsp;</td> <td>&nbsp;&nbsp;2&nbsp;&nbsp;</td> <td>&nbsp;&nbsp;3&nbsp;&nbsp;</td></tr>
    <tr><td>$a_q$</td> <td>$h_1$</td> <td>$h_2$</td> <td>$h_4$</td> <td>$h_3$</td></tr>
    <tr><td>$b_q$</td> <td>$\omega_5^1$</td> <td>$\omega_5^3$</td> <td>$\omega_5^4$</td> <td>$\omega_5^2$</td></tr>
  </table>
</div>

and for $p=7$ with $g=3$:

<div style="width: 100%;">
  <table class="lines" style="margin-left: auto; margin-right: auto;">
  <col class="left-col">
    <tr class="top-row"><td>$q$</td> <td>&nbsp;&nbsp;0&nbsp;&nbsp;</td> <td>&nbsp;&nbsp;1&nbsp;&nbsp;</td> <td>&nbsp;&nbsp;2&nbsp;&nbsp;</td> <td>&nbsp;&nbsp;3&nbsp;&nbsp;</td> <td>&nbsp;&nbsp;4&nbsp;&nbsp;</td> <td>&nbsp;&nbsp;5&nbsp;&nbsp;</td></tr>
    <tr><td>$a_q$</td> <td>$h_1$</td> <td>$h_3$</td> <td>$h_2$</td> <td>$h_6$</td> <td>$h_4$</td> <td>$h_5$</td></tr>
    <tr><td>$b_q$</td> <td>$\omega_7^1$</td> <td>$\omega_7^5$</td> <td>$\omega_7^4$</td> <td>$\omega_7^6$</td> <td>$\omega_7^2$</td> <td>$\omega_7^3$</td></tr>
  </table>
</div>

#### Calculation of convolution using FFT

Recall that, for a continuous convolution of the form

$$(f * g)(t) = \int_{-\infty}^{\infty} f(\tau) g(t - \tau) \, dt,$$

we have the convolution theorem:

$$\mathcal{F}(f * g) = \mathcal{F}(f) \cdot \mathcal{F}(g),$$

where we denote the Fourier transform of a function $f$ by
$\mathcal{F}(f)$.  A comparable result applies for the discrete cyclic
convolution in $(3)$.  Let us denote the sequence
$a_q$ defined above as $\tilde{h} = (h_{g^q})$ and the sequence $b_q$
as $\tilde{\omega}_p = (\omega_p^{g^{-q}})$, both for $0 \leq q \leq
p-2$, and let us write $\tilde{H} = (H_{g^{-q}})$ for the same range
of $q$ to represent the results of the convolution as a sequence.
Then:

$$\tilde{H} - h_0 = \mathrm{DFT}^{-1}\left[ \mathrm{DFT}[\tilde{h}]
    \cdot \mathrm{DFT}[\tilde{\omega}_p] \right]$$

The discrete Fourier transform of the prime-length input can thus be
calculated by:

1. Reordering the input vector according to the indexing scheme for
   the sequence $\tilde{h}$;
2. Calculating the DFT of $\tilde{h}$;
3. Multiplying $\mathrm{DFT}[\tilde{h}]$ pointwise by the DFT of the
   reordered sequence of powers of $\omega_p$ represented by the
   sequence $\tilde{\omega}_p$;
4. Calculating the inverse DFT of this product;
5. Reordering the result according to the indexing scheme for the
   sequence $\tilde{H}$ and adding in the DC offset $h_0$.

The convolution derived above is of length $p-1$, which, since $p$ is
prime, must be composite.  We can thus calculate the length $p-1$
discrete Fourier transforms required by the above approach using our
Cooley-Tukey divide and conquer algorithm.  The index reorderings and
the DFT of the sequence of powers of $\omega_p$ can all be determined
in advance since they depend only on $p$.

For $p=5$, taking $g=2$, this approach looks like this:

1. Reorder the input vector to give $\tilde{h} = (h_1, h_2, h_4, h_3)$.
2. Calculate $\mathrm{DFT}[\tilde{h}]$ using a conventional FFT
   algorithm; in this case, this is efficient because the length of
   $\tilde{h}$ is a power of two.
3. Multiply $\mathrm{DFT}[\tilde{h}]$ pointwise by
   $\mathrm{DFT}[\tilde{\omega}_5]$, which can be pre-computed:
   $\tilde{\omega}_5 = (\omega_5^1, \omega_5^3, \omega_5^4, \omega_5^2)$.
4. Calculate the inverse DFT of the pointwise product to give
   $\tilde{H}$ -- this is again an efficient calculation because the
   input length is a power of two.
5. Add the DC offset and extract the final results from $\tilde{H} =
   (H_1, H_3, H_4, H_2)$.

#### Padding for efficiency

For $p=5$, $p-1$ is a power of two, which means that the FFT
calculations needed in the Rader algorithm are as efficient as
possible.  However, if $p-1$ itself has large prime factors, it may
prove necessary to apply Rader's algorithm recursively, which will be
much less efficient than a direct recursive FFT.

An alternative (and better) approach is provided by zero-padding the
sequences involved in the convolution -- by padding to a length that
is a power of two, recursive application of Rader's algorithm is
avoided.  To do this, let us write the two sequences to be convolved
as $f[i]$ and $g[i]$, both of length $M-1$ (we write $M-1$ here to
match the convolution length $p-1$ in the FFT calculation, and we use
the square bracket notation for indexing to make some of the
expressions below less ambiguous).  The cyclic convolution of these
two sequences is

$$(f * g)[n] = \sum_{m=0}^{M-1} f[m] g[n-m]$$

where all indexes on the sequences $f[i]$ and $g[i]$ are zero-based
and are taken modulo $M$.

We produce new sequences $f\prime[j]$ and $g\prime[j]$ of length
$M\prime$ where $M\prime \geq 2M - 3$.  This condition on $M\prime$ is
required to avoid "interference" between unrelated elements of the
original sequences due to the wrap-around of the cyclic convolution
that we're going to compute.  In our FFT application, we will choose
$M\prime$ to be a power of two, but any value works as long as it is
large enough to satisfy this condition.  The sequence $f\prime[j]$ is
constructed by inserting $M\prime-M$ zeroes between the zeroth and
first element of $f[i]$.  Sequence $g\prime[j]$ is constructed by
cyclically repeating the values of $g[i]$ to give a sequence of total
length $M\prime$.  If we now convolve the sequences $f\prime[j]$ and
$g\prime[j]$, we find that the result contains the convolution of the
original sequences $f[i]$ and $g[i]$ as its first $M-1$ elements.

A minimal example gives a feeling for why this works.  Suppose that we
set $M=4$ (so the sequences are of length 3) and consider

$$f[i] = (1, 2, 3) \quad \text{and} \quad g[i] = (a, b, c).$$

The cyclic convolution of these sequences is found as:

$$\begin{gathered}
    (f * g)[0] = f[0] g[0-0] + f[1] g[0-1] + f[2] g[0-2] = 1 a + 2 c + 3 b, \\
    (f * g)[1] = f[0] g[1-0] + f[1] g[1-1] + f[2] g[1-2] = 1 b + 2 a + 3 c, \\
    (f * g)[2] = f[0] g[2-0] + f[1] g[2-1] + f[2] g[2-2] = 1 c + 2 b + 3 a.
  \end{gathered}$$

The condition on $M\prime$ is that $M\prime \geq 2M-3 = 5$.  Putting $M\prime=5$, the
new sequences are then

$$f\prime[j] = (1, 0, 0, 2, 3) \quad \text{and} \quad g\prime[j] = (a, b, c, a, b).$$

and the first three elements of the cyclic convolution of these new
sequences are:

$$\begin{gathered}
    \begin{split}
      (f\prime * g\prime)[0] &=
      f\prime[0] g\prime[0-0]+f\prime[1] g\prime[0-1]+f\prime[2] g\prime[0-2]+ \\
      &f\prime[3] g\prime[0-3]+f\prime[4] g\prime[0-4] =
      1 a + 0 b + 0 a + 2 c + 3 b =
      1 a + 2 c + 3 b,
    \end{split} \\
    \begin{split}
      (f\prime * g\prime)[1] &=
      f\prime[0] g\prime[1-0]+f\prime[1] g\prime[1-1]+f\prime[2] g\prime[1-2]+ \\
      &f\prime[3] g\prime[1-3]+f\prime[4] g\prime[1-4] =
      1 b + 0 a + 0 b + 2 a + 3 c =
      1 b + 2 a + 3 c,
    \end{split} \\
    \begin{split}
      (f\prime * g\prime)[2] &=
      f\prime[0] g\prime[2-0]+f\prime[1] g\prime[2-1]+f\prime[2] g\prime[2-2]+ \\
      &f\prime[3] g\prime[2-3]+f\prime[4] g\prime[2-4] =
      1 c + 0 b + 0 a + 2 b + 3 a =
      1 c + 2 b + 3 a.
    \end{split}
  \end{gathered}$$

We see that these are identical to the values found from the original
sequences $f[i]$ and $g[i]$.

To make use of this result in Rader's algorithm, we just choose the
smallest power of two that satisfies the length condition on the
sequences to be convolved, pad the input sequence $f[i]$ with zeroes
and convolve with the repeated $\omega_p$ sequence.  (Most of the
details can be pre-computed as part of the FFT "planning" phase.)


## Implementating Rader's algorithm

In this section, we'll look at a basic Haskell implementation of
Rader's algorithm.  The implementation here is structured more to be
understandable than to be particularly efficient.  We'll reorganise
the code to pre-compute some things for efficiency when we start
optimising the overall FFT code later on.

After describing the details of the main algorithm, we'll look at the
approach used for the calculation of primitive roots of $p$ to
determine generators of the group $\mathbb{Z}_p^{\times}$, since this
requires a little explanation.  Finally, we'll look at some test
results.  Here's the code for our Haskell implementation (we'll refer
to the line numbers in what follows):

~~~~ {.haskell .numberLines}
-- | FFT and inverse FFT drivers for Rader's algorithm.
raderFFT, raderIFFT :: VCD -> VCD
raderFFT = raderFFT' 1 1
raderIFFT v = raderFFT' (-1) (1.0 / (fromIntegral $ length v)) v

-- | Rader's algorithm for prime-length complex-to-complex Fast
-- Fourier Transform.
raderFFT' :: Int -> Double -> VCD -> VCD
raderFFT' sign scale xs
  | isPrime p = map ((scale :+ 0) *) $ generate p $ \idx -> case idx of
    0 -> sum xs
    _ -> xs ! 0 + convmap M.! idx
  | otherwise = error "non-prime length in raderFFT"
  where p = length xs
        p1 = p - 1
        -- ^ Convolution length.
        p1pad = if p1 == 2^(log2 p1)
                then p1
                else 2 ^ (1 + log2 (2 * p1 - 3))
        -- ^ Convolution length padded to next greater power of two.
        g = primitiveRoot p
        -- ^ Group generator.
        ig = invModN p g
        -- ^ Inverse group generator.
        as = backpermute xs $ iterateN p1 (\n -> (g * n) `mod` p) 1
        -- ^ Input values permuted according to group generator
        -- indexing.
        pad = p1pad - p1
        -- ^ Padding size.
        apad = generate p1pad $
               \idx -> if idx == 0 then as ! 0
                       else if idx > pad then as ! (idx - pad) else 0
        -- ^ Permuted input vector padded to next greater power of two
        -- size for fast convolution.
        iidxs = iterateN p1 (\n -> (ig * n) `mod` p) 1
        -- ^ Index vector based on inverse group generator ordering.
        w = omega p
        bs = backpermute (map ((w ^^) . (sign *)) $ enumFromTo 0 p1) iidxs
        -- ^ Root of unity powers based on inverse group generator
        -- indexing.
        bpad = generate p1pad (\idx -> bs ! (idx `mod` p1))
        -- ^ Root of unity powers cyclically repeated to make vector
        -- of next power of two length for fast convolution.
        conv = ifft $ zipWith (*) (fft apad) (fft bpad)
        -- ^ FFT-based convolution calculation.
        convmap = M.fromList $ toList $ zip iidxs conv
        -- ^ Map constructed to enable inversion of inverse group
        -- generator index ordering for output.
~~~~
<br>

#### Overall driver

As for the earlier [mixed-radix FFT][mixed-radix], we have top-level
functions to perform forward and inverse FFTs (lines 2-4), both of
which call a common driver function (`raderFFT'`), passing in sign and
scale parameters to control the direction and final output scaling of
the transform.

The final generation of the transformed output (lines 10-12) applies
the appropriate output scaling to a vector constructed from the sum of
the input elements (index 0, corresponding to $H_0$ in $(2)$) and
(other indexes) a DC offset ($h_0$ in $(2)$) and the appropriate
element of the generator-based convolution.  In order to deal with the
generator-based indexing for the $H$ values in $(3)$, the convolution
results are put into a map (`convmap`: line 46) from where they can
easily be extracted in output index order.

#### Index organisation

The determination of the indexes needed for the generator-based
indexing scheme in $(3)$ is done by:

1. Calculating the generator `g` for the group $\mathbb{Z}_p^{\times}$
   where $p$ is the prime input vector length (line 21: see below for
   details);
2. Permuting the input vector elements according to powers of the
   group generator (line 25: this is the computation of the sequence
   $a_q$ for the convolution using the indexing defined in $(3)$);
3. Calculating the inverse element of the generator in
   $\mathbb{Z}_p^{\times}$ (line 23: the `invModN` function finds the
   multiplicative inverse modulo $N$ of a given integer argument);
4. Calculating the inverse generator-based indexes needed for
   permuting both the powers of $\omega_p$ and the output elements
   (`iidxs`: line 35).

The `iidxs` index vector is used to permute the powers of $\omega_p$
(line 38) producing the $b_q$ sequence used in the convolution of
$(3)$ and is also used (line 46) to build the index map used to
extract result values from the result of the convolution.

#### Padding to power-of-two length

If $p-1$ is a power of two, no padding of the $a_q$ and $b_q$
sequences is needed for efficient calculation of the convolution
$(3)$.  In all other cases, both sequences need to be padded to a
suitable power of two.  Calculation of the padded length (lines 17-19)
take account of the minimum size requirement on the padded inputs to
the convolution to avoid "wrap-around" effects, and this length is
used to control the generation of padded versions of $a_q$ (`apad`:
lines 30-32) and $b_q$ (`bpad`: line 41).  (The "padded" version of
$b_q$ is just a cyclic repeat of the values calculated in line 38.)

#### Convolution

Once suitably padded versions of the $a_q$ and $b_q$ sequences are
computed, the actual convolution step is simple (line 44).  Both
sequences are Fourier transformed (recall that the padded sequence
lengths are a power of two, so this is an efficient computation),
multiplied together pointwise and inverse transformed.  The
convolution of the original unpadded sequences is then found in the
first $p-1$ entries of this result &mdash; this is what is picked out
by the indexing defined by `convmap`, defined in line 46.

#### Primitive root calculation

An important step in Rader's prime length FFT algorithm is the
determination of the generator of the group $\mathbb{Z}_p^{\times}$.
For this group, the group generator is called *the primitive root
modulo $p$*.

There is no simple general formula to compute primitive roots modulo
$n$, but there are methods to determine a primitive root faster than
simply trying out all candidate values.  In our case, we're only ever
going to be dealing with primitive roots of prime $p$, which
simplifies things a little.  We proceed by first calculating
$\varphi(p)$, where $\varphi$ is Euler's totient function.  For prime
$p$, $\varphi(p) = p - 1$.  We then determine the distinct prime
factors of $\varphi(p)$, which we'll call $p_1, p_2, \dots, p_k$.
Then, for each $m \in \mathbb{Z}_p^{\times}$, we compute

$$m^{\varphi(p) / p_i} \,\mathrm{mod}\, n \qquad\text{ for } i = 1, \dots, k$$

using a fast algorithm for modular exponentiation (we use
exponentiation by squaring).  A number $m$ for which these $k$ values
are all different from 1 is a primitive root.

The implementation is pretty much just a straightforward transcription
into Haskell of the description above.  In cases where there multiple
primitive roots (the number of primitive roots modulo $n$, if there
are any, is $\varphi(\varphi(n))$), we just take the first one:

~~~~ {.haskell}
primitiveRoot :: Int -> Int
primitiveRoot p
  | isPrime p =
    let tot = p - 1
        -- ^ Euler's totient function for prime values.
        totpows = map (tot `div`) $ fromList $ nub $ toList $ factors tot
        -- ^ Powers to check.
        check n = all (/=1) $ map (expt p n) totpows
        -- ^ All powers are different from 1 => primitive root.
    in fromIntegral $ head $ dropWhile (not . check) $ fromList [1..p-1]
  | otherwise = error "Attempt to take primitive root of non-prime value"


-- | Fast exponentation modulo n by squaring.
--
expt :: Int -> Int -> Int -> Int
expt n b pow = fromIntegral $ go pow
  where bb = fromIntegral b
        nb = fromIntegral n
        go :: Int -> Integer
        go p
          | p == 0 = 1
          | p `mod` 2 == 1 = (bb * go (p - 1)) `mod` nb
          | otherwise = let h = go (p `div` 2) in (h * h) `mod` nb
~~~~

We also have some support code for QuickCheck testing of the primitive
root algorithm:

~~~~ {.haskell}
-- | QuickCheck generator for prime values.  Inefficient...
--
newtype Prime a = Prime { getPrime :: a }
                deriving (Eq, Ord, Show, Read, Num, Integral, Real, Enum)
instance (Integral a, Ord a, Arbitrary a) => Arbitrary (Prime a) where
  arbitrary = (Prime . (\n -> P.head $ P.dropWhile (< n) primes)) `fmap`
              (arbitrary `suchThat` (> 1))

-- Test code for primitive root determination.

prop_primitive_root ((Prime n) :: Prime Int) =
  primitiveRootTest n $ primitiveRoot n

primitiveRootTest :: Int -> Int -> Bool
primitiveRootTest p root
  | isPrime p = (sort $ toList $ pows) == [1..p-1]
  | otherwise = error "Attempt to take primitive root of non-prime value"
  where pows = generate (p - 1) (expt p root)
~~~~

We need to generate prime numbers to test the primitive root
calculation, and it's much quicker to write a separate QuickCheck
generator, using a specialised `Arbitrary` instance for a custom
`Prime` `newtype` to do this than to try generating positive integers
and filtering out non-primes using QuickCheck's `==>` filtering
operator.  We can collect some evidence that the algorithm works by
doing something like this in GHCi:

~~~~
> :load Prime-FFT.hs
[1 of 1] Compiling Prime-FFT         ( Prime-FFT.hs, interpreted )
Ok, modules loaded: PrimeFFT.
> verboseCheckWith (stdArgs { maxSize=25 }) prop_primitive_root
Passed:
Prime {getPrime = 3}
Passed:
Prime {getPrime = 7}
...
Passed:
Prime {getPrime = 14771}
Passed:
Prime {getPrime = 6691}
Passed:
Prime {getPrime = 23753}
+++ OK, passed 100 tests.
~~~~

## Testing of Rader's algorithm

We can use the same approach for testing our implementation of Rader's
algorithm that we used for testing the
[mixed-radix FFT algorithm][mixed-radix], i.e. by comparing Rader FFT
results to results of the basic DFT algorithm, and testing FFT/IFFT
round-trip calculations.  The only slight wrinkle here is that we need
to test for input vectors of prime length only, so that we need a
specialised `Arbitrary` instance to generate these:

~~~~ {.haskell}
instance Arbitrary VCD where
  arbitrary = do
    len <- elements $ P.takeWhile (< 500) primes
    fromList <$> vectorOf len arbitrary
~~~~

With this, we can do some testing.  In GHCi:

~~~~
> :load Prime-FFT.hs
[1 of 1] Compiling PrimeFFT           ( Prime-FFT.hs, interpreted )
Ok, modules loaded: PrimeFFT.
> quickCheck prop_dft_vs_fft
+++ OK, passed 100 tests.
> quickCheck prop_ifft
+++ OK, passed 100 tests.
~~~~

Nice.  It works.  There are definitely improvements that we can make
to the code, which we'll do when we start optimising the overall FFT
calculation, but the algorithm is complicated enough that it's nice
just to have a working version to start from!

[last]: /blog/posts/2013/12/07/data-analysis-fft-8/index.html
[rader]: https://en.wikipedia.org/wiki/Rader%27s_FFT_algorithm
[mixed-radix]: /blog/posts/2013/11/27/data-analysis-fft-6.html
