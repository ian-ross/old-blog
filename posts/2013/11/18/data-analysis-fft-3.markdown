---
author: Ian
tags: data-analysis,haskell
title: Haskell FFT 3: Cooley-Tukey for Powers of Two
published: 2013-11-18 20:17:26
---

[The code from this article is available as a [Gist](https://gist.github.com/ian-ross/7533714).
I'm going to go on putting the code for these articles up as
individual Gists as long as we're doing experimentation -- eventually,
there will be a proper repo and a Hackage package for the full "bells
and whistles" code.]

The decomposition of the Fourier matrix described in the
[previous article](/blog/posts/2013/11/15/data-analysis-fft-2.html)
allows us to represent the DFT of a vector of length $N$ by two DFTs
of length $N/2$.  If $N$ is a power of two, we can repeat this
decomposition until we reach DFTs of length 1, which are just an
identity transform.  We can then use the decomposition

$$F_{2N} =
  \begin{pmatrix}
    I_N & D_N \\
    I_N & -D_N
  \end{pmatrix}
  \begin{pmatrix}
    F_N & \\
        & F_N
\end{pmatrix}
  P_{2N} \qquad (1)$$

to build the final result up from these decompositions.

At each step in this approach, we treat the even and odd indexed
elements of our input vectors separately.  If we think of the indexes
as binary numbers, at the first step we decompose based on the lowest
order bit in the index, at the next step we decompose based on the
next lowest bit and so on.

<!--MORE-->

## Bit reversal permutation

What overall permutation does performing this decomposition
recursively give us?  We can work this out by thinking about the
permutation matrices $P_{2N}$ from $(1)$.  We know that $P_2 = I_2$
and the total permutation matrix involved in a recursive decomposition
will be

$$P^{\mathrm{(full)}}_{2N} = P_2^{(2^N)} \dots P_{2N/4}^{(4)}
    P_{2N/2}^{(2)} P_{2N},$$

where $M^{(n)}$ denotes a block-diagonal matrix composed of $n$ blocks
of a matrix $M$.  For $2N = 8$, since $P_2^{(2^N)} = I_{2N}$we have:

$$\begin{aligned}
    P^{\mathrm{(full)}}_{8} &= P_{4}^{(2)} P_{8} \\
    &= \begin{pmatrix}
      1 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
      0 & 0 & 1 & 0 & 0 & 0 & 0 & 0\\
      0 & 1 & 0 & 0 & 0 & 0 & 0 & 0\\
      0 & 0 & 0 & 1 & 0 & 0 & 0 & 0\\
      0 & 0 & 0 & 0 & 1 & 0 & 0 & 0\\
      0 & 0 & 0 & 0 & 0 & 0 & 1 & 0\\
      0 & 0 & 0 & 0 & 0 & 1 & 0 & 0\\
      0 & 0 & 0 & 0 & 0 & 0 & 0 & 1\\
    \end{pmatrix}
    \begin{pmatrix}
      1 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
      0 & 0 & 1 & 0 & 0 & 0 & 0 & 0\\
      0 & 0 & 0 & 0 & 1 & 0 & 0 & 0\\
      0 & 0 & 0 & 0 & 0 & 0 & 1 & 0\\
      0 & 1 & 0 & 0 & 0 & 0 & 0 & 0\\
      0 & 0 & 0 & 1 & 0 & 0 & 0 & 0\\
      0 & 0 & 0 & 0 & 0 & 1 & 0 & 0\\
      0 & 0 & 0 & 0 & 0 & 0 & 0 & 1\\
    \end{pmatrix} \\
    &= \begin{pmatrix}
      1 & 0 & 0 & 0 & 0 & 0 & 0 & 0\\
      0 & 0 & 0 & 0 & 1 & 0 & 0 & 0\\
      0 & 0 & 1 & 0 & 0 & 0 & 0 & 0\\
      0 & 0 & 0 & 0 & 0 & 0 & 1 & 0\\
      0 & 1 & 0 & 0 & 0 & 0 & 0 & 0\\
      0 & 0 & 0 & 0 & 0 & 1 & 0 & 0\\
      0 & 0 & 0 & 1 & 0 & 0 & 0 & 0\\
      0 & 0 & 0 & 0 & 0 & 0 & 0 & 1\\
    \end{pmatrix}
  \end{aligned}$$

At each step of the Fourier matrix decomposition, we're splitting
between even and odd entries in our input vector, i.e. we're splitting
on the *least significant bit* of the index of entries in the input
vector.  Composing those individual even/odd permutations together, we
get an overall permutation that puts our input vector into *bit
reversed* order[^1]:

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\begin{scope}[val/.style={circle,draw=black!50,fill=black!20,thick},
              bits/.style={rectangle,draw=black!50,fill=black!20,thick}]
  \node[val]  (vi0)                 {0};
  \node[val]  (vi1) [below=of vi0,yshift=9mm] {1};
  \node[val]  (vi2) [below=of vi1,yshift=9mm] {2};
  \node[val]  (vi3) [below=of vi2,yshift=9mm] {3};
  \node[val]  (vi4) [below=of vi3,yshift=9mm] {4};
  \node[val]  (vi5) [below=of vi4,yshift=9mm] {5};
  \node[val]  (vi6) [below=of vi5,yshift=9mm] {6};
  \node[val]  (vi7) [below=of vi6,yshift=9mm] {7};
  \node[bits] (bi0) [right=of vi0,xshift=-5mm] {000};
  \node[bits] (bi1) [right=of vi1,xshift=-5mm] {001};
  \node[bits] (bi2) [right=of vi2,xshift=-5mm] {010};
  \node[bits] (bi3) [right=of vi3,xshift=-5mm] {011};
  \node[bits] (bi4) [right=of vi4,xshift=-5mm] {100};
  \node[bits] (bi5) [right=of vi5,xshift=-5mm] {101};
  \node[bits] (bi6) [right=of vi6,xshift=-5mm] {110};
  \node[bits] (bi7) [right=of vi7,xshift=-5mm] {111};
  \node[bits] (bo0) [right=of bi0,xshift=5mm] {000};
  \node[bits] (bo4) [right=of bi1,xshift=5mm] {100};
  \node[bits] (bo2) [right=of bi2,xshift=5mm] {010};
  \node[bits] (bo6) [right=of bi3,xshift=5mm] {110};
  \node[bits] (bo1) [right=of bi4,xshift=5mm] {001};
  \node[bits] (bo5) [right=of bi5,xshift=5mm] {101};
  \node[bits] (bo3) [right=of bi6,xshift=5mm] {011};
  \node[bits] (bo7) [right=of bi7,xshift=5mm] {111};
  \node[val]  (vo0) [right=of bo0,xshift=-5mm] {0};
  \node[val]  (vo4) [right=of bo4,xshift=-5mm] {4};
  \node[val]  (vo2) [right=of bo2,xshift=-5mm] {2};
  \node[val]  (vo6) [right=of bo6,xshift=-5mm] {6};
  \node[val]  (vo1) [right=of bo1,xshift=-5mm] {1};
  \node[val]  (vo5) [right=of bo5,xshift=-5mm] {5};
  \node[val]  (vo3) [right=of bo3,xshift=-5mm] {3};
  \node[val]  (vo7) [right=of bo7,xshift=-5mm] {7};
  \draw (vi0) -- (bi0);  \draw (bo0) -- (vo0);
  \draw (vi1) -- (bi1);  \draw (bo1) -- (vo1);
  \draw (vi2) -- (bi2);  \draw (bo2) -- (vo2);
  \draw (vi3) -- (bi3);  \draw (bo3) -- (vo3);
  \draw (vi4) -- (bi4);  \draw (bo4) -- (vo4);
  \draw (vi5) -- (bi5);  \draw (bo5) -- (vo5);
  \draw (vi6) -- (bi6);  \draw (bo6) -- (vo6);
  \draw (vi7) -- (bi7);  \draw (bo7) -- (vo7);
  \draw (bi0.east) -- (bo0.west);  \draw (bi1.east) -- (bo1.west);
  \draw (bi2.east) -- (bo2.west);  \draw (bi3.east) -- (bo3.west);
  \draw (bi4.east) -- (bo4.west);  \draw (bi5.east) -- (bo5.west);
  \draw (bi6.east) -- (bo6.west);  \draw (bi7.east) -- (bo7.west);
\end{scope}
@@@@@
<br>

Let's denote the overall bit-reversal permutation for a vector of
length $2N$ as $B_{2N}$.

## The full algorithm

We can now write $(1)$ as

$$F_{2N} = R_{2N}^{(1)} R_{2N/2}^{(2)} R_{2N/4}^{(4)} \dots I_{2N} \;
B_{2N}$$

where

$$R_{2N}^{(1)} =
  \begin{pmatrix}
    I_N & D_N \\
    I_N & -D_N
  \end{pmatrix}$$

and $R_{2N/M}^{(M)}$ denotes the $2N \times 2N$ block-diagonal matrix
composed of $M$ copies of $R_{2N/M}^{(1)}$ along the diagonal.  Each
of the multiplications by these block-diagonal matrices involves only
a small number of floating point operations, and once we rearrange our
input vector in bit-reversed order, we can compose the matrix
multiplications to get the final FFT result (this approach,
rearranging the input vector then using this hierarchical composition
of vector elements to produce the result, is called
*decimation-in-time* in the FFT literature, because we're
progressively splitting the input, which is a vector indexed by time,
into even- and odd-indexed entries).

## Haskell implementation

To implement this algorithm in Haskell, we first deal with bit
reversal.  To begin with, we'll use the simple approach of producing
an index vector with the input vector indices in bit reversed order
then using the `backpermute` function from `Data.Vector` to reorder
the input data according to this indexing scheme.  Recall that we're
still only dealing with vectors whose lengths are powers of two, so
the number of bits we have to deal with in our indices is just $\log_2
N$: we use `testBit` and `setBit` from `Data.Bits` to swap the bit
orders around.

~~~~ {.haskell}
bitrev :: Int -> Vector Int
bitrev n =
  let nbits = log2 n
      bs = generate nbits id
      onebit i r b = if testBit i b then setBit r (nbits - 1 - b) else r
  in generate n (\i -> foldl' (onebit i) 0 bs)

log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)
~~~~

The main Cooley-Tukey algorithm takes bit-reversal reordered input
vector and iteratively applies the inverse of the unpermuted
Danielson-Lanczos decomposition to it, starting with $2 \times 2$
matrix blocks, then $4 \times 4$ and so on up to the final $N \times
N$ matrix.  This iteration is compactly represented by the `recomb`
function below, which uses the `dl` ("Danielson-Lanczos") helper
function to apply one step of the matrix multiplication (which takes
far fewer than $N^2$ floating point operations because of the special
structure of the block-diagonal matrices we are dealing with).

At each step, we slice the partially processed input vector into
sub-vectors of length corresponding to the size of the matrix blocks
at this step, then compute the matrix product of the block-diagonal
$I_n$/$D_n$ matrix and the appropriate segment of the data vector.

~~~~ {.haskell}
fft, ifft :: Vector (Complex Double) -> Vector (Complex Double)
fft = fft' 1 1
ifft v = fft' (-1) (1.0 / (fromIntegral $ length v)) v

fft' :: Int -> Double -> Vector (Complex Double) -> Vector (Complex Double)
fft' sign scale h =
  if n <= 2
  then dft' sign scale h
  else map ((scale :+ 0) *) $ recomb $ backpermute h (bitrev n)
  where n = length h
        recomb = foldr (.) id $ map dl $ iterateN (log2 n) (`div` 2) n
        dl m v = let w = omega m
                     m2 = m `div` 2
                     ds = map ((w ^^) . (sign *)) $ enumFromN 0 m2
                     doone v = let v0 = slice 0 m2 v
                                   v1 = zipWith (*) ds $ slice m2 m2 v
                               in (zipWith (+) v0 v1) ++ (zipWith (-) v0 v1)
                 in concat $ P.map doone $ slicevecs m v
        slicevecs m v = P.map (\i -> slice (i * m) m v) [0..n `div` m - 1]
~~~~

We can use the same tests as we used for the `dft` function in the
article before last.  In GHCi:

~~~~
> :load FFT-v1.hs
[1 of 1] Compiling Main             ( FFT-v1.hs, interpreted )
Ok, modules loaded: Main.
> fft $ fromList [1,0,0,0,0,0,0,0]
fromList [1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,
  1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0]
> defuzz $ fft $ generate 8 (\i ->
    sin (2 * pi * fromIntegral i / 8))}
fromList [0.0 :+ 0.0,0.0 :+ 4.0,0.0 :+ 0.0,0.0 :+ 0.0,
  0.0 :+ 0.0,0.0:+ 0.0,0.0 :+ 0.0,0.0 :+ (-4.0)]
~~~~

The results are the same as for the direct DFT implementation, and
further tests reveal that the behaviour of the FFT code is identical
to that of the DFT code (apart from small numerical differences due to
different order of computation in the two algorithms).

## Operation counts

For an input vector of length $N$, the simple DFT implementation
multiplies our input vector by the $N \times N$ Fourier matrix, an
operation requiring $N^2$ multiplication operations and a comparable
number of addition operations -- we thus expect the time behaviour of
the simple DFT to scale as $O(N^2)$.  In the case of the FFT, after
permuting the input vector into bit-reversed order, we perform $\log_2
N$ "Danielson-Lanczos" steps, each of which involves multiplication by
a block-diagonal matrix, each of whose blocks is composed of diagonal
matrices in the $I_n$/$D_n$ pattern of $(1)$.  These matrix
multiplications involve $2N$ multiplications each, plus $N$ additions.
We thus expect the time behaviour of the FFT algorithm to scale as
$O(N \log N)$.

The difference between $O(N^2)$ and $O(N \log N)$ is huge.  For $N =
1024$, we expect the FFT to be about 100 times faster than the simple
DFT matrix multiplication.  The ratio is even more in favour of the
FFT for larger $N$.  It's pretty fair to say that the invention of the
FFT is what has enabled most of the spectral and filtering
computations that are ubiquitous in digital signal processing today.

## What next?

So far, the presentation we've followed is pretty much what you can
find in more or less any signal processing textbook -- everyone
explains this powers-of-two algorithm because it's nice and simple.
We're going to be a lot more ambitious than that though!  After
looking at a simple application of this code in the next article,
we're going to venture into deeper waters, as we begin exploring
what's needed to lift the "powers of two" restriction on input vector
lengths.  Conceptually, this turns out to be a relatively
straightforward generalisation of the powers-of-two algorithm, but the
details are quite delicate and quite interesting.


[^1]: Because of the importance of the FFT, some digital signal
      processor architectures even provide a special bit-reversed
      addressing mode!  (For example,
      [Analog's 219x series](http://www.analog.com/static/imported-files/processor_manuals/4996472923699ADSP_219x_ISR__web_.pdf)
      -- see page 6-16.)
