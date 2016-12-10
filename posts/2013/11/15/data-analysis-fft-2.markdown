---
author: Ian
tags: data-analysis,haskell
title: "Haskell FFT 2: Matrix factorisation"
published: 2013-11-15 07:46:37
---

[The code from this article is available as a [Gist](https://gist.github.com/7480227).]

In the
[first article in this series](/posts/2013/11/13/data-analysis-fft-1/index.html),
we considered a set of $N$ data values from a function $h(t)$ sampled
at a regular interval $\Delta$:

$$h_n = h(n \Delta) \qquad n = 0, 1, 2, \dots, N-1.$$

and defined the discrete Fourier transform of this set of data values
as

$$H_n = \sum_{k=0}^{N-1} h_k e^{2\pi i k n/N} \qquad n = 0, 1, 2,
\dots, N-1. \qquad (1)$$

The original Cooley and Tukey fast Fourier transform algorithm is
based on the observation that, for even $N$, the DFT calculation can
be broken down into two subsidiary DFTs, one for elements of the input
vector with even indexes and one for elements with odd indexes:

$$\begin{aligned}
    H_n &= \sum_{k=0}^{N-1} h_k e^{2\pi i k n/N} \\
    &= \sum_{k=0}^{N/2-1} h_{2k} e^{2\pi i (2k) n/N} + \sum_{k=0}^{N/2-1}
    h_{2k+1} e^{2\pi i (2k+1) n/N} \\
    &= \sum_{k=0}^{N/2-1} h_{2k} e^{2\pi i k n/(N/2)} + \omega^n
    \sum_{k=0}^{N/2-1} h_{2k+1} e^{2\pi i k n/(N/2)} \\
    &= H^e_n + \omega_N^n H^o_n,
  \end{aligned}$$

where we write $\omega_N = e^{2\pi i / N}$, and where $H^e_n$ is the
$n$th component of the DFT of the evenly indexed elements of $h$ and
$H^o_n$ is the $n$th component of the DFT of the oddly indexed
elements of $h$.  This decomposition is sometimes called the
*Danielson-Lanczos lemma*.

In this article, we're going to look in detail at this decomposition
to understand just how it works and how it will help us to calculate
the DFT in an efficient way.  In the next article, we'll see how to
put a series of these even/odd decomposition steps together to form a
full DFT.

<!--MORE-->

## Fourier matrix decomposition

The DFT calculation $(1)$ that we implemented in the last article can
be viewed as a matrix multiplication.  For a complex input vector of
length $N$, we can write $(1)$ as

$$\begin{pmatrix}
    H_1 \\
    H_2 \\
    \vdots \\
    H_N
  \end{pmatrix} =
  \begin{pmatrix}
    1      & 1              & 1              & \dots & 1 \\
    1      & \omega_N       & \omega_N^2      & \dots & \omega_N^{N-1} \\
    1      & \omega_N^2     & \omega_N^4      & \dots & \omega_N^{2(N-1)} \\
    \vdots & \vdots         & \vdots         & \ddots & \vdots \\
    1      & \omega_N^{N-1} & \omega_N^{2(N-1)} & \dots & \omega_N^{(N-1)^2}
  \end{pmatrix}
  \begin{pmatrix}
    h_1 \\
    h_2 \\
    \vdots \\
    h_N
  \end{pmatrix} \qquad(2)$$

We'll call the matrix in $(2)$ the *Fourier matrix*, $F_N$.

So far, since $F_N$ is a dense matrix, it seems as though the DFT will
require $O(N^2)$ floating point operations.  The *fast Fourier
transform* does much better than this, using a clever decomposition of
$F_N$ to calculate the DFT in $O(N \log N)$ operations.  This
decomposition and the algorithm that it leads to were introduced to
the world by Cooley and Tukey in the 1960s[^1] although many of the
ideas had been developed earlier.  We're going to write some simple
Haskell code to understand how this decomposition works.  In this
article, we'll consider only vectors with $N$ a power of two.  We'll
deal with the general case later.  (The Cooley-Tukey algorithm is now
one of a group of algorithms called fast Fourier transforms.  We may
look at another example later on for dealing with vectors of prime
length.)

Before we show an example of the FFT decomposition, let's look at a
couple of instances of $F_N$:

$$F_4 = \begin{pmatrix}
1 & 1 & 1 & 1\\
1 & \omega_{4} & -1 & \omega_{4}^{3}\\
1 & -1 & 1 & -1\\
1 & \omega_{4}^{3} & -1 & \omega_{4}\\
\end{pmatrix}
\qquad
F_8 = \begin{pmatrix}
1 & 1 & 1 & 1 & 1 & 1 & 1 & 1\\
1 & \omega_{8} & \omega_{4} & \omega_{8}^{3} & -1 & \omega_{8}^{5} &
\omega_{4}^{3} & \omega_{8}^{7}\\
1 & \omega_{4} & -1 & \omega_{4}^{3} & 1 & \omega_{4} & -1 & \omega_{4}^{3}\\
1 & \omega_{8}^{3} & \omega_{4}^{3} & \omega_{8} & -1 & \omega_{8}^{7}
& \omega_{4} & \omega_{8}^{5}\\
1 & -1 & 1 & -1 & 1 & -1 & 1 & -1\\
1 & \omega_{8}^{5} & \omega_{4} & \omega_{8}^{7} & -1 & \omega_{8} &
\omega_{4}^{3} & \omega_{8}^{3}\\
1 & \omega_{4}^{3} & -1 & \omega_{4} & 1 & \omega_{4}^{3} & -1 &
\omega_{4}\\
1 & \omega_{8}^{7} & \omega_{4}^{3} & \omega_{8}^{5} & -1 &
\omega_{8}^{3} & \omega_{4} & \omega_{8}\\
\end{pmatrix}$$

Clearly, although there are $N^2$ entries in each matrix, there can
only be $N$ distinct powers of $\omega_N$, so there is a lot of
redundancy in the entries in the Fourier matrix and in the operations
of the naïve DFT algorithm.  Further, $\omega_{2N}^2 = \omega_N$, so
there is a relationship between the entries in Fourier matrices of
different orders.  This second observation turns out to be critical
for the Cooley-Tukey algorithm.

As a first example, let's decompose $F_4$:

$$F_4 =
\begin{pmatrix}
1 & 1 & 1 & 1\\
1 & \omega_{4} & -1 & \omega_{4}^{3}\\
1 & -1 & 1 & -1\\
1 & \omega_{4}^{3} & -1 & \omega_{4}\\
\end{pmatrix} =
\begin{pmatrix}
1 & 0 & 1 & 0\\
0 & 1 & 0 & \omega_{4}\\
1 & 0 & -1 & 0\\
0 & 1 & 0 & -\omega_{4}\\
\end{pmatrix}
\begin{pmatrix}
1 & 1 & 0 & 0\\
1 & -1 & 0 & 0\\
0 & 0 & 1 & 1\\
0 & 0 & 1 & -1\\
\end{pmatrix}
\begin{pmatrix}
1 & 0 & 0 & 0\\
0 & 0 & 1 & 0\\
0 & 1 & 0 & 0\\
0 & 0 & 0 & 1\\
\end{pmatrix}$$

which can be rewritten as

$$F_4 =
\begin{pmatrix}
  I_2 & D_2 \\
  I_2 & -D_2
\end{pmatrix}
\begin{pmatrix}
  F_2 & \\
      & F_2
\end{pmatrix}
P \qquad (3)$$

where $P$ is a permutation matrix that picks out first the even then
the odd elements of the input vector $h$, $F_2$ is the $2 \times 2$
Fourier matrix, $I_2$ the $2 \times 2$ unit matrix and $D_2$ a $2
\times 2$ diagonal matrix whose diagonal entries are powers of
$\omega_4$.

## A "toy algebra system"

Verifying identities like $(3)$ by hand is extremely tedious, so let's
write some Haskell code to help.  This code will demonstrate a
technique that's useful for a lot of algebraic calculations.  Instead
of using a completely general computer algebra system (Mathematica or
Maple, for example), we'll represent a restricted set of algebraic
entities as Haskell data types.  The main benefit of doing this is
that it allows us to encode preconceptions that we have about the
calculations we're doing directly in the code.  If those
preconceptions turn out not to be correct, or if we extend the domain
of our calculations beyond the examples we use to construct those
preconceptions, we can get immediate feedback when our ideas turn out
to be wrong.

From a little bit of manual calculation, it seems as though we're
going to need to deal with matrices with entries that are either zero
or powers of roots of unity.  Also, it seems as though all of the
matrix multiplications we need to do result in dot products between
matrix rows and columns with only a single non-zero entry.  Here's an
encoding of these ideas in Haskell:

~~~~ {.haskell}
data Sign = P | M deriving (Eq, Show)

data Entry = Z | O Sign | W Sign Int Int deriving Eq

negS :: Sign -> Sign
negS P = M
negS M = P

multS :: Sign -> Sign -> Sign
multS s1 s2 | s1 == s2  = P
            | otherwise = M

instance Num Entry where
  abs _ = error "Absolute value not appropriate"
  signum _ = error "Sign not appropriate"
  negate (O s) = O (negS s)
  negate (W s n p) = W (negS s) n p
  Z + x = x
  x + Z = x
  _ + _ = error "Adding two non-zero terms!"
  x - y = x + negate y
  Z * x = Z
  x * Z = Z
  (O s1) * (O s2) = O (multS s1 s2)
  (O s1) * (W s2 n p) = W (multS s1 s2) n p
  (W s1 n p) * (O s2) = W (multS s1 s2) n p
  (W s1 n1 p1) * (W s2 n2 p2)
    | n1 == n2 = W (multS s1 s2) n1 ((p1+p2) `mod` n1)
    | gcd n1 n2 == 1 = error "Multiplying different omegas"
    | otherwise = if n1 > n2
                  then W (multS s1 s2) n1 ((p1+p2*(n1`div`n2))`mod`n1)
                  else W (multS s1 s2) n2 ((p2+p1*(n2`div`n1))`mod`n2)
  fromInteger n
    | n == 0 = Z
    | n == 1 = O P
    | n == -1 = O M
    | otherwise = error "Invalid integer"

normalise :: Entry -> Entry
normalise = fix norm
  where norm (W s _ 0) = O s
        norm (W M n k) | even n = W P n ((k + n `div` 2) `mod` n)
                       | otherwise = W M n (k `mod` n)
        norm (W s 2 k) | odd k = O s * O M
                       | otherwise = O s
        norm (W P n k) | even n && even k = W P (n `div` 2) (k `div` 2)
                       | n == 2 * k = O M
                       | otherwise = W P n (k `mod` n)
        norm x = x
        fix f x = let xs = iterate f x
                  in fst $ head $ dropWhile (\(x,y) -> x/=y) $ zip xs (tail xs)
~~~~

We have an `Entry` data type representing values of zero (constructor
`Z`), plus or minus one (constructor `O`) or powers of roots of unity
(constructor `W`).  We explicitly encode the sign of terms using a
`Sign` data type, so that

$$\begin{gathered}
  \mathtt{O P} \to +1 \qquad
  \mathtt{O M} \to -1 \\
  \mathtt{W P n k} \to \omega_{\mathtt{n}}^{\mathtt{k}} \qquad
  \mathtt{W M n k} \to -\omega_{\mathtt{n}}^{\mathtt{k}}
\end{gathered}$$

We make `Entry` an instance of `Num` in a way that conforms to our
preconceptions about how the matrix entries we have to deal with are
computed: we raise an error if an attempt is made to add two non-zero
terms, for instance.  We also perform normalisation of products of
powers of different roots of unity so that, for example, we correctly
determine that $\omega_3 \omega_6 = \omega_6^3$.  There is a separate
`normalise` function to perform other canonicalisation of matrix
entries:

$$\begin{gathered}
    \pm \omega_N^0 = \pm 1 \qquad
    \omega_2 = -1 \qquad
    \omega_{2N}^2 = \omega_N \\
    -\omega_{2N}^k = \omega_{2N}^{(k+N) \,\mathrm{mod}\, 2N}
\end{gathered}$$

Armed with these definitions of matrix entries, we can define the
matrix operations that we need:

~~~~ {.haskell}
newtype Mat = Mat [[Entry]] deriving Eq

normMat :: Mat -> Mat
normMat (Mat rs) = Mat $ map (map normalise) rs

transpose :: Mat -> Mat
transpose (Mat xs) = Mat (L.transpose xs)

dot :: [Entry] -> [Entry] -> Entry
dot v1 v2 = L.sum $ L.zipWith (*) v1 v2

(%*%) :: Mat -> Mat -> Mat
(Mat m1) %*% (Mat m2) =
  normMat $ Mat [[r `dot` c | c <- L.transpose m2] | r <- m1]

diag :: [Entry] -> Mat
diag ds = let bigN = length ds
              zs = replicate (bigN - 1) Z
              zsplits = map (flip splitAt zs) [0..bigN-1]
              rows = zipWith (\d (b, a) -> b ++ [d] ++ a) ds zsplits
          in Mat rows

unit :: Int -> Mat
unit bigN = diag $ replicate bigN (O P)
~~~~

We do this using a very simple nested list matrix representation,
since we don't really care about efficiency here -- this code is just
for exploration and understanding.  We've imported the `Data.List`
module qualified as `L`, which is where `L.transpose`, `L.sum`,
etc. come from.  We have simple functions to normalise the entries in
a matrix using the entry normalisation function from above, to
calculate matrix products (`%*%`) and to generate diagonal matrices
with specified elements, as well as unit matrices of a given size.

Using this matrix representation, we can now construct the components
of the FFT decomposition:

~~~~ {.haskell}
-- Basic Fourier matrix.
fourier :: Int -> Mat
fourier bigN =
  normMat $ Mat [[(W P bigN 1)^(n*k) | k <- [0..bigN-1]] | n <- [0..bigN-1]]

-- Doubled-up half-size Fourier matrices.
fourierDouble :: Int -> Mat
fourierDouble n2 =
  let n = n2 `div` 2
      zs = replicate n Z
      Mat f = fourier n
  in Mat $ (map (++ zs) f) ++ (map (zs ++) f)

-- Even/odd permutation matrix.
perm :: Int -> Mat
perm n2 = let n = n2 `div` 2
              evens = ((O P) : replicate (n2-1) Z) :
                      map (take n2 . (Z :) . (Z :)) evens
              odds = map (take n2 . (Z :)) evens
          in Mat $ take n evens ++ take n odds

-- I + D matrix.
idMatrix :: Int -> Mat
idMatrix n2 = let n = n2 `div` 2
                  Mat i = unit n
                  ds = (O P) : [W P n2 i | i <- [1..n-1]]
                  Mat d = diag ds
                  Mat nd = diag $ map negate ds
              in Mat $ (zipWith (++) i d) ++ (zipWith (++) i nd)
~~~~

First, the `fourier` function calculates the full Fourier matrix -- in
GHCi:

~~~~
> fourier 4
  1     1     1     1
  1    W4^1  -1    W4^3
  1    -1     1    -1
  1    W4^3  -1    W4^1
~~~~

(There are `Show` instances for matrices and entries to produce
readable representations -- the code isn't shown here, but is
available in the repository.)  There are also functions to generate
the nested double Fourier matrix, the even-odd permutation matrix and
the unit matrix/diagonal matrix composite that appears first in
$(3)$:

~~~~
> fourierDouble 4
  1     1     0     0
  1    -1     0     0
  0     0     1     1
  0     0     1    -1
> perm 4
  1     0     0     0
  0     0     1     0
  0     1     0     0
  0     0     0     1
> idMatrix 4
  1     0     1     0
  0     1     0    W4^1
  1     0    -1     0
  0     1     0   -W4^1
~~~~

We can then check that the decomposition is correct:

~~~~
> idMatrix 4 %*% (fourierDouble 4 %*% perm 4) == fourier 4
True
~~~~

What have we done here?  We've constructed a "toy algebra system" that
deals only with the algebraic entities that we believe are going to be
involved in the calculations we're interested in.  This restricted
system is just enough for us to experiment with and check the Fourier
matrix decomposition.  Of course, this decomposition is just a
restatement of the Danielson-Lanczos lemma in terms of matrices, so we
can directly prove it algebraically, but the Haskell code provides us
with a little playground to help our understanding of what's going on.
We're lead to the conclusion that

$$F_{2N} =
  \begin{pmatrix}
    I_N & D_N \\
    I_N & -D_N
  \end{pmatrix}
  \begin{pmatrix}
    F_N & \\
        & F_N
\end{pmatrix}
  P_{2N}$$

where $D_N = \mathrm{diag}(1, \omega_{2N}, \dots, \omega_{2N}^{N-1})$
and $P_{2N}$ is the $2N \times 2N$ even-odd permutation matrix.  (Note
that this decomposition doesn't rely on $N$ being even -- although
we're only considering input vectors whose length is a power of two at
the moment, this generality will be important later on.)

In fact, in the powers-of-two length case, these decompositions aren't
too onerous to calculate and check by hand.  When we come to consider
the general case (data vector lengths that aren't powers of two), the
algebra is quite a lot more tedious (and you won't find the answer in
most textbooks...), and this code will prove invaluable.

Next time, we'll see how we can use the knowledge we've gained here to
implement the full Cooley-Tukey algorithm for data vectors whose
lengths are powers of two.

[^1]: Cooley, James W.; Tukey, John W. (1965). "An algorithm for the
      machine calculation of complex Fourier series". *Math. Comput.*
      **19**:
      297-301. [doi:10.2307/2003354](http://dx.doi.org/10.2307%2F2003354)
