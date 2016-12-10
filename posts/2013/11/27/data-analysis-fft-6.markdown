---
author: Ian
tags: data-analysis,haskell
title: "Haskell FFT 6: Implementing the Mixed-Radix FFT"
published: 2013-11-27 18:52:39
---

[The code from this article is available as a [Gist](https://gist.github.com/ian-ross/7682237)]

In this article, we're going to implement the full "sorted prime
factorisation mixed-radix decimation-in-time Cooley-Tukey FFT".
That's a bit of a mouthful, and you might want to review some of the
algebra in the [last article](/blog/posts/2013/11/23/data-analysis-fft-5.html) to remind
yourself of some of the details of how it works.

The approach we're going to take to exploring the implementation of
this mixed-radix FFT is to start with a couple of the smaller and
simpler parts (prime factor decomposition, digit reversal ordering),
then to look at the top-level driver function, and finally to look at
the generalised Danielson-Lanczos step, which is the most complicated
part of things.  This code has an unavoidably large number of moving
parts (which is probably why this algorithm is rarely covered in
detail in textbooks!), so we'll finish up by writing some QuickCheck
properties to make sure that everything works[^1].

<!--MORE-->

Let's start with a couple of type synonyms to make things more
readable (typing `Vector (Complex Double)` or `Vector (Vector (Complex
Double))` gets old quickly):

~~~~ {.haskell}
type CD = Complex Double
type VCD = Vector CD
type VVCD = Vector (Vector CD)
~~~~

Calculating the prime factors for the decomposition of the input
vector length is simple:

~~~~ {.haskell}
-- From Haskell wiki.
primes :: [Int]
primes = 2 : primes'
  where primes' = sieve [3, 5 ..] 9 primes'
        sieve (x:xs) q ps@ ~(p:t)
          | x < q = x : sieve xs q ps
          | True  =     sieve [x | x <- xs, rem x p /= 0] (P.head t^2) t

-- Simple prime factorisation: small factors only; largest/last factor
-- picked out as "special".
factors :: Int -> (Int, Vector Int)
factors n = let (lst, rest) = go n primes in (lst, fromList rest)
  where go cur pss@(p:ps)
          | cur == p         = (p, [])
          | cur `mod` p == 0 = let (lst, rest) = go (cur `div` p) pss
                               in (lst, p : rest)
          | otherwise        = go cur ps
~~~~

The only clever thing here is the efficient prime sieve used to
generate an infinite list of primes: this is taken directly from the
[Haskell wiki page about primes](http://www.haskell.org/haskellwiki/Primes).
We treat the "last" prime factor specially, since it's going to give
us the size of the DFT calculations we're going to do at the "bottom"
of the recursive decomposition.

It's probably worth saying a word or two about this special treatment
of the last factor.  Instead of treating the last factor specially, we
could do one more decomposition step to end up with $N$ length-1
subvectors.  The FFT of a length-1 vector is just the identity[^2],
and we could then compose those length-1 FFT results just as for the
other levels of the decomposition.  This is precisely what we did for
the powers-of-two case: the `id` in the definition of the `recomb`
function in the FFT calculation in
[article 3](/blog/posts/2013/11/18/data-analysis-fft-3.html) is just this
length-1 identity transform.  In fact, if you work through the
calculation, this lowest-level of the FFT decomposition is exactly
equivalent to the naïve DFT that we're going to use instead.  The main
motivation for treating this last factor specially and explicitly
pulling out the simple DFT is that we may want to replace the "bottom
level" DFT with a more sophisticated algorithm for prime-length
vectors later on, and pulling it out here makes this more convenient.

A useful way to think of this prime factor decomposition is as a sort
of "generalised place value" scheme for representing numbers.  For
example: if we want to represent 32 different values, we can do this
exactly with five binary digits; if we want to represent 30 different
values, they don't quite "fit" into a binary scheme, but if we write
numbers as $x_5 y_3 z_2$ where $x \in \{ 0, 1, 2, 3, 4 \}$, $y \in \{
0, 1, 2 \}$, $z \in \{ 0, 1 \}$ and $z$ represents "units" in our
place value system, $y$ represents "threes" and $x$ "sixes", then we
can write the numbers zero to thirty as:

<div style="width: 100%;">
<table style="margin-left: auto; margin-right: auto;">
<colgroup span=6 width=75></colgroup>
<tr>
  <td> $0_5 0_3 0_2$ </td> <td> $0_5 0_3 1_2$ </td> <td> $0_5 1_3 0_2$ </td>
  <td> $0_5 1_3 1_2$ </td> <td> $0_5 2_3 0_2$ </td> <td> $0_5 2_3 1_2$ </td>
</tr>
<tr>
  <td> $1_5 0_3 0_2$ </td> <td> $1_5 0_3 1_2$ </td> <td> $1_5 1_3 0_2$ </td>
  <td> $1_5 1_3 1_2$ </td> <td> $1_5 2_3 0_2$ </td> <td> $1_5 2_3 1_2$ </td>
</tr>
<tr>
  <td> $2_5 0_3 0_2$ </td> <td> $2_5 0_3 1_2$ </td> <td> $2_5 1_3 0_2$ </td>
  <td> $2_5 1_3 1_2$ </td> <td> $2_5 2_3 0_2$ </td> <td> $2_5 2_3 1_2$ </td>
</tr>
<tr>
  <td> $3_5 0_3 0_2$ </td> <td> $3_5 0_3 1_2$ </td> <td> $3_5 1_3 0_2$ </td>
  <td> $3_5 1_3 1_2$ </td> <td> $3_5 2_3 0_2$ </td> <td> $3_5 2_3 1_2$ </td>
</tr>
<tr>
  <td> $4_5 0_3 0_2$ </td> <td> $4_5 0_3 1_2$ </td> <td> $4_5 1_3 0_2$ </td>
  <td> $4_5 1_3 1_2$ </td> <td> $4_5 2_3 0_2$ </td> <td> $4_5 2_3 1_2$ </td>
</tr>
</table>
</div>

This prime factor digit approach gives us a kind of customised place
value scheme for any value, and allows us to represent the
generalisation of the even/odd splitting in the powers-of-two FFT as a
removal of the lowest order digit in our place value representation --
recall that the even/odd splitting resulted from taking the lowest
order bit from the input data length each time we split the Fourier
matrix using the Danielson-Lanczos lemma.

As for the powers-of-two case, where we needed to reorder our input
data in bit-reversed order before doing the Danielson-Lanczos steps to
get our FFT result, in the general case, we need a sort of "sorted
prime factor decomposition digit reversal order".  That's a bit long
to type, so I'm just going to call it "digit reversed order" with the
understanding that we're only dealing with this sorted prime factor
decomposition for the moment.

Here's a simple example of this digit reversal ordering:

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\begin{scope}[val/.style={circle,draw=black!50,fill=black!20,thick},
              digs/.style={rectangle,draw=black!50,fill=black!20,thick}]
  \node[val]  (vi0)                 {0};
  \node[val]  (vi1) [below=of vi0,yshift=9mm] {1};
  \node[val]  (vi2) [below=of vi1,yshift=9mm] {2};
  \node[val]  (vi3) [below=of vi2,yshift=9mm] {3};
  \node[val]  (vi4) [below=of vi3,yshift=9mm] {4};
  \node[val]  (vi5) [below=of vi4,yshift=9mm] {5};
  \node[digs] (di0) [right=of vi0,xshift=-5mm] {$0_3 0_2$};
  \node[digs] (di1) [right=of vi1,xshift=-5mm] {$0_3 1_2$};
  \node[digs] (di2) [right=of vi2,xshift=-5mm] {$1_3 0_2$};
  \node[digs] (di3) [right=of vi3,xshift=-5mm] {$1_3 1_2$};
  \node[digs] (di4) [right=of vi4,xshift=-5mm] {$2_3 0_2$};
  \node[digs] (di5) [right=of vi5,xshift=-5mm] {$2_3 1_2$};
  \node[digs] (do0) [right=of di0,xshift=5mm] {$0_2 0_3$};
  \node[digs] (do3) [right=of di1,xshift=5mm] {$1_2 0_3$};
  \node[digs] (do1) [right=of di2,xshift=5mm] {$0_2 1_3$};
  \node[digs] (do4) [right=of di3,xshift=5mm] {$1_2 1_3$};
  \node[digs] (do2) [right=of di4,xshift=5mm] {$0_2 2_3$};
  \node[digs] (do5) [right=of di5,xshift=5mm] {$1_2 2_3$};
  \node[val]  (vo0) [right=of do0,xshift=-5mm] {0};
  \node[val]  (vo4) [right=of do4,xshift=-5mm] {4};
  \node[val]  (vo2) [right=of do2,xshift=-5mm] {2};
  \node[val]  (vo1) [right=of do1,xshift=-5mm] {1};
  \node[val]  (vo5) [right=of do5,xshift=-5mm] {5};
  \node[val]  (vo3) [right=of do3,xshift=-5mm] {3};
  \draw (vi0) -- (di0);  \draw (do0) -- (vo0);
  \draw (vi1) -- (di1);  \draw (do1) -- (vo1);
  \draw (vi2) -- (di2);  \draw (do2) -- (vo2);
  \draw (vi3) -- (di3);  \draw (do3) -- (vo3);
  \draw (vi4) -- (di4);  \draw (do4) -- (vo4);
  \draw (vi5) -- (di5);  \draw (do5) -- (vo5);
  \draw (di0.east) -- (do0.west);  \draw (di1.east) -- (do1.west);
  \draw (di2.east) -- (do2.west);  \draw (di3.east) -- (do3.west);
  \draw (di4.east) -- (do4.west);  \draw (di5.east) -- (do5.west);
\end{scope}
@@@@@
<br>

This case is particular simple because it represents an even/odd split
(we're splitting off a factor of two from the input data of length
$N=6$), which we can see quite clearly from the pattern -- even
indexed elements and odd indexed elements are segregated by the
rearrangement.

Although we could use a similar "place value" approach to reordering
the data as we used for the $2^N$ FFT case, it turns out to be simpler
to just compose appropriate permutations of indexes into the input
vector.  The bit-reversal approach gets a little confusing here
because we need to keep track of the place values of each digit, and
they change when we swap the digit order.  The permutation code is a
lot simpler to understand:

~~~~ {.haskell}
-- Generate digit reversal permutation using elementary "modulo"
-- permutations: last digit is not permuted to match with using a
-- simple DFT at the "bottom" of the overall algorithm.
digrev :: Int -> (Int, Vector Int) -> Vector Int
digrev n (lastf, fs) = foldl1' (%.%) $ map dupperm subperms
  where
    -- Sizes of the individual permutations that we need, one per
    -- factor.
    sizes = scanl div n fs

    -- Partial sub-permutations, one per factor.
    subperms = reverse $ zipWith perm sizes fs

    -- Duplicate a sub-permutation to fill the required vector length.
    dupperm p =
      let sublen = length p
          shift di = map (+(sublen * di)) p
      in concatMap shift $ enumFromN 0 (n `div` sublen)

    -- Generate a single "modulo" permutation.
    perm n fac = concatMap doone $ enumFromN 0 fac
      where n1 = n `div` fac
            doone i = generate n1 (\j -> j * fac + i)

    -- Composition of permutations.
    (%.%) :: Vector Int -> Vector Int -> Vector Int
    p1 %.% p2 = backpermute p2 p1
~~~~

We construct basic "modulo" permutations, the generalisation to
arbitrary factors of even/odd permutations, then compose them to form
the final vector of indexes giving the digit reversed reordering of
the input.  The only extra wrinkle is that we need to duplicate
sub-permutations for each lower recursive level of the Fourier matrix
decomposition: if the input vector length is $30 = 2 \times 3 \times
5$, for instance, we split the original input vector into two vectors
of length 15; we then split each of those two sub-vectors into three,
so we need to duplicate the modulo-3 permutation to treat each of the
sub-vectors in turn.  This approach is relatively efficient, but it
doesn't really matter anyway since we eventually plan to only ever do
this computation once at compile time.  It doesn't need to be
optimised too much: it just needs to work!

Let's step back a little now and look at the main driver function for
the full mixed-radix FFT:

~~~~ {.haskell}
-- FFT and inverse FFT drivers.
fft, ifft :: VCD -> VCD
fft = fft' 1 1
ifft v = fft' (-1) (1.0 / (fromIntegral $ length v)) v

-- Mixed-radix decimation-in-time Cooley-Tukey FFT.
fft' :: Int -> Double -> VCD -> VCD
fft' sign scale h = if n == 1 then h
                    else if null fs
                         then dft' sign scale h
                         else map ((scale :+ 0) *) fullfft
  where
    -- Full vector length.
    n = length h

    -- Factorise input vector length.
    splitfs@(lastf, fs) = factors n

    -- Generate sizing information for Danielson-Lanczos step.
    wfacs = map (n `div`) $ scanl (*) 1 fs
    dlinfo = zip wfacs fs

    -- Compose all Danielson-Lanczos steps and "final" DFT.
    recomb = foldr (.) (mdft lastf) $ map (dl sign) dlinfo

    -- Apply Danielson-Lanczos steps and "final" DFT to digit reversal
    -- ordered input vector.
    fullfft = recomb $ backpermute h (digrev n splitfs)

    -- Multiple DFT for "bottom" of algorithm.
    mdft :: Int -> VCD -> VCD
    mdft factor h = concatMap (dft' sign 1) $ slicevecs factor h
~~~~

This has a lot in common with the driver for the powers-of-two
algorithm, but it's complicated slightly by the need to keep track of
the factors we're using to decompose the input vector length.  The key
step is the definition of `recomb`, where we compose the generalised
Danielson-Lanczos steps for the Fourier matrix decomposition
(represented by the function `dl`, which we'll get to in a moment),
along with a "multiple" DFT to deal with the last level of the
decomposition ("multiple" in the sense that it's a size $M$ DFT
repeated $N/M$ times to cover each of the decomposed sub-vectors in
the input).

The generalised Danielson-Lanczos step itself looks like this:

~~~~ {.haskell}
-- Single Danielson-Lanczos step: process all duplicates and
-- concatenate into a single vector.
dl :: Int -> (Int, Int) -> VCD -> VCD
dl sign (wfac, split) v = concatMap doone $ slicevecs wfac v
  where
    -- Overall vector length.
    n = length v

    -- Size of each diagonal sub-matrix.
    ns = wfac `div` split

    -- Root of unity for the size of diagonal matrix we need.
    w = omega $ sign * wfac

    -- Basic diagonal entries for a given column.
    ds c = map ((w^^) . (c *)) $ enumFromN 0 ns

    -- Twiddled diagonal entries in row r, column c (both
    -- zero-indexed), where each row and column if a wfac x wfac
    -- matrix.
    d r c = map (w^(ns*r*c) *) (ds c)

    -- Process one duplicate by processing all rows and concatenating
    -- the results into a single vector.
    doone v = concatMap single $ enumFromN 0 split
      where vs :: VVCD
            vs = slicevecs ns v
            -- Multiply a single block by its appropriate diagonal
            -- elements.
            mult :: Int -> Int -> VCD
            mult r c = zipWith (*) (d r c) (vs!c)
            -- Multiply all blocks by the corresponding diagonal
            -- elements in a single row.
            single :: Int -> VCD
            single r = foldl1' (zipWith (+)) $ map (mult r) $ enumFromN 0 split
~~~~

This is kind of complicated, mostly because it involves keeping track
of sub-matrices and sub-vectors all over the place, and it's where the
tricky "twiddle factor" calculation is done.  To try to make things as
clear as possible, the `d` function is used to calculate the
$D_j^{(i)}$ sub-matrices from the $I+D$ matrix explicitly as a
function of the row and column of the sub-matrix in the full $I+D$
matrix.  This corresponds exactly to how we laid this calculation out
in the
[previous article](/blog/posts/2013/11/23/data-analysis-fft-5.html)
(e.g. the calculations of the decompositions of $F_6$ and $F_9$
there).  As for the "bottom level" DFT calculation, we need to
duplicate the multiplication by the $I+D$ matrix across each of the
sub-vectors we're dealing with at the current level of the
decomposition.  For example, if we're dealing with a vector of length
30, we split two ways at the first level, giving us two sub-vectors of
length 15, *each of which* then has to be split three ways.

And that's it.  "That's it?", you say, in horror.  "How do you know
that this works?"  There really is quite a lot going on in this
algorithm.  First, we need to trust that our prime factor
decomposition of the Fourier matrix is correct.  Hopefully, some
experimentation with the "toy algebra system" code in the previous
article is enough to gain some confidence on this point.  We can build
up the full decomposition of a Fourier matrix bit by bit, multiply
them all together and compare to the full DFT matrix.  And that does
all work.  But then we have prime factorisation, digit reversal
ordering, the top-level driver and the generalised Danielson-Lanczos
computation.  There's a lot that could go wrong.

Up-front thinking and algebra only gets us so far on this second
point.  In algorithms like this one, where there are multiple sizes of
things floating around, there are different $\omega_N$ factors at
different levels of the Fourier matrix decomposition, there are these
tricksy twiddle factors, and so on, we really do need to do some
testing.  What are good tests in this case?  First, we have the naïve
DFT algorithm to serve as a benchmark.  In all cases, the results from
our FFT algorithm should be identical to those from the simple DFT,
apart from minor differences due to the ordering of floating point
operations.  Second, the inverse FFT really should be the inverse of
the forward FFT!  The only difference between the forward and reverse
versions of the algorithm is the overall scaling of the result and the
sign of the exponent in the $\omega_N$ factors.  Sign errors are
notoriously easy to make, so this might be a good test -- if we've
made a sign error somewhere in the exponents, it's unlikely that our
inverse FFT will truly be the inverse of the forward FFT.

Both of these tests are stated in terms of properties that our
algorithm should have for all input vectors.  During the development
phase of an algorithm like this, we might try our code with some
simple "hand-made" input vectors, but if you only do that, it's quite
easy to convince yourself that your code works perfectly when in fact
it *only* works for your simple test data.  For an algorithm like this
one, before we declare it to be working, we should turn to QuickCheck.
Here are the definition of QuickCheck properties for the FFT vs. DFT
(`prop_dft_vs_fft`) and inverse FFT (`prop_ifft`) tests, along with an
`Arbitrary` instance for `Vector (Complex Double)`, which we need to
use the tests:

~~~~ {.haskell}
-- Clean up number display.
defuzz :: VCD -> VCD
defuzz = map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x

-- Check FFT against DFT.
check :: VCD -> (Double, VCD)
check v = let diff = defuzz $ zipWith (-) (fft v) (dft v)
          in (maximum $ map magnitude diff, diff)

-- Check FFT-inverse FFT round-trip.
icheck :: VCD -> (Double, VCD)
icheck v = let diff = defuzz $ zipWith (-) v (ifft $ fft v)
           in (maximum $ map magnitude diff, diff)

-- QuickCheck property for FFT vs. DFT testing.
prop_dft_vs_fft (v :: VCD) = fst (check v) < 1.0E-6

-- QuickCheck property for inverse FFT round-trip testing.
prop_ifft (v :: VCD) = maximum (map magnitude diff) < 1.0E-6
  where diff = zipWith (-) v (ifft $ fft v)

-- Non-zero length arbitrary vectors.
instance Arbitrary VCD where
  arbitrary = fromList <$> listOf1 arbitrary
~~~~

Once we have these things, we can do this in GHCi:

~~~~
> :load FFT-v2.hs
[1 of 1] Compiling Main             ( FFT-v2.hs, interpreted )
Ok, modules loaded: Main.
> import Test.QuickCheck
> quickCheck prop_dft_vs_fft}
+++ OK, passed 100 tests.
> quickCheck prop_ifft
+++ OK, passed 100 tests.
~~~~

Of course, random testing of this sort is no *proof* that our code is
correct, but it provides strong confidence that things are working.

In the next article, we'll take a bit of a breather to summarise where
we've got to, and to lay out what we're going to do next.

[^1]: An admission: it took me quite a while to get everything working
      correctly here.  The powers-of-two FFT took about an hour to
      write an initial version and perhaps another hour to tidy things
      up.  The mixed-radix FFT took several hours of off-line thinking
      time (mostly walking my dog), a couple of hours to put together
      an initial (non-working) version, then several more hours of
      debugging time.

[^2]: What else could it be?  The DFT is a linear operation, and for a
      length-1 vector $v$, $(\mathcal{F}^{-1} \cdot \mathcal{F}) v =
      v$.
