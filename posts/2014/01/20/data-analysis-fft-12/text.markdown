---
author: Ian
tags: data-analysis,haskell
specials: angular(myApp,['radian'])
title: "Haskell FFT 12: Optimisation Part 2"
published: 2014-01-20 17:12:41
---

In the last article, we did some basic optimisation of our FFT code.
In this article, we're going to look at ways of reordering the
recursive Cooley-Tukey FFT decomposition to make things more
efficient.  In order to make this work well, we're going to need more
straight line transform "codelets".  We'll start by looking at our
$N=256$ example in detail, then we'll develop a general approach.

<!--MORE-->

## Experiments with $N=256$

So far, we've been using a fixed scheme for decomposing the overall
Fourier matrix for our transforms, splitting out single prime factors
one at a time in increasing order of size, ending up with a base
transform of a prime number length, which we then process using either
a specialised straight line codelet, or Rader's algorithm.  However,
there's nothing in the generalised Danielson-Lanczos recursion step
that we're using that requires us to use *prime* factors (we can build
the necessary $I+D$ matrices for any factor size), and there's nothing
that constrains the order in which we process factors of our input
length.

For the $N=256$ example, we've been decomposing the input length as $2
\times 2 \times 2 \times 2 \times 2 \times 2 \times 2 \times 2$, but
there's no reason why we couldn't decompose it as $16 \times 16$, $4
\times 8 \times 8$ or any other ordering of factors that multiply to
give 256.  There are several things that could make one of these other
choices of factorisations give a faster FFT:

1. We could have a specialised straight line transform for $N=16$ say,
   which would make the $16 \times 16$ factorisation more attractive;

2. We could save work by doing less recursive calls: there's going to
   be less overhead in doing a single Danielson-Lanczos step for the
   $16 \times 16$ decomposition than in doing seven steps for the $2
   \times 2 \times 2 \times 2 \times 2 \times 2 \times 2 \times 2$
   decomposition;

3. We could take advantage of special structure in the
   Danielson-Lanczos $I+D$ matrices for certain factor sizes (we won't
   do this here, but it's a possibility if we ever develop specialised
   "twiddlets" as well as "bottomlets");

4. One factorisation might result in better cache performance than
   another (this obviously is more relevant for larger transform
   sizes).

In order to take advantage of different factorisations in the $N=256$
case, it will be advantageous to have specialised straight line base
transforms for various powers-of-two input lengths.  I've implemented
these specialised transforms for $N=2, 4, 8, 16, 32, 64$ (again just
converting the FFTW codelets to Haskell).

For $N = 256 = 2^8$, we can think about transform plans that make use
of any of these base transforms: if we use the $N=64$ base transform,
we account for 6 of those 8 factors of two, leaving us with 2 factors
of two to deal with via Danielson-Lanczos steps.  We can treat these
as a $2 \times 2$ decomposition, or as a single step of size 4.
Similarly, if we use the $N=32$ base transform, we have 3 factors of 2
left over to deal with using Danielson-Lanczos steps, which we can
treat as one of $\{ 2 \times 2 \times 2, 2 \times 4, 4 \times 2, 8 \}$
-- note that the order of factors may be relevant here: it's quite
possible that a $2 \times 4$ decomposition could have different
performance behaviour than a $4 \times 2$ decomposition.

For each base transform size, we thus have a number of decomposition
possibilities for dealing with the "left over" factors of two.  If
there are $m$ left over factors, there are $2^{m-1}$ possible
decompositions.  To see why this is so, note that this decomposition
problem is isomorphic to the combinatorical *compositions* problem,
the determination of the number of ways of writing an integer $m$ as
the sum of a sequence of strictly positive integers (where the order
of the sequence matters, making this distinct from the *partition*
problem).  For instance, for $m=3$, we can write $3=1+1+1$, $3=2+1$,
$3=1+2$, $3=3$, isomorphic to writing $8=2^1 \times 2^1 \times 2^1$,
$8=2^2 \times 2^1$, $8=2^1 \times 2^2$, $8=2^3$.

The composition problem can be solved by thinking of writing down a
row of $m$ ones, with $m-1$ gaps (shown as boxes):

@@@@ { display: block; margin-left: auto; margin-right: auto; }
    \path (0.0,0) node {(}
          (0.5,0) node {1}
          (1.0,0) node [shape=rectangle,draw] {}
          (1.5,0) node {1}
          (2.0,0) node [shape=rectangle,draw] {}
          (2.5,0) node {$\dots$}
          (3.0,0) node [shape=rectangle,draw] {}
          (3.5,0) node {1}
          (4.0,0) node [shape=rectangle,draw] {}
          (4.5,0) node {1}
          (5.0,0) node {)} ;
@@@@

If we now assign each box either a comma (",") or a plus sign ("+"),
we end up with a unique composition.  Since there are $m-1$ gaps and
two possibilities for each gap, there are $2^{m-1}$ total
compositions.

For $N=256$, we can determine the possible compositions of the left
over factors for each possible choice of base transform.  In the
following figure, possible FFT plans for $N=256$ are shown, given the
availability of base transforms of sizes 2, 4, 8, 16, 32, 64.  Each
dot represents a factor of two (eight for each instance: $256=2^8$).
The factors for the base transform are surrounded by a frame, and
factors that are treated together in a Danielson-Lanczos step are
joined by a line (i.e. three dots joined by a line represent a
Danielson-Lanczos step of size 8):

@@! { display: block; margin-left: auto; margin-right: auto; }
\tikzstyle{blob}=[circle,fill=black,inner sep=0.5mm]
\tikzstyle{edge}=[very thick]
\tikzstyle{box}=[draw=gray,very thick,rounded corners]
\begin{tikzpicture}[scale=\textwidth/34.0cm]
\node at (0.0,-0.0) [blob] {};
\node at (1.0,-0.0) [blob] {};
\draw [edge] (0.0,-0.0) -- (1.0,-0.0);
\draw [box] (1.7,-0.3) rectangle (7.3,0.3);
\node at (2.0,-0.0) [blob] {};
\node at (3.0,-0.0) [blob] {};
\node at (4.0,-0.0) [blob] {};
\node at (5.0,-0.0) [blob] {};
\node at (6.0,-0.0) [blob] {};
\node at (7.0,-0.0) [blob] {};
\node at (0.0,-1.0) [blob] {};
\node at (1.0,-1.0) [blob] {};
\draw [box] (1.7,-1.3) rectangle (7.3,-0.7);
\node at (2.0,-1.0) [blob] {};
\node at (3.0,-1.0) [blob] {};
\node at (4.0,-1.0) [blob] {};
\node at (5.0,-1.0) [blob] {};
\node at (6.0,-1.0) [blob] {};
\node at (7.0,-1.0) [blob] {};
\node at (0.0,-2.0) [blob] {};
\node at (1.0,-2.0) [blob] {};
\node at (2.0,-2.0) [blob] {};
\draw [edge] (0.0,-2.0) -- (2.0,-2.0);
\draw [box] (2.7,-2.3) rectangle (7.3,-1.7);
\node at (3.0,-2.0) [blob] {};
\node at (4.0,-2.0) [blob] {};
\node at (5.0,-2.0) [blob] {};
\node at (6.0,-2.0) [blob] {};
\node at (7.0,-2.0) [blob] {};
\node at (0.0,-3.0) [blob] {};
\node at (1.0,-3.0) [blob] {};
\node at (2.0,-3.0) [blob] {};
\draw [edge] (0.0,-3.0) -- (1.0,-3.0);
\draw [box] (2.7,-3.3) rectangle (7.3,-2.7);
\node at (3.0,-3.0) [blob] {};
\node at (4.0,-3.0) [blob] {};
\node at (5.0,-3.0) [blob] {};
\node at (6.0,-3.0) [blob] {};
\node at (7.0,-3.0) [blob] {};
\node at (0.0,-4.0) [blob] {};
\node at (1.0,-4.0) [blob] {};
\node at (2.0,-4.0) [blob] {};
\draw [edge] (1.0,-4.0) -- (2.0,-4.0);
\draw [box] (2.7,-4.3) rectangle (7.3,-3.7);
\node at (3.0,-4.0) [blob] {};
\node at (4.0,-4.0) [blob] {};
\node at (5.0,-4.0) [blob] {};
\node at (6.0,-4.0) [blob] {};
\node at (7.0,-4.0) [blob] {};
\node at (0.0,-5.0) [blob] {};
\node at (1.0,-5.0) [blob] {};
\node at (2.0,-5.0) [blob] {};
\draw [box] (2.7,-5.3) rectangle (7.3,-4.7);
\node at (3.0,-5.0) [blob] {};
\node at (4.0,-5.0) [blob] {};
\node at (5.0,-5.0) [blob] {};
\node at (6.0,-5.0) [blob] {};
\node at (7.0,-5.0) [blob] {};
\node at (0.0,-6.0) [blob] {};
\node at (1.0,-6.0) [blob] {};
\node at (2.0,-6.0) [blob] {};
\node at (3.0,-6.0) [blob] {};
\draw [edge] (0.0,-6.0) -- (3.0,-6.0);
\draw [box] (3.7,-6.3) rectangle (7.3,-5.7);
\node at (4.0,-6.0) [blob] {};
\node at (5.0,-6.0) [blob] {};
\node at (6.0,-6.0) [blob] {};
\node at (7.0,-6.0) [blob] {};
\node at (0.0,-7.0) [blob] {};
\node at (1.0,-7.0) [blob] {};
\node at (2.0,-7.0) [blob] {};
\node at (3.0,-7.0) [blob] {};
\draw [edge] (0.0,-7.0) -- (2.0,-7.0);
\draw [box] (3.7,-7.3) rectangle (7.3,-6.7);
\node at (4.0,-7.0) [blob] {};
\node at (5.0,-7.0) [blob] {};
\node at (6.0,-7.0) [blob] {};
\node at (7.0,-7.0) [blob] {};
\node at (0.0,-8.0) [blob] {};
\node at (1.0,-8.0) [blob] {};
\node at (2.0,-8.0) [blob] {};
\node at (3.0,-8.0) [blob] {};
\draw [edge] (0.0,-8.0) -- (1.0,-8.0);
\draw [edge] (2.0,-8.0) -- (3.0,-8.0);
\draw [box] (3.7,-8.3) rectangle (7.3,-7.7);
\node at (4.0,-8.0) [blob] {};
\node at (5.0,-8.0) [blob] {};
\node at (6.0,-8.0) [blob] {};
\node at (7.0,-8.0) [blob] {};
\node at (0.0,-9.0) [blob] {};
\node at (1.0,-9.0) [blob] {};
\node at (2.0,-9.0) [blob] {};
\node at (3.0,-9.0) [blob] {};
\draw [edge] (0.0,-9.0) -- (1.0,-9.0);
\draw [box] (3.7,-9.3) rectangle (7.3,-8.7);
\node at (4.0,-9.0) [blob] {};
\node at (5.0,-9.0) [blob] {};
\node at (6.0,-9.0) [blob] {};
\node at (7.0,-9.0) [blob] {};
\node at (0.0,-10.0) [blob] {};
\node at (1.0,-10.0) [blob] {};
\node at (2.0,-10.0) [blob] {};
\node at (3.0,-10.0) [blob] {};
\draw [edge] (1.0,-10.0) -- (3.0,-10.0);
\draw [box] (3.7,-10.3) rectangle (7.3,-9.7);
\node at (4.0,-10.0) [blob] {};
\node at (5.0,-10.0) [blob] {};
\node at (6.0,-10.0) [blob] {};
\node at (7.0,-10.0) [blob] {};
\node at (0.0,-11.0) [blob] {};
\node at (1.0,-11.0) [blob] {};
\node at (2.0,-11.0) [blob] {};
\node at (3.0,-11.0) [blob] {};
\draw [edge] (1.0,-11.0) -- (2.0,-11.0);
\draw [box] (3.7,-11.3) rectangle (7.3,-10.7);
\node at (4.0,-11.0) [blob] {};
\node at (5.0,-11.0) [blob] {};
\node at (6.0,-11.0) [blob] {};
\node at (7.0,-11.0) [blob] {};
\node at (0.0,-12.0) [blob] {};
\node at (1.0,-12.0) [blob] {};
\node at (2.0,-12.0) [blob] {};
\node at (3.0,-12.0) [blob] {};
\draw [edge] (2.0,-12.0) -- (3.0,-12.0);
\draw [box] (3.7,-12.3) rectangle (7.3,-11.7);
\node at (4.0,-12.0) [blob] {};
\node at (5.0,-12.0) [blob] {};
\node at (6.0,-12.0) [blob] {};
\node at (7.0,-12.0) [blob] {};
\node at (0.0,-13.0) [blob] {};
\node at (1.0,-13.0) [blob] {};
\node at (2.0,-13.0) [blob] {};
\node at (3.0,-13.0) [blob] {};
\draw [box] (3.7,-13.3) rectangle (7.3,-12.7);
\node at (4.0,-13.0) [blob] {};
\node at (5.0,-13.0) [blob] {};
\node at (6.0,-13.0) [blob] {};
\node at (7.0,-13.0) [blob] {};
\node at (0.0,-14.0) [blob] {};
\node at (1.0,-14.0) [blob] {};
\node at (2.0,-14.0) [blob] {};
\node at (3.0,-14.0) [blob] {};
\node at (4.0,-14.0) [blob] {};
\draw [edge] (0.0,-14.0) -- (4.0,-14.0);
\draw [box] (4.7,-14.3) rectangle (7.3,-13.7);
\node at (5.0,-14.0) [blob] {};
\node at (6.0,-14.0) [blob] {};
\node at (7.0,-14.0) [blob] {};
\node at (0.0,-15.0) [blob] {};
\node at (1.0,-15.0) [blob] {};
\node at (2.0,-15.0) [blob] {};
\node at (3.0,-15.0) [blob] {};
\node at (4.0,-15.0) [blob] {};
\draw [edge] (0.0,-15.0) -- (3.0,-15.0);
\draw [box] (4.7,-15.3) rectangle (7.3,-14.7);
\node at (5.0,-15.0) [blob] {};
\node at (6.0,-15.0) [blob] {};
\node at (7.0,-15.0) [blob] {};
\node at (0.0,-16.0) [blob] {};
\node at (1.0,-16.0) [blob] {};
\node at (2.0,-16.0) [blob] {};
\node at (3.0,-16.0) [blob] {};
\node at (4.0,-16.0) [blob] {};
\draw [edge] (0.0,-16.0) -- (2.0,-16.0);
\draw [edge] (3.0,-16.0) -- (4.0,-16.0);
\draw [box] (4.7,-16.3) rectangle (7.3,-15.7);
\node at (5.0,-16.0) [blob] {};
\node at (6.0,-16.0) [blob] {};
\node at (7.0,-16.0) [blob] {};
\node at (0.0,-17.0) [blob] {};
\node at (1.0,-17.0) [blob] {};
\node at (2.0,-17.0) [blob] {};
\node at (3.0,-17.0) [blob] {};
\node at (4.0,-17.0) [blob] {};
\draw [edge] (0.0,-17.0) -- (2.0,-17.0);
\draw [box] (4.7,-17.3) rectangle (7.3,-16.7);
\node at (5.0,-17.0) [blob] {};
\node at (6.0,-17.0) [blob] {};
\node at (7.0,-17.0) [blob] {};
\node at (0.0,-18.0) [blob] {};
\node at (1.0,-18.0) [blob] {};
\node at (2.0,-18.0) [blob] {};
\node at (3.0,-18.0) [blob] {};
\node at (4.0,-18.0) [blob] {};
\draw [edge] (0.0,-18.0) -- (1.0,-18.0);
\draw [edge] (2.0,-18.0) -- (4.0,-18.0);
\draw [box] (4.7,-18.3) rectangle (7.3,-17.7);
\node at (5.0,-18.0) [blob] {};
\node at (6.0,-18.0) [blob] {};
\node at (7.0,-18.0) [blob] {};
\node at (0.0,-19.0) [blob] {};
\node at (1.0,-19.0) [blob] {};
\node at (2.0,-19.0) [blob] {};
\node at (3.0,-19.0) [blob] {};
\node at (4.0,-19.0) [blob] {};
\draw [edge] (0.0,-19.0) -- (1.0,-19.0);
\draw [edge] (2.0,-19.0) -- (3.0,-19.0);
\draw [box] (4.7,-19.3) rectangle (7.3,-18.7);
\node at (5.0,-19.0) [blob] {};
\node at (6.0,-19.0) [blob] {};
\node at (7.0,-19.0) [blob] {};
\node at (0.0,-20.0) [blob] {};
\node at (1.0,-20.0) [blob] {};
\node at (2.0,-20.0) [blob] {};
\node at (3.0,-20.0) [blob] {};
\node at (4.0,-20.0) [blob] {};
\draw [edge] (0.0,-20.0) -- (1.0,-20.0);
\draw [edge] (3.0,-20.0) -- (4.0,-20.0);
\draw [box] (4.7,-20.3) rectangle (7.3,-19.7);
\node at (5.0,-20.0) [blob] {};
\node at (6.0,-20.0) [blob] {};
\node at (7.0,-20.0) [blob] {};
\node at (0.0,-21.0) [blob] {};
\node at (1.0,-21.0) [blob] {};
\node at (2.0,-21.0) [blob] {};
\node at (3.0,-21.0) [blob] {};
\node at (4.0,-21.0) [blob] {};
\draw [edge] (0.0,-21.0) -- (1.0,-21.0);
\draw [box] (4.7,-21.3) rectangle (7.3,-20.7);
\node at (5.0,-21.0) [blob] {};
\node at (6.0,-21.0) [blob] {};
\node at (7.0,-21.0) [blob] {};
\node at (0.0,-22.0) [blob] {};
\node at (1.0,-22.0) [blob] {};
\node at (2.0,-22.0) [blob] {};
\node at (3.0,-22.0) [blob] {};
\node at (4.0,-22.0) [blob] {};
\draw [edge] (1.0,-22.0) -- (4.0,-22.0);
\draw [box] (4.7,-22.3) rectangle (7.3,-21.7);
\node at (5.0,-22.0) [blob] {};
\node at (6.0,-22.0) [blob] {};
\node at (7.0,-22.0) [blob] {};
\node at (0.0,-23.0) [blob] {};
\node at (1.0,-23.0) [blob] {};
\node at (2.0,-23.0) [blob] {};
\node at (3.0,-23.0) [blob] {};
\node at (4.0,-23.0) [blob] {};
\draw [edge] (1.0,-23.0) -- (3.0,-23.0);
\draw [box] (4.7,-23.3) rectangle (7.3,-22.7);
\node at (5.0,-23.0) [blob] {};
\node at (6.0,-23.0) [blob] {};
\node at (7.0,-23.0) [blob] {};
\node at (0.0,-24.0) [blob] {};
\node at (1.0,-24.0) [blob] {};
\node at (2.0,-24.0) [blob] {};
\node at (3.0,-24.0) [blob] {};
\node at (4.0,-24.0) [blob] {};
\draw [edge] (1.0,-24.0) -- (2.0,-24.0);
\draw [edge] (3.0,-24.0) -- (4.0,-24.0);
\draw [box] (4.7,-24.3) rectangle (7.3,-23.7);
\node at (5.0,-24.0) [blob] {};
\node at (6.0,-24.0) [blob] {};
\node at (7.0,-24.0) [blob] {};
\node at (0.0,-25.0) [blob] {};
\node at (1.0,-25.0) [blob] {};
\node at (2.0,-25.0) [blob] {};
\node at (3.0,-25.0) [blob] {};
\node at (4.0,-25.0) [blob] {};
\draw [edge] (1.0,-25.0) -- (2.0,-25.0);
\draw [box] (4.7,-25.3) rectangle (7.3,-24.7);
\node at (5.0,-25.0) [blob] {};
\node at (6.0,-25.0) [blob] {};
\node at (7.0,-25.0) [blob] {};
\node at (0.0,-26.0) [blob] {};
\node at (1.0,-26.0) [blob] {};
\node at (2.0,-26.0) [blob] {};
\node at (3.0,-26.0) [blob] {};
\node at (4.0,-26.0) [blob] {};
\draw [edge] (2.0,-26.0) -- (4.0,-26.0);
\draw [box] (4.7,-26.3) rectangle (7.3,-25.7);
\node at (5.0,-26.0) [blob] {};
\node at (6.0,-26.0) [blob] {};
\node at (7.0,-26.0) [blob] {};
\node at (0.0,-27.0) [blob] {};
\node at (1.0,-27.0) [blob] {};
\node at (2.0,-27.0) [blob] {};
\node at (3.0,-27.0) [blob] {};
\node at (4.0,-27.0) [blob] {};
\draw [edge] (2.0,-27.0) -- (3.0,-27.0);
\draw [box] (4.7,-27.3) rectangle (7.3,-26.7);
\node at (5.0,-27.0) [blob] {};
\node at (6.0,-27.0) [blob] {};
\node at (7.0,-27.0) [blob] {};
\node at (0.0,-28.0) [blob] {};
\node at (1.0,-28.0) [blob] {};
\node at (2.0,-28.0) [blob] {};
\node at (3.0,-28.0) [blob] {};
\node at (4.0,-28.0) [blob] {};
\draw [edge] (3.0,-28.0) -- (4.0,-28.0);
\draw [box] (4.7,-28.3) rectangle (7.3,-27.7);
\node at (5.0,-28.0) [blob] {};
\node at (6.0,-28.0) [blob] {};
\node at (7.0,-28.0) [blob] {};
\node at (0.0,-29.0) [blob] {};
\node at (1.0,-29.0) [blob] {};
\node at (2.0,-29.0) [blob] {};
\node at (3.0,-29.0) [blob] {};
\node at (4.0,-29.0) [blob] {};
\draw [box] (4.7,-29.3) rectangle (7.3,-28.7);
\node at (5.0,-29.0) [blob] {};
\node at (6.0,-29.0) [blob] {};
\node at (7.0,-29.0) [blob] {};
\node at (0.0,-30.0) [blob] {};
\node at (1.0,-30.0) [blob] {};
\node at (2.0,-30.0) [blob] {};
\node at (3.0,-30.0) [blob] {};
\node at (4.0,-30.0) [blob] {};
\node at (5.0,-30.0) [blob] {};
\draw [edge] (0.0,-30.0) -- (5.0,-30.0);
\draw [box] (5.7,-30.3) rectangle (7.3,-29.7);
\node at (6.0,-30.0) [blob] {};
\node at (7.0,-30.0) [blob] {};
\node at (0.0,-31.0) [blob] {};
\node at (1.0,-31.0) [blob] {};
\node at (2.0,-31.0) [blob] {};
\node at (3.0,-31.0) [blob] {};
\node at (4.0,-31.0) [blob] {};
\node at (5.0,-31.0) [blob] {};
\draw [edge] (0.0,-31.0) -- (4.0,-31.0);
\draw [box] (5.7,-31.3) rectangle (7.3,-30.7);
\node at (6.0,-31.0) [blob] {};
\node at (7.0,-31.0) [blob] {};
\node at (9.0,-0.0) [blob] {};
\node at (10.0,-0.0) [blob] {};
\node at (11.0,-0.0) [blob] {};
\node at (12.0,-0.0) [blob] {};
\node at (13.0,-0.0) [blob] {};
\node at (14.0,-0.0) [blob] {};
\draw [edge] (9.0,-0.0) -- (12.0,-0.0);
\draw [edge] (13.0,-0.0) -- (14.0,-0.0);
\draw [box] (14.7,-0.3) rectangle (16.3,0.3);
\node at (15.0,-0.0) [blob] {};
\node at (16.0,-0.0) [blob] {};
\node at (9.0,-1.0) [blob] {};
\node at (10.0,-1.0) [blob] {};
\node at (11.0,-1.0) [blob] {};
\node at (12.0,-1.0) [blob] {};
\node at (13.0,-1.0) [blob] {};
\node at (14.0,-1.0) [blob] {};
\draw [edge] (9.0,-1.0) -- (12.0,-1.0);
\draw [box] (14.7,-1.3) rectangle (16.3,-0.7);
\node at (15.0,-1.0) [blob] {};
\node at (16.0,-1.0) [blob] {};
\node at (9.0,-2.0) [blob] {};
\node at (10.0,-2.0) [blob] {};
\node at (11.0,-2.0) [blob] {};
\node at (12.0,-2.0) [blob] {};
\node at (13.0,-2.0) [blob] {};
\node at (14.0,-2.0) [blob] {};
\draw [edge] (9.0,-2.0) -- (11.0,-2.0);
\draw [edge] (12.0,-2.0) -- (14.0,-2.0);
\draw [box] (14.7,-2.3) rectangle (16.3,-1.7);
\node at (15.0,-2.0) [blob] {};
\node at (16.0,-2.0) [blob] {};
\node at (9.0,-3.0) [blob] {};
\node at (10.0,-3.0) [blob] {};
\node at (11.0,-3.0) [blob] {};
\node at (12.0,-3.0) [blob] {};
\node at (13.0,-3.0) [blob] {};
\node at (14.0,-3.0) [blob] {};
\draw [edge] (9.0,-3.0) -- (11.0,-3.0);
\draw [edge] (12.0,-3.0) -- (13.0,-3.0);
\draw [box] (14.7,-3.3) rectangle (16.3,-2.7);
\node at (15.0,-3.0) [blob] {};
\node at (16.0,-3.0) [blob] {};
\node at (9.0,-4.0) [blob] {};
\node at (10.0,-4.0) [blob] {};
\node at (11.0,-4.0) [blob] {};
\node at (12.0,-4.0) [blob] {};
\node at (13.0,-4.0) [blob] {};
\node at (14.0,-4.0) [blob] {};
\draw [edge] (9.0,-4.0) -- (11.0,-4.0);
\draw [edge] (13.0,-4.0) -- (14.0,-4.0);
\draw [box] (14.7,-4.3) rectangle (16.3,-3.7);
\node at (15.0,-4.0) [blob] {};
\node at (16.0,-4.0) [blob] {};
\node at (9.0,-5.0) [blob] {};
\node at (10.0,-5.0) [blob] {};
\node at (11.0,-5.0) [blob] {};
\node at (12.0,-5.0) [blob] {};
\node at (13.0,-5.0) [blob] {};
\node at (14.0,-5.0) [blob] {};
\draw [edge] (9.0,-5.0) -- (11.0,-5.0);
\draw [box] (14.7,-5.3) rectangle (16.3,-4.7);
\node at (15.0,-5.0) [blob] {};
\node at (16.0,-5.0) [blob] {};
\node at (9.0,-6.0) [blob] {};
\node at (10.0,-6.0) [blob] {};
\node at (11.0,-6.0) [blob] {};
\node at (12.0,-6.0) [blob] {};
\node at (13.0,-6.0) [blob] {};
\node at (14.0,-6.0) [blob] {};
\draw [edge] (9.0,-6.0) -- (10.0,-6.0);
\draw [edge] (11.0,-6.0) -- (14.0,-6.0);
\draw [box] (14.7,-6.3) rectangle (16.3,-5.7);
\node at (15.0,-6.0) [blob] {};
\node at (16.0,-6.0) [blob] {};
\node at (9.0,-7.0) [blob] {};
\node at (10.0,-7.0) [blob] {};
\node at (11.0,-7.0) [blob] {};
\node at (12.0,-7.0) [blob] {};
\node at (13.0,-7.0) [blob] {};
\node at (14.0,-7.0) [blob] {};
\draw [edge] (9.0,-7.0) -- (10.0,-7.0);
\draw [edge] (11.0,-7.0) -- (13.0,-7.0);
\draw [box] (14.7,-7.3) rectangle (16.3,-6.7);
\node at (15.0,-7.0) [blob] {};
\node at (16.0,-7.0) [blob] {};
\node at (9.0,-8.0) [blob] {};
\node at (10.0,-8.0) [blob] {};
\node at (11.0,-8.0) [blob] {};
\node at (12.0,-8.0) [blob] {};
\node at (13.0,-8.0) [blob] {};
\node at (14.0,-8.0) [blob] {};
\draw [edge] (9.0,-8.0) -- (10.0,-8.0);
\draw [edge] (11.0,-8.0) -- (12.0,-8.0);
\draw [edge] (13.0,-8.0) -- (14.0,-8.0);
\draw [box] (14.7,-8.3) rectangle (16.3,-7.7);
\node at (15.0,-8.0) [blob] {};
\node at (16.0,-8.0) [blob] {};
\node at (9.0,-9.0) [blob] {};
\node at (10.0,-9.0) [blob] {};
\node at (11.0,-9.0) [blob] {};
\node at (12.0,-9.0) [blob] {};
\node at (13.0,-9.0) [blob] {};
\node at (14.0,-9.0) [blob] {};
\draw [edge] (9.0,-9.0) -- (10.0,-9.0);
\draw [edge] (11.0,-9.0) -- (12.0,-9.0);
\draw [box] (14.7,-9.3) rectangle (16.3,-8.7);
\node at (15.0,-9.0) [blob] {};
\node at (16.0,-9.0) [blob] {};
\node at (9.0,-10.0) [blob] {};
\node at (10.0,-10.0) [blob] {};
\node at (11.0,-10.0) [blob] {};
\node at (12.0,-10.0) [blob] {};
\node at (13.0,-10.0) [blob] {};
\node at (14.0,-10.0) [blob] {};
\draw [edge] (9.0,-10.0) -- (10.0,-10.0);
\draw [edge] (12.0,-10.0) -- (14.0,-10.0);
\draw [box] (14.7,-10.3) rectangle (16.3,-9.7);
\node at (15.0,-10.0) [blob] {};
\node at (16.0,-10.0) [blob] {};
\node at (9.0,-11.0) [blob] {};
\node at (10.0,-11.0) [blob] {};
\node at (11.0,-11.0) [blob] {};
\node at (12.0,-11.0) [blob] {};
\node at (13.0,-11.0) [blob] {};
\node at (14.0,-11.0) [blob] {};
\draw [edge] (9.0,-11.0) -- (10.0,-11.0);
\draw [edge] (12.0,-11.0) -- (13.0,-11.0);
\draw [box] (14.7,-11.3) rectangle (16.3,-10.7);
\node at (15.0,-11.0) [blob] {};
\node at (16.0,-11.0) [blob] {};
\node at (9.0,-12.0) [blob] {};
\node at (10.0,-12.0) [blob] {};
\node at (11.0,-12.0) [blob] {};
\node at (12.0,-12.0) [blob] {};
\node at (13.0,-12.0) [blob] {};
\node at (14.0,-12.0) [blob] {};
\draw [edge] (9.0,-12.0) -- (10.0,-12.0);
\draw [edge] (13.0,-12.0) -- (14.0,-12.0);
\draw [box] (14.7,-12.3) rectangle (16.3,-11.7);
\node at (15.0,-12.0) [blob] {};
\node at (16.0,-12.0) [blob] {};
\node at (9.0,-13.0) [blob] {};
\node at (10.0,-13.0) [blob] {};
\node at (11.0,-13.0) [blob] {};
\node at (12.0,-13.0) [blob] {};
\node at (13.0,-13.0) [blob] {};
\node at (14.0,-13.0) [blob] {};
\draw [edge] (9.0,-13.0) -- (10.0,-13.0);
\draw [box] (14.7,-13.3) rectangle (16.3,-12.7);
\node at (15.0,-13.0) [blob] {};
\node at (16.0,-13.0) [blob] {};
\node at (9.0,-14.0) [blob] {};
\node at (10.0,-14.0) [blob] {};
\node at (11.0,-14.0) [blob] {};
\node at (12.0,-14.0) [blob] {};
\node at (13.0,-14.0) [blob] {};
\node at (14.0,-14.0) [blob] {};
\draw [edge] (10.0,-14.0) -- (14.0,-14.0);
\draw [box] (14.7,-14.3) rectangle (16.3,-13.7);
\node at (15.0,-14.0) [blob] {};
\node at (16.0,-14.0) [blob] {};
\node at (9.0,-15.0) [blob] {};
\node at (10.0,-15.0) [blob] {};
\node at (11.0,-15.0) [blob] {};
\node at (12.0,-15.0) [blob] {};
\node at (13.0,-15.0) [blob] {};
\node at (14.0,-15.0) [blob] {};
\draw [edge] (10.0,-15.0) -- (13.0,-15.0);
\draw [box] (14.7,-15.3) rectangle (16.3,-14.7);
\node at (15.0,-15.0) [blob] {};
\node at (16.0,-15.0) [blob] {};
\node at (9.0,-16.0) [blob] {};
\node at (10.0,-16.0) [blob] {};
\node at (11.0,-16.0) [blob] {};
\node at (12.0,-16.0) [blob] {};
\node at (13.0,-16.0) [blob] {};
\node at (14.0,-16.0) [blob] {};
\draw [edge] (10.0,-16.0) -- (12.0,-16.0);
\draw [edge] (13.0,-16.0) -- (14.0,-16.0);
\draw [box] (14.7,-16.3) rectangle (16.3,-15.7);
\node at (15.0,-16.0) [blob] {};
\node at (16.0,-16.0) [blob] {};
\node at (9.0,-17.0) [blob] {};
\node at (10.0,-17.0) [blob] {};
\node at (11.0,-17.0) [blob] {};
\node at (12.0,-17.0) [blob] {};
\node at (13.0,-17.0) [blob] {};
\node at (14.0,-17.0) [blob] {};
\draw [edge] (10.0,-17.0) -- (12.0,-17.0);
\draw [box] (14.7,-17.3) rectangle (16.3,-16.7);
\node at (15.0,-17.0) [blob] {};
\node at (16.0,-17.0) [blob] {};
\node at (9.0,-18.0) [blob] {};
\node at (10.0,-18.0) [blob] {};
\node at (11.0,-18.0) [blob] {};
\node at (12.0,-18.0) [blob] {};
\node at (13.0,-18.0) [blob] {};
\node at (14.0,-18.0) [blob] {};
\draw [edge] (10.0,-18.0) -- (11.0,-18.0);
\draw [edge] (12.0,-18.0) -- (14.0,-18.0);
\draw [box] (14.7,-18.3) rectangle (16.3,-17.7);
\node at (15.0,-18.0) [blob] {};
\node at (16.0,-18.0) [blob] {};
\node at (9.0,-19.0) [blob] {};
\node at (10.0,-19.0) [blob] {};
\node at (11.0,-19.0) [blob] {};
\node at (12.0,-19.0) [blob] {};
\node at (13.0,-19.0) [blob] {};
\node at (14.0,-19.0) [blob] {};
\draw [edge] (10.0,-19.0) -- (11.0,-19.0);
\draw [edge] (12.0,-19.0) -- (13.0,-19.0);
\draw [box] (14.7,-19.3) rectangle (16.3,-18.7);
\node at (15.0,-19.0) [blob] {};
\node at (16.0,-19.0) [blob] {};
\node at (9.0,-20.0) [blob] {};
\node at (10.0,-20.0) [blob] {};
\node at (11.0,-20.0) [blob] {};
\node at (12.0,-20.0) [blob] {};
\node at (13.0,-20.0) [blob] {};
\node at (14.0,-20.0) [blob] {};
\draw [edge] (10.0,-20.0) -- (11.0,-20.0);
\draw [edge] (13.0,-20.0) -- (14.0,-20.0);
\draw [box] (14.7,-20.3) rectangle (16.3,-19.7);
\node at (15.0,-20.0) [blob] {};
\node at (16.0,-20.0) [blob] {};
\node at (9.0,-21.0) [blob] {};
\node at (10.0,-21.0) [blob] {};
\node at (11.0,-21.0) [blob] {};
\node at (12.0,-21.0) [blob] {};
\node at (13.0,-21.0) [blob] {};
\node at (14.0,-21.0) [blob] {};
\draw [edge] (10.0,-21.0) -- (11.0,-21.0);
\draw [box] (14.7,-21.3) rectangle (16.3,-20.7);
\node at (15.0,-21.0) [blob] {};
\node at (16.0,-21.0) [blob] {};
\node at (9.0,-22.0) [blob] {};
\node at (10.0,-22.0) [blob] {};
\node at (11.0,-22.0) [blob] {};
\node at (12.0,-22.0) [blob] {};
\node at (13.0,-22.0) [blob] {};
\node at (14.0,-22.0) [blob] {};
\draw [edge] (11.0,-22.0) -- (14.0,-22.0);
\draw [box] (14.7,-22.3) rectangle (16.3,-21.7);
\node at (15.0,-22.0) [blob] {};
\node at (16.0,-22.0) [blob] {};
\node at (9.0,-23.0) [blob] {};
\node at (10.0,-23.0) [blob] {};
\node at (11.0,-23.0) [blob] {};
\node at (12.0,-23.0) [blob] {};
\node at (13.0,-23.0) [blob] {};
\node at (14.0,-23.0) [blob] {};
\draw [edge] (11.0,-23.0) -- (13.0,-23.0);
\draw [box] (14.7,-23.3) rectangle (16.3,-22.7);
\node at (15.0,-23.0) [blob] {};
\node at (16.0,-23.0) [blob] {};
\node at (9.0,-24.0) [blob] {};
\node at (10.0,-24.0) [blob] {};
\node at (11.0,-24.0) [blob] {};
\node at (12.0,-24.0) [blob] {};
\node at (13.0,-24.0) [blob] {};
\node at (14.0,-24.0) [blob] {};
\draw [edge] (11.0,-24.0) -- (12.0,-24.0);
\draw [edge] (13.0,-24.0) -- (14.0,-24.0);
\draw [box] (14.7,-24.3) rectangle (16.3,-23.7);
\node at (15.0,-24.0) [blob] {};
\node at (16.0,-24.0) [blob] {};
\node at (9.0,-25.0) [blob] {};
\node at (10.0,-25.0) [blob] {};
\node at (11.0,-25.0) [blob] {};
\node at (12.0,-25.0) [blob] {};
\node at (13.0,-25.0) [blob] {};
\node at (14.0,-25.0) [blob] {};
\draw [edge] (11.0,-25.0) -- (12.0,-25.0);
\draw [box] (14.7,-25.3) rectangle (16.3,-24.7);
\node at (15.0,-25.0) [blob] {};
\node at (16.0,-25.0) [blob] {};
\node at (9.0,-26.0) [blob] {};
\node at (10.0,-26.0) [blob] {};
\node at (11.0,-26.0) [blob] {};
\node at (12.0,-26.0) [blob] {};
\node at (13.0,-26.0) [blob] {};
\node at (14.0,-26.0) [blob] {};
\draw [edge] (12.0,-26.0) -- (14.0,-26.0);
\draw [box] (14.7,-26.3) rectangle (16.3,-25.7);
\node at (15.0,-26.0) [blob] {};
\node at (16.0,-26.0) [blob] {};
\node at (9.0,-27.0) [blob] {};
\node at (10.0,-27.0) [blob] {};
\node at (11.0,-27.0) [blob] {};
\node at (12.0,-27.0) [blob] {};
\node at (13.0,-27.0) [blob] {};
\node at (14.0,-27.0) [blob] {};
\draw [edge] (12.0,-27.0) -- (13.0,-27.0);
\draw [box] (14.7,-27.3) rectangle (16.3,-26.7);
\node at (15.0,-27.0) [blob] {};
\node at (16.0,-27.0) [blob] {};
\node at (9.0,-28.0) [blob] {};
\node at (10.0,-28.0) [blob] {};
\node at (11.0,-28.0) [blob] {};
\node at (12.0,-28.0) [blob] {};
\node at (13.0,-28.0) [blob] {};
\node at (14.0,-28.0) [blob] {};
\draw [edge] (13.0,-28.0) -- (14.0,-28.0);
\draw [box] (14.7,-28.3) rectangle (16.3,-27.7);
\node at (15.0,-28.0) [blob] {};
\node at (16.0,-28.0) [blob] {};
\node at (9.0,-29.0) [blob] {};
\node at (10.0,-29.0) [blob] {};
\node at (11.0,-29.0) [blob] {};
\node at (12.0,-29.0) [blob] {};
\node at (13.0,-29.0) [blob] {};
\node at (14.0,-29.0) [blob] {};
\draw [box] (14.7,-29.3) rectangle (16.3,-28.7);
\node at (15.0,-29.0) [blob] {};
\node at (16.0,-29.0) [blob] {};
\node at (9.0,-30.0) [blob] {};
\node at (10.0,-30.0) [blob] {};
\node at (11.0,-30.0) [blob] {};
\node at (12.0,-30.0) [blob] {};
\node at (13.0,-30.0) [blob] {};
\node at (14.0,-30.0) [blob] {};
\node at (15.0,-30.0) [blob] {};
\draw [edge] (9.0,-30.0) -- (15.0,-30.0);
\draw [box] (15.7,-30.3) rectangle (16.3,-29.7);
\node at (16.0,-30.0) [blob] {};
\node at (9.0,-31.0) [blob] {};
\node at (10.0,-31.0) [blob] {};
\node at (11.0,-31.0) [blob] {};
\node at (12.0,-31.0) [blob] {};
\node at (13.0,-31.0) [blob] {};
\node at (14.0,-31.0) [blob] {};
\node at (15.0,-31.0) [blob] {};
\draw [edge] (9.0,-31.0) -- (14.0,-31.0);
\draw [box] (15.7,-31.3) rectangle (16.3,-30.7);
\node at (16.0,-31.0) [blob] {};
\node at (18.0,-0.0) [blob] {};
\node at (19.0,-0.0) [blob] {};
\node at (20.0,-0.0) [blob] {};
\node at (21.0,-0.0) [blob] {};
\node at (22.0,-0.0) [blob] {};
\node at (23.0,-0.0) [blob] {};
\node at (24.0,-0.0) [blob] {};
\draw [edge] (18.0,-0.0) -- (22.0,-0.0);
\draw [edge] (23.0,-0.0) -- (24.0,-0.0);
\draw [box] (24.7,-0.3) rectangle (25.3,0.3);
\node at (25.0,-0.0) [blob] {};
\node at (18.0,-1.0) [blob] {};
\node at (19.0,-1.0) [blob] {};
\node at (20.0,-1.0) [blob] {};
\node at (21.0,-1.0) [blob] {};
\node at (22.0,-1.0) [blob] {};
\node at (23.0,-1.0) [blob] {};
\node at (24.0,-1.0) [blob] {};
\draw [edge] (18.0,-1.0) -- (22.0,-1.0);
\draw [box] (24.7,-1.3) rectangle (25.3,-0.7);
\node at (25.0,-1.0) [blob] {};
\node at (18.0,-2.0) [blob] {};
\node at (19.0,-2.0) [blob] {};
\node at (20.0,-2.0) [blob] {};
\node at (21.0,-2.0) [blob] {};
\node at (22.0,-2.0) [blob] {};
\node at (23.0,-2.0) [blob] {};
\node at (24.0,-2.0) [blob] {};
\draw [edge] (18.0,-2.0) -- (21.0,-2.0);
\draw [edge] (22.0,-2.0) -- (24.0,-2.0);
\draw [box] (24.7,-2.3) rectangle (25.3,-1.7);
\node at (25.0,-2.0) [blob] {};
\node at (18.0,-3.0) [blob] {};
\node at (19.0,-3.0) [blob] {};
\node at (20.0,-3.0) [blob] {};
\node at (21.0,-3.0) [blob] {};
\node at (22.0,-3.0) [blob] {};
\node at (23.0,-3.0) [blob] {};
\node at (24.0,-3.0) [blob] {};
\draw [edge] (18.0,-3.0) -- (21.0,-3.0);
\draw [edge] (22.0,-3.0) -- (23.0,-3.0);
\draw [box] (24.7,-3.3) rectangle (25.3,-2.7);
\node at (25.0,-3.0) [blob] {};
\node at (18.0,-4.0) [blob] {};
\node at (19.0,-4.0) [blob] {};
\node at (20.0,-4.0) [blob] {};
\node at (21.0,-4.0) [blob] {};
\node at (22.0,-4.0) [blob] {};
\node at (23.0,-4.0) [blob] {};
\node at (24.0,-4.0) [blob] {};
\draw [edge] (18.0,-4.0) -- (21.0,-4.0);
\draw [edge] (23.0,-4.0) -- (24.0,-4.0);
\draw [box] (24.7,-4.3) rectangle (25.3,-3.7);
\node at (25.0,-4.0) [blob] {};
\node at (18.0,-5.0) [blob] {};
\node at (19.0,-5.0) [blob] {};
\node at (20.0,-5.0) [blob] {};
\node at (21.0,-5.0) [blob] {};
\node at (22.0,-5.0) [blob] {};
\node at (23.0,-5.0) [blob] {};
\node at (24.0,-5.0) [blob] {};
\draw [edge] (18.0,-5.0) -- (21.0,-5.0);
\draw [box] (24.7,-5.3) rectangle (25.3,-4.7);
\node at (25.0,-5.0) [blob] {};
\node at (18.0,-6.0) [blob] {};
\node at (19.0,-6.0) [blob] {};
\node at (20.0,-6.0) [blob] {};
\node at (21.0,-6.0) [blob] {};
\node at (22.0,-6.0) [blob] {};
\node at (23.0,-6.0) [blob] {};
\node at (24.0,-6.0) [blob] {};
\draw [edge] (18.0,-6.0) -- (20.0,-6.0);
\draw [edge] (21.0,-6.0) -- (24.0,-6.0);
\draw [box] (24.7,-6.3) rectangle (25.3,-5.7);
\node at (25.0,-6.0) [blob] {};
\node at (18.0,-7.0) [blob] {};
\node at (19.0,-7.0) [blob] {};
\node at (20.0,-7.0) [blob] {};
\node at (21.0,-7.0) [blob] {};
\node at (22.0,-7.0) [blob] {};
\node at (23.0,-7.0) [blob] {};
\node at (24.0,-7.0) [blob] {};
\draw [edge] (18.0,-7.0) -- (20.0,-7.0);
\draw [edge] (21.0,-7.0) -- (23.0,-7.0);
\draw [box] (24.7,-7.3) rectangle (25.3,-6.7);
\node at (25.0,-7.0) [blob] {};
\node at (18.0,-8.0) [blob] {};
\node at (19.0,-8.0) [blob] {};
\node at (20.0,-8.0) [blob] {};
\node at (21.0,-8.0) [blob] {};
\node at (22.0,-8.0) [blob] {};
\node at (23.0,-8.0) [blob] {};
\node at (24.0,-8.0) [blob] {};
\draw [edge] (18.0,-8.0) -- (20.0,-8.0);
\draw [edge] (21.0,-8.0) -- (22.0,-8.0);
\draw [edge] (23.0,-8.0) -- (24.0,-8.0);
\draw [box] (24.7,-8.3) rectangle (25.3,-7.7);
\node at (25.0,-8.0) [blob] {};
\node at (18.0,-9.0) [blob] {};
\node at (19.0,-9.0) [blob] {};
\node at (20.0,-9.0) [blob] {};
\node at (21.0,-9.0) [blob] {};
\node at (22.0,-9.0) [blob] {};
\node at (23.0,-9.0) [blob] {};
\node at (24.0,-9.0) [blob] {};
\draw [edge] (18.0,-9.0) -- (20.0,-9.0);
\draw [edge] (21.0,-9.0) -- (22.0,-9.0);
\draw [box] (24.7,-9.3) rectangle (25.3,-8.7);
\node at (25.0,-9.0) [blob] {};
\node at (18.0,-10.0) [blob] {};
\node at (19.0,-10.0) [blob] {};
\node at (20.0,-10.0) [blob] {};
\node at (21.0,-10.0) [blob] {};
\node at (22.0,-10.0) [blob] {};
\node at (23.0,-10.0) [blob] {};
\node at (24.0,-10.0) [blob] {};
\draw [edge] (18.0,-10.0) -- (20.0,-10.0);
\draw [edge] (22.0,-10.0) -- (24.0,-10.0);
\draw [box] (24.7,-10.3) rectangle (25.3,-9.7);
\node at (25.0,-10.0) [blob] {};
\node at (18.0,-11.0) [blob] {};
\node at (19.0,-11.0) [blob] {};
\node at (20.0,-11.0) [blob] {};
\node at (21.0,-11.0) [blob] {};
\node at (22.0,-11.0) [blob] {};
\node at (23.0,-11.0) [blob] {};
\node at (24.0,-11.0) [blob] {};
\draw [edge] (18.0,-11.0) -- (20.0,-11.0);
\draw [edge] (22.0,-11.0) -- (23.0,-11.0);
\draw [box] (24.7,-11.3) rectangle (25.3,-10.7);
\node at (25.0,-11.0) [blob] {};
\node at (18.0,-12.0) [blob] {};
\node at (19.0,-12.0) [blob] {};
\node at (20.0,-12.0) [blob] {};
\node at (21.0,-12.0) [blob] {};
\node at (22.0,-12.0) [blob] {};
\node at (23.0,-12.0) [blob] {};
\node at (24.0,-12.0) [blob] {};
\draw [edge] (18.0,-12.0) -- (20.0,-12.0);
\draw [edge] (23.0,-12.0) -- (24.0,-12.0);
\draw [box] (24.7,-12.3) rectangle (25.3,-11.7);
\node at (25.0,-12.0) [blob] {};
\node at (18.0,-13.0) [blob] {};
\node at (19.0,-13.0) [blob] {};
\node at (20.0,-13.0) [blob] {};
\node at (21.0,-13.0) [blob] {};
\node at (22.0,-13.0) [blob] {};
\node at (23.0,-13.0) [blob] {};
\node at (24.0,-13.0) [blob] {};
\draw [edge] (18.0,-13.0) -- (20.0,-13.0);
\draw [box] (24.7,-13.3) rectangle (25.3,-12.7);
\node at (25.0,-13.0) [blob] {};
\node at (18.0,-14.0) [blob] {};
\node at (19.0,-14.0) [blob] {};
\node at (20.0,-14.0) [blob] {};
\node at (21.0,-14.0) [blob] {};
\node at (22.0,-14.0) [blob] {};
\node at (23.0,-14.0) [blob] {};
\node at (24.0,-14.0) [blob] {};
\draw [edge] (18.0,-14.0) -- (19.0,-14.0);
\draw [edge] (20.0,-14.0) -- (24.0,-14.0);
\draw [box] (24.7,-14.3) rectangle (25.3,-13.7);
\node at (25.0,-14.0) [blob] {};
\node at (18.0,-15.0) [blob] {};
\node at (19.0,-15.0) [blob] {};
\node at (20.0,-15.0) [blob] {};
\node at (21.0,-15.0) [blob] {};
\node at (22.0,-15.0) [blob] {};
\node at (23.0,-15.0) [blob] {};
\node at (24.0,-15.0) [blob] {};
\draw [edge] (18.0,-15.0) -- (19.0,-15.0);
\draw [edge] (20.0,-15.0) -- (23.0,-15.0);
\draw [box] (24.7,-15.3) rectangle (25.3,-14.7);
\node at (25.0,-15.0) [blob] {};
\node at (18.0,-16.0) [blob] {};
\node at (19.0,-16.0) [blob] {};
\node at (20.0,-16.0) [blob] {};
\node at (21.0,-16.0) [blob] {};
\node at (22.0,-16.0) [blob] {};
\node at (23.0,-16.0) [blob] {};
\node at (24.0,-16.0) [blob] {};
\draw [edge] (18.0,-16.0) -- (19.0,-16.0);
\draw [edge] (20.0,-16.0) -- (22.0,-16.0);
\draw [edge] (23.0,-16.0) -- (24.0,-16.0);
\draw [box] (24.7,-16.3) rectangle (25.3,-15.7);
\node at (25.0,-16.0) [blob] {};
\node at (18.0,-17.0) [blob] {};
\node at (19.0,-17.0) [blob] {};
\node at (20.0,-17.0) [blob] {};
\node at (21.0,-17.0) [blob] {};
\node at (22.0,-17.0) [blob] {};
\node at (23.0,-17.0) [blob] {};
\node at (24.0,-17.0) [blob] {};
\draw [edge] (18.0,-17.0) -- (19.0,-17.0);
\draw [edge] (20.0,-17.0) -- (22.0,-17.0);
\draw [box] (24.7,-17.3) rectangle (25.3,-16.7);
\node at (25.0,-17.0) [blob] {};
\node at (18.0,-18.0) [blob] {};
\node at (19.0,-18.0) [blob] {};
\node at (20.0,-18.0) [blob] {};
\node at (21.0,-18.0) [blob] {};
\node at (22.0,-18.0) [blob] {};
\node at (23.0,-18.0) [blob] {};
\node at (24.0,-18.0) [blob] {};
\draw [edge] (18.0,-18.0) -- (19.0,-18.0);
\draw [edge] (20.0,-18.0) -- (21.0,-18.0);
\draw [edge] (22.0,-18.0) -- (24.0,-18.0);
\draw [box] (24.7,-18.3) rectangle (25.3,-17.7);
\node at (25.0,-18.0) [blob] {};
\node at (18.0,-19.0) [blob] {};
\node at (19.0,-19.0) [blob] {};
\node at (20.0,-19.0) [blob] {};
\node at (21.0,-19.0) [blob] {};
\node at (22.0,-19.0) [blob] {};
\node at (23.0,-19.0) [blob] {};
\node at (24.0,-19.0) [blob] {};
\draw [edge] (18.0,-19.0) -- (19.0,-19.0);
\draw [edge] (20.0,-19.0) -- (21.0,-19.0);
\draw [edge] (22.0,-19.0) -- (23.0,-19.0);
\draw [box] (24.7,-19.3) rectangle (25.3,-18.7);
\node at (25.0,-19.0) [blob] {};
\node at (18.0,-20.0) [blob] {};
\node at (19.0,-20.0) [blob] {};
\node at (20.0,-20.0) [blob] {};
\node at (21.0,-20.0) [blob] {};
\node at (22.0,-20.0) [blob] {};
\node at (23.0,-20.0) [blob] {};
\node at (24.0,-20.0) [blob] {};
\draw [edge] (18.0,-20.0) -- (19.0,-20.0);
\draw [edge] (20.0,-20.0) -- (21.0,-20.0);
\draw [edge] (23.0,-20.0) -- (24.0,-20.0);
\draw [box] (24.7,-20.3) rectangle (25.3,-19.7);
\node at (25.0,-20.0) [blob] {};
\node at (18.0,-21.0) [blob] {};
\node at (19.0,-21.0) [blob] {};
\node at (20.0,-21.0) [blob] {};
\node at (21.0,-21.0) [blob] {};
\node at (22.0,-21.0) [blob] {};
\node at (23.0,-21.0) [blob] {};
\node at (24.0,-21.0) [blob] {};
\draw [edge] (18.0,-21.0) -- (19.0,-21.0);
\draw [edge] (20.0,-21.0) -- (21.0,-21.0);
\draw [box] (24.7,-21.3) rectangle (25.3,-20.7);
\node at (25.0,-21.0) [blob] {};
\node at (18.0,-22.0) [blob] {};
\node at (19.0,-22.0) [blob] {};
\node at (20.0,-22.0) [blob] {};
\node at (21.0,-22.0) [blob] {};
\node at (22.0,-22.0) [blob] {};
\node at (23.0,-22.0) [blob] {};
\node at (24.0,-22.0) [blob] {};
\draw [edge] (18.0,-22.0) -- (19.0,-22.0);
\draw [edge] (21.0,-22.0) -- (24.0,-22.0);
\draw [box] (24.7,-22.3) rectangle (25.3,-21.7);
\node at (25.0,-22.0) [blob] {};
\node at (18.0,-23.0) [blob] {};
\node at (19.0,-23.0) [blob] {};
\node at (20.0,-23.0) [blob] {};
\node at (21.0,-23.0) [blob] {};
\node at (22.0,-23.0) [blob] {};
\node at (23.0,-23.0) [blob] {};
\node at (24.0,-23.0) [blob] {};
\draw [edge] (18.0,-23.0) -- (19.0,-23.0);
\draw [edge] (21.0,-23.0) -- (23.0,-23.0);
\draw [box] (24.7,-23.3) rectangle (25.3,-22.7);
\node at (25.0,-23.0) [blob] {};
\node at (18.0,-24.0) [blob] {};
\node at (19.0,-24.0) [blob] {};
\node at (20.0,-24.0) [blob] {};
\node at (21.0,-24.0) [blob] {};
\node at (22.0,-24.0) [blob] {};
\node at (23.0,-24.0) [blob] {};
\node at (24.0,-24.0) [blob] {};
\draw [edge] (18.0,-24.0) -- (19.0,-24.0);
\draw [edge] (21.0,-24.0) -- (22.0,-24.0);
\draw [edge] (23.0,-24.0) -- (24.0,-24.0);
\draw [box] (24.7,-24.3) rectangle (25.3,-23.7);
\node at (25.0,-24.0) [blob] {};
\node at (18.0,-25.0) [blob] {};
\node at (19.0,-25.0) [blob] {};
\node at (20.0,-25.0) [blob] {};
\node at (21.0,-25.0) [blob] {};
\node at (22.0,-25.0) [blob] {};
\node at (23.0,-25.0) [blob] {};
\node at (24.0,-25.0) [blob] {};
\draw [edge] (18.0,-25.0) -- (19.0,-25.0);
\draw [edge] (21.0,-25.0) -- (22.0,-25.0);
\draw [box] (24.7,-25.3) rectangle (25.3,-24.7);
\node at (25.0,-25.0) [blob] {};
\node at (18.0,-26.0) [blob] {};
\node at (19.0,-26.0) [blob] {};
\node at (20.0,-26.0) [blob] {};
\node at (21.0,-26.0) [blob] {};
\node at (22.0,-26.0) [blob] {};
\node at (23.0,-26.0) [blob] {};
\node at (24.0,-26.0) [blob] {};
\draw [edge] (18.0,-26.0) -- (19.0,-26.0);
\draw [edge] (22.0,-26.0) -- (24.0,-26.0);
\draw [box] (24.7,-26.3) rectangle (25.3,-25.7);
\node at (25.0,-26.0) [blob] {};
\node at (18.0,-27.0) [blob] {};
\node at (19.0,-27.0) [blob] {};
\node at (20.0,-27.0) [blob] {};
\node at (21.0,-27.0) [blob] {};
\node at (22.0,-27.0) [blob] {};
\node at (23.0,-27.0) [blob] {};
\node at (24.0,-27.0) [blob] {};
\draw [edge] (18.0,-27.0) -- (19.0,-27.0);
\draw [edge] (22.0,-27.0) -- (23.0,-27.0);
\draw [box] (24.7,-27.3) rectangle (25.3,-26.7);
\node at (25.0,-27.0) [blob] {};
\node at (18.0,-28.0) [blob] {};
\node at (19.0,-28.0) [blob] {};
\node at (20.0,-28.0) [blob] {};
\node at (21.0,-28.0) [blob] {};
\node at (22.0,-28.0) [blob] {};
\node at (23.0,-28.0) [blob] {};
\node at (24.0,-28.0) [blob] {};
\draw [edge] (18.0,-28.0) -- (19.0,-28.0);
\draw [edge] (23.0,-28.0) -- (24.0,-28.0);
\draw [box] (24.7,-28.3) rectangle (25.3,-27.7);
\node at (25.0,-28.0) [blob] {};
\node at (18.0,-29.0) [blob] {};
\node at (19.0,-29.0) [blob] {};
\node at (20.0,-29.0) [blob] {};
\node at (21.0,-29.0) [blob] {};
\node at (22.0,-29.0) [blob] {};
\node at (23.0,-29.0) [blob] {};
\node at (24.0,-29.0) [blob] {};
\draw [edge] (18.0,-29.0) -- (19.0,-29.0);
\draw [box] (24.7,-29.3) rectangle (25.3,-28.7);
\node at (25.0,-29.0) [blob] {};
\node at (18.0,-30.0) [blob] {};
\node at (19.0,-30.0) [blob] {};
\node at (20.0,-30.0) [blob] {};
\node at (21.0,-30.0) [blob] {};
\node at (22.0,-30.0) [blob] {};
\node at (23.0,-30.0) [blob] {};
\node at (24.0,-30.0) [blob] {};
\draw [edge] (19.0,-30.0) -- (24.0,-30.0);
\draw [box] (24.7,-30.3) rectangle (25.3,-29.7);
\node at (25.0,-30.0) [blob] {};
\node at (18.0,-31.0) [blob] {};
\node at (19.0,-31.0) [blob] {};
\node at (20.0,-31.0) [blob] {};
\node at (21.0,-31.0) [blob] {};
\node at (22.0,-31.0) [blob] {};
\node at (23.0,-31.0) [blob] {};
\node at (24.0,-31.0) [blob] {};
\draw [edge] (19.0,-31.0) -- (23.0,-31.0);
\draw [box] (24.7,-31.3) rectangle (25.3,-30.7);
\node at (25.0,-31.0) [blob] {};
\node at (27.0,-0.0) [blob] {};
\node at (28.0,-0.0) [blob] {};
\node at (29.0,-0.0) [blob] {};
\node at (30.0,-0.0) [blob] {};
\node at (31.0,-0.0) [blob] {};
\node at (32.0,-0.0) [blob] {};
\node at (33.0,-0.0) [blob] {};
\draw [edge] (28.0,-0.0) -- (31.0,-0.0);
\draw [edge] (32.0,-0.0) -- (33.0,-0.0);
\draw [box] (33.7,-0.3) rectangle (34.3,0.3);
\node at (34.0,-0.0) [blob] {};
\node at (27.0,-1.0) [blob] {};
\node at (28.0,-1.0) [blob] {};
\node at (29.0,-1.0) [blob] {};
\node at (30.0,-1.0) [blob] {};
\node at (31.0,-1.0) [blob] {};
\node at (32.0,-1.0) [blob] {};
\node at (33.0,-1.0) [blob] {};
\draw [edge] (28.0,-1.0) -- (31.0,-1.0);
\draw [box] (33.7,-1.3) rectangle (34.3,-0.7);
\node at (34.0,-1.0) [blob] {};
\node at (27.0,-2.0) [blob] {};
\node at (28.0,-2.0) [blob] {};
\node at (29.0,-2.0) [blob] {};
\node at (30.0,-2.0) [blob] {};
\node at (31.0,-2.0) [blob] {};
\node at (32.0,-2.0) [blob] {};
\node at (33.0,-2.0) [blob] {};
\draw [edge] (28.0,-2.0) -- (30.0,-2.0);
\draw [edge] (31.0,-2.0) -- (33.0,-2.0);
\draw [box] (33.7,-2.3) rectangle (34.3,-1.7);
\node at (34.0,-2.0) [blob] {};
\node at (27.0,-3.0) [blob] {};
\node at (28.0,-3.0) [blob] {};
\node at (29.0,-3.0) [blob] {};
\node at (30.0,-3.0) [blob] {};
\node at (31.0,-3.0) [blob] {};
\node at (32.0,-3.0) [blob] {};
\node at (33.0,-3.0) [blob] {};
\draw [edge] (28.0,-3.0) -- (30.0,-3.0);
\draw [edge] (31.0,-3.0) -- (32.0,-3.0);
\draw [box] (33.7,-3.3) rectangle (34.3,-2.7);
\node at (34.0,-3.0) [blob] {};
\node at (27.0,-4.0) [blob] {};
\node at (28.0,-4.0) [blob] {};
\node at (29.0,-4.0) [blob] {};
\node at (30.0,-4.0) [blob] {};
\node at (31.0,-4.0) [blob] {};
\node at (32.0,-4.0) [blob] {};
\node at (33.0,-4.0) [blob] {};
\draw [edge] (28.0,-4.0) -- (30.0,-4.0);
\draw [edge] (32.0,-4.0) -- (33.0,-4.0);
\draw [box] (33.7,-4.3) rectangle (34.3,-3.7);
\node at (34.0,-4.0) [blob] {};
\node at (27.0,-5.0) [blob] {};
\node at (28.0,-5.0) [blob] {};
\node at (29.0,-5.0) [blob] {};
\node at (30.0,-5.0) [blob] {};
\node at (31.0,-5.0) [blob] {};
\node at (32.0,-5.0) [blob] {};
\node at (33.0,-5.0) [blob] {};
\draw [edge] (28.0,-5.0) -- (30.0,-5.0);
\draw [box] (33.7,-5.3) rectangle (34.3,-4.7);
\node at (34.0,-5.0) [blob] {};
\node at (27.0,-6.0) [blob] {};
\node at (28.0,-6.0) [blob] {};
\node at (29.0,-6.0) [blob] {};
\node at (30.0,-6.0) [blob] {};
\node at (31.0,-6.0) [blob] {};
\node at (32.0,-6.0) [blob] {};
\node at (33.0,-6.0) [blob] {};
\draw [edge] (28.0,-6.0) -- (29.0,-6.0);
\draw [edge] (30.0,-6.0) -- (33.0,-6.0);
\draw [box] (33.7,-6.3) rectangle (34.3,-5.7);
\node at (34.0,-6.0) [blob] {};
\node at (27.0,-7.0) [blob] {};
\node at (28.0,-7.0) [blob] {};
\node at (29.0,-7.0) [blob] {};
\node at (30.0,-7.0) [blob] {};
\node at (31.0,-7.0) [blob] {};
\node at (32.0,-7.0) [blob] {};
\node at (33.0,-7.0) [blob] {};
\draw [edge] (28.0,-7.0) -- (29.0,-7.0);
\draw [edge] (30.0,-7.0) -- (32.0,-7.0);
\draw [box] (33.7,-7.3) rectangle (34.3,-6.7);
\node at (34.0,-7.0) [blob] {};
\node at (27.0,-8.0) [blob] {};
\node at (28.0,-8.0) [blob] {};
\node at (29.0,-8.0) [blob] {};
\node at (30.0,-8.0) [blob] {};
\node at (31.0,-8.0) [blob] {};
\node at (32.0,-8.0) [blob] {};
\node at (33.0,-8.0) [blob] {};
\draw [edge] (28.0,-8.0) -- (29.0,-8.0);
\draw [edge] (30.0,-8.0) -- (31.0,-8.0);
\draw [edge] (32.0,-8.0) -- (33.0,-8.0);
\draw [box] (33.7,-8.3) rectangle (34.3,-7.7);
\node at (34.0,-8.0) [blob] {};
\node at (27.0,-9.0) [blob] {};
\node at (28.0,-9.0) [blob] {};
\node at (29.0,-9.0) [blob] {};
\node at (30.0,-9.0) [blob] {};
\node at (31.0,-9.0) [blob] {};
\node at (32.0,-9.0) [blob] {};
\node at (33.0,-9.0) [blob] {};
\draw [edge] (28.0,-9.0) -- (29.0,-9.0);
\draw [edge] (30.0,-9.0) -- (31.0,-9.0);
\draw [box] (33.7,-9.3) rectangle (34.3,-8.7);
\node at (34.0,-9.0) [blob] {};
\node at (27.0,-10.0) [blob] {};
\node at (28.0,-10.0) [blob] {};
\node at (29.0,-10.0) [blob] {};
\node at (30.0,-10.0) [blob] {};
\node at (31.0,-10.0) [blob] {};
\node at (32.0,-10.0) [blob] {};
\node at (33.0,-10.0) [blob] {};
\draw [edge] (28.0,-10.0) -- (29.0,-10.0);
\draw [edge] (31.0,-10.0) -- (33.0,-10.0);
\draw [box] (33.7,-10.3) rectangle (34.3,-9.7);
\node at (34.0,-10.0) [blob] {};
\node at (27.0,-11.0) [blob] {};
\node at (28.0,-11.0) [blob] {};
\node at (29.0,-11.0) [blob] {};
\node at (30.0,-11.0) [blob] {};
\node at (31.0,-11.0) [blob] {};
\node at (32.0,-11.0) [blob] {};
\node at (33.0,-11.0) [blob] {};
\draw [edge] (28.0,-11.0) -- (29.0,-11.0);
\draw [edge] (31.0,-11.0) -- (32.0,-11.0);
\draw [box] (33.7,-11.3) rectangle (34.3,-10.7);
\node at (34.0,-11.0) [blob] {};
\node at (27.0,-12.0) [blob] {};
\node at (28.0,-12.0) [blob] {};
\node at (29.0,-12.0) [blob] {};
\node at (30.0,-12.0) [blob] {};
\node at (31.0,-12.0) [blob] {};
\node at (32.0,-12.0) [blob] {};
\node at (33.0,-12.0) [blob] {};
\draw [edge] (28.0,-12.0) -- (29.0,-12.0);
\draw [edge] (32.0,-12.0) -- (33.0,-12.0);
\draw [box] (33.7,-12.3) rectangle (34.3,-11.7);
\node at (34.0,-12.0) [blob] {};
\node at (27.0,-13.0) [blob] {};
\node at (28.0,-13.0) [blob] {};
\node at (29.0,-13.0) [blob] {};
\node at (30.0,-13.0) [blob] {};
\node at (31.0,-13.0) [blob] {};
\node at (32.0,-13.0) [blob] {};
\node at (33.0,-13.0) [blob] {};
\draw [edge] (28.0,-13.0) -- (29.0,-13.0);
\draw [box] (33.7,-13.3) rectangle (34.3,-12.7);
\node at (34.0,-13.0) [blob] {};
\node at (27.0,-14.0) [blob] {};
\node at (28.0,-14.0) [blob] {};
\node at (29.0,-14.0) [blob] {};
\node at (30.0,-14.0) [blob] {};
\node at (31.0,-14.0) [blob] {};
\node at (32.0,-14.0) [blob] {};
\node at (33.0,-14.0) [blob] {};
\draw [edge] (29.0,-14.0) -- (33.0,-14.0);
\draw [box] (33.7,-14.3) rectangle (34.3,-13.7);
\node at (34.0,-14.0) [blob] {};
\node at (27.0,-15.0) [blob] {};
\node at (28.0,-15.0) [blob] {};
\node at (29.0,-15.0) [blob] {};
\node at (30.0,-15.0) [blob] {};
\node at (31.0,-15.0) [blob] {};
\node at (32.0,-15.0) [blob] {};
\node at (33.0,-15.0) [blob] {};
\draw [edge] (29.0,-15.0) -- (32.0,-15.0);
\draw [box] (33.7,-15.3) rectangle (34.3,-14.7);
\node at (34.0,-15.0) [blob] {};
\node at (27.0,-16.0) [blob] {};
\node at (28.0,-16.0) [blob] {};
\node at (29.0,-16.0) [blob] {};
\node at (30.0,-16.0) [blob] {};
\node at (31.0,-16.0) [blob] {};
\node at (32.0,-16.0) [blob] {};
\node at (33.0,-16.0) [blob] {};
\draw [edge] (29.0,-16.0) -- (31.0,-16.0);
\draw [edge] (32.0,-16.0) -- (33.0,-16.0);
\draw [box] (33.7,-16.3) rectangle (34.3,-15.7);
\node at (34.0,-16.0) [blob] {};
\node at (27.0,-17.0) [blob] {};
\node at (28.0,-17.0) [blob] {};
\node at (29.0,-17.0) [blob] {};
\node at (30.0,-17.0) [blob] {};
\node at (31.0,-17.0) [blob] {};
\node at (32.0,-17.0) [blob] {};
\node at (33.0,-17.0) [blob] {};
\draw [edge] (29.0,-17.0) -- (31.0,-17.0);
\draw [box] (33.7,-17.3) rectangle (34.3,-16.7);
\node at (34.0,-17.0) [blob] {};
\node at (27.0,-18.0) [blob] {};
\node at (28.0,-18.0) [blob] {};
\node at (29.0,-18.0) [blob] {};
\node at (30.0,-18.0) [blob] {};
\node at (31.0,-18.0) [blob] {};
\node at (32.0,-18.0) [blob] {};
\node at (33.0,-18.0) [blob] {};
\draw [edge] (29.0,-18.0) -- (30.0,-18.0);
\draw [edge] (31.0,-18.0) -- (33.0,-18.0);
\draw [box] (33.7,-18.3) rectangle (34.3,-17.7);
\node at (34.0,-18.0) [blob] {};
\node at (27.0,-19.0) [blob] {};
\node at (28.0,-19.0) [blob] {};
\node at (29.0,-19.0) [blob] {};
\node at (30.0,-19.0) [blob] {};
\node at (31.0,-19.0) [blob] {};
\node at (32.0,-19.0) [blob] {};
\node at (33.0,-19.0) [blob] {};
\draw [edge] (29.0,-19.0) -- (30.0,-19.0);
\draw [edge] (31.0,-19.0) -- (32.0,-19.0);
\draw [box] (33.7,-19.3) rectangle (34.3,-18.7);
\node at (34.0,-19.0) [blob] {};
\node at (27.0,-20.0) [blob] {};
\node at (28.0,-20.0) [blob] {};
\node at (29.0,-20.0) [blob] {};
\node at (30.0,-20.0) [blob] {};
\node at (31.0,-20.0) [blob] {};
\node at (32.0,-20.0) [blob] {};
\node at (33.0,-20.0) [blob] {};
\draw [edge] (29.0,-20.0) -- (30.0,-20.0);
\draw [edge] (32.0,-20.0) -- (33.0,-20.0);
\draw [box] (33.7,-20.3) rectangle (34.3,-19.7);
\node at (34.0,-20.0) [blob] {};
\node at (27.0,-21.0) [blob] {};
\node at (28.0,-21.0) [blob] {};
\node at (29.0,-21.0) [blob] {};
\node at (30.0,-21.0) [blob] {};
\node at (31.0,-21.0) [blob] {};
\node at (32.0,-21.0) [blob] {};
\node at (33.0,-21.0) [blob] {};
\draw [edge] (29.0,-21.0) -- (30.0,-21.0);
\draw [box] (33.7,-21.3) rectangle (34.3,-20.7);
\node at (34.0,-21.0) [blob] {};
\node at (27.0,-22.0) [blob] {};
\node at (28.0,-22.0) [blob] {};
\node at (29.0,-22.0) [blob] {};
\node at (30.0,-22.0) [blob] {};
\node at (31.0,-22.0) [blob] {};
\node at (32.0,-22.0) [blob] {};
\node at (33.0,-22.0) [blob] {};
\draw [edge] (30.0,-22.0) -- (33.0,-22.0);
\draw [box] (33.7,-22.3) rectangle (34.3,-21.7);
\node at (34.0,-22.0) [blob] {};
\node at (27.0,-23.0) [blob] {};
\node at (28.0,-23.0) [blob] {};
\node at (29.0,-23.0) [blob] {};
\node at (30.0,-23.0) [blob] {};
\node at (31.0,-23.0) [blob] {};
\node at (32.0,-23.0) [blob] {};
\node at (33.0,-23.0) [blob] {};
\draw [edge] (30.0,-23.0) -- (32.0,-23.0);
\draw [box] (33.7,-23.3) rectangle (34.3,-22.7);
\node at (34.0,-23.0) [blob] {};
\node at (27.0,-24.0) [blob] {};
\node at (28.0,-24.0) [blob] {};
\node at (29.0,-24.0) [blob] {};
\node at (30.0,-24.0) [blob] {};
\node at (31.0,-24.0) [blob] {};
\node at (32.0,-24.0) [blob] {};
\node at (33.0,-24.0) [blob] {};
\draw [edge] (30.0,-24.0) -- (31.0,-24.0);
\draw [edge] (32.0,-24.0) -- (33.0,-24.0);
\draw [box] (33.7,-24.3) rectangle (34.3,-23.7);
\node at (34.0,-24.0) [blob] {};
\node at (27.0,-25.0) [blob] {};
\node at (28.0,-25.0) [blob] {};
\node at (29.0,-25.0) [blob] {};
\node at (30.0,-25.0) [blob] {};
\node at (31.0,-25.0) [blob] {};
\node at (32.0,-25.0) [blob] {};
\node at (33.0,-25.0) [blob] {};
\draw [edge] (30.0,-25.0) -- (31.0,-25.0);
\draw [box] (33.7,-25.3) rectangle (34.3,-24.7);
\node at (34.0,-25.0) [blob] {};
\node at (27.0,-26.0) [blob] {};
\node at (28.0,-26.0) [blob] {};
\node at (29.0,-26.0) [blob] {};
\node at (30.0,-26.0) [blob] {};
\node at (31.0,-26.0) [blob] {};
\node at (32.0,-26.0) [blob] {};
\node at (33.0,-26.0) [blob] {};
\draw [edge] (31.0,-26.0) -- (33.0,-26.0);
\draw [box] (33.7,-26.3) rectangle (34.3,-25.7);
\node at (34.0,-26.0) [blob] {};
\node at (27.0,-27.0) [blob] {};
\node at (28.0,-27.0) [blob] {};
\node at (29.0,-27.0) [blob] {};
\node at (30.0,-27.0) [blob] {};
\node at (31.0,-27.0) [blob] {};
\node at (32.0,-27.0) [blob] {};
\node at (33.0,-27.0) [blob] {};
\draw [edge] (31.0,-27.0) -- (32.0,-27.0);
\draw [box] (33.7,-27.3) rectangle (34.3,-26.7);
\node at (34.0,-27.0) [blob] {};
\node at (27.0,-28.0) [blob] {};
\node at (28.0,-28.0) [blob] {};
\node at (29.0,-28.0) [blob] {};
\node at (30.0,-28.0) [blob] {};
\node at (31.0,-28.0) [blob] {};
\node at (32.0,-28.0) [blob] {};
\node at (33.0,-28.0) [blob] {};
\draw [edge] (32.0,-28.0) -- (33.0,-28.0);
\draw [box] (33.7,-28.3) rectangle (34.3,-27.7);
\node at (34.0,-28.0) [blob] {};
\node at (27.0,-29.0) [blob] {};
\node at (28.0,-29.0) [blob] {};
\node at (29.0,-29.0) [blob] {};
\node at (30.0,-29.0) [blob] {};
\node at (31.0,-29.0) [blob] {};
\node at (32.0,-29.0) [blob] {};
\node at (33.0,-29.0) [blob] {};
\draw [box] (33.7,-29.3) rectangle (34.3,-28.7);
\node at (34.0,-29.0) [blob] {};
\end{tikzpicture}
@@!

<br>
In total, there are 126 possiblities:

<table class="packed">
<thead>
<tr>
<th align="center">*Base*</th>
<th>&nbsp;&nbsp;&nbsp;</th>
<th align="center">$m$</th>
<th>&nbsp;&nbsp;&nbsp;</th>
<th align="center">$2^{m-1}$</th>
</tr>
</thead>
<tbody>
<tr> <td align="center">64</td> <td></td> <td align="center">2</td> <td></td>  <td align="center">2</td> </tr>
<tr> <td align="center">32</td> <td></td> <td align="center">3</td> <td></td>  <td align="center">4</td> </tr>
<tr> <td align="center">16</td> <td></td> <td align="center">4</td> <td></td>  <td align="center">8</td> </tr>
<tr>  <td align="center">8</td> <td></td> <td align="center">5</td> <td></td> <td align="center">16</td> </tr>
<tr>  <td align="center">4</td> <td></td> <td align="center">6</td> <td></td> <td align="center">32</td> </tr>
<tr>  <td align="center">2</td> <td></td> <td align="center">7</td> <td></td> <td align="center">64</td> </tr>
<tr>   <td></td> <td></td>  <td></td> <td></td> <td align="center">**126**</td> </tr>
</tbody>
</table>

It's not too hard to derive the number of plans for an arbitrary
power-of-two input vector length.  Suppose $N=2^M$ and we have base
transforms for all $2^i$, $i = 1, 2, \dots, B$.  Then, if $M > B$,
the total number of plans $P$ is

$$P = \sum_{b=1}^B 2^{M-b-1} = 2^{M-1} \sum_{b=1}^B 2^{-b} = 2^{M-1}
  \frac{2^B-1}{2^B} = 2^{M-B-1} (2^B - 1).$$

For $N=256$ and bases up to $N=64=2^6$, we have $M=8$, $B=6$ and $P =
2 (2^6 - 1) = 126$.  It turns out not to be much harder to find a
general expression for the number of plans in the more complex
mixed-radix case, as we'll see below.

A priori, we can't say much about which of these factorisations is
going to give us the best FFT performance, but we can measure the
performance, using the same benchmarking approach that we've been
taking all along.  We need some code to generate the factorisations
and to produce a value of our `Plan` type from this information, but
given these things we can measure the execution time of the $N=256$
FFT using each of the 126 plans show above.  We'd expect plans using
larger base transforms to do well, since these straight line base
transforms are highly optimised, but it's not obvious what choice of
factorisation for the "left over" factors is going to be faster.

Here are the execution times from the fastest and slowest 20 plans out
of the 126 (for comparison, the execution time using the $2 \times 2
\times 2 \times 2 \times 2 \times 2 \times 2 \times 2$ plan we've been
using up to now is about 145&thinsp;&mu;s -- the top entry in the
right hand column here):

@@! { display: block; margin-left: auto; margin-right: auto; }
\tikzstyle{blob}=[circle,fill=black,inner sep=0.75mm]
\tikzstyle{edge}=[very thick]
\tikzstyle{box}=[draw=gray,very thick,rounded corners]
\tikzstyle{time}=[anchor=west]
\begin{tikzpicture}[scale=\textwidth/22.0cm]
\node at (0.0,-0.0) [blob] {};
\node at (1.0,-0.0) [blob] {};
\node at (2.0,-0.0) [blob] {};
\node at (3.0,-0.0) [blob] {};
\node at (4.0,-0.0) [blob] {};
\node at (5.0,-0.0) [blob] {};
\node at (6.0,-0.0) [blob] {};
\node at (7.0,-0.0) [blob] {};
\draw [edge] (0.0,-0.0) -- (1.0,-0.0);
\draw [box] (1.7,-0.3) rectangle (7.3,0.3);
\node at (2.0,-0.0) [blob] {};
\node at (3.0,-0.0) [blob] {};
\node at (4.0,-0.0) [blob] {};
\node at (5.0,-0.0) [blob] {};
\node at (6.0,-0.0) [blob] {};
\node at (7.0,-0.0) [blob] {};
\node at (7.5,-0.0) [time] {$24.38\,\mathrm{\mu{}s}$};
\node at (0.0,-1.0) [blob] {};
\node at (1.0,-1.0) [blob] {};
\node at (2.0,-1.0) [blob] {};
\node at (3.0,-1.0) [blob] {};
\node at (4.0,-1.0) [blob] {};
\node at (5.0,-1.0) [blob] {};
\node at (6.0,-1.0) [blob] {};
\node at (7.0,-1.0) [blob] {};
\draw [edge] (1.0,-1.0) -- (2.0,-1.0);
\draw [box] (2.7,-1.3) rectangle (7.3,-0.7);
\node at (3.0,-1.0) [blob] {};
\node at (4.0,-1.0) [blob] {};
\node at (5.0,-1.0) [blob] {};
\node at (6.0,-1.0) [blob] {};
\node at (7.0,-1.0) [blob] {};
\node at (7.5,-1.0) [time] {$26.02\,\mathrm{\mu{}s}$};
\node at (0.0,-2.0) [blob] {};
\node at (1.0,-2.0) [blob] {};
\node at (2.0,-2.0) [blob] {};
\node at (3.0,-2.0) [blob] {};
\node at (4.0,-2.0) [blob] {};
\node at (5.0,-2.0) [blob] {};
\node at (6.0,-2.0) [blob] {};
\node at (7.0,-2.0) [blob] {};
\draw [edge] (0.0,-2.0) -- (1.0,-2.0);
\draw [box] (2.7,-2.3) rectangle (7.3,-1.7);
\node at (3.0,-2.0) [blob] {};
\node at (4.0,-2.0) [blob] {};
\node at (5.0,-2.0) [blob] {};
\node at (6.0,-2.0) [blob] {};
\node at (7.0,-2.0) [blob] {};
\node at (7.5,-2.0) [time] {$26.62\,\mathrm{\mu{}s}$};
\node at (0.0,-3.0) [blob] {};
\node at (1.0,-3.0) [blob] {};
\node at (2.0,-3.0) [blob] {};
\node at (3.0,-3.0) [blob] {};
\node at (4.0,-3.0) [blob] {};
\node at (5.0,-3.0) [blob] {};
\node at (6.0,-3.0) [blob] {};
\node at (7.0,-3.0) [blob] {};
\draw [box] (1.7,-3.3) rectangle (7.3,-2.7);
\node at (2.0,-3.0) [blob] {};
\node at (3.0,-3.0) [blob] {};
\node at (4.0,-3.0) [blob] {};
\node at (5.0,-3.0) [blob] {};
\node at (6.0,-3.0) [blob] {};
\node at (7.0,-3.0) [blob] {};
\node at (7.5,-3.0) [time] {$26.70\,\mathrm{\mu{}s}$};
\node at (0.0,-4.0) [blob] {};
\node at (1.0,-4.0) [blob] {};
\node at (2.0,-4.0) [blob] {};
\node at (3.0,-4.0) [blob] {};
\node at (4.0,-4.0) [blob] {};
\node at (5.0,-4.0) [blob] {};
\node at (6.0,-4.0) [blob] {};
\node at (7.0,-4.0) [blob] {};
\draw [edge] (0.0,-4.0) -- (2.0,-4.0);
\draw [box] (2.7,-4.3) rectangle (7.3,-3.7);
\node at (3.0,-4.0) [blob] {};
\node at (4.0,-4.0) [blob] {};
\node at (5.0,-4.0) [blob] {};
\node at (6.0,-4.0) [blob] {};
\node at (7.0,-4.0) [blob] {};
\node at (7.5,-4.0) [time] {$27.79\,\mathrm{\mu{}s}$};
\node at (0.0,-5.0) [blob] {};
\node at (1.0,-5.0) [blob] {};
\node at (2.0,-5.0) [blob] {};
\node at (3.0,-5.0) [blob] {};
\node at (4.0,-5.0) [blob] {};
\node at (5.0,-5.0) [blob] {};
\node at (6.0,-5.0) [blob] {};
\node at (7.0,-5.0) [blob] {};
\draw [box] (2.7,-5.3) rectangle (7.3,-4.7);
\node at (3.0,-5.0) [blob] {};
\node at (4.0,-5.0) [blob] {};
\node at (5.0,-5.0) [blob] {};
\node at (6.0,-5.0) [blob] {};
\node at (7.0,-5.0) [blob] {};
\node at (7.5,-5.0) [time] {$27.84\,\mathrm{\mu{}s}$};
\node at (0.0,-6.0) [blob] {};
\node at (1.0,-6.0) [blob] {};
\node at (2.0,-6.0) [blob] {};
\node at (3.0,-6.0) [blob] {};
\node at (4.0,-6.0) [blob] {};
\node at (5.0,-6.0) [blob] {};
\node at (6.0,-6.0) [blob] {};
\node at (7.0,-6.0) [blob] {};
\draw [edge] (0.0,-6.0) -- (1.0,-6.0);
\draw [edge] (2.0,-6.0) -- (3.0,-6.0);
\draw [box] (3.7,-6.3) rectangle (7.3,-5.7);
\node at (4.0,-6.0) [blob] {};
\node at (5.0,-6.0) [blob] {};
\node at (6.0,-6.0) [blob] {};
\node at (7.0,-6.0) [blob] {};
\node at (7.5,-6.0) [time] {$32.76\,\mathrm{\mu{}s}$};
\node at (0.0,-7.0) [blob] {};
\node at (1.0,-7.0) [blob] {};
\node at (2.0,-7.0) [blob] {};
\node at (3.0,-7.0) [blob] {};
\node at (4.0,-7.0) [blob] {};
\node at (5.0,-7.0) [blob] {};
\node at (6.0,-7.0) [blob] {};
\node at (7.0,-7.0) [blob] {};
\draw [edge] (2.0,-7.0) -- (3.0,-7.0);
\draw [box] (3.7,-7.3) rectangle (7.3,-6.7);
\node at (4.0,-7.0) [blob] {};
\node at (5.0,-7.0) [blob] {};
\node at (6.0,-7.0) [blob] {};
\node at (7.0,-7.0) [blob] {};
\node at (7.5,-7.0) [time] {$33.84\,\mathrm{\mu{}s}$};
\node at (0.0,-8.0) [blob] {};
\node at (1.0,-8.0) [blob] {};
\node at (2.0,-8.0) [blob] {};
\node at (3.0,-8.0) [blob] {};
\node at (4.0,-8.0) [blob] {};
\node at (5.0,-8.0) [blob] {};
\node at (6.0,-8.0) [blob] {};
\node at (7.0,-8.0) [blob] {};
\draw [edge] (0.0,-8.0) -- (1.0,-8.0);
\draw [box] (3.7,-8.3) rectangle (7.3,-7.7);
\node at (4.0,-8.0) [blob] {};
\node at (5.0,-8.0) [blob] {};
\node at (6.0,-8.0) [blob] {};
\node at (7.0,-8.0) [blob] {};
\node at (7.5,-8.0) [time] {$35.21\,\mathrm{\mu{}s}$};
\node at (0.0,-9.0) [blob] {};
\node at (1.0,-9.0) [blob] {};
\node at (2.0,-9.0) [blob] {};
\node at (3.0,-9.0) [blob] {};
\node at (4.0,-9.0) [blob] {};
\node at (5.0,-9.0) [blob] {};
\node at (6.0,-9.0) [blob] {};
\node at (7.0,-9.0) [blob] {};
\draw [edge] (1.0,-9.0) -- (2.0,-9.0);
\draw [box] (3.7,-9.3) rectangle (7.3,-8.7);
\node at (4.0,-9.0) [blob] {};
\node at (5.0,-9.0) [blob] {};
\node at (6.0,-9.0) [blob] {};
\node at (7.0,-9.0) [blob] {};
\node at (7.5,-9.0) [time] {$35.24\,\mathrm{\mu{}s}$};
\node at (0.0,-10.0) [blob] {};
\node at (1.0,-10.0) [blob] {};
\node at (2.0,-10.0) [blob] {};
\node at (3.0,-10.0) [blob] {};
\node at (4.0,-10.0) [blob] {};
\node at (5.0,-10.0) [blob] {};
\node at (6.0,-10.0) [blob] {};
\node at (7.0,-10.0) [blob] {};
\draw [box] (3.7,-10.3) rectangle (7.3,-9.7);
\node at (4.0,-10.0) [blob] {};
\node at (5.0,-10.0) [blob] {};
\node at (6.0,-10.0) [blob] {};
\node at (7.0,-10.0) [blob] {};
\node at (7.5,-10.0) [time] {$36.89\,\mathrm{\mu{}s}$};
\node at (0.0,-11.0) [blob] {};
\node at (1.0,-11.0) [blob] {};
\node at (2.0,-11.0) [blob] {};
\node at (3.0,-11.0) [blob] {};
\node at (4.0,-11.0) [blob] {};
\node at (5.0,-11.0) [blob] {};
\node at (6.0,-11.0) [blob] {};
\node at (7.0,-11.0) [blob] {};
\draw [edge] (0.0,-11.0) -- (2.0,-11.0);
\draw [box] (3.7,-11.3) rectangle (7.3,-10.7);
\node at (4.0,-11.0) [blob] {};
\node at (5.0,-11.0) [blob] {};
\node at (6.0,-11.0) [blob] {};
\node at (7.0,-11.0) [blob] {};
\node at (7.5,-11.0) [time] {$37.80\,\mathrm{\mu{}s}$};
\node at (0.0,-12.0) [blob] {};
\node at (1.0,-12.0) [blob] {};
\node at (2.0,-12.0) [blob] {};
\node at (3.0,-12.0) [blob] {};
\node at (4.0,-12.0) [blob] {};
\node at (5.0,-12.0) [blob] {};
\node at (6.0,-12.0) [blob] {};
\node at (7.0,-12.0) [blob] {};
\draw [edge] (1.0,-12.0) -- (3.0,-12.0);
\draw [box] (3.7,-12.3) rectangle (7.3,-11.7);
\node at (4.0,-12.0) [blob] {};
\node at (5.0,-12.0) [blob] {};
\node at (6.0,-12.0) [blob] {};
\node at (7.0,-12.0) [blob] {};
\node at (7.5,-12.0) [time] {$37.98\,\mathrm{\mu{}s}$};
\node at (0.0,-13.0) [blob] {};
\node at (1.0,-13.0) [blob] {};
\node at (2.0,-13.0) [blob] {};
\node at (3.0,-13.0) [blob] {};
\node at (4.0,-13.0) [blob] {};
\node at (5.0,-13.0) [blob] {};
\node at (6.0,-13.0) [blob] {};
\node at (7.0,-13.0) [blob] {};
\draw [edge] (1.0,-13.0) -- (2.0,-13.0);
\draw [edge] (3.0,-13.0) -- (4.0,-13.0);
\draw [box] (4.7,-13.3) rectangle (7.3,-12.7);
\node at (5.0,-13.0) [blob] {};
\node at (6.0,-13.0) [blob] {};
\node at (7.0,-13.0) [blob] {};
\node at (7.5,-13.0) [time] {$45.97\,\mathrm{\mu{}s}$};
\node at (0.0,-14.0) [blob] {};
\node at (1.0,-14.0) [blob] {};
\node at (2.0,-14.0) [blob] {};
\node at (3.0,-14.0) [blob] {};
\node at (4.0,-14.0) [blob] {};
\node at (5.0,-14.0) [blob] {};
\node at (6.0,-14.0) [blob] {};
\node at (7.0,-14.0) [blob] {};
\draw [edge] (0.0,-14.0) -- (1.0,-14.0);
\draw [edge] (3.0,-14.0) -- (4.0,-14.0);
\draw [box] (4.7,-14.3) rectangle (7.3,-13.7);
\node at (5.0,-14.0) [blob] {};
\node at (6.0,-14.0) [blob] {};
\node at (7.0,-14.0) [blob] {};
\node at (7.5,-14.0) [time] {$46.17\,\mathrm{\mu{}s}$};
\node at (0.0,-15.0) [blob] {};
\node at (1.0,-15.0) [blob] {};
\node at (2.0,-15.0) [blob] {};
\node at (3.0,-15.0) [blob] {};
\node at (4.0,-15.0) [blob] {};
\node at (5.0,-15.0) [blob] {};
\node at (6.0,-15.0) [blob] {};
\node at (7.0,-15.0) [blob] {};
\draw [edge] (3.0,-15.0) -- (4.0,-15.0);
\draw [box] (4.7,-15.3) rectangle (7.3,-14.7);
\node at (5.0,-15.0) [blob] {};
\node at (6.0,-15.0) [blob] {};
\node at (7.0,-15.0) [blob] {};
\node at (7.5,-15.0) [time] {$46.92\,\mathrm{\mu{}s}$};
\node at (0.0,-16.0) [blob] {};
\node at (1.0,-16.0) [blob] {};
\node at (2.0,-16.0) [blob] {};
\node at (3.0,-16.0) [blob] {};
\node at (4.0,-16.0) [blob] {};
\node at (5.0,-16.0) [blob] {};
\node at (6.0,-16.0) [blob] {};
\node at (7.0,-16.0) [blob] {};
\draw [edge] (0.0,-16.0) -- (2.0,-16.0);
\draw [edge] (3.0,-16.0) -- (4.0,-16.0);
\draw [box] (4.7,-16.3) rectangle (7.3,-15.7);
\node at (5.0,-16.0) [blob] {};
\node at (6.0,-16.0) [blob] {};
\node at (7.0,-16.0) [blob] {};
\node at (7.5,-16.0) [time] {$48.62\,\mathrm{\mu{}s}$};
\node at (0.0,-17.0) [blob] {};
\node at (1.0,-17.0) [blob] {};
\node at (2.0,-17.0) [blob] {};
\node at (3.0,-17.0) [blob] {};
\node at (4.0,-17.0) [blob] {};
\node at (5.0,-17.0) [blob] {};
\node at (6.0,-17.0) [blob] {};
\node at (7.0,-17.0) [blob] {};
\draw [edge] (0.0,-17.0) -- (1.0,-17.0);
\draw [edge] (2.0,-17.0) -- (4.0,-17.0);
\draw [box] (4.7,-17.3) rectangle (7.3,-16.7);
\node at (5.0,-17.0) [blob] {};
\node at (6.0,-17.0) [blob] {};
\node at (7.0,-17.0) [blob] {};
\node at (7.5,-17.0) [time] {$49.02\,\mathrm{\mu{}s}$};
\node at (0.0,-18.0) [blob] {};
\node at (1.0,-18.0) [blob] {};
\node at (2.0,-18.0) [blob] {};
\node at (3.0,-18.0) [blob] {};
\node at (4.0,-18.0) [blob] {};
\node at (5.0,-18.0) [blob] {};
\node at (6.0,-18.0) [blob] {};
\node at (7.0,-18.0) [blob] {};
\draw [edge] (0.0,-18.0) -- (1.0,-18.0);
\draw [edge] (2.0,-18.0) -- (3.0,-18.0);
\draw [box] (4.7,-18.3) rectangle (7.3,-17.7);
\node at (5.0,-18.0) [blob] {};
\node at (6.0,-18.0) [blob] {};
\node at (7.0,-18.0) [blob] {};
\node at (7.5,-18.0) [time] {$49.33\,\mathrm{\mu{}s}$};
\node at (0.0,-19.0) [blob] {};
\node at (1.0,-19.0) [blob] {};
\node at (2.0,-19.0) [blob] {};
\node at (3.0,-19.0) [blob] {};
\node at (4.0,-19.0) [blob] {};
\node at (5.0,-19.0) [blob] {};
\node at (6.0,-19.0) [blob] {};
\node at (7.0,-19.0) [blob] {};
\draw [edge] (0.0,-19.0) -- (3.0,-19.0);
\draw [box] (3.7,-19.3) rectangle (7.3,-18.7);
\node at (4.0,-19.0) [blob] {};
\node at (5.0,-19.0) [blob] {};
\node at (6.0,-19.0) [blob] {};
\node at (7.0,-19.0) [blob] {};
\node at (7.5,-19.0) [time] {$49.51\,\mathrm{\mu{}s}$};
\node at (11.0,-0.0) [blob] {};
\node at (12.0,-0.0) [blob] {};
\node at (13.0,-0.0) [blob] {};
\node at (14.0,-0.0) [blob] {};
\node at (15.0,-0.0) [blob] {};
\node at (16.0,-0.0) [blob] {};
\node at (17.0,-0.0) [blob] {};
\node at (18.0,-0.0) [blob] {};
\draw [box] (17.7,-0.3) rectangle (18.3,0.3);
\node at (18.0,-0.0) [blob] {};
\node at (18.5,-0.0) [time] {$146.12\,\mathrm{\mu{}s}$};
\node at (11.0,-1.0) [blob] {};
\node at (12.0,-1.0) [blob] {};
\node at (13.0,-1.0) [blob] {};
\node at (14.0,-1.0) [blob] {};
\node at (15.0,-1.0) [blob] {};
\node at (16.0,-1.0) [blob] {};
\node at (17.0,-1.0) [blob] {};
\node at (18.0,-1.0) [blob] {};
\draw [edge] (11.0,-1.0) -- (14.0,-1.0);
\draw [edge] (15.0,-1.0) -- (16.0,-1.0);
\draw [box] (17.7,-1.3) rectangle (18.3,-0.7);
\node at (18.0,-1.0) [blob] {};
\node at (18.5,-1.0) [time] {$147.62\,\mathrm{\mu{}s}$};
\node at (11.0,-2.0) [blob] {};
\node at (12.0,-2.0) [blob] {};
\node at (13.0,-2.0) [blob] {};
\node at (14.0,-2.0) [blob] {};
\node at (15.0,-2.0) [blob] {};
\node at (16.0,-2.0) [blob] {};
\node at (17.0,-2.0) [blob] {};
\node at (18.0,-2.0) [blob] {};
\draw [edge] (12.0,-2.0) -- (15.0,-2.0);
\draw [box] (17.7,-2.3) rectangle (18.3,-1.7);
\node at (18.0,-2.0) [blob] {};
\node at (18.5,-2.0) [time] {$157.95\,\mathrm{\mu{}s}$};
\node at (11.0,-3.0) [blob] {};
\node at (12.0,-3.0) [blob] {};
\node at (13.0,-3.0) [blob] {};
\node at (14.0,-3.0) [blob] {};
\node at (15.0,-3.0) [blob] {};
\node at (16.0,-3.0) [blob] {};
\node at (17.0,-3.0) [blob] {};
\node at (18.0,-3.0) [blob] {};
\draw [edge] (11.0,-3.0) -- (14.0,-3.0);
\draw [box] (17.7,-3.3) rectangle (18.3,-2.7);
\node at (18.0,-3.0) [blob] {};
\node at (18.5,-3.0) [time] {$158.17\,\mathrm{\mu{}s}$};
\node at (11.0,-4.0) [blob] {};
\node at (12.0,-4.0) [blob] {};
\node at (13.0,-4.0) [blob] {};
\node at (14.0,-4.0) [blob] {};
\node at (15.0,-4.0) [blob] {};
\node at (16.0,-4.0) [blob] {};
\node at (17.0,-4.0) [blob] {};
\node at (18.0,-4.0) [blob] {};
\draw [edge] (11.0,-4.0) -- (12.0,-4.0);
\draw [edge] (13.0,-4.0) -- (16.0,-4.0);
\draw [box] (17.7,-4.3) rectangle (18.3,-3.7);
\node at (18.0,-4.0) [blob] {};
\node at (18.5,-4.0) [time] {$161.84\,\mathrm{\mu{}s}$};
\node at (11.0,-5.0) [blob] {};
\node at (12.0,-5.0) [blob] {};
\node at (13.0,-5.0) [blob] {};
\node at (14.0,-5.0) [blob] {};
\node at (15.0,-5.0) [blob] {};
\node at (16.0,-5.0) [blob] {};
\node at (17.0,-5.0) [blob] {};
\node at (18.0,-5.0) [blob] {};
\draw [edge] (13.0,-5.0) -- (16.0,-5.0);
\draw [box] (17.7,-5.3) rectangle (18.3,-4.7);
\node at (18.0,-5.0) [blob] {};
\node at (18.5,-5.0) [time] {$161.89\,\mathrm{\mu{}s}$};
\node at (11.0,-6.0) [blob] {};
\node at (12.0,-6.0) [blob] {};
\node at (13.0,-6.0) [blob] {};
\node at (14.0,-6.0) [blob] {};
\node at (15.0,-6.0) [blob] {};
\node at (16.0,-6.0) [blob] {};
\node at (17.0,-6.0) [blob] {};
\node at (18.0,-6.0) [blob] {};
\draw [edge] (12.0,-6.0) -- (16.0,-6.0);
\draw [box] (16.7,-6.3) rectangle (18.3,-5.7);
\node at (17.0,-6.0) [blob] {};
\node at (18.0,-6.0) [blob] {};
\node at (18.5,-6.0) [time] {$165.00\,\mathrm{\mu{}s}$};
\node at (11.0,-7.0) [blob] {};
\node at (12.0,-7.0) [blob] {};
\node at (13.0,-7.0) [blob] {};
\node at (14.0,-7.0) [blob] {};
\node at (15.0,-7.0) [blob] {};
\node at (16.0,-7.0) [blob] {};
\node at (17.0,-7.0) [blob] {};
\node at (18.0,-7.0) [blob] {};
\draw [edge] (11.0,-7.0) -- (15.0,-7.0);
\draw [edge] (16.0,-7.0) -- (17.0,-7.0);
\draw [box] (17.7,-7.3) rectangle (18.3,-6.7);
\node at (18.0,-7.0) [blob] {};
\node at (18.5,-7.0) [time] {$168.15\,\mathrm{\mu{}s}$};
\node at (11.0,-8.0) [blob] {};
\node at (12.0,-8.0) [blob] {};
\node at (13.0,-8.0) [blob] {};
\node at (14.0,-8.0) [blob] {};
\node at (15.0,-8.0) [blob] {};
\node at (16.0,-8.0) [blob] {};
\node at (17.0,-8.0) [blob] {};
\node at (18.0,-8.0) [blob] {};
\draw [edge] (12.0,-8.0) -- (13.0,-8.0);
\draw [edge] (14.0,-8.0) -- (17.0,-8.0);
\draw [box] (17.7,-8.3) rectangle (18.3,-7.7);
\node at (18.0,-8.0) [blob] {};
\node at (18.5,-8.0) [time] {$169.67\,\mathrm{\mu{}s}$};
\node at (11.0,-9.0) [blob] {};
\node at (12.0,-9.0) [blob] {};
\node at (13.0,-9.0) [blob] {};
\node at (14.0,-9.0) [blob] {};
\node at (15.0,-9.0) [blob] {};
\node at (16.0,-9.0) [blob] {};
\node at (17.0,-9.0) [blob] {};
\node at (18.0,-9.0) [blob] {};
\draw [edge] (11.0,-9.0) -- (12.0,-9.0);
\draw [edge] (14.0,-9.0) -- (17.0,-9.0);
\draw [box] (17.7,-9.3) rectangle (18.3,-8.7);
\node at (18.0,-9.0) [blob] {};
\node at (18.5,-9.0) [time] {$170.99\,\mathrm{\mu{}s}$};
\node at (11.0,-10.0) [blob] {};
\node at (12.0,-10.0) [blob] {};
\node at (13.0,-10.0) [blob] {};
\node at (14.0,-10.0) [blob] {};
\node at (15.0,-10.0) [blob] {};
\node at (16.0,-10.0) [blob] {};
\node at (17.0,-10.0) [blob] {};
\node at (18.0,-10.0) [blob] {};
\draw [edge] (11.0,-10.0) -- (13.0,-10.0);
\draw [edge] (14.0,-10.0) -- (17.0,-10.0);
\draw [box] (17.7,-10.3) rectangle (18.3,-9.7);
\node at (18.0,-10.0) [blob] {};
\node at (18.5,-10.0) [time] {$171.90\,\mathrm{\mu{}s}$};
\node at (11.0,-11.0) [blob] {};
\node at (12.0,-11.0) [blob] {};
\node at (13.0,-11.0) [blob] {};
\node at (14.0,-11.0) [blob] {};
\node at (15.0,-11.0) [blob] {};
\node at (16.0,-11.0) [blob] {};
\node at (17.0,-11.0) [blob] {};
\node at (18.0,-11.0) [blob] {};
\draw [edge] (14.0,-11.0) -- (17.0,-11.0);
\draw [box] (17.7,-11.3) rectangle (18.3,-10.7);
\node at (18.0,-11.0) [blob] {};
\node at (18.5,-11.0) [time] {$174.00\,\mathrm{\mu{}s}$};
\node at (11.0,-12.0) [blob] {};
\node at (12.0,-12.0) [blob] {};
\node at (13.0,-12.0) [blob] {};
\node at (14.0,-12.0) [blob] {};
\node at (15.0,-12.0) [blob] {};
\node at (16.0,-12.0) [blob] {};
\node at (17.0,-12.0) [blob] {};
\node at (18.0,-12.0) [blob] {};
\draw [edge] (11.0,-12.0) -- (15.0,-12.0);
\draw [box] (17.7,-12.3) rectangle (18.3,-11.7);
\node at (18.0,-12.0) [blob] {};
\node at (18.5,-12.0) [time] {$198.82\,\mathrm{\mu{}s}$};
\node at (11.0,-13.0) [blob] {};
\node at (12.0,-13.0) [blob] {};
\node at (13.0,-13.0) [blob] {};
\node at (14.0,-13.0) [blob] {};
\node at (15.0,-13.0) [blob] {};
\node at (16.0,-13.0) [blob] {};
\node at (17.0,-13.0) [blob] {};
\node at (18.0,-13.0) [blob] {};
\draw [edge] (12.0,-13.0) -- (16.0,-13.0);
\draw [box] (17.7,-13.3) rectangle (18.3,-12.7);
\node at (18.0,-13.0) [blob] {};
\node at (18.5,-13.0) [time] {$224.48\,\mathrm{\mu{}s}$};
\node at (11.0,-14.0) [blob] {};
\node at (12.0,-14.0) [blob] {};
\node at (13.0,-14.0) [blob] {};
\node at (14.0,-14.0) [blob] {};
\node at (15.0,-14.0) [blob] {};
\node at (16.0,-14.0) [blob] {};
\node at (17.0,-14.0) [blob] {};
\node at (18.0,-14.0) [blob] {};
\draw [edge] (11.0,-14.0) -- (12.0,-14.0);
\draw [edge] (13.0,-14.0) -- (17.0,-14.0);
\draw [box] (17.7,-14.3) rectangle (18.3,-13.7);
\node at (18.0,-14.0) [blob] {};
\node at (18.5,-14.0) [time] {$277.53\,\mathrm{\mu{}s}$};
\node at (11.0,-15.0) [blob] {};
\node at (12.0,-15.0) [blob] {};
\node at (13.0,-15.0) [blob] {};
\node at (14.0,-15.0) [blob] {};
\node at (15.0,-15.0) [blob] {};
\node at (16.0,-15.0) [blob] {};
\node at (17.0,-15.0) [blob] {};
\node at (18.0,-15.0) [blob] {};
\draw [edge] (13.0,-15.0) -- (17.0,-15.0);
\draw [box] (17.7,-15.3) rectangle (18.3,-14.7);
\node at (18.0,-15.0) [blob] {};
\node at (18.5,-15.0) [time] {$279.93\,\mathrm{\mu{}s}$};
\node at (11.0,-16.0) [blob] {};
\node at (12.0,-16.0) [blob] {};
\node at (13.0,-16.0) [blob] {};
\node at (14.0,-16.0) [blob] {};
\node at (15.0,-16.0) [blob] {};
\node at (16.0,-16.0) [blob] {};
\node at (17.0,-16.0) [blob] {};
\node at (18.0,-16.0) [blob] {};
\draw [edge] (11.0,-16.0) -- (16.0,-16.0);
\draw [box] (16.7,-16.3) rectangle (18.3,-15.7);
\node at (17.0,-16.0) [blob] {};
\node at (18.0,-16.0) [blob] {};
\node at (18.5,-16.0) [time] {$292.99\,\mathrm{\mu{}s}$};
\node at (11.0,-17.0) [blob] {};
\node at (12.0,-17.0) [blob] {};
\node at (13.0,-17.0) [blob] {};
\node at (14.0,-17.0) [blob] {};
\node at (15.0,-17.0) [blob] {};
\node at (16.0,-17.0) [blob] {};
\node at (17.0,-17.0) [blob] {};
\node at (18.0,-17.0) [blob] {};
\draw [edge] (11.0,-17.0) -- (16.0,-17.0);
\draw [box] (17.7,-17.3) rectangle (18.3,-16.7);
\node at (18.0,-17.0) [blob] {};
\node at (18.5,-17.0) [time] {$346.78\,\mathrm{\mu{}s}$};
\node at (11.0,-18.0) [blob] {};
\node at (12.0,-18.0) [blob] {};
\node at (13.0,-18.0) [blob] {};
\node at (14.0,-18.0) [blob] {};
\node at (15.0,-18.0) [blob] {};
\node at (16.0,-18.0) [blob] {};
\node at (17.0,-18.0) [blob] {};
\node at (18.0,-18.0) [blob] {};
\draw [edge] (12.0,-18.0) -- (17.0,-18.0);
\draw [box] (17.7,-18.3) rectangle (18.3,-17.7);
\node at (18.0,-18.0) [blob] {};
\node at (18.5,-18.0) [time] {$489.60\,\mathrm{\mu{}s}$};
\node at (11.0,-19.0) [blob] {};
\node at (12.0,-19.0) [blob] {};
\node at (13.0,-19.0) [blob] {};
\node at (14.0,-19.0) [blob] {};
\node at (15.0,-19.0) [blob] {};
\node at (16.0,-19.0) [blob] {};
\node at (17.0,-19.0) [blob] {};
\node at (18.0,-19.0) [blob] {};
\draw [edge] (11.0,-19.0) -- (17.0,-19.0);
\draw [box] (17.7,-19.3) rectangle (18.3,-18.7);
\node at (18.0,-19.0) [blob] {};
\node at (18.5,-19.0) [time] {$942.40\,\mathrm{\mu{}s}$};
\end{tikzpicture}
@@!

<br>
Two things stand out from these results.  First, the fastest
transforms are those using the largest base transforms ($N=64$ or
$N=32$).  This isn't too much of a surprise, since these plans offload
the largest proportion of the FFT processing to optimised straight
line code.  Conversely, the slowest plans are those that use the
smallest ($N=2$ or $N=4$) base transforms.  Further, the slowest of
the slow transforms appear to be those that use the largest
Danielson-Lanczos steps.  For example, the overall slowest plan (by a
factor of about two in time) uses an $N=2$ base transform and a single
Danielson-Lanczos step of size 128.  These larger Danielson-Lanczos
steps could be more efficient if we had specialised "twiddlets" for
them, but as it is currently, they do a lot of wasted work and so are
less efficient than a series of smaller decompositions.

Exactly where the trade-off between larger and smaller
Danielson-Lanczos steps lies isn't immediately obvious.  For example,
the fastest plans using an $N=16$ base transform use $4 \times 4$, $2
\times 2 \times 4$ and $4 \times 2 \times 2$ Danielson-Lanczos steps
for the other factors, but the time differences are small, perhaps not
even larger than the margin of error in measurement.  We can get a
further idea of how this trade-off works by looking at the best plans
for $N=1024$.  Here shows the best 40
plans for this input length size (for comparison, the execution time
for the "standard" $N=1024$ transform from the earlier articles is
about 550&thinsp;&mu;s):

@@! { display: block; margin-left: auto; margin-right: auto; }
\tikzstyle{blob}=[circle,fill=black,inner sep=0.75mm]
\tikzstyle{edge}=[very thick]
\tikzstyle{box}=[draw=gray,very thick,rounded corners]
\tikzstyle{time}=[anchor=west]
\begin{tikzpicture}[scale=\textwidth/29.0cm]
\node at (0.0,-0.0) [blob] {};
\node at (1.0,-0.0) [blob] {};
\node at (2.0,-0.0) [blob] {};
\node at (3.0,-0.0) [blob] {};
\node at (4.0,-0.0) [blob] {};
\node at (5.0,-0.0) [blob] {};
\node at (6.0,-0.0) [blob] {};
\node at (7.0,-0.0) [blob] {};
\node at (8.0,-0.0) [blob] {};
\node at (9.0,-0.0) [blob] {};
\draw [edge] (2.0,-0.0) -- (3.0,-0.0);
\draw [box] (3.7,-0.3) rectangle (9.3,0.3);
\node at (4.0,-0.0) [blob] {};
\node at (5.0,-0.0) [blob] {};
\node at (6.0,-0.0) [blob] {};
\node at (7.0,-0.0) [blob] {};
\node at (8.0,-0.0) [blob] {};
\node at (9.0,-0.0) [blob] {};
\node at (9.25,-0.0) [time] {$116.84\,\mathrm{\mu{}s}$};
\node at (0.0,-1.0) [blob] {};
\node at (1.0,-1.0) [blob] {};
\node at (2.0,-1.0) [blob] {};
\node at (3.0,-1.0) [blob] {};
\node at (4.0,-1.0) [blob] {};
\node at (5.0,-1.0) [blob] {};
\node at (6.0,-1.0) [blob] {};
\node at (7.0,-1.0) [blob] {};
\node at (8.0,-1.0) [blob] {};
\node at (9.0,-1.0) [blob] {};
\draw [edge] (0.0,-1.0) -- (1.0,-1.0);
\draw [edge] (2.0,-1.0) -- (3.0,-1.0);
\draw [box] (3.7,-1.3) rectangle (9.3,-0.7);
\node at (4.0,-1.0) [blob] {};
\node at (5.0,-1.0) [blob] {};
\node at (6.0,-1.0) [blob] {};
\node at (7.0,-1.0) [blob] {};
\node at (8.0,-1.0) [blob] {};
\node at (9.0,-1.0) [blob] {};
\node at (9.25,-1.0) [time] {$117.29\,\mathrm{\mu{}s}$};
\node at (0.0,-2.0) [blob] {};
\node at (1.0,-2.0) [blob] {};
\node at (2.0,-2.0) [blob] {};
\node at (3.0,-2.0) [blob] {};
\node at (4.0,-2.0) [blob] {};
\node at (5.0,-2.0) [blob] {};
\node at (6.0,-2.0) [blob] {};
\node at (7.0,-2.0) [blob] {};
\node at (8.0,-2.0) [blob] {};
\node at (9.0,-2.0) [blob] {};
\draw [edge] (0.0,-2.0) -- (1.0,-2.0);
\draw [edge] (3.0,-2.0) -- (4.0,-2.0);
\draw [box] (4.7,-2.3) rectangle (9.3,-1.7);
\node at (5.0,-2.0) [blob] {};
\node at (6.0,-2.0) [blob] {};
\node at (7.0,-2.0) [blob] {};
\node at (8.0,-2.0) [blob] {};
\node at (9.0,-2.0) [blob] {};
\node at (9.25,-2.0) [time] {$118.30\,\mathrm{\mu{}s}$};
\node at (0.0,-3.0) [blob] {};
\node at (1.0,-3.0) [blob] {};
\node at (2.0,-3.0) [blob] {};
\node at (3.0,-3.0) [blob] {};
\node at (4.0,-3.0) [blob] {};
\node at (5.0,-3.0) [blob] {};
\node at (6.0,-3.0) [blob] {};
\node at (7.0,-3.0) [blob] {};
\node at (8.0,-3.0) [blob] {};
\node at (9.0,-3.0) [blob] {};
\draw [edge] (3.0,-3.0) -- (4.0,-3.0);
\draw [box] (4.7,-3.3) rectangle (9.3,-2.7);
\node at (5.0,-3.0) [blob] {};
\node at (6.0,-3.0) [blob] {};
\node at (7.0,-3.0) [blob] {};
\node at (8.0,-3.0) [blob] {};
\node at (9.0,-3.0) [blob] {};
\node at (9.25,-3.0) [time] {$119.48\,\mathrm{\mu{}s}$};
\node at (0.0,-4.0) [blob] {};
\node at (1.0,-4.0) [blob] {};
\node at (2.0,-4.0) [blob] {};
\node at (3.0,-4.0) [blob] {};
\node at (4.0,-4.0) [blob] {};
\node at (5.0,-4.0) [blob] {};
\node at (6.0,-4.0) [blob] {};
\node at (7.0,-4.0) [blob] {};
\node at (8.0,-4.0) [blob] {};
\node at (9.0,-4.0) [blob] {};
\draw [edge] (1.0,-4.0) -- (2.0,-4.0);
\draw [edge] (3.0,-4.0) -- (4.0,-4.0);
\draw [box] (4.7,-4.3) rectangle (9.3,-3.7);
\node at (5.0,-4.0) [blob] {};
\node at (6.0,-4.0) [blob] {};
\node at (7.0,-4.0) [blob] {};
\node at (8.0,-4.0) [blob] {};
\node at (9.0,-4.0) [blob] {};
\node at (9.25,-4.0) [time] {$119.54\,\mathrm{\mu{}s}$};
\node at (0.0,-5.0) [blob] {};
\node at (1.0,-5.0) [blob] {};
\node at (2.0,-5.0) [blob] {};
\node at (3.0,-5.0) [blob] {};
\node at (4.0,-5.0) [blob] {};
\node at (5.0,-5.0) [blob] {};
\node at (6.0,-5.0) [blob] {};
\node at (7.0,-5.0) [blob] {};
\node at (8.0,-5.0) [blob] {};
\node at (9.0,-5.0) [blob] {};
\draw [edge] (0.0,-5.0) -- (1.0,-5.0);
\draw [box] (3.7,-5.3) rectangle (9.3,-4.7);
\node at (4.0,-5.0) [blob] {};
\node at (5.0,-5.0) [blob] {};
\node at (6.0,-5.0) [blob] {};
\node at (7.0,-5.0) [blob] {};
\node at (8.0,-5.0) [blob] {};
\node at (9.0,-5.0) [blob] {};
\node at (9.25,-5.0) [time] {$119.61\,\mathrm{\mu{}s}$};
\node at (0.0,-6.0) [blob] {};
\node at (1.0,-6.0) [blob] {};
\node at (2.0,-6.0) [blob] {};
\node at (3.0,-6.0) [blob] {};
\node at (4.0,-6.0) [blob] {};
\node at (5.0,-6.0) [blob] {};
\node at (6.0,-6.0) [blob] {};
\node at (7.0,-6.0) [blob] {};
\node at (8.0,-6.0) [blob] {};
\node at (9.0,-6.0) [blob] {};
\draw [edge] (0.0,-6.0) -- (1.0,-6.0);
\draw [edge] (2.0,-6.0) -- (3.0,-6.0);
\draw [box] (4.7,-6.3) rectangle (9.3,-5.7);
\node at (5.0,-6.0) [blob] {};
\node at (6.0,-6.0) [blob] {};
\node at (7.0,-6.0) [blob] {};
\node at (8.0,-6.0) [blob] {};
\node at (9.0,-6.0) [blob] {};
\node at (9.25,-6.0) [time] {$121.89\,\mathrm{\mu{}s}$};
\node at (0.0,-7.0) [blob] {};
\node at (1.0,-7.0) [blob] {};
\node at (2.0,-7.0) [blob] {};
\node at (3.0,-7.0) [blob] {};
\node at (4.0,-7.0) [blob] {};
\node at (5.0,-7.0) [blob] {};
\node at (6.0,-7.0) [blob] {};
\node at (7.0,-7.0) [blob] {};
\node at (8.0,-7.0) [blob] {};
\node at (9.0,-7.0) [blob] {};
\draw [edge] (1.0,-7.0) -- (2.0,-7.0);
\draw [box] (3.7,-7.3) rectangle (9.3,-6.7);
\node at (4.0,-7.0) [blob] {};
\node at (5.0,-7.0) [blob] {};
\node at (6.0,-7.0) [blob] {};
\node at (7.0,-7.0) [blob] {};
\node at (8.0,-7.0) [blob] {};
\node at (9.0,-7.0) [blob] {};
\node at (9.25,-7.0) [time] {$122.28\,\mathrm{\mu{}s}$};
\node at (0.0,-8.0) [blob] {};
\node at (1.0,-8.0) [blob] {};
\node at (2.0,-8.0) [blob] {};
\node at (3.0,-8.0) [blob] {};
\node at (4.0,-8.0) [blob] {};
\node at (5.0,-8.0) [blob] {};
\node at (6.0,-8.0) [blob] {};
\node at (7.0,-8.0) [blob] {};
\node at (8.0,-8.0) [blob] {};
\node at (9.0,-8.0) [blob] {};
\draw [box] (3.7,-8.3) rectangle (9.3,-7.7);
\node at (4.0,-8.0) [blob] {};
\node at (5.0,-8.0) [blob] {};
\node at (6.0,-8.0) [blob] {};
\node at (7.0,-8.0) [blob] {};
\node at (8.0,-8.0) [blob] {};
\node at (9.0,-8.0) [blob] {};
\node at (9.25,-8.0) [time] {$122.32\,\mathrm{\mu{}s}$};
\node at (0.0,-9.0) [blob] {};
\node at (1.0,-9.0) [blob] {};
\node at (2.0,-9.0) [blob] {};
\node at (3.0,-9.0) [blob] {};
\node at (4.0,-9.0) [blob] {};
\node at (5.0,-9.0) [blob] {};
\node at (6.0,-9.0) [blob] {};
\node at (7.0,-9.0) [blob] {};
\node at (8.0,-9.0) [blob] {};
\node at (9.0,-9.0) [blob] {};
\draw [edge] (0.0,-9.0) -- (1.0,-9.0);
\draw [box] (4.7,-9.3) rectangle (9.3,-8.7);
\node at (5.0,-9.0) [blob] {};
\node at (6.0,-9.0) [blob] {};
\node at (7.0,-9.0) [blob] {};
\node at (8.0,-9.0) [blob] {};
\node at (9.0,-9.0) [blob] {};
\node at (9.25,-9.0) [time] {$123.53\,\mathrm{\mu{}s}$};
\node at (0.0,-10.0) [blob] {};
\node at (1.0,-10.0) [blob] {};
\node at (2.0,-10.0) [blob] {};
\node at (3.0,-10.0) [blob] {};
\node at (4.0,-10.0) [blob] {};
\node at (5.0,-10.0) [blob] {};
\node at (6.0,-10.0) [blob] {};
\node at (7.0,-10.0) [blob] {};
\node at (8.0,-10.0) [blob] {};
\node at (9.0,-10.0) [blob] {};
\draw [box] (4.7,-10.3) rectangle (9.3,-9.7);
\node at (5.0,-10.0) [blob] {};
\node at (6.0,-10.0) [blob] {};
\node at (7.0,-10.0) [blob] {};
\node at (8.0,-10.0) [blob] {};
\node at (9.0,-10.0) [blob] {};
\node at (9.25,-10.0) [time] {$124.35\,\mathrm{\mu{}s}$};
\node at (0.0,-11.0) [blob] {};
\node at (1.0,-11.0) [blob] {};
\node at (2.0,-11.0) [blob] {};
\node at (3.0,-11.0) [blob] {};
\node at (4.0,-11.0) [blob] {};
\node at (5.0,-11.0) [blob] {};
\node at (6.0,-11.0) [blob] {};
\node at (7.0,-11.0) [blob] {};
\node at (8.0,-11.0) [blob] {};
\node at (9.0,-11.0) [blob] {};
\draw [edge] (1.0,-11.0) -- (2.0,-11.0);
\draw [box] (4.7,-11.3) rectangle (9.3,-10.7);
\node at (5.0,-11.0) [blob] {};
\node at (6.0,-11.0) [blob] {};
\node at (7.0,-11.0) [blob] {};
\node at (8.0,-11.0) [blob] {};
\node at (9.0,-11.0) [blob] {};
\node at (9.25,-11.0) [time] {$124.47\,\mathrm{\mu{}s}$};
\node at (0.0,-12.0) [blob] {};
\node at (1.0,-12.0) [blob] {};
\node at (2.0,-12.0) [blob] {};
\node at (3.0,-12.0) [blob] {};
\node at (4.0,-12.0) [blob] {};
\node at (5.0,-12.0) [blob] {};
\node at (6.0,-12.0) [blob] {};
\node at (7.0,-12.0) [blob] {};
\node at (8.0,-12.0) [blob] {};
\node at (9.0,-12.0) [blob] {};
\draw [edge] (2.0,-12.0) -- (3.0,-12.0);
\draw [box] (4.7,-12.3) rectangle (9.3,-11.7);
\node at (5.0,-12.0) [blob] {};
\node at (6.0,-12.0) [blob] {};
\node at (7.0,-12.0) [blob] {};
\node at (8.0,-12.0) [blob] {};
\node at (9.0,-12.0) [blob] {};
\node at (9.25,-12.0) [time] {$125.46\,\mathrm{\mu{}s}$};
\node at (0.0,-13.0) [blob] {};
\node at (1.0,-13.0) [blob] {};
\node at (2.0,-13.0) [blob] {};
\node at (3.0,-13.0) [blob] {};
\node at (4.0,-13.0) [blob] {};
\node at (5.0,-13.0) [blob] {};
\node at (6.0,-13.0) [blob] {};
\node at (7.0,-13.0) [blob] {};
\node at (8.0,-13.0) [blob] {};
\node at (9.0,-13.0) [blob] {};
\draw [edge] (0.0,-13.0) -- (2.0,-13.0);
\draw [edge] (3.0,-13.0) -- (4.0,-13.0);
\draw [box] (4.7,-13.3) rectangle (9.3,-12.7);
\node at (5.0,-13.0) [blob] {};
\node at (6.0,-13.0) [blob] {};
\node at (7.0,-13.0) [blob] {};
\node at (8.0,-13.0) [blob] {};
\node at (9.0,-13.0) [blob] {};
\node at (9.25,-13.0) [time] {$130.32\,\mathrm{\mu{}s}$};
\node at (0.0,-14.0) [blob] {};
\node at (1.0,-14.0) [blob] {};
\node at (2.0,-14.0) [blob] {};
\node at (3.0,-14.0) [blob] {};
\node at (4.0,-14.0) [blob] {};
\node at (5.0,-14.0) [blob] {};
\node at (6.0,-14.0) [blob] {};
\node at (7.0,-14.0) [blob] {};
\node at (8.0,-14.0) [blob] {};
\node at (9.0,-14.0) [blob] {};
\draw [edge] (0.0,-14.0) -- (2.0,-14.0);
\draw [box] (3.7,-14.3) rectangle (9.3,-13.7);
\node at (4.0,-14.0) [blob] {};
\node at (5.0,-14.0) [blob] {};
\node at (6.0,-14.0) [blob] {};
\node at (7.0,-14.0) [blob] {};
\node at (8.0,-14.0) [blob] {};
\node at (9.0,-14.0) [blob] {};
\node at (9.25,-14.0) [time] {$130.66\,\mathrm{\mu{}s}$};
\node at (0.0,-15.0) [blob] {};
\node at (1.0,-15.0) [blob] {};
\node at (2.0,-15.0) [blob] {};
\node at (3.0,-15.0) [blob] {};
\node at (4.0,-15.0) [blob] {};
\node at (5.0,-15.0) [blob] {};
\node at (6.0,-15.0) [blob] {};
\node at (7.0,-15.0) [blob] {};
\node at (8.0,-15.0) [blob] {};
\node at (9.0,-15.0) [blob] {};
\draw [edge] (1.0,-15.0) -- (3.0,-15.0);
\draw [box] (3.7,-15.3) rectangle (9.3,-14.7);
\node at (4.0,-15.0) [blob] {};
\node at (5.0,-15.0) [blob] {};
\node at (6.0,-15.0) [blob] {};
\node at (7.0,-15.0) [blob] {};
\node at (8.0,-15.0) [blob] {};
\node at (9.0,-15.0) [blob] {};
\node at (9.25,-15.0) [time] {$131.76\,\mathrm{\mu{}s}$};
\node at (0.0,-16.0) [blob] {};
\node at (1.0,-16.0) [blob] {};
\node at (2.0,-16.0) [blob] {};
\node at (3.0,-16.0) [blob] {};
\node at (4.0,-16.0) [blob] {};
\node at (5.0,-16.0) [blob] {};
\node at (6.0,-16.0) [blob] {};
\node at (7.0,-16.0) [blob] {};
\node at (8.0,-16.0) [blob] {};
\node at (9.0,-16.0) [blob] {};
\draw [edge] (0.0,-16.0) -- (1.0,-16.0);
\draw [edge] (2.0,-16.0) -- (4.0,-16.0);
\draw [box] (4.7,-16.3) rectangle (9.3,-15.7);
\node at (5.0,-16.0) [blob] {};
\node at (6.0,-16.0) [blob] {};
\node at (7.0,-16.0) [blob] {};
\node at (8.0,-16.0) [blob] {};
\node at (9.0,-16.0) [blob] {};
\node at (9.25,-16.0) [time] {$133.71\,\mathrm{\mu{}s}$};
\node at (0.0,-17.0) [blob] {};
\node at (1.0,-17.0) [blob] {};
\node at (2.0,-17.0) [blob] {};
\node at (3.0,-17.0) [blob] {};
\node at (4.0,-17.0) [blob] {};
\node at (5.0,-17.0) [blob] {};
\node at (6.0,-17.0) [blob] {};
\node at (7.0,-17.0) [blob] {};
\node at (8.0,-17.0) [blob] {};
\node at (9.0,-17.0) [blob] {};
\draw [edge] (0.0,-17.0) -- (2.0,-17.0);
\draw [box] (4.7,-17.3) rectangle (9.3,-16.7);
\node at (5.0,-17.0) [blob] {};
\node at (6.0,-17.0) [blob] {};
\node at (7.0,-17.0) [blob] {};
\node at (8.0,-17.0) [blob] {};
\node at (9.0,-17.0) [blob] {};
\node at (9.25,-17.0) [time] {$134.43\,\mathrm{\mu{}s}$};
\node at (0.0,-18.0) [blob] {};
\node at (1.0,-18.0) [blob] {};
\node at (2.0,-18.0) [blob] {};
\node at (3.0,-18.0) [blob] {};
\node at (4.0,-18.0) [blob] {};
\node at (5.0,-18.0) [blob] {};
\node at (6.0,-18.0) [blob] {};
\node at (7.0,-18.0) [blob] {};
\node at (8.0,-18.0) [blob] {};
\node at (9.0,-18.0) [blob] {};
\draw [edge] (1.0,-18.0) -- (3.0,-18.0);
\draw [box] (4.7,-18.3) rectangle (9.3,-17.7);
\node at (5.0,-18.0) [blob] {};
\node at (6.0,-18.0) [blob] {};
\node at (7.0,-18.0) [blob] {};
\node at (8.0,-18.0) [blob] {};
\node at (9.0,-18.0) [blob] {};
\node at (9.25,-18.0) [time] {$136.50\,\mathrm{\mu{}s}$};
\node at (0.0,-19.0) [blob] {};
\node at (1.0,-19.0) [blob] {};
\node at (2.0,-19.0) [blob] {};
\node at (3.0,-19.0) [blob] {};
\node at (4.0,-19.0) [blob] {};
\node at (5.0,-19.0) [blob] {};
\node at (6.0,-19.0) [blob] {};
\node at (7.0,-19.0) [blob] {};
\node at (8.0,-19.0) [blob] {};
\node at (9.0,-19.0) [blob] {};
\draw [edge] (2.0,-19.0) -- (4.0,-19.0);
\draw [box] (4.7,-19.3) rectangle (9.3,-18.7);
\node at (5.0,-19.0) [blob] {};
\node at (6.0,-19.0) [blob] {};
\node at (7.0,-19.0) [blob] {};
\node at (8.0,-19.0) [blob] {};
\node at (9.0,-19.0) [blob] {};
\node at (9.25,-19.0) [time] {$137.10\,\mathrm{\mu{}s}$};
\node at (14.0,-0.0) [blob] {};
\node at (15.0,-0.0) [blob] {};
\node at (16.0,-0.0) [blob] {};
\node at (17.0,-0.0) [blob] {};
\node at (18.0,-0.0) [blob] {};
\node at (19.0,-0.0) [blob] {};
\node at (20.0,-0.0) [blob] {};
\node at (21.0,-0.0) [blob] {};
\node at (22.0,-0.0) [blob] {};
\node at (23.0,-0.0) [blob] {};
\draw [edge] (16.0,-0.0) -- (17.0,-0.0);
\draw [edge] (18.0,-0.0) -- (19.0,-0.0);
\draw [box] (19.7,-0.3) rectangle (23.3,0.3);
\node at (20.0,-0.0) [blob] {};
\node at (21.0,-0.0) [blob] {};
\node at (22.0,-0.0) [blob] {};
\node at (23.0,-0.0) [blob] {};
\node at (23.25,-0.0) [time] {$147.98\,\mathrm{\mu{}s}$};
\node at (14.0,-1.0) [blob] {};
\node at (15.0,-1.0) [blob] {};
\node at (16.0,-1.0) [blob] {};
\node at (17.0,-1.0) [blob] {};
\node at (18.0,-1.0) [blob] {};
\node at (19.0,-1.0) [blob] {};
\node at (20.0,-1.0) [blob] {};
\node at (21.0,-1.0) [blob] {};
\node at (22.0,-1.0) [blob] {};
\node at (23.0,-1.0) [blob] {};
\draw [edge] (14.0,-1.0) -- (15.0,-1.0);
\draw [edge] (16.0,-1.0) -- (17.0,-1.0);
\draw [edge] (18.0,-1.0) -- (19.0,-1.0);
\draw [box] (19.7,-1.3) rectangle (23.3,-0.7);
\node at (20.0,-1.0) [blob] {};
\node at (21.0,-1.0) [blob] {};
\node at (22.0,-1.0) [blob] {};
\node at (23.0,-1.0) [blob] {};
\node at (23.25,-1.0) [time] {$148.20\,\mathrm{\mu{}s}$};
\node at (14.0,-2.0) [blob] {};
\node at (15.0,-2.0) [blob] {};
\node at (16.0,-2.0) [blob] {};
\node at (17.0,-2.0) [blob] {};
\node at (18.0,-2.0) [blob] {};
\node at (19.0,-2.0) [blob] {};
\node at (20.0,-2.0) [blob] {};
\node at (21.0,-2.0) [blob] {};
\node at (22.0,-2.0) [blob] {};
\node at (23.0,-2.0) [blob] {};
\draw [edge] (18.0,-2.0) -- (19.0,-2.0);
\draw [box] (19.7,-2.3) rectangle (23.3,-1.7);
\node at (20.0,-2.0) [blob] {};
\node at (21.0,-2.0) [blob] {};
\node at (22.0,-2.0) [blob] {};
\node at (23.0,-2.0) [blob] {};
\node at (23.25,-2.0) [time] {$149.90\,\mathrm{\mu{}s}$};
\node at (14.0,-3.0) [blob] {};
\node at (15.0,-3.0) [blob] {};
\node at (16.0,-3.0) [blob] {};
\node at (17.0,-3.0) [blob] {};
\node at (18.0,-3.0) [blob] {};
\node at (19.0,-3.0) [blob] {};
\node at (20.0,-3.0) [blob] {};
\node at (21.0,-3.0) [blob] {};
\node at (22.0,-3.0) [blob] {};
\node at (23.0,-3.0) [blob] {};
\draw [edge] (14.0,-3.0) -- (15.0,-3.0);
\draw [edge] (18.0,-3.0) -- (19.0,-3.0);
\draw [box] (19.7,-3.3) rectangle (23.3,-2.7);
\node at (20.0,-3.0) [blob] {};
\node at (21.0,-3.0) [blob] {};
\node at (22.0,-3.0) [blob] {};
\node at (23.0,-3.0) [blob] {};
\node at (23.25,-3.0) [time] {$151.36\,\mathrm{\mu{}s}$};
\node at (14.0,-4.0) [blob] {};
\node at (15.0,-4.0) [blob] {};
\node at (16.0,-4.0) [blob] {};
\node at (17.0,-4.0) [blob] {};
\node at (18.0,-4.0) [blob] {};
\node at (19.0,-4.0) [blob] {};
\node at (20.0,-4.0) [blob] {};
\node at (21.0,-4.0) [blob] {};
\node at (22.0,-4.0) [blob] {};
\node at (23.0,-4.0) [blob] {};
\draw [edge] (15.0,-4.0) -- (16.0,-4.0);
\draw [edge] (18.0,-4.0) -- (19.0,-4.0);
\draw [box] (19.7,-4.3) rectangle (23.3,-3.7);
\node at (20.0,-4.0) [blob] {};
\node at (21.0,-4.0) [blob] {};
\node at (22.0,-4.0) [blob] {};
\node at (23.0,-4.0) [blob] {};
\node at (23.25,-4.0) [time] {$151.97\,\mathrm{\mu{}s}$};
\node at (14.0,-5.0) [blob] {};
\node at (15.0,-5.0) [blob] {};
\node at (16.0,-5.0) [blob] {};
\node at (17.0,-5.0) [blob] {};
\node at (18.0,-5.0) [blob] {};
\node at (19.0,-5.0) [blob] {};
\node at (20.0,-5.0) [blob] {};
\node at (21.0,-5.0) [blob] {};
\node at (22.0,-5.0) [blob] {};
\node at (23.0,-5.0) [blob] {};
\draw [edge] (14.0,-5.0) -- (15.0,-5.0);
\draw [edge] (17.0,-5.0) -- (18.0,-5.0);
\draw [box] (19.7,-5.3) rectangle (23.3,-4.7);
\node at (20.0,-5.0) [blob] {};
\node at (21.0,-5.0) [blob] {};
\node at (22.0,-5.0) [blob] {};
\node at (23.0,-5.0) [blob] {};
\node at (23.25,-5.0) [time] {$153.64\,\mathrm{\mu{}s}$};
\node at (14.0,-6.0) [blob] {};
\node at (15.0,-6.0) [blob] {};
\node at (16.0,-6.0) [blob] {};
\node at (17.0,-6.0) [blob] {};
\node at (18.0,-6.0) [blob] {};
\node at (19.0,-6.0) [blob] {};
\node at (20.0,-6.0) [blob] {};
\node at (21.0,-6.0) [blob] {};
\node at (22.0,-6.0) [blob] {};
\node at (23.0,-6.0) [blob] {};
\draw [edge] (15.0,-6.0) -- (16.0,-6.0);
\draw [edge] (17.0,-6.0) -- (18.0,-6.0);
\draw [box] (19.7,-6.3) rectangle (23.3,-5.7);
\node at (20.0,-6.0) [blob] {};
\node at (21.0,-6.0) [blob] {};
\node at (22.0,-6.0) [blob] {};
\node at (23.0,-6.0) [blob] {};
\node at (23.25,-6.0) [time] {$154.10\,\mathrm{\mu{}s}$};
\node at (14.0,-7.0) [blob] {};
\node at (15.0,-7.0) [blob] {};
\node at (16.0,-7.0) [blob] {};
\node at (17.0,-7.0) [blob] {};
\node at (18.0,-7.0) [blob] {};
\node at (19.0,-7.0) [blob] {};
\node at (20.0,-7.0) [blob] {};
\node at (21.0,-7.0) [blob] {};
\node at (22.0,-7.0) [blob] {};
\node at (23.0,-7.0) [blob] {};
\draw [edge] (14.0,-7.0) -- (15.0,-7.0);
\draw [edge] (16.0,-7.0) -- (17.0,-7.0);
\draw [box] (19.7,-7.3) rectangle (23.3,-6.7);
\node at (20.0,-7.0) [blob] {};
\node at (21.0,-7.0) [blob] {};
\node at (22.0,-7.0) [blob] {};
\node at (23.0,-7.0) [blob] {};
\node at (23.25,-7.0) [time] {$157.07\,\mathrm{\mu{}s}$};
\node at (14.0,-8.0) [blob] {};
\node at (15.0,-8.0) [blob] {};
\node at (16.0,-8.0) [blob] {};
\node at (17.0,-8.0) [blob] {};
\node at (18.0,-8.0) [blob] {};
\node at (19.0,-8.0) [blob] {};
\node at (20.0,-8.0) [blob] {};
\node at (21.0,-8.0) [blob] {};
\node at (22.0,-8.0) [blob] {};
\node at (23.0,-8.0) [blob] {};
\draw [edge] (17.0,-8.0) -- (18.0,-8.0);
\draw [box] (19.7,-8.3) rectangle (23.3,-7.7);
\node at (20.0,-8.0) [blob] {};
\node at (21.0,-8.0) [blob] {};
\node at (22.0,-8.0) [blob] {};
\node at (23.0,-8.0) [blob] {};
\node at (23.25,-8.0) [time] {$157.21\,\mathrm{\mu{}s}$};
\node at (14.0,-9.0) [blob] {};
\node at (15.0,-9.0) [blob] {};
\node at (16.0,-9.0) [blob] {};
\node at (17.0,-9.0) [blob] {};
\node at (18.0,-9.0) [blob] {};
\node at (19.0,-9.0) [blob] {};
\node at (20.0,-9.0) [blob] {};
\node at (21.0,-9.0) [blob] {};
\node at (22.0,-9.0) [blob] {};
\node at (23.0,-9.0) [blob] {};
\draw [edge] (16.0,-9.0) -- (17.0,-9.0);
\draw [box] (19.7,-9.3) rectangle (23.3,-8.7);
\node at (20.0,-9.0) [blob] {};
\node at (21.0,-9.0) [blob] {};
\node at (22.0,-9.0) [blob] {};
\node at (23.0,-9.0) [blob] {};
\node at (23.25,-9.0) [time] {$157.54\,\mathrm{\mu{}s}$};
\node at (14.0,-10.0) [blob] {};
\node at (15.0,-10.0) [blob] {};
\node at (16.0,-10.0) [blob] {};
\node at (17.0,-10.0) [blob] {};
\node at (18.0,-10.0) [blob] {};
\node at (19.0,-10.0) [blob] {};
\node at (20.0,-10.0) [blob] {};
\node at (21.0,-10.0) [blob] {};
\node at (22.0,-10.0) [blob] {};
\node at (23.0,-10.0) [blob] {};
\draw [edge] (14.0,-10.0) -- (16.0,-10.0);
\draw [edge] (18.0,-10.0) -- (19.0,-10.0);
\draw [box] (19.7,-10.3) rectangle (23.3,-9.7);
\node at (20.0,-10.0) [blob] {};
\node at (21.0,-10.0) [blob] {};
\node at (22.0,-10.0) [blob] {};
\node at (23.0,-10.0) [blob] {};
\node at (23.25,-10.0) [time] {$160.37\,\mathrm{\mu{}s}$};
\node at (14.0,-11.0) [blob] {};
\node at (15.0,-11.0) [blob] {};
\node at (16.0,-11.0) [blob] {};
\node at (17.0,-11.0) [blob] {};
\node at (18.0,-11.0) [blob] {};
\node at (19.0,-11.0) [blob] {};
\node at (20.0,-11.0) [blob] {};
\node at (21.0,-11.0) [blob] {};
\node at (22.0,-11.0) [blob] {};
\node at (23.0,-11.0) [blob] {};
\draw [edge] (15.0,-11.0) -- (17.0,-11.0);
\draw [edge] (18.0,-11.0) -- (19.0,-11.0);
\draw [box] (19.7,-11.3) rectangle (23.3,-10.7);
\node at (20.0,-11.0) [blob] {};
\node at (21.0,-11.0) [blob] {};
\node at (22.0,-11.0) [blob] {};
\node at (23.0,-11.0) [blob] {};
\node at (23.25,-11.0) [time] {$162.46\,\mathrm{\mu{}s}$};
\node at (14.0,-12.0) [blob] {};
\node at (15.0,-12.0) [blob] {};
\node at (16.0,-12.0) [blob] {};
\node at (17.0,-12.0) [blob] {};
\node at (18.0,-12.0) [blob] {};
\node at (19.0,-12.0) [blob] {};
\node at (20.0,-12.0) [blob] {};
\node at (21.0,-12.0) [blob] {};
\node at (22.0,-12.0) [blob] {};
\node at (23.0,-12.0) [blob] {};
\draw [box] (19.7,-12.3) rectangle (23.3,-11.7);
\node at (20.0,-12.0) [blob] {};
\node at (21.0,-12.0) [blob] {};
\node at (22.0,-12.0) [blob] {};
\node at (23.0,-12.0) [blob] {};
\node at (23.25,-12.0) [time] {$163.29\,\mathrm{\mu{}s}$};
\node at (14.0,-13.0) [blob] {};
\node at (15.0,-13.0) [blob] {};
\node at (16.0,-13.0) [blob] {};
\node at (17.0,-13.0) [blob] {};
\node at (18.0,-13.0) [blob] {};
\node at (19.0,-13.0) [blob] {};
\node at (20.0,-13.0) [blob] {};
\node at (21.0,-13.0) [blob] {};
\node at (22.0,-13.0) [blob] {};
\node at (23.0,-13.0) [blob] {};
\draw [edge] (14.0,-13.0) -- (15.0,-13.0);
\draw [box] (19.7,-13.3) rectangle (23.3,-12.7);
\node at (20.0,-13.0) [blob] {};
\node at (21.0,-13.0) [blob] {};
\node at (22.0,-13.0) [blob] {};
\node at (23.0,-13.0) [blob] {};
\node at (23.25,-13.0) [time] {$163.38\,\mathrm{\mu{}s}$};
\node at (14.0,-14.0) [blob] {};
\node at (15.0,-14.0) [blob] {};
\node at (16.0,-14.0) [blob] {};
\node at (17.0,-14.0) [blob] {};
\node at (18.0,-14.0) [blob] {};
\node at (19.0,-14.0) [blob] {};
\node at (20.0,-14.0) [blob] {};
\node at (21.0,-14.0) [blob] {};
\node at (22.0,-14.0) [blob] {};
\node at (23.0,-14.0) [blob] {};
\draw [edge] (14.0,-14.0) -- (16.0,-14.0);
\draw [edge] (17.0,-14.0) -- (18.0,-14.0);
\draw [box] (19.7,-14.3) rectangle (23.3,-13.7);
\node at (20.0,-14.0) [blob] {};
\node at (21.0,-14.0) [blob] {};
\node at (22.0,-14.0) [blob] {};
\node at (23.0,-14.0) [blob] {};
\node at (23.25,-14.0) [time] {$165.20\,\mathrm{\mu{}s}$};
\node at (14.0,-15.0) [blob] {};
\node at (15.0,-15.0) [blob] {};
\node at (16.0,-15.0) [blob] {};
\node at (17.0,-15.0) [blob] {};
\node at (18.0,-15.0) [blob] {};
\node at (19.0,-15.0) [blob] {};
\node at (20.0,-15.0) [blob] {};
\node at (21.0,-15.0) [blob] {};
\node at (22.0,-15.0) [blob] {};
\node at (23.0,-15.0) [blob] {};
\draw [edge] (15.0,-15.0) -- (16.0,-15.0);
\draw [edge] (17.0,-15.0) -- (19.0,-15.0);
\draw [box] (19.7,-15.3) rectangle (23.3,-14.7);
\node at (20.0,-15.0) [blob] {};
\node at (21.0,-15.0) [blob] {};
\node at (22.0,-15.0) [blob] {};
\node at (23.0,-15.0) [blob] {};
\node at (23.25,-15.0) [time] {$166.63\,\mathrm{\mu{}s}$};
\node at (14.0,-16.0) [blob] {};
\node at (15.0,-16.0) [blob] {};
\node at (16.0,-16.0) [blob] {};
\node at (17.0,-16.0) [blob] {};
\node at (18.0,-16.0) [blob] {};
\node at (19.0,-16.0) [blob] {};
\node at (20.0,-16.0) [blob] {};
\node at (21.0,-16.0) [blob] {};
\node at (22.0,-16.0) [blob] {};
\node at (23.0,-16.0) [blob] {};
\draw [edge] (14.0,-16.0) -- (15.0,-16.0);
\draw [edge] (17.0,-16.0) -- (19.0,-16.0);
\draw [box] (19.7,-16.3) rectangle (23.3,-15.7);
\node at (20.0,-16.0) [blob] {};
\node at (21.0,-16.0) [blob] {};
\node at (22.0,-16.0) [blob] {};
\node at (23.0,-16.0) [blob] {};
\node at (23.25,-16.0) [time] {$166.86\,\mathrm{\mu{}s}$};
\node at (14.0,-17.0) [blob] {};
\node at (15.0,-17.0) [blob] {};
\node at (16.0,-17.0) [blob] {};
\node at (17.0,-17.0) [blob] {};
\node at (18.0,-17.0) [blob] {};
\node at (19.0,-17.0) [blob] {};
\node at (20.0,-17.0) [blob] {};
\node at (21.0,-17.0) [blob] {};
\node at (22.0,-17.0) [blob] {};
\node at (23.0,-17.0) [blob] {};
\draw [edge] (14.0,-17.0) -- (16.0,-17.0);
\draw [box] (19.7,-17.3) rectangle (23.3,-16.7);
\node at (20.0,-17.0) [blob] {};
\node at (21.0,-17.0) [blob] {};
\node at (22.0,-17.0) [blob] {};
\node at (23.0,-17.0) [blob] {};
\node at (23.25,-17.0) [time] {$167.35\,\mathrm{\mu{}s}$};
\node at (14.0,-18.0) [blob] {};
\node at (15.0,-18.0) [blob] {};
\node at (16.0,-18.0) [blob] {};
\node at (17.0,-18.0) [blob] {};
\node at (18.0,-18.0) [blob] {};
\node at (19.0,-18.0) [blob] {};
\node at (20.0,-18.0) [blob] {};
\node at (21.0,-18.0) [blob] {};
\node at (22.0,-18.0) [blob] {};
\node at (23.0,-18.0) [blob] {};
\draw [edge] (15.0,-18.0) -- (16.0,-18.0);
\draw [box] (19.7,-18.3) rectangle (23.3,-17.7);
\node at (20.0,-18.0) [blob] {};
\node at (21.0,-18.0) [blob] {};
\node at (22.0,-18.0) [blob] {};
\node at (23.0,-18.0) [blob] {};
\node at (23.25,-18.0) [time] {$168.75\,\mathrm{\mu{}s}$};
\node at (14.0,-19.0) [blob] {};
\node at (15.0,-19.0) [blob] {};
\node at (16.0,-19.0) [blob] {};
\node at (17.0,-19.0) [blob] {};
\node at (18.0,-19.0) [blob] {};
\node at (19.0,-19.0) [blob] {};
\node at (20.0,-19.0) [blob] {};
\node at (21.0,-19.0) [blob] {};
\node at (22.0,-19.0) [blob] {};
\node at (23.0,-19.0) [blob] {};
\draw [edge] (15.0,-19.0) -- (17.0,-19.0);
\draw [box] (19.7,-19.3) rectangle (23.3,-18.7);
\node at (20.0,-19.0) [blob] {};
\node at (21.0,-19.0) [blob] {};
\node at (22.0,-19.0) [blob] {};
\node at (23.0,-19.0) [blob] {};
\node at (23.25,-19.0) [time] {$169.02\,\mathrm{\mu{}s}$};
\end{tikzpicture}
@@!

<br>
Again we see that the fastest FFTs plans use the largest specialised
base transforms along with a combination of smallish Danielson-Lanczos
steps to form the overall transform.  In particular, we see that none
of the fastest plans involve Danielson-Lanczos steps of sizes greater
than eight.

The situation here is simpler than for the general mixed-radix
transform case that we'll deal with next, but these results give some
idea of two approaches to use for a heuristic to choose reasonable
plans to test for a given problem size:

1. Try the largest few base transforms that are available;

2. Use relatively small Danielson-Lanczos steps.

Within these constraints, the choice of which plan is best should be
settled empirically by benchmarking examples for each plan.

## Mixed-radix case

Now let's think about the general mixed-radix case.  In terms of base
transforms, for larger prime factors we can use Rader's algorithm
(which should now be faster since we've improved the performance of
the powers-of-two transforms that are used for the necessary
convolution), but it would be useful to have some other specialised
straight line base transforms available too.  I've now implemented
specialised transforms for all $N$ up to 16, plus 20, 25, 32 and 64.
This is the same set of specialised base transforms used in the
standard distribution of FFTW (and indeed, the versions I'm using are
just Haskell translations of the FFTW C codelets).

As possible base transforms on which to build a full FFT, we should
consider any of the specialised base transform sizes that are factors
of $N$, and we should also consider transforms using Rader's algorithm
for any prime factors greater than 13 (the largest prime for which we
have a specialised base transform).  We should never consider the
naïve DFT algorithm for a base transform, since it's almost certain
that there will be a better way, now that we have specialised
transforms for a range of small prime and other sizes (remember the
$O(N^2)$ scaling of the naïve DFT!).  Once we've selected a base
transform size (which we'll call $N_b$ in what follows) we need to
decide what to do with the "left over" factors.  Suppose that the
prime factorisation of the input length $N$ can be written as

$$N = f_1^{m_1} f_2^{m_2} \dots f_D^{m_D} N_b$$

where the $f_i$, $i = 1, 2, \dots, D$ are unique prime factors and the
$m_i$ are "multiplicities", i.e. the number of powers of each unique
factor included in the left over part of $N$.  Let's think about the
number of FFT plans that we can build in this case.  First of all, we
need to decide on an order for the factors.  Ignoring the issue of
duplicate factors, there are $N_D = m_1 + m_2 + \dots + m_D$ factors
and the total number of possible orderings of these is just the
factorial of this number.  To take account of duplicate factors, we
need to divide this overall factorial by the factorials of the
individual multiplicities.  The total number of distinct orderings of
factors is thus

$$\frac{N_D!}{\prod_{i=1}^D m_i!}.$$

Having chosen an ordering for the factors, we then need to compute the
number of distinct ways to compose adjacent prime factors to give
composite factors.  This is entirely analogous to the situation in the
$2^N$ case, and we can use the same result.  One might thus think that
the total number of plans for this choice of base transform is then

$$\frac{N_D!}{\prod_{i=1}^D m_i!} 2^{N_D - 1}.$$

However, because it's possible for different compositions of different
factor permutations to result in the same plan, the number is somewhat
less than this.  (As an example, think of $12 = 2 \times 2 \times 3$.
If we use a base transform of size 2, we have two distinct
permutations of the remaining factors, i.e. $(2,3)$ and $(3,2)$.  Each
of these has two compositions, $(2,3)$ and $(6)$ and $(3,2)$ and
$(6)$.  Because of the commutativity of ordinary multiplication we get
a plan with a single size-6 Danielson-Lanczos step from each
permutation.)

In order to get an accurate count of the number of plans for different
values of $N$, we need to generate the plans to check for this kind of
overlap.  This requires a little bit of sneakiness.  For a given $N$,
we need to determine the possible base transforms, then we need to
generate all distinct permutations of the "left over" factors to use
for calculating compositions.  We need to retain the unique result
vectors of factors (some prime, some composite) to use as the sizes
for Danielson-Lanczos steps.  The most difficult part is the
generation of the permutations -- because we may have duplicates in
the list of factors, we don't want to use a standard permutation
algorithm.  If we did this, for the factors $2^{10}$, we would
generate $10!$ permutations (all the same, because all of our factors
are identical) instead of the single distinct permutation!

Wikipedia is our friend here.  There's a simple algorithm to generate
all distinct permutations of a multiset in lexicographic order,
starting from the "sorted" permutation, i.e. the one with all the
entries in ascending numerical order.  It goes like this -- for a
sequence $a_j$, $1 \leq j \leq n$:

1. Find the largest index $k$ such that $a_k < a_{k+1}$. If no such
   index exists, the permutation is the last permutation.

2. Find the largest index $l$ such that $a_k < a_l$.

3. Swap the value of $a_k$ with that of $a_l$.

4. Reverse the sequence from $a_{k+1}$ up to and including the final
   element $a_n$.

Short and sweet!  Here's some (not very clever or efficient, but good
enough) code (here, `Data.Vector` is imported, and `Data.List is
imported qualified as `L`):

~~~~ {.haskell}
-- One step of multiset permutation algorithm.
permStep :: Vector Int -> Maybe (Vector Int)
permStep v =
  if null ks
  then Nothing
  else let k = maximum ks
           l = maximum $ filter (\i -> v!k < v!i) $ enumFromN 0 n
       in Just $ revEnd k (swap k l)
  where n = length v
        ks = filter (\i -> v!i < v!(i+1)) $ enumFromN 0 (n-1)
        swap a b = generate n $ \i ->
          if      i == a then v!b
          else if i == b then v!a
                         else v!i
        revEnd f vv = generate n $ \i ->
          if i <= f then vv!i else vv!(n - i + f)

-- Generate all distinct multiset permutations in lexicographic order:
-- input must be the "sorted" permutation.
allPerms :: Vector Int -> [Vector Int]
allPerms idp = idp : L.unfoldr step idp
  where step v = case permStep v of
          Nothing -> Nothing
          Just p -> Just (p, p)
~~~~

It's more or less a direct Haskell transcription of the algorithm from
the Wikipedia page.  I'm sure it could be sped up and cleaned up a
lot, but it works well enough for this application.  (There's also the
Johnson-Trotter loopless permutation algorithm, described in Chapter
28 of Richard Bird's *Pearls of Functional Algorithm Design*, which
looks monstrously clever and deserved some study.  Probably overkill
for now, since the code in here is plenty quick enough for what we
need here.)

Given a way to generate all the permutations for each set of "left
over" factors, we can calculate the compositions of each and retain
the distinct ones for counting (we just use a `Set` from `Data.Set` to
hold the results to maintain distinctness).  We can do this for each
possible base transform for a given $N$ and sum them to get the total
number of possible FFT plans.  This plot shows the number of plans
available for each input vector length in the range 8--1024 (using all
the range of specialised base transforms that I've implemented):

<plot-data name="counts" format="csv" separator=" " cols="n,p"
           src="/blog/posts/2014/01/20/data-analysis-fft-12/plan-counts.dat">
</plot-data>

<plot width=800 aspect=1.6 font-size=16 range-x="8,1100"
      axis-y-transform="log"
      axis-x-label="Input vector length" axis-y-label="Number of plans"
      marker="circle" marker-size=25 fill="black" stroke="none">
  <points x="[[counts.n]]" y="[[counts.p]]"></points>
</plot>

All prime input lengths have only a single possible plan, there are
some larger highly factorisable input lengths that have thousands of
possible plans, and most input lengths have a number of plans lying
somewhere between these two extremes.

The primary challenge in selecting a good plan from this range of
possibilities is to choose a heuristic that requires us to *measure*
the performance of only a few plans: out of the hundreds or thousands
of possibilities for a given input vector length, most plans will be
duds.  We need to pick out a handful of likely candidate plans for
benchmarking.  In order to develop some heuristic plan selection
rules, we'll do some benchmarking experiments, as we did for the
$N=256$ case in the last section.  We'll look at some input vector
lengths that have lots of possible plans, some that have only a few
possible plans and some that lie in the middle range:

<table class="packed">
<thead>
<tr>
<th align="center">**Size**</th>
<th>&nbsp;&nbsp;&nbsp;</th>
<th align="center">**Factors**</th>
<th>&nbsp;&nbsp;&nbsp;</th>
<th align="center">**No. of plans**</th>
</tr>
</thead>
<tbody>
<tr>
<td align="center">960</td> <td></td>
<td align="center">2<sup>6</sup> &times; 3 &times; 5</td> <td></td>
<td align="center">2400</td> </tr>
<tr>
<td align="center">800</td> <td></td>
<td align="center">2<sup>5</sup> &times; 5<sup>2</sup></td> <td></td>
<td align="center">516</td> </tr>
<tr>
<td align="center">512</td> <td></td>
<td align="center">2<sup>9</sup></td> <td></td>
<td align="center">252</td> </tr>
<tr>
<td align="center">216</td> <td></td>
<td align="center">2<sup>3</sup> &times; 3<sup>3</sup></td> <td></td>
<td align="center">230</td> </tr>
<tr>
<td align="center">378</td> <td></td>
<td align="center">2 &times; 3<sup>3</sup> &times; 7</td> <td></td>
<td align="center">109</td> </tr>
<tr>
<td align="center">208</td> <td></td>
<td align="center">2<sup>4</sup> &times; 13</td> <td></td>
<td align="center">40</td> </tr>
<tr>
<td align="center">232</td> <td></td>
<td align="center">2<sup>3</sup> &times; 29</td> <td></td>
<td align="center">16</td> </tr>
<tr>
<td align="center">238</td> <td></td>
<td align="center">2 &times; 7 &times; 17</td> <td></td>
<td align="center">10</td> </tr>
<tr>
<td align="center">92</td> <td></td>
<td align="center">2<sup>2</sup> &times; 23</td> <td></td>
<td align="center">6</td> </tr>
<tr>
<td align="center">338</td> <td></td>
<td align="center">2 &times; 13<sup>2</sup></td> <td></td>
<td align="center">5</td> </tr>
<tr>
<td align="center">1003</td> <td></td>
<td align="center">17 &times; 59</td> <td></td>
<td align="center">2</td> </tr>
<tr>
<td align="center">115</td> <td></td>
<td align="center">5 &times; 23</td> <td></td>
<td align="center">2</td> </tr>
</tbody>
</table>

For each of these problem sizes, the table below shows the execution
times for each of the fastest ten plans or for all possible plans, if
there are less than ten (the base transform sizes are highlighted in
bold):

<table class="packed">
<tr>
<td colspan=2 align="center">$N=960$ (235.25&thinsp;&mu;s)</td>
<td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td colspan=2 align="center">$N=378$ (71.52&thinsp;&mu;s)</td>
<td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td colspan=2 align="center">$N=92$ (285.60&thinsp;&mu;s)</td>
</tr>
<tr>
<td>3 &times; 2 &times; 5:**32**</td>
<td>115.13&thinsp;&mu;s</td>
<td></td>
<td>3 &times; 3 &times; 3:**14**</td>
<td>52.16&thinsp;&mu;s</td>
<td></td>
<td>23:**4**</td>
<td>43.65&thinsp;&mu;s</td>
</tr>
<tr>
<td>3 &times; 5:**64**</td>
<td>116.19&thinsp;&mu;s</td>
<td></td>
<td>9 &times; 3:**14**</td>
<td>59.54&thinsp;&mu;s</td>
<td></td>
<td>23 &times; 2:**2**</td>
<td>65.04&thinsp;&mu;s</td>
</tr>
<tr>
<td>3 &times; 5 &times; 2:**32**</td>
<td>117.68&thinsp;&mu;s</td>
<td></td>
<td>3 &times; 9:**14**</td>
<td>60.95&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 23:**2**</td>
<td>76.00&thinsp;&mu;s</td>
</tr>
<tr>
<td>2 &times; 5 &times; 3:**32**</td>
<td>117.89&thinsp;&mu;s</td>
<td></td>
<td>3 &times; 2 &times; 7:**9**</td>
<td>68.10&thinsp;&mu;s</td>
<td></td>
<td>46:**2**</td>
<td>125.16&thinsp;&mu;s</td>
</tr>
<tr>
<td>5 &times; 3:**64**</td>
<td>118.49&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 3 &times; 7:**9**</td>
<td>68.44&thinsp;&mu;s</td>
<td></td>
<td>4:**23**</td>
<td>295.37&thinsp;&mu;s</td>
</tr>
<tr>
<td>2 &times; 3 &times; 5:**32**</td>
<td>118.99&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 7 &times; 3:**9**</td>
<td>68.74&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 2:**23**
<td>299.11&thinsp;&mu;s</td>
</tr>
<tr>
<td>5 &times; 2 &times; 3:**32**</td>
<td>119.04&thinsp;&mu;s</td>
<td></td>
<td>7 &times; 6:**9**</td>
<td>69.49&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>5 &times; 3 &times; 2:**32**</td>
<td>119.29&thinsp;&mu;s</td>
<td></td>
<td>6 &times; 7:**9**</td>
<td>69.73&thinsp;&mu;s</td>
<td></td>
<td colspan=2 align="center">$N=338$ (67.50&thinsp;&mu;s)</td>
</tr>
<tr>
<td>6 &times; 5:**32**</td>
<td>121.05&thinsp;&mu;s</td>
<td></td>
<td>7 &times; 2 &times; 3:**9**</td>
<td>69.83&thinsp;&mu;s</td>
<td></td>
<td>13 &times; 2:**13**</td>
<td>66.91&thinsp;&mu;s</td>
</tr>
<tr>
<td>5 &times; 6:**32**</td>
<td>124.21&thinsp;&mu;s</td>
<td></td>
<td>7 &times; 3 &times; 2:**9**</td>
<td>70.76&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 13:**13**</td>
<td>67.30&thinsp;&mu;s</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td>26:**13**</td>
<td>106.41&thinsp;&mu;s</td>
</tr>
<tr>
<td colspan=2 align="center">$N=800$ (192.05&thinsp;&mu;s)</td>
<td></td>
<td colspan=2 align="center">$N=208$ (32.85&thinsp;&mu;s)</td>
<td></td>
<td>13 &times; 13:**2**</td>
<td>212.18&thinsp;&mu;s</td>
</tr>
<tr>
<td>5 &times; 5:**32**</td>
<td>94.64&thinsp;&mu;s</td>
<td></td>
<td>4 &times; 4:**13**</td>
<td>28.97&thinsp;&mu;s</td>
<td></td>
<td>169:**2**</td>
<td>1701.35&thinsp;&mu;s</td>
</tr>
<tr>
<td>2 &times; 4 &times; 4:**25**</td>
<td>101.56&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 2 &times; 4:**13**</td>
<td>30.31&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>4 &times; 2 &times; 4:**25**</td>
<td>101.67&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 4 &times; 2:**13**</td>
<td>31.70&thinsp;&mu;s</td>
<td></td>
<td colspan=2 align="center">$N=1003$ (2413.41&thinsp;&mu;s)</td>
</tr>
<tr>
<td>2 &times; 2 &times; 2 &times; 4:**25**</td>
<td>104.10&thinsp;&mu;s</td>
<td></td>
<td>4 &times; 2 &times; 2:**13**</td>
<td>32.04&thinsp;&mu;s</td>
<td></td>
<td>59:**17**</td>
<td>1843.99&thinsp;&mu;s</td>
</tr>
<tr>
<td>4 &times; 4 &times; 2:**25**</td>
<td>104.20&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 8:**13**</td>
<td>33.21&thinsp;&mu;s</td>
<td></td>
<td>17:**59**</td>
<td>2538.96&thinsp;&mu;s</td>
</tr>
<tr>
<td>2 &times; 2 &times; 4 &times; 2:**25**</td>
<td>104.56&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 2 &times; 2 &times; 2:**13**</td>
<td>33.58&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 4 &times; 2 &times; 2:**25**</td>
<td>106.56&thinsp;&mu;s</td>
<td></td>
<td>8 &times; 2:**13**</td>
<td>34.26&thinsp;&mu;s</td>
<td></td>
<td colspan=2 align="center">$N=115$ (362.84&thinsp;&mu;s)</td>
</tr>
<tr>
<td>4 &times; 2 &times; 2 &times; 2:**25**</td>
<td>106.61&thinsp;&mu;s</td>
<td></td>
<td>13:**16**</td>
<td>35.31&thinsp;&mu;s</td>
<td></td>
<td>23:**5**</td>
<td>46.50&thinsp;&mu;s</td>
</tr>
<tr>
<td>2 &times; 2 &times; 2 &times; 2 &times; 2:**25**</td>
<td>107.50&thinsp;&mu;s</td>
<td></td>
<td>16:**13**</td>
<td>45.04&thinsp;&mu;s</td>
<td></td>
<td>5:**23**</td>
<td>373.16&thinsp;&mu;s</td>
</tr>
<tr>
<td>2 &times; 5 &times; 4:**20**</td>
<td>108.47&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 13:**8**</td>
<td>47.74&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>&nbsp;</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td colspan=2 align="center">$N=512$ (278.94&thinsp;&mu;s)</td>
<td></td>
<td colspan=2 align="center">$N=232$ (584.14&thinsp;&mu;s)</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>4 &times; 4:**32**</td>
<td>55.10&thinsp;&mu;s</td>
<td></td>
<td>29:**8**</td>
<td>87.52&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 4:**64**</td>
<td>55.63&thinsp;&mu;s</td>
<td></td>
<td>29 &times; 2:**4**</td>
<td>113.47&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>4 &times; 2:**64**</td>
<td>55.66&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 29:**4**</td>
<td>136.21&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 2 &times; 4:**32**</td>
<td>56.01&thinsp;&mu;s</td>
<td></td>
<td>29 &times; 4:**2**</td>
<td>142.41&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 2 &times; 2:**64**</td>
<td>56.65&thinsp;&mu;s</td>
<td></td>
<td>29 &times; 2 &times; 2:**2**</td>
<td>159.00&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>4 &times; 2 &times; 2:**32**</td>
<td>57.00&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 29 &times; 2:**2**</td>
<td>185.15&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 4 &times; 2:**32**</td>
<td>57.34&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 2 &times; 29:**2**</td>
<td>229.41&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 2 &times; 2 &times; 2:**32**</td>
<td>58.71&thinsp;&mu;s</td>
<td></td>
<td>4 &times; 29:**2**</td>
<td>231.43&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>8:**64**</td>
<td>60.27&thinsp;&mu;s</td>
<td></td>
<td>58:**4**</td>
<td>254.22&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 8:**32**</td>
<td>62.38&thinsp;&mu;s</td>
<td></td>
<td>58 &times; 2:**2**</td>
<td>294.84&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>&nbsp;</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td colspan=2 align="center">$N=216$ (69.19&thinsp;&mu;s)</td>
<td></td>
<td colspan=2 align="center">$N=238$ (316.93&thinsp;&mu;s)</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>3 &times; 6:**12**</td>
<td>31.10&thinsp;&mu;s</td>
<td></td>
<td>17:**14**</td>
<td>51.85&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 3 &times; 3:**12**</td>
<td>31.18&thinsp;&mu;s</td>
<td></td>
<td>17 &times; 2:**7**</td>
<td>69.44&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>3 &times; 2 &times; 3:**12**</td>
<td>31.51&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 17:**7**</td>
<td>69.68&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>6 &times; 3:**12**</td>
<td>31.63&thinsp;&mu;s</td>
<td></td>
<td>34:**7**</td>
<td>109.80&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>3 &times; 3 &times; 2:**12**</td>
<td>32.60&thinsp;&mu;s</td>
<td></td>
<td>17 &times; 7:**2**</td>
<td>127.19&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 9:**12**</td>
<td>35.79&thinsp;&mu;s</td>
<td></td>
<td>7 &times; 17:**2**</td>
<td>160.65&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>3 &times; 2 &times; 4:**9**</td>
<td>36.03&thinsp;&mu;s</td>
<td></td>
<td>2 &times; 7:**17**</td>
<td>327.17&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>6 &times; 4:**9**</td>
<td>36.67&thinsp;&mu;s</td>
<td></td>
<td>7 &times; 2:**17**</td>
<td>329.79&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>2 &times; 3 &times; 4:**9**</td>
<td>37.00&thinsp;&mu;s</td>
<td></td>
<td>14:**17**</td>
<td>339.61&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>9 &times; 2:**12**</td>
<td>37.04&thinsp;&mu;s</td>
<td></td>
<td>119:**2**</td>
<td>823.30&thinsp;&mu;s</td>
<td></td>
<td></td>
<td></td>
</tr>
</table>

It's important to note that we don't need to use these results to
identify the *best* plan, just a reasonable set of plans to test
empirically.  When a call is made to the `plan` function, we're going
to use Criterion to benchmark a number of likely looking plans and
we'll take the fastest one.  It doesn't matter if this benchmarking
takes a while (a few seconds, say) since we'll only need to do it once
for each input length (we'll eventually save the best plan away in a
"wisdom" file so that we can get at it immediately if a plan for the
same input length is requested later on).  If some transform takes
about 200&thinsp;&mu;s on average, then we can run 5,000 tests in one
second.  Since Criterion runs 100 benchmarks to get reasonable timing
statistics, we could test about 50 plans in a second.  Let's aim to
come up with a heuristic that yields 20-50 plans for a given input
length, test them all, then pick the best one.

So, based on these results and the earlier results for the
powers-of-two case, what appear to be good heuristics for plan
selection?

 * First, make use of the larger specialised base transforms.  In both
   the $N=256$ and $N=1024$ cases, the fastest plans all made use of
   the size-64 or size-32 base transforms, and likewise in the general
   experiments shown above, the faster plans tend to make use of the
   larger base transforms.

 * Specialised base transforms are generally faster than using Rader's
   algorithm for a larger prime factor as the base transform.  For
   example, for $N = 92 = 2^2 \times 23$, plans using the size-4 or
   size-2 base transforms are faster than those using Rader's
   algorithm for the factor of 23.

 * Using multiple Danielson-Lanczos steps of smaller sizes is
   generally faster than trying to fold everything into one big
   composite step.  For example, the results for the $N=256$ case show
   that the *slowest* plans tend to be those using the largest
   Danielson-Lanczos steps.

 * In cases where there's no but to use Rader's algorithm for the base
   transform, it can make a difference to use a base transform size
   that doesn't need any padding for the Rader algorithm convolution.
   For example, for $N = 1003 = 17 \times 59$, using a base transform
   of size 17 is significantly faster than using a base transform of
   size 59, presumably because, for the size-17 transform, Rader's
   algorithm requires us to perform a convolution of size 16, and no
   zero padding is needed, while for the size-59 transform, the
   convolution requires padding of an intermediate vector to length
   128 (the smallest power of two that works).

Based on this, here's a set of heuristics for choosing plans to test:

1. Determine the usable base transforms for the given $N$; sort the
   specialised base transforms in descending order of size, followed
   by any other prime base transforms, in descending order of size
   with transform sizes of the form $2^m+1$ before any others
   (i.e. those that don't require any padding for the Rader's
   algorithm convolution).

2. For each base transform, generate all the plans that make use of
   Danielson-Lanczos steps for the remaining "left over" factors that
   are "small", in the sense that they involve only one, two or three
   factors at a time (i.e. don't bother generating plans that use
   Danielson-Lanczos steps of larger sizes).

3. Limit the number of generated plans to around 50 so that the
   benchmarking step doesn't take too long.

4. Benchmark all the generated plans and select the fastest!

This approach may not find the optimal plan every time, but it should
have a relatively good chance and won't require the benchmarking of
too many plans.

There is one aspect of all this that would require revisiting if we
had specialised "twiddlets" for the Danielson-Lanczos steps.  In
that case, we would want to pick plans where we could use a large
specialised base transform and make use of the largest possible
specialised Danielson-Lanczos "twiddlets".  For the moment, since we
don't have such specialised machinery for the Danielson-Lanczos steps,
the approach above should be a good choice.

## Implementation

The code described here is the `pre-release-3` version of the GitHub
repository.

Generation of the candidate plans for a given input size is basically
a question of determining which base transforms are usable, then
generating all of the permutations and combinations of the "left
over" factors for each base transform.  The plans are sorted
according to the heuristic ordering described above and we take the
first 50 or so for benchmarking.

For the base transforms, we define a helper type called `BaseType` to
represent base transforms and to allow us to define an `Ord` instance
for the heuristic ordering of the base transforms.  We also define a
`newtype` wrapper, `SPlan`, to wrap the whole of a plan definition,
again so that we can write a custom `Ord` instance based on the
heuristic ordering of plans:

~~~~ {.haskell}
-- | Base transform type with heuristic ordering.
data BaseType = Special Int | Rader Int deriving (Eq, Show)

-- | Newtype wrapper for custom sorting.
newtype SPlan = SPlan (BaseType, Vector Int) deriving (Eq, Show)

-- | Base transform size.
bSize :: BaseType -> Int
bSize (Special b) = b
bSize (Rader b) = b

-- | Heuristic ordering for base transform types: special bases come
-- first, then prime bases using Rader's algorithm, ordered according
-- to size compensating for padding needed in the Rader's algorithm
-- convolution.
instance Ord BaseType where
  compare (Special _)  (Rader _)    = LT
  compare (Rader _)    (Special _)  = GT
  compare (Special s1) (Special s2) = compare s1 s2
  compare (Rader r1)   (Rader r2)   = case (isPow2 $ r1 - 1, isPow2 $ r2 - 1) of
    (True, True) -> compare r1 r2
    (True, False) -> compare r1 (2 * r2)
    (False, True) -> compare (2 * r1) r2
    (False, False) -> compare r1 r2

-- | Heuristic ordering for full plans, based first on base type, then
-- on the maximum size of Danielson-Lanczos step.
instance Ord SPlan where
  compare (SPlan (b1, fs1)) (SPlan (b2, fs2)) = case compare b1 b2 of
    LT -> LT
    EQ -> compare (maximum fs2) (maximum fs1)
    GT -> GT
~~~~

Here's the code for the generation of candidate plans:

~~~~ {.haskell}
-- | Generate test plans for a given input size, sorted in heuristic
-- order.
testPlans :: Int -> Int -> [(Int, Vector Int)]
testPlans n nplans = L.take nplans $
                     L.map clean $
                     L.sortBy (comparing Down) $ P.concatMap doone bs
  where vfs = allFactors n
        bs = usableBases n vfs
        doone b = basePlans n vfs b
        clean (SPlan (b, fs)) = (bSize b, fs)

-- | List plans from a single base.
basePlans :: Int -> Vector Int -> BaseType -> [SPlan]
basePlans n vfs bt = if null lfs
                     then [SPlan (bt, empty)]
                     else P.map (\v -> SPlan (bt, v)) $ leftOvers lfs
  where lfs = fromList $ (toList vfs) \\ (toList $ allFactors b)
        b = bSize bt

-- | Produce all distinct permutations and compositions constructable
-- from a given list of factors.
leftOvers :: Vector Int -> [Vector Int]
leftOvers fs =
  if null fs
  then []
  else S.toList $ L.foldl' go S.empty (multisetPerms fs)
  where n = length fs
        go fset perm = foldl' doone fset (enumFromN 0 (2^(n - 1)))
          where doone s i = S.insert (makeComp perm i) s

-- | Usable base transform sizes.
usableBases :: Int -> Vector Int -> [BaseType]
usableBases n fs = P.map Special bs P.++ P.map Rader ps
  where bs = toList $ filter ((== 0) . (n `mod`)) specialBaseSizes
        ps = toList $ filter isPrime $ filter (> maxPrimeSpecialBaseSize) fs
~~~~

The main `testPlans` function calls `usableBases` to determine what
base transforms can be used for a given input size, distinguishing
between specialised straight-line transforms (the `Special`
constructor of the `BaseType` type) and larger prime transforms that
require use of Rader's algorithm (the `Rader` constructor for
`BaseType`).  For each possible base transform, the `basePlans`
function determines the "left over" factors of the input size and uses
the `leftOvers` function to generate a list of possible
Danielson-Lanczos steps, which are then bundled up with the base
transform information into values of type `SPlan`.  The `leftOvers`
function calculates all possible permutations of the multiset of
left-over factors (using the `multiPerms` function, which is
essentially identical to the multiset permutation code shown earlier),
and for each permutation calculates all possible compositions of the
factors.  Plans are collected into an intermediate `Set` to remove
duplicates.

The full set of candidate plans is then sorted in `testPlans` in
descending order of desirability according to the planning heuristics,
and the first `nplans` plans are returned for further processing.

There are several places where this code could be made more efficient
(there's quite a bit of intermediate list construction, for instance),
but there's really not too much point in expending effort on it, since
the planning step should only be done once before executing an FFT
calculation multiple times using the same plan.  In any case, the plan
generation is relatively quick even for quite large input sizes.

The top-level code that drives the empirical planning process is shown
here:

~~~~ {.haskell}
-- | Globally shared timing environment.  (Not thread-safe...)
timingEnv :: IORef (Maybe Environment)
timingEnv = unsafePerformIO (newIORef Nothing)
{-# NOINLINE timingEnv #-}

-- | Plan calculation for a given problem size.
empiricalPlan :: Int -> IO Plan
empiricalPlan n = do
  wis <- readWisdom n
  case wis of
    Just p -> return $ planFromFactors n p
    Nothing -> do
      let ps = testPlans n nTestPlans
      withConfig (defaultConfig { cfgVerbosity = ljust Quiet
                                , cfgSamples   = ljust 1 }) $ do
        menv <- liftIO $ readIORef timingEnv
        env <- case menv of
          Just e -> return e
          Nothing -> do
            meas <- measureEnvironment
            liftIO $ writeIORef timingEnv $ Just meas
            return meas
        let v = generate n (\i -> sin (2 * pi * fromIntegral i / 511) :+ 0)
        tps <- CM.forM ps $ \p -> do
          let pp = planFromFactors n p
          pptest <- case plBase pp of
            bpl@(RaderBase _ _ _ _ csz _) -> do
              cplan <- liftIO $ empiricalPlan csz
              return $ pp { plBase = bpl { raderConvPlan = cplan } }
            _ -> return pp
          ts <- runBenchmark env $ nf (execute pptest Forward) v
          return (sum ts / fromIntegral (length ts), p)
        let (rest, resp) = L.minimumBy (compare `on` fst) tps
        liftIO $ writeWisdom n resp
        let pret = planFromFactors n resp
        case plBase pret of
          bpl@(RaderBase _ _ _ _ csz _) -> do
            cplan <- liftIO $ empiricalPlan csz
            return $ pret { plBase = bpl { raderConvPlan = cplan } }
          _ -> return pret
~~~~

We'll describe the "wisdom" stuff in a minute, but first let's look at
the `Nothing` branch of the `case` expression at the top of the
`empiricalPlan` function.  The `testPlans` function we looked at above
is used to generate a list of candidate plans and we then use
Criterion to run benchmarks for each of these plans, choosing the plan
with the best execution time.  There are a few wrinkles to make things
a little more efficient.  First, we have a global `IORef` that we use
to store the Criterion timing environment information -- this avoids
repeated calls to `measureEnvironment` to determine the system clock
resolution.  Using an `IORef` in this way is not thread-safe, which is
something we would have to fix to make this a production-ready
library.  We'll not worry about it for now.  Second, we make Criterion
only collect a single sample for each plan, just to make the
benchmarking go quicker.  If we have a number of plans that are within
a few percent of each other in terms of their execution times, it
probably doesn't matter too much exactly which one we choose, so
there's not much to be gained from running lots of benchmarking tests
to get accurate timing information.  In most cases, the differences
between plans are large enough that we can easily identify the best
plan from a single benchmark run.  Finally, we have to do some slight
messing around to fix up the plans for the convolution step in prime
base transforms using Rader's algorithm.

From a user's point of view, all of this complexity is hidden behind
the `empiricalPlan` function: you call this with your input size and
you get a `Plan` back, one that's hopefully close to the optimal plan
that we can generate.

Once the optimal plan for a given input size has been determined on a
given machine, it won't change, since it depends only on fixed details
of the machine architecture (processor, cache sizes and so on).
Instead of doing the work of running the empirical benchmarking tests
every time that `empiricalPlan` is called, we can thus save the
planning results away in "wisdom" files for reuse later on.
Here's the code to do this:

~~~~ {.haskell}
-- | Read from wisdom for a given problem size.
readWisdom :: Int -> IO (Maybe (Int, Vector Int))
readWisdom n = do
  home <- getEnv "HOME"
  let wisf = home </> ".fft-plan" </> show n
  ex <- doesFileExist wisf
  case ex of
    False -> return Nothing
    True -> do
      wist <- readFile wisf
      let (wisb, wisfs) = read wist :: (Int, [Int])
      return $ Just (wisb, fromList wisfs)

-- | Write wisdom for a given problem size.
writeWisdom :: Int -> (Int, Vector Int) -> IO ()
writeWisdom n (b, fs) = do
  home <- getEnv "HOME"
  let wisd = home </> ".fft-plan"
      wisf = wisd </> show n
  createDirectoryIfMissing True wisd
  writeFile wisf $ show (b, toList fs) P.++ "\n"
~~~~

We save one file per input size in a directory called `~/.fft-plan`,
writing and reading the `(Int, Vector Int)` planning information using
Haskell's basic `show` and `read` capabilities.  Whenever
`empiricalPlan` is called, we check to see if we have a wisdom file
for the requested input size and only generate plans and run
benchmarks if there is no wisdom.  Conversely, when we benchmark and
find an optimal plan, we save that information to a wisdom file for
later.

## Benchmarking

We can measure the performance of the `pre-release-3` code in the same
way as we've done for earlier versions.  Here's a view of the
performance of this version of the code that should be pretty familiar
by now:

<plot-data name="dat" format="csv" separator=" "
           src="/blog/posts/2014/01/20/data-analysis-fft-12/benchmark-4.dat">
</plot-data>

<plot width=800 aspect=1.6 font-size=16 range-x="8,1100"
      axis-x-label="Input length" axis-y-label="Time"
      axis-x-transform="log" ui-axis-x-transform
      axis-y-transform="log" ui-axis-y-transform
      axis-x-ticks="[[[[8,'8'],[16,'16'],[32,'32'],[64,'64'],[128,'128'],[256,'256'],[512,'512'],[1024,'1024']]]]"
      axis-y-ticks="[[[[1,'1&thinsp;&mu;s'],[10,'10&thinsp;&mu;s'],[100,'100&thinsp;&mu;s']]]]"
      axis-x-end-tick-size=0 oxs="[[seqStep(8,1024,4)]]">
  <legend order="FFT|FFTW|O(N log N)">
  </legend>
  <plot-options stroke-width=1.5 stroke="black">
    <lines label="O(N log N)" x="[[oxs]]" y="[[0.02*x*log(x)]]"></lines>
    <lines x="[[oxs]]" y="[[0.01*x*log(x)]]"></lines>
  </plot-options>
  <plot-options stroke-width=2>
    <lines label="FFT" stroke="green" x="[[dat.Size]]" y="[[dat.FFT]]"></lines>
    <lines label="FFTW" stroke="blue" x="[[dat.Size]]" y="[[dat.FFTW]]"></lines>
  <plot-options>
</plot>

and here are the ratio plots showing the relative performance of the
original unoptimised version of the code (`pre-release-1`), the
current version (`pre-release-3`) and FFTW:

<plot-data name="ratios" format="csv"
           src="/blog/posts/2014/01/20/data-analysis-fft-12/ratios.dat">
</plot-data>

<plot-stack width=800 aspect=1.6>
  <plot title="pre-release-1" font-size=16
        axis-x-label="FFT/FFTW execution time ratio" axis-y-label="Frequency"
        fill="blue" fill-opacity="0.3" stroke="none" bar-width="-1px">
    <bars hist="[[histogram(ratios.rel2, 50)]]"
          x="[[hist.centres]]" y="[[hist.counts]]"></bars>
  </plot>
  <plot title="pre-release-3" font-size=16
        axis-x-label="FFT/FFTW execution time ratio" axis-y-label="Frequency"
        fill="blue" fill-opacity="0.3" stroke="none" bar-width="-1px">
    <bars hist="[[histogram(ratios.rel3, 50)]]"
          x="[[hist.centres]]" y="[[hist.counts]]"></bars>
  </plot>
  <plot title="Speed-up" font-size=16
        axis-x-label="Execution time speed-up" axis-y-label="Frequency"
        fill="blue" fill-opacity="0.3" stroke="none" bar-width="-1px">
    <bars hist="[[histogram(ratios.speedup, 50)]]"
          x="[[hist.centres]]" y="[[hist.counts]]"></bars>
  </plot>
</plot-stack>

It appears that the empirical optimisation approach we've taken here
has been quite successful.  The "pre-release-3" tab above shows that
for most input sizes in the range that we're benchmarking, our code is
around 10 times slower than FFTW, and never more than 20 times slower.
In the previous article, we saw that, for the `pre-release-2` code
version, most input lengths were between 40 and 100 times slower than
FFTW.  The "Speed-up" tab also shows that we've significantly
increased the range of input lengths getting 50-fold or better
speedups compared to the original unoptimised code.

Most of the remaining slower cases can be put down to our
implementation of Rader's algorithm.  When the input length $N$ is not
of the form $2^m+1$, allocation is required of a zero-padded vector is
required for the convolution in Rader's algorithm.  It ought to be
possible to avoid this allocation and speed up the code a little, which ought
to help with some of the slower cases (most of which are either prime
lengths, or involve a comparatively large prime factor).

In the next (and penultimate) article in this series, we'll clear up
this issue a little, play with some compiler flags and catalogue the
remaining opportunities for optimisation.
