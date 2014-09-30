---
author: Ian
title: Non-diffusive atmospheric flow #6: principal components analysis
tags: data-analysis,haskell
published: 2014-09-18 13:39:03
---

The pre-processing that we've done hasn't really got us anywhere in
terms of the main analysis we want to do -- it's just organised the
data a little and removed the main source of variability (the seasonal
cycle) that we're not interested in.  Although we've subsetted the
original geopotential height data both spatially and temporally, there
is still a *lot* of data: 66 years of 181-day winters, each day of
which has $72 \times 15$ $Z_{500}$ values.  This is a very common
situation to find yourself in if you're dealing with climate,
meteorological, oceanographic or remote sensing data.  One approach to
this glut of data is something called *dimensionality reduction*, a
term that refers to a range of techniques for extracting "interesting"
or "important" patterns from data so that we can then talk about the
data in terms of how strong these patterns are instead of what data
values we have at each point in space and time.

I've put the words "interesting" and "important" in quotes here
because what's interesting or important is up to us to define, and
determines the dimensionality reduction method we use.  Here, we're
going to side-step the question of determining what's interesting or
important by using the de facto default dimensionality reduction
method, *principal components analysis* (PCA).  We'll take a look in
detail at what kind of "interesting" and "important" PCA give us a
little later.

<!--MORE-->

PCA is, in principle, quite a simple method, but it causes many people
endless problems.  There are some very good reasons for this:

 * PCA is in some sense nothing more than a generic change of basis
   operation (with the basis we change to chosen in a special way).
   The result of this is that a lot of the terminology used about PCA
   is also very generic, and hence very confusing (words like "basis",
   "component", "eigenvector", "projection" and so on could mean more
   or less anything in this context!).

 * PCA is used in nearly every field where multivariate data is
   analysed, and is the archetypical "unsupervised learning" method.
   This means that it has been invented, reinvented, discovered and
   rediscovered many times, under many different names.  Some other
   names for it are: empirical orthogonal function (EOF) analysis, the
   Karhunen-Loève decomposition, proper orthogonal decomposition
   (POD), and there are many others.  Each of these different fields
   also uses different terms for the different outputs from PCA.  This
   is *very* confusing: some people talk about principal components,
   some about empirical orthogonal functions and principal component
   time series, some about basis functions, and so on.  Here, we're
   going to try to be very clear and careful about the names that we
   use for things to try to alleviate some of the confusion.

 * There is a bit of a conceptual leap that's necessary to go from
   very basic examples of using PCA to using PCA to analyse the kind
   of spatio-temporal data we have here.  I used to say something
   like: "Well, there's a nice two-dimensional example, and it works
   just the same in 100 dimensions, so let's just apply it to our
   atmospheric data!"  A perfectly reasonable reponse to that is:
   "WHAT?!  Are you an idiot?".  Here, we're going to take that
   conceptual leap slowly, and describe exactly how the "change of
   basis" view of PCA works for spatio-temporal data.

 * There are some aspects of the scaling of the different outputs from
   PCA that are *really* confusing.  In simple terms, PCA breaks your
   data down into two parts, and you could choose to put the units of
   your data on either one of those parts, normalising the other part.
   Which one you put the units on isn't always an obvious choice and
   it's really easy to screw things up if you do it wrong.  We'll look
   at this carefully here.

So, there's quite a bit to cover in the next couple of articles.  In
this article, we will: explain the basic idea of PCA with a very
simple (two-dimensional!) example; give a recipe for how to perform
PCA on a data set; talk about why PCA works from an algebraic
standpoint; talk about how to do these calculations in Haskell.  Then
in the next article, we will: describe exactly how we do PCA on
spatio-temporal data; demonstrate how to perform PCA on the $Z_{500}$
anomaly data; show how to visualise the $Z_{500}$ PCA results and save
them for later use.  What we will end up with from this stage of our
analysis is a set of "important" spatial patterns (we'll see what
"important" means for PCA) and time series of how strong each of those
spatial patterns is at a particular point in time.  The clever thing
about this decomposition is that we can restrict our attention to the
few most "important" patterns and discard all the rest of the
variability in the data.  That makes the subsequent exploration of the
data much simpler.

### The basic idea of PCA

We're going to take our first look at PCA using a very simple example.
It might not be immediately obvious how the technique we're going to
develop here will be applicable to the spatio-temporal $Z_{500}$ data
we really want to analyse, but we'll get to that a little later, after
we've seen how PCA works in this simple example and we've done a
little algebra to get a clearer understanding of just why the "recipe"
we're going to use works the way that it does.

Suppose we go to the seaside and measure the shells of mussels[^1].
We'll measure the length and width of each shell and record the data
for each mussel as a two-dimensional (length, width) vector.  There
will be variation in the sizes and shapes of the mussels, some longer,
some shorter, some fatter, some skinnier.  We might end up with data
that looks something like what's shown below, where there's a spread
of length in the shells around a mean of about 5 cm, a spread in the
width of shells around a mean of about 3 cm, and there's a clear
correlation between shell length and width (see Figure 1
[below](#figs)).  Just from eyeballing this picture, it seems apparent
that maybe measuring shell length and width might not be the best way
to represent this data -- it looks as though it could be better to
think of some combination of length and width as measuring the overall
"size" of a mussel, and some other combination of length and width as
measuring the "fatness" or "skinniness" of a mussel.  We'll see how a
principal components analysis of this data extracts these two
combinations in a clear way.

*The code for this post is available in a [Gist][gist].  The Gist
 contains a Cabal file as well as the Haskell source, to make it easy
 to build.  Just do something like this to build and run the code in a
 sandbox:*

~~~~
git clone https://gist.github.com/d39bf143ffc482ea3700.git pca-2d
cd pca-2d
cabal sandbox init
cabal install
./.cabal-sandbox/bin/pca-2d
~~~~

Just for a slight change, I'm going to produce all the plots in this
section using Haskell, specifically using the [`Chart`][chart]
library.  We'll use the `hmatrix` library for linear algebra, so the
imports we end up needing are:

~~~~ {.haskell}
import Control.Monad
import Numeric.LinearAlgebra.HMatrix
import Graphics.Rendering.Chart.Easy hiding (Matrix, Vector, (|>), scale)
import Graphics.Rendering.Chart.Backend.Cairo
~~~~

There are some name overlaps between the monadic plot interface
provided by the `Graphics.Rendering.Chart.Easy` module and `hmatrix`,
so we just hide the overlapping ones.

We generate 500 synthetic data points:

~~~~ {.haskell}
-- Number of test data points.
n :: Int
n = 500

-- Mean, standard deviation and correlation for two dimensions of test
-- data.
meanx, meany, sdx, sdy, rho :: Double
meanx = 5.0 ; meany = 3.0 ; sdx = 1.2 ; sdy = 0.6 ; rho = 0.75

-- Generate test data.
generateTestData :: Matrix Double
generateTestData =
  let seed = 1023
      mean = 2 |> [ meanx, meany ]
      cov = matrix 2 [ sdx^2       , rho*sdx*sdy
                     , rho*sdx*sdy , sdy^2       ]
      samples = gaussianSample seed n mean cov
  in fromRows $ filter ((> 0) . minElement) $ toRows samples
~~~~

The mussel shell length and width values are generated from a
two-dimensional Gaussian distribution, where we specify mean and
standard deviation for both shell length and width, and the
correlation between the length and width (as the usual Pearson
correlation coefficient).  Given this information, we can generate
samples from the Gaussian distribution using `hmatrix`'s
`gaussianSample` function.  (If we didn't have this function, we would
calculate the Cholesky decomposition of the covariance matrix we
wanted, generate samples from a pair of standard one-dimensional
Gaussian distributions and multiple two-dimensional vectors of these
samples by one of the Cholesky factors of the covariance matrix --
this is just what the `gaussianSample` function does for us.)  We do a
little filtering in `generateTestData` to make sure that we don't
generate any negative values[^2].

The main program that drives the generation of the plots we'll look at
below is:

~~~~ {.haskell}
main :: IO ()
main = do
  let dat = generateTestData
      (varexp, evecs, projs) = pca dat
      (mean, cov) = meanCov dat
      cdat = fromRows $ map (subtract mean) $ toRows dat
  forM_ [(PNG, "png"), (PDF, "pdf"), (SVG, "svg")] $ \(ptype, suffix) -> do
    doPlot ptype suffix dat evecs projs 0
    doPlot ptype suffix cdat evecs projs 1
    doPlot ptype suffix cdat evecs projs 2
    doPlot ptype suffix cdat evecs projs 3
  putStrLn $ "FRACTIONAL VARIANCE EXPLAINED: " ++ show varexp
~~~~

and you can see the `doPlot` function that generates the individual
plots in the [Gist][gist].  I won't say a great deal about the
plotting code, except to observe that the new monadic API to the
`Chart` library makes generating this kind of simple plot in Haskell
no harder than it would be using Gnuplot or something similar.  The
plot code produces one of four plots depending on an integer
parameter, which ranges from zero (the first plot above) to three.
Because we're using the Cairo backend to the `Chart` library, we can
generate image output in any of the formats that Cairo supports --
here we generate PDF (to insert into LaTeX documents), SVG (to insert
into web pages) and PNG (for a quick look while we're playing with the
code).

The main program above is pretty simple: generate test data, do the
PCA calculation (by calling the `pca` function, which we'll look at in
detail in a minute), do a little bit of data transformation to help
with plotting, then call the `doPlot` function for each of the plots
we want.  Here are the plots we produce, which we'll refer to below as
we work through the PCA calculation:

<a name="figs"></a>
<ul class="nav nav-tabs" role="tablist">
  <li class="active">
    <a href="#fig1" role="tab" data-toggle="tab">
      Figure 1
    </a>
  </li>
  <li>
    <a href="#fig2" role="tab" data-toggle="tab">
      Figure 2
    </a>
  </li>
  <li>
    <a href="#fig3" role="tab" data-toggle="tab">
      Figure 3
    </a>
  </li>
  <li>
    <a href="#fig4" role="tab" data-toggle="tab">
      Figure 4
    </a>
  </li>
</ul>

<div class="tab-content">
<div class="tab-pane active" id="fig1">
Synthetic mussel shell test data for two-dimensional PCA example.

<img src="pca-2d-0.svg">
</div>
<div class="tab-pane" id="fig2">
Centred synthetic mussel shell test data for two-dimensional PCA
example.

<img src="pca-2d-1.svg">
</div>
<div class="tab-pane" id="fig3">
PCA eigenvectors for two-dimensional PCA example.

<img src="pca-2d-2.svg">
</div>
<div class="tab-pane" id="fig4">
Data projection onto PCA eigenvectors for two-dimensional PCA
example.

<img src="pca-2d-3.svg">
</div>
</div>

<br>

Let's now run through the "recipe" for performing PCA, looking at the
figures above in parallel with the code for the `pca` function:

~~~~ {.haskell .numberLines}
pca :: Matrix Double -> (Vector Double, Matrix Double, Matrix Double)
pca xs = (varexp, evecs, projs)
  where (mean, cov) = meanCov xs
        (_, evals, evecCols) = svd cov
        evecs = fromRows $ map evecSigns $ toColumns evecCols
        evecSigns ev = let maxelidx = maxIndex $ cmap abs ev
                           sign = signum (ev ! maxelidx)
                       in cmap (sign *) ev
        varexp = scale (1.0 / sumElements evals) evals
        projs = fromRows $ map project $ toRows xs
        project x = evecs #> (x - mean)
~~~~

We'll look at just why this recipe works in the next section, but for
the moment, let's just see what happens:

1. We start with our original mussel shell data (Figure 1 above).

2. We calculate the mean and covariance of our data (line 3 of the
   `pca` function listing).  PCA analyses the deviations of our data
   from the mean, so we effectively look at "centred" data, as shown
   in Figure 2, where we've just removed the mean from each coordinate
   in our data.  The mean and covariance calculation is conveniently
   done using `hmatrix`'s `meanCov` function.

3. Then we calculate the eigendecomposition of the covariance matrix.
   Because the covariance matrix is a real symmetric matrix, by
   construction, we know that the eigenvectors will form a complete
   set that we can use as a basis to represent our data.  (We're going
   to blithely ignore all questions of possible degeneracy here -- for
   real data, "almost surely" means always!)  Here, we do the
   eigendecomposition using a singular value decomposition (line 4 in
   the listing of the `pca` function).  The singular values give us
   the eigenvalues and the right singular vectors give us the
   eigenvectors.  The choice here to use SVD (via `hmatrix`'s `svd`
   function) rather than some other means of calculating an
   eigendecomposition is based primarily on the perhaps slightly
   prejudiced idea that SVD has the best and most stable
   implementations -- here, `hmatrix` calls out to LAPACK to do this
   sort of thing, so there's probably not much to choose, since the
   other eigendecomposition implementations in LAPACK are also good,
   but my prejudice in favour of SVD remains!  If you want some better
   justification for why SVD is "the" matrix eigendecomposition, take
   a look at this very interesting historical review of the
   development of SVD: G. W. Stewart (1993). On the early history of
   the singular-value decomposition. *SIAM Rev.* **35**(4), 551-566.

4. We do a little manipulation of the directions of the eigenvectors
   (lines 5-8 in the listing), flipping the signs of them to make the
   largest components point in the positive direction -- this is
   mostly just to make the eigenvectors look good for plotting.  The
   eigenvectors are shown in the Figure 3: we'll call them
   $\mathbf{e}_1$ (the one pointing to the upper right) and
   $\mathbf{e}_2$ (the one pointing to the upper left).  Note that
   these are unit vectors.  We'll talk about this again when we look
   at using PCA for spatio-temporal data.

5. Once we have unit eigenvectors, we can project our (centred) data
   points onto these eigenvectors (lines 10 and 11 of the listing: the
   `project` function centres a data point by taking off the mean,
   then projects onto each eigenvector using `hmatrix`'s matrix-vector
   product operator `#>`).  Figure 4 shows in schematic form how this
   works -- we pick out one data point in green and draw lines
   parallel and orthogonal to the eigenvectors showing how we project
   the data point onto the eigenvectors.  Doing this for each data
   point is effectively just a change of basis: instead of
   representing our centred data value by measurements along the $x$-
   and $y$-axes, we represent it by measurements in the directions of
   $\mathbf{e}_1$ and $\mathbf{e}_2$.  We'll talk more about this
   below as well.

6. Finally, the eigenvalues from the eigendecomposition of the
   covariance matrix tell us something about how much of the total
   variance in our input data is "explained" by the projections onto
   each of the eigenvectors.  I've put the word "explained" in quotes
   because I don't think it's a very good word to use, but it's what
   everyone says.  Really, we're just saying how much of the data
   variance lies in the direction of each eigenvector.  Just as you
   can calculate the variance of the mussel length and width
   individually, you can calculate the variance of the projections
   onto the eigenvectors.  The eigenvalues from the PCA
   eigendecomposition tell you how much variance there is in each
   direction, and we calculate the "fraction of variance explained"
   for each eigenvector and return it from the `pca` function.

So, the `pca` function returns three things: eigenvalues (actually
fractional explained variance calculated from the eigenvalues) and
eigenvectors from the PCA eigendecomposition, plus projections of each
of the (centred) data points onto each of the eigenvectors.  The
terminology for all these different things is very variable between
different fields.  We're going to sidestep the question of what these
things are called by always explicitly referring to *PCA eigenvectors*
(or, later on when we're dealing with spatio-temporal data, *PCA
eigenpatterns*), *PCA explained variance fractions* and *PCA projected
components*.  These terms are a bit awkward, but there's no chance of
getting confused this way.  We could choose terminology from one of
the fields where PCA is commonly used, but that could be confusing for
people working in other fields, since the terminology in a lot of
cases is *not* very well chosen.

Together, the PCA eigenvectors and PCA projected components constitute
nothing more than a change of orthonormal basis for representing our
input data -- the PCA output contains *exactly* the same information
as the input data.  (Remember that the PCA eigenvectors are returned
as *unit vectors* from the `pca` function, so we really are just
looking at a simple change of basis.)  So it may seem as though we
haven't really done anything much interesting with our data.  The
interesting thing comes from the fact that we can order the PCA
eigenvectors in decreasing order of the explained variance fraction.
If we find that data projected onto the first three (say) eigenvectors
explains 80% of the total variance in our data, then we may be
justified in considering *only* those three components.  In this way,
PCA can be used as a *dimensionality reduction* method, allowing us to
use low-dimensional data analysis and visualisation techniques to deal
with input data that has high dimensionality.

This is exactly what we're going to do with the $Z_{500}$ data: we're
going to perform PCA, and take only the leading PCA eigenvectors and
components, throwing some information away.  The way that PCA works
guarantees that the set of orthogonal patterns we keep are the "best"
patterns in terms of explaining the variance in our data.  We'll have
more to say about this in the next section when we look at why our
centre/calculate covariance/eigendecomposition recipe works.


### The algebra of PCA

In the last section, we presented a "recipe" for PCA (at least for
two-dimensional data): centre the data; calculate the covariance
matrix; calculate the eigendecomposition of the covariance matrix;
project your centred data points onto the eigenvectors.  The
eigenvalues give you a measure of the proportion of the variance in
your data in the direction of the corresponding eigenvector.  And the
projection of the data points onto the PCA eigenvectors is just a
change of basis, from whatever original basis your data was measured
in (mussel shell length and width as the two components of each data
point in the example) to a basis with the PCA eigenvectors as basis
vectors.

So why does this work?  Obviously, you can use whatever basis you like
to describe your data, but why is the PCA eigenbasis useful and
interesting?  I'll explain this quite quickly, since it's mostly
fairly basic linear algebra, and you can read about it in more detail
in more or less any linear algebra textbook[^3].

To start with, let's review some facts about eigenvectors and
eigenvalues.  For a matrix $\mathbf{A}$, an eigenvector $\mathbf{u}$
and its associated eigenvalue $\lambda$ satisfy

$$ \mathbf{A} \mathbf{u} = \lambda \mathbf{u}. $$

The first thing to note is that any scalar multiple of $\mathbf{u}$ is
also an eigenvector, so an eigenvector really refers to a "direction",
not to a specific vector with a fixed magnitude.  If we multiply both
sides of this by $\mathbf{u}^T$ and rearrange a little, we get

$$ \lambda = \frac{\mathbf{u}^T \mathbf{A} \mathbf{u}}{\mathbf{u}^T
\mathbf{u}}. $$

The denominator of the fraction on the right hand side is just the
length of the vector $\mathbf{u}$.  Now, we can find the largest
eigenvalue $\lambda_1$ and corresponding eigenvector $\mathbf{u}_1$ by
solving the optimisation problem

$$ \mathbf{u}_1 = \underset{\mathrm{arg max}}{\mathbf{u}^T \mathbf{u}
   = 1} \; \mathbf{u}^T \mathbf{A} \mathbf{u}, $$

where for convenience, we've restricted the optimisation to find a
*unit* eigenvector, and we find $\lambda_1$ directly from the fact
that $\mathbf{A} \mathbf{u}_1 = \lambda_1 \mathbf{u}_1$.

We can find next largest (in magnitude) eigenvalue and corresponding
eigenvector of the matrix $\mathbf{A}$ by projecting the rows of
$\mathbf{A}$ into the subspace orthogonal to $\mathbf{u}_1$ to give a
new matrix $\mathbf{A}_1$ and solving the optimisation problem

$$ \mathbf{u}_2 = \underset{\mathrm{arg max}}{\mathbf{u}^T \mathbf{u}
   = 1} \; \mathbf{u}^T \mathbf{A}_1 \mathbf{u}, $$

finding the second largest eigenvalue $\lambda_2$ from $\mathbf{A}_1
\mathbf{u}_2 = \lambda_2 \mathbf{u}_2$.  Further eigenvectors and
eigenvalues can be found in order of decreasing eigenvalue magnitude
by projecting into subspaces orthogonal to all the eigenvectors found
so far and solving further optimisation problems.

This link between this type of optimisation problem and the
eigenvectors and eigenvalues of a matrix is the key to understanding
why PCA works the way that it does.  Suppose that we have centred our
($K$-dimensional) data, and that we call the $N$ centred data vectors
$\mathbf{x}_i$, $i = 1, 2, \dots N$.  If we now construct an $N \times
K$ matrix $\mathbf{X}$ whose rows are the $\mathbf{x}_i$, then the
sample covariance of the data is

$$ \mathbf{C} = \frac{1}{N - 1} \mathbf{X}^T \mathbf{X}. $$

Now, given a direction represented as a unit vector $\mathbf{u}$, we
can calculate the data variance in that direction as $||\mathbf{X}
\mathbf{u}||^2$, so that if we want to know the direction in which the
data has the greatest variance, we solve an optimisation problem of
the form

$$ \mathbf{u}_1 = \underset{\mathrm{arg max}}{\mathbf{u}^T \mathbf{u}
  = 1} \; (\mathbf{X} \mathbf{u})^T \mathbf{X} \mathbf{u} =
  \underset{\mathrm{arg max}}{\mathbf{u}^T \mathbf{u} = 1} \;
  \mathbf{u}^T \mathbf{C} \mathbf{u}. $$

But this optimisation problem is just the eigendecomposition
optimisation problem for the covariance matrix $\mathbf{C}$.  This
demonstrates we can find the directions of maximum variance in our
data by looking at the eigendecomposition of the covariance matrix
$\mathbf{C}$ in decreasing order of eigenvalue magnitude.

There are a couple of things to add to this.  First, the covariance
matrix is, by construction, a real symmetric matrix, so its
eigenvectors form a complete basis -- this means that we really can
perform a change of basis from our original data to the PCA basis with
no loss of information.  Second, because the eigenvectors of the
covariance matrix are orthogonal, the projections of our data items
onto the eigenvector directions (what we're going to call the PCA
projected components) are *uncorrelated*.  We'll see some consequences
of this when we look at performing PCA on the $Z_{500}$ data.
Finally, and related to this point, it's worth noting that PCA is a
*linear* operation -- the projected components are linearly
uncorrelated, but that doesn't mean that there can't be some nonlinear
relationship between them.  There are generalisations of PCA to deal
with this case, but we won't be talking about them for the purposes of
this analysis.

Everything we've done here is pretty straightforward, but you might be
wondering why we would want to change to this PCA basis at all?
What's the point?  As I noted above, but is worth reiterating, the
most common use for PCA, and the way that we're going to use it with
the $Z_{500}$ data, is as a *dimensionality reduction* method.  For
the $Z_{500}$ data, we have, for each day we're looking at, $72 \times
15 = 1080$ spatial points, which is a lot of data to look at and
analyse.  What we usually do is to perform PCA, then *ignore* all but
the first few leading PCA eigenvectors and projected components.
Because of the way the optimisation problems described above are set
up, we can guarantee that the leading $m$ PCA eigenvectors span the
$m$-dimensional subspace of the original data space containing the
most data variance, and we can thus convince ourselves that we aren't
missing interesting features of our data by taking only those leading
components.  We'll see how this works in some detail when we do the
PCA analysis of the $Z_{500}$ data, but in the mussel measurement
case, this would correspond to thinking just of the projection of the
mussel length and width data along the leading $\mathbf{e}_1$
eigendirection, so reducing the measurements to a single "size"
parameter that neglects the variation in fatness or skinniness of the
mussels.  (This two-dimensional case is a bit artificial.  Things will
make more sense when we look at the 1080-dimensional case for the
$Z_{500}$ data.)

[^1]: Well, not really, since I live in the mountains of Austria and
      there aren't too many mussels around here, so I'll generate some
      synthetic data!
[^2]: Obviously, a Gaussian distribution is *not right* for quantities
      like lengths that are known to be positive, but here we're just
      generating some data for illustrative purposes, so we don't care
      all that much.  If we were trying to *model* this kind of data
      though, we'd have to be more careful.
[^3]: I like Gilbert Strang's *Linear Algebra and its Applications*,
      although I've heard from some people that they think it's a bit
      hard for a first textbook on the subject -- if you've had any
      exposure to this stuff before though, it's good.

[chart]: http://hackage.haskell.org/package/Chart
[gist]: https://gist.github.com/ian-ross/d39bf143ffc482ea3700
