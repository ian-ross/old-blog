---
author: Ian
tags: haskell,data-analysis
title: Data Analysis in Haskell
published: 2013-11-05 22:08:13
---

I'm in the process of starting up a blogging series, to write about
*Data Analysis in Haskell*.  This is a sort of
introduction/manifesto...

### What's "Data Analysis"?

To me, *data analysis* means turning *data* (bits and bytes) into
*knowledge* (some higher level of conceptual information).  It can be
exploratory (i.e. we don't really know what we're doing when we start,
we just discover along the way) or something more organised.  It
usually involves a few common tasks: data intake and pre-processing,
some "main analyses" and some post-processing and visualisation.  The
input data can range from a few measurements in the laboratory to
enormous remote sensing data sets.  The main analyses can range from
simple data aggregation to complicated inverse modelling or even
weirder stuff.

I've done a lot of this sort of work in the past, mostly in climate
science or environmental remote sensing, and almost exclusively using
Fortran, C++ or R.  I've been using Haskell a lot over the past year
and a bit though, and I've been thinking more and more that it's a
perfect language for this sort of work.

### Why Haskell?

Most of the serious programming work I've done in this area has been
kind of unsatisfactory.  For developing new kinds of data analyses,
you spend some time sitting down with pen and paper to work out the
details of your algorithms, then you code them up (in C++ say).  The
code you write often bears little relationship to the equations you
write down on paper, and you spend a lot of time bogged down in the
minutiae of iterating over arrays, avoiding off-by-one errors,
managing memory, and so on.  It's hard to make a direct link between
your mathematics and your code, which leads to mistakes.

Haskell provides an opportunity to make this situation quite a bit
nicer, I think.  [Some][le1] [experiments][le2] I did a while ago with
Lyapunov exponent calculations for dynamical systems convinced me that
Haskell is a great vehicle for mixed algebraic/numerical calculations
-- you can easily work with symbolic representations of the
mathematical structures you care about, then simplify and convert
these to efficient numerical calculations.  You're unlikely to get
performance that competes with Fortran that's been lovingly hand-coded
and optimised over decades of effort, but you'll be able to move much
more quickly while getting some help with the algebra *and* ending up
reasonably efficient numerical code that you can be confident works.

### What I'm going to do

To put this somewhat hubristic series of pronouncements to the test,
I'm going to try doing some "serious" data analysis examples in
Haskell.  I'm mostly interested in scientific applications, so that's
where I'm going to concentrate.  The applications I have in mind
include some atmospheric science (the field I did my PhD in), some
non-linear time series analysis, some remote sensing work, some
medical imaging, and a bunch of other fun stuff.  Here's an incomplete
list of what I have on my "probable" list so far:

 * Ultra-low frequency variability in the northern hemisphere
   atmosphere;
 * Critical scaling in tropical rainfall rates;
 * Inverse methods for medical X-ray CT imaging;
 * Nonlinear time series methods for noise reduction and signal
   extraction (extracting foetal heart rate data from ECG signals);
 * Land cover classification from MODIS/ASTER data -- applications to
   hydrology of North African saline lakes;
 * Landform classification from laser altimetry data (using data from
   the Mars Orbital Laser Altimeter).

What all of these applications have in common is that they require
data intake from large and complicated data sets, often in obscure
file formats, they require significant domain knowledge to make any
real progress, they tend to require analyses that are mathematically
complex and are in some way "non-standard", and they require complex
(or if not complex, at least good!) data visualisation.

The goal is to as much of this stuff in Haskell in a way that gives
some pedagogical insight into what's going on but is still efficient
enough to do the job.  (For some of these problems, that's a tall
order, but we'll see how we go.)

### Warm-up

Now, I have a couple of slightly easier examples to start with (one
that I've even done more or less end-to-end in C++ and R before now),
but I wanted to start with something venerable to demonstrate some
interesting Haskell things before I really get into the meat of data
analysis examples.

In the world of data analysis algorithms, you don't get much more
venerable than the fast Fourier transform.  The market for new FFT
software has been quite effectively killed by [FFTW][fftw] ("the
fastest Fourier transform in the West"), but that doesn't mean there
aren't things to be learnt.  So, the first few articles in this series
are going to be about an attempt to implement a mixed-radix FFT in
Haskell with empirical compile-time optimisation of the data
decomposition ordering.  This sort of empirical optimisation is one of
the keys to FFTW's success, and is also used by the [ATLAS][atlas]
linear algebra libraries.

### What we'll learn

There are a number of different things we'll be able to look at with
this:

1. A technique I call "toy computer algebra".  The heart of the FFT is
a clever matrix decomposition that turns an $O(n^2)$ algorithm into an
$O(n \log n)$ algorithm.  This decomposition isn't obvious, and for
any reasonable size of matrix, working through the decomposition by
hand is extremely tedious.  You could just fire up Mathematica or
Maple, but I prefer a different approach that's really easy in
Haskell, and that can give you a lot more insight into what's going
on.

2. For numerical code, profiling and benchmarking is often really
important.  Even once you've found a nice $O(n \log n)$ algorithm,
real-world performance can be disappointing (damn those constant
factors!).  Once you have code that works, there's no substitute for
measuring performance.  Haskell has some great facilities for
benchmarking (primarily Bryan O'Sullivan's [criterion][criterion]
package) and we'll do lots of performance measurement this way.

3. Once you can measure performance, you can use those measurements do
drive optimisation.  The FFT algorithm we're going to look at has a
great deal of flexibility in the order that calculations are
performed, flexibility that can have a big impact on performance,
depending on input data size, your machine's processor, and cache
characteristics.  The enormous success of FFTW indicates pretty
clearly that empirical optimisation is the way to go -- you could
perhaps come up with optimal execution plans for some data sizes for
some known cache architectures, but you'd have to redo that analysis
every time you moved to a new machine.

4. Haskell has a powerful metaprogramming facility called Template
Haskell.  I'm going to try to use this to move the benchmarking and
FFT executaion plan optimisation to compile time.  (This might be
tricky...)

Anyway, next time, I'll give a quick introduction to the discrete
Fourier transform to give some grounding before we talk about the FFT.

[le1]: /blog/posts/2012/11/01/lyapunov-exponents-1/index.html
[le2]: /blog/posts/2012/11/06/lyapunov-exponents-2/index.html
[fftw]: http://www.fftw.org/
[atlas]: http://math-atlas.sourceforge.net/
[criterion]: http://hackage.haskell.org/package/criterion
