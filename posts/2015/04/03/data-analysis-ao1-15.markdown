---
author: Ian
title: "Non-diffusive atmospheric flow #15: Wrap-up"
tags: data-analysis
published: 2015-04-03 09:13:31
---

OK, so we're done with this epic of climate data analysis.  I've
prepared an [index][index] of the articles in this series, on the off
chance that it might be useful for someone.

The goal of this exercise was mostly to try doing some "basic" climate
data analysis tasks in Haskell, things that I might normally do using
R or NCL or some cobbled-together C++ programs.  Once you can read
NetCDF files, a lot of the data manipulation is pretty easy, mostly
making use of standard things from the `hmatrix` package.  It's really
not any harder than doing these things using "conventional" tools.
The only downside is that most of the code that you need to write to
do this stuff in Haskell already exists in those "conventional" tools.
A bigger disadvantage is that data visualisation tools for Haskell are
pretty thin on the ground -- `diagrams` and `Chart` are good for
simpler two-dimensional plots, but maps and geophysical data plotting
aren't really supported at all.  I did all of the map and contour
plots here using UCAR's [NCL language](http://www.ncl.ucar.edu/) which
although it's not a very nice language from a theoretical point of
view, has built-in capabilities for generating more or less all the
plot types you'd ever need for climate data.

I think that this has been a reasonably useful exercise.  It helped me
to fix a couple of problems with my `hnetcdf` package and it turned up
a bug in `hmatrix`.  But it went on a little long -- my notes are up
to 90 pages.  (Again: the same thing happened on the FFT stuff.)
That's too long to maintain interest in a problem you're just using as
a finger exercise.  The next thing I have lined up should be quite a
bit shorter.  It's a problem using satellite remote sensing data,
which is always fun.

[index]: /haskell-data-analysis-ao1-index.html
