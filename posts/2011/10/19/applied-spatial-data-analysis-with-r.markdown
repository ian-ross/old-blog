---
author: Ian
tags: book-reviews,day-job,r,gis
published: 2011-10-19 19:20:58
title: Applied Spatial Data Analysis With R
---
*by Roger S. Bivand, Edzer J. Pebesma & Virgilio Gómez-Rubio*

I recently had to do a bunch of geostatistical analysis on some
climate data (to be specific, using universal kriging to interpolate a
time series of solar radiation data covering the region I'm working on
to a different grid).  I started off trying to use the geostatistical
analysis toolbox in ArcGIS, which works fine as far as it goes, but
seems to be very difficult indeed to access via ArcGIS's Python
scripting interface.  Since I had 36 years of daily data to process,
doing it by hand was not an option.

<!--MORE-->

I did the job using R, eventually, more by trial and error than
through any great process of inspiration.  I'd never done much
geostatistical processing with R before, although I use it quite a lot
for "normal" statistics and data analysis.  My little voyage through
the land of R-GeoStatistica was quite an eye opener.  You can really
get a lot done this way.  If you know what you're doing.  Which is
where this excellent little book comes in.

Part of the *Use R!* series published by Springer, *Applied Spatial
Data Analysis With R* is written by the authors of some of the main R
geostatistical packages, in particular the `sp` package which is used
for the representation of geostatistical data.  The book is divided
into two main parts, the first dealing with general issues related to
the representation of spatial data, and the second part divided into
three sections dealing with point data, interpolation and
geostatistics, and areal data.  The point data and areal data sections
seem to be well done, but they're about subjects I'm not too familiar
with, and that I don't really need to know about for my current work.
I mostly just skimmed them, but they seem comprehensive and
well-explained.  The interpolation and geostatistics section was what
I was really here to see, and it's good.

If I'd had this book before I started, I could easily have saved
myself a day or two of scrabbling around not quite knowing which
packages to use or how to set things up.  The examples in the book are
realistic, comprehensive and clear, and they serve as a very good
basis to build from.  *Applied Spatial Data Analysis With R* is never
going to be ranked among the greats of scientific literature, but
that's not its aim -- its aim is to explain how to do spatial data
analysis in R, and it does that perfectly.  Highly recommended.

