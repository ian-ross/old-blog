---
author: Ian
title: "Non-diffusive atmospheric flow #4: exploring Z500"
tags: data-analysis
published: 2014-09-01 20:31:45
---

In the [last article][last], I talked a little about geopotential
height and the $Z_{500}$ data we're going to use for this analysis.
[Earlier][netcdf], I talked about how to read data from the NetCDF
files that the NCEP reanalysis data comes in.  Now we're going to take
a look at some of the features in the data set to get some idea of
what we might see in our analysis.  In order to do this, we're going
to have to produce some plots.  As I've said before, I tend not to be
very dogmatic about what software to use for plotting -- for simple
things (scatter plots, line plots, and so on) there are lots of tools
that will do the job (including some Haskell tools, like the
[Chart][Chart] library), but for more complex things, it tends to be
much more efficient to use specialised tools.  For example, for 3-D
plotting, something like Paraview or Mayavi is a good choice.  Here,
we're mostly going to be looking at geospatial data, i.e. maps, and
for this there aren't really any good Haskell tools.  Instead, we're
going to use something called NCL ([NCAR Command Language][ncl]).
This isn't by any stretch of the imagination a pretty language from a
computer science point of view, but it has a *lot* of specialised
features for plotting climate and meteorological data and is pretty
perfect for the needs of this task (the sea level pressure and
$Z_{500}$ plots in the last post were made using NCL).  I'm not going
to talk about the NCL scripts used to produce the plots here, but I
might write about NCL a bit more later since it's a very good tool for
this sort of thing.

<!--MORE-->

The first thing we can do is to look at some snapshot views of
different patterns of $Z_{500}$.  This set of plots shows a sequence
of variations in the "normal" pattern of flow over the North Atlantic
-- contours of constant $Z_{500}$ run more or less east-to-west,
representing the prevailing westerly winds we find there:

<div class="centered-image"><a href="z500-normal-snapshots.png">![Normal Z500 snapshots](z500-normal-snapshots.png)</a></div>

Remember that because of the rotation of the Earth, the wind blows
more or less *along* lines of constant pressure in the atmosphere,
flowing counter-clockwise around areas of low pressure in the Northern
hemisphere.  The plots here cover a period of nine days, and you can
see that the pattern of $Z_{500}$ is pretty consistent over that
period.  There is some "waviness" in the main east-west pattern of
contours over the North Atlantic, but there's a pretty constant band
of gradient in $Z_{500}$ running from Nova Scotia and St. Pierre et
Miquelon to the United Kingdom and Scandinavia.  There are some other
"bulls-eye" features that come and go on timescales of a few days
(localised regions of lower or higher surface pressure), but the
prevailing east-west pattern is quite clear.

The next plots show another nine-day period from just
a few days later than the sequence above:

<div class="centered-image"><a href="z500-blocking-snapshots.png">![Blocking Z500 snapshots](z500-blocking-snapshots.png)</a></div>

Here, the pattern over the North Atlantic is quite different to the
first plots.  We have a big incursion of higher values of $Z_{500}$
coming in from the southern part of the North Atlantic, eventually
forming an isolated region of higher pressure with a distinctive
"horseshoe" or "omega" (i.e. &Omega;) shape around it that persists
from about 17 January until about 21 January.  This atmospheric flow
regime is usually called a "blocking flow", since the prevailing
westerly winds over the North Atlantic are "blocked" by a region of
high pressure.  Similar patterns occur over the North American
continent.  Again, these patterns persist for a few days or so, with
some smaller scale variation.

The "normal" (first plots) and "blocking" (second plots) flows in the
North Atlantic tend to be associated with different kinds of weather
over Northern Europe, particularly in the Northern Hemisphere winter.
The high pressure systems associated with blocking flow lead to
periods of settled fine weather over Northern Europe, while the
"normal" east-west pattern of flow is more associated with variable
weather and the propagation of low-pressure weather systems from North
America across the Atlantic to Europe (along the North Atlantic "storm
track", which is the band of prevailing westerly flow seen in first
plots).

We can get some idea of the temporal behaviour of these different
regimes from this plot:

<div class="centered-image"><a href="z500-time-series.png">![Z500 time series](z500-time-series.png)</a></div>

This shows a smoothed time series of the spatial average of $Z_{500}$
over the region 20&deg;W--0&deg;E, 50&deg;N--60&deg;N (an area of the
North Atlantic and most of the United Kingdom).  The higher pressures
(and consequently higher $Z_{500}$) associated with the blocking flow
starting around 15 January is quite clear (compare the time series
plot with the map views of the blocking conditions above), as is the
persistence of these patterns over time -- the blocking flow appears
to persist until the last week of January in this case.  Also visible
on the time series plot are fluctuations in the spatial mean of
$Z_{500}$ over this region on timescales of a few days or so (this is
particularly clear for the period from 1 December 1999 until 15
January 2000).  These fluctuations are associated with the passage of
weather systems across the Atlantic and with the "waviness" we noted
in in the first set of "normal" spatial plots.  (These really are
waves in the atmosphere, called Rossby waves.)

So there are characteristic spatial patterns in the North Atlantic
atmosphere that persist in time and can be associated with different
surface weather regimes.  What we want to try to do is to disentangle
some of this variability, to determine just what these persistent
regimes are and to decide whether there is any predictability in the
dynamics of transition between these different regimes.

To give you a better feeling for what these variations look like, here
is an animation that displays variations in $Z_{500}$ over a few
interesting periods:

<div class="centered-image-noscale">
<iframe src="//player.vimeo.com/video/104223833" width="500" height="529" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>
</div>

The values of $Z_{500}$ are shown as coloured regions (warmer colours
are larger $Z_{500}$ values) and regions of blocking flow are
highlighted with cross-hatching.  (The exact criterion used to
identify these regions isn't all that important for our purposes here
-- it's just a way of picking out and highlighting these flow regimes
for display.)  When viewing the animation, notice in particular:

1. the fairly persistent gradient of $Z_{500}$ from the equatorial
   regions towards the poles (the orange to yellow or green transition
   in the mid-latitudes);

2. the appearance of lower-pressure regions that persist for a day or
   two (mostly showing up as green circles);

3. the wave-like disturbances that appear on the main equator-to-pole
   gradient of $Z_{500}$;

4. the blocking pattern that appears in the North Atlantic around 14
   January 2000 (compare with the blocking maps above), associated
   with persistent high pressures over North Europe, and its
   subsequent decay back to the "normal" flow regime;

5. in the polar view later in the animation, the wave-like patterns of
   $Z_{500}$ centred on the North Pole and the occurrence of blocking
   flow over the North Atlantic and North Pacific (highlighted by
   cross-hatching).

In the next article, we'll get back to Haskell, and we'll start with
the most basic part of the data analysis that we need to do to examine
these persistent flow regimes, by doing some data pre-processing.
This is a step that we need to do in almost all scientific data
analysis tasks, since it's rare that the data we get are exactly the
data that we need.

[last]: /blog/posts/2014/08/29/data-analysis-ao1-3/index.html
[netcdf]: /blog/posts/2014/07/16/data-analysis-ao1-1.html
[Chart]: http://hackage.haskell.org/package/Chart
[paraview]: http://www.paraview.org/
[mayavi]: http://code.enthought.com/projects/mayavi/
[ncl]: http://www.ncl.ucar.edu/
