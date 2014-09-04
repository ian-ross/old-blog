---
author: Ian
title: Non-diffusive atmospheric flow #2: outline & plan
tags: data-analysis
published: 2014-08-27 11:56:32
---

As I said in the last article, the next bit of this data analysis
series is going to attempt to use Haskell to reproduce the analysis in
the paper:
[D. T. Crommelin (2004). Observed nondiffusive dynamics in large-scale atmospheric flow. *J. Atmos. Sci.* **61**(19), 2384--2396][crommelin].
Before we can do this, we need to cover some background, which I'm
going to do in this and the next couple of articles.  There won't be
any Haskell code in any of these three articles, so I'm not tagging
them as "Haskell" so that they don't end up on Planet Haskell,
annoying category theorists who have no interest in atmospheric
dynamics.  I'll refer to these background articles from the later
"codey" articles as needed.

<!--MORE-->

### The problem

So what's the problem that this paper considers?

We all live in the atmosphere, and we're all intimately familiar with
its different moods.  The weather can change from day to day and from
place to place, and superimposed on that short-term variability are
larger scale patterns of climate variability, both temporal (the
seasons, slower natural cycles like El Niño, responses to point events
such as volcanic eruptions, as well as longer term trends due to human
influence or other factors) and spatial (Manila is warmer than
Vladivostok!).

Understanding all the factors involved in the variability of weather
and climate is a big topic, but people who study the dynamics of the
atmosphere have observed that short-term variations (day-to-day
weather) tend to be more random and less predictable than some
longer-term variations.  One way of thinking about this is that there
is some large-scale nonlinear dynamics that influences the slower
variations, and shorter-term random variations are forced on top of
that.

In some senses, everyone has experience of this: it's quite common,
even in the middle of winter in many places, to experience periods of
a few days of fine weather in between periods of atrocious weather.
On a weather map this might appear as a stable region of high pressure
hanging around for a few days before being swept off to be replaced by
storms and rain.

So, can we do some *science* to try to understand some aspects of
what's going on here?  In this little study, we're going to use a
single atmospheric data set that's reasonably easy to explain and the
data analysis approach from Daan Crommelin's paper to try to answer
these questions:

 * Can we identify these more stable patterns in the atmosphere, these
   patterns that "hang around"?

 * Can we convince ourselves that these patterns are in some sense
   real, and not just an artefact of whatever data analysis approach
   we use?

 * Given that we can identify such patterns, can we then identify
   preferred sequences of transitions between them?  Or put another
   way, is there any predictability in the transitions between these
   patterns that might hint at some deterministic component to the
   dynamics driving them?

We're going to take a brutally instrumental approach to atmospheric
physics, dynamical systems theory, and a dozen other fields.  I am
going to lie outrageously to steer us out of deep waters, I am going
to skim fearlessly past technical details, "funnies" and many
interesting topics on which an interested reader could base a whole
PhD thesis.  Instead, I'm going to focus on the data analysis tasks we
need to do to follow the chain of reasoning in the paper.  I will try
to introduce just as much atmospheric physics and dynamical systems
theory as is needed to understand what's going on and to motivate the
data analysis, but no more.



### The plan

I think of scientific data analysis as involving four main steps: data
intake, pre-processing, the main analyses and data visualisation.  In
a typical problem, you'll iterate over some or all of these steps
multiple times.  As we work on our atmospheric data analysis problem,
we'll see aspects of each of these four steps:

#### Data intake

The data that we're going to use is held in NetCDF files, and we'll
use some of the techniques from the last post to read and process
these in Haskell.

#### Pre-processing

The data we're going to use is a very general "reanalysis" data set
that contains lots of different atmospheric data at relatively high
spatial resolution.  We're going to be interested in large-scale
patterns in the Northern Hemisphere winter atmosphere, so we're going
to do some spatial and temporal subsetting, and we're also going to
remove the seasonal cycle from our data (I'll explain why later).  All
of these tasks are common pre-processing steps when dealing with
climate data.  We'll write our pre-processed data into a NetCDF file
for further analysis.

#### Main analyses

In order to answer the questions we're interested in, we need to do a
number of different analyses.  First, we need to look for spatial
patterns in the atmosphere.  For this, we'll use a standard technique
called principal components analysis (PCA).  Once we have some
patterns, we'll use an interesting method to simplify the results to
help with visualisation and to set things up so that we can do some
significance testing to figure out whether these patterns are in some
sense "real".  This step will involve kernel density estimation to
build a kind of probability distribution function for our patterns.

Once we've identified these spatial patterns, we need to look at
dynamics, i.e. transitions between different patterns.  To do this,
we'll build a Markov chain representing probabilistic transitions
between different patterns.  We can then partition the transition
matrix of this Markov chain into diffusive ("random") and
non-diffusive (not quite "deterministic", but more predictable) parts
and see what we can see.  The final step will be to try to determine
whether the transitions that we find (which will be stated
probabilistically) are statistically significant.  As well as the
method used to do this in the paper, I might have a play with another
approach that I've been thinking about.

#### Visualisation

Data visualisation is important during all of the preceding steps.
When you first get a data set, you usually want to do some sort of
"quick look" or exploratory visualisation to get some idea of what you
have, what pre-processing you might want to do and what main analyses
might make sense.

During and after pre-processing, data visualisation is used to check
on results as you go and to look at your final pre-processed data to
ensure that it has the properties that you expect.

Once you move on to the main analyses, you obviously need to visualise
your results one way or another, both as you go along, to make sure
that things are working and to help come up with ideas of what to do
next, and then, once you're done, to prepare high-quality
visualisations for publication.

There are at least four different kinds of data visualisation that are
needed in many projects (the last two aren't always needed):

1. a highly interactive exploratory environment for exploratory data
   analysis and the kind of "quick and dirty" visualisation you need
   to do when first looking at a data set;

2. a scriptable tool for producing visualisations of
   low-to-intermediate "working" quality to use as you work on the
   data analysis -- it's often the case that you'll want to do
   multiple analyses of the same type and view the results together,
   or that you'll get some details of an analysis wrong first time and
   need to rerun it and reproduce all your plots;

3. a tool for producing high-quality visualisations for final
   publication;

4. a tool for producing interactive web-based visualisations.

(I won't talk about the fourth item on this list too much, although
for simple data, I tend to use the [Radian][radian] HTML plotting
library that I wrote, which is based on the much more flexible
[D3.js][d3js] library.)

If the data that you're dealing with is simple enough, you can often
use the same tool for all of the first three items on this list.  If
you just need some scatter and line plots, more or less any tool will
do the job.  When things are more complicated, you have a combination
of different kinds of data and you need a wide range of different
kinds of visualisation, you may end up using three or four different
tools.  In our atmospheric data example, we're going to need to
produce the following types of plot: Northern Hemisphere polar
projection map plots of atmospheric patterns, some scatter and line
plots, and projections of probability distribution functions on a
sphere (in various different ways with various different kinds of
annotations).

I tend not to be very dogmatic about data visualisation tools,
primarily because I have experience of trying to use inappropriate
tools ("because they're the tools I use!") and struggling, then
switching over to a more appropriate tool and producing the results I
wanted very quickly and easily.  In this series of articles, I'm not
going to try to do all the data visualisation in Haskell, since it's
kind of a waste of time, both because there aren't any good existing
data visualisation tools in Haskell yet, and because I want to
concentrate on the data *analysis* side of things.  I'll talk about
the tools I use and how best to drive them from Haskell as we go
along.


[crommelin]: http://journals.ametsoc.org/doi/full/10.1175/1520-0469%282004%29061%3C2384%3AONDILA%3E2.0.CO%3B2
[radian]: http://openbrainsrc.github.io/Radian/
[d3js]: http://d3js.org/