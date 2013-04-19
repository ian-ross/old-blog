---
author: Ian
tags: day-job
published: 2012-12-21 12:06:56
title: The Contracting Life
---
Ah, another involuntary blogging hiatus.  This time, for a good reason
though.  I have work.  And it's work that pays money, is interesting
and has some future in it.  It's pretty cool.  I've also had an offer
of more work that I've had to defer until next year, which is
encouraging.  It seems as though this contracting thing might actually
work out.

I have two clients I'm working with at the moment.  The first is my
old research group in Bristol.  I'm doing some Fortran programming for
them, extending a tool they use for generating input files for the
main climate model that they use.  It's not a very complicated job,
apart from the historical aspects -- there are three main versions of
the code for this tool, one of which comes in 269 distinct, slightly
different versions...  So sorting out which version is most
"canonical" is an interesting problem.  I've calculated
[Levenshtein distances](https://en.wikipedia.org/wiki/Levenshtein_distance)
between all the distinct versions and have used hierarchical
clustering to get some idea of the historical relationships between
all these versions.  I think that this is probably going to be the
trickiest part of the job!  (There's also a Tcl/Tk GUI that will need
to be modified, which will be about as much fun as a root canal, but
the changes needed should be pretty localised.)

The second client is a start-up that's doing some really interesting
stuff.  They're still in a pre-release development phase, so I'm not
sure how much I'll be able to blog about for now, but suffice it to
say that there's Haskell, Bayesian statistics, Markov chain Monte
Carlo and data visualisation aspects to it.  I've spent the last few
weeks working on part of the web GUI front end, which has been an
eye-opener, since it involved rather more JavaScript than is good for
my mental health.  I'll write about some of that in general terms over
the next few days.

I have another mad idea that I'm going to start on Monday.  I'll write
about the details of that tomorrow, but it will either result in a
100% productivity increase or some sort of institutionalisation.
We'll see.  More from the Department of Irresponsible Human
Experimentation tomorrow...
