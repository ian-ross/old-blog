---
author: Ian
tags: day-job
timestamp: 10:37:46
title: Some recent(ish) papers
---
Some recent-ish papers of mine, that is.  Although I didn't enjoy my
last post-doc in Montpellier a whole lot, there was a small amount of
output from it, which probably deserves recording.  So here goes.

> I. Ross, L. Misson, S. Rambal, A. Arneth, R. L. Scott, A. Carrara,
A. Cescatti, L. Genesio (2012).  How do variations in the temporal
distribution of rainfall events affect ecosystem fluxes in seasonally
water-limited Northern Hemisphere shrublands and forests?
_Biogeosciences_ **9**(3), 1007-1024.  [PDF][precipitation]

This was a paper that was started by Laurent Misson, who worked at
CEFE until his death in a mountaineering accident in March 2010.  I
picked it up, redid and revised the analysis and rewrote the paper,
and we published it as a sort of tribute to Laurent.

The motivation for the analysis in the paper is that projections of
future climate change seem to suggest that, even if total rainfall
might not change all that much, the temporal distribution of rainfall
events might change quite a bit, with less frequent but more intense
rainfall events.  This is potentially quite important for
water-limited ecosystems, where plants may already be living in quite
marginal conditions in terms of water stress.

In water-limited grassland ecosystems, it's pretty well established
that grass growth can be triggered by very short pulses of rainfall
that only substantially wet the uppermost soil layers.  What then
about woody ecosystems, where the vegetation structure tends to be
more complex?  We'd like to know something about the relative impact
of changes in rainfall distribution on plant productivity and
respiration (including microbial soil respiration) and the net outcome
in terms of ecosystem carbon fluxes.

We took a look at what happens by using data from the
[FLUXNET][fluxnet] project, a worldwide network of flux towers
measuring carbon fluxes and a whole bunch of associated data for
long-term ecological and climate studies.  The work mostly consisted
of processing this data and trying to pick out the effects of
variations in precipitation regimes in a systematic way.  We found
enough of an effect to get it published in Biogeosciences!

> R. Wania, K. J. Meissner, M. Eby, V. K. Arora, I. Ross, A. J. Weaver
(2012).  Carbon-nitrogen feedbacks in the UVic ESCM. _Geosci. Model
Dev._ **5**, 1137-1160.  [PDF][uvic]

This is the latest in a long series of collaborations with my partner
Rita.  For this one, my contributions were a weekend-long hacking
session to get a first version of the nitrogen model that Rita wanted
to use working and figuring out some of the details of how the
vegetation model within the UVic ESCM _really_ works.  It was quite
interesting, although we're not completely convinced about the
scientific merit of putting a detailed nitrogen model into a
coarse-scale earth system model.  But hey, it's the fashion.

One thing that is very interesting to me is the disparity in work
involved between a first author paper (often, full-time for months)
and a second-or-subsidiary author paper (sometimes, not much more than
a few discussion sessions with the principal authors).  Out of ten
papers in my publication list, only two are first author papers.  That
either shows that I play very nicely with others, or that I'm
pathologically lazy and exploit other scientists to pad my publication
list!

> F. Gogé, R. Joffre, C. Jolivet, I. Ross, L. Ranjard (2012).
Optimization criteria in sample selection step of local regression for
quantitative analysis of large soil NIRS database.
_Chemometr. Intell.  Lab._ **110**(1), 168-176.

This was another paper where my contribution was, I would like to
think, limited in temporal extent, but pivotal in terms of scientific
impact.  (Heh.)  One of the things I spent a lot of time thinking
about during my PhD was dimensionality reduction methods.  It turns
out that these are used a lot in the spectral analysis of ecological
samples (soils in this case), so I could offer a bit of advice to some
coworkers at CEFE.

So, these three papers were more or less the entire output from an
18-month post-doc (I left before the two years were up).  I had
another manuscript that was quite interesting almost ready to go when
I left, but I couldn't really summon up the enthusiasm for a last push
on that.  It was another FLUXNET paper, this time about the
relationship between global radiation (basically, sunshine) and net
radiation (the balance of shortwave solar radiation and longwave
thermal radiation) over different kinds of vegetation canopies.  There
are some interesting general relationships between these quantities
that have been known since the 1960s or 70s, but no-one had done a
really comprehensive comparative study of a lot of different sites of
different ecosystem types.  I don't know if that stuff will ever get
published, but the statistical models I built might be quite useful.

Apart from that, there was another abortive effort to use boosted
regression trees to model fire occurrence in the Languedoc-Roussillon
region.  That failed because of a paucity of data, but a manuscript
got to its second round of reviewing before I spotted a big mistake in
the calculations that had made it look as though we _could_ get some
sort of decent results.  So we had to withdraw that one.

Anyway, I think that might be my last desperate gasp for academic
papers.  If I do write any more in the future, I'm going to make sure
that they are _fun_!

[precipitation]: http://www.biogeosciences.net/9/1007/2012/bg-9-1007-2012.html
[fluxnet]: http://www.fluxdata.org/
[uvic]: http://www.geosci-model-dev.net/5/1137/2012/gmd-5-1137-2012.html
