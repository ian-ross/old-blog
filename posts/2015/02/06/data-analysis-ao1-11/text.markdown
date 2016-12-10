---
author: Ian
title: "Non-diffusive atmospheric flow #11: flow pattern visualisations"
tags: data-analysis
published: 2015-02-06 14:41:51
---

A quick post today to round off the "static" part of our atmospheric
flow analysis.

Now that we've satisfied ourselves that the bumps in the spherical PDF
in [article 8 of this series][blog8] are significant (in the narrowly
defined sense of the word "significant" that we've discussed), we
might ask what to sort of atmospheric flow regimes these bumps
correspond.  Since each point on our unit sphere is really a point in
the three-dimensional space spanned by the first three $Z_{500}$ PCA
eigenpatterns that we calculated earlier, we can construct composite
maps to look at the spatial patterns of flow for each bump just by
combining the first three PCA eigenpatterns in proportions given by
the "$(x, y, z)$" coordinates of points on the unit sphere.

<!--MORE-->

Here's what these spatial patterns of $Z_{500}$ variation look like
for the four bumps we labelled in the [spherical PDF][blog8] .  Here,
we really aren't all that interested in the scale of the contours,
just their spatial patterns.

<br>
<div class="img-center">
<a href="pdf-bump-pattern.png">![PDF bump patterns](pdf-bump-pattern.png)</a>
</div>
<br>

Of most interest here are the distinct patterns of flow seen in the
first two bump patterns.  Pattern 2 appears to be something closer to
the "normal" flow regime, with contours of $Z_{500}$ variation running
more or less east-west across the Atlantic (representing the
prevailing westerly winds there).  Pattern 1, on the other hand, is
much more like a "blocking" pattern, with contours of $Z_{500}$
variation bulging downwards into the Atlantic from over Greenland (you
can compare these to the patterns of $Z_{500}$ in
[an earlier article][blog4].  The correspondence between the patterns
here and the hand-selected "normal" and "blocking" regimes shown in
the earlier figures isn't perfect, but it's quite interesting that the
analysis here (pre-processing, PCA, truncation to 3 PCA components,
projection to the unit sphere, KDE on the unit sphere) has led us to
patterns of spatial variation that are similar to what we think of as
these archetypal flows.

At this point, we have developed a purely static picture of what's
going on: we've established that there are statistically significant
patterns of atmospheric flow that occur in our data set that
correspond to our ideas of what are "normal" and "blocking"
regimes.  We've not yet said anything about transitions between these
regimes.  This question of dynamics is what we'll address next.


[gist]: https://gist.github.com/ian-ross/e748bc06fa115796bbf7
[blog8]: /blog/posts/2015/01/27/data-analysis-ao1-8/index.html
[blog4]: /blog/posts/2014/09/01/data-analysis-ao1-4/index.html
