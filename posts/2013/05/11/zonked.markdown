---
author: Ian
tags: day-job, web-programming, haskell
title: Zonkatonium
published: 2013-05-11 22:20:33
---

In the last three days, I've averaged 10 hours of billable time per
day.  That's ten hours per day actually standing at my desk coding,
perhaps 10-20% more than that "on the job".  I've been working on
social and search aspects of the [BayesHive][bh] web app: we have a
pile of different kinds of items (data sets, documents, models, and
some others) that are all searchable and shareable in slightly
different ways, and this all needs to be managed in a sensible way.

After a frenzy of hacking, I merged the branch I'd been working on
just before dinnertime.  It all works pretty well, I think, although
there are probably some holes.  It's pretty inefficient, but we're
planning to move to a different storage system for documents and data
soon, and that will make full-text indexing much easier, streamlining
all of this stuff, so I was happy to produce a clean design and not
worry too much about optimisation for now.

I've been really enjoying this work.  I think what we have now may be
one of the more complex Haskell web apps out there, and we're
gradually converging on a design that looks really pretty nice.  The
combination of Haskell, Yesod and AngularJS is very effective, and
there's enough Haskell work to balance out the horror that is
JavaScript.

Tom should be inviting some people to start playing with BayesHive
some time in the next week or so.  It will be good to get some other
eyes on what we're doing.  I'm going to spend some of tomorrow trying
to do some example analyses, but I'm too familiar with the foibles of
the web app to be a really good tester.

The only question now is whether, after three glasses of wine, I'll
still wake up at dawn (about 5:30 here right now) tomorrow morning, as
I've done for the last few days...

[bh]: http://www.bayeshive.com/
