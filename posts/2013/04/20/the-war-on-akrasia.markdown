---
author: Ian
tags: organisation
title: The War on Akrasia
published: 2013-04-20 16:26:33
---
I learnt a new word yesterday: *akrasia* (rhymes with "aphasia" and
"Malaysia").  It means not doing the things you want to do and know
that you want to do; acting against your better judgement.  The
classic example of akratic behaviour is weight loss (or weight
control).  That extra slice of pie always looks like such a good idea
at the time and there's no short-term consequence.  It's only later
that you look back with a queasy feeling of regret at the empty pie
dish and think of how your rolls of kidney flab are just not
fashionable any more (and probably haven't been since the 1830s).

Everyone is akratic to some extent.  I know that I am.  Some days I
just can't face the work I have to do (even though it's usually fun
once I get started) and just doodle around on the internet.  I've
tried various methods for organising myself (I use
[org-mode][org-mode] in Emacs for pretty much everything: TODO lists,
scheduling, time tracking for invoicing), I've tried
[structured procrastination][sproc]. Sometimes these things work and
sometimes they don't.  The main problem is that, while these methods
can help you to organise yourself, they don't impose any short-term
consequences on you to make you do things.

A good example for me is blogging.  There are no short-term
consequences at all for me if I fail (for weeks and weeks) to write
any blog articles.  In the long run though, there probably are
consequences.  One of my current contracts came about because Tom
Nielsen at [OpenBrain][bh] had been reading some stuff on my blog and
as a direct result contacted me to ask if I'd be interested in some
Haskell contracting work.  No blogging &rArr; no contract.

So how do you get some control on this?  Enter [Beeminder][bm].  I
first read about this on Brent Yorgey's blog a while ago, but Brent
just posted a [follow-up][brent-article] to his experiences with
Beeminder.  He had such positive outcomes from using the thing that I
decided I ought to give it a try too.

The basic idea of Beeminder is pretty simple.  You set yourself a goal
which has some numerical measurement associated with it (it might be
body weight, it might be number of blog articles written per week, it
might be hours per week spent on a particular project, whatever you
like).  Beeminder then shows you a "yellow brick road" which you need
to follow -- if you fall off your target a little bit, you have a day
or so to recover, if you get ahead of your target you can relax a
little.  If you fall off the wagon again though, there are
*consequences*.  The consequences, as well as the natural
embarrassment that anyone would feel, are monetary.  There is an
exponential scale of payments you have to make each time you stray
from the yellow brick road -- small payments at first, but they get
bigger quickly.  The idea is that you need to reach a level of payment
that *really* motivates you to stick to your goals.  And because there
is continuous monitoring (if you let it, Beeminder will remind you
every day how much of a safety buffer you have before you end up in
the hole) *and* short-term consequences, there really is a strong
chance that you will stick to the plan.

There is quite a bit more clever stuff involved (there are nice
graphics, there's flexibility about changing your goals as you go
along, but done in a very smart way that prevents you from just
weaseling out of the goal you've set yourself, and there's some good
stuff for data smoothing for weight measurements), but that's the
essential idea.

For me, it's already working!  I've set up goals for
[German study][german], [blogging][blogging], a work project
(Beeminder has GitHub integration, which means you can set yourself a
target of a certain number of commits per week to a certain
repository) and a bunch of other things.  And as soon as I've
published this blog article, I'll go and update my stats...

[org-mode]: http://orgmode.org/
[sproc]: http://www.structuredprocrastination.com/
[bh]: http://www.bayeshive.com/
[bm]: http://www.beeminder.com/
[brent-article]: http://byorgey.wordpress.com/2013/04/16/beeminding-for-fun-and-profit/
[german]: https://www.beeminder.com/iross/goals/german
[blogging]: https://www.beeminder.com/iross/goals/blogging