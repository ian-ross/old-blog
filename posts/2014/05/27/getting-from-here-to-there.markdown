---
author: Ian
title: Getting From Here To There
tags: day-job, gis
published: 2014-05-27 13:59:54
---

In particular, getting from where you are now to where you want to be,
in terms of your career.

As a result of an email I sent to the Haskell-Cafe mailing list a
couple of weeks ago looking for someone to take over a contract I had
been working on, someone contacted me asking for career advice.
Clearly not someone who knew me at all, otherwise they would have
known what a *crazy* idea that was.  Anyway, this person was asking
about one of the fundamental problems when you're starting out in more
or less any profession: how do you acquire the experience you need to
apply for jobs that say "experience required", which is more or less
all of them?

They asked: "What is the path to getting involved in this stuff?  How
do I bridge the gap from just playing around with these technologies
to having real world experience?  It seems that most opportunities are
for people with experience."  And this is exactly right.  Particularly
for contracting, no-one wants to hire someone they think will have to
learn on the job.  You need to know what you're doing, which means
getting experience somehow.  And it would of course be nice to be able
to eat and have a life while getting that experience.

I wrote an epic email in reply, and was told that it would have worked
better as a blog post (or perhaps a short novel).  So here I am,
turning it into a blog post!

<!--MORE-->

### Learning on the job anyway

So what do you do?  This discussion was all in the context of GIS work
using Haskell, which is a bit of a niche market, but I think a lot of
what I said applies to the tech industry in a more general sense.

The person who emailed me had actually done quite a lot of different
things, and appeared to be someone with no lack of motivation and
ideas.  And really (barring some potential obstacles I'll talk about
below) that's all that's needed to "bridge the gap from just playing
around with these technologies to having real world experience".

My main suggestion was pretty simple and was based on the fact that I
learnt about GIS in a job where I was hired to do something not very
GIS-based, but it turned out that GIS skills were needed (so I sat
down and learnt ArcGIS "on the job") and similarly, before this recent
contract, I'd never used PostGIS.  I'd done quite a bit of database
work in the past, so I knew SQL, but I'd not done any spatial SQL.

Again, learning on the job (and a vast amount of hubris: I thought to
myself "Spatial SQL?  I bet it's just like normal SQL with a few extra
join conditions!  And you can only really use spatial indexes one way,
so that'll be simple!" and off I went, telling the client "Sure, I can
do that"...).  I know that this goes against the "experience required"
idea, but there's a balance to be struck: I usually work on the basis
that if I know a bit more than half of what I need to know in a new
job, I'll do OK, probably with some late nights and pressure to start
with, but eventually it'll work out.  That takes a little bit of
confidence and chutzpah, of course.

Another alternative, and one that I used when I was starting out, is
to take the best job you can find with your current level of
experience, and then gently twist that job to learn skills that you're
interested in.  That can be a real win -- you learn the things you
want to learn, you get paid for it, and you may even incidentally do
something useful for your employer.

### Learning off the job

What do you do if you don't have a job where that might work, and you
don't have enough experience or confidence to do hubris-based in-post
self-education?  In the absence of a *job* that would force you to
learn things this way, why not invent one?  There are a huge range of
environmental, public health, social and legal questions that rely on
spatial data analysis of one sort or another (and if you're not
interested in GIS work, just replace all this talk of spatial data
analysis with the thing you are interested in and figure out what
fields it's useful for).  Pick one that you care about, trawl the web
a little to see what data sets are available (you'd be surprised how
much stuff is out there if you look), pick a question you think it
would be interesting to answer, and do it, forcing yourself to use the
tools you want to learn.

I followed this up with a description of a problem I'd been working on
recently, the details of which aren't terribly important, but that
illustrated two things.  First: you can learn an awful lot from trying
to answer seemingly simple questions.  It was pretty amazing how many
different data analysis techniques and tools I ended up using to solve
what on the face of it initially seemed like quite a simple problem
(it was in fact far from simple, but it sounded that way to start
with).  It's hard to get to the end of a process like that without
learning something!  And you can choose the skills you develop by
choosing the tools that you use (learn GIS + Haskell by doing GIS +
Haskell).  Second: to show you that if you're persistent at pushing
through a problem, it's quite easy to do things that really very few
people have done.  This problem I'd been working on (basically
producing street-level housing occupancy estimates using OpenStreetMap
data along with large scale official population figures) is really
useful and, from talking to a few people about it, it seems like it's
something that hasn't really been done much before.  It's quite easy
to find new problems.

So, if you pick something simple that you care enough about to do it
properly, you do it, and you write up what you've done, that would
count more to me as "real experience" than someone doing this kind of
stuff in a paying job.  If I was looking to hire someone with GIS
experience to do something difficult, and I had one candidate who'd
worked in a job doing this stuff for a couple of years and another
candidate who said "I've never been paid for doing this, but look at
this report I wrote about public transport scheduling in Chicago based
on a neural network model for predicting traffic delays" (or whatever
you might do), I know who I'd hire.

### Obstacles

Earlier, I mentioned some "potential obstacles".  What do you do if
you don't have a job you can twist to your own purposes, or the time
or resources to learn new skills in "your own time"?  (You might have
caring responsibilities, you might have to hold down a second job to
pay the bills.  There are lots of things that can get in the way.)
This is a structural problem in the tech industry.  There are no (or
very few) apprenticeship-level positions where people can learn the
skills they need and earn enough to live on at the same time.  That's
not a good position for anyone and I honestly don't have any good
ideas about how to get around it.  I guess it helps if you can find a
mentor to help you get the most out of what time you do have, but you
do still need *some* time to do this stuff.

Apart from badgering companies to offer (non-exploitative!)
apprenticeships or other entry-level positions with training
possibilities, it's hard to see what to suggest.  I've been really
lucky in my career to date, but I've still spent a lot of time working
on things that were ultimately not really what I wanted to be doing.
Getting to that higher level of job satisfaction, beyond paying the
rent and not feeling like your just slaving away for someone else's
"thing", takes a lot of time, and even more luck.
