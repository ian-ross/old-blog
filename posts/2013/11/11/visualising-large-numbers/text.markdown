---
author: Ian
tags: mathematics
title: Visualising Large Numbers
published: 2013-11-11 19:32:42
---

I was out walking Winnie the other morning and got to thinking about
the storage capacity of neural systems (I'm reading Nils Nilsson's
*The Quest for Artificial Intelligence* at the moment, which was what
triggered this line of thought).  I'll write about that in another
article, but I realised as I was figuring through this that I would
end up talking about some rather large numbers.  Everyone throws
around big numbers (millions, billions, trillions, and up and up), but
how often do you stop to visualise what these things mean?

There are some nice animations around that help with thinking about
relative scales (for instance,
[this one](http://www.htwins.net/scale2/) is really good).  What these
things don't do is to give you a sense of *numbers*.  What does a
billion of something look like?  Can you get a really strong physical
feeling for numbers of this magnitude and larger?

Here, I'll show you what I do.  It's nothing ground-breaking, and it's
kind of obvious once you get started, but it is effective.  It's also
quite a nice mental exercise.

<!--MORE-->

## Easy numbers

We're aiming for *large numbers*, so let's start ambitious.  One.  How
do you visualise "one of something"?  There are lots of ways, but
let's start with a little square:

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\begin{scope}[very thick]
  \draw (0.5,0.5) +(-.25,-.25) rectangle ++(.25,.25);
\end{scope}
@@@@@

<br>
Being (mostly) bipedal (mostly) pentadactyl lifeforms, we'll follow
tradition and work in powers of ten.  Ten of something is a small
enough number to capture in a single glance:

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\begin{scope}[very thick]
  \draw[step=5mm] (0,0) grid (5,0.5);
\end{scope}
@@@@@

<br>
But then we need to start getting a little creative.  One hundred is
pretty easy -- it's 10<sup>2</sup>, so let's just make a square with
ten things along a side:

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\begin{scope}[very thick]
  \draw[step=5mm] (0,0) grid (5,5);
\end{scope}
@@@@@

<br>
Nice and easy.  We can think in three dimensions as well as two, so
one thousand (10<sup>3</sup>) is just as easy:

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\begin{scope}[very thick,step=5mm]
  \draw[yslant=-0.5] (0,0) grid (5,5);
  \draw[yslant=0.5] (5,-5) grid (10,0);
  \draw[yslant=0.5,xslant=-1] (5,0) grid (10,5);
\end{scope}
@@@@@

<br>
Now we're kind of stuck -- we can't really think in four spatial
dimensions, so we need to do something else.

In fact, it's pretty easy to visualise 100 and 1000 things directly.
Remember those metre sticks you used in school science classes?  One
metre of wood or plastic with centimetre and millimetre markings all
the way along?

<div class="img-center-small">
<a href="metre-stick-cm.jpg">![Metre stick](metre-stick-cm.jpg)</a>
</div>
<br>

Those 100 centimetres, they're easy to visualise as a unit.  Most
people have handled a metre stick enough[^1] to have a physical
intuition about its size, and those centimetre marks can be pinned to
something really direct -- for me, 1 centimetre is about the width of
the fingernail on my index finger[^2].

Once we've got our heads round 100 things in a row, we can try 1000.
Focus on the millimetre marks on your mental metre stick.  Visualising
all 1000 of them in one go isn't really practical, but that's not what
we want to do -- we want to hold in our minds the *idea* of 1000
things in a row.  What do I mean by that?  Well, we have a strong
physical intuition of what a metre is.  From there, we can get a good
idea of the *relative sizes* of a metre and a millimetre, just on the
basis of their relationship to our own physical size: a metre is, for
most people, not terribly far from the distance from fingertips to
shoulder with a straight arm.  And for a millimetre, you can think of
ten laid end to end across your fingernail.

<div class="img-center-small">
<a href="metre-stick-mm.jpg">![Metre stick](metre-stick-mm.jpg)</a>
</div>

At this point, we have two ways of thinking about hundreds (as 10
&times; 10, or as 100 in a row), and two ways of thinking about
thousands (as 10 &times; 10 &times; 10 or as 1000 things in a row).
We can use these representations to work our way up in size.

## Big but imaginable

So, what's next?  Let's try 10,000.  Here's a picture of 10,000 things
(admittedly not very practical, and in some ways quite eye-watering,
depending on your computer monitor).  It's just a 100 &times; 100
grid.  Think of taking four metre sticks and laying them out to form
the boundary of a square.  The centimetre markings on each square
delineate the edges of a 100 &times; 100 grid, just like this one.

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\draw[step=1mm] (0,0) grid (10,10);
\filldraw (0,0) rectangle ++(7,0.1);
@@@@@

<br>
It would take a long time to count all the squares in this grid one by
one, but we can still take in the magnitude of the number of squares
more or less "at a glance".

And here's an interesting thing.  If each square in this grid
represents one year, a typical human lifespan is about two-thirds of a
row (70 years is filled in black in the diagram above).  Human
civilisation has been going for about 10,000 years (depending on who
you ask and what you mean by "civilisation").  You can get some
feeling for the place of an individual human, birth to death, in the
whole history of civilisation this way.

Moving on up, it's going to get harder to draw pictures, so you're
going to have to rely on your imagination more.  Let's think about
10<sup>6</sup> next, one million.  We can go to three dimensions,
framing a cube with metre sticks marked in centimetres, since 100
&times; 100 &times; 100 = 1,000,000.  Or we can measure out a square
metre with metre sticks marked with millimetres, since 1000 &times;
1000 = 1,000,000.  Either way, we get a nice physical picture of what
a million things looks like.

Look at the picture of the 100 &times; 100 square above, and think
about 100 copies of the square stacked up to make a cube.  If each
cube is a single year, we have one million years stacked up in front
of us.  A million years isn't such a long time geologically speaking
-- the dinosaurs became extinct about 65 million years ago, and the
earth has been around for about 4.5 *billion* years, 4,500 times one
million years.  And yet, an individual human life rarely makes it to
the end of the first of one hundred rows in the first of one hundred
layers of our million year cube[^3].

Going ahead, I tend to prefer the "cubic metre" standard over the
"square metre".  A cubic metre of water weighs a ton, 1000 kilograms,
which is 1000 litres (or about 2000 pints, if you're American).  As I
stand at my desk, I can look to one side and visualise a cubic metre
of water sitting there confined by invisible walls, and I can use that
cubic metre as a framework for imagining numbers.

So, if we're going to go on up to a billion, 10<sup>9</sup>, let's
think of a cubic metre of water, framed by metre sticks graduated in
millimetres: 1000 &times; 1000 &times; 1000 = 10<sup>9</sup>.  If you
can hold this in your head, you can see what a billion of something
looks like.  You can look at the whole of the cubic metre of water,
then you can zoom in and look at the millimetre graduations on the
metre sticks framing the cube and you can see what a single cubic
millimetre of water looks like.  Zoom in, zoom out, zoom in, zoom out.
You can think of the relative sizes of your cubic metre and the cubic
millimetres from which it's composed: a cubic millimetre of water is a
tiny amount (one microlitre) -- a very small raindrop that you might
just be able to feel on your skin.  A cubic metre of water, on the
other hand, is a quite considerable drenching.

## Duplication takes us further

Mental zooming in and out is the key to getting a handle on bigger
numbers.  It's hard to hold more than one level of zooming in your
head at a time (at least for me!), so before we take the next step,
you should do a bit of zooming around your billion cube.

All zoomed out?  OK, let's continue.

Suppose we imagine zooming in to one of the little cubes of our 1000
&times; 1000 &times; 1000 cube.  Then zoom in some more, and imagine
that each of those little cubes is a copy of the big cube, i.e. that
each of the billion millimetre cubes is itself a 1000 &times; 1000
&times; 1000 cube (with each side being 1&mu;m, 10<sup>-6</sup>m).

How many of the smallest cubes are there?  Well, there are
10<sup>9</sup> millimetre cubes in our original metre cube, and we're
now saying that each of those millimetre cubes is itself composed of
10<sup>9</sup> sub-cubes.  That means that there are 10<sup>9</sup>
&times; 10<sup>9</sup> = 10<sup>18</sup> of the tiny cubes.

Now, 10<sup>18</sup> really is quite a large number, so let's stop to
think about it carefully.  Although we're really dealing with three
levels of sizes here (our one metre cube, the billion millimetre cubes
and the billion billion micrometre cubes), the self-similarity between
the embedding of the millimetre cubes in the metre cube and the
embedding of the micrometre cubes in each millimetre cube allows us to
use the same mental template to represent both scales of zooming.  I
can form a clear image in my mind of what the one billion cube looks
like (one cubic metre broken up as cubic millimetres), and I can then
hold that image in my mind alongside a zoomed in version of one of
those millimetre cubes.

It takes a little practice, but if you sit staring into the middle
distance visualising your cubes of cubes of cubes long enough, you can
start to get an inkling of what 10<sup>18</sup> of something looks
like.

Buoyed by our success, we might think about replacing each of our
10<sup>18</sup> micrometre cubes with copies of our billion cube.
That would give us 10<sup>18</sup> &times; 10<sup>9</sup> =
10<sup>27</sup> *really* small cubes (each 10<sup>-9</sup>m = 1
nanometre) on a side.  Now that's a *BIG* number!  However, I have to
confess that this doesn't work for me.  The "duplication scaling"
trick works for only one level for me -- I can't keep a mental grasp
on the original one metre cube, the billion millimetre cubes, one of
the 10<sup>18</sup> micrometre cubes and its constituent nanometre
cubes all at once.

## Avogadro's number

Let's try something else.  Let's try to visualise
[Avogadro's number](https://simple.wikipedia.org/wiki/Avogadro%27s_number).
This is about 6 &times; 10<sup>23</sup>, but let's approximate it to
an order of magnitude as 10<sup>24</sup>.  Why think about Avogadro's
number?  It's really determined by an arbitrary choice of units: it's
the number of particles in one mole of a given substance, so it's not
really anything more than a way of defining what a mole is.  So why
bother thinking about it?  Well, it's a number that makes a very clear
connection between the macroscopic world of "everyday things" and the
microscopic world of atoms and molecules.  For instance, one mole of
water is 18 grammes, which would more or less fill a medium sized test
tube.  That means that a test tube of water contains about 6 &times;
10<sup>23</sup> (or for our purposes, 10<sup>24</sup>) molecules of
water.  So if we could build a mental picture of how big Avogadro's
number is, we might be able to develop some intuition on how big a
water molecule is.

Now, 10<sup>24</sup> = 10<sup>18</sup> &times; 10<sup>6</sup>, so
we're somehow going to need to visualise one million of our nested
10<sup>18</sup> cubes.  As I said before, I have trouble doing more
than two levels of zooming in -- the nested billions to give the
10<sup>18</sup> cube is about all my poor little brain can handle.
However, I find that something interesting happens if I try zooming
*out* instead of in.  I can keep a level of "out" along with my two
levels of "in".

Let's think about making a million copies of our cubic metre of water
and stacking them into a single big cube.  That means a cube 100
metres along each side.  To get some perspective on what that means,
let's take a typical outdoor area of around 100 metres in dimension.
An athletics stadium works really well -- the 100 metre straight that
sprinters run along is pretty clear, and we can get some idea of what
our big cube of water looks like in a setting with people and some
other objects for scale:

<div class="img-center3">
<a href="water-stadium.jpg">![Stadium](water-stadium.jpg)</a>
</div>
<br>

To get 10<sup>24</sup> things, we need to imagine that each cubic
metre of water in this big 100 metre cube is one of our
10<sup>18</sup> cubes.  To see one of the 10<sup>24</sup> individual
"things" (which are tiny tiny cubes of water one micrometre on a
side), we zoom into one of the one metre cubes that make up our big
100 metre cube, then zoom in by a billion (thinking about the
millimetre gradations along the edge of the metre cube to guide us),
then zoom in by a billion again (we can mentally "magnify" the
millimetre cube we pick out by our first "billion" zoom and think
about it being graduated by thousands along each edge as well).

So why does this way of thinking about things work, but trying to zoom
in three levels from a metre cube leaves me sad and confused?  I was
thinking about this earlier today, and I think that the reason for
this is that the "middle" level, the one metre cube, is a human order
of magnitude.  In some way, it's our "reference" scale, the one
compared to which we measure everything.  People often talk about "the
width of a human hair" or some such visual stand-in for a small length
scale, but that doesn't really grab your visual and proprioceptive
sense in the way that "as long as your arm" does.  This engagement of
our "internal" proprioceptive sense is what makes us "feel" the size
of things.  The span of your hand, the width of a finger, the length
of your arm, these are all measures for which you have both a visual
model and an "internal" tactile/proprioceptive model.  That means that
it's easy to mentally zoom in or out from these scales.

Anyway, let's spend a bit of time zooming in and out of our
stadium-sized cube of water.  Once you get the hang of that, think
about shrinking that cue of water down so that you could pour it into
a test tube.  At that scale, the micrometre cubes we've been thinking
of as our bottom level are the size of water molecules!

## Conclusions

Avogadro's number seems like a good place to stop.  Visualising
Avogadro's number is actually quite useful, because of its role in
linking everyday scales to atomic scales.  Bigger numbers do turn
up[^4] but it's not clear how useful it is to try to visualise these
numbers -- billions, trillions and Avogadro's number have a lot of
practical applications for understanding, but these bigger numbers
really are getting on for "unimaginably large".  Also "uselessly
large" in some cases, except for some pure mathematical issues.

Here's an example to show how unwieldy things get.  A googol is
10<sup>100</sup>.  It doesn't really have any practical application.
It's just a big number.  If we start with our 10<sup>18</sup> cube,
and progressively zoom in, replacing every one of the 10<sup>18</sup>
small cubes with a copy of the whole 10<sup>18</sup> cube, we can make
the following numbers:

10<sup>18</sup> &nbsp;&nbsp;&rarr;&nbsp;&nbsp;
10<sup>36</sup> &nbsp;&nbsp;&rarr;&nbsp;&nbsp;
10<sup>54</sup> &nbsp;&nbsp;&rarr;&nbsp;&nbsp;
10<sup>72</sup> &nbsp;&nbsp;&rarr;&nbsp;&nbsp;
10<sup>90</sup>

Four levels of zooming in and we're still not there!  It seems as
though some other means than our duplication approach would be needed
to build intuition about numbers of this magnitude.

Despite that, for "everyday" large numbers -- millions, billions,
trillions, even up to 10<sup>24</sup> -- the methods we've looked at
really can help to give a direct feeling for these numbers.  That
intuition can then be applied to thinking about relative sizes
(molecules *versus* test tubes, for instance) in a way that's not
accessible via by step-wise scaling.

[^1]: Even if only to pretend that it's a light sabre and make
      swooshing noises around the classroom while the teacher wasn't
      looking...

[^2]: You can joke all you like about using
      [cubits](https://en.wikipedia.org/wiki/Cubit) as a measure
      leading to all sorts of problems with incommensurability between
      the Cubit of Rhodes and the Cubit of Alexandria, but I challenge
      you to find a better and more intuitive unit of measure for
      personal use than something directly associated with your own
      body.

[^3]: No, I'm not trying to be depressing!  One of the things that
      thinking about some AI topics brought home to me were some of
      the problems of scale involved in trying to simulate real neural
      systems.  To understand these scale issues, you need to get a
      feeling for how big these big numbers are.  If a little
      existential angst helps with that, it's all to the good.

[^4]: A quick back of the envelope calculation revealed that I might
      need to talk about numbers as big as 10<sup>42</sup> if I want
      to talk about storage capacities.  This number comes from an
      absolute limit for information storage capacity based on the
      [Bekenstein bound](https://en.wikipedia.org/wiki/Bekenstein_bound),
      which is way larger than the information storage capacity of any
      realistic neural system -- it's just an interesting theoretical
      upper limit for these things.
