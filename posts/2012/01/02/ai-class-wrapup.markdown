---
author: Ian
tags: AI
timestamp: 19:09:22
title: AI Class Wrap-up
---
One thing that came to an end over the Christmas holidays was the
Stanford Artificial Intelligence course.  The final exam was on the
weekend of 17/18 December and final grades were distributed a couple
of days later.

Overall, it was an interesting experience, and I certainly learnt some
things.  The instructors (Peter Norvig and Sebastian Thrun) were
enthusiastic and clearly extremely knowledgeable, and they took on an
extremely challenging task in trying to run an online class for tens
of thousands of students.  For that, I'm very grateful.  There are a
few things that weren't 100% ideal about the presentation of the class
material, but that's to be expected the first time out with something
like this.

The highlight of the course was definitely Sebastian's presentation of
his work on self-driving cars.  I'd heard a bit about the DARPA Grand
Challenge before, but I hadn't known about the progress that the
Stanford team had made with self-driving cars in traffic.  That's
really impressive work, and Sebastian did a nice job of relating the
algorithms used for guiding cars to the simplified versions considered
in the class.

<!--MORE-->

This word, *algorithms*, is kind of key to what I want to talk about
here.  The content of the course was, to my mind, very focused on
algorithms for practical applications.  For instance, machine
learning, planning, and perception under uncertainty were all covered
in a lot of detail (particle filters are a good example).  What wasn't
really covered at all was what I tend to think of when I think of
"Artificial Intelligence", which is general reasoning and problem
solving.

All of the examples considered in the course were quite constrained,
and very far from the early dreams of "intelligent machines".  This is
clearly just a reflection of where the field of AI is today: there is
much less emphasis on the (very hard) problems of general
intelligence, and much more focus on solving practical problems.
"Putting AI to work" is the name of the game.  I understand why things
are this way.  It's much easier to get a research grant for something
with an obvious industrial application, rather than a vague programme
of research that might have applications in 50 years or 100 years or
just... sometime.  It's a little disappointing all the same.

The disappointment stems not so much from the fact that we're thinking
about practical problems (that's a good thing!), but a feeling that
most of the methods being used (probability theory, most of the
machine learning algorithms) aren't really about intelligence as such,
but are just mathematical or statistical methods that are useful for
some applications.  That word "just" is doing a lot of work there:
some of these methods are highly nontrivial and very interesting in
themselves.

But where are our intelligent machines?  You could definitely make a
case that an internet search engine capable of finding relevant pages
for a vaguely expressed search term is intelligent in some sense, and
the same goes for a self-driving car.  But you can't engage them in
conversation about what they do.  They have no power of
introspection.  They don't know that they're doing what they're
doing.  This is all a long way from the idea of "human level AI".

Some people have expressed this sense of disappointment more strongly
than I would.  Here's John McCarthy, a man who knew a thing or two
about artificial intelligence, writing in 2007[^1]:

> The computer science world is still suffering from a 1990s fit of
> pseudo-practicality that is inimical to the solution of difficult
> scientific problems. Lip service is given to basic research, and a
> lot of basic research is done, but the initiation of ambitious
> research by young people is hampered by the now prevalent doctrine
> that "basic research" should be done in connection with applied
> problems that have been identified by the competent committees. I
> think that Newell and Minsky and I would have had a much harder time
> initiating AI research if the atmosphere of the 1950s had been like
> that of the 1990s.

So, we should still hold out for our intelligent machines!  McCarthy's
paper lays out some of the requirements to get there from here.  It's
a daunting list, and introspection is one of the features he
highlights.

[^1]: John McCarthy (2007).  From here to human-level AI.  *Artificial
Intelligence* **171**, 1174-1182.
