---
author: Ian
tags: computer-science,moocs
title: Automata Theory MOOC
published: 2013-12-21 15:22:09
---

I've just finished the final exam for
[Stanford's Automata Theory online course][mooc], run by Jeff Ullman.
I originally chose to follow this course because 1). I wanted to learn
(and in some cases re-learn) about finite automata and context-free
grammars, and 2). it's Jeff Ullman (duh).  The finite automata and
context-free grammar stuff was fun and useful and pretty easy, but the
most interesting part of the course ended up being the material about
decidability, computability and tractability (**P** vs. **NP** and all
that jazz).  I was re-reading Hoftstadter's *Gödel, Escher, Bach* at
the same time as doing the course, and so questions of decidability
and computability were bouncing around in my mind.

Hoftstadter makes a big effort to present some really deep ideas in an
accessible way without wading through the details of proofs.  Ullman's
course takes a complementary approach, with lots of semi-formal
proofs: "semi-formal" in the sense that they're what I call "story"
proofs with quite a bit of narrative argument &mdash; there's nothing
wrong with that, and it's a fairly digestible approach for video
lectures.  You do need to pay attention and skip back and forth in the
videos a little sometimes, but only a couple of the proofs are
complicated enough to cause trouble.

The course was very well-organised, with useful homeworks and a couple
of interesting programming exercises.  Ullman and his TAs participated
in the discussion forums quite a bit and there were some extra
"challenge problems" to add to the fun.  I have some more general
comments I want to make about MOOCs in another post, but this was
definitely a good example of what you can do in a course like this.

Like I said, the most interesting material was the stuff on
computability and complexity.  It's not something I know a whole lot
about, and I found myself adrift in a twisty maze of complexity
classes when I tried to do some background reading.  I've since
discovered some useful resources.  Emphatically *not* included among
those is the "standard" textbook, which is more than US$100 from the
vultures at Addison-Wesley[^1].  The best alternative I've found seems
to be [Scott Aaronson's stuff][aaronson].  He's been teaching a course
at MIT for a few years on
[Automata, Computability, and Complexity][mit6045], and all the course
materials are available through MIT's website.  Beyond that, there's
the [Complexity Zoo][zoo], also an Aaronson project, which has an
overwhelmingly comprehensive bibliography and treatment of more or
less every complexity class known to humanity.

I don't know how much of this material is useful for day-to-day
"practical" programming work, but it's certainly very entertaining.

[mooc]: https://class.coursera.org/automata-002/class/index
[aaronson]: http://www.scottaaronson.com/
[mit6045]: http://stellar.mit.edu/S/course/6/sp13/6.045/
[zoo]: https://complexityzoo.uwaterloo.ca/Complexity_Zoo

[^1]: I just bought Michael Spivak's *Physics for Mathematicians*,
      which is 700-and-some pages of really good stuff for about $45
      in hardback.  I never believed that there was any real
      justification for the inflated prices of US college textbooks.
      When I was teaching in Canada, I always felt really bad telling
      students that they had to buy some overpriced and crummy
      textbook that the department had mandated because of pressure
      from a publisher's salesman.