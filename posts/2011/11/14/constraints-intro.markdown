---
author: Ian
tags: constraints,haskell,programming
published: 2011-11-14 20:04:49
title: "A project: Constraints"
---
I've recently started making some tiny contributions to Brent Yorgey's
Haskell [diagrams](http://projects.haskell.org/diagrams/) library,
mostly bug fixes.  The bug fixes are primarily just a way of
familiarising myself with the diagrams codebase though.  I have my eye
on a rather chunkier task in the open issues list.  That's writing a
constraints solver for diagram layout.  I've been interested in this
sort of problem for some time, and have never had a good reason to get
down to it, so I'm going to see what I can come up with.

And I'm going to try something I've not done before, which is to
document experiments, design and development as blog articles.  It may
all turn out to be hideously embarrassing, it may end up that none of
what I do actually makes it into the mainstream of the diagrams
library, but having to write about what I'm doing will keep me honest,
force me to be clear about what I'm doing, and will provide an
incentive to work on this stuff.

<!--MORE-->

# Constraints 101 #

OK, so what do we mean here when we talk about "constraints"?  The
word "constraint" is used in lots of ways: we have type class
constraints in Haskell, people talk about constraint logic
programming, in mechanics constraints are extra side-conditions on
Newton's equations of motions imposed by the structure of the problem
we're solving[^1] (think bead-on-wire problems), and so on.  Here,
we're talking about something closer to the last case.  Given a bunch
of geometrical objects (points, lines, polygons, circles, arcs, Bézier
curves, whatever), we want to be able to specify *geometrical*
relationships between our objects, for instance that the end-point of
one line segment is coincident with the corner of a rectangle, or that
a certain circle is tangent to a certain line segment.

Why do this?  My first exposure to constraint-based drawing was
through AutoDesk's Mechanical Desktop, which was then part of the
AutoCAD product suite[^2].  This had a sketching tool that was just
magical.  Draw a very rough sketch of approximately what a component
looked like, then say, "OK, this line should be horizontal, this one
should be perpendicular to this one.  This curve is a fillet and
should be tangential to this line and this line.  These two circles
are concentric, these two lines are parallel.  This point lies on this
line, not quite sure where yet."  What you end up with is a drawing
that has a smaller number of free parameters (usually the major
dimensions of whatever part you're designing).  Specify those
dimensions and the rest of your drawing jumps around to make
everything right with the constraints you defined[^3].  I was using
Mechanical Desktop for work, but I got a real kick out of just playing
with the constraint-based drawing tool.  It really seemed like magic.
Set up your drawing with some constraints, drag some points around and
watch as AutoCAD updates the rest of the drawing to maintain the
constraints you specified.  Cute and powerful.

The same power can be applied to layout in the diagrams library.  You
might not want to go the whole hog with the complex geometric
constraints available in CAD packages (although why not?), but you can
certainly say things like: "These three items lie on a horizontal
line; the gaps between them should be equal; I don't know how wide the
items are yet."  This is a very common situation in GUI layout and
lots of GUI toolkits provide the capability to specify layout in this
fashion.  Since the diagrams library is intended to produce images
more complex than typical GUI layout, it seems to me that something
closer to the AutoCAD experience is what we want.  (It will also be
more fun!)

[^1]: For examples, see Section 1.3 of Goldstein's *Classical
      Mechanics*.

[^2]: Now obsolete.  Do I feel old?  No way!

[^3]: In Mechanical Desktop, the dimensions were often pulled from a
      spreadsheet or a database so that you could, for example, draw
      one metric standard bolt, parameterised by the relevant
      dimensions, and have an M8 bolt produced from the same base
      drawing as an M10 bolt.  More recent Autodesk and comparable
      products go a lot further on this front.  The other day, I was
      reading some publicity for Autodesk Inventor, which claims to
      have a library of some 700,000 standard components to choose
      from, many of which are presumably parameterised in just this
      way.  (I sure hope so.  If not, some poor schmuck had to draw
      them all by hand...)

# Goals #

Given all that spiel, how do I get from nothing to a usable
constraints module for the diagrams library?

1. The first thing to do is probably to implement some existing
   constraint solver algorithms in Haskell and do some experiments.
   [Cassowary][cassowary] seems like a good starting point.  Other
   interesting possibilities are the system used in [Juno-2][juno2]
   and [Mike Gleicher's][gleicher] snap-together mathematics.  This
   shouldn't be too hard, and would result in a constraint solver
   working at the level of individual equations and inequalities for
   sets of real variables.

2. The diagrams library already has primitives for drawing various
   geometric objects, but to start with, I'd like to abstract away
   from that.  Two reasons for this.  The first is to reduce the
   dependence on the details of the representations used in the base
   diagrams library in order to get an idea of exactly what is
   required in the constraints system.  The second reason is that
   there are multiple ways of looking at geometrical objects.  A point
   can be identified by its Cartesian coordinates or by its polar
   coordinates.  A line segment can be identified by two points or by
   a single point, a direction and a length.  A rectangle can be
   identified by giving two corner points and a rotation angle, or by
   a single corner point, a width and height and a rotation angle.
   Each of these "views" of these objects is equivalent, and it should
   be possible to express constraints in any convenient view.  For
   instance, I might want to constrain the centre of a circle $C$ to
   lie on the circle $r=1$.  I should be able to give a constraint
   $r(\mathrm{centre}(C)) = 1$ to achieve this.  I have an idea of
   some sort of "constrainable lens" floating around in my head but
   it's going to require some experimentation to pin down just how
   this should work, and keeping this independent from diagrams to
   start with should help to reduce my confusion to manageable levels.
   (There are some relevant thoughts about this idea of different
   views of geometrical objects in Mike Gleicher's PhD thesis.)

3. The most important part of a constraint system from the user's
   point of view is the interface for defining and managing
   constraints.  The constraint that two lines are perpendicular, or
   that a circle is tangent to a particular line segment, can be
   expressed as sets of equations and/or inequalities that can be
   handled by a constraint solver, and we'd like a way to work at this
   higher level: in the most trivial case, I want to be able to say
   "Point $P_1$ is coincident with point $P_2$", not "$x(P_1) = x(P_2)
   \wedge y(P_1) = y(P_2)$".  More complex geometrical constraints can
   be broken down and expressed as equations in similar ways.
   Providing an easy-to-use interface between this geometrical view of
   the world and the equational view required by the constraints
   solver will be key to making the constraint system useable.  Part
   of that will also involve finding a good way to deal with
   underconstrained and overconstrained systems.

4. The last job to tackle will be taking whatever I come up with and
   trying to massage it into a state where it's usable in the diagrams
   library.  I'm going to keep this final goal in mind, but I'm not
   going to let it constrain what I do too much[^3].  Chances are, someone
   else will develop something usable for diagrams before I'm too far
   out of the starting gate on this, and it's mostly for my own
   amusement anyway...

# First things first #

OK, so I'll start with reading the Cassowary papers and will implement
the relevant algorithms in Haskell to act as a baseline for
experimentation.  Off we go!


[cassowary]: http://www.cs.washington.edu/research/constraints/cassowary/
[juno2]: http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-131A.html
[gleicher]: http://pages.cs.wisc.edu/~gleicher/

[^4]: Ha ha.  I made a joke.
