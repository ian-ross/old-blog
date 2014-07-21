---
author: Ian
tags: book-reviews
title: Functional Differential Geometry
published: 2014-02-17 19:41:56
---
*by Gerald Jay Sussman & Jack Wisdom (with Will Farr)*

This book follows very much in the mould of Sussman & Wisdom's
[Structure and Interpretation of Classical Mechanics][sicm] in that
it's an attempt to take a body of mathematics (differential geometry
here, classical mechanics in SICM) and to "computationalise" it,
i.e. to use computer programs to make the mathematical structures
involved manifest in a way that a more traditional approach to these
subjects doesn't.

This computational viewpoint is a very powerful one to adopt, for a
number of reasons.

<!--MORE-->

First, by endeavouring to compute things, we're forced to be
*extremely* rigorous about the meaning of our notation.  When things
become notationally inconvenient, it's common to say "by abuse of
notation..." but that doesn't work if you have to translate your
notation to computer code.

Second, by endeavouring to compute things, well, we can compute
things!  If you've ever taken a differential geometry course, you'll
probably remember just how tedious some of the calculations can be.
Once you start looking at curvature and geodesics, you have
geometrical objects with four indices floating around and everything
becomes rather annoying.  If you do it right (which mostly means
functional programming), taking a computational approach allows you to
exploit the compositional property of (functional) programs and to
build up from simple examples to complex cases without ever losing
track of what's going on[^1].

## Notation

The example that Sussman & Wisdom use on the very first page of their
book to motivate notational rigour is the Euler-Lagrange equations,
traditionally written

$$\frac{d}{dt} \frac{\partial L}{\partial \dot{q}} - \frac{\partial
L}{\partial q} = 0.$$

Here, $L(t, q, \dot{q})$ is the Lagrangian, written as a function of
time $t$, generalised coordinates $q$ and generalised velocities
$\dot{q}$.  In the first term on the left hand side, the partial
derivative is with respect to $\dot{q}$, treating the $\dot{q}$ as
independent variables.  But then, what does the outer time derivative
mean?  Suppose that we have a solution of the Euler-Lagrange
equations, represented by a path $q = w(t)$, a function of time.  Then
$\dot{q} = dw/dt$, and what we really mean by the Euler-Lagrange
equations is something like

$$\frac{d}{dt} \left( \left. \frac{\partial L(t, q, \dot{q})}{\partial
\dot{q}} \right|_{q=w(t), \dot{q}=\frac{dw(t)}{dt}} \right) -
\left. \frac{\partial L(t, q, \dot{q})}{\partial q} \right|_{q=w(t),
\dot{q}=\frac{dw(t)}{dt}} = 0.$$

This is what we *really* mean by the traditional expression.  It's a
bit of a mouthful, but it's completely explicit and there are no
notational gaps that we need to fill in with our imagination.

And this is the level of notational rigour that you need if you're
going to compute with these things.  Achieving this level of rigour
takes some effort, and much of the book is devoted to ensuring that
this is done correctly.

This is one place where the book shines, giving a very good impression
of what it feels like to think this way, and follow the steps that are
needed to turn a slightly unclear expression of a mathematical idea
into something completely explicit with which you can compute.
Although the focus here isn't really on numerical analysis, there's
much the same feeling you have when you're developing code for a PDE
solver, for instance -- you start with a more-or-less clear expression
of what you want, but then you need to pin down *every single little
detail* to get to something you can write code for.

Sussman & Wisdom use a notation for calculus that attempts to be
clearer and "more functional" than the traditional notation.  It has
some things in common with the sort of coordinate-free notation for
differential geometry used, for example, in Misner, Thorne & Wheeler's
*Gravitation* (a big fat book about general relativity) and although
it takes a little getting used to, it works well and makes the gap
between mathematical expressions and the code to represent them a
little narrower.

## Coverage, length & explicitness

The coverage of topics is more or less what you would expect from the
title: most of what would appear in a first undergraduate course in
differential geometry, plus some applications -- manifolds, vector
fields, a bit of exterior calculus, directional derivatives, curvature
and geodesics.  The presentation of the material (aside from the
computational focus) is relatively standard, although there is a nice
chapter called "Over a Map" about defining vector fields on manifolds
that takes an approach I've not seen before.

My one disappointment with the book is its length.  I was hoping for a
weighty tome like *Structure and Interpretation of Computer Programs*,
another Sussman production, but instead *Functional Differential
Geometry* is only a shade over 200 pages.  Frankly, this isn't enough
space to do the subject justice, particularly given the unusual
approach that the book takes.

The result of this abbreviated presentation is that there are lots of
gaps in derivations, and lots of aspects of the code that's presented
that aren't described.  To really understand what's going on, you need
to look at the `scmutils` Scheme library on which the code in the book
is based.  This is the same library as is used in SICM, but I'm not
sure that things are really explained in detail there either.  The
`scmutils` code is pretty good, but there's quite a lot of it
(SLOCCOUNT says something around 65,000 lines).  You can certainly
take examples from the book and trace what's going on through the
`scmutils` functions that they use, but it's not a small or easy task.
The main problem is that `scmutils` is rather clever and rather
powerful, and the generality that it offers means that the code is
quite difficult to understand in places.  It would certainly be much
better to have more explicit descriptions of what the code examples in
the book are doing and how they're doing it.

## Scheme

All the code is in Scheme, as in SICP and SICM.  This isn't bad by any
means (I used to be a fairly committed Schemer), but I realised while
browsing through the `scmutils` library that I've become a little
spoiled with Haskell and its static types.  Whatever arguments you
want to make about static versus dynamic typing, the plain fact of the
matter is that the type of a function in Haskell provides you with
some information about what it does.  A type signature is a constraint
on the behaviour of a function that helps to narrow down the
conceptual space you have to explore to understand what a function
does.  A very simple and obvious example is that, if the return value
of a Haskell function doesn't live in the `IO` monad, there's no way
that the function can have any side-effects[^2].

And the fact is that mathematical objects do have types.  A connection
is a different type of thing from a vector field, which is a different
type of thing from a trajectory on a manifold, and so on.  It seems
that making those types manifest in the code would be of benefit.
Indeed, the `scmutils` code internally uses a sort of tagged structure
approach to distinguish these different kinds of objects, but Scheme
doesn't provide language-level support for saying "this is a vector
field" in the way that might be possible in a statically typed
language.

There are efforts to develop type systems that are more suitable for
expressing mathematics (for example, this [recent paper][atkey] about
conservation laws and type parametricity; the type system of the
[Axiom computer algebra system][axiom] is also worth mentioning), but
it seems as though it ought to be possible to have some half-way house
between dynamic typing as in Scheme and "all the types for all the
things EVAR" which seems to be the goal of some type theorists.
Whether it's possible to arrive at that half-way house with existing
tools isn't clear, but it might be interesting to reimplement
`scmutils` in Haskell or ML or some other statically typed language to
see how good the fit is.

## Conclusion

Overall, this is definitely worth a read if you're interested in any
or all of: differential geometry, functional programming, computer
algebra, "fancy" numerical methods.  There's much in here that's very
thought-provoking, even if the treatment is frustratingly brief.


[^1]: The compositional nature of functional programming often seems
      somewhat magical to me -- you can have a sort of true
      encapsulation of ideas with well-insulated boundaries that allow
      you to take a concept you've expressed programmatically and
      reuse it and compose it in an entirely natural way.

[^2]: Yes, yes, I know about `unsafePerformIO`.  Give me a break, all
      right?

[sicm]: http://mitpress.mit.edu/sites/default/files/titles/content/sicm/book.html
[atkey]: http://bentnib.org/conservation-laws.html
[axiom]: http://www.axiom-developer.org/axiom-website/screenshots.html
