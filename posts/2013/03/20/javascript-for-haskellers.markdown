---
author: Ian
tags: haskell,web-programming
published: 2013-03-20 21:12:39
title: JavaScript Choices for Haskell Programmers
---
So, I've not done any blogging since the ill-fated Midwinter
Polyphasic Sleep Experiment.  I have a backlog of articles I want to
write, but most of those involve some significant investment of time
and effort.  So you get this instead...

## Context

I've been doing a *lot* of JavaScript programming recently.  One of
the two contracts I'm working on at the moment is for a UK university
spin-off called OpenBrain, who are trying to bring Bayesian statistics
to the masses by making [tools](http://www.bayeshive.com) that allow
scientists to build complex (or simple!) Bayesian models and have them
analysed using Markov chain Monte Carlo methods.  All the really
clever stuff is in the server-side MCMC inference code, but the
user-facing side of things is a web app with a Yesod backend.  My
mission is to prettify this and make it ready for prime-time.

It's a fairly complicated application that needs to manage literate
Markdown documents describing statistical models, data sets for
analysis and a bunch of other stuff.  The first thing I looked at was
a tool for building statistical models based on dynamical systems
represented as systems of ordinary or stochastic differential
equations.  This tool allows you to give algebraic representations of
ODEs or SDEs, which are then rendered as MathML (for prettiness and
familiarity), you can do simulations of the systems and visualise the
results, you can specify data sets that correspond to observations of
system variables, you can set up prior distributions for system
parameters, and you can then get some code (in a proprietary
Haskell-like language specialised for statistical modelling) that can
be used to drive MCMC inference for the parameter values.

All that stuff happens on the client side, except for the listing of
the available data sets and the rendering of the model into code.
This means that you need client side code for expression parsing,
analysis of coupled systems of ODEs and SDEs (to turn them into a
canonical form that you can simulate from), numerical integration of
ODEs and SDEs, graphing, and a framework to tie it all together.
That's a lot of client side code!

<!--MORE-->

## Choices, choices...

My original inclination was to use [Fay][fay], both because it's
[fun][fayart] and because the server side is all Haskell, but I was a
little anxious that it might be too much of a moving target.  There's
a lot of work being done at the moment to make Fay better and more
robust, which is great, but I don't have the resources to contribute
to that as well as trying to write something fairly substantial using
Fay, with the expectation that my code will rot quite quickly as Fay
development proceeds.

None of the other Haskelly options appealed very much, which meant I
was left with the prospect of writing a big old pile of JavaScript.
The expression parsing stuff wasn't too painful, mostly because of
[Jison][jison], a JavaScript almost-clone of Bison that made it easy
to write a JavaScript equivalent to my Haskell Parsec parser.  And the
numerical stuff for simulations wasn't hard either.  But the code for
taking a set of ODEs and transforming them into a canonical form that
can be numerically integrated?  That looked super-simple in Haskell
and really horrible in JavaScript[^1].  Case analysis by pattern
matching really does work better when you have types to match on...

However, the really tricky part was the user interface side of things.
I wanted something responsive: if you're typing an expression for a
differential equation and you introduce a new parameter ($\sigma$,
say), I want a UI element for that parameter to appear as you type
(and to disappear if you erase all references to $\sigma$).  In the
simulation panel, I wanted to be able to drag over parameter values to
change them and have all the plots update immediately.  That sort of
responsiveness, using the normal JavaScript event-driven approach to
interaction, is going to lead to a big bowl of spaghetti.

## Angular it is then

Shortly before this, Michael Snoyman had written a
[blog article][snoyberg] about using Fay with Yesod.  He also talked
about integrating the [AngularJS][angular] JavaScript framework with
Yesod.  As we'd ruled out Fay for the moment, I took a look at
Angular.  It turns out to be really quite neat.  It's a little hard to
explain briefly exactly all that Angular is, but there are two factors
that stuck out for me:

1. Data binding: Angular allows you to associate JavaScript variables
with DOM elements (these variables live in nested "scopes", which are
also associated with DOM elements).  The binding is bidirectional so
that if you have a piece of text bound to a variable and you change
the variable, the text changes; if you have an input element bound to
a variable and the user changes the contents of the input element, the
variable changes.  There are various ways of writing code that
interacts with this data binding and it's quite a flexible and
sophisticated system.

2. Directives: you can "extend" the HTML language by defining new
attributes, classes or element types.  These can have HTML templates
associated with them, as well as bits of code that can run at
different points during Angular's "HTML compilation" process.
Directives are flexible enough that you can (with a little patience)
do more or less anything.

A couple of examples of how these things really help: first, the
equation editing input elements are now `<equation>` tags, where
`equation` is an Angular directive that encapsulates a normal input
element (active when the element has the focus), error reporting
elements and a MathML rendering of the equation content.  All of the
equation parsing and event handling associated with the interactions
between these three elements is encapsulated within the `equation`
directive.  The second example is a `<scrubbable>` element directive.
This is a new kind of input element that shows a MathML rendered
equation for a parameter (e.g. $\sigma = 28$).  Clicking on the
element allows the user to edit it, control-clicking resets the
original value, and click-dragging continuously (or discretely, if an
attribute on the element is set appropriately) varies the value of the
parameter.  Using data binding, the changing value of the "scrubbed"
parameter can be directly communicated to other code: in the dynamical
systems simulation page, this is used to trigger re-running of
simulations and re-rendering of plots.

All in all, I'm pretty happy with Angular.  The documentation is a
little patchy, there's a bit of a learning curve, and doing
complicated things can require a bit of experimentation, but it's a
very good system for writing complicated interactive client side
JavaScript.

## Yesod integration

Furthermore, the integration with Yesod is really great.  Michael
Snoyman wrote a little `yesod-angular` module as part of his Yesod
client-side experimentation, and it turns out to be really rather
nice.  I've hacked my version around quite a bit to help with managing
Angular's dependency injection mechanism, but that's about all I've
had to change.  Perhaps the nicest feature is the idea of "commands".
Here, in my Yesod handler, in the bit of code that runs in the
`Angular` monad, I can define commands as, for example

~~~~ {.haskell}
    cmdContents <- addCommand treeGetContents

    ...

treeGetContents :: [Text] -> Handler [TreeItem]
treeGetContents path = do
  ...
~~~~

Here, `TreeItem` is some type with a `ToJSON` instance so that it can
be converted to JSON for transmission back to the client.  On the
client side, you can call this command as

~~~~ {.javascript}
      $http.post("#{cmdContents}", [node.data.fullPath]).
        success(function(data) {
          ...
        });
~~~~

All the data marshalling is handled by `ToJSON` and `FromJSON`
instances.  On the server side, the code is in Yesod's `Handler`
monad, so you can do all the kinds of things you might normally do in
response to client requests, and everything is nicely encapsulated.
It's a really good system.

## Conclusion

Writing a Haskell web app?  Dithering about the client side?  If you
can take the uncertainty of programming against a dynamic and
developing interface, go with Fay.  A lot of people are investing a
lot of time in it, it's already really good, and it's going to be
*great*.  Can't stand the uncertainty?  Look at `Yesod.Angular`.  It's
in Michael's `yesod-js` [repository][yesodjs].


[fay]: http://fay-lang.org
[fayart]: /blog/posts/2012/11/13/fay-ring-oscillator
[jison]: http://zaach.github.com/jison/
[snoyberg]: http://www.yesodweb.com/blog/2012/10/yesod-fay-js
[angular]: http://angularjs.org/
[yesodjs]: https://github.com/snoyberg/yesod-js

[^1]: To get an idea of the issue here, think about the differential
equation $y''(t) + 3 y'(t) + 2 y(t) = 3 \sin t$.  If you want to
integrate this numerically, you need to break it into the two
equations $w'(t) + 3 w(t) + 2 y(t) = 3 \sin t$, $y'(t) - w(t) = 0$,
where we introduce a new function $w(t)$ for the first derivative of
$y$.  This is the simplest case.  You also get cases where you end up
with a matrix equation for the first derivatives.  And you need to
deal with SDEs.  It's not very hard to do on paper, but writing code
to do it systematically in all cases is more challenging.
