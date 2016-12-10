---
author: Ian
title: A Haskeller in Pythonland
tags: programming
published: 2015-07-20 15:22:55
---

I recently did some work for Andy Ridgwell, an old colleague from
Bristol, writing a build and configuration system and GUI for a
medium-sized climate model called [GENIE][genie].  GENIE is an *EMIC*,
an Earth system Model of Intermediate Complexity.  It's about 55,000
lines of Fortran and includes models of the atmosphere and ocean plus
models of atmospheric chemistry and biogeochemistry in the ocean and
ocean sediments.

This model had been in use for some years by different groups, and the
infrastructure around it had become quite baroque.  Andy wanted this
tidied up and made nice (i.e. rewritten...) to make the model easier
to set up and use.  He also wanted a cross-platform GUI for
configuring and running the model, allowing you to keep track of the
model state in real-time, to pause and restart model runs, changing
the model configuration in between, and so on.

A major consideration for this work was that as well as being easy to
use the new system had to be easy to install (on both Linux and
Windows) and easy for scientists to hack on.  That ruled out Haskell,
my usual tool of choice.  I decided to use Python instead, for a
couple of reasons.

<!--MORE-->

The first reason (one that I didn't tell Andy until after we were
done!) was that I'd never written a line of Python in my life, and
this seemed like a good project to use to learn -- quite a bit of file
handling and munging, some process communication, GUI programming.
That seemed like a good set of requirements to get a basic feel for
the language and its ecosystem.  The second reason was that I wanted
to try [SCons][scons] as a build system.  This is written in Python,
so I'd have to do some Python coding in any case, and limiting the
number of tools used seemed like a good idea.  And why SCons?  GNU
autotools didn't seem like a good fit here, and I've just never really
got on with CMake (I've tried it on a couple of projects, but always
ended up dropping it in favour of something else).

So, Python it was.  Being used to Haskell (compiled, static typing,
sophisticated type system), Python is going to feel very different
(interpreted, dynamic typing, run-time instead of compile-time
errors).  I was expecting it to be much more painful than it was.

Here are the Good and the Bad (I can do the Ugly myself...).


### The Good

 * Getting set up with Python is *easy*!  Perhaps it was because I
   almost exclusively used features from the core Python library so I
   didn't need to install extra packages, but the contrast between
   getting started with Python (on Linux, just start writing code!)
   and Haskell (install GHC and associated tools, work out what's the
   latest recipe for avoiding Cabal hell, etc.[^1]) was striking.  I
   did end up using the [Anaconda][anaconda] Python distribution, to
   be able to use `matplotlib` without making users manage Python
   packages themselves, but that's trivial to install.

 * If you've used any other "standard"-ish language, Python is easy to
   pick up.  There's nothing weird, the terminology used in the
   documentation is standard, and there are enough points of reference
   in common with other languages that you have something to pin
   Python knowledge on.  And the core language has lambdas, iterators
   and operator overloading, all features that make code more compact
   and easier to understand.

 * The standard library is good.  Really good.  Apart from
   `matplotlib`, I didn't need to use any external packages.  More or
   less every problem I wanted a "library" solution for I could find
   in the standard library.  For sheer usability, there's a lot to be
   said for this "batteries included" approach.  The standard library
   is well documented, and it's easy to find what you need.  (The
   language reference documentation isn't quite as good, but this is
   compensated by a good tutorial and a bunch of HOWTOs.)

 * It's easy to write code that is *almost* platform-independent.  The
   only issues I had getting code I'd written on Linux to work on
   Windows were kind of things you'd expect -- different file system
   semantics and process control, mostly.  Otherwise moving code from
   one platform to the other was smooth.

 * One thing I didn't need to use for this project, but that I covet
   in a most unseemly way for Haskell, is the Python scientific
   computing ecosystem.  There are good packages for linear algebra,
   machine learning, symbolic computation, data frames and statistics,
   machine learning, JIT compilation to LLVM, etc., etc., plus a bunch
   of other more specialised things that build on these frameworks.
   My covetousness reaches its zenith of unseemliness when I
   contemplate [Fenics][fenics], a wonderful FEM system for PDEs that
   I really must make up an excuse to use for something.  There are
   lots of other things of equivalent quality.  There has been a big
   push in part of the Python community to develop these tools over
   the last 10-15 years and some fields (astronomers, some
   neurobiologists, plus others) have adopted Python as their default
   data analysis and modelling platform, which means that there's a
   big user community pushing further development[^2].


### The Bad

The "bad" about Python really comes down to one thing, which is that
it's a dynamic language.  The holy wars between proponents of static
and dynamic languages aren't very interesting, but it's important to
recognise the trade-offs.

For Python, the positive side of the trade-off is pretty clear --
dynamic languages are easy to use, and they're especially suited for
interactive development and data analysis (which is why all the
scientific Python stuff is so popular).  It's hard to imagine how you
could replicate that flexibility in a statically typed language,
although that shouldn't stop people from trying.

However, dynamic languages, particularly compared to languages like
Haskell with a sophisticated and expressive type system, do lose out
in that they lack a certain kind of "rigidity" that static languages
can have.  When people talk about static languages being "rigid", they
usually mean it in a negative sense, but what I'm talking about is a
positive kind of rigidity.  In Python, if you have a working piece of
code and decide to refactor it, you can make changes in one part of
your code, rerun it and you'll get run-time errors, i.e. your code
will just fail to work.  You have to track down the other places in
your code that are affected by your refactoring yourself.  In Haskell,
if you have a working piece of code and you refactor it somewhere, the
Haskell compiler will tell you all the places that you need to change
other things to fit with your refactoring.  If you're used to dynamic
languages or languages with weaker type systems, this kind of
refactoring can feel very strange -- you start with working code, hack
away at it intensively for a couple of hours until the compiler says
you're good to go, you run the code and... it works, just the same as
before.  The compiler really is your friend in this situation, and the
"rigidity" that demands that all the parts of your code fit together
in compatible ways is a huge advantage.

This aspect of Haskell is one of my favourite things about the
language.  It encourages a much more exploratory approach to
programming, which is kind of paradoxical, given that you might expect
dynamic languages to be more, well, "dynamic" in that sense.  If you
know that making big structural changes to your code is rendered more
or less painless by the requirements imposed by the type system and
support from the compiler, the barrier to refactoring is lowered and
you can experiment with different design approaches without worrying
that you'll lock yourself into one way of doing things simply because
you can't face the work of refactoring to do things a better way.

This is why test-driven development is popular in dynamic languages:
it's easy to break your code, so you need lots of tests to ensure that
doesn't happen.  Simon Peyton Jones (one of the original developers of
Haskell) often describes static typing as a form of "lightweight
formal methods" -- a mechanism for proving certain properties about
programs that is invoked every single time you compile your code.  You
can think of it as the compiler automatically doing a whole bunch of
tests that you would probably have to write by hand in a dynamic
language.  But because they're always there and they're completely
automated, you can have a higher degree of assurance that those things
really are getting tested.  (Of course, you still need tests, but the
tests that you write tend to involve higher-level invariants of your
code that should be preserved, rather than checking argument types or
ranges.)

When I was working on the GENIE GUI code, I did a couple of
small-scale refactors that really brought home to me how much I depend
on the type system in Haskell.  My usual working practice is to change
what I want to change then run the compiler to tell me where I should
look to make things fit together again.  In Python, I have to find
those places myself, either by thinking, grepping, through tests (that
I have to write) or by crashing my code and working out what went
wrong.  It feels a bit primitive.


### Conclusions

Well, the obvious conclusion is that I wish I could have my cake and
eat it too.  It would be great if someone could port all the
scientific computing infrastructure that's available in Python to
Haskell.  The problem, of course, is where to find those "someones".
There are only a small number of research groups or companies working
on numerical applications of Haskell and, of those that I know about,
I can't think of any who could or would push the development of the
huge open-source scientific computing infrastructure that Python has.
The companies doing numerically oriented work are mostly in industries
where proprietary software development is the norm, with strong IP
protection policies.  The research groups are mostly coming at things
from a computer science perspective, so they have fewer resources to
put into developing libraries focused on applications.  It's quite
notable that the original developers of the core of the Python
scientific computing infrastructure were *scientists* who wanted to
use this stuff...

Back in the real world, I've written numerical code in Haskell
(notably [arb-fft] and a stiff ODE solver I wrote for FP Complete) and
it feels natural and clean and the compiler helps a lot.  But writing
all the code to get from where the Haskell numerical infrastructure is
now to being able to write something like Fenics (and a Haskell Fenics
would be *great*!) seems like an almost Sisyphean task.

My personal take on this is that I think I'll probably end up writing
more Python.  I want to explore some of the packages that are out
there (a [talk from the SciPy 2015 conference][jvdp-talk] gives an
overview) and use them for some fun stuff.  I think that the key thing
is to focus more on applications than technology -- doing things in
Haskell could be better if decent libraries existed, but the amount of
work you currently need to do to get to the "fun stuff" is
prohibitive.  Any discomfort I feel about Python the language, I think
I can assuage by the fact that getting fun stuff done with tools you
don't like so much is immeasurably better than not getting fun stuff
done at all because the tools you like would need years of effort to
build the infrastructure you need.


[genie]: http://www.seao2.info/mycgenie.html
[scons]: http://scons.org/
[anaconda]: https://store.continuum.io/cshop/anaconda/
[fenics]: http://fenicsproject.org/
[arb-fft]: http://hackage.haskell.org/package/arb-fft
[jvdp-talk]: https://youtu.be/5GlNDD7qbP4


[^1]: This is getting a *lot* better with some of the new tools that
      are being developed, but it's still much more intimidating to
      start out with than Python.

[^2]: The question "Why can't this happen in Haskell too?" is one I've
      been thinking about for at least two years.  I'm not sure that
      there's a large enough segment of the Haskell community that
      cares about these things to push the required development
      forwards, and I think we might have missed the boat a bit in
      terms of getting a big community of scientists to adopt Haskell
      for data analysis and modelling, now that they can just use
      Python and get all these great tools.  Sure, as languages, from
      a computer science, perspective, Haskell is in many ways
      "better" than Python, but that's not an argument that's going to
      appeal to working scientists very much.
