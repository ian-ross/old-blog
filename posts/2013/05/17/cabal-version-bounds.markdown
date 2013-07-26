---
author: Ian
tags: haskell
title: Cabal Version Bounds
published: 2013-05-17 21:53:13
---
**TL; DR: use package version number bounds in the `build-depends`
clauses of your Cabal files.  You may save yourself some time and
frustration.**

Most Haskell programmers seem to have Cabal war stories to tell.
Library version management in Haskell is complicated by the aggressive
inter-module optimisation that GHC tries to do, which makes Cabal's
life tough and sometimes necessitates blowing away both Cabal and
GHC's knowledge of installed packages to start over.  It's also
frustratingly easy to inadvertently get into a state where you have
versions of packages installed that prevent Cabal from finding a
consistent set of packages to install to satisfy the constraints for a
new package you're trying to install.

I thought I'd cleverly avoided most of these problems by using the
[hsenv][hsenv] sandboxing tool, which allows you to isolate the set of
packages needed for a particular project, and even to maintain
projects that use different versions of GHC, and
[cabal-meta][cabal-meta], which allows you to build multiple packages
from source together so that Cabal can find a set of package versions
that are acceptable for all of the packages at once.  I often found
myself succumbing to a smug sense of self-satisfaction as I read about
other peoples' Cabal problems on haskell-cafe.

Of course, pride goeth before a fall.  My hubristic little bubble was
burst the other day when I decided to reset my sandbox for a
[project][bh] I'm currently working on.  For some reason, typing
`cabal-meta install` just wasn't working, and I was being told that
there was no set of package versions that could consistently satisfy
the requirements of the packages I was trying to build from source.

Off down the rabbit hole I went.  How could this have happened?
Before I reset my sandbox, everything was building fine.  That meant
that a new version of some package must have appeared on Hackage with
different version dependencies than the version I had in the sandbox
before reset.  Before reset, the old package was fine and everything
worked.  After reset, Cabal downloaded the most recent package, which
broke the dependencies.

OK, so which package was the culprit?  And, more fundamentally, why
was everything so brittle that a single update of a dependent package
(that's all it turned out to be!) broke everything?

The first part involved some fun.  First of all, I wanted to find the
full set of packages that all of our code depended on, which is
essentially just the transitive closure of the `build-depends`
relation in the relevant Cabal files.  I wasn't thinking very clearly
at this point (having one's hubristic little bubble popped will do
that), so I started writing a Perl (!) script to parse Cabal files.
That plan ran aground very quickly once I realised what an idiot I was
being.  I switched over to using the Haskell Cabal library, which made
parsing and processing Cabal files a real breeze.  Right tool for the
job.

I thus ended up with a (long) list of packages.  Did I really want to
look at *all* of those to figure out what was going on?  At this
point, I made the critical observation, which after the fact, feels a
bit obvious: very few of the packages listed in the `build-depends`
clauses of our Cabal files had version number bounds.  There was no
excuse for this -- it was plain laziness.  Whenever I needed to add a
new package dependency, I just stuck the package name in the
`build-depends` list and went on about my business.

The problem with this approach, of course, is that if your package `A`
depends on Hackage packages `B` and `C` and both `B` and `C` further
depend on package `D` (all without any version bounds specified), it's
very easy to transition from a state where everything works (`B` and
`C` have compatible version requirements for `D`) to a state where
nothing works (`C` has been upgraded and now depends on a version of
`D` that is incompatible with the version required by `B`).

So, I learnt a valuable lesson -- *always* provide version bounds for
packages listed in `build-depends`.  And I paid the price for my
lesson: half a day of frustration trying to figure out what was going
on, then a couple of hours of laboriously tracking down appropriate
version bounds for *all* of the packages we refer to.  And now
everything works and I can retreat back into my hubristic little
bubble...

[hsenv]: https://github.com/Paczesiowa/hsenv
[cabal-meta]: http://www.yesodweb.com/blog/2012/04/cabal-meta
[bh]: http://www.bayeshive.com/
