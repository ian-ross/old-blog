---
author: Ian
tags: colophonia,haskell
title: Upgrading to Hakyll 4
published: 2013-04-21 13:02:51
---
So, I lied a little in my [last post](the-war-on-akrasia).  I didn't
quite get to go off right away and update my Beeminder totals, because
I had a bit a problem with my blogging software.  I use Jasper Van der
Jeugt's [Hakyll][hakyll], a Haskell static site generator.  Jasper
recently (well, not that recently...) released a new major version of
Hakyll, Hakyll 4.  I had been putting off upgrading because I have so
much custom code that it's kind of a big job -- my blog is a little
different to most Hakyll sites, so I had to write quite a bit of extra
stuff to work around that.

When I tried to rebuild the blog with the Beeminder article though, I
started off down an amusing little path.  I'd upgraded my version of
GHC, so needed to reinstall things.  No problem.  Well, maybe.  The
version of Pandoc that Hakyll 3 uses no longer seems to build, even in
a completely clean sandbox.  I'd guess that there's some inconsistent
version bound resulting from one of Pandoc's many dependencies in that
version (1.9.4.5, if anyone cares).

So, I had a choice.  I could try to track down the dependency problem
in this old version of Pandoc (it's now at version 1.11.1, so that
would be an exercise of purely historical interest for most people),
or I could upgrade to Hakyll 4.  Cue much rolling up of sleeves and
girding of loins.  I jumped in.  It wasn't so bad, though it took me
all day plus a little bit.

In the end, I had far less custom code than before: the new Hakyll is
quite a bit more flexible than the old one and Jasper has made some
really good design choices this time around.  Two things stand out:

### Arrows be gone!

First, Hakyll 3 used arrows for its main sequencing mechanism for the
steps required to turn a Markdown (or whatever) blog post into its
final HTML incarnation.  Arrows are great, and can be really neat if
you learn and use the special `proc` syntax for them.  I've never got
around to learning that though, so I had a lot of code that looked a
little like this:

``` {.haskell}
processTikZs :: Compiler (Page String) (Page String)
processTikZs = (id &&& (arr pageBody
                       >>> (id &&& unsafeCompiler generateTikZs)
                       >>> arr (uncurry xformTikZs)))
               >>> (arr (\(p, pbnew) -> p{pageBody = pbnew}))
```

Urgh.  I felt a bit like [this guy][rubiks] writing this sort of
thing.  In Hakyll 4, the arrows are gone, and there's a `Compiler`
monad for sequencing.  That `processTikZs` function now looks like
this:

``` {.haskell}
processTikZs :: Compiler (Item String)
processTikZs = do
  b <- fmap itemBody getResourceBody
  ts <- unsafeCompiler (generateTikZs b)
  makeItem (xformTikZs b ts)
```

No more arrowised tuple juggling.  You can actually see what's going
on.  For sure, you can do exactly the same thing using `proc` with
GHC's `Arrows` extension enabled, but that's nowhere near as familiar
to most Haskell programmers as a monad.  It's also pretty clear that
the additional flexibility of arrows compared to monads just isn't
needed in this case.

### Contexts

The second major change in Hakyll 4 is the introduction of "contexts",
which are a monoidal interface for packaging up information about
posts, either as constant values, or as functions for extracting
values from page metadata or whatever you like.  This replaces the old
system of carrying this stuff around with the `Page` type, which led
to all sorts of one-hand-behind-the-back arrow tricks.

Everything is much clearer and cleaner now, and it's easy to write
extra context field functions to pick bits out of pages (for example,
I have a thing called `teaserField` which pulls out the text from the
body of a post up to the "Read more" link and puts it into a named
field in the context that can be used for rendering index page
entries).  It's *much* easier than the old way of doing this stuff.

### Summary

There are other things that are good (snapshots are cool, for
instance), but those are the two that struck me most as I was
upgrading my blog.

All in all, I think Jasper has done a great job with this.  Hakyll is
now much easier for new users to get into, and it's far easier to
customise the behaviour of Hakyll now.

I'm actually kind of disappointed with myself now that I didn't do the
upgrade earlier!  You can see my new code on [GitHub][repo].

[hakyll]: http://jaspervdj.be/hakyll/
[rubiks]: http://www.youtube.com/watch?v=zAIPL5O9Uwk
[repo]: https://github.com/ian-ross/blog
