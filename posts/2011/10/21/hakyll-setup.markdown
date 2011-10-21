---
author: Ian
tags: colophonia,haskell
timestamp: 20:10:38
title: Hakyll Setup
---
As blogging software, I use the really rather nice [Hakyll][hakyll],
by Jasper Van der Jeugt.  This is a static website generator written
in Haskell, and it's a great solution for smaller blogs or personal
websites.

Hakyll works just fine out of the box, but the blog setup I wanted was
a bit different from what I've seen other people do with it, so a bit
of hacking was required.  I wanted to share some of the things I've
done, since they might be of interest to other Hakyllers
(Hakyllites?).  All the code is available on [Github][repos].

<!--MORE-->

I was going to write some sort of extensive description of what I'd
done, but it makes more sense just to point at some places in the
code.  There are three source files, [blog.hs][blog.hs] which has all
the main Hakyll-required definitions, [Overrides.hs][Overrides.hs]
which contains definitions of some functions from the Hakyll libraries
that I override, and [TikZ.hs][TikZ.hs] which contains my slightly
rough around the edges code for converting images represented as
[TikZ][tikz] code into SVG for inclusion in web pages.

Here are the "highlights":

* [blog.hs][blog.hs] lines 109-115, and functions `makeIndexPages`,
  `makeIndexPage` and `indexNavLink`: most of the material for
  splitting articles across index pages, generating the index pages
  and the links between them.  The only tricky bit here was figuring
  out just how to get the Hakyll metacompilation feature working
  right.
* [blog.hs][blog.hs], function `addTeaser`: this is used to generate
  the article extract that appears on the index pages, and is a slight
  elaboration of a [suggestion of Jasper's][teaser]; the only nasty is
  fixing up the URLs for any images that appear in the teaser so that
  they refer to the right directory under the `posts` hierarchy.
* various places in [blog.hs][blog.hs] and
  [Overrides.hs][Overrides.hs]: code to allow me to use a more
  "Wordpressy" article naming convention, i.e. `yyyy/mm/dd/title`
  instead of Hakyll's default `yyyy-mm-dd-title.ext`.  I like to keep
  image files and other resources next to the text of articles, and
  this helps me to do that.
* everything in [TikZ.hs][TikZ.hs]: a while ago, when I was trying to
  write my own blog framework in Haskell, I had the idea of using
  [TikZ][tikz] to represent inline images in articles, and I cobbled
  up something to render TikZ code into SVG for embedding.  It seemed
  like a good idea at the time, but I'm now undecided about it, mostly
  because I've not yet been able to get SVG font styling working
  properly.  Getting the size of SVGs right in Chrome turns out to be
  a bit of a headache too.  Still, the code is there: it uses
  `htlatex` to do the conversion, and it seems to work.

I'm not exactly the world's best Haskell programmer, and some of this
stuff could do with some polishing.  The `processTikZs` function in
[TikZ.hs][TikZ.hs] in particular is a bit grim, including a use of the
pattern `(id &&& f) >>> (arr (\(x, fx) -> blah x fx))` that I seemed
to find myself using often enough that it looked like there ought to
be a standard combinator for it, but for which I couldn't find one in
`Control.Arrow`.  It's the first time I've used [arrows][arrows]
though, so I was just happy to end up with something that worked!

For what it's worth, all the code is free for reuse.

[teaser]: http://groups.google.com/group/hakyll/browse_thread/thread/43dc235755da8347?pli=1
[tikz]: http://www.texample.net/tikz/
[hakyll]: http://jaspervdj.be/hakyll/
[repos]: http://github.com/ian-ross/blog
[arrows]: http://en.wikibooks.org/wiki/Haskell/Understanding_arrows
[blog.hs]: http://github.com/ian-ross/blog/blob/9f78b89921742b8090967e3d4946732593114117/bin/blog.hs
[Overrides.hs]: http://github.com/ian-ross/blog/blob/9f78b89921742b8090967e3d4946732593114117/bin/Overrides.hs
[TikZ.hs]: https://github.com/ian-ross/blog/blob/9f78b89921742b8090967e3d4946732593114117/bin/TikZ.hs

