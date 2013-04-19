---
title: Introduction
tags: colophonia
author: Ian
published: 2011-10-09 09:00:00
---

I've been meaning to start a blog for some time, but couldn't settle
on a platform.  I'd used Wordpress before for a [little
blog](http://www.skybluetrades/montpellier) we set up to let our
friends and family know what we were up to when we moved to
Montpellier, and I liked it (easy to install, easy to use).  For a
personal blog though, I wanted something a bit more hackable (yeah, I
know, you can hack on Wordpress too, but if it's going to be for
*fun*, I want to be using something other than PHP!).

<!--MORE-->

I spent a bit of time writing the beginnings of a blog platform of my
own in [Haskell][haskell], using the [Happstack][happstack] framework.
I got some way with that, but it was never really ready for primetime.
Recently though, I heard about [Hakyll][hakyll], thought "What a great
idea!" and jumped back in to getting something in shape.

[haskell]: http://www.haskell.org/
[happstack]: http://happstack.com/
[hakyll]: http://jaspervdj.be/hakyll/

Hakyll is a static website generator, which makes it great for
situations where server resources are limited.  I'm serving my site
from a [Linode VPS][linode] using the [Cherokee][cherokee] web server,
and I just don't need the full flexibility of a general webapp for
what I want to do.  Hakyll seemed like an ideal solution, and it's
Haskell, so it would be fun!

[linode]: http://www.linode.com/
[cherokee]: http://www.cherokee-project.com/

I'll write later about how I set Hakyll up for what I wanted, because
there were a number of things I wanted to do differently to the way
most Hakyll-based sites are set up, but you can see what I did
[here](http://github.com/ian-ross/blog).  The basic requirements were:

* Markdown syntax for posts;
* Wordpress-style `YYYY/MM/DD/TITLE` post addresses;
* Easy to organise posts with ancillary material (images, code, etc.);
* Good LaTeX math support.

In addition, I wanted a feature I'd implemented in my
not-quite-ever-finished blog framework.  This allows me to embed
[TikZ][tikz] code representing an image in a blog post and have it
rendered as an inline SVG image in the resulting HTML.  Useful?
Sometimes.  Cute?  Well, I think so.  I'll show how I did that in a
later article.

[tikz]: http://sourceforge.net/projects/pgf/

As for what else I'm going to write about here, it'll be pretty
eclectic.  Basically, anything I find interesting.  Which might range
from Haskell and Common Lisp stuff, to ecology, remote sensing and
vegetation modelling (my day job), book reviews, plus whatever else
takes my fancy.
