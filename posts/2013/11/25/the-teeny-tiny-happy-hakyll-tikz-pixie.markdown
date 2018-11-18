---
author: Ian
tags: haskell,colophonia
title: The Teeny-Tiny Happy Hakyll TikZ Pixie
published: 2013-11-25 16:56:03
---

For my blog, I use Jasper Van der Jeugt's
[Hakyll](http://jaspervdj.be/hakyll/) system.  When I was first
looking for a blogging platform, I rejected all the usual choices
(Wordpress, etc.), mostly because I'm a borderline obsessive control
freak and they didn't give enough configurability.  Plus, they weren't
Haskell.  I started writing my own blogging system, but then I found
Hakyll.  "Great," I thought, "that's perfect, except I want something
almost completely different!"

Fortunately, Hakyll is written as a library that allows you to do more
or less anything you like with it.  This is common with Haskell tools:
[XMonad](http://xmonad.org/) is another "do what you like example".

Anyway, I set my blog up in a way completely different from the
"normal" Hakyll blog, but still using all the handy tools that Jasper
provided.  It was pretty easy to do (and it's even easier now, since
Jasper redid the central page processing abstraction to use monads
instead of arrows).

Carried away with my success, I added an extra feature that I thought
would be cool.  I've since changed my mind three or four times about
whether it really was a good idea, but I've now come to the opinion
that it's kind of sweet.

Here's how it works.  There's a very powerful LaTeX drawing package
called TikZ[^1] that allows you to say things like this:

~~~~ {.latex}
\begin{scope}[very thick]
  \draw (0.5,0.5) +(-.25,-.25) rectangle ++(.25,.25);
\end{scope}
~~~~

to draw a little square (TikZ can do a little bit more than this,
obviously!).  The thing I did was to allow you to embed this within
the Markdown for blog posts (between rows of `@` signs, just like you
put code samples between rows of `~` characters -- there's a special
syntax to say "don't wrap my TikZ code" so that you can add extra
libraries and arbitrary LaTeX setup code as well).  These bits of TikZ
code are pulled out during processing of the blog articles and
processed to generate SVG files, using `htlatex`, and these SVGs are
then embedded in the resulting HTML pages, like this:

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\begin{scope}[very thick]
  \draw (0.5,0.5) +(-.25,-.25) rectangle ++(.25,.25);
\end{scope}
@@@@@

Why do this?  Well, often when you're writing things with some
mathematical content, you want to embed little diagrams along the way
to explain things.  You can switch out of Emacs, start up Inkscape or
something and do some drawing.  The problem with that is that it's
really easy to get distracted by the details of the prettiness of your
drawing, it's hard to maintain consistent styling throughout a series
of pictures, and it's hard (or annoying) to get things lined up right
in precise diagrams.  Or you can quickly write some TikZ right in the
document where you are.  Once you've learnt enough TikZ to be useful,
it's very quick to knock out an accurate sketch of what you want
(since you're talking directly in terms of coordinates, rather than
shifting things around by hand), and it's easy to come back and apply
consistent styling when you're done.

Here are some examples taken straight from the TikZ manual:

<div style="width: 100%">
@@! { width: 24%; }
\usetikzlibrary{trees}
\tikzstyle{level 1}=[sibling angle=120]
\tikzstyle{level 2}=[sibling angle=60]
\tikzstyle{level 3}=[sibling angle=30]
\tikzstyle{every node}=[fill]
\tikzstyle{edge from parent}=[snake=expanding waves,segment length=1mm,segment angle=10,draw]
\tikz [grow cyclic,shape=circle,very thick,level distance=13mm,cap=round]
  \node {} child [color=\A] foreach \A in {red,green,blue}
    { node {} child [color=\A!50!\B] foreach \B in {red,green,blue}
      { node {} child [color=\A!50!\B!50!\C] foreach \C in {black,gray,white}
        { node {} }
      }
    };
@@!

@@@ { width: 24%; }
\draw[thick,rounded corners=8pt]
(0,0) -- (0,2) -- (1,3.25) -- (2,2) -- (2,0) -- (0,2) -- (2,2) -- (0,0) -- (2,0);
@@@

@@! { width: 24%; }
\usetikzlibrary{petri}
\tikzstyle{every place}= [minimum size=6mm,thick,draw=blue!75,fill=blue!20]
\tikzstyle{every transition}=[thick,draw=black!75,fill=black!20]
\tikzstyle{red place}= [place,draw=red!75,fill=red!20]
\tikzstyle{every label}= [red]
\begin{tikzpicture}[node distance=1.3cm,>=stealth',bend angle=45,auto]
  \node [place,tokens=1] (w1) {};
  \node [place] (c1) [below of=w1] {};
  \node [place] (s) [below of=c1,label=above:$s\le 3$] {};
  \node [place] (c2) [below of=s] {};
  \node [place,tokens=1] (w2) [below of=c2] {};
  \node [transition] (e1) [left of=c1] {}
  edge [pre,bend left] (w1)
  edge [post,bend right] (s)
  edge [post] (c1);
  \node [transition] (e2) [left of=c2] {}
  edge [pre,bend right] (w2)
  edge [post,bend left] (s)
  edge [post] (c2);
  \node [transition] (l1) [right of=c1] {}
  edge [pre] (c1)
  edge [pre,bend left] (s)
  edge [post,bend right] node[swap] {2} (w1);
  \node [transition] (l2) [right of=c2] {}
  edge [pre] (c2)
  edge [pre,bend right] (s)
  edge [post,bend left] node {2} (w2);
\end{tikzpicture}
@@!

@@@ { width: 24%; }
\pgfsetfillopacity{0.5}
\fill[red] (90:1cm) circle (11mm);
\fill[green] (210:1cm) circle (11mm);
\fill[blue] (-30:1cm) circle (11mm);
@@@
</div>

Personally, I think that's kind of fun, although I agree that it won't
be to everyone's taste.  If you want to play with it, you can find the
code in [my blog repository](https://github.com/ian-ross/blog).  You
can see the raw Markdown code for this post [here](https://raw.github.com/ian-ross/blog/master/posts/2013/11/25/the-teeny-tiny-happy-hakyll-tikz-pixie.markdown).

[^1]: *TikZ ist kein Zeichenprogramm* -- "TikZ is not a drawing
      program".  It's based on a lower-level package called PGF.
