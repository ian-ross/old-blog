<div class="row">
<div class="col">
# GUI programming

## Early exposure

When I think about it, I've done a surprising amount of GUI
programming, on a really surprising range of platforms.  My first
exposure was writing X applications using Xt and Motif.  You had to
write a *lot* of code to get anything done and the "object-oriented C"
approach used for defining widgets took a bit of getting used to.

Since then, I've done more and bigger stuff with Motif, I got to play
with an early version of NeXTStep, I've used Qt on Linux and I've even
done some Windows GUI programming, which I didn't enjoy a whole lot.

## wxWindows and Isla

My most recent GUI programming work has been with wxWindows, writing a
tool to produce island outline data for a general circulation model of
the ocean (this is part of a big climate model, and needs island
outlines computed according to a fairly complicated geometrical
prescription as part of its input data[^1]).

<a href="misc-images/isla-screenshot.png"><div class="img-left">![Isla](misc-images/isla-screenshot.png)</div></a>

This application (called [Isla](https://github.com/ian-ross/isla)) was
interesting to write, partially because of the complicated island
determination rules, but also because it's a scrolling zoomable map
view of a land/sea mask at variable resolution (the climate model can
be run in lots of different configurations with lots of different
ocean grid resolutions, all of which have to be dealt with), with
island information overlaid as it's calculated (the user can also
coarsen or refine the sets of bounding rectangles used to define each
island if they want).

GUI programming in C or C++ tends to be similar whatever platform or
framework you're using.  wxWindows is straightforward -- all the usual
tasks of laying out forms, making menus and so on are easy to do and
there are some nice pre-processor helpers that make event routing
convenient.  Isla, like a lot of GUI applications, has a wrapper of
menu commands and a few dialogues around a "canvas" where most of the
interaction happens: for Isla, this is the zoomable map view showing
the land/sea mask and the island data.  Implementing this kind of
thing is usually quite a good test of how convenient a GUI framework
is to use, since you have to do most things by hand: coordinate
transformations, drawing, mouse events and so on, and you have to fall
back on the lower-level features of the framework.  wxWindows is nice
in this respect, and it feels like something close to an optimum for
GUI development in C++.  I'll use it again if I have clients who want
this sort of application.


## Web stuff

The other option for GUI development that's become common is to write
a web app and use the browser as the GUI.  This has advantages for
cross-platform development and it strongly decouples the server code
from the client code.  It has the disadvantage that the languages and
tools in the browser are pretty poor: HTML and CSS are not a great fit
for laying out complex GUIs and Javascript is terrible, both for
intrinsic language reasons and for its GUI model -- it was just never
intended to be used for this purpose.

I've now done quite a bit of this kind of development, mostly for
[BayesHive](http://www.bayeshive.com).  It's really unfortunate that
we've got to the state we have with the browser infrastructure because
it makes a lot of things just much more difficult than they need to
be.  There are some improvements in the standards pipeline: among
other things, the HTML5 canvas element is now nearly universally
supported, the CSS3 grid layout proposal looks promising, and the
proposals for HTML data binding also look good, and will go a long way
to cleaning up the zoo of Javascript frameworks.  All that said, front
end programming for complex applications is still only doable, not
pleasant.


## Things I'd like to try

There are are three main things I'd like to spend a little bit of time
on in terms of GUI programming:

1. The first is alternatives to Javascript.  There are several efforts
   to compile Haskell or subsets of Haskell to Javascript for use in
   the browser.  I've done some simple experiments with [Fay][fay],
   which I really like, and it will be interesting to see what happens
   when the Javascript backend to GHC is released into the wild.  I'm
   also kind of tempted by
   [Clojurescript](https://github.com/clojure/clojurescript): although
   I've mostly been Haskelling of late, I do like a nice Lisp...

2. [Functional reactive programming][frp] is a completely different
   idea to the "standard" widgets-and-events approach to GUI
   programming, and it looks quite exciting.  I think it's fair to say
   that it's still quite "researchy", but it shows a lot of promise.
   I'm kind of tempted to try writing a little game using
   [Netwire][netwire] and [SDL][sdl] at some point to see how it
   feels.

3. One option that's become available recently for quick and easy GUI
   development in Haskell is [Threepenny-GUI][three].  Applications
   written using this basically run as local web servers, which you
   connect to using your browser.  You then get a GUI in the browser,
   but without having to write any HTML or CSS if you don't want to,
   and without having to write much Javascript.  It's an interesting
   idea, and one I'd like to play with.  I have a bibliography
   database management tool I wrote some years ago that has a Curses
   interface, and it might be interesting to upgrade it to a
   browser-based GUI using this.  Threepenny-GUI also has FRP
   facilities, which might be fun.


[^1]: If you want the grubby details, the "island outlines" are closed
      curves on the streamfunction (or velocity) grid of the model
      (the land/sea mask defining the islands is given on the tracer
      grid, which is offset from the velocity grid).  The model does a
      line integral of the streamfunction around these curves and
      performs a relaxation calculation to fix the streamfunction
      values in the "holes" in the ocean flow defined by the islands.
      There are a couple of pages of rules defining how the island
      outlines should be determined from the land/sea mask.

[fay]: /blog/posts/2012/11/13/fay-ring-oscillator/index.html
[frp]: https://en.wikipedia.org/wiki/Functional_reactive_programming
[netwire]: http://hackage.haskell.org/package/netwire
[sdl]: https://en.wikipedia.org/wiki/Simple_DirectMedia_Layer
[three]: http://www.haskell.org/haskellwiki/Threepenny-gui

</div>
</div>
