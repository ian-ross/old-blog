---
author: Ian
tags: web-programming
title: AngularJS + D3.js = Radian
specials: angular(myApp)
published: 2013-04-24 19:30:39
---

**UPDATE: Radian is now open source.  Read about it on the
[BayesHive website here](http://bayeshive.com/blog/5/radian--free-declarative-plotting-for-the-web).**

*Courtesy of Hideharu Sakai, this article is also available in
 [Japanese](http://ja.d3js.info/skybluetrades/).*

One of the things we needed to be able to do for the [BayesHive][bh]
software I'm working on with Tom Nielsen of OpenBrain is easily
produce plots within our front-end web app.  There are lots of
JavaScript libraries for doing this: Tom started off using
[Flot][flot], which is very quick and easy to use, but wasn't really
flexible enough for what we wanted.

I'd just been working on another part of the software, a tool for
constructing algebraic representations of dynamical systems (defined
by systems of ordinary or stochastic differential equations), and I'd
been using the [AngularJS][angular] framework.  It took a while to get
used to, but it seemed as though it might, in conjunction with
[D3.js][d3], provide a way to produce a declarative plotting API that
would be really easy to use and could take advantage of the clever
data binding features of Angular.

Thus was Radian born: a declarative extension to HTML for rendering
data and functional plots as SVG graphics.  You can go straight to a
gallery of examples [here](/radian).  Details and some more examples
below the fold.  (Note that, since Radian uses SVG graphics, you'll
need a fairly recent browser to see anything...)

<!--MORE-->

## The basic idea

Angular allows you to define *directives*, custom HTML elements that
can use template replacement or arbitrary JavaScript code that is
called at various points during Angular's "HTML compilation" process.
Plots in Radian are specified using custom directives: the main
wrapper around a set of plots on the same axes is a `<plot>`
directive.  Within a `<plot>` directive, you can place multiple
directives specifying different kinds of plots.  For example:

~~~~ {.html}
<plot height=300 aspect=3 stroke-width=2 x="[[seq(0,4*PI,101)]]"
      axis-x-label="Time" axis-y-label="sin(x) / cos(x)">
  <lines y="[[sin(x)]]" stroke="red"/>
  <lines y="[[cos(x)]]" stroke="blue"/>
</plot>
~~~~

The attributes with the double square brackets are *Radian
expressions*.  These are very much like Angular expressions, but the
language they support is a strict superset of the JavaScript rather
than a restricted subset as for Angular expressions (I use a
customised version of the very nice [Acorn JavaScript parser][acorn]
to deal with these things).  Here, the `seq` function produces an
array of regularly spaced values, and the `sin` and `cos` functions
are implicitly vectorised over arrays so that the `y` values are
calculated as you might expect.  (The Radian plotting library includes
a bunch of useful functions, all of which exhibit this useful
vectorised behaviour, as do arithmetic operators in Radian
expressions.)  The above code results in the following plot:

<plot height=300 aspect=3 stroke-width=2 x="[[seq(0,4*PI,101)]]"
      axis-x-label="Time" axis-y-label="sin(x) / cos(x)">
  <lines y="[[sin(x)]]" stroke="red"/>
  <lines y="[[cos(x)]]" stroke="blue"/>
</plot>

(All the examples here are produced by just cutting and pasting the
appropriate HTML into the source of this blog article.  Apart from
making sure that the appropriate JavaScript libraries are loaded and
activating Angular on the page, nothing else is needed.)

## Handling data

Plotting functions is all very well, but we more often need to
generate plots based on data -- either data uploaded by users or the
results of statistical analyses.  Radian makes this easy with a
directive called `<plot-data>`.  We can specify data in-line within
our HTML page or cause it to be loaded from a URL as required.  Here's
an example:

~~~~ {.html}
<plot-data name="fam" src="familyr.csv"
           format="csv" cols="name,age,height,sex,salaryhr">
</plot-data>
~~~~

We can then use this data in a plot as:

~~~~ {.html}
<palette name="mfpal" type="discrete">
  female #FF7F7F; male #7F7FFF
</palette>

<plot height=600 aspect=1 stroke="none" marker="circle"
      axis-x-label="Age" axis-y-label="Height">
  <points x="[[fam.age]]" y="[[fam.height]]"
          fill="[[mfpal(fam.sex)]]"
          marker-size="[[30*sqrt(fam.salaryhr)]]">
  </points>
</plot>
~~~~

yielding the following figure:

<palette name="mfpal" type="discrete">
  female #FF7F7F; male #7F7FFF
</palette>

<plot height=600 aspect=1 stroke="none" marker="circle"
      axis-x-label="Age" axis-y-label="Height">
  <points x="[[fam.age]]" y="[[fam.height]]"
          fill="[[mfpal(fam.sex)]]"
          marker-size="[[30*sqrt(fam.salaryhr)]]">
  </points>
</plot>

<plot-data name="fam" src="familyr.csv"
           format="csv" cols="name,age,height,sex,salaryhr">
</plot-data>

Note how we've used a palette here to specify the mapping from gender
values to colours -- the `<palette>` directive produces a JavaScript
*function* that can then be applied to a data array.  In general, all
paint attributes (fill, stroke, marker size, stroke width, etc.) can
be calculated as functions of data.  This orthogonality, along with
the power of the Radian expression formulation, makes it possible (and
easy) to produce bubble charts and other data-dependent graphics.

## Binding to Angular

The relationship between the variable names used in Radian directives
and Angular scope variables is designed to make using Angular's data
binding capabilities easy.  *All* attribute names in Radian plotting
directives generate Angular scope variables (with nested scopes being
created as appropriate), and all free variables in Radian expressions
are treated as referring to Angular variables in the enclosing scope.
(It helps to understand how Angular scopes work here, of course...)

The upshot of all this is that you can do things like this:

~~~~ {.html}
<div class="form-inline">
  <label>Mean</label>
  <input type="range" min=0 max=10 step=0.01 ng-model="mu" ng-init="mu=5">
  <label>&nbsp;&nbsp;Standard deviation</label>
  <input type="range" min=0 max=10 step=0.01 ng-model="sigma" ng-init="sigma=1">
</div>
<br>

<plot height=300 aspect=3 stroke-width=2 stroke="red">
  <lines x="[[seq(0,10,200)]]" y="[[normal(x,mu,sigma)]]"/>
</plot>
~~~~

to get the following:

<div class="form-inline">
  <label>Mean</label>
  <input type="range" min=0 max=10 step=0.01 ng-model="mu" ng-init="mu=5">
  <label>&nbsp;&nbsp;Standard deviation</label>
  <input type="range" min=0.01 max=10 step=0.01 ng-model="sigma" ng-init="sigma=1">
</div>
<br>

<plot height=300 aspect=3 stroke-width=2 stroke="red">
  <lines x="[[seq(0,10,200)]]" y="[[normal(x,mu,sigma)]]"/>
</plot>

Manipulating the UI elements (bound to Angular variables using
`ng-model`) leads to immediate changes in the plot.

## Summary

Radian is very much a work-in-progress.  I'm adding new functionality
(and fixing bugs) nearly every day.  I'm learning a lot as Tom and I
use Radian in the BayesHive web app: the examples here and in the
gallery are standalone, and don't really get into the gritty corners
of the interactions between Radian and other Angular code.  Those
gritty corners are being exercised by the integration of Radian into
BayesHive, which is essentially a document authoring system oriented
towards Bayesian data analysis.  The functionality of the system is
completely open-ended and every data analysis example we come up with
stresses Radian in a different way.

Radian is currently proprietary.  There are some contractual
encumbrances that prevent us from open-sourcing it right now, but that
may change at some point in the future.  In the meantime, if you're
interested in learning more, contact
[me](mailto:ian@skybluetrades.net) or
[Tom](mailto:tomn@openbrain.org).


[bh]: http://www.bayeshive.com/
[flot]: http://www.flotcharts.org/
[angular]: http://angularjs.org/
[d3]: http://d3js.org/
[acorn]: http://marijnhaverbeke.nl/blog/acorn.html

<style type="text/css">
.radian svg {
  -webkit-touch-callout: none;
  -webkit-user-select: none;
  -khtml-user-select: none;
  -moz-user-select: none;
  -ms-user-select: none;
  user-select: none;
  display: block;
  width:100%;
  height:100%;
}
.radian svg text{font:normal 12px Arial;}
.radian svg .title{font:bold 14px Arial;}
.radian .no-data{font-size:18px;font-weight:bold;}
.radian .background{fill:white;fill-opacity:0;}
.radian div.radian-ui{margin:5px;}
.radian div.radian-ui .var-select{width:50px;}
.radian .axis path{fill:none;stroke:#000;stroke-opacity:.75;shape-rendering:crispEdges;}
.radian .axis line{fill:none;stroke:#000;stroke-opacity:.25;shape-rendering: crispEdges;}
.radian .axis path.domain{stroke-opacity:.75;}
.radian .axis line.zero{stroke-opacity:.75;}
.radian .axis .axisMaxMin text{font-weight:bold;}
.radian .brush .extent{stroke:#fff;fill-opacity:.125;shape-rendering:crispEdges;}
</style>
<script src="/radian/js/jquery.js"></script>
<script src="/radian/js/bootstrap.js"></script>
<script src="/radian/js/d3.v2.js"></script>
<script src="/radian/js/angular.min.js"></script>
<script src="/radian/js/radian.js"></script>
<script src="/radian/js/html5slider.js"></script>
<script>
angular.module('myApp', ['radian']);
</script>
