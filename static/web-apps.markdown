<div class="row">
<div class="col">
# Web apps

## BayesHive

I spent most of 2013 working more or less full-time on
[BayesHive](http://www.bayeshive.com), a web platform for Bayesian
statistical modelling.  This has a back-end written in Haskell using
the Yesod web framework and a front-end using AngularJS.  The
front-end is fairly complicated, with a hierarchical explorer-style
file browser, a document editing and rendering interface and
wizard-like dialogue chains for building different kinds of Bayesian
models.

I've done more or less all the front-end development and most of the
back-end development that's not related to the Bayesian statistical
inference engine.


## The Radian plotting library

During the development of BayesHive, it became clear that we needed a
flexible solution for including plots into user documents.  These
plots are used for displaying basic data plots and plots of
probability distributions resulting from Bayesian data analysis.
Because the number and type of plots in a document can't be known in
advance, we settled on an approach using AngularJS directives to
construct a custom HTML library for describing plots, which I called
[Radian](http://openbrainsrc.github.io/Radian/).

</div>
</div>
