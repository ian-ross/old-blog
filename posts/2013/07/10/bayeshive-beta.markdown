---
author: Ian
tags: haskell,day-job,web-programming,yesod
title: BayesHive Beta
published: 2013-07-10 23:19:19
---

For the past few months, I've been spending most of my working hours
on [BayesHive](http://www.bayeshive.com), the brainchild of Tom
Nielsen, CEO of OpenBrain.  This is a web platform for doing Bayesian
data analysis, intended to make these powerful methods more accessible
for scientists and other data analysts.

We opened the system to some alpha users a while ago, but this week
we've decided we're ready for wider beta testing, for people to have a
look, kick the tyres and tell us what they think.

I've mostly been working on the web app part of the system, but in
this post I want to talk a little bit about the general idea behind
the system and the Bayesian approach to statistics.  (As far as
BayesHive goes, the system is made up of a reasonably complicated
single page web app using the
[AngularJS framework](http://angularjs.org/), a
[Yesod](http://www.yesodweb.com/) server process to deal with the
management of documents, data and models, and a model compiler and
inference engine that does all the clever stuff.)


## Why Bayesian?

Suppose we have some data and we want to fit a linear regression
model.  If we have a single predictor $x$ and an outcome variable $y$,
we can write the usual linear regression model as something like $y
\sim \mathcal{N}(a + b x, \sigma^2)$, where $a$ and $b$ are regression
parameters, and $\sigma^2$ is the variance of our outcome variable
that isn't explained by the linear variation.  In Baysig, the
modelling language used in BayesHive, this model looks like this:

```
regress = prob
  a ~ improper_uniform
  b ~ improper_uniform
  variance ~ improper_uniform_positive
  repeat 150 $ prob
    x ~ any
    y ~ normal (a + b * x) variance
    return { x => x; y => y }
```

Here, we give prior distributions for $a$, $b$ and $\sigma^2$, then
generate samples from this model, taking arbitrary values of $x$ and
generating corresponding values of $y$.

From this model description, BayesHive can produce code suitable for
doing Markov chain Monte Carlo estimation of the parameters $a$, $b$
and $\sigma^2$.  You can fit the model to data by doing something
like:

```
par <* estimate regress (#data)
```

The outcome of that estimation process is a joint distribution of
these parameters that you can then use to answer all the normal
statistical questions you might ask about linear regression.

Of course, in this setting, the natural reaction is "big deal".
Linear regression is easy.  Well understood theoretically, easy to
calculate regression coefficients and confidence intervals on them
using standard formulae.  Why would you ever mess around with this
sort of Bayesian estimation for such a simple model?

OK, that may well be true.  So let's think about a trickier example.
What about estimating parameters for dynamical systems models,
specifically stochastic differential equation models?  A commonly used
model for stock prices is Heston's stochastic volatility model, which
is a pair of coupled SDEs:

$$d S_t = \mu S_t dt + \sqrt{\nu_t} S_t d W^{(1)}_t$$

$$d \nu_t = \kappa (\theta - \nu_t) dt + \xi \sqrt{\nu_t} d W^{(2)}_t$$

Here, $S_t$ is the price of a stock, $\nu_t$ is its volatility, $\mu$,
$\kappa$, $\theta$ and $\xi$ are parameters to be estimated from data
and $W^{(1)}_t$ and $W^{(2)}_t$ are two (possibly correlated) Wiener
processes.

Standard formulae aren't looking so good now, are they?  In Baysig:

```
heston = prob
  s_0 ~ improper_uniform_positive
  v_0 ~ improper_uniform_positive
  kappa ~ improper_uniform_positive
  mu ~ improper_uniform
  theta ~ improper_uniform_positive
  xi ~ improper_uniform_positive
  w0 ~ wiener
  w1 ~ wiener
  d s t = mu * s t + sqrt (v t) * s t * d w0 t
  d v t = kappa * (theta - v t) + xi * sqrt (v t) * d w1 t
  return { s => s; v => v }
```

and we can estimate the parameters from data by doing this:

```
par <* estimate heston ({ s => #gsk_l })
```

where `gsk_l` is some stock price time series data.  From this, we get
out a joint distribution of the system parameters, plus a
*distribution over paths* of the latent volatility variable $\nu_t$.

The point of this is that we can write **generative** models, like
`regress` or `heston`, that parallel the mathematical notation we
would use to write these models down to think about them, and
BayesHive can take those descriptions and turn them into the code
necessary to do Bayesian parameter estimation.  The posterior
distribution of parameters (and latent variables) that you get out of
this process is all you need to answer any questions there are about
hypothesis testing, confidence intervals, decision processes and so
on[^1].

So, that's all great.  It comes at a price of course, first in
computational resources -- MCMC methods are not as cheap as methods
specialised to particular models.  On the other hand, if you decide
that you want to use a model that doesn't fit the assumptions of those
special methods, you're out of luck.  With MCMC methods, you can often
modify your model and the methods go on working.  The second price you
pay is in the complexity of implementation, but that's what BayesHive
is all about.  In its current incarnation, there are lots of
restrictions on what sort of models you can use if you want them
smoothly compiled and transformed into MCMC sampling schemes, but
we'll be lifting a lot of those restrictions as we go forwards.  It's
not a simple task, but the benefits of this approach really do
make it worth it.


## The way forward

We're at an early beta stage with BayesHive.  There is work to do on
the browser and server sides of the web app, on the Baysig language
platform, and on the inference engine.  There are tools to be
developed for post-simulation data analysis, there are tutorials to be
written to try to get people thinking about statistics and data
analysis in this new way, building bridges from the "conventional"
approaches.  There is work to be done on specialised data processing
and analysis methods for things like spatial and spatio-temporal data
(there's a big community that builds hierarchical Bayesian models for
this sort of data).

It's going to be a lot of fun!



[^1]: Of course, this is eliding a huge field of questions about how
      well the MCMC inference is working, whether the chains are
      properly mixed, whether you've sampled enough, whether your
      model is properly identifiable in the first place.  Complicated
      details, but you have to worry about issues of the same degree
      of complexity no matter how you go about fitting these complex
      models.
