---
author: Ian
tags: haskell,day-job
title: Haskell Quasiquotation
published: 2013-09-10 14:07:31
---

For [BayesHive](http://www.bayeshive.com), one of the things we need
to do a lot of is program transformation.  The main reason for this is
to generate code to represent probability distribution functions
derived from descriptions of probability models -- the models are
described using code written in the probability monad, and this needs
to be manipulated quite extensively to determine the PDF, which is
needed for the Markov chain Monte Carlo sampling we use for estimating
parameters in probability models.

Program transformation is something that Haskell is really great at,
since it basically just involves pattern matching against lots of
different cases of syntax trees in the language you're transforming.
Unfortunately, this leads to lots of code that looks like this
(picking a simple case at random):

~~~~ {.haskell}
...
case last rvs of
   Observed (EApp (EApp (EVar "map") (EVar f)) (EVar nm)) -> rewriteFst f nm
   _     -> rvs
...
~~~~

All that stuff with `EApp` and `EVar` is the explicit AST
representation of the expression `map f nm` in the language (Baysig)
that we're manipulating.

At first, doing things like this is pretty unavoidable, but wouldn't
it be nice if you could write something like this instead?

~~~~ {.haskell}
...
case last rvs of
   Observed [baysig|map $f $nm|] -> rewriteFst f nm
   _     -> rvs
...
~~~~

<!--MORE-->

Well, with the magic of [Haskell quasiquotation][qq], that's exactly
what you can do.  I'd been looking at all this AST pattern matching
stuff for a while, thinking "Oh, I *wish* we had a quasiquoter", but
kept shying away from trying to write one.  I'd never done it before,
and it seemed like it might not be very simple.  However, it turned
out to be easy as pie (at least to get a first, sort of useful,
version going).

Assuming you already have a parser for your language (we do, of
course), adapting it for use within a quasiquoter turns out to be
pretty simple.  You need to add some stuff to your AST data types to
represent anti-quoters (`$f` and `$nm` in the pattern above, used to
represent *Haskell* names for *Baysig* AST values within a quasiquoted
Baysig expression) and rejig your parser to handle them.

Then you set up your quasiquoter.  This is just a value that looks
like:

~~~~ {.haskell}
baysig :: QuasiQuoter
baysig = QuasiQuoter { quoteExp  = baysigEToTHExp
                     , quotePat  = baysigEToTHPat
                     , quoteType = error "No type quoter"
                     , quoteDec  = error "No declaration quoter" }
~~~~

where the functions `baysigEToTHExp` and `baysigEToTHPat` have types
`String -> Q Exp` and `String -> Q Pat` respectively, i.e. they take a
string (which is everything inside the `[baysig|...|]` brackets) and
generate a Template Haskell expression (`Exp`) or pattern (`Pat`)
value (the `Q` thing is a monad that provides unique name generation
if you need it).

Of course, our Baysig parser doesn't return a Template Haskell `Exp`
value, but `Language.Haskell.TH.Quote` provides a couple of helper
functions to turn Haskell values (so long as they're instances of
`Data`) into TH expressions or patterns.  These functions have rather
inscrutable types:

~~~~ {.haskell}
dataToExpQ :: Data a => (forall b. Data b => b -> Maybe (Q Exp)) -> a -> Q Exp
dataToPatQ :: Data a => (forall b. Data b => b -> Maybe (Q Pat)) -> a -> Q Pat
~~~~

The first function argument is for dealing with special cases (the
documentation calls them "type-specific cases") and allows you to
intercept the data-to-TH conversion machinery to deal with things like
anti-quoters.  There's a neat little package called
[`antiquoter`][anti] that makes this really easy in a kind of "scrap
your boilerplate" way.  You define some functions that take values of
your input AST type and either return `Nothing` or `Just` a TH value.
Here, we deal with anti-quoter AST nodes, just turning them into TH
name references; everything else we ignore and return `Nothing`:

~~~~ {.haskell}
antiExpP :: E -> Maybe (Q Pat)
antiExpP (EAntiVar s) = Just . varP $ mkName s
antiExpP _            = Nothing
~~~~

The `antiquoter` package then provides some combinators to wrap these
functions up into a function suitable for use with the `dataToExpQ`
and `dataToPatQ` functions:

~~~~ {.haskell}
antiP :: AntiQuoter Pat
antiP = antiExpP <>> const Nothing
~~~~

Once you have this, dealing with quasiquotation is just a matter of
calling your parser and if the parse is successful, passing the result
to `dataToExpQ` or, as here, `dataToPatQ`:

~~~~ {.haskell}
baysigEToTHPat :: String -> Q Pat
baysigEToTHPat s = case parseQuoteEs s of
  Left err -> error $ "Parse failed in baysig quasiquoter pattern: " ++ err
  Right exp -> dataToPatQ antiP exp
~~~~

Easy-peasy.  And then you can use these things in pattern matches or
to build more complex expression from smaller ones:

~~~~ {.haskell}
let xval = [baysig|y+1|]
    fval = [baysig|f x|]
    expr2 = [baysig|let x = $xval
                     in $fval|]
~~~~

Cute, eh?  (And yes, the Baysig parser has a layout rule too, which
works perfectly happily when Baysig code is embedded within Haskell
like this!)

[qq]: http://www.haskell.org/haskellwiki/Quasiquotation
[anti]: http://hackage.haskell.org/package/antiquoter
