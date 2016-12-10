---
title: "Haskell Comic Scraper: Part 1"
published: 2011-10-10 13:00:00
author: Ian
tags: haskell,programming
---

I'm a relative beginner with [Haskell][haskell], and like many people,
to start with I was a little perplexed by the Haskell approach to I/O.
A small worked example helped a lot.  I was curious to see how easy it
would be to do something like the webcomic scraper application
implemented in [Clojure][clojure] [here][comics1] and [here][comics2].
This is a simple application, but it does do realistic I/O,
downloading files from the web, writing them to disk, and also doing
some computations on the file contents.  Over the course of two
articles, I'm going to build something comparable in Haskell.  It
turns out to be pretty easy!

<!--MORE-->

## The I/O problem ##

Before we get started, just why is I/O a problem in Haskell?  (A
problem for beginners anyway.)  Haskell places great weight on purity
and referential transparency, and this makes dealing with the outside
world more interesting than in imperative languages.  Haskell's
approach to dealing safely with impure actions with side effects is to
use a monad (the `IO` monad) to segregate I/O "actions" from pure
functional code.  This takes a bit of getting used to.

The other common approach to handling I/O in pure code is exemplified
by the uniqueness types of [Clean][clean] and [Mercury][mercury],
where some "world" object is threaded through purely functional code
-- the *old* state of the world is passed into a function that does
I/O, the *new* state of the world is passed back out, and the type
system of the language enforces "single-threaded" access to the state
of the world (Mercury calls the input state a *destructive input*
argument, and the output state a *unique output*).  The same sort of
threading of I/O state has to happen in the run-time system of any
functional language, and Haskell's monadic I/O syntax can be thought
of as a way to hide this plumbing.  Of course, monads can be used for
much more than this, but I'm not going to talk about that at all.
Here, I'm taking a beginner's eye view of things, and I'm going to
talk only about the I/O monad.

## Getting started ##

First, let's define the problem we want to solve.  We want to start
with definitions of some webcomics (base page, a regular expression to
use to pick out the address of the image file for the latest episode,
etc.) and download the image files for the latest episode of each
comic.  To do this, we'll get the base page of the webcomic, use the
contents of that page to figure out the URL of the latest episode
image, download that image and save it to a file.  We'll get a little
more fancy about this later (saving images to directories based on
today's date and reading the comic definitions from an XML
configuration file), but let's start with this simple goal in mind.
The code for this stage of the example can be found
[here](ComicScraper1.hs).

We're going to need a couple of Haskell packages that aren't installed
by default, in particular `Network.HTTP` and the Posix regular
expression package `Text.Regex.Posix`.  If you have [Cabal][cabal] set
up (which you most definitely should), installing these is as simple
as typing

~~~~
cabal install HTTP
cabal install regex-posix
~~~~

Let's start by playing a little with a function to download the
contents of a URL.  If we put the following into a Haskell script
file, `tst.hs`, say:

~~~~ {.haskell}
import Network.HTTP
fetchURL :: String -> IO String
fetchURL url = do
  resp <- simpleHTTP (getRequest url)
  return (getResponseBody resp)
~~~~

then we can do the following in `ghci`

~~~~
Prelude> :load tst.hs
*Main> fetchURL "http://www.xkcd.com/"
~~~~

and we get back a string containing the HTML contents of the XKCD
front page.  So, how does this work?  First, let's think about the
type of `fetchURL`, which is `String -> IO String`: it takes a single
string as input (the URL we want to fetch) and returns a value of type
`IO String`.  This return type represents an I/O action that yields a
`String` when we execute it.

If we think about a typical I/O action, reading a character from a
file say, it's clear why these need to be treated differently from
pure functions.  If we call a putative `getCharFromFile` function
twice in a row, passing the same file handle as an argument, we
don't expect to get the same result from each call -- the results that
we get depend on the contents of the file, and in general, each call
to `getCharFromFile` will yield a different result.  This is quite
different to the behaviour of pure functions: if we evaluate `head
[1, 2, 3]`, we always get `1` as the result, independent of the state
of the outside world.

These considerations mean that we need some framework for sequencing
and executing I/O actions.  In Haskell, this is provided by the `IO`
monad, which the Haskell run-time system treats specially to enable
this non-pure interaction with the outside world.

I/O actions are sequenced using Haskell's `do` syntax, which we will
examine in more detail below.  For the moment, take the details of
`fetchURL` on trust, and concentrate more on the similarity to a more
familiar imperative way of programming displayed in the `do` block.

## Data structures ##

We're going to represent the comics we want to download as simple
structures, with string entries for the name of the comic, the URL of
the base page, a regular expression to allow us to extract the URL of
the latest story image from the base page, and a prefix to put on the
front of that URL (which will probably be a relative URL) to make an
absolute URL we can use to download the story image:

~~~~ {.haskell}
data Comic = Comic { coName :: String,
                     coURL :: String,
                     coRegex :: String,
                     coPrefix :: String }
             deriving Show
~~~~

(The `Comic` type is defined using Haskell's
[record syntax][haskell-report].)  Eventually, we'll read a list of
comics to download from a configuration file, but for now, let's
define a few comics for testing:

~~~~ {.haskell}
comics :: [Comic]
comics = [ Comic "Girl Genius"
                 "http://www.girlgeniusonline.com/comic.php"
                 "ggmain/strips/ggmain[^']+"
                 "http://www.girlgeniusonline.com/",
           Comic "XKCD"
                 "http://www.xkcd.com/"
                 "comics/.+png"
                 "http://imgs.xkcd.com/",
           Comic "Girls With Slingshots"
                 "http://www.daniellecorsetto.com/gws.html"
                 "images/gws/GWS[^\";]+"
                 "http://www.daniellecorsetto.com/" ]
~~~~

## Regular expression matching ##

Given a `Comic` definition, the first thing we want to do is download
the base page and extract the URL of the latest image file.  We'll
write a function `imageURL` to do this.  It will have type `Comic ->
IO String`, returning an I/O action with a `String` result (the URL we
want).  The result here is an I/O action because we're interacting
with the outside world: we certainly don't expect to get the same
result from `imageURL` every time we call it with the same arguments.
It wouldn't be very useful if we did...  This is a good general guide
for when you need to think about using actions: if a piece of code
should return the same answer whenever it's called with a given set of
arguments, then it can be implemented as a pure function.  Otherwise,
the I/O monad is going to be involved.

Here's `imageURL`:

~~~~ {.haskell}
imageURL :: Comic -> IO String
imageURL c = do
  pg <- fetchURL (coURL c)
  let re = pg =~ (coRegex c) :: String
  case re of
    "" -> return (error $ "no match for regular expression for " ++ coName c)
    otherwise -> return (coPrefix c ++ re)
~~~~

One feature of I/O actions that distinguishes them from pure
calculations is the need to impose an ordering on the execution of
actions.  In a pure computation, order of evaluation does not affect
the result of the computation: the expression `a + b` yields the same
result, regardless of which of `a` or `b` is evaluated first.  Here,
we need to fetch the base page for the comic, and only then can we
match the regular expression that will give us the URL to get the
latest story image.  This sequencing is expressed using Haskell's `do`
notation[^1].  Three kinds of expressions can appear within a `do`
block:

1. Regular Haskell expressions;

2. `let` forms, which bind names to pure functional expressions;

3. Expressions of the form `name <- action`, which execute an I/O
   action and bind the result of the action to the given name.

Our `imageURL` function contains expressions of each of these three
types.  First, we use the `fetchURL` function (which returns an I/O
action of type `IO String`), executing the returned I/O action to
retrieve the comic base page, and binding the result to the name `pg`.
Subsequent lines in the `do` block can then refer to this value.  This
binding of names permits only a single assignment to a name: once `pg`
is bound here, it may not be rebound.  Next, we use a `let` expression
to do regular expression pattern matching on the comic base page.  The
`=~` operator from the `Text.Regex.Posix` module is used (we have to
give an explicit type annotation to resolve the ambiguity of the
return type here).  Finally, we use a simple Haskell `case` expression
to decide what to return from `imageURL` on the basis of the pattern
match.  Note that both branches of the `case` expression contain uses
of the `return` function, which has type `a -> IO a` and is used to
"inject" a value into the `IO` monad: it's just a way of turning a
regular `a` value into a value of type `IO a`, which is the return
type we need for `imageURL`.

## Retrieving images ##

We can now use out `imageURL` function as part of a function to
retrieve the latest image file for a given comic.  We'll call this
`writeImageToFile`:

~~~~ {.haskell}
writeImageToFile :: Comic -> IO ()
writeImageToFile c = do
  putStrLn $ "Retrieving: " ++ (coName c)
  url <- imageURL c
  img <- fetchURL url
  writeBinary (normaliseName $ coName c ++ takeExtension url) img
~~~~

The overall type of our function here is `Comic -> IO ()`, showing
that the function takes a `Comic` and returns an I/O action.  The `IO
()` return type means that the result of the I/O action is discarded:
we are only interested in the I/O action for its side effects.  Here
again, we see sequencing of I/O actions using `do`.  The first action
writes a message to the terminal using the `putStrLn` function in the
Haskell prelude (with type `String -> IO ()`).  Then we use our
`imageURL` function to download the base URL for the comic and extract
the URL of the latest image file.  Next, we use the `fetchURL`
function to download the contents of the image URL, binding the result
to the name `img`.  Finally, we write the contents of the image URL
into a file with a name built from the name of the comic using the
`normaliseName` function and the `takeExtension` function from the
standard `System.FilePath` library.  Here is the definition for
`normaliseName`:

~~~~ {.haskell}
normaliseName :: String -> String
normaliseName = map fix_spaces . map toLower
  where fix_spaces ' ' = '_'
        fix_spaces ch = ch
~~~~

We write the image data to the file using the `writeBinary` helper
function, which uses some `System.IO` library functions to write the
file as binary data (i.e. no text encoding, no newline conversion --
Haskell strings are written using UTF-8 encoding by default, which
will mess up the binary image data we're dealing with here):

~~~~ {.haskell}
writeBinary :: String -> String -> IO ()
writeBinary f s = do
  h <- openFile f WriteMode
  hSetBinaryMode h True
  hPutStr h s
  hClose h
~~~~

The sequencing provided by `do` allows us to write this sort of I/O
code in a very natural imperative-looking way.  The Haskell compiler
translates the `do` notation into pure code that threads the I/O state
through the calls, but we don't need to think about that to use the
`IO` monad.  We just need to remember two things:

1. To distiquish between pure functional expressions (which use `let`)
   and capturing the execution of I/O actions (which uses the `<-`
   notation);

2. That all assignments are immutable -- once we assign a value using
   `let` or `<-`, that name cannot be reassigned.

## Putting it together ##

Next, we want to take our `writeImageToFile` function and apply it to
the list of comics defined above.  We expect to do this with a
function with type `[Comic] -> IO ()`.  We might initially be tempted
to write something like `map writeImageToFile comics`, but this won't
work.  To see why, consider the types of `map` and `writeImageToFile`:

~~~~ {.haskell}
map :: (a -> b) -> [a] -> [b]
writeImageToFile :: Comic -> IO ()
~~~~

The type of `map writeImageToFile` would then be something like
`[Comic] -> [IO ()]`, which is not quite what we want.  Composition of
I/O actions should not result in a list of I/O actions, but in a
single action that performs the individual actions in sequence.  A
suitable mapping function that provides this sequencing functionality
is provided in the `mapM_` function in the `Control.Monad` library.
This library contains monadic equivalents of many common list
processing functions (map, folds, etc.).  Specialising to the `IO`
monad, since that's the only one we care about here, `mapM_` has type
`(a -> IO b) -> [a] -> IO ()` and applies a function with an `IO`
result to a list of values, returning an I/O action that discards its
result (which is what `IO ()` means).

We can then write a function to process all of our comics as:

~~~~ {.haskell}
processAll :: [Comic] -> IO ()
processAll cs = mapM_ writeImageToFile cs
~~~~

Typing `processAll comics` at the GHCi prompt results in the current
story image for each of the comics in our list being downloaded and
saved to a file.

## What next? ##

In Part 2, we'll extend what we've done to organise our downloaded
images a little, placing all the images we download into directories
with names based on the current date -- this will require us to do
some I/O to create the directories, but also to determine the date
(how could a `getCurrentDate` function always return the same value?
i.e. how could it be pure?).  We'll also write some code to read a
list of comics to download from an XML configuration file.


[haskell]: http://www.haskell.org/
[clojure]: http://clojure.org/
[comics1]: http://gnuvince.wordpress.com/2008/10/31/fetching-web-comics-with-clojure-part-1/
[comics2]: http://gnuvince.wordpress.com/2008/11/18/fetching-web-comics-with-clojure-part-2/
[clean]: http://clean.cs.ru.nl/
[mercury]: http://www.mercury.csse.unimelb.edu.au/
[cabal]: http://www.haskell.org/haskellwiki/Cabal-Install
[haskell-report]: http://haskell.org/onlinereport/exps.html#field-ops

[^1]: Expressions involving `do` are convenient syntactic sugar for
      expressions based on the monadic `>>=` operator.  We don't need
      to worry about this here.
