---
author: Ian
tags: haskell,programming
published: 2011-10-13 20:44:02
title: Haskell Comic Scraper: Part 2
---
[Last time][part1], we looked at the basics for a webcomic scraper.
In this post, we'll look at extending our code a little to incorporate
some date handling and some simple XML processing.

<!--MORE-->

The second version of the code is [here](ComicScraper2.hs).

## Date based output directories ##

The first thing we'll do is set things up so that the webcomic images
we download are stored in a directory whose name is taken from today's
date.  Date and time handling functions are in the `Data.Time` module,
so we import that:

~~~~ {.haskell}
import Data.Time
~~~~

Following our "does it always return the same value" heuristic, it
seems pretty clear that a function to get today's date has to live in
the `IO` monad, since it wouldn't be very useful for such a function
to return the same value every time it's called.  The `Date.Time`
module has a number of functions for dealing with dates and times and
timezones, mostly modelled on the Unix C library functions for doing
the same jobs.  We can get a representation of the current date (the
`Day` type is just a wrapper for an integer giving the [modified
Julian day][julian]) in the local timezone using the following code:

~~~~ {.haskell}
getToday :: IO Day
getToday = do
  tz <- getCurrentTimeZone
  tm <- getCurrentTime
  return (localDay $ utcToLocalTime tz tm)
~~~~

Here, both getting the current time zone and getting the current time
require IO actions (the current time zone can change from time to time
if the machine where our code is running is moved, for instance), and
obviously the current time changes with time...  All of the functions
`getCurrentTimeZone`, `getCurrentTime`, `localDay` and
`utcToLocalTime` are defined in the `Data.Time` module, whose
documentation you can read [here][datatime].

Once we have a date, we'd like to turn it into a good name to use for
a pathname.  This a simple manipulation of the result of `getToday`,
but because `getToday` is an IO action, our function to make the
pathname returns an IO action too:

~~~~ {.haskell}
makeDateName :: String -> IO String
makeDateName base = do
  date <- getToday
  return (base ++ "/" ++ showGregorian date)
~~~~

Here, the `showGregorian` function is a handy utility from `Data.Time`
that formats a date in ISO YYYY-MM-DD format, and we pass in a "base
directory" where all the webcomic download directories should go.

We can now modify the `processAll` function from last time.  We'll
need to pass in a base directory name (parameter `bd` in the function
below), plus a list of comics to get.  We can determine the path to
save the downloads to using `makeDateName`, and we then use the
`createDirectoryIfMissing` function from `System.Directory` to make
the relevant directory (the first `True` argument acts the same as the
`-p` flag to `mkdir`, making parent directories as required), then
change working directory to the new directory, then download the
files:

~~~~ {.haskell}
processAll :: String -> [Comic] -> IO ()
processAll bd cs = do
  dateDirectory <- makeDateName bd
  createDirectoryIfMissing True dateDirectory
  setCurrentDirectory dateDirectory
  mapM_ writeImageToFile cs
~~~~

As before, a lot of this looks just like what we would write in a
traditional imperative language.  The monadic structure of the code
that the Haskell compiler generates from a `do` statement deals with
all the details of routing the results of one step of the computation
to the next in a way that preserves referential transparency.


## An XML configuration file ##

Until now, we've been specifying the list of comics to read directly
in our code.  This isn't very convenient, so it would be good to read
the comic details from a configuration file.  We'll use a simple XML
file that we'll call `comics.xml`: an example is [here](comics.xml).

Haskell has a number of [very sophisticated libraries][haskxml] for
dealing with XML documents, but sometimes these are slight overkill.
If all you want to do is pull some information out of an XML file
without too much fuss, then the [TagSoup][tagsoup] package is what you
want.  This has a nice simple interface for reading XML (or HTML) that
may or may not be well-formed, from which you can extract the data
items you need.  We import the package as:

~~~~ {.haskell}
import Text.HTML.TagSoup
~~~~

and we use it as shown in the `getConfig` function, which reads the
contents of a configuration file and parses it using the `TagSoup`
`parseTags` function.  The result of this is a list of HTML/XML tags
with attached attributes that you can process using Haskell's usual
list processing functions.  Here, we first use `filter` to pick out
the entry in the tag list having an open tag of `baseDirectory`, from
which we extract the name attribute to use as our base directory.  If
that works, we pick out all the entries with an open tag of `comic`
and process them with the `makeComic` function, which simply pulls the
relevant items out of attributes in the comic tags.

~~~~ {.haskell}
getConfig :: FilePath -> IO (String, [Comic])
getConfig cfg_path = do
  fc <- readFile cfg_path
  let tags = parseTags fc
  let bdtags = filter (isTagOpenName "baseDirectory") tags
  case bdtags of
    [] -> return (error "baseDirectory field missing from comics.xml")
    otherwise -> do
      let bd = fromAttrib "name" $ head bdtags
      let cs = map makeComic $ filter (isTagOpenName "comic") tags
      return (bd, cs)
  where makeComic t = if n == "" || u == "" || r == "" || p == "" then
                         error "attribute missing in comic tag"
                         else Comic n u r p 
          where n = fromAttrib "name" t 
                u = fromAttrib "url" t
                r = fromAttrib "regex" t 
                p = fromAttrib "prefix" t
~~~~

While it's not up to more complex XML processing tasks that require
walking and transforming the tree of entries in an XML file, for this
type of application, `TagSoup` is just about perfect.  It's
lightweight, easy to use, and interfaces with standard ways of working
with lists in Haskell in a seamless way.

To make use of this, we just modify our main program to read the
configuration information, which we pass right along to `processAll`.

~~~~ {.haskell}
main :: IO ()
main = do
  (bd, cs) <- getConfig "comics.xml"
  processAll bd cs
~~~~

Simple!

[part1]: /blog/posts/2010/10/10/haskell-comic-scraper-1
[julian]: http://en.wikipedia.org/wiki/Julian_day
[datatime]: http://haskell.org/ghc/docs/7.0-latest/html/libraries/time-1.2.0.3/Data-Time.html
[haskxml]: http://en.wikibooks.org/wiki/Haskell/XML
[tagsoup]: http://hackage.haskell.org/package/tagsoup

