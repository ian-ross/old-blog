---
author: Ian
tags: haskell
title: Getting down and dirty with Haskell
published: 2013-08-11 17:11:40
---

I've started using [c2hs][c2hs] recently, for a Haskell NetCDF library
I'm writing (which might see the light of day in a couple of months).
I like c2hs, so I volunteered to help out with maintenance.  The first
thing we did was transfer the source code repository from Darcs to
GitHub, which was easy, using Steve Purcell's
[darcs-to-git][darcs-to-git] utility.  Once that was done, the next
task was to transfer all the tickets on the c2hs Trac site to GitHub's
issue tracking.  That's what I want to talk about here.

I didn't have access to the Trac database, so this was going to be a
nasty hacky web-scraping kind of job.  Trac makes this easier by
having some nice semantic markup in its HTML pages for each issue, but
it was still going to be kind of ugly.  There was a time when this
kind of job would have made me reach for Perl, but these days, I'm
trying to do as much as possible in Haskell.  It ended up being fun
and I learnt some things (as well as getting the job done!).

The code is [here][trac-to-github].  I want to talk about three
different aspects of what I had to do: CSV file parsing, HTML
scraping, and communicating with the GitHub web API, all of which are
"bread and butter" activities that we'd hope might be supported by
libraries (they are).

## CSV files

Trac provides basic information about each issue as a CSV file that
you can pull from the Trac site with an HTTP request.  CSV is pretty
easy to parse, but why reinvent the wheel?  There are a number of
different options in Haskell, but I've become quite fond of
[`csv-conduit`][csv-conduit].  It interfaces with conduits if you need
that, but it also has convenience functions for simple uses: here, I
just needed to use the `parseCSV` function from
`Data.CSV.Conduit.Parser.Text`.  No muss, no fuss.

## HTML scraping

Parsing HTML isn't as easy as parsing CSV data, and I'd have avoided
it if I could, but I wanted to convert the comment history from each
Trac issue to comments on GitHub and I couldn't figure out a way to
get Trac comments in any other form than as fields on an HTML page.  I
also wanted to carry over information about attachments for Trac
issues to the GitHub issues, just providing a link back to the Trac
site so that people would still be able to see the attachments.

The go-to solution for HTML and XML parsing in Haskell is [HXT][hxt],
but this is what I would definitely categorise as a *scarey* package.
The list of modules on the contents page of the Haddock documentation
starts with 14 modules called `Control.Arrow.SomethingFrightening`,
which is never a good sign.  Fortunately, there is good documentation
available if you look around a bit: the
["gentle introduction"][gentle] is about as gentle as Haskell "gentle
introductions" ever get, but even better is the page
[Working With HTML In Haskell][html], written by the author of the
[HandsomeSoup][HandsomeSoup] package.  (HandsomeSoup adds a number of
features convenient for working with HTML to HXT.  In particular, it
allows you to use full CSS selector syntax for locating elements in
your document, which is a real boon.)

Once you get over the initial barrier to entry, HXT and HandsomeSoup
are really easy to use for the kind of page scraping I needed.  Here's
a little example: given the contents of a Trac HTML page for an issue
as a `Text` value (called `html` in the code below), let's get a list
of the URLs of attachments on the page -- these are held in a
definition list of class `attachments` in hyperlinks with the word
"View" in their title:

~~~~ {.haskell}
 let doc = parseHtml html
 attachUrls <- runX $
   doc >>> css "dl.attachments dt a[title~=View]" ! "href"
~~~~

This code (which runs in the `IO` monad here) parses the HTML into a
tree of XML elements, then uses HandsomeSoup's `css` combinator to
pull out the relevant elements we want (note the full CSS3 selector
syntax!), then extracts the `href` attribute from each of those
elements, giving us a list of URLs for the attachments.  HXT and
HandsomeSoup combinators use arrows for plumbing, hence the `>>>`
combinator, and the `runX` function is used to execute a chain of HXT
combinators to get a result.

This was about the simplest kind of thing I needed to do: I also had
to extract comments and convert the HTML markup to GitHub Markdown so
that I could insert the comments into GitHub.  This was a bit more
complicated, but the arrow pipeline processing model of HXT makes this
sort of thing as straightforward as it's ever going to be.  Here's how
I got the comments out:

~~~~ {.haskell}
 comments <- runX . xshow $
   doc >>> pres >>> ps >>> tts >>> css "div.comment" >>> getChildren
~~~~

Here, `pres`, `ps` and `tts` are arrows to convert various kinds of
HTML elements to equivalents or near equivalents in GitHub Markdown:

~~~~ {.haskell}
 pres = processTopDown
        ((getChildren >>> changeText backticks) `when` hasName "pre")
 backticks = ("\n\n```\n" ++) . (++ "\n```\n\n")
 ps = processTopDown (getChildren `when` (hasName "p"))
 tts = processTopDown
       ((getChildren >>> changeText backticks1) `when` hasName "tt")
 backticks1 = ("`" ++) . (++ "`")
~~~~

HXT's `processTopDown` function allows you to recursively transform
the elements of an XML document tree, and the `when` combinator allows
you to do this transformation selectively, depending on the kind of
element you're looking at.

There's a huge amount of stuff you can do with HXT, and I can see that
it's going to take a long time to learn even a fraction of it.
However, despite the initially forbidding facade, once you've played
with it a little, it's a lot less intimidating.

## GitHub API

Once I'd scrabbled and scraped my way to some data that looked
reasonable, I wanted to create issues on the `c2hs` GitHub
repository.  GitHub has a nice and well-documented web API for doing
most administrative actions for a repository, so this ought to be
easy.  What makes it even easier is that there is already a Haskell
package for talking to GitHub, called, logically enough,
[github][github].  When I originally looked at this, it didn't have
functions for creating and editing issues and comments, but all the
infrastructure was there, and it was easy to add the new
functionality.  It all worked fine and the new functions are now
incorporated in the latest version of the package.

It's really easy to use.  Here's a function that creates a new GitHub
issue from a Trac ticket (represented as a value of type
`TracTicket`).  You create a value of type `NewIssue` (which has
fields corresponding exactly to the fields in the JSON payload passed
to the GitHub API endpoint) and pass it to `createIssue` along with
some authentication information and the user name and repository name
for the repository you want to add the issue to.  You get back either
an error, or an `Issue` value constructed from GitHub's reply.  (Here,
I also edit the issue right after creating it if it needs it -- adding
labels or changing the issue state, for example).

~~~~ {.haskell}
newTick :: TracTicket -> [String] -> IO (Either Error Issue)
newTick tick dates = do
  let title = T.unpack $ ttSummary tick
      body = T.unpack $ ttDescription tick
      orig = origLine tick dates
      prio = priorityLine tick
      iss = (newIssue title) { newIssueBody = Just $ orig ++ prio ++ body }
  r <- createIssue ghAuth ghUser ghRepo iss
  case r of
    Left err -> return $ Left err
    Right riss -> do
      putStrLn $ "Issue #" ++ show (issueNumber riss) ++ " created"
      case neededit tick of
        False -> return $ Right riss
        True -> editIssue ghAuth ghUser ghRepo
                (issueNumber riss) (tickEdit tick)
~~~~

## Conclusion

I always find it kind of funny when people mention Haskell as being an
"academic" language or "too difficult to use for everyday things".
The Haskell ecosystem is now at a point where you really can do a lot
of those "everyday things" with very little effort, and the Haskell
solutions to problems like XML manipulation and other nasty but
necessary tasks often feel much more powerful (and safer) than the
untyped alternatives.  And, perhaps contrary to expectations, doing
these "scripting" tasks in a strongly-typed language feels really
natural.


[c2hs]: https://github.com/haskell/c2hs
[darcs-to-git]: https://github.com/purcell/darcs-to-git
[trac-to-github]: https://github.com/ian-ross/trac-to-github
[csv-conduit]: http://hackage.haskell.org/package/csv-conduit
[hxt]: http://hackage.haskell.org/package/hxt-9.3.1.1
[gentle]: http://www.haskell.org/haskellwiki/HXT
[html]: http://adit.io/posts/2012-04-14-working_with_HTML_in_haskell.html
[HandsomeSoup]: http://hackage.haskell.org/package/HandsomeSoup
[github]: http://hackage.haskell.org/package/github
