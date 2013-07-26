---
author: Ian
tags: haskell
title: In Soviet Russia, Boilerplate Scrap You
published: 2013-06-07 20:43:40
---

I've been working on [BayesHive][bh] with Tom Nielsen for something
like six months now, and it's been a lot of fun.  We've been ironing
bugs out of the back-end of the system, which implements a
probabilistic functional language (called Baysig).  We found an
entertaining thing the other day -- I raised the bug report, Tom found
out what was going on, and I laughed quite a lot when he told me.

We use the [Scrap Your Boilerplate][syb] generics framework for doing
various transformations of the AST of the Baysig language.  It's a
great way to define transformations of complex hierarchical data
structures in a nice compact way.  It does have a trap for the unwary
though...

Let's look at some Haskell.  We start with a language pragma so we can
derive instances of `Data` and `Typeable` in a minute, and we import
the main SYB generics module:

~~~~ {.haskell}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics
~~~~

We're going to look at a very simple example, where we represent the
types of identifiers using a simple association list.  Here's a first
attempt:

~~~~ {.haskell}
type Id1 = String
type Type1 = String
type AList1 = [(String, String)]

alist1 = [("f", "int"), ("g", "long"), ("h", "string")]
~~~~

We just define type aliases for the identifier and type, and we define
an example association list.  Now, suppose we want to be able to
rename identifiers.  We can write a function that's suitable for use
with SYB very simply:

~~~~ {.haskell}
rename1 :: String -> String -> (String -> String)
rename1 from to s
  | s == from = to
  | otherwise = s
~~~~

And here's how we use it -- `mkT` makes a generic transformer from a
function, and `everywhere` applies it everywhere on a data structure:

~~~~ {.haskell}
λ > everywhere (mkT (rename1 "f" "fnew")) alist1
[("fnew","int"),("g","long"),("h","string")]
~~~~

That looks fine!  Let's try another example:

~~~~ {.haskell}
λ > everywhere (mkT (rename1 "g" "gnew")) alist1
[("f","int"),("gnew","longnew"),("h","stringnew")]
~~~~

Uh-oh.  That's quite a bit less fine!  As well as renaming `g` to
`gnew`, we seem to have renamed `long` to `longnew` and `string` to
`stringnew`, which was definitely not what we intended.

What's happening here is, in retrospect, really obvious, but it's
quite a bit harder to spot once you get this sort of problem cropping
up in a large codebase.  SYB works by matching on *constructors*, and
when you say `everywhere`, SYB interprets "everywhere" really to mean
everywhere.  Given a string, which unfortunately is just a list of
characters, SYB will traverse *into* the string.  When we write `mkT
(rename1 "g" "gnew")`, we're thus producing a generic transformer that
will match any string whose **suffix** is `"g"`, not just exact
matches for `"g"`.

What's cute about this is that it is exactly correct behaviour, but
still very surprising until you think about it for two minutes.  It's
very easy to fall into the habit of thinking of strings in Haskell as
atomic values, but they're not.  They're just lists of characters,
built from `[]` and `(:)`, and SYB will happily traverse into them
just like it will into any other ADT.  (Of course, we shouldn't be
using `String` at all.  That's what `Text` is for.  But we've not got
round to going through the Baysig compiler code with the hot swift
sword of `Text`ification yet.)

So, how to fix it with the least pain?  Pretty obvious really.  Just
wrap our identifier and type types in a `newtype`:

~~~~ {.haskell}
newtype Id2 = Id String deriving (Eq, Show, Data, Typeable)
newtype Type2 = Type String deriving (Eq, Show, Data, Typeable)

alist2 = [(Id "f", Type "int"), (Id "g", Type "long"), (Id "h", Type "string")]
~~~~

The new constructors that this introduces allow us to write a renaming
function that does what we want:

~~~~ {.haskell}
rename2 :: String -> String -> (Id2 -> Id2)
rename2 from to s
  | s == (Id from) = (Id to)
  | otherwise = s
~~~~

Now, SYB doesn't traverse into the data structure past the `newtype`
wrapper, and our renaming happens correctly:

~~~~ {.haskell}
λ > everywhere (mkT (rename2 "f" "fnew")) alist2
[(Id "fnew",Type "int"),(Id "g",Type "long"),(Id "h",Type "string")]
λ > everywhere (mkT (rename2 "g" "gnew")) alist2
[(Id "f",Type "int"),(Id "gnew",Type "long"),(Id "h",Type "string")]
~~~~

In the end, quite a little thing.  But it did amuse me.


[bh]: http://www.bayeshive.com/
[syb]: http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/