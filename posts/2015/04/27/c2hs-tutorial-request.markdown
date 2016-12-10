---
author: Ian
title: C2HS Tutorial Ideas
tags: haskell
published: 2015-04-27 15:46:42
---

One of the things that C2HS is lacking is a good tutorial.  So I'm
going to write one (or try to, anyway).

To make this as useful as possible, I'd like to base a large part of
the tutorial on a realistic case study of producing Haskell bindings
to a C library.  My current plan is to break the tutorial into three
parts: the basics, the case study and "everything else", for C2HS
features that don't get covered in the first two parts.  To make this
*even more* useful, I'd like to base the case study on a C library
that someone actually cares about and wants Haskell bindings for.

The requirements for the case study C library are:

1. There shouldn't already be Haskell bindings for it -- I don't want
   to duplicate work.

2. The C library should be "medium-sized": big enough to be realistic,
   not so big that it takes forever to write bindings.

3. The C library should be of medium complexity.  By this, I mean that
   it should have a range of different kinds of C functions,
   structures and things that need to be made accessible from Haskell.
   It shouldn't be completely trivial, and it should require a little
   thought to come up with good bindings.  On the other hand, it
   shouldn't be so unusual that the normal ways of using C2HS don't
   work.

4. Ideally it should be something that more than one person might want
   to use.

5. It needs to be a library that's available for Linux.  I don't have
   a Mac and I'm not that keen on doing something that's Windows-only.

Requirements #2 and #3 are kind of squishy, but it should be fairly
clear what's appropriate and what's not: any C library for which you
think development of Haskell bindings would make a good C2HS tutorial
case study is fair game.

If you have a library you think would be a good fit for this, drop me
an [email](mailto:ian@skybluetrades.net), leave a comment here or give
me a shout on IRC (I'm usually on `#haskell` as `iross` or `iross_` or
something like that).
