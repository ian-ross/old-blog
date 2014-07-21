---
author: Ian
tags: haskell
title: Using Images in Haddock Documentation on Hackage
published: 2014-02-21 15:53:28
---

A useful but little used feature of [Haddock][haddock] is the ability
to include inline images in Haddock pages.  Here are [a][eg1]
[few][eg2] [examples][eg3].  You can use images for diagrams or for
inserting mathematics into your documentation in a readable way.  In
order to use an image, you just put `<<`*path-to-image*`>>` in a
Haddock comment.  The image can be of any format that browsers
support: PNG, JPEG, SVG, whatever.

Until the most recent (1.18) version of Cabal, using this feature was
kind of a pain because there was no way to ensure that your image
files ended up in a reasonable place in the documentation tree.  That
meant either hosting the images somewhere other than Hackage, or
inserting the image data directly into your Haddock comments, which is
painful in the extreme.

Now though, Cabal has a new `extra-doc-files` option you can specify
in the Cabal file for your package, which lists files that should be
copied from your source tree into the documentation tarball.  That
means you can get your image files into just the right place with no
pain at all.

As an example, in the [`arb-fft`][arb-fft] package I use a couple of
SVG images to render some equations in the documentation.  In the
source tree there's a directory called `doc-formulae` that contains a
couple of bits of LaTeX and a Makefile that processes them and uses
`dvisvgm` to turn the resulting DVI output into SVG files.  The Cabal
file contains a line saying `extra-doc-files: doc-formulae/*.svg`
which ensures that the SVG images end up in the Haddock documentation
tarball.  I can then refer to them in Haddock comments as something
like `<<doc-formulae/fft-formula.svg>>`, which is really convenient.

This should now work for any new uploads to Hackage after a fix last
night by Duncan Coutts.

[haddock]: http://www.haskell.org/haddock/
[eg1]: http://hackage.haskell.org/package/lens
[eg2]: http://hackage.haskell.org/package/split-channel-0.2.0.1/docs/Control-Concurrent-Chan-Split.html#v:split
[eg3]: http://hackage.haskell.org/package/arb-fft-0.2.0.2/docs/Numeric-FFT.html
[arb-fft]: http://hackage.haskell.org/package/arb-fft
