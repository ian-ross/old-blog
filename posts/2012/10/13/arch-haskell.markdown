---
author: Ian
tags: haskell
published: 2012-10-13 19:57:27
title: Haskell on Arch Linux
---
So, [GHC 7.6.1][ghc] was released last month.  And there was a
[new release of Cabal][cabal] too, bumping the version number to 1.16
from 0.14.  The [Arch Haskell][arch-haskell] people did a great job of
quickly producing Arch packages for the new version, which meant that
everyone pulled the new versions when they did an update via `pacman
-Su`.  Unfortunately, there seems to be a [problem][bug-report].  The
new `cabal-install` version can't read its own default configuration
file and just fails more or less immediately.  There seems to be a
fix, but it's not yet percolated into the Arch packages.

In the meantime, the [Arch Rollback Machine][arm] is your friend.  You
can find the last known good packages there ([ghc 7.4.2][arm-ghc-742]
and [cabal-install 0.14.0-2][arm-cabal-14] -- links for x86_64).  You
can make a working Haskell development environment by removing the
latest `ghc` and `cabal-install` packages, downloading the last known
good packages from ARM and installing them using `pacman -U`.  You
need to make sure that these packages subsequently don't get updated
by passing the `--ignore ghc,cabal-install` flags to `pacman` when you
do an update.

Once you have a good GHC and `cabal-install` installation, I would
strongly recommend using [hsenv][hsenv] to do Haskell development
within a sandbox -- you don't install any Haskell packages to either
the global or user-local package database, but use a project-specific
sandboxed package database.  This more or less eliminates
[Cabal dependency hell][hell].  I no longer bellow like a wounded
walrus when trying to install [Yesod][yesod], frightening the dog and
any other small mammals within a distance of two or three miles.  I
just create a `hsenv` sandbox, activate it, type `cabal install
yesod-platform`, and watch in glee as everything installs without a
hitch.


[ghc]: http://www.haskell.org/ghc/download_ghc_7_6_1
[cabal]: http://hackage.haskell.org/package/cabal-install
[arch-haskell]: https://archhaskell.wordpress.com/
[bug-report]: https://bugs.archlinux.org/task/31864?project=1&cat%5B0%5D=2&string=cabal-install
[arm]: http://arm.konnichi.com/search/
[arm-ghc-742]: http://arm.konnichi.com/extra/os/x86_64/ghc-7.4.2-1-x86_64.pkg.tar.xz
[arm-cabal-14]: http://arm.konnichi.com/extra/os/x86_64/cabal-install-0.14.0-2-x86_64.pkg.tar.xz
[hsenv]: https://github.com/Paczesiowa/hsenv
[hell]: http://www.yesodweb.com/blog/2012/03/cabal-nirvana
[yesod]: http://www.yesodweb.com/
