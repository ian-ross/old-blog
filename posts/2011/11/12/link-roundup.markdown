---
author: Ian
published: 2011-11-12 09:50:33
title: Saturday Morning Linkfest
---
#### Haskell ####

[Stanford CS240h: Functional Systems in Haskell](http://www.scs.stanford.edu/11au-cs240h/)
<br>
Stanford is really pushing e-learning at the moment, what with the AI,
machine learning and database classes that the engineering department
is running.  This is another good course with content available
online.

[Reddit: What does your company use Haskell for?](http://www.reddit.com/r/haskell/comments/lfwlv/what_does_your_company_use_haskell_for_ill_go/)
<br>
What it says on the tin: quite a few people chime in.  Seems like
Haskell isn't so much of an "academic-only" language any more!

[Storage and Identification of Cabalized Packages](http://www.vex.net/~trebla/haskell/sicp.xhtml)
<br>
Very helpful guide to GHC package management from Albert Lai.

[Unused constraints in GHC](http://byorgey.wordpress.com/2011/11/05/wanted-ghc-feature-warn-about-unused-constraints/)
<br>
This week, we spotted an interesting thing in some of the
[diagrams](http://projects.haskell.org/diagrams/) code.  There are a
bunch of places where an earlier implementation of a feature required
certain type class constraints.  With a more recent implementation,
that requirement has now gone away, but the constraints remain in the
code.  That makes using these particular functions trickier than it
needs to be.  We'd like for the compiler to warn about these extra
un-needed constraints, since otherwise they just hang around like a
bad smell.  Brent describes it as "a nice project for someone wanting
to dig into hacking on GHC"...


#### The other culture ####

[Money](http://www.nakedcapitalism.com/2011/09/david-graeber-on-the-invention-of-money-%E2%80%93-notes-on-sex-adventure-monomaniacal-sociopathy-and-the-true-function-of-economics.html) (*via Crooked Timber*)
<br>
Where does it come from?  There's a nice little Just-So story that
money arose naturally out of barter economies as a natural consequence
of Immutable Economic Laws.  It's thus slightly embarrassing that
anthropologists haven't found any evidence at all for barter economies
of the required type.  Cue immense academic pissing contest, of
course.  Read it all.  It's good.


#### Cool stuff ####

[Visual 6502](http://visual6502.org/welcome.html)
<br>
Remember the 6502?  Come on, no need to be shy.  The *other* 8-bit
processor from the 1980s.  I was a Z80 boy myself, but I did a bit of
6502 assembler programming back in the day (for my A-Level computer
studies project, I wrote a little data capture package for an
infra-red spectrometer in our school's chemistry lab using the A/D
ports on a BBC Micro).  Any other fans of retro 8-bit should take a
look at this site.  They're building sub-gate-level simulations and
visualisations of old microprocessors.  The visualisations are very
neat, but the way they're doing it is the kicker -- working directly
from dies, they're photographing them, doing some image analysis to
get the patterns of the chip layers out and using these to build
transistor-level models of the chips.  Lots of fun!

[Circos](http://circos.ca/)
<br>
You may have seen those super sweet circular charts floating around in
some biology papers in Nature or Science.  This is where they're made,
and you can make them too.

[Quasicrystal animations](http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html)
<br>
This is "Haskell" too, but it lives in "Cool stuff".
