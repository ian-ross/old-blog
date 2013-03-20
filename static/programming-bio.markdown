<div class="grid_11">
# Programming bio

*WARNING: This is nearly 6000 words, and mostly written for my own
amusement and reminiscence-enablement.  Some of it is funny, though
not all that much, and not all that funny.  Read at own risk.*

## The early days (1982-1992)

#### School

The first computer I ever saw was a Sinclair ZX81 belonging to a
neighbour.  I remember writing Pacman-like games in **BASIC** in a
little notebook during breaks at school.  I don't think I ever even
typed most of them into a real computer!  The first computer I owned
was a Sinclair ZX Spectrum, which was the big thing in the UK at the
time.  **BASIC** again, though I did learn **Z80 assembler** using it,
and had a first play with **C** using a compiler from a company called
HiSOFT.  After a bit, I moved on to an Amstrad CPC machine, running
CP/M.  I wrote some little adventure games in **Z80 assembler**, using
CP/M's nice relocating macro assembler.

#### Sixth form

It wasn't until sixth form college that I got access to something more
substantial.  There was an old Honeywell "super-micro" the size of a
washing machine[^1] that had been "acquired" by the head of computer
science, along with four terminals, a proper mainframe-style
dot-matrix printer and a six foot high pile of documentation.  No-one
really knew what to do with it, so I got it going and had my first
joyous experience of system administration.  This thing ran an
operating system called GCOS, which was heavily influenced by Multics
and was pretty cool.  The only distressing feature from a systems
admin point of view was that it was so secure that it was possible to
put files into a state where no-one could read them and no-one could
delete them, not even the system administrator[^2].  I only did that
once...

We had a **Pascal** compiler for this beast and a friend of mine
implemented a simple **Lisp** interpreter using it for his A-Level
computing project.  I helped, making sure the washing machine kept the
whites white and the colours bright.

Meanwhile, my own A-Level project was a bit more gritty and
industrial.  The chemistry department had "acquired" (lots of
acquiring going on!) an infra-red photometer from a local company that
no longer needed it, and one of the chemistry teachers wanted to use
it in class, to try to show some more modern analytical techniques to
the students than "What colour is it?  What does it smell like?  No,
Jenkins, just a *little* sniff!  You're not huffing paint!".
Unfortunately, this thing recorded traces of spectra on paper rolls
that were expensive and hard to get.  The photometer had two
intriguing little wires coming out of it though, providing an analogue
representation of the transmissivity of the sample at different
wavelengths, along with some timing information.  At this time, the
most common computer in British schools was the BBC Micro.  The
designers of this thing had the genius idea of including an
analogue-to-digital converter as part of the standard build.  Combined
with the very clever assembler built into the **BBC BASIC**
interpreter, this made a platform that was ideal for all sorts of
laboratory applications.  So, adventures in **6502 assembler**
followed, as I wrote an application to collect, display and archive
infra-red spectra.  This was a really fun little project, involving
lots of cycle-counting to get sampling rates just right.  I don't know
how much use this thing saw in practice, but it kept me entertained
for a couple of weeks.

After sixth form, there was a bit of a programming gap, since my
physics undergraduate degree involved only a little bit of programming
(in **Pascal**, of all things, although this time on **Unix**).

## Coding for cash (1992-1997)

At the end of my physics degree, I knew I wanted to do some sort of
research, but didn't have a clue what.  So, I thought I'd go out and
get a job for a year or two before coming back for a PhD.  Good plan,
very poorly executed.  I was pretty strapped for cash at the time, and
quickly landed a job with a little company just outside Oxford that
wrote software for editing dictionaries.  Perhaps not the most
exciting application domain ever, but they were prepared to pay me
real actual money for writing code, mostly in **C**.  (They also had
some SNOBOL code in house, though I never touched that.)  I didn't
last long there, since I didn't feel like I was learning all that
much[^3], and I applied for a job at a company in London writing
software for a dealing room data distribution system for banks.

#### IISC: London & Paris

My abiding memory of the interview there was walking out afterwards
and having Duncan, who would be a long-term colleague, tell me "Ask
them for more money!".  So I did, and I got the job.  I spent a year
and a bit at IISC (Investment Intelligence Systems Corporation), first
working on a news quality monitoring system for Reuters, which was a
thing written in Borland **C++** that was supposed to sit in Reuters
data centres and collect timing information about the propagation of
news stories through their network, sending information over an X.25
link to a VAX somewhere, where statistics would be calculated.  This
was pretty much pre-internet, so the Reuters newsfeeds came in over
phone lines as RS-232 or something equally entertaining.  I spent many
happy hours swearing at an RS-232 protocol analyser in the dusty
cupboard in our offices where the Reuters line came into our building.

The Reuters thing was just a sideline though.  The main product we
worked on was a dealing room data distribution system.  This was a
distributed system (a big deal back then) that took data from multiple
stock market or data supplier feeds, multiplexed it and made it
available to traders at their desks.  This stuff was all running on
IBM RS/6000 machines, using IBM's almost-Unix-but-not-quite, AIX[^4].
The core code was mostly written in **C**, with some **APL**[^5], but
I did some "experimental" work to try to build an **SNMP** agent for
our system in **C++**.  The AIX **C++** compiler at the time was
"immature", at best, which meant that effort stalled pretty quickly.

One of the main clients for our system was Banque Nationale de Paris,
and the boss of our company decided it would be good to have some
people on the ground in Paris.  Duncan and me, his trusty sidekick,
were duly dispatched to the lion's den of the main BNP test centre in
Paris, where we lived for about nine months, with the constant promise
of our own office and occasional fly-by booze-up visits from head
office to keep us sweet.  This was my first experience of working at a
client site, and it was dire.  The BNP people didn't trust us, the
dealers and the bank bigwigs were in an office just round the corner
and prone to fly off the handle at any software failure, and for sure,
we got the fall-out.  We pulled a lot of all-nighters trying to get
things working, and we had a couple of inglorious moments: debugging
the latest install at 3 a.m. in the basement of the main BNP dealing
room while our main BNP contact wrung his hands watching us shouting
at each other; causing a full-scale crash of the whole BNP dealing
room when we pushed a release with a seemingly innocuous change made
by someone in London (which made all instances of our Curses-based
front-end display all text in black letters on a black background...).

Two events stick out in my mind.  The first was a bit of silly pissing
around when the head of development was over from London.  After a
"heavy" lunch, we were all sitting there, I was doing some system
admin clean-up on one machine and the others were sitting next to me,
kibitzing.  In a root shell, I typed "`/bin/rm -fr *`", and the wine
from lunch made me fail to notice I was in the root directory of the
machine.  "No!  NO!  Shit, shit, shit, NOOOO!".  By the time my
frantic mashing of the keyboard had stopped the wholesale deletion
that was going on, I had essentially bricked the machine.  Still,
that's what OS install tapes are for.  (Oh, those were the days.)  The
install was going fine, just chugging along, but I was watching it
carefully.  I'd already screwed up once, and I didn't fancy anything
else going wrong.  But then weird messages started appearing on the
console.  I can't remember exactly what they said, but they started as
"slightly anomalous" and quickly moved into "what the hell?!"
territory.  Duncan and Phil were working away at the next machine, and
I didn't want to disturb them, but I was getting antsier and antsier
about this and muttering quietly to myself.  The two others eventually
broke down into fits of giggles.  Apparently, AIX activated its
network connection at some point during the OS install, allowing Phil
to log in over the network and start piping silly messages to the
system console.  Bastard.  The big bang and the howling over the phone
from London when Phil spilled his coffee into the power supply of his
desktop RS/6000 sort of made up for it a little while later though.

The second incident was a bit more "serious", or at least
work-oriented.  One of the stock market feeds we had to deal was a
French feed called TOPVAL, which carried price information for all the
stocks on the main French exchange, including those used to form the
CAC40 index.  BNP, being the Banque *Nationale* de Paris, was supposed
to be one of the main market makers for these stocks.  Unfortunately,
the TOPVAL feeds were more or less unmitigated crap, falling over
completely a couple of times a day.  The main dealing room had a
couple of separate feeds going in, but our feed handlers were not
properly fault-tolerant, so there wasn't any real mechanism to recover
from failure of one feed.  Our eternal nemesis at the BNP test centre
was a guy called Didier, who smirked his way over to us at least once
a day with some new problem, smiling that he had broken our code *once
again*.  He was a nice guy, but he definitely enjoyed his job a little
too much.  This TOPVAL thing had got to the point where people at IISC
in London and near the top in BNP were starting to hear about it,
which meant we really need to fix it.  So I was tasked with making it
all nice again.  I invented a fault-tolerant recovery protocol so
that, no matter what happened with the feeds going up and down or
delivering values in a weird order, the feed handlers would be able to
agree on a consistent view of the world between themselves and
communicate values so that a failed feed could recover from a working
feed.  This took a bit of thinking, and was basically an *ad hoc*
solution I pulled out of the air, but I got it working and handed it
over to Didier for testing.  All was quiet for three or four days,
until Didier came to see me, looking quite glum.  "I cannot break it.
I have tried and tried.  But I think it is perfect."  Result!  No more
TOPVAL-related whining.

The full-time residence at BNP wore both Duncan and me out, and the
boss's eternal reassurances that he was "really close" to securing a
deal on an office for us didn't seem to be going anywhere.  We both
bailed out of IISC and left Paris, to move to a company called
Teknekron in London (now called TIBCO, I think).

#### Teknekron

Life at Teknekron was a lot lower-key than at IISC, even though there
was some on-site work.  That was mostly back-office stuff and in a
relatively early stage of development, so there was no-one to shout at
us, and lots of meetings.  I also worked on a distributed transaction
processing infrastructure, which was supposed to be something like
what RabbitMQ or ZeroMQ are now.  I remember writing a large
**Motif**-based **X** application for administering the
infrastructure: painful.  Most of the work was in **C++**, but I also
did a bit of **Lisp** programming, using a proprietary dialect, and I
had my first exposure to **Haskell**, reading the language report
while I was supposed to be thinking about other things, and wondering
how it was actually possible to compile this amazing-sounding language
into something executable.

My tenure at Teknekron lasted a little less than a year, and was
brought to an end when me and my girlfriend at the time had the great
idea of moving to Japan, where she would teach English in a school and
I would do, well, whatever.  The Teknekron people were positive about
being able to find me a place in their Tokyo office, but the salaries
they were talking about didn't sound very realistic for living in a
place as expensive as Japan.  Fortunately, one of the people in our
development team had a girlfriend who was a recruitment consultant in
the City.  I asked her if she thought she'd be able to find me a job
in Japan.  A week later, I made my way to Canary Wharf for an
interview in the Credit Suisse building, and not too long after that,
we were getting on a plane to Japan.

#### CSFP & UBS: Tokyo & New York

The job in Japan (at Credit Suisse Financial Products, which was
Credit Suisse's derivatives trading wing) was a front-line dealing
room support position, which meant a lot of system admin, a lot of
hacking at **Lotus 1-2-3** spreadsheets, which was what all the
traders used for their pricing calculations, and a lot of being
shouted at by irate traders when things went wrong.  I don't think I
fitted in there very well: I nearly got fired for trying to arrange a
transfer to the "Product Development" team (the quants), I got in
trouble for developing and deploying stuff[^6] to help the traders in
Tokyo without consulting with London, etc., etc.  From a tech point of
view, my work was pretty dull (hence the dedicated attempts to get in
trouble).  I spent Christmas Day 1995 in the dealing room with two
guys from London upgrading all of the traders' desktop Sun machines.
There were about 200 of them, all of which needed memory upgrades.  We
went out and got quite drunk when we were done.

Working at CSFP was pretty stressful and I decided to move on shortly
after the "No, you can't move departments!" incident (which, in
retrospect, I could have handled *much* better).  Duncan (from IISC)
was leading a team at UBS in New York to develop a new fixed-income
analytics library, and he was good enough to give me a job.  That
involved **C++** and **SQL** for the main part of the job.

## Science! (1997-2001)

#### Part III Maths: Cambridge
After a working break of five years (four more than I was planning!),
I returned to academia in October 1997, to do Part III Maths at
Cambridge.  I had all these great plans to do a PhD in particle
physics theory and had spent all summer reading to get myself back up
to speed before going to Cambridge.  The Part III course is pretty
intense, with a real "sink or swim" attitude, and I didn't have much
time for recreational programming.  I did finally learn how to use
**LaTeX** properly, using it to typeset notes for a couple of courses
with some fairly complicated requirements (lots of Feynman diagrams in
the quantum field theory...).

#### PhD attempt #1: Oxford

My plans changed a bit once I got some idea of the sort of work I
might be doing as a particle theory PhD student in Cambridge, and I
got a place in Oxford instead, in Atmospheric, Oceanic and Planetary
Physics (AOPP).  This was proper scientific programming.  Lots of
**Fortran**, including a line-by-line radiative transfer code whose
main loop was so complicated (for efficiency reasons) that everyone
was too scared to touch it, even the people who originally wrote it.
I spent the first year of my time at AOPP working on retrieval code
for a radiometer of an unusual design that was on its way to Mars.  We
hoped to be able to measure water vapour in the atmosphere, among
other things.  There was an existing "production" retrieval code
written by some people at JPL, but it was relatively limited in
application and used a retrieval method that the Oxford people didn't
like very much.  So I set out to implement a new retrieval algorithm
in **C++**.  This was all going pretty well until September 1999, when
the Mars Climate Orbiter carrying our instrument was supposed to enter
orbit around Mars.  Instead, it crashed into the surface.  And yes,
this was the "metric mixup" one, although there were cultural and
communication issues in one of the spacecraft contractors that were
probably the ultimate cause of the failure.

A sensible person would have stuck with the project I was working on,
generated some synthetic data using a forward model of the instrument
and results from the Mars general circulation model that was being
developed at AOPP at the time, finished the retrieval code, written my
PhD thesis and been done and doctored in the allotted time.  Not being
a sensible person, and being surrounded by glum people, all a bit
annoyed at losing this instrument for the second time (an earlier
version of the thing had been on another orbiter that was lost), I
took a rather silly decision, to switch projects, switch supervisors,
and to do something I had no experience for, which was to try to
develop hardware and associated electronics and software for a
prototype optical delay line for a space mission to detect
gravitational waves using laser interferometry.  I spent two years
learning all sorts of things about metalworking and workshop work,
electronics design, including some **FPGA** stuff, did a bit of
optical design (using **Mathematica**), learnt about **Forth**,
optical heterodyning and precision interferometric measurement, **DSP
assembler** programming, and sundry other things.  At the end of two
years, I was much more knowledgeable than before, but had nothing to
show for it in terms of material to turn into a PhD thesis.

## Back in the world (2001-2004)

After the emotional trauma of disengaging from my failed PhD attempt,
I needed to find a job quickly (one of the main reasons for leaving
AOPP was lack of funds).  I got a job with a small software company in
Oxford, developing software for managing architectural drawings for
building companies, in **C++**.  I was mostly hired for my networking
experience, and was told I would be in the middle of the range of
experience of the team.  It turned out I was the only person in the
team with any real networking experience, and that the code base that
they had was really pretty bad (including a bad C++ implementation of
something that looked very much like a broken version of the TCP
congestion control mechanism, which they then layered on top of a
regular TCP socket connection...).  It became pretty clear pretty
quickly that this company was going to fail and I ended up doing
"people work" during the wind-up process, which I didn't enjoy *at
all*.  I did learn one lesson there though, which is to be much more
careful about understanding the position you'll be going into when you
join a new company -- are the people involved really as good as they
think? do they really have the necessary experience (or raw
intelligence) to do what they're trying to do?  If you can't be sure
about those things, you can end up in a world of pain.

My next, less disastrous, gig was with an aerospace company in
Somerset, who hired me because of my work in Oxford and my programming
background.  These people were a much more professional outfit.  I did
a bit of **C++** programming here, a bit of **Matlab** and
**Mathematica** stuff, but spent most of my time working on
performance calculations for a naval sonar system ("aerospace", they
said, but submarines orbit the planet a bit slower than most
satellites, I think).  This wasn't really what I was hired to do, and
I was less than keen about working on defence stuff[^7].  I did learn
quite a bit about ocean acoustics, and wrote a long report
recommending the way forward for development of synthetic acoustic
environments for testing sonar system designs, which would have been
fun to develop.  I don't know if that ever went anywhere -- I left,
both because of the defence issue, and for personal reasons.

After SEA, I spent a few months living in Aberystwyth, where I did
some teaching, some **AutoLISP** coding for a local renewable energy
company (a little parameterisable wind turbine model for doing
visibility and impact assessments in AutoCAD), and tried to sell the
local arts centre on the idea of letting me redo their website and
booking system using a **Scheme** web-compositing system I was
developing.  The arts centre didn't bite, but they paid me a bit of
money (for my chutzpah, I think).

Eventually, a better opportunity came along in the form of a startup
being led by an old school friend (he of the Lisp interpreter A-Level
project).  I think I was employee number three here.  The idea was to
build new, more language-aware, development tools.  This started out
as being mostly language-agnostic, but ended up, for reasons of
familiarity and tractability mostly being about **Java**.  I wrote
some interesting things in **Scheme** as well though, in particular
some code for persistent representation of syntax trees.  I wish I'd
kept at that, since those kinds of ideas are at the basis of the
persistent data structures now used in Haskell and Clojure (and
starting to be used in other places too).  I also did a bit of
**Haskell** stuff here, mostly for small experiments.

We managed to get some angel investor funding, hired some more people
(including Julian, someone I'd worked with in Oxford before), and we
seemed to be pretty set.  There were a lot of good ideas floating
around and we had quite a buzz going about the project.  Things didn't
really pan out though.  I don't know if it was because we were
initially too ambitious, or that we just all had different ideas about
the ultimate goal, so were all pulling in different directions, but in
the end, things dissolved a little unpleasantly, and there were bad
feelings all round.  It was a real shame, as there really was
something there.

After that, I did a bit of contracting work for a company producing
specialised **FPGA**-like hardware and the associated synthesis and
place-and-route software (written in **C++**).  My work here was
mostly an attempt to bring a more scientific and systematic approach
to the assessment of the performance of the place-and-route tools,
which used a simulated annealing method to find good solutions.  The
hardware was quite heterogeneous, with a lot of specialised units,
which made the place-and-route tricky and quite variable in its
performance.  I did a bunch of experiments to try to come up with some
systematic performance measurements for the software.  It was pretty
interesting, and something quite new for me.

## Academia again (2004-2012)

#### Research Assistant: Bristol

About this time, my new girlfriend Rita (who had just started a PhD)
told me about a possible research job in a group associated with the
one she worked in.  I applied, got the job, and so moved back into
academia.

This was another new thing for me, since the job involved running
paleoclimate simulations with a large atmosphere-ocean general
circulation model coupled to a dynamic vegetation model.  The GCM was
in a mixture of **Fortran** and **C++**, using **MPI** for
parallelisation on a Linux cluster, and the vegetation model was LPJ,
something I went on to use a lot.  There was some fun and games
getting this thing working on our cluster, since the model had quite a
few bugs that only popped up with some Fortran compilers and some
initial conditions.  I got to hate this one error message, where the
semi-Lagrangian transport scheme for water vapour in the atmosphere
model detected wind speeds that were unrealistic and so stopped.
After a *long* time spent tracing this down, it turned out to have
nothing to do with the semi-Lagrangian transport, nothing to do with
the winds, nothing to do with the atmosphere at all, but was due to
NaNs being produced under certain conditions in the code in the
vegetation model that calculated surface evaporation.  These NaNs
propagated into the atmosphere model and buggered everything up.  All
in all, it was a pretty delicate piece of software, and took a lot of
massaging to get it working satisfactorily.

As well as the main work on the GCM simulations, I also wrote some
**Fortran 95** post-processing code to take results from the model
(and another model being used by someone else in the group) and
transform them into the archival format required by the EU project we
were working on.  This turned out to be fairly non-trivial!

I also did a *lot* of **Perl** scripting, for preparing initial and
boundary conditions for the model, and a lot of **NCL** scripting for
plotting results (including a C++ program driving NCL to plot hundreds
of plots for different models from the project database for a
benchmarking exercise).

#### PhD attempt #2: Bristol

As the Research Assistant contract I was on was coming to an end, it
seemed like the natural thing to do would be to apply for a PhD place.
I was ready for another go, having almost forgotten the trauma of my
AOPP days.  So, I wrote a proposal for some funding, applied for a PhD
place, got in, and spent the next three years doing a whole bunch of
new and even weirder stuff.

To start with, my main goal was to try to apply some methods from
dynamical systems theory to the analysis of climate models.  There had
been a whole series of papers on this stuff for ocean models, from
Henk Dijkstra's group in Holland, and I was thinking about how I might
apply some of this stuff to atmosphere models.  This turned out not to
be practical, mostly because of the absence of good distinguished
low-dimensional states in atmosphere models to start a numerical
continuation from, but I learnt a lot trying to do this.  My two
supervisors (Paul Valdes, the head of our climate modelling group in
the Bristol geography department, and Steve Wiggins, at that time head
of the maths department) mostly let me get on with it, working on the
basis that I was doing *something*, and it might end up going
somewhere interesting.

I spent a lot of time writing **C++** code to do numerical
continuation of solutions to computer models of dynamical systems,
using some of the libraries from Sandia's **Trilinos** project.  Along
the way, I also did a fair bit of **Mathematica** stuff for
experimentation, mostly driven by things Steve pointed me at.  I also
continued with some vegetation modelling work using the LPJ model, did
lots of data visualisation using **NCL** and **POV-Ray**, and set up
and administered a wiki for our research group, which involved a bit
of **PHP** hacking.

After I realised that my atmospheric model numerical continuation
plans weren't going to work, I spent a bit of time thinking about
whether or not I could apply the same kinds of methods to ice sheet
models -- there was a big group in Bristol that did ice sheet
modelling, their main 3-D model was pretty well-written and
documented, and ice sheet models were known to exhibit the kind of
multiple equilibria that numerical continuation can be used to
explore.  Seemed like the methods I was developing would be a good
fit.  Unfortunately, I ran up against a little problem: in more or
less all realistic ice sheet models, the boundary of the ice sheet is
dynamic.  As more snow falls on the ice sheet, the sheet gets bigger
and its boundary extends; as ice melts, the sheet shrinks and its
boundary contracts.  Mathematically, this is usually expressed as
something called an *obstacle problem*.  Although these aren't
insurmountably tricky, as far as I could find out, no-one had
developed numerical continuation methods for equilibria of these
problems at the time.  I found some theoretical papers and thought it
might be possible to implement the methods they described, but it
would have been a bit of a gamble, and time was getting short.

In parallel with the dynamical systems work, I had also been
developing a library of dimensionality reduction and computational
geometry code in **C++**, implementing methods like Isomap, nonlinear
PCA and Hessian locally linear embedding.  I decided that the best bet
for producing a [reasonable thesis](http://arxiv.org/abs/0901.0537)
within the allotted three years was to use this code to do some
analyses on climate model output.  It wasn't the most exciting thing
ever, but it got the job done, and got me my PhD.

#### Post-docs: Canada & France

After my PhD, Rita and me both got post-doc positions in Canada at the
University of Victoria.  Here I was employed in the maths department
to work on stochastic models of tropical convection to be used as
parameterisations in atmospheric general circulation models.  All
**Fortran** again, big codes, lots of **MPI**.  Although the
stochastic modelling aspect of the work was really interesting, I
ended up having to do a *lot* of numerical analysis in this job, which
is not something that I know very much about, not something that I
enjoy very much, and not something that I'm very good at.  My boss was
really into it, and that not-quite-overlap in interests led to a bit
of miscommunication and a post-doc that wasn't as productive as it
could have been.  I had some ideas and some work that was going quite
well on stochastic models of interactions between clouds, but that got
put aside in favour of detailed work on the numerics of an existing
convective parameterisation that I really made no decent headway on at
all.  All a bit disappointing, but I learnt a lot.

After the end of my post-doc, we stayed on in Canada for a while, and
I worked at NEPTUNE Canada, an ocean observation project that's based
at the University of Victoria.  There, my job title was *Scientific
Data Specialist*, which meant that I was supposed to make sure things
worked nicely for the scientists trying to use the project's data, and
worked on the development of new data products.  I mostly concentrated
on sonar and hydrophone data.  There were a lot of problems with the
way things were done at NEPTUNE at this time, with what was ostensibly
a science project not being very clearly focused on the needs of the
scientists.  There was a big pile of **Java** code that had been
written for data collection and archiving, and some of the design
decisions that had been locked in very early on didn't make a whole
lot of sense for the kind of sophisticated sensors that people were
wanting to deploy in the system.  I tried to bypass some of the slow
development of new software by writing **C++** code to process raw
sonar instrument output into formats usable by the scientists, and had
some success with this.  It was actually kind of fun to process some
of this stuff and know that you were the first person to visualise
this data that had been pulled out of some instrument on the bottom of
the Pacific!

The management aspects of NEPTUNE got on my nerves though, and when
Rita found a likely looking pair of post-doc positions in the south of
France, we decided to leave Canada and come back to Europe.  The
post-docs were both in Montpellier.  The fact that I quit and left
before the end of my two-year contract is some indication of how
acceptable the setup was.  I was mostly doing data analysis and
statistical modelling in **R** and **ArcGIS**, which was fine as far
as it went, but I didn't have a lot in common with most of the team,
the atmosphere was bureaucratically quite stifling, and I never really
felt comfortable there.

## Recent interests

Which brings us right up to date: after quitting the post-doc in
France, we moved here to Austria, where Rita is in school studying to
become an occupational therapist and I am working as a freelancer.

I've currently been focusing most of my programming time on Haskell.
I feel as though I learn things when I write Haskell, and it forces me
to think in new ways.  It's fun.  And the weird thing is that even
though it initially felt strange, it's quite easy to get things done.
I've been playing with **Yesod**, a Haskell web framework, both as
Haskell exercise and as a way of learning some more about web
programming, and it's really great.

I currently have two clients I'm working with.  One is my old research
group in Bristol, for whom I'm writing data preparation tools (in
**Fortran**, **C++** and, of all things, **Tcl/Tk**!) for the work
they do with the UK Met Office Hadley Centre climate model.  The other
is a start-up company that's developing tools to make Bayesian
statistical methods (in particular Markov chain Monte Carlo for
complex models) accessible to more scientists.  This is nominally a
**Haskell** contract, with a backend based on **Yesod**, but I've also
been doing a lot of **JavaScript** work for the front end, including a
data visualisation library that works with **AngularJS** -- that's
still at a relatively early stage of development, but I think it's
quite neat and might be something that could be pretty useful.

In my totally inadequate spare time between paid work, I'm going to
try to "Haskellise" some of the huge pile of unfinished and nascent
ideas that I have lying around.  Should be fun!


[^1]: Computers have got smaller over the years.  Washing machines,
      not so much.
[^2]: Perhaps it was for the best that the very nice system admin at
      the University of Bristol refused to grant us access to their
      Multics mainframe, no matter how keen we were and no matter how
      much experience we'd had mauling our GCOS system...
[^3]: One of the people there did point me at Fred Brooks' *The
      Mythical Man-Month*, which I still have on my bookshelf and
      consult regularly.
[^4]: Looks like Unix, feels like MVS.  I think it was one of the
      first Unix-a-likes to introduce a memory-exhaustion process
      killer, which was fun.  It also had a journaled file system,
      which meant there wasn't time to go out for lunch while
      rebooting after a power failure.
[^5]: Yes, I really have been paid actual cash money for programming
      in APL.  We spent a whole afternoon once debugging one line of
      APL, and fixed it by changing one character.  That's hardcore.
[^6]: In particular, a little **X** application for network-aware
      post-it notes, so that traders could slap little stickies on
      each others' screens -- more direct and harder to miss than
      email, which they liked.
[^7]: My main manager tried to reassure me that it was OK: "We don't
      make anything that actually goes bang!" he told me...

</div>

<div class="grid_4">
<div id="toc"><p><strong>Contents</strong></p>
<p><a class="toc_a" target="_self" href="#the-early-days-1982-1992">The early days (1982-1992)</a></p>
<p><a class="toc_a" target="_self" href="#coding-for-cash-1992-1997">Coding for cash (1992-1997)</a></p>
<p><a class="toc_a" target="_self" href="#science-1997-2001">Science! (1997-2001)</a></p>
<p><a class="toc_a" target="_self" href="#back-in-the-world-2001-2004">Back in the world (2001-2004)</a></p>
<p><a class="toc_a" target="_self" href="#academia-again-2004-2012">Academia again (2004-2012)</a></p>
<p><a class="toc_a" target="_self" href="#recent-interests">Recent interests</a></p>
</div>
</div>
