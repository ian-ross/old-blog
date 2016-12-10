---
author: Ian
title: Q1 2015 Review
published: 2015-04-05 12:20:04
---

I've started doing a new thing this year to try to help with "getting
things done".  I normally have a daily to-do list and a list of weekly
goals from which I derive my daily tasks, but I've also now started
having a list of quarterly goals to add another layer of structure.
Three months is a good timespan for medium-term planning, and it's
very handy to have that list of quarterly goals in front of you (I
printed it out and stuck it to the front of my computer so it's there
whenever I'm working).  Whatever you're doing, you can think "Is this
contributing to fulfilling one of my goals?"  and if the answer is
"No, watching funny cat videos is not among my goals for this
quarter", it can be a bit of a boost to get you back to work.

So, how did I do?  Not all that badly, although there were a couple of
things that fell by the wayside.

<!--MORE-->

### Paid work

For the last three months, I've been working on non-Haskell things.
An ex-colleague of mine in Bristol has a climate/biogeochemical model
called [GENIE](http://www.seao2.info/mycgenie.html) that he wants to
simplify and get set up for various future research and educational
purposes.  The model was developed by a sort of loose consortium and
had a bunch of "e-Science" money thrown at it by the UK research
councils, the result of which was something that was way more
complicated than it needed to be.

I've been working on simplifying and improving the model configuration
and build system, getting the thing working on Windows, doing various
tidying up tasks (converting everything from Fortran 77 to Fortran 90,
for instance) and am now working on optimising the model, starting
with changing the memory management from Fortran 77 statically
allocated arrays to Fortran 90 dynamically allocated things.  This
will hopefully reduce cache contention, make things go faster and also
incidentally simplify building the model, since at the moment we need
a different model build for each model configuration.  That means that
you need a Fortran compiler to use the model, so it's not possible to
produce a turnkey installer for Windows, which is something of a
requirement for educational use.  Once that's done, I'm going to
expand some GUI experiments I've been doing into a full configuration
and job management GUI for the model.

I've used this project as an opportunity to learn a couple of new
things.  The model configuration and build system is all written in
Python, using [SCons](http://scons.org) to manage builds.  I hadn't
written a single line of Python in my life before starting this, and
it's been quite a good way to learn it quickly!  I mostly chose Python
because I knew I'd need to make all this work on both Linux and
Windows, I knew that there was likely to be some cross-platform GUI
work, and I liked the look of SCons.  It's worked pretty well, and I
now have a good working knowledge of Python, although I don't like it
very much...  To be fair, there *are* a couple of good things about
Python: cross-platform support is really good, as is GUI stuff (using
Tkinter).  I've been writing things on Linux and mostly just having
them work on Windows with only minor changes.

It would be really nice if the ecosystem surrounding Haskell was as
mature as the Python ecosystem, because the combination of Haskell's
static typing (the number of stupid Python run-time errors that would
be caught at compile-time in Haskell is just shocking) and Python's
extensive standard library would be *great*.  There's been a bit of
talk recently about sprucing up the Haskell Platform, but there needs
to be a tighter binding between the "standard library" and the
compiler to achieve the kind of integration that Python has.  Getting
the Haskell community to agree to that would require some fairly
advanced cat-herding skills -- everyone likes the flexibility of using
whatever packages they want, without pinning things down to an agreed
set of "standard" package versions to give a real "batteries included"
solution.

### Other projects

Apart from working to keep Winnie in kibble and Rita in chocolate,
I've been trying to get some other projects done.  The first of these
was what I hubristically wrote down as "Zero-issue C2HS".  I think I
more or less got there -- I certainly cleared out all the issues on
the GitHub issue tracker, although there are a couple of big remaining
questions about what to do with C2HS next.

The other thing I wanted to get out of the way was the current series
of blog articles I've been writing about scientific data analysis in
Haskell.  I'd picked a topic I thought I could deal with relatively
quickly, but it turned into something of a monster.  I've now written
the last two articles and should post them this week, so I can tick
that off.  I have a smaller project of a similar kind to do next, and
someone else has (conveniently!) already written a lot of the code to
do it so I won't be starting from zero there.

Some other things I'd been hoping to do this quarter were however a
total washout.  I had plans to write prototypes for a couple of mobile
games and for an occupational therapy application, but I made no
progress on any of those things.  I need to work on scheduling my time
better to allow me to do some of these projects that aren't
immediately urgent.

### Courses and reading

I've been trying to follow a few MOOCs and other courses during this
quarter and I rather miraculously managed to finish ten (!) and I'm
half way through another.  Some of these were quick audits of the
course material, some were more detailed, with problem sets and/or
programming exercises (in particular, a couple of parallel programming
courses and a really good electronics course from MIT OpenCourseWare).
I find these courses a pretty effective way to learn new things, and
they offer a bit more structure than you get from studying from a
book.

Speaking of books, I only managed to read about half of the books I'd
planned to read.  Part of the reason for that was that one of those
books was a 1500 page molecular biology textbook that took me about
two months to wade through.  It was useful and interesting, but it
took a long time.

### What's up next

For Q2, paid work is going to be more of the same.  For "other
projects", I'm going to try to get to some of the things I didn't
manage in Q1, plus a couple of smaller Haskell projects (since my day
job is Fortran and Python for the moment, I need to get my Haskell fix
somewhere else).  For courses, I have a bunch of MIT courses I'd like
to follow, including one practical "build yourself a radar system"
electronics course (I need to price that up and work out how long it
would take before committing to it).  And for books, I have three or
four books about numerical methods for differential equations I want
to read as preparation for a possible future Haskell project, and I
finally want to get round to reading
[The Chaos Book](http://chaosbook.org/).

Better get started...