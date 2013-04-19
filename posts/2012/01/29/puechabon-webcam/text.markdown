---
author: Ian
tags: day-job
published: 2012-01-29 17:21:32
title: Puéchabon webcam
---
<div class="img-right">![Webcam installation](installation.jpg)</div>

One of the things I'm responsible for in my day job is a phenology
webcam at our [experimental site at Puéchabon][site].  The idea of
this is to observe colour changes in the canopy of the forest up
there, with a view perhaps eventually to replacing manual phenological
observations with information drawn from digital photos.  In this
sense, *phenology* means things like when flowers come out, when fruit
forms, and so on.  The [*Quercus ilex*][qi] (holm oak) forest at
Puéchabon is evergreen, so you don't get the same spectacular seasonal
changes in leaf colour that you see in deciduous forests, so we're not
sure whether this is going to work -- the changes we'll be looking at
will be a bit more subtle.

We installed the webcam on the 20 metre tower at Puéchabon in July and
have been collecting images every half hour since, apart from a few
outages.  The images aren't all that exciting (below left), but it's
quite nice to watch the changing angle of the sun on the canopy
throughout the day.  Very preliminary data analysis indicates that we
will at least be able to see *something* changing through the seasons,
even if it's not abundantly clear to the eye from the images.  A
recent paper by [Sonnentag et al.][sonnentag] (unfortunately behind
the evil Elsevier paywall...) gives some ideas about how to analyse
these images.  In the plot below right is a time series of the mean
*chromatic green* component of the part of the webcam field of view
that covers the forest canopy.  This is defined as $g_{cc} = G / (R +
G + B)$, where $R$, $G$ and $B$ are the red, green and blue components
of the digital image.  There is definitely some seasonal variation,
although with a lot of noise.  I want a full year of data, so that we
can see the new growth in the spring, before doing some more detailed
analysis, since there are definitely still some scene illumination
artefacts in what we're seeing.  It's encouraging that we can at least
see something though!

<div class="img2-left">
  <a href="webcam-image.jpg">![Webcam image](webcam-image.jpg)</a>
  <p style="text-align: center;">*Webcam image*</p>
</div>
<div class="img2-right">
  <a href="gcc.png">![Chromatic green time series](gcc.png)</a>
  <p style="text-align: center;">*Chromatic green time series*</p>
</div>
<div class="img-spacer"/>

The potential difficulties in data analysis aren't helped much by the
local criminal low-lives.  We've had four major thefts at Puéchabon in
the last six months, the latest of which involved break-ins at both of
the sheds on site and theft of a load of gas analysis and data
processing equipment (including the PC collecting the webcam images),
as well as power conditioning equipment and batteries used with the
generator we use to power the site.  Until the *third* theft of the
year, we did have a nice big set of photovoltaic panels for power, but
some enterprising lads took a truck up there and carted them all away.
Probably 20 square metres of panels, gone in one night.  It's pretty
weird.  Much of the stuff these idiots have taken is basically
worthless to them.  It's hard to imagine them successfully fencing a
gas analyser or a fluorometer at one of the local flea markets!  They
seem to be taking stuff just because it's there and it's shiny.

It's not quite clear what's going to happen at Puéchabon now going
forwards: we can't afford to have guards at the site 24 hours a day to
protect stuff, and it's getting kind of silly when Alain and
Jean-Marc, the technicians who go up there weekly, come back more
often than not with long faces and another tale of broken locks and
missing stuff.  The whole point of a site like this is to have
continuous observations, and if those keep getting interrupted by
equipment walking away, it reduces the value of the remaining data
quite a lot.  We'll have to see what happens.

[site]: http://maps.google.com/maps?q=43.7414,+3.59583&hl=en&ll=43.740941,3.596987&spn=0.001905,0.004112&sll=43.742143,3.595834&sspn=0.030477,0.065789&vpsrc=6&t=h&z=19
[qi]: http://en.wikipedia.org/wiki/Quercus_ilex
[sonnentag]: http://www.sciencedirect.com/science/article/pii/S0168192311002851
