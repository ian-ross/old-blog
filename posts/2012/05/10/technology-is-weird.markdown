---
author: Ian
tags: day-job,science
published: 2012-05-10 20:38:40
title: Technology is weird
---
I was up at our experimental site at Puéchabon the other day,
installing a new PC to collect images from our webcam.  The previous
one had been stolen by some local yokels, along with all the solar
panels and the electronics to go with them, and a load of other
hardware.  The technicians who maintain the site have made a Fort
Knox-like enclosure for the instrumentation in our little cabin, and I
had a shelf in there to put the Eee PC that collects the images from
the camera at the top of our flux tower.  (We're monitoring colour
changes in the foliage to see if we can use these not-very-remotely
sensed data to detect important phenological changes.)

I got the computer attached to the Ethernet cable and the PoE box
going to the camera, connected to the power supply in the cabin, and
checked that I could see images from the camera on the live web page
view (we're using a [StarDot](http://stardot.com) camera).  That all
worked fine, so I just needed to set up the regular FTPing of images
from the camera to the laptop.

Oops.  I'd forgotten to install an FTP server on the laptop.  The
laptop is running Ubuntu Linux, the camera runs an embedded Linux
distribution, and there's a cron job on the camera that takes an image
and FTPs it to an archival server at regular intervals.  Of course,
the "archival server" in this case is a little laptop in a shed in the
woods, and it needs an FTP server.  Which I hadn't installed.  And I
was off in a shed in the woods, far from an internet connection...

I swore at my stupidity for a couple of minutes, then had a rummage in
my rucksack.  Yes!  USB cable.  Two minutes later, I was connected to
the internet via my smart phone and a USB tethered network interface
on the laptop.  Wow.  I had to hold the phone up in the air to get
decent reception to download an FTP server package, and it wasn't the
fastest, but it was a moment that made me think about how weird our
constantly connected world has become.  We may not have electricity at
our experimental site, but we can haz internets!
