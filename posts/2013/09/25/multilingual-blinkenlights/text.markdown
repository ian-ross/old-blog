---
author: Ian
tags: embedded,haskell
title: Multilingual Blinkenlights!
published: 2013-09-25 23:15:48
---

<div class="img-right">
![Blinkenlights](blinken.jpg)
<div style="text-align: center;">
<p>*Lights, mid-blink*<br>
</div>
</div>

I got hold of a [BeagleBone Black][bbblk] a few weeks ago (courtesy of
Tom Nielsen, who'd had it for a while, but had no time to play with
it).  This is a small (credit card sized) Linux machine with an ARM
processor, intended for, well, pretty much anything you can use a
little computer for.  It has 512 Mb of RAM, plus 2 Gb of flash
storage, so it's not a completely trivial machine.

Obviously, to do anything really significant with it requires some
hardware work (there are lots of general purpose I/O pins to play
with, plus UARTs, Ethernet, USB, SPI, a couple of analogue-to-digital
converters, PWM drivers for motor control, and even HDMI video
output!), but there are a few LEDs on the board itself that lend
themselves to a blinkenlights demo...

The BeagleBone comes with some built-in software to let people write
code quickly, using JavaScript of all things.  There's also a bit of
documentation about programming the thing in C.  But (of course) I
wanted *Haskell* blinkenlights!

So, the goal was to write a little bit of code to count in binary on
the four user-addressable LEDs on the board, first in Javascript, then
in C, then (somehow) in Haskell.

<!--MORE-->

### Counting in Javascript

This was easy.  You plug the BeagleBone into a USB port on your PC,
then you can SSH to it and you have a relatively full Linux
distribution to play with, which includes Node.js, so you can run
Javascript directly from the command line.  There's a little
Javascript library called "Bonescript" that includes utilities for
setting up the hardware and doing I/O, and all it takes to flip the
LEDs is this:

~~~~ {.javascript}
var b = require('bonescript');

var i = 0;

b.pinMode("USR0", b.OUTPUT);
b.pinMode("USR1", b.OUTPUT);
b.pinMode("USR2", b.OUTPUT);
b.pinMode("USR3", b.OUTPUT);
setInterval(step, 1000);

function step() {
  var p0 = i & 1 ? b.HIGH : b.LOW;
  var p1 = i & 2 ? b.HIGH : b.LOW;
  var p2 = i & 4 ? b.HIGH : b.LOW;
  var p3 = i & 8 ? b.HIGH : b.LOW;
  i = (i + 1) % 16;
  b.digitalWrite("USR0", p0);
  b.digitalWrite("USR1", p1);
  b.digitalWrite("USR2", p2);
  b.digitalWrite("USR3", p3);
}
~~~~

We set up the four I/O pins that control the LEDs to act as outputs
using the `pinMode` function, then once a second we set the states of
those pins using the `digitalWrite` function.

Easy peasy.  But it's Javascript.  Not a fan.


### Counting in C

A bit of investigation in the `bonescript.js` library reveals that all
of the magic of controlling the BeagleBone's hardware is done through
some cunning Linux device-driver-to-filesystem jiggery-pokery.  All of
the hardware is accessed by reading and writing files under the `/sys`
filesystem.  For example, writing the string `"1"` to the file
`/sys/class/leds/beaglebone:green:usr0/brightness` switches LED0 on.

This means that the C code is no more complicated than the Javascript,
although it's a little more verbose because the stuff that's hidden
away in the Bonescript library in Javascript is in plain sight here:

~~~~ {.c}
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>

void cleanup(int);
void step(int);
void setupLED(int led);
void digitalWrite(int led, int state);

int i = 0;

int main(void)
{
  setupLED(0); setupLED(1); setupLED(2); setupLED(3);
  signal(SIGINT, cleanup);
  signal(SIGTERM, cleanup);
  signal(SIGALRM, step);
  alarm(1);
  while (1) sleep(10);
}

void cleanup(int n) { exit(0); }

void step(int n)
{
  digitalWrite(0, (i & 1) ? 1 : 0);
  digitalWrite(1, (i & 2) ? 1 : 0);
  digitalWrite(2, (i & 4) ? 1 : 0);
  digitalWrite(3, (i & 8) ? 1 : 0);
  i = (i + 1) % 16;
  alarm(1);
}

const char *triggers[] = {
  "/sys/class/leds/beaglebone:green:usr0/trigger",
  "/sys/class/leds/beaglebone:green:usr1/trigger",
  "/sys/class/leds/beaglebone:green:usr2/trigger",
  "/sys/class/leds/beaglebone:green:usr3/trigger"
};

void setupLED(int led)
{
  FILE *fp = fopen(triggers[led], "w");
  fprintf(fp, "gpio\n");
  fclose(fp);
}

const char *brightness[] = {
  "/sys/class/leds/beaglebone:green:usr0/brightness",
  "/sys/class/leds/beaglebone:green:usr1/brightness",
  "/sys/class/leds/beaglebone:green:usr2/brightness",
  "/sys/class/leds/beaglebone:green:usr3/brightness"
};

void digitalWrite(int led, int value)
{
  value = value ? 1 : 0;
  FILE *fp = fopen(brightness[led], "w");
  fprintf(fp, "%d\n", value);
  fclose(fp);
}
~~~~

The flow of control is just the same as the Javascript code, except
that we use `SIGALRM` to manage the 1-second delay between state
changes.

Being Linux, the OS on the BeagleBone comes with a C compiler (GCC
4.7.3 on the version I have), so it's easy to compile and run this.

Again, easy peasy.


### Counting in Haskell!

Now though, what about Haskell?  The BeagleBone, although it's pretty
beefy for an "embedded" platform, is probably a little too puny to
host a full GHC installation.  (For GHC 7.6.3, on my 64-bit Linux
machine, the GHC executable is about 33 Mb, and `/usr/lib/ghc-7.6.3`
is about 812 Mb...)

So we need some sort of "diet" Haskell.  It turns out that there is a
project called [Ajhc][ajhc] that goes under the byline "Haskell
Everywhere".  The people involved in this have managed to compile and
run Haskell code for platforms *much* more restricted than the
BeagleBone, so I had great hopes for this.

First question: how to compile things?  I installed Ajhc on my PC and
had no trouble compiling some simple test programs.  But Ajhc is
written in Haskell, and is generally built with GHC, so I wasn't going
to be able to host it on the BeagleBone.  Ajhc is set up to make
cross-compiling easy, but I decided to take a much more pragmatic
approach.  Ajhc emits C code which it then compiles using GCC, so all
I needed to do was to capture that intermediate C code, bundle it up
and copy it to the BeagleBone, where I could compile it to an
executable.

It turns out that a suitable incantation to Ajhc is something like

~~~~ {.shell}
ajhc -dc -fffi --tdir=./tmp BlinkLED.hs
~~~~

This tells Ajhc to dump its intermediate C code, to enable the Haskell
foreign function interface, and to use directory `./tmp` for its
temporary files.  This last thing is important because Ajhc builds its
own run-time system (RTS) on an as-needed basis, and you need to
capture the C files needed for this as well as the main program C
file.  To make life easy, Ajhc writes the GCC command needed to
compile all of the C files together into an executable into the first
line of its intermediate C file, so I wrote a little script to run
Ajhc, pick out this GCC command, and tar up all the files needed to
build things from the intermediate C code.  I could then copy this to
the BeagleBone and just run the GCC compile command to get an
executable.

Sounds simple, eh?  There remained only one trap for the unwary.
Using GHC all day every day, you kind of get used to the idea that
what is implemented by GHC *is* Haskell.  But that's not completely
true, and there are some differences between Ajhc and GHC.  I didn't
trip over any language differences so far, but one library difference
that comes up immediately if you want to do anything hardwarey is that
the `threadDelay` function in GHC's `Control.Concurrent` is *not* part
of the Haskell standard.  No problem, since we can roll our own delay
function by using a FFI call to `usleep`!

Anyway, here's the Haskell code:

~~~~ {.haskell}
module Main where

import Control.Monad (forM_)
import Data.Bits (testBit)
import System.IO
import Foreign.C.Types

led :: String -> Int -> String
led c i = "/sys/class/leds/beaglebone:green:usr" ++ show i ++ "/" ++ c

ledWrite :: String -> String -> Int -> IO ()
ledWrite c s i = withFile (led c i) WriteMode $ \h -> do
    hSetBuffering h NoBuffering
    hPutStrLn h s

setupLED :: Int -> IO ()
setupLED = ledWrite "trigger" "gpio"

digitalWrite :: Int -> Bool -> IO ()
digitalWrite i v = ledWrite "brightness" (if v then "1" else "0") i

usleep :: Int -> IO ()
usleep us = usleep' (fromIntegral us) >> return ()

foreign import ccall "unistd.h usleep"
  usleep' :: CUInt -> IO CInt

main :: IO ()
main = do
  forM_ [0..3] setupLED
  forM_ (cycle [0..15::Int]) $ \i -> do
    forM_ [0..3] $ \b -> digitalWrite b (testBit i b)
    usleep 1000000
~~~~

It's pretty simple.  And it's Haskell on an "embedded" device, for
rather large values of "embedded".

What's next?  I'm going to try to get the [Scotty web server][scotty]
working on the BeagleBone, then I'm going to try to get the thing
talking to the cheap USB webcam I have.  As a first hardware project
(involving a minimal amount of hardware!), I'd like to make a little
wildlife cam I can stick out in the woods to take pictures of deer and
whatnot as they frolic through the trees.  I'll either use
[OpenCV][opencv] to do motion detection between frames captured by the
camera or (a tiny bit more ambitious), I'll use an infrared sensor to
wake the board up and start taking pictures when something comes
close.  I have lots of other ideas, but something like this should be
a good start.


[bbblk]: http://www.beagleboard.org/Products/BeagleBone%20Black
[ajhc]: http://ajhc.metasepi.org/
[scotty]: http://hackage.haskell.org/package/scotty
[opencv]: http://opencv.org/
