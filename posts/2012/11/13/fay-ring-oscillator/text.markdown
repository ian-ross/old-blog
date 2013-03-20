---
author: Ian
tags: haskell,mathematics
timestamp: 15:27:01
title: Fun with Fay
---
This won't sound like fun (or Fay!) to start with, but we'll get
there...

<!--MORE-->

I've been playing a bit with calculating Lyapunov exponents using
Haskell in the [previous](posts/2012/11/01/lyapunov-exponents-1)
[two](posts/2012/11/06/lyapunov-exponents-2) articles.  My original
motivation for looking at these methods was to try to calculate
Lyapunov exponents for partial differential equations.  That's pretty
hard though, from both a practical and a theoretical point of view
(I'm not even 100% sure I know what a Lyapunov exponent means for an
infinite-dimensional system).  Of course, for computational purposes,
we always compute using a finite discretisation of a PDE, and there's
an interest in seeing how Lyapunov exponents calculated for these
discretisations converge (or not) as the discretisations become finer.
That's also kind of hard, but we can get started by thinking about
computational methods by using ODE systems that we can set up with a
variable number of degrees of freedom.

One system that's been used by a few people[^1] is a driven ring
oscillator, like this:

$$\frac{d^2 y}{dt^2} = -\alpha (y^2-1) \dot{y} - \omega^2 y$$

$$\frac{d^2 x_1}{dt^2} = -d_1 \dot{x_1} - \beta[V'(x_1 - x_N) - V'(x_2 - x_1)] + \sigma y$$

$$\frac{d^2 x_i}{dt^2} = -d_i \dot{x_i} - \beta[V'(x_i - x_{i-1}) - V'(x_{i+1} - x_i)], \qquad i = 2, \dots, N$$

Here $y(t)$ is a nonlinear forcing oscillator and the $x_i$ ($i =
1,\dots,N$) are the "ring" oscillators, with $x_1$ being forced by the
external oscillator $y$.  Each of the ring oscillators has a damping
term and a term that depends on a potential based on the separation to
the neighbouring oscillators in either direction around the ring.

It's at least partially clear what's going to happen in a system like
this.  The forcing is a fairly standard looking nonlinear oscillator
and we can pick parameters to ensure that it has a stable limit cycle
and provides a periodic forcing to the ring oscillator.  The ring
oscillator itself obviously has adjacent elements pushing each other
around, but what sort of orbits you get isn't very clear (or not to
me, at least).

So, I thought a little bit of a visualisation might be in order.  But
what to use for the visualisation?  It would be nice to have a little
JavaScript toy so that I could embed it in a blog post, but that would
mean writing JavaScript!  There has to be a better way[^2].  Enter
[Fay](http://fay-lang.org), written by
[Chris Done](http://chrisdone.com/index.html)!  Fay is a compiler for
a strict subset of Haskell that targets JavaScript.  There are a
number of other Haskell-to-JS compilers out there, but Fay is
distinguished by being very lightweight and having a very nice
approach to calling JavaScript functions from Haskell.

Here, I want to talk about the experience of using Fay to build a
little toy to explore the behaviour of the ring oscillator described
above.


## The toy

*Note: this uses the HTML5 canvas element and the JavaScript API for
manipulating it.  If you're using some crappy old browser (in
particular, Internet Explorer 8 or earlier), you won't see much...*

Here it is (and [here](https://gist.github.com/4066030) is the code,
along with a stand-alone HTML page to embed it in).  The grey blob and
graph trace show the external forcing oscillator, blue shows the ring
oscillator that is attached to the external forcing and red shows the
other ring oscillators.  The graph traces have a mean displacement
subtracted for convenience -- if you wait long enough, you can see the
mean drift of the oscillators around the ring.  Have a play...

<br>

<div class="clear"></div>
<div class="grid_11">
  <canvas id="canvas" width=400 height=400></canvas>
</div>
<div class="grid_4">
  <input id="go" type="button">
  <input id="stop" type="button">
  <input id="reset" type="button" value="Reset">

  <p>N = <select id="nosc">
      <option value="3">3</option>
      <option value="4">4</option>
      <option selected value="5">5</option>
      <option value="6">6</option>
      <option value="10">10</option>
    </select>
  <br>
  &alpha; = <select id="alpha">
      <option value="0.6">0.6</option>
      <option value="0.8">0.8</option>
      <option selected value="1">1</option>
      <option value="1.2">1.2</option>
      <option value="1.4">1.4</option>
    </select>
  <br>
  &omega; = <select id="omega">
      <option value="1.0">1</option>
      <option value="1.4">1.4</option>
      <option selected value="1.8">1.8</option>
      <option value="2.2">2.2</option>
      <option value="2.6">2.6</option>
    </select>
  <br>
  d<sub>even</sub> = <select id="deven">
      <option value="0.0025">0.0025</option>
      <option value="0.005">0.005</option>
      <option selected value="0.0075">0.0075</option>
      <option value="0.01">0.01</option>
      <option value="0.0125">0.0125</option>
    </select>
  <br>
  d<sub>odd</sub> = <select id="dodd">
      <option value="0.0075">0.0075</option>
      <option value="0.01">0.01</option>
      <option selected value="0.0125">0.0125</option>
      <option value="0.015">0.015</option>
      <option value="0.0175">0.0175</option>
    </select>
  <br>
  &beta; = <select id="beta">
      <option value="0.6">0.6</option>
      <option value="0.8">0.8</option>
      <option selected value="1">1</option>
      <option value="1.2">1.2</option>
      <option value="1.4">1.4</option>
    </select>
  <br>
  &sigma; = <select id="sigma">
      <option value="2">2</option>
      <option value="3">3</option>
      <option selected value="4">4</option>
      <option value="5">5</option>
      <option value="6">6</option>
    </select>
  </p>
</div>
<div class="clear"></div>
<canvas id="graph" width=640 height=200></canvas>


## The model

So, how does this work in Fay?  Most pure code in Fay is identical to
what you would write in Haskell, apart from a couple of caveats
related to the fact that Fay is very new and isn't intended to be a
full implementation of Haskell 2010 anyway[^3].

Here's an example.  We need to integrate the ODE system presented
above.  To keep things simple, we'll use a fixed time-step 4th order
Runge-Kutta step.  Here's what that looks like in Haskell, for a
function `f`, an initial state `yn` and a time step `h`:

~~~~ {.haskell}
rk4 :: ([Double] -> [Double]) -> [Double] -> Double -> [Double]
rk4 f yn h = zipWith5 (\y a b c d -> y+(a+2*b+2*c+d)/6) yn k1 k2 k3 k4
  where k1 = map mh $ f yn
        k2 = map mh $ f (zipWith (+) yn (map half k1))
        k3 = map mh $ f (zipWith (+) yn (map half k2))
        k4 = map mh $ f (zipWith (+) yn k3)
        mh x = h * x
        half x = 0.5 * x
~~~~

And the Fay code?  Well, it's just the same!  This is the really sweet
thing about using something like Fay (or UHC-js or Haste or any of the
other Haskell-to-JS compiler approaches) for developing the
interactive part of web pages: you write Haskell code, which you can
test in GHCi using all the normal methods you would use for developing
any other Haskell code.  For web applications, there's another big
advantage, which is that code can be shared between the server and
client sides of the application, which eliminates the hassle of
maintaining both client and server side representations of data
structures.

For the ring oscillator, that's obviously not an issue, since it all
lives in the browser, but the representation of our little ODE system
in Haskell is quite succinct and clear.


## Foreign function interface

Of course, we can't hide forever from the fact that we're living in
the swampy impure world of a web browser, so we need some sort of way
to talk back and forth to JavaScript.  The approach that Fay takes to
this foreign function interface (FFI) problem is really nice.  Here's
a definition that allows Haskell code to access the JavaScript
`Math.sin` function:

~~~~ {.haskell}
sin :: Double -> Double
sin = ffi "Math.sin(%1)"
~~~~

For impure interactions with JavaScript, you can use the `Fay` monad
to impose sequencing.  For instance, getting a DOM element from your
page by ID is done like this:

~~~~ {.haskell}
getElementById :: String -> Fay Element
getElementById = ffi "document['getElementById'](%1)"
~~~~

This can only be called in a monadic context, so ordering of
computation is controlled.

The possibilities of this approach are pretty much endless.  For the
ring oscillator toy, I needed mutable circular buffers to store the
data used to draw the graph view.  I did this using a simple Haskell
record holding information about the size of the buffer, the current
number of entries and the position to insert the next entry, along
with a JavaScript array to hold the entries:

~~~~ {.haskell}
data Array
instance Foreign Array

data Buffer = Buffer { bufSize :: Int
                     , bufCurSize :: Int
                     , bufNext :: Int
                     , bufArr :: Array }
instance Foreign Buffer

newBuf :: Int -> Fay Buffer
newBuf size = do
  arr <- newArray size
  return $ Buffer size 0 0 arr

bufAdd :: Buffer -> Double -> Fay Buffer
bufAdd (Buffer sz cursz nxt arr) x = do
  let cursz' = if cursz < sz then cursz + 1 else sz
  setArrayVal arr nxt x
  let nxt' = (nxt + 1) `rem` sz
  return $ Buffer sz cursz' nxt' arr

bufVal :: Buffer -> Int -> Fay Double
bufVal (Buffer sz cursz nxt arr) i = do
  let idx = (if cursz < sz then i else nxt + i) `rem` sz
  arrayVal arr idx >>= return
  
newArray :: Int -> Fay Array
newArray = ffi "new Array(%1)"

setArrayVal :: Array -> Int -> Double -> Fay ()
setArrayVal = ffi "%1[%2]=%3"

arrayVal :: Array -> Int -> Fay Double
arrayVal = ffi "%1[%2]"
~~~~

This stuff is really easy to use, and just takes all of the pain out
of developing interactive web pages.


## Event handling

Event handling is nice and simple too.  The JavaScript
`addEventListener` method is made accessible as:

~~~~ {.haskell}
addEventListener :: Element -> String -> (Event -> Fay Bool) -> Fay ()
addEventListener = ffi "%1['addEventListener'](%2,%3,false)"
~~~~

so that event handlers are written as functions of type `Event -> Fay
Bool`.  Because event handlers are just regular Haskell functions, you
can partially apply them, in order to pass in extra context needed
during event handling from the computation where the event handler is
registered.  For example, to handle presses of the "go" button, we set
up the event handler:

~~~~ {.haskell}
  addEventListener go "click" $ 
    doGo timerref (animate c cg pref xref gdataref renderrng) framems
~~~~

and implement it like this:

~~~~ {.haskell}
doGo :: Ref (Maybe Int) -> Fay () -> Double -> Event -> Fay Bool
doGo tref anim interval _ = do
  oldtimer <- readRef tref
  case oldtimer of
    Nothing -> do
      timer <- setInterval anim interval
      writeRef tref (Just timer)
    Just _ -> return ()
  return False
~~~~

Here, `timerref` is a mutable reference used to communicate the return
value from calls to `setInterval` between different event handlers,
the second `anim` argument to `doGo` is an action in the `Fay` monad
that deals with animating the views of the model (and which is
produced by the `animate` function, which takes a bunch of arguments
that specify the model context for the animation) and the final
argument is the number of milliseconds between animation frames.  This
form of partial application makes setting up event handlers
convenient, and replaces the need for global variables for maintaining
state.


## Rendering

Take a look at the `render` and `renderGraph` functions in
[`RingOscillator.hs`](RingOscillator.hs).  The Fay FFI makes accessing
the JavaScript HTML5 canvas API a real breeze.  Combined with being
able to express computational elements of the render functions in
Haskell, this is a real win.  Here's an example from `renderGraph`,
where we need to calculate the positions of the time ticks on the
scrolling animated graph and render them:

~~~~ {.haskell}
  let ticks = takeWhile (\t -> t <= floor (ts + gwwtime)) [floor ts + 1..]
  let tickxs = map (\t -> (fromIntegral t - ts) * pxpert) ticks
  forM_ (zip ticks tickxs) $ \(t,x) -> do
    moveTo cg (x,gwh/2-ticklen/2)
    lineTo cg (x,gwh/2+ticklen/2)
    let txt = show t
    txtw <- measureText cg txt
    fillText cg txt (x-txtw/2,gwh/2+2*ticklen) Nothing
~~~~

Here, `ts` is the current earliest time displayed on the graph,
`gwwtime` is the total width of the graph in time units, `pxpert` is
the number of pixels per time unit, `cg` is the graphics context used
to draw into the graph canvas and `gwh` is the height of the graph
canvas.


## Conclusions

It's Haskell in the browser.  It's great.  In more detail:

* The FFI is a really good idea.  While it's perhaps not quite as
  type-safe as the GHC FFI, it is *really* easy to use.  So much of
  the pain of programming in heterogeneous environments comes from
  boundary issues that it's very cool to have it feel so effortless
  here.
  
* It works.  It's an early stage project, so there are bugs, but
  they're not hard to track down -- the JavaScript code produced by
  the compiler is quite readable, and it's easy to write test cases
  that you can run in a terminal with Node.js.  I've had a go at
  fixing a couple of very minor issues in Fay and I was pleasantly
  surprised at how painless the workflow was.
  
* There is Yesod integration.  Again, very new, but it's starting to
  look quite feasible to write web apps end-to-end in Haskell.
  Minimising the client/server divide is potentially quite a big
  deal, and anything that reduces the number of languages needed to
  write web apps has to be a good thing (especially if the language we
  eliminate is JavaScript...).
  
* The JavaScript code that Fay produces isn't the fastest in the
  world.  But honestly, who cares?  I view this as more a proof of
  concept than anything else.  There are various ideas floating around
  for how to make better backends[^4] but, for the meantime, Fay is here
  and it works.

Oh, and did I learn anything about the ring oscillator?  Well, a
little bit.  There's this canonical drift of the oscillators around
the ring, which is probably associated with a zero Lyapunov exponent
somewhere.  There are interesting "locked" modes that seem to be
related to the symmetry of the system (perhaps because the
inter-oscillator interaction potential is symmetric?).  There are
these weird things called "chimera" states that appear in some
nonlinear coupled oscillators[^5], where some of the degrees of
freedom become synchronously locked while the rest continue to evolve
chaotically -- I don't know if this is a very simple example of that
kind of phenomenon.

I've not yet convinced myself that it will be possible (or easy
anyway) to compute the Lyapunov exponents of this sort of system using
the approach of Rangarajan et al. that I used for the smaller systems
in the earlier articles.  Although in theory, it's possible to compute
a subset of the Lyapunov exponents using a smaller system than the
$n(n+3)/2$ degree of freedom system needed for the whole spectrum,
it's not clear how to do that in an efficient way.  More thinking
required, or perhaps I'll have to try a cleverer method...


[^1]: I saw it in T. J. Bridges & S. Reich (2001). Computing Lyapunov
      exponents on a Stiefel manifold. *Physica D* **156**(3-4), 219-238.

[^2]: I also had a bit of a play with [Elm](http://elm-lang.org)
      before settling on Fay.  Elm is neat and I really like the idea
      of using functional reactive programming, but as a language it's
      just different enough from Haskell that I was finding it hard to
      write things quickly and just similar enough to Haskell that I
      felt like I ought to be able to write things quickly.

[^3]: In particular, Fay doesn't have type classes yet, which makes
      some things tricky to express.  Even though there is no `Monad`
      type class though, it does have *a* monad, called `Fay`, which
      is used for sequencing operations that have to deal with
      potentially impure effects in the JavaScript ecosystem.  That
      means that Haskell monad utility functions (`mapM`, etc.) can be
      carried across to Fay with only minor changes.

[^4]: Some of these ideas sound really quite cheeky: one approach
      that's been suggested is to use the front end of GHC to produce
      a type-annotated AST, then to strip out all the GHC-specific
      bits, replacing them with Fay-specific things.  You could then
      feed the resulting modified AST back into GHC, let it do all its
      optimisation magic and take either the resulting Core or STG
      assembler output and compile *that* to JavaScript.  That's kind
      of a scary proposition, since it would involve wallowing in the
      murky depths of the GHC API, but it would mean that a). you
      could compile all of GHC's fancy type system extensions and
      b). you would get the benefit of all of GHC's optimisation
      passes (including things like list fusion).

[^5]: D. M. Abrams & S. H. Strogatz (2006). Chimera states in a ring
      of nonlocally coupled oscillators. *Int. J. Bifurc. Chaos*
      **16**(1), 21-37.

<style type="text/css">
#go { background-image: url(play-icon.png); width: 20px; }
#stop { background-image: url(stop-icon.png); width: 20px; }
</style>
<script type="text/javascript" src="RingOscillator.js"></script>
