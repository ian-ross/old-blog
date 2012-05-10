---
author: Ian
tags: constraints,haskell
timestamp: 18:22:04
title: Playing with the Simplex Algorithm
---
I was originally planning to write a quick-and-dirty implementation of
the [simplex algorithm][simplex] myself to demonstrate some of the
gritty details, but I decided to leave that kind of thing for later,
since there are going to be other, less well-known, constraint solving
algorithms that I want to look at[^1].  There is a Haskell
implementation of a simple version of simplex algorithm in the
`Matrix.Simplex` module in the `dsp` package, although that comes with
a comment that says "I only guarantee that this module wastes inodes",
so let's view this more as a way to play with some interface issues
than a serious attempt to implement any kind of constraint solver!
The code that goes with this article is available
[here](SimplexExperiment.hs).


## The simplex algorithm ##

The simplex algorithm, as implemented in `Matrix.Simplex` solves the
following problem: find a vector $\mathbf{x} \in \mathbb{R}^N$ that
minimises a linear objective function

<div class="eq">$z = \mathbf{c} \cdot \mathbf{x}$</div>
<br>

subject to the constraints $A \mathbf{x} = \mathbf{b}$, with all
components of $\mathbf{x}$ non-negative, i.e. $x_i \geq 0$.

To set the problem up, you put all the coefficients of the objective
function and the constraint equations into a big matrix (usually
called the *tableau*), you turn the handle, then you pick the answers
out of a transformed version of the tableau.  This is very similar to
the way that Gauss-Jordan elimination and such algorithms work (the
simplex algorithm looks a lot like Gauss-Jordan elimination with some
extra steps and restrictions), and is fine as far as it goes, but it's
not very convenient.  Instead of building a big matrix, it would be
nice to be able to set a problem up like this:

~~~~ {.haskell}
exampleSystem :: ConstraintSystem String
exampleSystem = buildSystem $ do
  let [x1, x2, x3, x4] = map var ["x1", "x2", "x3", "x4"]
  con $ x1 + 2 * x3 <=# 740
  con $ 2 * x2 - 7 * x4 <=# 0
  con $ x2 - x3 + 2 * x4 >=# 1/2
  con $ x1 + x2 + x3 + x4 ==# 9
  return $ -x1 - x2 - 3*x3 + 1/2*x4
~~~~

The idea here is that we create some variables, here labelled with
strings, although they can be labelled with anything with an `Ord`
instance, we define some constraints, using comparison operators with
a `#` stuck at the end to mark them as constraint operators, then we
`return` an objective function.  Some writer monad fiddling gives us a
nice little monadic interface to build our system, and we then have a
`solve` function of type

~~~~ {.haskell}
solve :: (Ord a, Show a, Slack a) => ConstraintSystem a -> 
         Either SimplexError (M.Map a Double)
~~~~

that takes a constraint system, tries to solve it, and returns either
an error or a map assigning numerical values to each of the variables.

This is all pretty pedestrian, but I wanted to have a play with these
ideas because a real geometrical constraints system is going to be a
bit more complicated and is going to incorporate some extra wrinkles
that we don't have here.  The simplex algorithm example is nice and
simple...


## Expressions ##

For the moment, I'm using a super-simple ADT for linear expressions,
defined as

~~~~ {.haskell}
data Expr a = Expr { vars :: M.Map a Double, numval :: Double }
              deriving (Show, Eq)
~~~~

with a map from variable names to coefficients, plus a constant term.
`Num` and `Fractional` instances are defined to make these act like
numbers under the usual operators.  For example, adding two
expressions is defined as

~~~~ {.haskell}
  (Expr vs1 c1) + (Expr vs2 c2) = Expr (M.unionWith (+) vs1 vs2) (c1 + c2)
~~~~

Of course, there's something slightly funny going on here, since we
only really want to be able to construct *linear* expressions.  I
played around with this, and I sort of convinced myself that it ought
to be possible to constrain expressions to linearity using phantom
type methods, but I've not yet got it working.  This is something I'm
still undecided about, and the toy code I've written here just throws
a run-time error when non-linear operations are attempted.


## Constraints ##

Constraints are very simple, built from a left-hand side expression, a
right-hand side expression, and a comparison operator, one of `<=#`,
`==#` or `>=#`.

A constraint system is then a list of constraints and an objective
function, which is just an expression.  By defining `ConstraintSystem`
as

~~~~ {.haskell}
type ConstraintSystem a = (Expr a, [Constraint a])
~~~~

and defining a couple of writer monad related helpers

~~~~ {.haskell}
con c = tell [c]
buildSystem = runWriter
~~~~

we can construct constraint systems like `exampleSystem` as shown
above.


## Canonicalising... ##

The implementation of the simplex algorithm in `Matrix.Simplex`
requires the problem to be in a canonical form, which has no
inequality constraints, except for the requirement that all variables
in the solution are non-negative.  We can get from a constraint system
involving inequality constraints to one with only equality constraints
by introducing "slack" variables.  For instance, if we have a
constraint $x_1 + 2 x_3 \leq 740$, we can introduce a variable $s \geq
0$, and then write our original inequality constraint as the equality
$x_1 + 2 x_3 + s = 740$.  It's pretty obvious how to do this for
inequalities involving greater than signs as well.

The code to take a constraint system involving inequalities and
transform it into an equivalent constraint system involving only
equality constraints plus slack variables uses a typeclass called
`Slack` whose sole purpose is to capture the idea of generating a name
for a temporary variable.  Here, I just define an instance for
`[Char]`, since this is what I'm using to label variables.  The
function `makeSlacks` does the transformation, introducing slack
variables as needed for each inequality:

~~~~ {.haskell}
makeSlacks :: (Show a, Ord a, Slack a) => ConstraintSystem a -> ConstraintSystem a
makeSlacks (obj, cs) = (obj, zipWith makeSlack cs [1..])
  where makeSlack (Constraint e1 EQL e2) _ = (e1 - e2 ==# 0)
        makeSlack (Constraint e1 GEQ e2) s = (e1 - e2 - var (genSlk s) ==# 0)
        makeSlack (Constraint e1 LEQ e2) s = (e1 - e2 + var (genSlk s) ==# 0)

class Slack a where
  genSlk :: Int -> a
  
instance Slack [Char] where
  genSlk s = "slk_" ++ show s
~~~~

Given a canonical constraint system, we can then construct the initial
tableau needed to run the simplex algorithm.  This isn't a very
interesting step, since it just involves constructing a big array and
systematically copying the coefficients from the constraints and the
objective function in the canonical system into the right places.


## Getting the answer out ##

Once we've run the simplex algorithm, we get either an error return
indicating that the problem was unsatisfiable because there were no
feasible values or the feasible region was unbounded, or a final
tableau that contains the variable assignments for the optimal
solution.  Picking these out of the final tableau is kind of ugly,
since we need to look for columns in the tableau that look like
columns of a unit matrix.  This isn't hard to do, although it's not
pretty.


## Conclusions ##

One thing I confirmed from this little exercise is that things like
the simplex algorithm are tricky (no surprise there).  I was working
with a version of the algorithm that's about as simple as it's
possible to get, but it's still quite complicated and there's quite a
bit of housekeeping to be done.  (In fact, I think there is at least
one minor error in the `Matrix.Simplex` code that I identified.)

The other main points that came out of this exercise were:

1. The writer monad approach to building constraint systems looks
   quite nice, and it ought to be possible to extend it to easily
   build and update more complex types of systems than the ones dealt
   with here.
   
2. Specialised expression types that catch attempts to build invalid
   constraints at compile time might be a good idea, although I'm not
   yet convinced that this would be convenient in more general cases.
   I'll revisit this later once I have a better picture of the
   geometrical constraints we need to deal with.

3. Hiding the implementation details of algorithms like the simplex
   algorithm makes them *much* more usable.  This will be even more
   true for the more complex cases I want to tackle later.
   
4. I need more Haskell practice.  Everything more or less works, but
   some of the solutions I came up with are a little painful and could
   probably be cleaned up considerably.

There are two things I want to do next.  The first is to read some
literature, including the PhD thesis of [Michael Gleicher][gleicher]
which, in particular, deals with the issue of geometrical objects that
have multiple "views" (e.g. Cartesian versus polar coordinates for a
point, representation of a line segments by two end points or by a
single point, a length and an angle, and so on).  The second is to
learn more about [lenses][lenses], and to try to come up with a way to
make lenses or some generalisation of them work nicely in a constraint
setting.

[simplex]: http://en.wikipedia.org/wiki/Simplex_algorithm
[gleicher]: http://pages.cs.wisc.edu/~gleicher/
[lenses]: http://stackoverflow.com/questions/5767129/lenses-fclabels-data-accessor-which-library-for-structure-access-and-mutatio

[^1]: The Cassowary algorithm is also a tableau-based algorithm, like
      the simplex algorithm, so I might spend a bit of time thinking
      about that in a bit more detail.
      
