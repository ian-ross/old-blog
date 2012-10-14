---
author: Ian
tags: AI,haskell
timestamp: 19:48:56
title: AI Class: Bayes Networks
---
The second week of the online [Stanford AI class][ai-class] was about
probability, reasoning when there is uncertainty and, more
specifically [Bayes networks][bns].  I don't much want to talk about
the theory behind these things here since that's covered in a lot of
detail on the linked Wikipedia page, in the lectures and in
[AIMA][aima].

Instead, I want to present a mildly amusing, although not very
efficient, Haskell implementation of Bayes networks.  It's often said
that if you know the full joint probability distribution function
(PDF) for a system, you can calculate any marginal or conditional
probabilities you want.  Well, Haskell is a functional programming
language, a PDF is a *function*, so can we represent the PDF
explicitly and use it for calculating?  The answer, of course, is yes.

<!--MORE-->

The idea here is to implement something that will allow us to write
the two-test cancer problem from the lectures as:

~~~~ {.haskell}
import BayesNet

data Event = C | T1 | T2 deriving (Eq, Ord, Show)

bn = fromList [(C, [], [0.01]),
               (T1, [C], [0.9,0.2]),
               (T2, [C], [0.9,0.2])]
~~~~

where the entries in the list passed to the `BayesNet.fromList`
function are triples giving the label for a node in the network, a
list of other nodes that it is conditional on (i.e. nodes at the other
end of incoming edges) and the probability table for the node.  If a
node $A$ is conditional on nodes $B$ and $C$, the probability table
entries are stored in the order $P(A|+B,+C)$, $P(A|+B,-C)$,
$P(A|-B,+C)$, $P(A|-B,-C)$.  Given this definition, we can then
calculate $P(C|T1,T2)$ as

~~~~ {.haskell}
p_3_20 = prob bn C [(T1,True),(T2,True)]
~~~~

and $P(C|T1,\neg T2)$ as

~~~~ {.haskell}
p_3_21 = prob bn C [(T1,True),(T2,False)]
~~~~


## Data structures ##

A Bayesian network is basically just a directed acyclic graph with
probability tables of the appropriate size associated with each node.
In my Haskell code, I treat Bayesian networks as an abstract data
type, exporting only a way to construct a network from a simple
list-based description, and functions to calculate probability values
given a network.  (You can imagine lots of other possible things you
might want to do, including updating networks, calculating conditional
independence relationships, and so on, but here, we're going to
concentrate on the basics.)

~~~ {.haskell}
module BayesNet(fromList, prob, jointProb, jointProbTF) where

import qualified Data.Map as M
import Data.List
~~~~

The internal representation of a node in the network is as a list of
nodes on which this node is conditional, and a probability table.
Nodes are parameterised by type `e` ("event") and the type of
probability values is parameterised too, by type `p`, so that we can
use, for instance, rational values if we want.  (I'll show an example
of this later on, but basically, we just want to impose the type class
constraint `Fractional p` everywhere to make sure that the probability
values that get passed in are numbers with a fractional part.)

~~~~ {.haskell}
data BayesNode e p = BayesNode { cond :: [e], ps :: [p] } 
                   deriving (Eq, Ord, Show)
~~~~

A Bayesian network is then just a map from node labels to nodes.

~~~~ {.haskell}
newtype BayesNet e p = BayesNet (M.Map e (BayesNode e p)) 
                     deriving (Eq, Ord, Show)
~~~~

We can generate Bayesian network values from a simple list
representation, which is a list of nodes of the form:

~~~~
  (event, conditions, probabilities)
~~~~

where: 

* `event` is a label for the node (of type `e` in all the function
  type signatures here);
* `conditions` is a list of the events on which this event is
  conditional (i.e. a list of type `[e]`); and
* `probabilities` is a table of probability values, stored as a flat
  list.  The type of these values (represented as type variable `p`
  everywhere) can be any fractional numeric type.  The values are
  stored in lexical order of the truth value assignment to the
  conditions variables, where `True` sorts before `False`.  For
  instance, if `conditions` is `[R,S]`, then the probability values
  are in the order `+R,+S`; `+R,-S`; `-R,+S`; `-R,-S`.

Creating a network from its list representation is simple.  The only
small thing we need to do is to check that the probability table has
the right number of entries, dependent on the number of conditional
variables.  It would be interesting to try to apply this constraint at
the type level.  No idea how to do that.  It's probably easy in
something like Agda, with dependent types, or Qi, where you can write
programs at the type level.  In Haskell, I guess it might require
Template Haskell.  I don't know though.  In any case, let's keep
things simple here:

~~~~ {.haskell}
fromList :: (Ord e, Fractional p) => [(e, [e], [p])] -> BayesNet e p
fromList = BayesNet . (foldl doOne M.empty)
  where doOne m (ev, cond, ps) = 
          if (length ps /= 2^(length cond)) 
          then error "Invalid length for probability table"
          else M.insert ev (BayesNode cond ps) m
~~~~


## Probability calculations ##

Given a Bayesian network, we want to use it to calculate
probabilities.  A couple of special cases, first the probability that
a single event is true, given conditions (true or false) on a set of
other events.

~~~~ {.haskell}
prob :: (Ord e, Fractional p) => BayesNet e p -> e -> [(e,Bool)] -> p
prob bn ev cond = jointProb bn [ev] cond
~~~~

Then, the joint probability that a set of events is true, given
conditions (true or false) on a set of other events.

~~~~ {.haskell}
jointProb :: (Ord e, Fractional p) => BayesNet e p -> [e] -> [(e,Bool)] -> p
jointProb bn evs cond = jointProbTF bn (zip evs $ repeat True) cond
~~~~

Then the general case: the probability that a set of events is true or
false, as specified, given conditions (true or false) on a set of
other events.

We do this by calculating the joint PDF of the whole network, then
enumerating the possible assignments of truth values to the free
variables in the numerator and denominator of the representation of
$P(X | Y) = P(X ^ Y) / P(Y)$, applying the joint PDF to each of these
variable assignments, summing the results and forming the fraction
representing the conditional probability.

~~~~ {.haskell}
jointProbTF :: (Ord e, Fractional p) => 
               BayesNet e p -> [(e,Bool)] -> [(e,Bool)] -> p
jointProbTF bn@(BayesNet m) evs cond = eval (cond ++ evs) / eval cond
  where eval = sum . (map (jointPDF bn)) . (enumerate bn)
~~~~

All the work here is being done in the computation of the joint PDF,
which represented just as a regular Haskell function, and the
enumeration of the possibilities (remember that we're only dealing
with binary random variables throughout, just as in the lectures).


## The joint PDF ##

The joint PDF for the network

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\node (b) at (-1,1) [circle,draw] {$B$};
\node (e) at (1,1) [circle,draw] {$E$};
\node (a) at (0,0) [circle,draw] {$A$};
\node (j) at (-1,-1) [circle,draw] {$J$};
\node (m) at (1,-1) [circle,draw] {$M$};
\draw [->] (b) -- (a);
\draw [->] (e) -- (a);
\draw [->] (a) -- (j);
\draw [->] (a) -- (m);
@@@@@

is:

$P(B,E,A,J,M) = P(B) P(E) P(A|B,E) P(J|A) P(M|A)$

where we have one factor per node in the network, each of which is
conditioned on the variables in the nodes connected to the incoming
edges.  In the code below, the joint PDF is represented as a function
that takes a map from node labels to Boolean values and returns a
probability value.  To calculate the joint PDF of the whole network,
we form the product of the individual PDF factors for each node in the
network, applying each of these factor functions to the input variable
assignment, and forming the product.

~~~~ {.haskell}
jointPDF :: (Ord e, Fractional p) => BayesNet e p -> (M.Map e Bool -> p)
jointPDF bn@(BayesNet m) = 
  (\vals -> product $ zipWith (applyFactor vals) (M.keys m) factors)
  where factors = map (jointPDFFactor bn) (M.keys m)
        applyFactor vs k f = f (vs M.! k) (map (vs M.!) (cond $ m M.! k))
~~~~

We then need to produce the constituent functions for the individual
factors in the PDF.  Thes functions, for a factor of the form $P(X |
Y)$ are represented by a function that takes a single Boolean value
representing the value of $X$, and a list of Boolean values
representing the values of $Y$, and returns $P(X | Y)$.  To do this,
we need to know the predecessors of the node in question in the graph
of the network, we need to extract the correct value from the node's
probability table, and we need to deal with complementary probability
values.

~~~~ {.haskell}
jointPDFFactor :: (Ord e, Fractional p) => 
                  BayesNet e p -> e -> (Bool -> [Bool] -> p)
jointPDFFactor bn@(BayesNet m) ev =
  (\b bs -> let pval = pvals !! (length pvals - idx - 1)
                idx = foldl (\i b -> (if b then 1 else 0) + 2 * i) 0 bs
            in if b then pval else (1 - pval))
  where pvals = ps (m M.! ev)
~~~~


## Enumerating cases ##

The other element we need in order to calculate probabilities is to be
able to enumerate the possible assignments of Boolean truth values to
variables in the network, given that some variables may have their
values fixed by the requirements of the calculation.  Here, we assume
that there are no extra *logical* constraints on the values that
variables may take.  In many more realistic problems, there may be
consistency requirements that eliminate some possible assignments (see
*The Wumpus World Revisited*, Section 13.6 of [AIMA][aima] for a
simple example where this is the case).  Given that caveat, state
enumeration is quite simple: 

~~~~ {.haskell}
enumerate :: (Ord e, Fractional p) => 
             BayesNet e p -> [(e,Bool)] -> [M.Map e Bool]
enumerate bn@(BayesNet m) fixed = map compose msks
  where base = M.fromList fixed
        vars = (M.keys m) \\ (map fst fixed)
        msks = masks (length vars)
        compose = (M.union base) . M.fromList . (zip vars)
~~~~

We have a little helper to generate combinations of Boolean values:

~~~~ {.haskell}
masks :: Int -> [[Bool]]
masks 0 = [[]]
masks 1 = [[False], [True]]
masks n = map (False :) xs' ++ map (True :) xs'
  where xs' = masks (n - 1)
~~~~


## Applications ##

Given the definitions above, the two test cancer example works as
expected.  We can also deal with Sebastian's happiness example.  The
necessary definition of the Bayes network is:

~~~~ {.haskell}
import BayesNet
import Data.Ratio

data Event = S | R | H deriving (Eq, Ord, Show)

bn = fromList [(S, [], [7 % 10]),
               (R, [], [1 % 100]),
               (H, [S, R], [1, 7 % 10, 9 % 10, 1 % 10])]
~~~~

Here, I've made the probability values in the Bayes network be
rational numbers (remember that we can use any instance of typeclass
`Fractional` for this).  We can then answer some of the questions
Sebastian posed in the lecture.  First we load the definition into
GHCi:

~~~~ {.haskell}
*Main> :load "happiness.hs"
[1 of 2] Compiling BayesNet         ( BayesNet.hs, interpreted )
[2 of 2] Compiling Main             ( happiness.hs, interpreted )
Ok, modules loaded: BayesNet, Main.
~~~~

Then we ask the value for $P(R|S)$:

~~~~ {.haskell}
*Main> prob bn R [(S,True)]
Loading package array-0.3.0.2 ... linking ... done.
Loading package containers-0.4.0.0 ... linking ... done.
1 % 100
~~~~

The answer is just $P(R)$ because $R$ and $S$ are independent.  Notice
that we get the result as a rational here.  What about $P(R|H,S)$?
Just as easy:

~~~~ {.haskell}
*Main> prob bn R [(H,True),(S,True)]
10 % 703
*Main> fromRational $ prob bn R [(H,True),(S,True)]
1.422475106685633e-2
~~~~

Here, I've converted the result from a rational to a floating point
for comparison with the next result, which is $P(R|H)$:

~~~~ {.haskell}
*Main> prob bn R [(H,True)]
97 % 5245
*Main> fromRational $ prob bn R [(H,True)]
1.8493803622497616e-2
~~~~

Here, $P(R|H) > P(R|H,S)$, demonstrating the "explaining away" effect.

For sure, you can do the homework questions using the same setup.


## Conclusions ##

This is *not* a good way to do probability calculations on Bayes
networks in general.  There are much better approaches available than
enumeration.  My goal here was more to investigate how easy it would
be to construct an explicit programmatic representation of the full
joint PDF of a Bayes network.  Whether writing all this code to answer
three homework problems is an efficient use of time, I'll leave for
the reader to decide...

The code is in: [BayesNet.hs](BayesNet.hs), [cancer.hs](cancer.hs) and
[happiness.hs](happiness.hs).  There's also code implementing the
earthquake alarm example from the lectures in [alarm.hs](alarm.hs).

[ai-class]: http://www.ai-class.com/
[bns]: http://en.wikipedia.org/wiki/Bayesian_network
[aima]: http://aima.cs.berkeley.edu/
