---
author: Ian
tags: AI,haskell,programming
published: 2011-10-16 21:34:14
title: "AI Class: A* Search"
---
The first week of the online [Stanford AI class][ai-class] has gone
by.  The format of the presentation is pretty good, with lots of short
lecture videos chained together with little quizzes embedded.  It's
really pretty neat.  There are some minor errors in some of the
lectures, which is normal, though in this kind of online setting, it's
more difficult for the keener students to correct the lecturer as the
lecture goes along, so some people are likely to spend a bit of time
confused because of that.  Some people have complained on Reddit about
the relatively superficial level of the coverage of the lectures, but
as far as I'm concerned, that's what the book (and papers) are for.
The course is called "Introduction to Artificial Intelligence", after
all.

<!--NOTEASERBEGIN-->

## Motivation: Take me home, Romanian roads ##

<!--NOTEASEREND-->

The two things we covered this week were a very quick introduction to
the field, then some stuff about search.  Although there is a big pile
of code available for download associated with Russell & Norvig's [AI
book][aima] which would have made playing with some of this stuff
easier, I decided to write some code of my own in Haskell, mostly for
my own amusement.  I wanted to implement A* search, with a clean
interface for setting up problems.

<!--MORE-->

The example used in the lectures, of finding driving routes on a
simple road map of Romania, is a good simple problem to look at.  I
wanted the definition of this problem to look something like this:

~~~~ {.haskell}
import AStar

import Data.Maybe
import Data.List

data Rom = A | B | C | D | E | F | G | H | I | L | M |
           N | O | P | R | S | T | U | V | Z
         deriving (Eq, Ord, Show)

initial :: Rom
initial = A

goal :: Rom -> Bool
goal = (== B)

distances :: [((Rom, Rom), Int)]
distances = [((A, S), 140), ((A, T), 118), ((A, Z), 75), ((B, F), 211),
             ((B, G), 90), ((B, P), 101), ((B, U), 85), ((C, D), 120),
             ((C, P), 138), ((C, R), 146), ((D, M), 75), ((E, H), 86),
             ((F, S), 99), ((H, U), 98), ((I, N), 87), ((I, V), 92),
             ((L, M), 70), ((L, T), 111), ((O, S), 151), ((O, Z), 71),
             ((P, R), 97), ((R, S), 80), ((U, V), 142)]

euclidean :: [(Rom, Int)]
euclidean = [(A, 366), (B, 0), (C, 160), (D, 242), (E, 161), (F, 176),
             (G, 77), (H, 151), (I, 226), (L, 244), (M, 241), (N, 234),
             (O, 380), (P, 100), (R, 193), (S, 253), (T, 329), (U, 80),
             (V, 199), (Z, 374)]

instance SearchState Rom where
  actions x = map const $ nub $ filter (/= x) $ (map fst ps) ++ (map snd ps)
    where ps = map fst $ filter (\((a, b), _) -> a == x || b == x) distances

  stepCost x y
    | x < y = fromJust $ lookup (x, y) distances
    | otherwise = fromJust $ lookup (y, x) distances

heuristic :: Rom -> Int
heuristic x = fromJust $ lookup x euclidean
~~~~

Here, we define a data type `Rom` to represent Romanian cities, define
an initial state and a goal function, set up a couple of data
structures to record city-to-city road distances and Euclidean
distances of cities from Bucharest (our goal), then define `Rom` as an
instance of the type class `SearchState` which includes code to find
the possible actions we can take from any state (i.e. the cities we
can travel to directly from a given city) and the costs
(i.e. distance) associated with going from one state to another.
Finally, we define a heuristic, which just returns the Euclidean
distance from a given city to the goal city.  Given that definition, I
wanted to be able to type something like

~~~~ {.haskell}
astar initial goal heuristic
~~~~

to run an A* search for the goal state, starting at the initial state
and using the given heuristic.  We might also want to look at `astar
initial goal (const 0)` to do a uniform cost search.  I'd like to both
get a result and a count of the number of search tree nodes expanded
(which will allow me to see how much of a difference different
heuristics make to the efficiency of the search).

## A* implementation ##

To make this work, the `AStar` module is defined as follows.  It's
basically one type class, some supporting scaffolding, and the `astar`
search function.  We need a few data structures to implement A* search
efficiently, so we import, in particular, `Data.Set` for sets and
`Data.PSQueue` for priority search queues (priority queues that also
support fast membership testing):

~~~~ {.haskell}
module AStar where

import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Set as S
import qualified Data.PSQueue as PS
import Debug.Trace
~~~~

The `SearchState` type class is defined as:

~~~~ {.haskell}
class SearchState a where
  actions :: a -> [a -> a]
  pathCost :: [a] -> Int
  stepCost :: a -> a -> Int
  stepCost _ _ = 1
  pathCost sts = sum $ zipWith stepCost (tail sts) (init sts)
~~~~

First, for a given search state, we want to find the actions that we
can perform from that state.  The `actions` method of the type class
does this, returning a list of functions that will transform one state
to another -- we can then just apply these functions to a state to get
the result of any action.  We also want to be able to calculate the
costs of paths composed of states, either in one go, from a list of
states, using `pathCost`, or step by step, which is what `stepCost`
does.  We define defaults for `stepCost` (each state change has a cost
of 1) and `pathCost` (the cost of a path through a list of states is
just the sum of the individual steps).

To maintain a record of the search tree, we need a node type, holding
a state, possibly a parent (`Nothing` if this is the initial node) and
the cost at this node.  Note that states can nominally appear in
multiple nodes if a state is reached via multiple paths in the search
graph (in fact, in the search function below, we only keep the best
path to any particular state, so at any time, at most one node is
associated with any state).  We make our nodes an instance of `Ord` so
that we can put them into data structures that require comparison.  We
set things up so that the comparison is done on the *cost* of the
nodes.

~~~~ {.haskell}
data NodeInfo a = NodeInfo { state :: a,
                             parent :: Maybe (NodeInfo a),
                             cost :: Int } deriving (Eq, Show)

instance Eq a => Ord (NodeInfo a) where
  compare = comparing cost
~~~~

The main search function is a little complicated, but it has 7 main
steps:

1. Initialise the search frontier with a node for the initial state
   (with no parent and zero cost).
2. If the goal has been reached, extract the path from the initial to
   the goal node from the search tree and return it, along with a
   count of the number of search tree nodes expanded to find the
   solution.
3. As long as there are nodes in the frontier, pick the lowest cost
   node for expansion (the frontier is represented using a priority
   search queue, so this minimum cost extraction is $O(log n)$ in
   time).
4. Add the node to be expanded to the "explored" set.
5. Determine the actions from the state of the node to be expanded,
   the result states of those actions and the costs of the possible
   result states (here, "cost" means the known path cost to the state
   of the node being expanded, plus the heuristic cost estimate for
   the result state).
6. Combine the existing frontier (minus the node being expanded) with
   the new states: any states not already in the frontier are added,
   and states that already exist in the frontier have their paths and
   costs updated if we have found a better path to the state than the
   one already recorded in the search tree.
7. Repeat from step 3.

Here's a Haskell implementation:

~~~~ {.haskell}
astar :: (Show a, Eq a, SearchState a, Ord a) =>
         a -> (a -> Bool) -> Maybe [a]
astar init goalTest = helper initFrontier S.empty
  where -- Initial frontier contains just the single initial node (no
        -- parent, zero cost).
        initFrontier = PS.singleton init (NodeInfo init Nothing 0)

        -- Tail recursive helper.
        helper fr ex
          | PS.null fr = Nothing   -- Fail if the frontier is empty.
          | otherwise = if (goalTest s)
                        then Just (reverse $ res s p)
                        else helper fr'' ex'
            where -- Extract next state to expand and associated
                  -- information.  We produce trace output to show the
                  -- states on the frontier.
                  Just (s PS.:-> ni@(NodeInfo _ p c), fr') =
                    PS.minView (traceShow (PS.keys fr) fr)

                  -- Add to explored set.
                  ex' = S.insert s ex

                  -- Determine possible actions from this state and
                  -- resulting state, filtering out any in the
                  -- explored set.
                  rs = filter (\r -> not (r `S.member` ex')) $
                       map (result s) (actions s)

                  -- Calculate full costs of possible result states.
                  cs = map (\r -> c + stepCost s r + heuristic r) rs

                  -- Find which of the new states are already in the
                  -- queue.
                  exis = map (\n -> PS.lookup n fr) rs

                  -- Combine the existing states in the queue and the
                  -- new ones we've found.  Only ever keep one entry
                  -- in the queue for each state, the one with the
                  -- smallest cost.
                  toadd = catMaybes $ zipWith3 combine rs cs exis
                  combine r c Nothing = Just (r, NodeInfo r (Just ni) c)
                  combine r c (Just (NodeInfo _ _ c'))
                    | c < c' = Just (r, NodeInfo r (Just ni) c)
                    | otherwise = Nothing

                  -- Insert new states into new frontier.
                  fr'' = foldl (\q (k, p) -> PS.insert k p q) fr' toadd

                  -- Build the result by following the parent links
                  -- through the search graph.
                  res n Nothing = [init]
                  res n (Just p) = n : res (state p) (parent p)
~~~~

This is a bit more verbose than the implementations in the
`Data.Graph` module, but it works fine and is relatively efficient.
Here's what we get for the Romanian roads problem:

~~~~ {.haskell}
*Main> astar initial goal (const 0)
[A]
[S,T,Z]
[O,S,T]
[L,O,S]
[F,L,O,R]
[F,L,R]
[C,F,L,P]
[C,F,M,P]
[B,C,M,P]
[B,C,D,P]
[B,C,D]
[B,D]
[B]
Just ([A,S,R,P,B],13)
*Main> astar initial goal heuristic
[A]
[S,T,Z]
[F,O,R,T,Z]
[C,F,O,P,T,Z]
[B,C,O,P,T,Z]
[B,C,O,T,Z]
Just ([A,S,R,P,B],6)
*Main>
~~~~

First, we try the search using uniform cost search (i.e. a heuristic
function of `(const 0)`).  This gives us the right answer (as reported
in the lectures), and expands 13 nodes in the search tree to find the
solution.  Then, using the Euclidean distance as a heuristic for the
actual distance by road, we get the same answer (guaranteed since our
heuristic is admissible, i.e. optimistic) by only expanding 6 search
tree nodes.  You can look at the order in which the nodes in the
search tree are expanded and you'll see that they correspond to the
discussion in the lecture.

## Homework problem ##

Let's look at how to set up a couple of other problems.  First, one of
the homework problems dealt with a very simple setting: a 4 &times; 6
grid where we need to find a route from the top left to the bottom
right moving only horizontally or vertically at each step, with a
given heuristic function at each grid cell to estimate the remaining
distance to the goal cell (numbers in the grid cells here show the
heuristic value and the circles mark the initial and goal states):

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\begin{scope}[very thick]
\foreach \x in {1,...,6}
  \foreach \y in {1,...,4}
  {
    \draw (\x,\y) +(-.5,-.5) rectangle ++(.5,.5);
  }
\end{scope}

\foreach \x in {1,...,6} \draw (\x,5) node{\x};
\draw (0,4) node{A};
\draw (0,3) node{B};
\draw (0,2) node{C};
\draw (0,1) node{D};

\foreach \x in {1,...,6} \draw (\x,1) node{1};
\foreach \y in {2,...,4} \draw (6,\y) node{1};
\foreach \x in {1,...,5} \draw (\x,2) node{2};
\foreach \y in {3,...,4} \draw (5,\y) node{2};
\foreach \x in {1,...,4} \draw (\x,3) node{3};
\draw (4,4) node{3};
\foreach \x in {1,...,3} \draw (\x,4) node{4};

\draw (1,4) circle (0.4);
\draw (6,1) circle (0.4);
@@@@@

In our framework, this problem looks like this:

~~~~ {.haskell}
import AStar

data HWState = HWState Int Int
             deriving (Eq, Ord)

instance Show HWState where
  show (HWState r c) = ("ABCD" !! (r-1)) : (show c)

instance SearchState HWState where
  actions (HWState r c) =
    map (const . uncurry HWState) $
    filter valid [ (r-1, c), (r+1, c), (r, c-1), (r, c+1) ]
      where valid (r, c) = r >= 1 && r <= 4 && c >= 1 && c <= 6

initial :: HWState
initial = HWState 1 1

goal :: HWState -> Bool
goal (HWState 4 6) = True
goal (HWState _ _) = False

heuristic :: HWState -> Int
heuristic (HWState 4 _) = 1
heuristic (HWState _ 6) = 1
heuristic (HWState 3 _) = 2
heuristic (HWState _ 5) = 2
heuristic (HWState 2 _) = 3
heuristic (HWState _ 4) = 3
heuristic (HWState _ _) = 4
~~~~

The problem state, defined by the `HWState` type, just gives the row
and column where we are at this step, we define a `Show` instance for
the state so we can see what's going on, we define an initial state
(top left) and a function to test whether a state is the goal state
(easy here: are we at the bottom right?).  Then we get to the meat of
the code, which is an instance definition for the `SearchState` class
(defined in the `AStar` module).  Here, this just has a method to find
out what possible actions we can take from a given state (moving to
any of the adjacent cells, module edge effects).  We use the default
path cost definition from the `SearchState` type class.  Finally, we
have a function to calculate the suggested heuristic for a given
state.  This all works as expected (I've taken out the `traceShow`
call, so we don't see the states that are expanded at each step),
although here the heuristic doesn't seem to help at all:

~~~~ {.haskell}
*Main> astar initial goal (const 0)
Just ([A1,A2,A3,A4,A5,A6,B6,C6,D6],24)
*Main> astar initial goal heuristic
Just ([A1,A2,A3,A4,A5,A6,B6,C6,D6],24)
*Main>
~~~~

## Sliding blocks puzzles ##

Let's try a more substantial example.  The sliding blocks puzzles of
the 15-Puzzle kind are a classic search problem.  Things are quite a
bit more complicated here than in the two previous examples.  The
boards for these puzzles look like this (for the 3 &times; 3
"8-puzzle" version on the left and the 4 &times; 4 "15-puzzle" version
on the right):

@@@@@ { display: block; margin-left: auto; margin-right: auto; }
\begin{scope}[very thick]
\foreach \x in {1,...,3}
  \foreach \y in {1,...,3}
  {
    \draw (\x,\y) +(-.5,-.5) rectangle ++(.5,.5);
  }
\end{scope}

\foreach \x in {1,...,3}
  \foreach \y in {2,...,3}
  {
    \draw (\x,\y) +(-.425,-.425) rectangle ++(.425,.425);
  }

\foreach \x in {1,...,2}
  {
    \draw (\x,1) +(-.425,-.425) rectangle ++(.425,.425);
  }

\draw (1,3) node{1};
\draw (2,3) node{2};
\draw (3,3) node{3};
\draw (1,2) node{4};
\draw (2,2) node{5};
\draw (3,2) node{6};
\draw (1,1) node{7};
\draw (2,1) node{8};


\begin{scope}[very thick]
\foreach \x in {6,...,9}
  \foreach \y in {1,...,4}
  {
    \draw (\x,\y) +(-.5,-.5) rectangle ++(.5,.5);
  }
\end{scope}

\foreach \x in {6,...,9}
  \foreach \y in {2,...,4}
  {
    \draw (\x,\y) +(-.425,-.425) rectangle ++(.425,.425);
  }

\foreach \x in {6,...,8}
  {
    \draw (\x,1) +(-.425,-.425) rectangle ++(.425,.425);
  }

\draw (6,4) node{1};
\draw (7,4) node{2};
\draw (8,4) node{3};
\draw (9,4) node{4};
\draw (6,3) node{5};
\draw (7,3) node{6};
\draw (8,3) node{7};
\draw (9,3) node{8};
\draw (6,2) node{9};
\draw (7,2) node{10};
\draw (8,2) node{11};
\draw (9,2) node{12};
\draw (6,1) node{13};
\draw (7,1) node{14};
\draw (8,1) node{15};
@@@@@

<br>

First we need some imports (we'll use a map to help represent the
board state, and we'll need some random numbers to generate scrambled
initial board states).

~~~~ {.haskell}
import Prelude hiding (Left, Right)
import qualified Data.Map as M
import System.Random

import AStar
~~~~

Next, we define our board state.  We need to know how big the board is
(typically, we'll consider the 3 &times; 3 "8-puzzle" and the 4
&times; 4 "15-puzzle"), and what tiles are in each position, which we
represent as a map from a `(row, column)` tuple to an integer tile
value.  For convenience, we also record the row and column where the
blank is.

~~~~ {.haskell}
type Board = M.Map (Int, Int) Int
data BoardState = BoardState { size :: Int,
                               board :: Board,
                               bpos :: (Int, Int) } deriving (Eq, Ord)
~~~~

We often need to generate the list of all coordinate pairs for the
board, so let's have a helper function that does that for a given
board size.

~~~~ {.haskell}
coords :: Int -> [(Int, Int)]
coords n = [(r, c) | r <- [1..n], c <- [1..n]]
~~~~

Moves in these puzzles are most easily thought of in terms of moving
the blank, rather than moving a tile.  Any single move just moves the
blank by one square, either up, down, left or right.  Here, if we try
to apply an invalid move to a board state (e.g. moving the blank off
the left hand side of the board), we define the result to be an
unchanged board.  This is convenient when we come to generate
scrambled boards, but it's not a problem in the A* search, since we
will never try to apply invalid moves.

~~~~ {.haskell}
data MoveDir = Up | Down | Left | Right

move :: MoveDir -> BoardState -> BoardState
move dir (BoardState n m bpos@(brow, bcol)) = BoardState n m'' bpos'
  where m'' = M.insert bpos' 0 m'
        m' = M.insert bpos (m M.! bpos') m
        bpos' = moveBlank dir
        moveBlank Up = if (brow > 1) then (brow-1, bcol) else (brow, bcol)
        moveBlank Down = if (brow < n) then (brow+1, bcol) else (brow, bcol)
        moveBlank Left = if (bcol > 1) then (brow, bcol-1) else (brow, bcol)
        moveBlank Right = if (bcol < n) then (brow, bcol+1) else (brow, bcol)
~~~~

We add a `Show` instance for the board state so that we can see what's
going on.

~~~~ {.haskell}
instance Show BoardState where
  show (BoardState n m _) =
    unlines $ map concat $ chunks n $ map (format . (m M.!)) (coords n)
      where format x
              | x == 0 = " XX"
              | 1 <= x && x <= 9 = "  " ++ show x
              | otherwise = " " ++ show x
            chunks _ [] = []
            chunks n xs = (take n xs) : chunks n (drop n xs)
~~~~

Then, the `SearchState` instance is a bit of an anticlimax, since all
we need to do is to generate the possible valid moves given a board
state.  We use the default implementation of path costs from
`SearchState`, with each moving having the same cost.

~~~~ {.haskell}
instance SearchState BoardState where
  actions (BoardState n m (brow, bcol)) = map move (lr ++ ud)
    where lr
            | bcol == 1 = [Right]
            | bcol == n = [Left]
            | otherwise = [Left, Right]
          ud
            | brow == 1 = [Down]
            | brow == n = [Up]
            | otherwise = [Up, Down]
~~~~

The "initial" state (we'll actually start our searches from a
scrambled state derived from this):

~~~~ {.haskell}
initial :: Int -> BoardState
initial n = BoardState n
            (M.fromList $ zip (coords n) ([1..n^2-1] ++ [0])) (n, n)
~~~~

and the goal state is the same:

~~~~ {.haskell}
goal :: BoardState -> Bool
goal (BoardState n m _) = map (m M.!) (coords n) == ([1..n^2-1] ++ [0])
~~~~

There are two possible heuristics that were mentioned in the
lectures.  The first just counts the number of tiles that are not in
the correct position:

~~~~ {.haskell}
heuristic1 :: BoardState -> Int
heuristic1 (BoardState n m _) = sum $ zipWith
                                (\x y -> if x /= y then 1 else 0)
                                (map (m M.!) (init $ coords n)) [1..n^2-1]
~~~~

The second measures the Manhattan distance of each tile from its
correct position and uses the sum of these distances for all the tiles
as the heuristic value:

~~~~ {.haskell}
heuristic2 :: BoardState -> Int
heuristic2 (BoardState n m _) = M.foldrWithKey folder 0 m
  where folder (r, c) v s = s + if (v > 0) then dist (r, c) (goodPos v) else 0
        dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)
        goodPos v = ((v - 1) `div` n + 1, (v - 1) `rem` n + 1)
~~~~

Finally, we need to be able to generate scrambled starting states.
This is most easily done by generating a random list of moves and
applying them to the initial board state.  Since we ignore invalid
moves in the `move` function above, this all works fine.

~~~~ {.haskell}
scramble :: Int -> BoardState -> IO BoardState
scramble n b = do
  g <- getStdGen
  return $ foldl (flip move) b (makeMoves g)
    where makeMoves gen = map ([Up, Down, Left, Right] !!)
                          (take n $ randomRs (0,3) gen :: [Int])
~~~~

And then everything works as you might expect:

~~~~ {.haskell}
*Main> b0 <- scramble 20 (initial 3)
*Main> b0
  5  1 XX
  4  2  3
  7  8  6

*Main> heuristic2 b0
6
*Main> astar b0 goal heuristic2
Just ([  5  1 XX
  4  2  3
  7  8  6
,  5  1  3
<< lines skipped >>
  7 XX  8
,  1  2  3
  4  5  6
  7  8 XX
],54)
*Main>
~~~~

Here, we generate a scrambled state for the 8-puzzle from the initial
"perfect" state by applying 20 randomly selected moves.  We display
the scrambled initial state, the value of the Manhattan distance
heuristic for this state (which gives us some idea of how much effort
we'll have to expend to find a solution), then run the A* search using
the Manhattan distance heuristic.  The result is a list of states
(some of the output snipped) and a count of the number of search tree
nodes expanded (54, in this case).

In a post in a day or two, I'll look a little at how much of a
difference using heuristics really makes in this problem...

[ai-class]: http://www.ai-class.com/
[aima]: http://aima.cs.berkeley.edu/
