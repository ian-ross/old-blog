-- We treat our Bayesian networks as an abstract data type, exporting
-- only a way to construct a Bayesian network from a simple list-based
-- description, and to calculate probability values given a network.
--
-- The implementation of probability calculations here is pretty dumb,
-- using simple enumeration over possibilities, although we do it in a
-- mildly cute functional way by constructing a functional
-- representation of the full joint PDF of the network.

--module BayesNet(fromList, prob, jointProb, jointProbTF) where
module BayesNet where

import qualified Data.Map as M
import Data.List


-- The internal representation of a node in the network is as a list
-- of nodes on which this node is conditional, and a probability
-- table.  Nodes are parameterised by type e ("event") and the type of
-- probability values is parameterised too, so that we can use, for
-- instance, rational values if we want.

data BayesNode e p = BayesNode { cond :: [e], ps :: [p] } 
                   deriving (Eq, Ord, Show)

-- A Bayesian network is then just a map from node labels to nodes.

newtype BayesNet e p = BayesNet (M.Map e (BayesNode e p)) 
                     deriving (Eq, Ord, Show)


-- The list representation of a Bayesian network is a list of nodes of
-- the form:
--
--   (event, conditions, probabilities)
--
-- where: 
--
--  - event is a label for the node (of type e in all the function
--    type signatures here);
--  - conditions is a list of the events on which this event is
--    conditional (i.e. a list of type [e]); and
--  - probabilities is a table of probability values, stored as a flat
--    list.  The type of these values (represented as type variable p
--    everywhere) can be any fractional numeric type.  The values are
--    stored in lexical order of the truth value assignment to the
--    conditions variables, where True sorts before False.  For
--    instance, if conditions is [R,S], then the probability values
--    are in the order +R,+S; +R,-S; -R,+S; -R,-S.
--
-- Creating a network from its list representation is simple.  We
-- check that the probability table has the right number of entries,
-- dependent on the number of conditional variables.  (It would be
-- interesting to try to apply this constraint at the type level.  No
-- idea how to do that.)

fromList :: (Ord e, Fractional p) => [(e, [e], [p])] -> BayesNet e p
fromList = BayesNet . (foldl doOne M.empty)
  where doOne m (ev, cond, ps) = 
          if (length ps /= 2^(length cond)) 
          then error "Invalid length for probability table"
          else M.insert ev (BayesNode cond ps) m


-- Functions to calculate probabilities, given a Bayesian network.
-- First, the probability that a single event is true, given
-- conditions (true or false) on a set of other events.

prob :: (Ord e, Fractional p) => BayesNet e p -> e -> [(e,Bool)] -> p
prob bn ev cond = jointProb bn [ev] cond

-- Then, the joint probability that a set of events is true, given
-- conditions (true or false) on a set of other events.

jointProb :: (Ord e, Fractional p) => BayesNet e p -> [e] -> [(e,Bool)] -> p
jointProb bn evs cond = jointProbTF bn (zip evs $ repeat True) cond

-- The most general case: the probability that a set of events is true
-- or false, as specified, given conditions (true or false) on a set
-- of other events.  
--
-- We do this by calculating the joint PDF of the whole network, then
-- enumerating the possible assignments of truth values to the free
-- variables in the numerator and denominator of the representation of
-- P(X | Y) as P(X ^ Y) / P(Y), applying the joint PDF to each of
-- these variable assignments, summing the results and forming the
-- fraction representing the conditional probability.

jointProbTF :: (Ord e, Fractional p) => 
               BayesNet e p -> [(e,Bool)] -> [(e,Bool)] -> p
jointProbTF bn@(BayesNet m) evs cond = eval (cond ++ evs) / eval cond
  where eval = sum . (map (jointPDF bn)) . (enumerate bn)


-- Calculate the joint PDF of the whole network.  The PDF is
-- represented as a function that takes a map from node labels to
-- Boolean values and returns a probability value.  The PDF is found
-- by forming the product of the individual PDF factors for each node
-- in the network, applying each of these factor functions to the
-- input variable assignment, and forming the product.
        
jointPDF :: (Ord e, Fractional p) => BayesNet e p -> (M.Map e Bool -> p)
jointPDF bn@(BayesNet m) = 
  (\vals -> product $ zipWith (applyFactor vals) (M.keys m) factors)
  where factors = map (jointPDFFactor bn) (M.keys m)
        applyFactor vs k f = f (vs M.! k) (map (vs M.!) (cond $ m M.! k))

-- Return a function representing a single factor in the full joint
-- PDF.  This function, representing a factor of the form P(X | [Y])
-- takes a single Boolean value representing the value of X, and a
-- list of Boolean values representing the values of Y, and returns
-- P(X | [Y]).  To do this, we need to know the predecessors of the
-- node in question in the graph of the network, we need to extract
-- the correct value from the node's probability table, and we need to
-- deal with complementary probability values.
        
jointPDFFactor :: (Ord e, Fractional p) => 
                  BayesNet e p -> e -> (Bool -> [Bool] -> p)
jointPDFFactor bn@(BayesNet m) ev =
  (\b bs -> let pval = pvals !! (length pvals - idx - 1)
                idx = foldl (\i b -> (if b then 1 else 0) + 2 * i) 0 bs
            in if b then pval else (1 - pval))
  where pvals = ps (m M.! ev)


-- Enumerate possible assignments of values to the Boolean random
-- variables in a network, given that some variables have fixed
-- values.

enumerate :: (Ord e, Fractional p) => 
             BayesNet e p -> [(e,Bool)] -> [M.Map e Bool]
enumerate bn@(BayesNet m) fixed = map compose msks
  where base = M.fromList fixed
        vars = (M.keys m) \\ (map fst fixed)
        msks = masks (length vars)
        compose = (M.union base) . M.fromList . (zip vars)

-- Helper function to generate combinations of Boolean values.

masks :: Int -> [[Bool]]
masks 0 = [[]]
masks 1 = [[False], [True]]
masks n = map (False :) xs' ++ map (True :) xs'
  where xs' = masks (n - 1)
