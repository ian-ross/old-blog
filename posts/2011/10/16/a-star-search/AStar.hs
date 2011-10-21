module AStar where

import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Set as S
import qualified Data.PSQueue as PS
import Debug.Trace

class SearchState a where
  actions :: a -> [a -> a]
  result :: a -> (a -> a) -> a
  pathCost :: [a] -> Int
  stepCost :: a -> a -> Int
  heuristic :: a -> Int
  result s a = a $ s
  stepCost _ _ = 1
  pathCost sts = sum $ zipWith stepCost (tail sts) (init sts)
  heuristic _ = 0

data NodeInfo a = NodeInfo { state :: a,
                             parent :: Maybe (NodeInfo a), 
                             cost :: Int } deriving (Eq, Show)

instance Eq a => Ord (NodeInfo a) where
  compare = comparing cost


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
            where -- Extract next state to expand and associated information.
                  Just (s PS.:-> ni@(NodeInfo _ p c), fr') = PS.minView fr
                  
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
