-- HOMEWORK PROBLEM DEFINITION

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
  heuristic (HWState 4 _) = 1
  heuristic (HWState _ 6) = 1
  heuristic (HWState 3 _) = 2
  heuristic (HWState _ 5) = 2
  heuristic (HWState 2 _) = 3
  heuristic (HWState _ 4) = 3
  heuristic (HWState _ _) = 4

initialHWState :: HWState
initialHWState = HWState 1 1

goalTestHW :: HWState -> Bool
goalTestHW (HWState 4 6) = True
goalTestHW (HWState _ _) = False
