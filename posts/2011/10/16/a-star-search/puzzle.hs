-- 8/15 PUZZLE PROBLEM DEFINITION

import Prelude hiding (Left, Right)
import qualified Data.Map as M
import System.Random

import AStar

type Board = M.Map (Int, Int) Int
data BoardState = BoardState { size :: Int,
                               board :: Board,
                               brow :: Int,
                               bcol :: Int } deriving (Eq, Ord)

coords :: Int -> [(Int, Int)]
coords n = [(r, c) | r <- [1..n], c <- [1..n]]

data MoveDir = Up | Down | Left | Right

move :: MoveDir -> BoardState -> BoardState
move dir (BoardState n m brow bcol) = BoardState n m'' brow' bcol'
  where m'' = M.insert (brow', bcol') 0 m'
        m' = M.insert (brow, bcol) (m M.! (brow', bcol')) m
        (brow', bcol') = moveBlank dir
        moveBlank Up = if (brow > 1) then (brow-1, bcol) else (brow, bcol)
        moveBlank Down = if (brow < n) then (brow+1, bcol) else (brow, bcol)
        moveBlank Left = if (bcol > 1) then (brow, bcol-1) else (brow, bcol)
        moveBlank Right = if (bcol < n) then (brow, bcol+1) else (brow, bcol)

instance Show BoardState where
  show (BoardState n m _ _) = 
    unlines $ map concat $ chunks n $ map (format . (m M.!)) (coords n)
      where format x
              | x == 0 = " XX"
              | 1 <= x && x <= 9 = "  " ++ show x
              | otherwise = " " ++ show x
            chunks _ [] = []
            chunks n xs = (take n xs) : chunks n (drop n xs)

instance SearchState BoardState where
  actions (BoardState n m brow bcol) = map move (lr ++ ud)
    where lr
            | bcol == 1 = [Right]
            | bcol == n = [Left]
            | otherwise = [Left, Right]
          ud
            | brow == 1 = [Down]
            | brow == n = [Up]
            | otherwise = [Up, Down]
  -- heuristic (BoardState (n, m, _, _)) = sum $ zipWith 
  --                                      (\x y -> if x /= y then 1 else 0) 
  --                                      (map (m M.!) (init $ coords n)) [1..n^2-1]
  heuristic (BoardState n m _ _) = M.foldrWithKey folder 0 m
    where folder (r, c) v s = s + if (v > 0) then dist (r, c) (goodPos v) else 0
          dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)
          goodPos v = ((v - 1) `div` n + 1, (v - 1) `rem` n + 1)

initialBoardState :: Int -> BoardState
initialBoardState n = BoardState n (M.fromList $ zip (coords n) ([1..n^2-1] ++ [0])) n n

goalTestBoard :: BoardState -> Bool
goalTestBoard (BoardState n m _ _) = map (m M.!) (coords n) == ([1..n^2-1] ++ [0])

scramble :: Int -> BoardState -> IO BoardState
scramble n b = do
  g <- getStdGen
  return $ foldl (flip move) b (makeMoves g)
    where makeMoves gen = map ([Up, Down, Left, Right] !!) 
                          (take n $ randomRs (0,3) gen :: [Int])
