{-# LANGUAGE FlexibleInstances #-}

module Data.Constraint.Simplex where

import qualified Data.Map as M
import Data.List
import Data.Array
import Control.Monad.Writer
import Text.Printf
--import Matrix.Simplex  (The code for this is cut and pasted below.)

--------------------------------------------------
--
--  EXPRESSIONS
--

-- Linear expressions with variables labelled by values of type a: map
-- from variable names to coefficients, plus constant term.

data Expr a = Expr { vars :: M.Map a Double, numval :: Double }
              deriving (Show, Eq)

exprVars :: Expr a -> [a]
exprVars (Expr vs _) = M.keys vs

(+!+) :: Ord a => Expr a -> a -> Double
(Expr vs _) +!+ v = M.findWithDefault 0.0 v vs


-- Make expressions behave like numbers.

instance (Show a, Ord a) => Num (Expr a) where
  (Expr vs1 c1) + (Expr vs2 c2) = Expr (M.unionWith (+) vs1 vs2) (c1 + c2)
  (Expr vs1 c1) * (Expr vs2 c2) | M.null vs1 = Expr (M.map (* c1) vs2) (c2 * c1)
                               | M.null vs2 = Expr (M.map (* c2) vs1) (c2 * c1)
                               | otherwise = error "non-linear operation"
  abs (Expr vs c) = Expr (M.map abs vs) (abs c)
  signum (Expr vs c) = Expr (M.map signum vs) (signum c)
  negate (Expr vs c) = Expr (M.map negate vs) (negate c)
  fromInteger i = Expr M.empty (fromInteger i)
  
instance (Show a, Ord a) => Fractional (Expr a) where
  (Expr vs1 c1) / (Expr vs2 c2) | M.null vs2 = Expr (M.map (/ c2) vs1) (c1 / c2)
                               | otherwise = error "non-linear operation"
  recip (Expr vs c) | M.null vs = Expr (M.empty) (recip c)
                    | otherwise = error "non-linear operation"
  fromRational r = Expr M.empty (fromRational r)


-- Create new variables.
  
var :: Ord a => a -> Expr a
var vn = Expr (M.singleton vn 1.0) 0.0


-- Comparison between expressions, i.e. constraints.

data Comparison = LEQ | GEQ | EQL deriving (Eq, Ord, Show)

data Constraint a = Constraint { lhs :: Expr a, comp :: Comparison, rhs :: Expr a } 
                  deriving Show

conVars :: Eq a => Constraint a -> [a]
conVars (Constraint e1 _ e2) = exprVars e1 `union` exprVars e2

(<=#) :: Expr a -> Expr a -> Constraint a
e1 <=# e2 = Constraint e1 LEQ e2

(==#) :: Expr a -> Expr a -> Constraint a
e1 ==# e2 = Constraint e1 EQL e2

(>=#) :: Expr a -> Expr a -> Constraint a
e1 >=# e2 = Constraint e1 GEQ e2

infix 4 <=#, ==#, >=#


-- A constraint system has an objective function and a list of
-- constraints.

type ConstraintSystem a = (Expr a, [Constraint a])

sysVars :: Eq a => ConstraintSystem a -> [a]
sysVars (obj, cs) = exprVars obj `union` foldr union [] (map conVars cs)


-- Some little helpers to build systems using a writer monad.

con c = tell [c]
buildSystem = runWriter


--------------------------------------------------
--
--  SLACK VARIABLE INTRODUCTION
--

makeSlacks :: (Show a, Ord a, Slack a) => ConstraintSystem a -> ConstraintSystem a
makeSlacks (obj, cs) = (obj, zipWith makeSlack cs [1..])
  where makeSlack (Constraint e1 EQL e2) _ = (e1 - e2 ==# 0)
        makeSlack (Constraint e1 GEQ e2) s = (e1 - e2 - var (genSlk s) ==# 0)
        makeSlack (Constraint e1 LEQ e2) s = (e1 - e2 + var (genSlk s) ==# 0)

class Slack a where
  genSlk :: Int -> a
  
instance Slack [Char] where
  genSlk s = "slk_" ++ show s


--------------------------------------------------
--
--  TABLEAU CREATION
--

makeTableau :: Ord a => ConstraintSystem a -> Array (Int,Int) Double
makeTableau sys@(obj, cs) = array ((0,0), (m,n)) $ zv ++ cv ++ bv ++ av
  where m = length cs
        n = length svs
        svs = sysVars sys
        zv = [((0,0), 0.0)]
        cv = [((0,i), obj +!+ (svs !! (i-1))) | i <- [1..n]]
        bv = [((i,0), (negate . numval . lhs) $ cs !! (i-1)) | i <- [1..m]]
        av = [((i,j), (lhs $ cs !! (i-1)) +!+ (svs !! (j-1))) | i <- [1..m], j <- [1..n]]


--------------------------------------------------
--
--  RESULT EXTRACTION
--

extractResults :: Ord a => Array (Int,Int) Double -> ConstraintSystem a -> M.Map a Double
extractResults t sys = M.fromList $ zip vs vals
  where b = [t ! (i,0) | i <- [1..n]]
        a = [[t ! (i,j) | i <- [1..n]] | j <- [1..length vs]]
        res = map unitCheck a
        vals = map (\pos -> if (pos < 0) then 0.0 else b !! pos) res
        (_, (n,m)) = bounds t
        vs = sysVars sys

unitCheck :: [Double] -> Int
unitCheck xs = unitCheck' 0 (-1) xs

unitCheck' :: Int -> Int -> [Double] -> Int
unitCheck' idx pos [x]
  | fuzz x 0.0 = pos
  | fuzz x 1.0 = if pos >= 0 then -1 else idx
  | otherwise = -1
unitCheck' idx pos (x:xs)
  | fuzz x 0.0 = unitCheck' (idx+1) pos xs
  | fuzz x 1.0 = if pos >= 0 then -1 else unitCheck' (idx+1) idx xs
  | otherwise = -1

fuzz :: Double -> Double -> Bool
fuzz x y = abs (x - y) < eps


--------------------------------------------------
--
--  DRIVER FUNCTION
--

data SimplexError = Unbounded | Infeasible deriving (Eq, Show)

solve :: (Ord a, Show a, Slack a) => ConstraintSystem a -> 
         Either SimplexError (M.Map a Double)
solve sys = case solved of (Optimal a) -> Right (extractResults a sys)
                           SUnbounded -> Left Unbounded
                           SInfeasible -> Left Infeasible
  where solved = (twophase . makeTableau . makeSlacks) sys


--------------------------------------------------
--
--  USAGE EXAMPLES
--

exampleSystem :: ConstraintSystem String
exampleSystem = buildSystem $ do
  let [x1, x2, x3, x4] = map var ["x1", "x2", "x3", "x4"]
  con $ x1 + 2 * x3 <=# 740
  con $ 2 * x2 - 7 * x4 <=# 0
  con $ x2 - x3 + 2 * x4 >=# 1/2
  con $ x1 + x2 + x3 + x4 ==# 9
  return $ -x1 - x2 - 3*x3 + 1/2*x4

exampleSystem2 :: ConstraintSystem String
exampleSystem2 = buildSystem $ do
  let [x, y, z] = map var ["x", "y", "z"]
  con $ 3*x + 2*y + z <=# 10
  con $ 2*x + 5*y + 3*z <=# 15
  return $ -2*x - 3*y - 4*z

tst :: Expr String
tst = x1 + x2 + 3 * x3 - 1/2 * x4
  where [x1, x2, x3, x4] = map var ["x1", "x2", "x3", "x4"]

simpleSystem :: ConstraintSystem String
simpleSystem = buildSystem $ do
  let [x, y] = map var ["x", "y"]
  con $ x + y <=# 4
  con $ x + 2*y <=# 6
  return $ -2*x - y

simpleSystem2 :: ConstraintSystem String
simpleSystem2 = buildSystem $ do
  let [x, y] = map var ["x", "y"]
  con $ x + y <=# 4
  return $ -2*y - x

abcSystem :: ConstraintSystem String
abcSystem = buildSystem $ do
  let [x1, x2] = map var ["x1", "x2"]
  con $ x1 <=# 2
  con $ x2 <=# 3
  con $ x1 + x2 <=# 4
  return $ -15*x1 - 10*x2


putTableau :: (Fractional a, Show a) => Array (Int,Int) a -> IO ()
putTableau t = do
  putStrLn $ intercalate "\n" $ map concat srs
  where srs = map (map (pad 9 . show)) rs
        rs = [[t ! (r,c) | c<-[cb..ce]] | r<-[rb..re]]
        ((rb,cb), (re,ce)) = bounds t
        pad n s = replicate (n - length s) ' ' ++ s

putTableauF :: (PrintfArg a, Fractional a, Show a) => Array (Int,Int) a -> IO ()
putTableauF t = do
  putStrLn $ intercalate "\n" $ map concat srs
  where srs = map (map (pad 7 . printf "%6.2f")) rs
        rs = [[t ! (r,c) | c<-[cb..ce]] | r<-[rb..re]]
        ((rb,cb), (re,ce)) = bounds t
        pad n s = replicate (n - length s) ' ' ++ s



----------------------------------------------------------------------

-- CUT AND PASTED IN HERE TO AVOID SOME DEPENDENCY ISSUES AND TO MAKE
-- IT EASY TO FIX A MINOR BUG IN THE DELETION OF THE ARTIFICIAL
-- VARIABLES IN THE TWO-PHASE ALGORITHM.

-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Matrix.Simplex
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Two-step simplex algorithm
--
-- I only guarantee that this module wastes inodes
--
-----------------------------------------------------------------------------

-- Originally based off the code in Sedgewick, but modified to match the
-- conventions from Papadimitriou and Steiglitz.

-- TODO: Is our column/row selection the same as Bland's anti-cycle
-- algorithm?

-- TODO: Add check for redundant rows in two-phase algorithm

-- TODO: Lots of testing

--module Matrix.Simplex (Simplex(..), simplex, twophase) where

--import Data.Array

eps :: Double
eps = 1.0e-10

-------------------------------------------------------------------------------

-- Pivot around a!(p,q)

pivot :: Int -> Int -> Array (Int,Int) Double -> Array (Int,Int) Double
pivot p q a0 = step4 $ step3 $ step2 $ step1 $ a0
    where step1 a = a // [ ((j,k), a!(j,k) - a!(p,k) * a!(j,q) / a!(p,q)) | k <- [0..m], j <- [ph..n], j /= p && k /= q ]
          step2 a = a // [ ((j,q),0) | j <- [ph..n], j /= p ]
          step3 a = a // [ ((p,k), a!(p,k) / a!(p,q)) | k <- [0..m], k /= q ]
          step4 a = a // [ ((p,q),1) ]
          ((ph,_),(n,m)) = bounds a0

-- chooseq picks the lowest numbered favorable column.  If there are no
-- favorable columns, then q==m is returned, and we have reached an
-- optimum.


chooseq :: (Ord b, Num b, Ix a, Ix b, Num a) =>
           Array (a, b) Double -> b
chooseq a0 = chooseq' 1 a0
    where chooseq' q a | q > m          = q
                       | a!(0,q) < -eps = q
                       | otherwise      = chooseq' (q+1) a
          ((_,_),(_,m)) = bounds a0

-- choosep picks a row with a positive element in column q.  If no such
-- element exists, then the p==n is returned, and the problem is
-- unfeasible.

choosep :: (Ord a, Num a, Ix a, Ix b) =>
           b -> Array (a, b) Double -> a
choosep q a0 = choosep' 1 a0
    where choosep' p a | p > n         = p
                       | a!(p,q) > eps = p
                       | otherwise     = choosep' (p+1) a
          ((_,_),(n,_)) = bounds a0

-- refinep picks the row using the ratio test.

refinep :: (Ord a, Num a, Num b, Ix a, Ix b) =>
           a -> b -> Array (a, b) Double -> a
refinep p0 q a0 = refinep' (p0+1) p0 a0
    where refinep' i p a | i > n = p
                         | a!(i,q) > eps && a!(i,0) / a!(i,q) < a!(p,0) / a!(p,q) = refinep' (i+1) i a
                         | otherwise = refinep' (i+1) p a
          ((_,_),(n,_)) = bounds a0

-- * Types

-- | Type for results of the simplex algorithm

data Simplex a = SUnbounded | SInfeasible | Optimal a deriving (Read,Show)

-- * Functions

-- | The simplex algorithm for standard form:
--
-- min   c'x
--
-- where Ax = b, x >= 0
--
-- a!(0,0) = -z
--
-- a!(0,j) = c'
--
-- a!(i,0) = b
--
-- a!(i,j) = A_ij

simplex :: Array (Int,Int) Double -- ^ stating tableau
        -> Simplex (Array (Int,Int) Double) -- ^ solution

simplex a | q > m      = Optimal a
          | p > n      = SUnbounded
          | otherwise  = simplex $ pivot p' q $ a
    where q = chooseq a
          p = choosep q a
          p' = refinep p q a
          ((_,_),(n,m)) = bounds a

-------------------------------------------------------------------------------

addart :: (Num e, Enum a, Ix a, Num a) =>
          Array (a, a) e -> Array (a, a) e
addart a = array ((-1,0),(n,m+n)) $ z ++ xsi ++ b ++ art ++ x
    where z = ((-1,0), a!(0,0)) : [ ((-1,j),0) | j <- [1..n] ] ++ [ ((-1,j+n),a!(0,j)) | j <- [1..m] ]
          xsi = ((0,0), -colsum a 0) : [ ((0,j),0) | j <- [1..n] ] ++ [ ((0,j+n), -colsum a j) | j <- [1..m] ]
          b = [ ((i,0), a!(i,0)) | i <- [1..n] ]
          art = [ ((i,j), if i == j then 1 else 0) | i <- [1..n], j <- [1..n] ]
          x = [ ((i,j+n), a!(i,j)) | i <- [1..n], j <- [1..m] ]
          ((_,_),(n,m)) = bounds a

colsum :: (Num e, Num a, Enum a, Ix a, Ix b) =>
          Array (a, b) e -> b -> e
colsum a j = sum [ a!(i,j) | i <- [1..n] ]
    where ((_,_),(n,_)) = bounds a

delart :: (Enum a, Ix a, Num a, Fractional e) =>
          Array (a, a) e -> Array (a, a) e -> Array (a, a) e
delart a a' = array ((0,0),(n,m)) $ z ++ b ++ x
--    where z = ((0,0), a'!(-1,0)) : [ ((0,j), a!(0,j)) | j <- [1..m] ]
    where z = ((0,0), 0.0) : [ ((0,j), a!(0,j)) | j <- [1..m] ]
          b = [ ((i,0), a'!(i,0)) | i <- [1..n] ]
          x = [ ((i,j), a'!(i,j+n)) | i <- [1..n], j <- [1..m] ]
          ((_,_),(n,m)) = bounds a

-- | The two-phase simplex algorithm

twophase :: Array (Int,Int) Double -- ^ stating tableau
         -> Simplex (Array (Int,Int) Double) -- ^ solution

twophase a | cost a' > eps = SInfeasible
           | otherwise     = simplex $ delart a (gettab a')
    where a' = simplex $ addart $ a


-- How to handle cases where 'simplex' does not return Optimal?
gettab :: Simplex a -> a
gettab (Optimal a) = a

cost :: (Num e, Ix a, Ix b, Num a, Num b) =>
        Simplex (Array (a, b) e) -> e
cost (Optimal a) = negate $ a!(0,0)

-------------------------------------------------------------------------------

{-

Test vectors

This is from Sedgewick

> x1 = listArray ((0,0),(5,8)) [  0, -1, -1, -1, 0, 0, 0, 0, 0,
>                                 5, -1,  1,  0, 1, 0, 0, 0, 0,
>                                45,  1,  4,  0, 0, 1, 0, 0, 0,
>                                27,  2,  1,  0, 0, 0, 1, 0, 0,
>                                24,  3, -4,  0, 0, 0, 0, 1, 0,
>                                 4,  0,  0,  1, 0, 0, 0, 0, 1 ] :: Array (Int,Int) Double

P&S, Example 2.6

> x2 = listArray ((0,0),(3,5)) [ 0, 1, 1, 1, 1, 1,
>                                1, 3, 2, 1, 0, 0,
>                                3, 5, 1, 1, 1, 0,
>                                4, 2, 5, 1, 0, 1 ] :: Array (Int,Int) Double

P&S, Example 2.6 (after BFS selection)

> x2' = listArray ((0,0),(3,5)) [ -6, -3, -3,  0,  0,  0,
>                                 1,  3,  2,  1,  0,  0,
>                                 2,  2, -1,  0,  1,  0,
>                                 3, -1,  3,  0,  0,  1 ] :: Array (Int,Int) Double

P&S, Example 2.2 / Section 2.9

> x3 = listArray ((0,0),(4,7)) [ -34, -1, -14, -6, 0, 0, 0, 0,
>                                  4,  1,   1,  1, 1, 0, 0, 0,
>                                  2,  1,   0,  0, 0, 1, 0, 0,
>                                  3,  0,   0,  1, 0, 0, 1, 0,
>                                  6,  0,   3,  1, 0, 0, 0, 1 ] :: Array (Int,Int) Double

P&S, Example 2.7

> x4 = listArray ((0,0),(3,7)) [ 3, -3/4,  20, -1/2, 6, 0, 0, 0,
>                                0,  1/4,  -8,   -1, 9, 1, 0, 0,
>                                0,  1/2, -12, -1/2, 3, 0, 1, 0,
>                                1,    0,   0,    1, 0, 0, 0, 1 ] :: Array (Int,Int) Double

These come in handy for testing

> row j a = listArray (0,m) [ a!(j,k) | k <- [0..m] ]
>    where ((_,_),(n,m)) = bounds a

> column k a = listArray (0,n) [ a!(j,k) | j <- [0..n] ]
>    where ((_,_),(n,m)) = bounds a

> solution (Optimal a) = listArray (1,m) $ [ find a j | j <- [1..m] ]
>    where ((_,_),(n,m)) = bounds a

> find a j = findone' a 1 j
>     where findone' a i j | i > n          = 0
>                          | a!(i,j) == 1.0 = b!i
>                          | otherwise      = findone' a (i+1) j
>           b = listArray (1,n) [ a!(i,0) | i <- [1..n] ]
>           ((_,_),(n,m)) = bounds a

-}
