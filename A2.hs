-- Name: Mahdiyar Ashrafioun
-- UCID: 30232243 
-- Tut: 02
-- Date: 10/8/25
-- Assignment 2 

module A2 where

-- p1
iteratedCartesianProduct :: [a] -> Int -> [[a]]
iteratedCartesianProduct _ 0 = [[]]
iteratedCartesianProduct l n = [x:xs | x <- l, xs <- iteratedCartesianProduct l (n-1)]

--p2
numSums :: Int -> Int
numSums n = length [xs | k <- [1..n],
                         xs <- iteratedCartesianProduct [1..n] k,
                         sum xs == n,
                         and [x <= y | (x,y) <- zip xs (tail xs)]]
                         
-- p3 
ballsIntoBins :: Int -> Int -> Int
ballsIntoBins k n =
  length [xs | xs <- compositions k n, all (>0) xs]
  where
    compositions _ 0 = [[]]
    compositions m c
      | c == 1    = [[m]]
      | otherwise = [x:rest | x <- [0..m], rest <- compositions (m - x) (c - 1)]

 
--p4
-- a
hasConsecElem :: [Int] -> Bool
hasConsecElem xs = any (\(a,b) -> abs (a - b) == 1) (zip xs (tail xs ++ [head xs]))

-- b
twoRunLessArrs :: Int -> Int
twoRunLessArrs n =
  length [arr | arr <- allArrs [1..n],
                not (hasConsecElem arr),
                head arr == minimum arr]
  where
    allArrs [] = [[]]
    allArrs xs = [y:zs | y <- xs, zs <- allArrs [x | x <- xs, x /= y]]

--c
hasConsecSubl :: [Int] -> Int -> Bool
hasConsecSubl xs i =
  any runAt [0..(n-1)]
  where
    n = length xs
    ys = xs ++ take (i-1) xs
    runAt j =
      all (\k -> ys !! (j+k+1) - ys !! (j+k) == 1) [0..i-2] ||
      all (\k -> ys !! (j+k) - ys !! (j+k+1) == 1) [0..i-2]

-- d
iRunLessArrs :: Int -> Int -> Int
iRunLessArrs n i =
  length [arr | arr <- allArrs [1..n],
                not (hasConsecSubl arr i),
                head arr == minimum arr]
  where
    allArrs [] = [[]]
    allArrs xs = [y:zs | y <- xs, zs <- allArrs [x | x <- xs, x /= y]]
