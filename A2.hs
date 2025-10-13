-- Name: Mahdiyar Ashrafioun
-- UCID: 30232243 
-- Tut: 02
-- Date: 10/8/25
-- Assignment 2 

module A2 where

-- p1: 
iteratedCartesianProduct :: [a] -> Int -> [[a]]
iteratedCartesianProduct _ 0 = [[]]
iteratedCartesianProduct l n = [x:xs | x <- l, xs <- iteratedCartesianProduct l (n-1)]  -- build recursively

--p2: 
numSums :: Int -> Int
numSums n = length [xs | k <- [1..n],
                         xs <- iteratedCartesianProduct [1..n] k,
                         sum xs == n,                           --must add up to n
                         and [x <= y | (x,y) <- zip xs (tail xs)]]    -- ensure uniqu by non decreasing order

-- p3: 
ballsIntoBins :: Int -> Int -> Int
ballsIntoBins k n =
  length [xs | xs <- compositions k n, all (>0) xs]  -- filter only positive allocations
  where
    compositions _ 0 = [[]]
    compositions m c
      | c == 1    = [[m]]  -- last bin gets remaining balls
      | otherwise = [x:rest | x <- [0..m], rest <- compositions (m - x) (c - 1)]  -- recursive split

-- p4a:
hasConsecElem :: [Int] -> Bool
hasConsecElem xs = any (\(a,b) -> abs (a - b) == 1) pairs
  where
    -- Create pairs of adjacent elements for checking
    -- Add the pair of last and first element to handle circular seating
    pairs = zip xs (tail xs ++ [head xs])


--p4b: 
twoRunLessArrs :: Int -> Int
twoRunLessArrs n =
  length [arr | arr <- allArrs [1..n],
                not (hasConsecElem arr),
                head arr == minimum arr]  -- fix rotation to avoid duplicates
  where
    allArrs [] = [[]]
    allArrs xs = [y:zs | y <- xs, zs <- allArrs [x | x <- xs, x /= y]]  -- generate all permutations

-- p4c:
hasConsecSubl :: [Int] -> Int -> Bool
hasConsecSubl xs i =
  any runAt [0..(n-1)]
  where
    n = length xs
    ys = xs ++ take (i-1) xs  -- extend for circular check
    runAt j =
      all (\k -> ys !! (j+k+1) - ys !! (j+k) == 1) [0..i-2] ||  -- increasing run
      all (\k -> ys !! (j+k) - ys !! (j+k+1) == 1) [0..i-2]     -- decreasing run

--p4d: 
iRunLessArrs :: Int -> Int -> Int
iRunLessArrs n i =
  length [arr | arr <- allArrs [1..n],
                not (hasConsecSubl arr i),
                head arr == minimum arr]  -- fix duplicates due to rotation
  where
    allArrs [] = [[]]
    allArrs xs = [y:zs | y <- xs, zs <- allArrs [x | x <- xs, x /= y]]  -- all permutations of [1..n]


