-- Name: Mahdiyar Ashrafioun
-- UCID: 30232243 
-- Tut: 02
-- Date: 10/8/25
-- Assignment 2 

module A2 where

iteratedCartesianProduct :: [a] -> Int -> [[a]]
iteratedCartesianProduct _ 0 = [[]]
iteratedCartesianProduct l n = [x:xs | x <- l, xs <- iteratedCartesianProduct l (n-1)]


numSums :: Int -> Int
numSums n = length [xs | k <- [1..n],
                         xs <- iteratedCartesianProduct [1..n] k,
                         sum xs == n,
                         and [x <= y | (x,y) <- zip xs (tail xs)]]
                         

ballsIntoBins :: Int -> Int -> Int
ballsIntoBins k n = length [xs | xs <- iteratedCartesianProduct [1..n] k,
                                 and [i `elem` xs | i <- [1..n]]]
 


iRunLessArrs :: Int -> Int -> Int
iRunLessArrs =