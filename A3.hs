-- Author: Mahdiyar Ashrafioun
-- UCID: 30232243
-- Tut: 02
-- Date: 11/05/25
-- Assignment 3

module A3 where


-- Problem 1: part a 

processNestedList :: [[Int]] -> [Int]
processNestedList [] = []
processNestedList(xs:xss) = map (^2)(filter(\x -> x `mod`2 == 0)xs) ++ processNestedList xss


-- Problem 1: part b

pairwiseSum :: [(Int, Int)] -> [Int]
pairwiseSum [] = []
pairwiseSum((x,y):xs) = x + y : pairwiseSum xs

-- problem 2: part a 

matrixAddition :: [[Int]] -> [[Int]] -> [[Int]]
matrixAddition [] [] = [] 
matrixAddition (x:xs)(y:ys) = zipWith (+) x y : matrixAddition xs ys    -- or zipWith(zipWith(+))

-- problem 2: part b

matrixTrace :: [[Int]] -> Int
matrixTrace x = foldr (+) 0 [(x !! i) !! i | i <- [0,1,2]]

-- problem 2: part c

transposeMatrix :: [[Int]] -> [[Int]]
transposeMatrix [] = []
transposeMatrix ([]:_) = []
transposeMatrix xss = map head xss : transposeMatrix (map tail xss)

-- problem 2: part d

matrixMultiply :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultiply a b
  | null a || null b = error "One or both matrices are empty!"
  | length (head a) /= length b = error "Matrix size mismatch: cannot multiply!"
  | otherwise = [[ sum (zipWith (*) row col)
                 | col <- transposeMatrix b ]
                 | row <- a]


main :: IO()
main = do 
    print( matrixTrace[[1,2,3],[4,5,6],[7,8,9]])  -- output 15