-- Author: Mahdiyar Ashrafioun
-- UCID: 30232243
-- Tut: 02
-- Date: 11/05/25
-- Assignment 3

module A3 where


-- problem 1 part a


processNestedList :: [[Int]] -> [Int]
processNestedList [] = []  -- empty list case
processNestedList (xs:xss) = map (^2) (filter even xs) ++ processNestedList xss  -- keep evens, square, recurse


-- problem 1 part b


pairwiseSum :: [(Int, Int)] -> [Int]
pairwiseSum [] = []  -- empty list
pairwiseSum (x:xs) = uncurry (+) x : pairwiseSum xs  -- use uncurry to add pair


-- problem 2 part a


matrixAddition :: [[Int]] -> [[Int]] -> [[Int]]
matrixAddition [] [] = []  -- both empty
matrixAddition (x:xs) (y:ys) = zipWith (+) x y : matrixAddition xs ys  -- add rows with zipWith


-- problem 2 part b


matrixTrace :: [[Int]] -> Int
matrixTrace m =
  let diag = [row !! i | (row, i) <- zip m [0..]]  --pick diagonal elements
  in foldr (+) 0 diag  -- sum using fold


-- problem 2 part c


transposeMatrix :: [[Int]] -> [[Int]]
transposeMatrix [] = []  -- empty matrix
transposeMatrix ([]:_) = []  -- empty row
transposeMatrix xss = map head xss : transposeMatrix (map tail xss)  -- take heads, then tails


-- problem 2 part d


matrixMultiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
matrixMultiply [] _ = Nothing  -- invalid
matrixMultiply _ [] = Nothing  -- invalid
matrixMultiply a b
  | length (head a) /= length b = Nothing  --size mismatch
  | otherwise =
      let bt = transposeMatrix b  -- transpose b
      in Just [[sum (zipWith (*) row col) | col <- bt] | row <- a]  -- multiply rows and cols


-- problem 3 worker typeclass


class Worker w where
  workerName :: w -> String
  monthlyPay :: w -> Double
  yearlyPay :: w -> Double
  yearlyPay w = 12 * monthlyPay w  -- default yearly
  giveRaise :: w -> Double -> w  -- raise by percent

-- employee has name and salary
data Employee = Employee String Double
-- contractor has name, rate, and hours
data Contractor = Contractor String Double Int
-- manager has name, base, and bonus multiplier
data Manager = Manager String Double Double

-- employee instance
instance Worker Employee where
  workerName (Employee name _) = name  -- extract name
  monthlyPay (Employee _ salary) = salary  -- salary given
  giveRaise (Employee name salary) r = Employee name (salary * (1 + r))  -- raise salary

-- contractor instance
instance Worker Contractor where
  workerName (Contractor name _ _) = name  -- name
  monthlyPay (Contractor _ rate hours) = rate * fromIntegral hours  -- rate * hours
  giveRaise (Contractor name rate hours) r = Contractor name (rate * (1 + r)) hours  -- raise rate

-- manager instance
instance Worker Manager where
  workerName (Manager name _ _) = name  -- name
  monthlyPay (Manager _ base bonus) = base + base * bonus  -- base + bonus
  giveRaise (Manager name base bonus) r =
    Manager name base (bonus * (1 + r))  -- multiply bonus

-- given workers
emp1 = Employee "Alice" 5000
cont1 = Contractor "Bob" 50 160
mgr1 = Manager "Charlie" 7000 0.1

-- raised versions
emp1Raised = giveRaise emp1 0.1
cont1Raised = giveRaise cont1 0.1
mgr1Raised = giveRaise mgr1 0.1


-- problem 4 part a


data Queue a = Empty | Node a (Queue a)
  deriving (Show)  -- so we can see queue

-- remove first item
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue Empty = Nothing  -- empty queue
dequeue (Node x xs) = Just (x, xs)  -- return first and rest


-- problem 4 part b


enqueue :: a -> Queue a -> Queue a
enqueue x Empty = Node x Empty  -- insert at empty
enqueue x (Node y ys) = Node y (enqueue x ys)  -- go to end


-- problem 4 part c


isOrderedQueue :: (Ord a) => Queue a -> Bool
isOrderedQueue Empty = True  -- empty ok
isOrderedQueue (Node _ Empty) = True  -- one element ok
isOrderedQueue (Node x (Node y ys)) =
  x <= y && isOrderedQueue (Node y ys)  -- check order


-- problem 4 part d


queueToList :: (Ord a) => Queue a -> [a]
queueToList Empty = []  -- empty list
queueToList (Node x xs) = insert x (queueToList xs)  -- insert in sorted order
  where
    insert y [] = [y]  -- insert first
    insert y (z:zs)
      | y <= z    = y : z : zs  -- place here
      | otherwise = z : insert y zs  -- go deeper


-- problem 4 part e


mergeQueues :: (Ord a) => Queue a -> Queue a -> Queue a
mergeQueues Empty q2 = q2  -- if first empty
mergeQueues q1 Empty = q1  -- if second empty
mergeQueues (Node x xs) (Node y ys)
  | x <= y    = Node x (mergeQueues xs (Node y ys))  -- keep x first
  | otherwise = Node y (mergeQueues (Node x xs) ys)  -- keep y first
