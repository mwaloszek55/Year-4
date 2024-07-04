{-
- Name: Mateusz Waloszek.
- Number: 120412764.
- Assignment: 2.
-}

-- Task 1.
-- Define the function cart_prod which calculates the cartesian product of nested lists.
cart_prod :: [[a]] -> [[a]]

-- Base case: if the list of lists is empty, return a list containing an empty list.
cart_prod []       = [[]]

-- For a non-empty list of lists: for each element x in the first list xs,
-- combine x with every possible list ys formed from the rest of the lists.
-- This uses recursion to handle the rest of the lists (xss).
cart_prod (xs:xss) = [x:ys | x <- xs, ys <- cart_prod xss]



-- Task 2.
-- Define the function sum_of_solutions.
sum_of_solutions :: [[Int]] -> [Int] -> [[Int]]

-- For each list s in the cartesian product of summands,
-- and for each number a in addends, include s in the result if the sum of s equals a.
-- This uses nested list comprehensions.
sum_of_solutions summands addends = [s | s <- cart_prod summands, a <- addends, sum s == a]



-- Task 3.
-- Define the function guardless_combinations.
guardless_combinations :: [[a]] -> [[a]] -> [[a]]

-- For each list a1s from the first list of lists (a1ss) and each list a2s from the second list of lists (a2ss),
-- combine them (a1s ++ a2s). If the combined list has a length of 2, include it in the result.
-- This uses pattern matching through the let binding.
guardless_combinations a1ss a2ss = [a1s ++ a2s | a1s <- a1ss, a2s <- a2ss, let result = a1s ++ a2s, length result == 2]



-- Task 4 
-- Define a function 'recurrence' that takes three Int parameters and returns a list of Ints.
recurrence :: Int -> Int -> Int -> [Int]
-- If 'i' is 0, return a list containing only 'f0'.
recurrence f0 f1 i
  | i == 0    = [f0]
  -- If 'i' is 1, return a list starting with 'f1' followed by 'f0'.
  | i == 1    = [f1, f0]
  -- For all other values of 'i', invoke the helper function 'recurrenceHelper' with 'f0', 'f1', 'i-1' and the list '[f1, f0]'.
  | otherwise = (recurrenceHelper f0 f1 (i - 1) [f1, f0])
  where
    -- If the third argument of 'recurrenceHelper' is 0, return the accumulated list 'acc'.
    recurrenceHelper _ _ 0 acc = acc
    -- Otherwise, recursively call 'recurrenceHelper' with the next values for the sequence.
    -- 'a' is the first value, 'b' is the second value, 'n' is the counter (reduced by 1 in each step),
    -- and 'acc' is the accumulated list of results so far.
    -- The next values for the sequence are obtained by swapping 'a' and 'b' and adding them, 
    -- and then adding the result at the front of 'acc'.
    recurrenceHelper a b n acc = recurrenceHelper b (a + b) (n - 1) (a + b : acc)



-- Task 5.
main :: IO ()
main = do
  putStrLn $ show $ cart_prod [[1], [2], [3]]
  putStrLn $ show $ sum_of_solutions [[1, 1], [2], [1, 3], [2, 3]] [2, 5]
  putStrLn $ show $ guardless_combinations [[] , [1], [1, 2]] [[] , [3], [3, 4]]
  putStrLn $ show $ recurrence 0 1 8
