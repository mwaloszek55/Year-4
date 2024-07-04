{-
- Name: Mateusz Waloszek.
- Number: 120412764.
- Assignment: 1.
-}
-- Main.hs

-- xor1: Implementation using nested conditional statements
--
-- This implementation uses nested if-then-else statements to check the conditions
-- for exclusive or and return the appropriate Boolean value.
xor1 :: Bool -> Bool -> Bool
xor1 a b =
  if a then
    if b then
      False
    else
      True
  else
    if b then
      True
    else
      False

-- xor2: Implementation using guards and returning Bool constants
--
-- This implementation uses guards to check different conditions and returns
-- Bool constants based on those conditions.
xor2 :: Bool -> Bool -> Bool
xor2 True True   = False
xor2 True False  = True
xor2 False True  = True
xor2 False False = False

-- xor3: Implementation using pattern matching and returning Bool constants
--
-- This implementation uses pattern matching to define cases and return Bool
-- constants based on the input patterns.
xor3 :: Bool -> Bool -> Bool
xor3 True True   = False
xor3 True False  = True
xor3 False True  = True
xor3 False False = False

-- xor4: Implementation using pattern matching without returning Bool constants
--
-- This implementation uses pattern matching to define cases and returns a
-- Boolean value based on the input patterns. It does not return Bool constants.
xor4 :: Bool -> Bool -> Bool
xor4 True  b = not b
xor4 False b = b

-- xor5: Implementation using pattern matching and one existing function
--
-- This implementation uses pattern matching to define cases and relies on
-- the existing function `not` to compute the XOR.
xor5 :: Bool -> Bool -> Bool
xor5 True  b = not b
xor5 False b = b

-- xor6: Implementation using pattern matching and two existing functions
--
-- This implementation uses pattern matching to define cases and relies on
-- two existing functions, `not` and `&&`, to compute the XOR.
xor6 :: Bool -> Bool -> Bool
xor6 True  b = not b
xor6 False b = b && b

-- xor7: Implementation using a lambda expression
--
-- This implementation defines the XOR function using a lambda expression.
xor7 :: Bool -> Bool -> Bool
xor7 = \a b -> if a then not b else b

-- xor8: Implementation using a built-in function without pattern matching or partial applications
--
-- This implementation uses the built-in inequality operator `/=` to compute the XOR.
xor8 :: Bool -> Bool -> Bool
xor8 a b = a /= b

-- xor9: Implementation using partial application without pattern matching or lambda expressions
--
-- This implementation uses partial application of the built-in inequality operator `/=` to compute the XOR.
xor9 :: Bool -> Bool -> Bool
xor9 = (/=)

-- xor10: Implementation using a different type of partial application
--
-- This implementation uses partial application of the built-in inequality operator `/=` with a different order of arguments.
xor10 :: Bool -> Bool -> Bool
xor10 a = (/=) a

-- Unit test for xor functions
main :: IO ()
main = do
  let bs = [True, False]  -- List of Bool values
      fs = [xor1, xor2, xor3, xor4, xor5, xor6, xor7, xor8, xor9, xor10]  -- List of xor implementations

      -- Verify that for each pair of functions (f1, f2) and for each pair of Bool values (b1, b2),
      -- f1 and f2 always return the same value when applied to b1 and b2.
      verification = and [f b1 b2 == g b1 b2 | f <- fs, g <- fs, b1 <- bs, b2 <- bs]

  putStrLn $ "Unit Test Result: " ++ show verification