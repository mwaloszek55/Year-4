{-
- Name: Mateusz Waloszek
- Number: 120412764
- Assignment: 5
-}

-- Class 'Printable' that ensures a type can be printed.
class Show a => Printable a where
    print_it :: a -> IO ()      -- Function to print a value.
    println_it :: a -> IO ()    -- Function to print a value 
                                -- followed by a newline.

    -- Default implementations for the class functions.
    print_it = print            -- Default implementation using 'print'.
    println_it x = print x >> putStrLn "" -- Print followed by a newline.

-- 'Bool' type an instance of 'Printable'.
instance Printable Bool        -- No implementation needed, 
                               -- using defaults.

-- Data type for unary functions.
data UnaryFunction a b = UnaryFunction {
    name :: String,            -- Name of the function.
    definition :: a -> b       -- The function itself.
}

-- 'UnaryFunction' an instance of 'Show'.
instance (Bounded a, Enum a, Show a, Show b) => Show (
    UnaryFunction a b
    ) where
    show (UnaryFunction funcName funcDef) =
        -- All possible values of type 'a'.
        let domainVals = [minBound .. maxBound]
            -- Apply func to each.
            pairs = map (\v -> (v, funcDef v)) domainVals
            pairsStr = map (
                -- Make string.
                \(a, b) -> show a ++ " -> " ++ show b) pairs
            -- Join all strings with commas.
            body = joinStrings ", " pairsStr
        -- Final representation.
        in funcName ++ ": { " ++ body ++ " }"

-- A custom function to join strings with a separator.
joinStrings :: String -> [String] -> String
joinStrings _ [] = ""            -- Base case for empty list.
joinStrings _ [x] = x            -- Base case for single-element list.

-- Recursive case.
joinStrings sep (x:xs) = x ++ sep ++ joinStrings sep xs

-- 'UnaryFunction' an instance of 'Printable'.
instance (Bounded a, Enum a, Show a, Show b) => Printable (
    UnaryFunction a b)
    -- Default implementations are suitable

-- Main function to demonstrate the usage of 'Printable'.
main :: IO ()
main = do
    println_it True           -- Print 'True' with newline.
    print_it False            -- Print 'False' without newline.
    putStrLn ""               -- Print a newline for separation.

    -- Create and print a 'UnaryFunction' representing logical NOT.
    let notFunction = UnaryFunction "my_not" not
    println_it notFunction    -- Use 'println_it' to print the function.
