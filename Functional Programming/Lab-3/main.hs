{-
- Name: Mateusz Waloszek.
- Number: 120412764.
- Assignment: 3.
-}

import Data.Char (isAlpha)
import Data.List (span)

-- Checks if a character is a letter or an apostrophe.
isLetter :: Char -> Bool
isLetter c = isAlpha c || c == '\''

-- Applies consonant rule of Pig Latin to a word.
consonantRule :: String -> String
consonantRule word =
  -- Takes consonants before the first vowel 
  -- and moves them to the end, adds "ay".
  let (consonants, rest) = span (`notElem` "aeiou") word
  in if not (null consonants) then rest ++ consonants ++ "ay" else word

-- Applies vowel rule of Pig Latin to a word.
vowelRule :: String -> String
vowelRule word = word ++ "way"  
-- Appends "way" to the end of vowel-starting words.

-- Splits text into words and non-words, preserving 
-- original spaces and punctuation.
splitWords :: String -> [String]
splitWords "" = []
splitWords text@(x:xs)
  | isLetter x = 
      -- If it starts with a letter, take the word 
      -- and continue splitting.
      let (word, rest) = span isLetter text 
      in word : splitWords rest
  | otherwise  = 
      -- If it starts with a non-letter, take 
      -- the non-word and continue splitting.
      let (nonWord, rest) = span (not . isLetter) text 
      in nonWord : splitWords rest


-- Translates words starting with consonants 
-- using the consonant rule.
consonant_translate :: String -> String
consonant_translate = concat . map processWord . splitWords
  where
    -- Processes each word; if it starts 
    -- with a consonant, applies consonant rule.
    processWord word@(firstChar:_)
      | isLetter firstChar 
      && notElem firstChar "aeiou" = consonantRule word
      | otherwise = word  -- Leaves other words/non-words unchanged.
    processWord [] = []

-- Translates words starting with vowels using the vowel rule.
vowel_translate :: String -> String
vowel_translate = concat . map processWord . splitWords
  where
    -- Processes each word; if it starts with a vowel, 
    -- applies vowel rule.
    processWord word@(firstChar:_)
      | isLetter firstChar && elem firstChar "aeiou" = vowelRule word
      | otherwise = word  -- Leaves other words/non-words unchanged.
    processWord [] = []

-- Translates words using both consonant and vowel rules.
translate_both :: String -> String
translate_both = concat . map processWord . splitWords
  where 
    -- Processes each word and applies 
    -- the appropriate Pig Latin rule.
    processWord word@(firstChar:_)
      | isLetter firstChar = 
          -- Applies vowel rule if word starts with vowel, 
          -- else consonant rule.
          if elem firstChar "aeiou" 
          then vowelRule word else consonantRule word
      | otherwise = word  -- Leaves non-words unchanged.
    processWord [] = []

-- Main function to read a line and translate it to Pig Latin.
main :: IO ()
main = do
  -- Reads a line from input.
  line <- getLine
  -- Applies consonant rule and prints the result.
  putStrLn $ consonant_translate line
  -- Applies vowel rule and prints the result.
  putStrLn $ vowel_translate line
  -- Applies both rules and prints the result.
  putStrLn $ translate_both line
