module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. inRange

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x>=lo && x<=hi]


-- 2. multDigits

multDigits :: String -> Int
multDigits str = product [ digitToInt x | x <- str, isDigit x]

countDigits :: String -> Int
countDigits str = length [ x | x <- str, isDigit x]


-- TODO
prop_multDigits :: String -> Bool
prop_multDigits str = multDigits str <= 9^countDigits str


-- 3. capitalise and title

capitalise :: String -> String
capitalise word = toUpper (head word) : [ toLower x | x <- tail word]

title :: [String] -> [String]
title words = capitalise (head words) : [ if length x<4 then [toLower s | s <- x] else capitalise x | x <- tail words ]

-- 4. score and totalScore

score :: Char -> Int
score x
  | isCapitalVowel = 3
  | isVowel = 2
  | isUpper x = 2
  | isLetter x = 1
  | otherwise = 0
  where
      isVowel = x `elem` vowels
      isCapitalVowel = x `elem` capitalVowels
      vowels = ['a', 'e', 'i', 'o', 'u']
      capitalVowels = [toUpper v | v <- vowels]

totalScore :: String -> Int
totalScore xs = product [score x | x <- xs, score x /= 0]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1


-- ** Optional Material

-- 5. crosswordFind

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [ w | w <- words, length w==len && w!!pos == letter]
    


-- 6. search

search :: String -> Char -> [Int]
search str goal = [ i | i <- [0..length str-1], str!!i == goal ]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) <= length str 

