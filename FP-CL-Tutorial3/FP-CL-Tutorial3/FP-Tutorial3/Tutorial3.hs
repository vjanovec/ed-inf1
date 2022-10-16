module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- ** Caesar Cipher Exercises

-- 1.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c l = if null result then c else head result
      where result = [ b | (a, b) <- l, a==c]

lookUpRec :: Char -> [(Char, Char)] -> Char

lookUpRec c [] = c
lookUpRec c ((a, b) : xs)     | a == c          = b
                              | otherwise       = lookUpRec c xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c l = lookUp c l == lookUpRec c l

-- 2.
encipher :: Int -> Char -> Char
encipher i c = lookUp c (makeKey i)

-- 3.
normalise :: String -> String
normalise s = [ toUpper a | a <- s, a `elem` ['A'..'Z'] || a `elem` ['a'..'z']]

normaliseRec :: String -> String
normaliseRec [] = []
normaliseRec (x : xs)   | x `elem` ['A'..'Z'] || x `elem` ['a'..'z']    = toUpper x : normaliseRec xs
                        | otherwise                                     = normaliseRec xs

prop_normalise :: String -> Bool
prop_normalise s = normalise s == normaliseRec s

-- 4.
enciphers :: Int -> String -> String
enciphers i s = [ encipher i c | c <- normalise s]

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey l = [ (b, a) | (a, b) <- l]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((a, b) : xs) = (b, a) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey l = reverseKey l == reverseKeyRec l

-- 6.
decipher :: Int -> Char -> Char
decipher i c = lookUp c (reverseKey (makeKey i))

decipherStr :: Int -> String -> String
decipherStr i [] = []
decipherStr i (x:xs)    | isUpper x = decipher i x : decipherStr i xs
                        | otherwise = decipherStr i xs


-- ** Optional Material

-- 7.
candidates :: String -> [(Int, String)]
-- candidates s = [ [ (dec, i) | dec <- decipherStr i s, isInfixOf ("AND") dec || isInfixOf ("THE" d)], i <- [1..26], ] 
candidates = undefined


splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')

-- The following example shows why 'transpose' is not
--  invertible in general. The transpose function
--  takes the 'columns' of a list of lists, and makes
--  them the 'rows' of a new list of lists. 
--
-- [[o n e],           [[o t t f f],       [[o n e e e],
--  [t w o],            [n w h o i],        [t w o r],  
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],  
--  [f o u r],          [e r e],            [f o u], 
--  [f i v e]   ]       [e],        ]       [f i v]     ]   

-- 8.
encrypt :: Int -> String -> String
encrypt = undefined

-- 9.
decrypt :: Int -> String -> String
decrypt = undefined
