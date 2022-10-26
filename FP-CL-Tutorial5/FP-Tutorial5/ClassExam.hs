-- Informatics 1 - Functional Programming
-- Class Test 2022

module ClassExam where

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: String -> Int
f s = sum [ord x | x <- s, isAlpha x]

-- b

g :: String -> Int
g [] = 0
g (x : xs) = if isAlpha x then ord x + g xs else g xs

-- c

h :: String -> Int
h s = foldr ((+) . ord) 0 (filter isAlpha s)

-- Before simplification
-- h s = foldr (+) 0 (map ord (filter (isAlpha) s))

-- d

prop_fgh :: String -> Bool
prop_fgh s = f s == g s && f s == h s

-- Problem 2

-- a

c :: String -> String -> Bool
c s1 s2 = and (zipWith (\c1 c2 -> not (isAlpha c1 && isAlpha c2) || (c1 == c2)) (s1) (s2))

-- b

d :: String -> String -> Bool
d [] _ = True
d _ [] = True
d (x:xs) (y:ys) = if isAlpha x && isAlpha y then x==y && d xs ys else d xs ys

-- c

prop_cd :: String -> String -> Bool
prop_cd s1 s2 = c s1 s2 == d s1 s2 
