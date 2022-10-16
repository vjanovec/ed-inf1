module Tutorial4 where

import Data.Char
import Data.List
import Test.QuickCheck


-- ** Optional material

-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp l =  [ 2*x | x <- l]

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] = []
doublesRec (x:xs) = 2*x : doublesRec(xs)

-- c.
doublesHO :: [Int] -> [Int]
doublesHO xs = map (2 *) xs

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles l = doublesComp l == doublesRec l && doublesComp l == doublesHO l

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp i l =  [ x | x <- l, x > i]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec i [] = []
abovesRec i (x:xs)  | x > i = x : abovesRec i xs
                    | otherwise = abovesRec i xs

-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO i = filter (>i)
-- We can miss out the list parameter in the function body

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves i l =  abovesComp i l == abovesRec i l && abovesComp i l == abovesHO i l

-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor i j =  not i == j

-- b.
parityRec :: [Bool] -> Bool
parityRec [] = True
parityRec (x:xs) = x `xor` parityRec xs

-- c.
parityHO :: [Bool] -> Bool
parityHO xs = foldr xor True xs

-- d.
prop_parity :: [Bool] -> Bool
prop_parity l =  parityRec l == parityHO l

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp l =  and [ isUpper x | x <- l, isAlpha x]

-- b.
allcapsRec :: String -> Bool
allcapsRec [] = True
allcapsRec (x:xs)
    | isAlpha x = isUpper x && allcapsRec xs
    | otherwise = allcapsRec xs

-- c.
allcapsHO :: String -> Bool
allcapsHO s =  foldr (&&) True (map isUpper (filter isAlpha s))


-- (alternative 1 ) allcapsHO s =  and (map isUpper (filter isAlpha s))
-- (alternative 2) allcapsHO s =  all isUpper (filter isAlpha s)


-- d.
prop_allcaps :: String -> Bool
prop_allcaps s =  allcapsComp s == allcapsRec s && allcapsComp s == allcapsHO s


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform li = all (==head li) li
-- I used the comparison in question 4c 
-- all = and map

--uniformRational :: [Rational] -> Bool
--uniformRational lr = all (==head lr) lr

-- b.
valid :: Matrix -> Bool
valid m = uniform [ length r | r <- m] && not (null m) && not (null (head m))




-- 6.
width :: Matrix -> Int
width m = length (head m)

height :: Matrix -> Int
height = length

addUpRows :: [Rational] -> [Rational] -> [Rational]
addUpRows = zipWith (+)

plusM :: Matrix -> Matrix -> Matrix
plusM m1 m2 = [addUpRows a b | valid m1 && valid m2, (a, b) <- zip m1 m2]

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 = [zipWith (*) r1 r2 | valid m1 && valid m2, r1 <- m1, r2 <- m2]
-- dimensions heigth M1 and width M2

-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined

determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined

scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)

prop_inverse3 :: Triple Rational ->
                 Triple Rational ->
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
