module Tutorial1 where

import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck


-- 2.
double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x*x

-- 3.
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = c*c == a*a + b*b

-- 4.
leg1 :: Int -> Int -> Int
leg1 x y = x*x-y*y

leg2 :: Int -> Int -> Int
leg2 x y = 2*x*y

hyp :: Int -> Int -> Int
hyp x y = x^2+y^2

-- 5.
prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

-- 7.
pic1 :: Picture
pic1 = above (beside knight (invert knight)) (beside (invert knight) knight)

pic2 :: Picture
pic2 = above (beside (knight) (invert knight)) (beside (flipV(invert knight)) (flipV(knight)))

-- ** Functions

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- 8.
twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoBeside (twoAbove x)

-- 9.
-- a)
emptyRow :: Picture
emptyRow = repeatH 4 (twoBeside blackSquare)

-- b)
otherEmptyRow :: Picture
otherEmptyRow = invert emptyRow

-- c)
middleBoard :: Picture
middleBoard = repeatV 2 (twoAbove emptyRow)

-- d)
whiteRow :: Picture
whiteRow = above (over (beside (beside (beside rook knight) (beside bishop queen)) (beside (beside king bishop) (beside knight rook))) emptyRow) (over (repeatH 8 pawn) otherEmptyRow)

blackRow :: Picture
blackRow = above (over (repeatH 8 (invert pawn)) emptyRow) (over (invert (beside (beside (beside rook knight) (beside bishop queen)) (beside (beside king bishop) (beside knight rook)))) emptyRow)

-- e)
populatedBoard :: Picture
populatedBoard = above (above whiteRow middleBoard) blackRow
