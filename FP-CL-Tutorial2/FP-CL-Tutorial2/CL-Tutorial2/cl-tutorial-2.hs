-- Questions 2 & 3
data Thing = A | B | C | D | E deriving (Eq,Show)
things = [A, B, C, D, E]

data Shape = Disc | Square deriving (Eq, Show)
data Color = Amber | Blue deriving (Eq, Show)
data Border = Thick | Thin deriving (Eq, Show)
data Size = Big | Small deriving (Eq, Show)
data Fuzziness = Hard | Soft deriving (Eq, Show)

-- Question 2

fuzziness :: Thing -> Fuzziness
fuzziness t = if isHard t then Hard else Soft

shape :: Thing -> Shape
shape t = if isDisc t then Disc else Square

color :: Thing -> Color
color t = if isAmber t then Amber else Blue

border :: Thing -> Border
border t = if hasThickBorder t then Thick else Thin

size :: Thing -> Size
size t = if isBig t then Big else Small


type Predicate u = u -> Bool

-- Question 3

isDisc :: Predicate Thing
isDisc x = x `elem` [C]

isSquare :: Predicate Thing
isSquare x = x `elem` [A, B, D, E]

isAmber :: Predicate Thing
isAmber x = x `elem` [A, B, C, E]

isBlue :: Predicate Thing
isBlue x = x `elem` [D]

isSmall :: Predicate Thing
isSmall x = x `elem` [E]

isBig :: Predicate Thing
isBig x = x `elem` [A, B, C, D]

hasThickBorder :: Predicate Thing
hasThickBorder x = x `elem` [A, C, D, E]

hasThinBorder :: Predicate Thing
hasThinBorder x = x `elem` [B]

isHard :: Predicate Thing
isHard x = x `elem` [A, B, C]

isSoft :: Predicate Thing
isSoft x = x `elem` [D, E]

{- I could have used "not isHard" for isSoft -}

-- Question 4

-- Every blue square has a thin border.
exp1 :: [Thing] -> Bool
exp1 uni = and [ hasThinBorder t | t <- uni, isBlue t && isSquare t]

-- Some amber disc is not big.
exp2 :: [Thing] -> Bool
exp2 uni = or [ isSmall t | t <- uni, isAmber t && isDisc t]
{- Alternative I could have used "not isBig t" -}

-- Question 5
-- No square is blue
-- It is not the case that some X is Y
exp3 :: [Thing] -> Bool
exp3 uni = not (or [isBlue t | t <- uni, isSquare t ])

-- “Every X is not Y
exp4 :: [Thing] -> Bool
exp4 uni = and [ not (isBlue t) | t <- uni, isSquare t]

-- Question 6
thingsOtherThan :: Thing -> [Thing]
thingsOtherThan t = [ i | i <- things, i /= t]

properties :: [Predicate Thing]
properties = [isBlue, isAmber, isBig, isSmall, hasThickBorder, hasThinBorder, isDisc, isSquare]

{- Cant print PropertiesOf in terminal -}
propertiesOfLength :: Thing -> {-[Predicate Thing]-} Int
propertiesOfLength t = length [ p | p <- properties, p t]

propertiesOf :: Thing -> [Predicate Thing]
propertiesOf t = [ p | p <- properties, p t]


isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing p t = not (null [ i | i <- thingsOtherThan t, p i])

