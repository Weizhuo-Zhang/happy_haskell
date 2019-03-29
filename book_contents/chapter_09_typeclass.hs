module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where

import qualified Data.Map as Map

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- ghci> surface $ Circle 10 20 10
-- 314.15927
-- ghci> surface $ Rectangle 0 0 100 100
-- 10000.0

-- ghci> Circle 10 20 5
-- Circle 10.0 20.0 5.0
-- ghci> Rectangle  50 230 60 90
-- Rectangle 50.0 230.0 60.0 90.0

-- ghci> map (Circle 10 20) [4,5,6,6]
-- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 -y1)

-- ghci> surface (Circle (Point 0 0) 24)
-- 1809.5574
-- ghci> surface (Rectangle (Point 0 0) (Point 100 100))
-- 10000.0

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- ghci> nudge (Circle (Point 34 34 ) 10) 5 10
-- Circle (Point 39.0 44.0) 10.0)))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)


-- Record Syntax
--

-- First_name, Last name, Age...
-- data Person = Person String String Int Float String String deriving (Show)

-- ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- ghci> guy
-- Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"


-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname
--
-- lastName :: Person -> String
-- lastName (Person _ lastName _ _ _ _) = lastName
--
-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age
--
-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height
--
-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number
--
-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

-- ghci> firstName guy
-- "Buddy"
-- ghci> height guy
-- 184.2
-- ghci> flavor guy
-- "Chocolate"



-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      , phoneNumber :: String
--                      , flavor :: String
--                      } deriving (Show)

-- ghci> :t flavor
-- flavor :: Person -> String
-- ghci> :t firstName
-- firstName :: Person -> String

-- data Car = Car String String Int deriving (Show)

-- ghci> Car "Ford" "Mustang" 1967
-- Car "Ford" "Mustang" 1967

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- ghci> Car {company="Ford", model="Mustang", year=1967}
-- Car {company = "Ford", model = "Mustang", year = 1967}


-- Type parameters
--
-- ghci> Just "Haha"
-- Just "Haha"
-- ghci> Just 84
-- Just 84
-- ghci> :t Just "Haha"
-- Just "Haha" :: Maybe [Char]
-- ghci> :t Just 84
-- Just 84 :: Num a => Maybe a
-- ghci> :t Nothing
-- Nothing :: Maybe a
-- ghci> Just 10 :: Maybe Double


tellCar :: Car -> String
tellCar (Car {company=c, model=m, year=y}) = "This " ++ c ++ " " ++ m ++ " war made in " ++ show y

-- ghci> let stang = Car {company="Ford", model="Mustang", year=1967}
-- ghci> tellCar stang
-- "This Ford Mustang war made in 1967"

data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)
scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- ghci> Vector 3 5 8 `vplus` Vector 9 2 8
-- Vector 12 7 16
-- ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
-- Vector 12 9 19
-- ghci> Vector 4 9 5 `scal
-- scalarMult  scaleFloat
-- ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0
-- 74.0
-- ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
-- Vector 148 666 222


-- Derived instances

-- data Person = Person  { firstName :: String
--                       , lastName  :: String
--                       , age       :: Int
--                       } deriving (Eq)

-- ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
-- ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
-- ghci> mca == adRock
-- False
-- ghci> mikeD == adRock
-- False
-- ghci> mikeD == mikeD
-- True
-- ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- True

-- ghci> let beastieBoys = [mca, adRock, mikeD]
-- ghci> mikeD `elem` beastieBoys 
-- True

data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     } deriving (Eq, Show, Read)

-- ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- ghci> mikeD
-- Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- ghci> "mikeD is: " ++ show mikeD
-- "mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"

-- ghci> read "Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}" :: Person
-- Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- ghci> read "Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}" == mikeD
-- True

-- data Bool = False | True deriving (Ord)
-- ghci> True `compare` False
-- GT
-- ghci> True > False
-- True
-- ghci> True < False
-- False

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- ghci> Wednesday 
-- Wednesday
-- ghci> show Wednesday 
-- "Wednesday"
-- ghci> read "Saturday" :: Day
-- Saturday

-- ghci> Saturday == Sunday 
-- False
-- ghci> Saturday == Saturday 
-- True
-- ghci> Saturday > Friday 
-- True
-- ghci> Monday `compare` Wednesday 
-- LT

-- ghci> minBound :: Day 
-- Monday
-- ghci> maxBound :: Day 
-- Sunday

-- ghci> succ Monday 
-- Tuesday
-- ghci> pred Saturday 
-- Friday
-- ghci> [Thursday .. Sunday]
-- [Thursday,Friday,Saturday,Sunday]
-- ghci> [minBound .. maxBound] :: [Day]
-- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]


-- Type synonyms

-- type PhoneBook = [(String, String)]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook


-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

-- ghci> Shapes.Right 20
-- Right 20
-- ghci> Shapes.Left "w00t"
-- Left "w00t"
-- ghci> :t Shapes.Right 'a'
-- Shapes.Right 'a' :: Shapes.Either a Char
-- ghci> :t Shapes.Left True
-- Shapes.Left True :: Shapes.Either Bool b

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
         Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
         Just (state, code) -> if state /= Taken
                               then Right code
                               else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "ZD39I"))
    ,(101, (Free,  "JAH3I"))
    ,(103, (Free,  "IQSA9"))
    ,(105, (Free,  "QITSA"))
    ,(109, (Free,  "893JJ"))
    ,(110, (Taken, "99292"))
    ]

-- ghci> lockerLookup 101 lockers
-- Right "JAH3I"
-- ghci> lockerLookup 100 lockers
-- Left "Locker 100 is already taken!"
-- ghci> lockerLookup 102 lockers
-- Left "Locker number 102 doesn't exist!"

-- Recursive data structures
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

-- ghci> Empty
-- Empty
-- ghci> 5 `Cons` Empty 
-- Cons {listHead = 5, listTail = Empty}
-- ghci> 4 `Cons` (5 `Cons` Empty )
-- Cons {listHead = 4, listTail = Cons {listHead = 5, listTail = Empty}

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- ghci> 3 :-: 4 :-: 5 :-: Empty
-- 3 :-: (4 :-: (5 :-: Empty))
-- ghci> let a = 3 :-: 4 :-: 5 :-: Empty
-- ghci> 100 :-: a
-- 100 :-: (3 :-: (4 :-: (5 :-: Empty)))

-- infixr 5 ++
-- (++) :: [a] -> [a] -> [a]
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- ghci> let a = 3 :-: 4 :-: 5 :-: Empty
-- ghci> let b = 6 :-: 7 :-: Empty
-- ghci> a .++ b
-- 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))








