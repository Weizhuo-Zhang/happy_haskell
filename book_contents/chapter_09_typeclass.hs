module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where

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



data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

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









