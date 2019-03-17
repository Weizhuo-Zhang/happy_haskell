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



