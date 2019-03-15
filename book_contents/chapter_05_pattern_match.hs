capital :: String -> String
capital "" = "Empty String, whoops!"
capital all@(x:y:z) = "The first letter of " ++ all ++ " is " ++ [y]

-- bmiTell :: (RealFloat a) => a -> String
-- bmiTell bmi
--     | bmi <= 18.5 = "You're underweight, you emo, you!"
--     | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty"
--     | otherwise = "You're a whale, congragulations!"

-- bmiTell :: (RealFloat a) => a -> a -> String
-- bmiTell weight height
--     | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
--     | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty"
--     | otherwise = "You're a whale, congragulations!"

max' :: (Ord a) => a -> a -> a
max' x y
    | x > y     = x
    | otherwise = y

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b      = GT
    | a == b     = EQ
    | otherwise  = LT

-- bmiTell :: (RealFloat a) => a -> a -> String
-- bmiTell weight height
--     | bmi <= 18.5 = "You're underweight, you emo, you!"
--     | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty"
--     | otherwise = "You're a whale, congragulations!"
--     where bmi = weight / height ^ 2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty"
    | otherwise = "You're a whale, congragulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat    = 30.0
          -- (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

unroll :: Int -> [a] -> [a]
unroll num [] = []
unroll num x  = take num ( cycle x )
-- unroll 0 x  = []
-- unroll num x = unroll (num - 1) x ++ [x !! mod (num - 1) (length x)] 

-- unroll num [] = unroll num all
-- unroll num all@(x:xs)
--     |   num == 0        = []
--     |   xs  == []       = unroll num all
--     |   otherwise       = [x] ++ unroll (num - 1) xs

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

calcBmisLet :: (RealFloat a) => [(a, a)] -> [a]
calcBmisLet xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- Head function for pattern match
-- head' :: [a] -> a
-- head' [] = error "No head for empty lists!"
-- head' (x:_) = x

-- Head function for case expressions
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "This list is " ++ what xs
    where what [] = "empty"
          what [x] = "a singleton list."
          what  xs = "a longer list."
