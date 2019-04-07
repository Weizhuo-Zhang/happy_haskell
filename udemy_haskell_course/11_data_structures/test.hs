data Compass = North | East | South | West
    deriving (Eq, Ord, Enum, Show)

data Expression = Number Int
                | Add Expression Expression
                | Substract Expression Expression
                deriving (Eq, Ord, Show)

calculate :: Expression -> Int
calculate (Number x) = x
calculate (Add x y)  = (calculate x) + (calculate y)
calculate (Substract x y)  = (calculate x) - (calculate y)

newHead :: [a] -> a
newHead [] = error "empty list"
newHead (x:xs) = x

newHead :: [a] -> [a]
newTail [] = error "empty list"
newTail (x:xs) = xs
