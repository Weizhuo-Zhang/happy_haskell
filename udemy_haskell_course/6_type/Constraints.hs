myFold :: (a -> b -> b) -> b -> [a] -> b
myFold _ b [] = b
myFold f b (a : as) = myFold f (f a b) as

main :: IO ()
main = print $ myFold (+) 100 [10, 20, 30]
