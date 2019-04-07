main :: IO ()
main = putStrLn (greet "World")

greeting = "Howdy"
greet who = greeting ++ ", " ++ who
