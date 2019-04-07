main :: IO()
main = do
    content <- readFile "numbers.txt"
    print content
