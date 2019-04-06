s0 :: String
s0 = "XXX"

s1 :: String
s1 = "\0088\x0058\o0130"

main :: IO ()
main = do
    putStrLn s0
    putStrLn s1
