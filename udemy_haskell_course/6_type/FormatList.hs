import Data.List

formatList :: String -> String -> String -> [String] -> String
formatList start end seperator xs = start ++ (intercalate seperator (map show xs)) ++ end

main :: IO ()
main = putStrLn $ formatList "<list>" "</list>" "|" ["first", "second", "third", "fourth"]
