import Data.Char
import Control.Monad

-- main = putStrLn "Hello World"

-- $ ghc --make chapter_10_input_output
-- [1 of 1] Compiling Main             ( chapter_10_input_output.hs, chapter_10_input_output.o )
-- Linking chapter_10_input_output ...

-- main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn ("Hey " ++ name ++ ", you rock!")

-- main = do
--     putStrLn "What's your first name?"
--     firstName <- getLine
--     putStrLn "What's your last name?"
--     lastName  <- getLine
--     let bigFirstName = map toUpper firstName
--         bigLastName  = map toUpper lastName
--     putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- main = do
--     line <- getLine
--     if null line
--         then return ()
--         else (do
--             putStrLn $ reverseWords line
--             main)
--
-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- ghci> words "hey there man"
-- ["hey","there","man"]
-- ghci> map reverse ( words "hey there man")
-- ["yeh","ereht","nam"]
-- ghci> map reverse (words "hey there man")
-- ["yeh","ereht","nam"]
-- ghci> unwords(map reverse (words "hey there man"))
-- "yeh ereht nam"

-- main = do
--     return ()
--     return "HAHAHA"
--     line <- getLine
--     return "BLAH BLAH BLAH"
--     return 4
--     putStrLn line

-- main = do
--     a <- return "hell"
--     b <- return "yeah!"
--     putStrLn $ a ++ " " ++ b

-- main = do
--     let a = "hell"
--         b = "yeah!"
--     putStrLn $ a ++ " " ++ b

-- main = do putStr "Hey, "
--           putStr "I'm "
--           putStrLn "Andy!"

-- main = do putChar 't'
--           putChar 'e'
--           putChar 'h'

-- main = do print True
--           print 2
--           print "haha"
--           print 3.2
--           print [3,4,3]

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main



