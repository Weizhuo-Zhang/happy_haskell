import Data.Char
import Control.Monad
import System.IO
import System.Directory
import Data.List
import System.Environment

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

-- main = do
--     c <- getChar
--     when (c /= ' ') $ do
--         putChar c
--         main

-- main = do
--     rs <- sequence [getLine, getLine, getLine]
--     print rs

-- main = forever $ do
--     putStr "give me some input: "
--     l <- getLine
--     putStrLn $ map toUpper l

-- main = do
--     colors <- forM[1,2,3,4] (\a -> do
--             putStrLn $ "Which color do you associate with the number" ++ show a ++ "?"
--             color <- getLine
--             return color)
--     putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
--     mapM putStrLn colors

-- main = do
--     contents <- getContents
--     putStr (map toUpper contents)

-- main = do
--     contents <- getContents
--     putStr (shortLineOnly contents)

-- shortLineOnly :: String -> String
-- shortLineOnly input =
--     let allLines = lines input
--         shortLines = filter (\line -> length line < 10) allLines
--         result = unlines shortLines
--     in result

-- respondPalindromes contents = unlines (map (\xs ->
--     if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
--         where isPalindrome xs = xs == reverse xs

-- main = interact respondPalindromes
--
-- respondPalindromes = unlines . map (\xs ->
--     if isPalindrome xs then "palindrome" else "not a palindrome") . lines
--         where isPalindrome xs = xs == reverse xs

-- main = do
--     handle <- openFile "girlfriend.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle

-- main = do
--     withFile "girlfriend.txt" ReadMode (\handle -> do
--             contents <- hGetContents handle
--             putStr contents)

-- withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- withFile' path mode f = do
--     handle <- openFile path mode
--     result <-f handle
--     hClose handle
--     return result


-- main = do
--     contents <- readFile "girlfriend.txt"
--     putStr contents

-- main = do
--     contents <- readFile "girlfriend.txt"
--     writeFile "girlfriendCap.txt" (map toUpper contents)

-- main = do
--     todoItem <- getLine
--     appendFile "todo.txt" (todoItem ++ "\n")


-- main = do
--     handle <- openFile "todo.txt" ReadMode
--     (tempName, tempHandle) <- openTempFile "." "temp"
--     contents <- hGetContents handle
--     let todoTasks = lines contents
--     numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
--     putStrLn "These are your TODO items:"
--     putStr $ unlines numberedTasks
--     putStrLn "Which one do you want to delete?"
--     numberString <- getLine
--     let number = read numberString
--     newTodoItems = delete (todoTasks !! number) todoTasks
--     hPutStr tempHandle $ unlines newTodoItems
--     hClose handle
--     hClose tempHandle
--     removeFile "todo.txt"
--     renameFile tempName "todo.txt"

-- main = do
--     args <- getArgs
--     progName <- getProgName
--     putStrLn "The arguments are:"
--     mapM putStrLn args
--     putStrLn "The program name is:"
--     putStrLn progName

-- main = do
--     (fileName:_) <- getArgs
--     fileExists <- doesFileExist fileName
--     if fileExists
--         then do contents <- readFile fileName
--             putStrLn $ "The file has " ++ show (length (lines contents)) + " lines!"
--         else do putStrLn $ "The file doesn't exist!"

main = toTry `catch` handler

toTry :: IO()
toTry = do (fileName:_) <- getArgs
           contents <- readFile filename
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

-- handler :: IOError -> IO ()
-- handler e
--     | isDoesNotExistError e = putStrLn "The file doesn't exist!"
--     | isFullError e = freeSomeSpace
--     | isIllegalOperation e = notifyCops
--     | otherwise = ioError e

handler :: IoError -> IO()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
    | otherwisee = ioError e

main = do toTry `catch` handler1
          thenTryThis `catch` handler2
          lauchRockets












































