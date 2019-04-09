module Main where

import GoatAST
import System.Environment
import System.Exit

data ProgramParameters = ProgramParameters
    { isPrettyPrint :: Bool
    , fileName      :: String
    }

-- pMain :: Parser GoatProgram
-- pMain
--   = do
--     whiteSpace
--     p <- pProg
--     eof
--     return p

checkArgs :: String -> [String] -> ProgramParameters
checkArgs _ (x:xs)
  = if "-p" == x then
        ProgramParameters { isPrettyPrint = True
                          , fileName      = head xs}
    else
        ProgramParameters { isPrettyPrint = False
                          , fileName      = x}
checkArgs progName _
  = error ("\nUsage: " ++ progName ++ " [-p] fileName\n\n")
--     exitWith (ExitFailure 1)

main :: IO ()
main
  = do { progName <- getProgName             -- getProgName: System.Environment
       ; args     <- getArgs                 -- getArgs: System.Environment
       ; let newArgs = checkArgs progName args
       ; input <- readFile (fileName newArgs)
--        ; let output = runParser pMain 0 fileName input
--        ; case output of
--             Right ast -> print ast
--             Left  err -> do { putStr "Parse error at "
--                             ; print err}
       ; print "end"
       }
