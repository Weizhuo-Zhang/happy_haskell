module Main where

import GoatAST
import Text.Parsec

type Parser a
    = Parsec String Int a

myReserved
    = [ "begin", "bool", "call" , "do"  , "else", "end"
      , "false", "fi"  , "float", "if"  , "int" , "od"
      , "proc" , "read", "ref"  , "then", "true", "val"
      , "while", "write"
      ]

pMain :: Parser GoatProgram
pMain
  = do
    whiteSpace
    p <- pProg
    eof
    return p

-- data ProgramParameters = ProgramParameters
--     { isPrettyPrint :: Bool
--     , fileName      :: String
--     }

main :: IO ()
main
  = do { -- let a = ProgramParameters { isPrettyPrint = True
         --                           , fileName      = "filename"}
         input <- readFile ("asg.gt")
--        ; let output = runParser pMain 0 fileName input
--        ; case output of
--             Right ast -> print ast
--             Left  err -> do { putStr "Parse error at "
--                             ; print err}
       ; print "testMain"
       }
