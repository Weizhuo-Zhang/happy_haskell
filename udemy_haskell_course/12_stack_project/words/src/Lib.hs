module Lib
    ( someFunc
    , someString
    , outputGrid
    , formatGrid
    , findWord
    , findWords
    , findWordInLine
    , skew
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

someFunc :: IO ()
someFunc = putStrLn someString

someString :: String
someString = "someString"

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

getLines :: Grid -> [String]
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize horizontal
        diagonal2 = diagonalize (map reverse horizontal)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)

diagonalize :: Grid -> Grid
diagonalize = transpose . skew
-- diagonalize grid = transpose (skew grid)

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line

findWord :: Grid -> String -> Maybe String
findWord grid word =
    let lines = getLines grid
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf
