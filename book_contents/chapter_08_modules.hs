import Data.List
import Data.Char

-- Set a alias for modules
-- Examples: Map.filter
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Only import nub and sort
import Data.List (nub, sort)

-- Exclude the nub modules from import
import Data.List hiding (nub)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- ghci> :m Data.List
-- ghci> :m Data.List Data.Map Data.Set

-- Data.List modules

-- intersperse
-- ghci> intersperse '.' "MONKEY"
-- "M.O.N.K.E.Y"
-- ghci> intersperse 0 [1,2,3,4,5,6]
-- [1,0,2,0,3,0,4,0,5,0,6]

-- intercalate
-- ghci> intercalate " " ["hey", "there", "guys"]
-- "hey there guys"
-- ghci> intercalate [0,0,0] [[1,2,3], [4,5,6], [7,8,9]]
-- [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

-- transpose
-- ghci> transpose [[1,2,3], [4,5,6], [7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
-- ghci> transpose ["hey", "there", "guys"]
-- ["htg","ehu","yey","rs","e"]

-- cancat
-- ghci> concat ["foo", "bar", "car"]
-- "foobarcar"
-- ghci> concat [[3,4,5],[2,3,4],[2,1,1]]
-- [3,4,5,2,3,4,2,1,1]

-- concatMap
-- ghci> concatMap (replicate 4) [1..3]
-- [1,1,1,1,2,2,2,2,3,3,3,3]
-- ghci> map (replicate 4) [1..3]
-- [[1,1,1,1],[2,2,2,2],[3,3,3,3]]

-- and
-- all the elements meet the requirements returns True
-- ghci> and $ map (>4) [5,6,7,8]
-- True
-- ghci> and $ map (==4) [4,4,4,3,4]
-- False

-- or
-- ghci> or $ map (==4) [2,3,4,5,6,1]
-- True
-- ghci> or $ map (>4) [2,3,1]
-- False

-- any
-- ghci> any (==4) [2,3,4,5,6,1]
-- True
-- ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
-- True

-- all
-- ghci> all (>4) [6,9,10]
-- True
-- ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
-- False

-- iterate
-- ghci> take 10 $ iterate (*2) 1
-- [1,2,4,8,16,32,64,128,256,512]
-- ghci> take 3 $ iterate (++ "haha") "haha"
-- ["haha","hahahaha","hahahahahaha"]

-- splitAt
-- ghci> splitAt 3 "heyman"
-- ("hey","man")
-- ghci> splitAt 100 "heyman"
-- ("heyman","")
-- ghci> splitAt (-3) "heyman"
-- ("","heyman")
-- ghci> let (a, b) = splitAt 3 "foobar" in b ++ a
-- "barfoo"

-- takeWhile dropWhile
-- ghci> takeWhile (/=' ') "This is a sentence"
-- "This"
-- ghci> dropWhile (/=' ') "This is a sentence"
-- " is a sentence"

-- span
-- ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++
-- fw ++ " , the rest:" ++ rest
-- "First word:This , the rest: is a sentence"

-- span break
-- ghci> break (==4) [1,2,3,4,5,6,7]
-- ([1,2,3],[4,5,6,7])
-- ghci> span (/=4) [1,2,3,4,5,6,7]
-- ([1,2,3],[4,5,6,7])

-- sort
-- ghci> sort [8,5,3,2,1,6,4,2]
-- [1,2,2,3,4,5,6,8]

-- group
-- ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
-- [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

-- inits    tails
-- ghci> inits "w00t"
-- ["","w","w0","w00","w00t"]
-- ghci> tails "w00t"
-- ["w00t","00t","0t","t",""]

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- isInfixOf
-- ghci> "cat" `isInfixOf` "im a cat burglar"
-- True
-- ghci> "Cat" `isInfixOf` "im a cat burglar"
-- False
-- ghci> "cats" `isInfixOf` "im a cat burglar"
-- False

-- isPrefixOf            isSurfixOf
-- ghci> "hey" `isPrefixOf` "hey there!"
-- True
-- ghci> "hey" `isPrefixOf` "oh hey there!"
-- False
-- ghci> "there!" `isSuffixOf` "oh hey there!"
-- True
-- ghci> "there!" `isSuffixOf` "oh hey there"
-- False

-- partition
-- !!!! is different than 'span' and 'break'
-- ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
-- ("BOBMORGAN","sidneyeddy")
-- ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]
-- ([5,6,7],[1,3,3,2,1,0,3])

-- find                 findIndex       findIndices
-- find :: (a -> Bool) -> [a] -> Maybe a
-- ghci> find (>4) [1,2,3,4,5,6]
-- Just 5
-- ghci> find (>7) [1,2,3,4,5,6]
-- Nothing
--
-- ghci> findIndex (==4) [5,3,2,1,6,4]
-- Just 5
-- ghci> findIndex (==7) [5,3,2,1,6,4]
-- Nothing
-- ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
-- [0,6,10,14]

-- elemIndex            elemIndices
-- elemIndex :: (Eq a) -> a -> [a] -> Maybe a
-- ghci> 4 `elemIndex` [1,2,3,4,5,6]
-- Just 3
-- ghci> 9 `elemIndex` [1,2,3,4,5,6]
-- Nothing
--
-- ghci> ' ' `elemIndices` "This is a sentence"
-- [4,7,9]

-- lines                unlines
-- ghci> lines "first line\nsecond line\nthird line"
-- ["first line","second line","third line"]
-- ghci> unlines ["first line", "second line", "third line"]
-- "first line\nsecond line\nthird line\n"]

-- words                unwords
-- ghci> words "hey these are the words in this sentence"
-- ["hey","these","are","the","words","in","this","sentence"]
-- ghci> words "hey these are the words in this\nsentence"
-- ["hey","these","are","the","words","in","this","sentence"]
-- ghci> unwords ["hey", "there", "mate"]
-- "hey there mate"

-- nub
-- same as unique
-- ghci> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
-- [1,2,3,4]
-- ghci> nub "Lots of words and stuff"
-- "Lots fwrdanu"

-- delete
-- ghci> delete 'h' "hey there hhhhhhh!"
-- "ey there hhhhhhh!"
-- ghci> delete 'h' . delete 'h' $ "hey there!"
-- "ey tere!"

-- \\
--ghci> [1..10] \\ [2,5,9]
--[1,3,4,6,7,8,10]
--ghci> "Im a big boy" \\ "big"
--"Im a  boy"

-- union            intersect
-- ghci> "hey man" `union` "man what's up"
-- "hey manwt'sup"
-- ghci> [1..7] `union` [5..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- ghci> [1..7] `intersect` [5..10]
-- [5,6,7]

-- insert
-- insert the first element which is 'larger' than this
-- ghci> insert 4 [1,2,3,5,6,7]
-- [1,2,3,4,5,6,7]
-- ghci> insert 'g' $ ['a'..'f'] ++ ['h'..'z']
-- "abcdefghijklmnopqrstuvwxyz"
-- ghci> insert 3 [1,2,4,3,2,1]
-- [1,2,3,4,3,2,1]

-- groupBy          on
-- ghci> let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4,
-- -14.5, 2.9, 2.3]
-- ghci> groupBy (\x y -> (x > 0) == (y > 0)) values
-- [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
--
-- on : Data.Function
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- f `on` g = \x y -> f(g x) (g y)
-- ghci> groupBy ((==) `on` (> 0)) values
-- [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]


-- Data.Char
--
-- all : Data.List          isAlphaNum : Data.Char
-- ghci> all isAlphaNum "bobby283"
-- True
-- ghci> all isAlphaNum "eddy the fish!"
-- False

-- isSpace : Data.Char
-- ["hey","guys","its","me"]
-- ghci> groupBy ((==) `on` isSpace ) "hey guys its me"
-- ["hey"," ","guys"," ","its"," ","me"]
--
-- ghci> filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"
--["hey","guys","its","me"]

-- generalCategory
-- ghci> generalCategory ' '
-- Space
-- ghci> generalCategory 'A'
-- UppercaseLetter
-- ghci> generalCategory 'a'
-- LowercaseLetter
-- ghci> generalCategory '.'
-- OtherPunctuation
-- ghci> generalCategory '9'
-- DecimalNumber
-- ghci> map generalCategory "\t\nA9?|"
-- [Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]

-- ghci> map digitToInt "34538"
-- [3,4,5,3,8]
-- ghci> map digitToInt "FF85AB"
-- [15,15,8,5,10,11]
-- ghci> intToDigit 15
-- 'f'
-- ghci> intToDigit 5
-- '5'

-- ghci> ord 'a'
-- 97
-- ghci> chr 97
-- 'a'
-- ghci> map ord "abcdefg"
-- [97,98,99,100,101,102,103]

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted
-- ghci> encode 3 "Heeeeey"
-- "Khhhhh|"
-- ghci> encode 4 "Heeeeey"
-- "Liiiii}"
-- ghci> encode 1 "abcd"
-- "bcde"
-- ghci> encode 5 "Marry Christmas! Ho ho ho!"
-- "Rfww~%Hmwnxyrfx&%Mt%mt%mt&"

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
-- ghci> encode 3 "Im a litter teapot"
-- "Lp#d#olwwhu#whdsrw"
-- ghci> decode 3 "Lp#d#olwwhu#whdsrw"
-- "Im a litter teapot"
-- ghci> decode 5 . encode 5 $ "This is a sentence"
-- "This is a sentence"


-- Data.Map
-- phoneBook = [("betty", "555-2938"),
--              ("bonnie", "452-2938"),
--              ("pasty", "493-2938"),
--              ("lucille", "205-2938"),
--              ("wendy", "939-8282"),
--              ("penny", "853-2493")]
-- findKey :: (Eq k) => k -> [(k, v)] -> v
-- findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr(\(k,v) acc -> if key == k then Just v else acc) Nothing
-- findKey key [] = Nothing
-- findKey Key ((k, v) : xs) =
--     if key == k then
--         just v
--     else
--         findKey key xs


-- Map.fromList
-- ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)]
-- fromList [(1,2),(3,2),(5,5)]

-- ghci> Map.empty
-- fromList []
-- ghci> Map.insert 3 100 Map.empty
-- fromList [(3,100)]
-- ghci> Map.insert 3 100 Map.empty
-- fromList [(3,100)]
-- ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100 Map.empty))
-- fromList [(3,100),(4,200),(5,600)]

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

-- null
-- ghci> Map.null Map.empty
-- True
-- ghci> Map.null $ Map.fromList [(2,3), (5,5)]
-- False

-- size
-- ghci> Map.size Map.empty
-- 0
-- ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]
-- 5

-- singleton
-- ghci> Map.singleton 3 9
-- fromList [(3,9)]
-- ghci> Map.insert 5 9 $ Map.singleton 3 9
-- fromList [(3,9),(5,9)]

-- member
-- ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]
-- True
-- ghci> Map.member 5 $ Map.fromList [(3,6),(4,3),(6,9)]
-- False

-- map      filter
-- only affects the values
-- ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
-- fromList [(1,100),(2,400),(3,900)]
-- ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
-- fromList [(2,'A'),(4,'B')]


-- toList
-- just opposite function of fromList
-- ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3
-- [(4,3),(9,2)]

phoneBook = [("betty", "555-2938"),
             ("betty", "342-2492"),
             ("bonnie", "452-2938"),
             ("pasty", "493-2938"),
             ("pasty", "943-2929"),
             ("pasty", "827-9162"),
             ("lucille", "205-2938"),
             ("wendy", "939-8282"),
             ("penny", "853-2493")]
-- phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
-- phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
-- ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
-- Nothing
-- ghci> Map.lookup "pasty" $ phoneBookToMap phoneBook
-- Just "827-9162, 943-2929, 493-2938"
-- ghci> Map.lookup "penny" $ phoneBookToMap phoneBook
-- Just "853-2493"

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
-- ghci> Map.lookup "pasty" $ phoneBookToMap phoneBook
-- Just ["827-9162","943-2929","493-2938"]

-- fromListWith
-- ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,222),(3,11),(4,22),(4,11)]
-- fromList [(2,100),(3,222),(4,22)]
-- ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,222),(3,11),(4,22),(4,11)]
-- fromList [(2,108),(3,262),(4,33)]

-- insertWith
-- ghci> Map.insertWith (+) 3 100 $ Map.fromList[(3,4),(5,103),(6,330)]
-- fromList [(3,104),(5,103),(6,330)]


-- Data.Set
--
-- text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
-- text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
-- ghci> let set1 = Set.fromList text1
-- ghci> let set2 = Set.fromList text2
-- ghci> set1
-- fromList " .?AIRadefhijlmnorstuy"
-- ghci> set2

-- intersection
-- fromList " !Tabcdefghilmnorstuvwy"
-- ghci> Set.intersection set1 set2
-- fromList " adefhilmnorstuy"

-- difference
-- ghci> Set.difference set1 set2
-- fromList ".?AIRj"
-- ghci> Set.difference set2 set1
-- fromList "!Tbcgvw"

-- union
-- ghci> Set.union set1 set2
-- fromList " !.?AIRTabcdefghijlmnorstuvwy"

-- null
-- ghci> Set.null Set.empty
-- True
-- ghci> Set.null $ Set.fromList [3,4,5,5,4,3]
-- False

-- size
-- ghci> Set.size $ Set.fromList [3,4,5,5,4,3]
-- 3

-- singleton
-- ghci> Set.singleton 9
-- fromList [9]

-- insert
-- ghci> Set.insert 4 $ Set.fromList [9,3,8,1]
-- fromList [1,3,4,8,9]
-- ghci> Set.insert 8 $ Set.fromList [5..10]
-- fromList [5,6,7,8,9,10]

-- delete
-- ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]
-- fromList [3,5]

-- isSubsetOf           isProperSubsetOf
-- ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
-- True
-- ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
-- True
-- ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList
-- [1,2,3,4,5]
-- False
-- ghci> Set.fromList [2,3,4] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]
-- True
-- ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
-- False

-- filter               map
-- ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
-- fromList [3,5,7]
-- ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
-- fromList [3,4,5,6,7,8]

-- nub
-- ghci> let setNub xs = Set.toList $ Set.fromList xs
-- ghci> setNub "HEY WHATS CRACKALACKIN"
-- " ACEHIKLNRSTWY"
-- ghci> nub "HEY WHATS CRACKALACKIN"
-- "HEY WATSCRKLIN"










