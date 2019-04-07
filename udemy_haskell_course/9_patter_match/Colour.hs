-- data Colour = RGB Int Int Int deriving Show
-- red :: Colour -> Int
-- red (RGB r _ _ ) = r
--
-- green :: Colour -> Int
-- green (RGB _ g _ ) = g
--
-- blue :: Colour -> Int
-- blue (RGB _ _ b ) = b

-- main :: IO ()
-- main = do
-- let c = RGB 10 20 30
--     print $ red c
--     print $ green c
--     print $ blue c

-- data Colour = RGB Int Int Int deriving Show
-- data Pixel = Pixel Int Int Int Colour
--
-- pixelRed :: Pixel -> Int
-- pixelRed (Pixel _ _ _ ( RGB r _ _)) = r
--
-- main :: IO ()
-- main = do
--     let p = Pixel 100 100 100 (RGB 10 20 30)
--     print $ pixelRed p

data Colour = RGB Int Int Int | CMYK Float Float Float Float deriving Show

-- colourModel :: Colour -> String
-- colourModel (RGB _ _ _) = "RGB"
-- colourModel (CMYK _ _ _ _) = "CMYK"

colourModel :: Colour -> String
colourModel c =
    case c of RGB _ _ _ -> "RGB"
--               CMYK _ _ _ _ -> "CMYK"

main :: IO ()
main = do
    let c = CMYK 1.0 2.0 3.0 4.0
    putStrLn $ colourModel c
