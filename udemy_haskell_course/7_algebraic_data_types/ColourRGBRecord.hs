module ColourRGBRecord (Colour) where

data Colour = RGB
    { red :: Int,
    , green :: Int,
    , blur :: Int
    } deriving Show
