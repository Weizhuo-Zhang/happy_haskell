module ColourRGBCMYK (Colour) where

data Colour = RGB Int Int Int | CMYK Float Float Float Float deriving Show
