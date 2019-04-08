{
module Main (main) where
}

%wrapper "basic"

tokens :-

    $white+             ;
    an                  { \s -> AN }
    a                   { \s -> A }
    the                 { \s -> THE }
    .                   ;

{

data Token = A
           | AN
           | THE
           deriving (Eq, Show)

calfreq :: (Eq a) => [a] -> a -> Int
calfreq [] _ = 0
calfreq (x:xs) a =
    if x == a then 1 + calfreq xs a
    else calfreq xs a

main = do
    s <- getContents
    let tokenList = alexScanTokens s
    let numAn  = calfreq tokenList AN
    let numA   = calfreq tokenList A
    let numThe = calfreq tokenList THE
    print ("num of An "  ++ show numAn)
    print ("num of A "   ++ show numA)
    print ("num of The " ++ show numThe)
}
