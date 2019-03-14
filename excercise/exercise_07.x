{
module Main (main) where
}

%wrapper "basic"

$normalc = [^\" \/ \*]
$ndbquot = [^\"]
@string = \" $ndbquot* \"
@middle = \/ | (\*)* ($normalc | @string)
@comment = \/ \* @middle* \* \/

rules :-

    @comment    { \s -> "<<<" ++ s ++ ">>>" }
    .           { id }
    \n          ;

{
main
    = do
        s <- getContents
        putStrLn(concat (alexScanTokens s))
}
