{
module Main (main) where
}

%wrapper "basic"

$alpha  = [a-zA-Z]
$alnum  = [a-zA-Z0-9]
@digits = (0-9)+
@float  = @digits \. @digits
@ident  = $alpha $alnum*

rules :-

    $white+     ;
    \=          { \s -> ASG }
    \+          { \s -> ADD }
    \-          { \s -> SUB }
    \*          { \s -> MUL }
    \/          { \s -> DIV }
    \(          { \s -> LEFTPAREN }
    \)          { \s -> RIGHTPAREN }
    \;          { \s -> SEMICOLON }
    @ident      { ID }
    @digits     { INT_CONST }
    @float      { FLOAT_CONST }
    \n          ;
    .           { \s -> OTHER }

{
data Token
    = ASG | ADD | SUB | MUL | DIV
    | LEFTPAREN | RIGHTPAREN | SEMICOLON | OTHER
    | ID String | INT_CONST String | FLOAT_CONST String
    deriving (Eq, Show)
main
    = do
        s <- getContents
        mapM_ print (alexScanTokens s)
}
