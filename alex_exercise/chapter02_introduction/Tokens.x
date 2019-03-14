--    Used to declare the module name for the generated Haskell module, in this
--    case Main.
{
module Main (main) where
}


--    Controls what kind of support code Alex should producea along with the basic
--    scanner.
--    The "basic" wrapper selects a scanner that tokenises a "String" and returns
--    a list of tokens.
%wrapper "basic"


--    Defines the "$digit" and "$alpha" macros for use in the token definitions.
$digit = 0-9                -- digits
$alpha = [a-zA-Z]           -- alphabetic characters



--    The "tokens :-" line ends the macro definitions and starts the definition
--    of the scanner.
--
--    The scanner is specified as a series of token definitinos where each token
--    specification takes the form of
--
--        "regexp { code }"
--
--    The meaning of this rule is "if the input matches 'regexp', then return 
--    'code'". The code part along with the braces can be replaced by simply ';',
--    meaning that this token should be ingnored in the input stream. As you can
--    see, we've used this to ignore whitespace in our example.
--
--    Our scanner is set up so that the actions are all functions with type
--    'String -> Token'. When the token is matched, the portion of the input
--    stream that it matched is matched is passed to the appropriate action
--    function as a 'String'.
tokens :-

    $white+                             ;
    "--".*                              ;
    let                                 { \s -> Let }
    in                                  { \s -> In }
    $digit+                             { \s -> Int (read s) }
    [\=\+\-\*\/\(\)]                    { \s -> Sym (head s) }
    $alpha [$alpha $digit \_ \']*       { \s -> Var s }



--    At the bottom of the file we have another code fragment, surrounded by
--    braces { ... }. In this fragment, we declare the type of the tokens, and
--    give a 'main' function that we can use for testing it; the 'main' function
--    just tokenises the input and prints the results to standard output.
--
--    Alex has kindly provided the following function which we can use to invoke
--    the scanner:
--
--        alexScanTokens :: String -> [Token]
--
--    Alex arranges for the input stream to be tokenised, each of the action
--    functions to be passed the appropriate 'String', and a list of 'Token's
--    returned as the result. If the input stream is lazy, the output stream will
--    also be produced lazily.
--
--    We have demostrated the simplest form of scanner here, which was seleceted
--    by the '%wrapper "basic"' line near the top of the file. In general, actions
--    do not have type 'String->Token', and there's no requirement for the scanner
--    to return a list of tokens.
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
        Let             |
        In              |
        Sym Char        |
        Var String      |
        Int Int
        deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
