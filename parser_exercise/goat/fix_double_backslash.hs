import Text.Parsec

pString = do
    char '"'
    str <- many (satisfy (/= '"'))
    char '"'
    return str

main :: IO ()
main = do
    pString "sdfd\n"
