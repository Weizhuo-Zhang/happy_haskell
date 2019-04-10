module Main where

import GoatAST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q

type Parser a
    = Parsec String Int a

lexer :: Q.TokenParser Int
lexer
    = Q.makeTokenParser
    ( emptyDef
    { Q.commentLine     = "#"
    , Q.nestedComments  = True
    , Q.identStart      = letter
    , Q.identLetter     = alphaNum <|> char '_'
    , Q.opStart         = oneOf "+-*:"
    , Q.opLetter        = oneOf "+=*:=<>!|&"
    , Q.reservedNames   = myReserved
    , Q.reservedOpNames = myOpnames
    })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
float      = Q.float lexer
decimal    = Q.decimal lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
dot        = Q.dot lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved, myOpnames :: [String]

myReserved
    = [ "begin", "bool", "call" , "do"  , "else", "end"
      , "false", "fi"  , "float", "if"  , "int" , "od"
      , "proc" , "read", "ref"  , "then", "true", "val"
      , "while", "write"
      ]

myOpnames
  = ["+",  "-",  "*", "/", "<", "<=", ">", ">=", "=", "!=",
     "||", "&&", "!", ":="]

pProg :: Parser GoatProgram
pProg
  = do
      procedures <- many1 pProcedure
      return (Program procedures)

pProcedure :: Parser Procedure
pProcedure
  = do
      reserved "proc"
      header <- pProgHeader
      body   <- pProgBody
      reserved "end"
      return (Procedure header body)

pProgHeader :: Parser Header
pProgHeader
  = do
    ident  <- identifier
    char '('
    params <- sepBy pParameter comma
    char ')'
    newline
    whiteSpace
    return (Header ident params)

pParameter :: Parser Parameter
pParameter
  = do
    pidcat <- pPindicator
    ptype  <- pType
    ident  <- identifier
    return (Parameter pidcat ptype ident)

pPindicator :: Parser PIndicator
pPindicator
  = do { reserved "var"; return VarType }
    <|>
    do { reserved "ref"; return RefType }

pType :: Parser PType
pType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }

pProgBody :: Parser Body
pProgBody
  = do
    vdecls <- many pVDecl
    reserved "begin"
    stmts   <- many1 pStmt
    return (Body vdecls stmts)

pVDecl :: Parser VDecl
pVDecl
  = do
    ptype  <- pType
    ident  <- identifier
    sidcat <- optional pSindicator
    semi
    return (VDecl ptype ident sidcat)

pSindicator :: Parser SIndicator
pSindicator
  = do { char '['
       ; n <- pNum
       ; comma;
       ; m <- pNum
       ; char ']'
       ; return (Matrix (n, m))
       }
    <|>
    do { char '['
       ; n <- pNum
       ; char ']'
       ; return (Array n)
       }

pStmt, pAsg, pRead, pWrite, pCall, pIf, pIfElse, pWhile :: Parser Stmt

pStmt
  = choice [pAsg, pRead, pWrite, pCall, pIf, pIfElse, pWhile]

pAsg
  = do
    lvalue <- pLvalue
    reservedOp ":="
    rvalue <- pExp
    semi
    return (Assign lvalue rvalue)

pRead
  = do
    reserved "read"
    lvalue <- pLvalue
    semi
    return (Read lvalue)

pWrite
  = do
    reserved "write"
    exp <- (pString <|> pExp)
    semi
    return (Write exp)

pCall
  = do
    reserved "call"
    lvalue <- pLvalue
    explist <- optional (sepBy pExp comma)
    semi
    return (Call lvalue explist)

pIf
  = do
    reserved "if"
    exp <- pExp
    reserved "then"
    stmts <- many1 pStmt
    reserved "fi"
    semi
    return (If exp stmts)

pIfElse
  = do
    reserved "if"
    exp <- pExp
    reserved "then"
    stmts1 <- many1 pStmt
    reserved "else"
    stmts2 <- many1 pStmt
    reserved "fi"
    semi
    return (IfElse exp stmts1 stmts2)

pWhile
  = do
    reserved "while"
    exp <- pExp
    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    semi
    return (While exp stmts)

pExp, pNum, pIdent, pString, pBool :: Parser Expr

pExp
  = buildExpressionParser table pFac

pFac :: Parser Expr
pFac = choice [parens pExp, pNum, pIdent, pBool]

table = [ [ prefix   "-" UnaryMinus]
        , [ binary   "*" Mul, binary   "/" Div]
        , [ binary   "+" Add, binary   "-" Sub]
        , [ relation "=" Eq,  relation "!=" NotEq
          , relation "<" Les, relation "<=" LesEq
          , relation ">" Grt, relation ">=" GrtEq
          ]
        , [prefix "!"  UnaryNot]
        , [binary "&&" And]
        , [binary "||" Or]
        ]

prefix name func
  = Prefix ( do { reservedOp name; return func})

binary name op
  = Infix ( do { reservedOp name; return op}) AssocLeft

relation name rel
  = Infix ( do { reservedOp name; return rel}) AssocNone

pNum
  = try
    ( do
      { n <- natural
        <?>
        "integer";
        return (IntConst (fromInteger n :: Int))
      }
      <|>
      do
      { n <- many1 digit;
        char '.'
        <?>
        "float";
        m <- many1 digit;
        return (FloatConst (read (n ++ "." ++ m) :: Float))

      }
    )

pIdent
  = do
    ident <- identifier
    return (Id ident)
    <?>
    "identifier"

pString
  = do
    char '"'
    str <- many1 (satisfy (/= '"'))
    char '"'
    return (StrConst str)
    <?>
    "String"

pBool
  = do { reserved "true"; return (BoolConst True) }
    <|>
    do { reserved "false"; return (BoolConst False) }
    <?>
    "bool"

pLvalue :: Parser Lvalue
pLvalue
  = do
    ident <- identifier
    return (LId ident)
    <?>
    "lvalue"


pMain :: Parser GoatProgram
pMain
  = do
    whiteSpace
    p <- pProg
    eof
    return p

-- data ProgramParameters = ProgramParameters
--     { isPrettyPrint :: Bool
--     , fileName      :: String
--     }

main :: IO ()
main
  = do { -- let a = ProgramParameters { isPrettyPrint = True
         --                           , fileName      = "filename"}
         input <- readFile ("asg.gt")
                                    -- 0 stands for initState
        ; let output = runParser pMain 0 "asg.gt" input
        ; case output of
             Right ast -> print ast
             Left  err -> do { putStr "Parse error at "
                             ; print err}
       ; print "testMain"
       }
