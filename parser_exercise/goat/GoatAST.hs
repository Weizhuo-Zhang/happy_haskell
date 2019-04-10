module GoatAST where

type Ident = String

data PType
    = BoolType
    | IntType
    | FloatType
    deriving (Show)

data Lvalue
    = LId Ident
    deriving (Show)

data Expr
    = Id Ident
    | BoolConst Bool
    | IntConst Int
    | FloatConst Float
    | StrConst String
    | Expr Expr
    | Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    | Or Expr Expr
    | And Expr Expr
    | Eq Expr Expr
    | NotEq Expr Expr
    | Les Expr Expr
    | LesEq Expr Expr
    | Grt Expr Expr
    | GrtEq Expr Expr
    | UnaryMinus Expr
    | UnaryNot Expr
    deriving (Show)

data VDecl
    = VDecl PType Ident ()
    deriving (Show)

data Stmt
    = Assign Lvalue Expr
    | Read Lvalue
    | Write Expr
    | Call Lvalue ()
    | If Expr [Stmt]
    | IfElse Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    deriving (Show)

data Body
    = Body [VDecl] [Stmt]
    deriving (Show)

data PIndicator
    = VarType
    | RefType
    deriving (Show)

data SIndicator
    = Array Expr
    | Matrix (Expr, Expr)
    deriving (Show)

data Parameter
    = Parameter PIndicator PType Ident
    deriving (Show)

data Header
    = Header Ident [Parameter]
    deriving (Show)

data Procedure
    = Procedure Header Body
    deriving (Show)

data GoatProgram
    = Program [Procedure]
    deriving (Show)
