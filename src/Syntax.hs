module Syntax where

-- A variable in an expression
data Variable 
    = Sym String -- A Variable with a given name
    | GenSym String Integer -- We need to generate variables, and use the index to keep track of x1, x2 ...
    | Dummy -- Only used to pretty print
    deriving (Show, Eq, Ord)



-- An abstraction '(x,t,e)' is just a Variable 'x' bound to a Type 't' in Expr 'e'
type Abstraction = (Variable, Expr, Expr)

-- Types are also expressions, because dependent types
data Expr
    = Var Variable 
    | Universe Integer -- Universes of types, to avoid the type theory version of Russels paradox
    | Pi Abstraction -- Dependent Product Type
    | Lambda Abstraction -- Lambda Function
    | App Expr Expr -- Application
    deriving Show
