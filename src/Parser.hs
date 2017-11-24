{-# LANGUAGE OverloadedStrings #-}

module Parser (parseExpr, parseTop, Top(Parameter, Definition)) where

import Prelude hiding (pi)

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax

import Debug.Trace

integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser Expr
variable = do
    x <- identifier
    return $ Var (Sym x)

universe :: Parser Expr
universe = do
    reserved "Type"
    k <- integer
    return $ Universe k

lambda :: Parser Expr
lambda = do
    reserved "fun"
    x <- identifier
    reservedOp ":"
    t <- expr
    reservedOp "=>"
    e <- expr
    return $ Lambda (Sym x, t, e)

forall :: Parser Expr
forall = do
    reserved "forall"
    x <- identifier
    reservedOp ":"
    t <- expr
    _ <- comma
    e <- expr
    return $ Pi (Sym x, t, e)

aexp :: Parser Expr
aexp = 
        parens expr
    <|> lambda
    <|> universe
    <|> forall
    <|> variable


term :: Parser Expr
term = aexp >>= \x ->
                (many1 aexp >>= \xs -> return $ foldl App x xs)
                <|> return x

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
table = [
        [
            infixOp "->" (\t1 t2 -> Pi (Dummy, t1, t2)) Ex.AssocLeft
        ]
    ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

data Top 
    = Parameter Variable Expr
    | Definition Variable Expr 

parameter :: Parser Top
parameter = do
    x <- identifier
    reservedOp ":"
    t <- expr
    return $ Parameter (Sym x) t

definition :: Parser Top
definition = do
    x <- identifier
    reservedOp ":="
    t <- expr
    return $ Definition (Sym x) t

top :: Parser Top
top =
        try parameter
    <|> definition

parseExpr :: L.Text -> Either ParseError Expr
parseExpr s = parse expr "<stdint>" s

parseTop :: L.Text -> Either ParseError Top
parseTop s = parse top "<stdint>" s

