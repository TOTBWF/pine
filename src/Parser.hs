{-# LANGUAGE OverloadedStrings #-}

module Parser where

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

arrow :: Parser (Expr -> Expr -> Expr)
arrow = do
    reservedOp "->"
    return (\t1 t2 -> Pi (Dummy, t1, t2))

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

quit :: Parser Directive
quit = do
    reserved "Quit"
    return $ Quit

help :: Parser Directive
help = do
    reserved "Help"
    return $ Help

context :: Parser Directive
context = do
    reserved "Context"
    return $ Context

parameter :: Parser Directive
parameter = do
    reserved "Parameter"
    x <- identifier
    reservedOp ":"
    e <- expr
    return $ Parameter (Sym x) e

definition :: Parser Directive
definition = do
    reserved "Definition"
    x <- identifier
    reservedOp ":="
    e <- expr
    return $ Definition (Sym x) e


check :: Parser Directive
check = do
    reserved "Check"
    e <- expr
    return $ Check e

eval :: Parser Directive
eval = do
    reserved "Eval"
    e <- expr
    return $ Eval e

directive :: Parser Directive
directive =
        quit
    <|> help
    <|> context
    <|> parameter
    <|> definition
    <|> check
    <|> eval

parseDirective :: L.Text -> Either ParseError Directive
parseDirective s = parse directive "<stdint>" s

