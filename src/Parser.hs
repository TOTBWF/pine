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

variable :: Parser ITerm
variable = do
    x <- identifier
    return $ IVar x

universe :: Parser ITerm
universe = do
    reserved "Type"
    k <- integer
    return $ IUniverse $ fromIntegral k

prop :: Parser ITerm
prop = do
    reserved "Prop"
    return IProp

typeof :: Parser (Variable, ITerm)
typeof = do
    x <- identifier
    reservedOp ":"
    t <- term
    return (x, t)

lambda :: Parser ITerm
lambda = do
    reserved "fun"
    ts <- many $ parens typeof
    reservedOp "=>"
    e <- term
    return $ foldr (\(x, t) e' -> ILambda(x, t, e')) e ts

forall :: Parser ITerm
forall = do
    reserved "forall"
    x <- identifier
    reservedOp ":"
    t <- term
    _ <- comma
    e <- term
    return $ IPi (x, t, e)

aexp :: Parser ITerm
aexp = 
        parens term
    <|> lambda
    <|> prop
    <|> universe
    <|> forall
    <|> variable


t :: Parser ITerm
t = aexp >>= \x ->
                (many1 aexp >>= \xs -> return $ foldl IApp x xs)
                <|> return x

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators ITerm
table = [
        [
            infixOp "->" (\t1 t2 -> IPi ("_", t1, t2)) Ex.AssocLeft
        ]
    ]

term :: Parser ITerm
term = Ex.buildExpressionParser table t

data Top 
    = Parameter Variable ITerm
    | Definition Variable ITerm 

parameter :: Parser Top
parameter = do
    x <- identifier
    reservedOp ":"
    t <- term
    return $ Parameter x t

definition :: Parser Top
definition = do
    x <- identifier
    reservedOp ":="
    t <- term
    return $ Definition x t

top :: Parser Top
top =
        try parameter
    <|> definition

parseTerm :: L.Text -> Either ParseError ITerm
parseTerm s = parse term "<stdint>" s

parseTop :: L.Text -> Either ParseError Top
parseTop s = parse top "<stdint>" s

