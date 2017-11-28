{-# LANGUAGE OverloadedStrings #-}

module Parser (parseTerm, parseTop, Top(IParameter, IDefinition, IInductive)) where

import Prelude hiding (pi)

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax

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
    if k > 0
    then return $ IUniverse $ fromIntegral k
    else fail "Undefined Universe"

prop :: Parser ITerm
prop = do
    reserved "Prop"
    return IProp

typeann :: Parser ITerm
typeann = do
    reservedOp "::"
    t <- term
    return t

typedef :: Parser (Variable, ITerm)
typedef = do
    x <- identifier
    t <- typeann
    return (x, t)

lambda :: Parser ITerm
lambda = do
    reserved "fun"
    ts <- many $ parens typedef
    reservedOp "=>"
    e <- term
    return $ foldr (\(x, t) e' -> ILambda(x, t, e')) e ts

forall :: Parser ITerm
forall = do
    reserved "forall"
    x <- identifier
    reservedOp "::"
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
            infixOp "->" (\t1 t2 -> IPi ("_", t1, t2)) Ex.AssocRight
        ]
    ]

term :: Parser ITerm
term = Ex.buildExpressionParser table t

data Top 
    = IParameter Variable ITerm
    | IDefinition Variable ITerm --ITerm 
    | IInductive Variable ITerm [(Variable, ITerm)]

parameter :: Parser Top
parameter = do
    (x, t) <- typedef
    return $ IParameter x t

definition :: Parser Top
definition = do
    reserved "let"
    x <- identifier
    -- args <- many $ parens typedef
    -- t <- typeann
    reservedOp ":="
    e <- term
    return $ IDefinition x e --e

inductive :: Parser Top
inductive = do
    reserved "inductive"
    n <- identifier
    args <- many $ parens typedef
    t <- typeann
    reservedOp ":="
    c <- typedef `sepBy1` reservedOp "|" 
    return $ IInductive n (foldr (\(x, t) p -> IPi (x, t, p)) t args) c

top :: Parser Top
top =
        try parameter
    <|> definition
    <|> inductive

parseTerm :: L.Text -> Either ParseError ITerm
parseTerm s = parse term "<stdint>" s

parseTop :: L.Text -> Either ParseError Top
parseTop s = parse top "<stdint>" s

