
module Lexer where

    
import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity

-- Use the parsec built in expression parsers
type Op a = Ex.Operator L.Text () Identity a
type Operators a = Ex.OperatorTable L.Text () Identity a

reservedNames :: [String]
reservedNames = [
        "fun",
        "forall",
        "let",
        "Type",
        "Prop",
        "Inductive"
    ]

reservedOpNames :: [String]
reservedOpNames = [
        "=>",
        "->",
        "::",
        ":=",
        "|"
    ]

lexer :: Tok.GenTokenParser L.Text () Identity
lexer = Tok.makeTokenParser $ Tok.LanguageDef
    {
        Tok.commentStart        = "{",
        Tok.commentEnd          = "}",
        Tok.commentLine         = "{{",
        Tok.nestedComments      = True,
        Tok.identStart          = letter <|> oneOf "_",
        Tok.identLetter         = alphaNum <|> oneOf "_'",
        Tok.reservedNames       = reservedNames,
        Tok.reservedOpNames     = reservedOpNames,
        Tok.opStart             = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.opLetter            = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.caseSensitive       = True
    }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

comma :: Parser String
comma = Tok.comma lexer

dotEnd :: Parser a -> Parser [a]
dotEnd p = endBy p (Tok.dot lexer)

dotEnd1 :: Parser a -> Parser [a]
dotEnd1 p = endBy1 p (Tok.dot lexer)