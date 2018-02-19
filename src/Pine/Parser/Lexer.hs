
module Pine.Parser.Lexer
    ( symbol
    , parens
    , comma
    , arrow
    , fatArrow
    , doubleColon
    , colonEquals
    , pipe
    , integer
    , reserved
    , identifier
    , uidentifier
    , Parser
    )
where
    
import Data.Functor (void)
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Text (Text)

import Pine.Parser.Stack

lineCmt :: Parser ()
lineCmt = L.skipLineComment "--"

blockCmt :: Parser ()
blockCmt = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space P.space1 lineCmt blockCmt

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser ()
symbol' = void . symbol 

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

comma :: Parser ()
comma = symbol' ","

arrow :: Parser ()
arrow = symbol' "->"

fatArrow :: Parser ()
fatArrow = symbol' "=>"

doubleColon :: Parser ()
doubleColon = symbol' "::"

colonEquals :: Parser ()
colonEquals = symbol' ":="

pipe :: Parser ()
pipe = symbol' "|"

reserved :: Text -> Parser ()
reserved s = lexeme (P.string s *> P.notFollowedBy P.alphaNumChar)

identifier :: Parser Text
identifier = (lexeme . P.try) (ident >>= checkReserved)
    where
        ident = T.cons <$> P.lowerChar <*> (T.pack <$> P.many (P.alphaNumChar <|> P.char '\''))

uidentifier :: Parser Text
uidentifier = (lexeme . P.try) (ident >>= checkReserved)
    where
        ident = T.cons <$> P.upperChar <*> (T.pack <$> P.many (P.alphaNumChar <|> P.char '\''))

checkReserved :: Text -> Parser Text
checkReserved i = if i `elem` reservedWords
                    then fail $ "reserved word " ++ (T.unpack i) ++ " is not a valid identifier"
                    else return i

reservedWords :: [Text]
reservedWords = [
        "fun",
        "forall",
        "let",
        "Type",
        "Prop",
        "Inductive"
    ]

-- lexer :: Tok.GenTokenParser L.Text () Identity
-- lexer = Tok.makeTokenParser $ Tok.LanguageDef
--     {
--         Tok.commentStart        = "{",
--         Tok.commentEnd          = "}",
--         Tok.commentLine         = "{{",
--         Tok.nestedComments      = True,
--         Tok.identStart          = letter <|> oneOf "_",
--         Tok.identLetter         = alphaNum <|> oneOf "_'",
--         Tok.reservedNames       = reservedNames,
--         Tok.reservedOpNames     = reservedOpNames,
--         Tok.opStart             = oneOf ":!#$%&*+./<=>?@\\^|-~",
--         Tok.opLetter            = oneOf ":!#$%&*+./<=>?@\\^|-~",
--         Tok.caseSensitive       = True
--     }

-- reserved :: String -> Parser ()
-- reserved = Tok.reserved lexer

-- reservedOp :: String -> Parser ()
-- reservedOp = Tok.reservedOp lexer

-- identifier :: Parser String
-- identifier = Tok.identifier lexer

-- parens :: Parser a -> Parser a
-- parens = Tok.parens lexer

-- comma :: Parser String
-- comma = Tok.comma lexer

-- dotEnd :: Parser a -> Parser [a]
-- dotEnd p = endBy p (Tok.dot lexer)

-- dotEnd1 :: Parser a -> Parser [a]
-- dotEnd1 p = endBy1 p (Tok.dot lexer)