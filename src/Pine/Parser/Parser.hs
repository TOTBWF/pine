module Pine.Parser.Parser 
    ( Top(..)
    , parseTerm
    , parseTop
    )
where

import Prelude hiding (pi)

import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P 
import qualified Text.Megaparsec.Expr as P
import Data.Text (Text)
import Data.List (foldl')

import Pine.Parser.Stack
import Pine.Parser.Lexer
import Pine.AbsSyntax

typeann :: Parser AbsTerm
typeann = doubleColon *> term

typedef :: Parser (Variable, AbsTerm)
typedef = (,) <$> identifier <*> typeann

termExpr :: Parser AbsTerm
termExpr = P.choice 
    [ parens term
    , flip (foldr (\(x, t) e -> AbsLambda x t e)) <$> (reserved "fun" *> P.many (parens typedef)) <*> (fatArrow *> term)
    , reserved "Prop" *> pure AbsProp
    , AbsUniverse . fromIntegral <$> (reserved "Type" *> integer)
    , AbsPi <$> (reserved "forall" *> identifier) <*> (typeann) <*> (comma *> term)
    , AbsVar <$> identifier
    , AbsVar <$> uidentifier
    ]


appTerm :: Parser AbsTerm
appTerm = termExpr >>= \t ->
                        ((\ts -> foldl' AbsApp t ts) <$> P.some termExpr)
                        <|> return t


operators :: [[P.Operator Parser AbsTerm]]
operators = 
    [[ P.InfixR (AbsPi "_" <$ symbol "->") ]]


term :: Parser AbsTerm
term = P.makeExprParser appTerm operators

data Top 
    = AbsParameter Variable AbsTerm
    | AbsDefinition Variable AbsTerm
    | AbsInductive Variable AbsTerm [(Variable, AbsTerm)]

parameter :: Parser Top
parameter = uncurry AbsParameter <$> typedef

definition :: Parser Top
definition = AbsDefinition <$> (reserved "let" *> identifier) <*> (colonEquals *> term)

inductive :: Parser Top
inductive = AbsInductive 
                <$> (reserved "inductive" *> uidentifier) 
                <*> (flip (foldr (\(x,t) p -> AbsPi x t p)) <$> (P.many $ parens typedef) <*> typeann) 
                <*> (colonEquals *> typedef `P.sepBy1` pipe)


top :: Parser Top
top = P.try parameter
    <|> definition
    <|> inductive

parseError :: (P.ShowToken t, Ord t, P.ShowErrorComponent e) => Either (P.ParseError t e) a -> Either String a
parseError (Left err) = Left $ P.parseErrorPretty err
parseError (Right r) = (Right r)

parseTerm :: Text -> Either String AbsTerm
parseTerm s = parseError $ P.parse term "<stdint>" s 


parseTop :: Text -> Either String Top
parseTop s = parseError $ P.parse top "<stdint>" s

