module Pine.AbsSyntax
    ( AbsTerm (..)
    , Variable
    , desugar
    )
where

import Data.Text (Text)
import Pine.Syntax
import Pine.Context
import Data.List 

data AbsTerm
    = AbsVar Variable
    | AbsProp
    | AbsUniverse Int
    | AbsApp AbsTerm AbsTerm
    | AbsLambda Variable AbsTerm AbsTerm
    | AbsPi Variable AbsTerm AbsTerm
    deriving Show

data DesugarError 
    = UnknownIdentifier Variable
    deriving Show

index :: [Variable] -> Variable -> Either DesugarError Int
index ctx x = case elemIndex x ctx of
    Just i -> Right i
    Nothing -> Left $ UnknownIdentifier x

-- Transforms an input term into the DeBruijn Index form
desugar :: Context -> AbsTerm -> Either DesugarError Term
desugar c i = desugar' (names c) i
    where
        desugar' ctx i = case i of
            AbsVar x -> Var <$> (index ctx x)
            AbsUniverse k -> return $ Universe k
            AbsProp -> return Prop
            AbsLambda x t e -> Lambda <$> desugarAbstraction ctx (x,t,e)
            AbsPi x t e -> Pi <$> desugarAbstraction ctx (x, t, e)
            AbsApp e1 e2 -> do
                e1' <- desugar' ctx e1
                e2' <- desugar' ctx e2
                return $ App e1' e2'
        desugarAbstraction ctx (x, t, e) = do
                t' <- desugar' ctx t
                e' <- desugar' (x:ctx) e
                return (x, t', e')