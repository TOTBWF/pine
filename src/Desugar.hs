module Desugar where

import Syntax
import Context
import Data.List 

data DesugarError 
    = UnknownIdentifier Variable
    deriving Show

index :: [Variable] -> Variable -> Either DesugarError Int
index ctx x = case elemIndex x ctx of
    Just i -> Right i
    Nothing -> Left $ UnknownIdentifier x


-- Transforms an input term into the DeBruijn Index form
desugar :: Context -> ITerm -> Either DesugarError Term
desugar c i = desugar' (names c) i
    where
        desugar' ctx i = case i of
            IVar x -> Var <$> (index ctx x)
            IUniverse k -> return $ Universe k
            IProp -> return Prop
            ILambda a -> Lambda <$> desugarAbstraction ctx a
            IPi a -> Pi <$> desugarAbstraction ctx a
            IApp e1 e2 -> do
                e1' <- desugar' ctx e1
                e2' <- desugar' ctx e2
                return $ App e1' e2'
        desugarAbstraction ctx (x, t, e) = do
                t' <- desugar' ctx t
                e' <- desugar' (x:ctx) e
                return (x, t', e')
