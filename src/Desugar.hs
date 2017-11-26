module Desugar where

import Syntax
import Context
import Data.List 

data DesugarError = UnknownIdentifier Variable

index :: [Variable] -> Variable -> Either DesugarError Int
index ctx x = case elemIndex x ctx of
    Just i -> Right i
    Nothing -> Left $ UnknownIdentifier x


desugar :: [Variable] -> ITerm -> Either DesugarError Term
desugar ctx i = case i of
    IVar x -> Var <$> (index ctx x)
    IUniverse k -> return $ Universe k
    IProp -> return Prop
    ILambda a -> Lambda <$> desugarAbstraction a
    IPi a -> Pi <$> desugarAbstraction a
    IApp e1 e2 -> do
        e1' <- desugar ctx e1
        e2' <- desugar ctx e2
        return $ App e1' e2'
    where desugarAbstraction (x, t, e) = do
            t' <- desugar ctx t
            e' <- desugar (x:ctx) e
            return (x, t', e')

    