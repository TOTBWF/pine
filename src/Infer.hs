module Infer where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map

import Syntax

newtype Context = Context (Map.Map Variable (Expr, Maybe Expr))

data Unique = Unique { count :: Int }

data TypeError
    = UnknownIdentifier Variable

type Infer a = ExceptT TypeError (State Unique) a

-- Takes a variable and generates a fresh one
fresh :: Variable -> Infer Variable
fresh x = case x of
    Sym s -> return $ Sym s
    GenSym s _ -> do
        c <- get
        put c { count = count c + 1 }
        return $ GenSym s (count c + 1) 
    Dummy -> do
        c <- get
        put c { count = count c + 1}
        return $ GenSym "_" (count c + 1)

-- Returns the type of 'x' in the Context 'ctx'
lookupType :: Variable -> Context -> Infer Expr
lookupType x (Context ctx) = fmap fst $ Map.lookup x ctx

-- Returns the value of 'x' in the Context 'ctx' if it is a value
lookupValue :: Variable -> Context -> Infer Maybe Expr
lookupValue x (Context ctx) = 
    case Map.lookup x ctx of
    Just v -> return snd v
    Nothing -> throwError $UnknownIdentifier
    

-- Extends 'ctx' with a either a variable bound to a type or a type and a value
extend :: Variable -> Expr -> Maybe Expr -> Context -> Context
extend x t e (Context ctx) = Context $ Map.insert x (t, e) ctx

-- infer :: Expr -> Context -> Infer Expr
-- infer e ctx = 
--     case e of
--     Var v -> do
