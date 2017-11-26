module Infer where

import Prelude hiding ((!!))
import Control.Monad.Except
import Control.Monad.Identity

import Syntax


data TypeError 
    = IndexError Integer
type Infer = ExceptT TypeError Identity

infer :: Term -> Context -> Infer Term
infer t ctx = case t of
    Prop -> return $ Universe 1
    Universe k -> return $ Universe (k + 1)
    Var k -> lookupType ctx k