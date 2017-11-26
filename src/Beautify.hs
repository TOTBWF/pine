module Beautify (beautify) where

import Data.Map as Map
import Data.Char
import Text.Read

import Syntax

type VarSubst = Map.Map Variable Variable


-- Splits a string into a base and a numerical postfix
splitStr :: String -> (String, Integer)
splitStr s = 
    let (b, p) = span isAlpha s
    in case readMaybe p of
        Just p' -> (b, p')
        Nothing -> (s, 0)

-- Given a string 'x' and a variable substitution set 's',
-- Does a variable that normalizes to 'x' exist in the co-domain of 's'
used :: VarSubst -> String -> Bool
used s x = Map.foldr (\y b -> b || (x == y)) False s
        

-- Given a variable 'v' and a variable substitution set 's',
-- Finds a variant of 'v' that is not in the co-domain of 's'
findAvailible :: Variable -> VarSubst -> String   
findAvailible x s = 
    if not $ used s x 
        then x 
        else findAvailible' $ splitStr x
          findAvailible' (b, p) = 
            if used s (x ++ show p) 
                then findAvailible' (b, p + 1)
                else (x ++ show p)
          
-- Determines if a variable occurs freely in an expression
occurs :: Variable -> Expr -> Bool
occurs v (Var x) = v == x
occurs _ (Universe _) = False
occurs v (Pi (y, t, e)) = occurs v t || (v /= y && occurs v e)
occurs v (Lambda (y, t, e)) = occurs v t || (v /= y && occurs v e)
occurs v (App e1 e2) = occurs v e1 || occurs v e2

beautify :: Expr -> Expr
beautify e = beautify' e Map.empty
    where
        beautify' (Var x) s = case Map.lookup x s of
            Just x' -> Var x'
            Nothing -> Var x
        beautify' (Universe k) _ = Universe k
        beautify' (Pi a) s = Pi $ beautifyAbstraction a s
        beautify' (Lambda a) s = Lambda $ beautifyAbstraction a s
        beautify' (App e1 e2) s = App (beautify' e1 s) (beautify' e2 s)
        beautifyAbstraction (x, t, e) s =
            let t' = beautify' t s
                y = if occurs x e then Sym $ findAvailible x s else Dummy
            in (y, t', beautify' e (Map.insert x y s))

            