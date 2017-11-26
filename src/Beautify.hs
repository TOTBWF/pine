module Beautify (refresh) where

import Data.Map as Map
import Data.Char
import Text.Read

import Syntax




-- type VarSubst = Map.Map Variable Variable


-- Splits a string into a base and a numerical postfix
splitStr :: String -> (String, Integer)
splitStr s = 
    let (b, p) = span isAlpha s
    in case readMaybe p of
        Just p' -> (b, p')
        Nothing -> (s, 0)

refresh :: [Variable] -> Variable -> Variable
refresh ctx x =
    if notElem x ctx
        then x 
        else findAvailible' $ splitStr x
    where
          findAvailible' (b, p) = 
            if elem (x ++ show p) ctx
                then findAvailible' (b, p + 1)
                else (x ++ show p)


            