module Pine.Beautify (refresh) where

import Data.Map as Map
import Data.Char
import Text.Read
import Data.Text as T

import Pine.Syntax

-- Splits a string into a base and a numerical postfix
splitStr :: Text -> (Text, Integer)
splitStr s = 
    let (b, p) = T.span isAlpha s
    in case readMaybe $ T.unpack p of
        Just p' -> (b, p')
        Nothing -> (s, 0)

refresh :: [Variable] -> Variable -> Variable
refresh ctx x =
    if notElem x ctx
        then x 
        else findAvailible' $ splitStr x
    where
          findAvailible' (b, p) = 
            if elem (x `T.append` (T.pack $ show p)) ctx
                then findAvailible' (b, p + 1)
                else (x `T.append` (T.pack $ show p))


            