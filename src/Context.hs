module Context where

import Prelude hiding ((!!))

import Syntax

data Declaration
    = Type Term 
    | Definition Term Term -- 'Definition t e' where t is the declared type and e is the defining expression

data Context = Context {
        names :: [Variable],
        decls :: [Declaration]
    }

emptyCtx :: Context
emptyCtx = Context {
        names = [],
        decls = []
    }

-- We redefine !! because the default prelude version is unsafe
(!!) :: (Integral n) => [a] -> n -> Maybe a
(!!) xs i = index xs (toInteger i)
    where
        index [] _ = Nothing 
        index (x:xs) n | n == 0 = Just x
                        | n < 0 = Nothing
                        | otherwise = (!!) xs (n-1)

-- | Looks up the type of an index in a context
lookupType :: Context -> Index -> Maybe Term 
lookupType ctx k = do
        t <- (decls ctx) !! k
        case t of
            Definition t _ -> return $ shift (k + 1) t
            Type t -> return $ shift (k + 1) t

-- | Looks up the type of an index in a context
lookupDefinition :: Context -> Index -> Maybe Term 
lookupDefinition ctx k = do
        t <- (decls ctx) !! k
        case t of
            Definition _ e -> return $ shift (k + 1) e
            Type _ -> Nothing

-- | Looks up the name of an index in a context
lookupName :: Context -> Index -> Maybe Variable 
lookupName ctx k = (names ctx) !! k 

-- | Pushes a term and its name to the context stack
extendType :: Context -> Variable -> Term -> Context
extendType ctx v t = Context {
        names = v:names ctx,
        decls = (Type t):decls ctx
    }

extendDefinition :: Context -> Variable -> Term -> Term -> Context
extendDefinition ctx v t d = Context {
        names = v:names ctx,
        decls = (Definition t d):decls ctx
    }
