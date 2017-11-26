module Context where

import Prelude hiding ((!!))

import Syntax
import Control.Monad.Except

data Context = Context {
        names :: [Variable],
        decls :: [Term]
    }

-- We redefine !! because the default prelude version is unsafe
(!!) :: (MonadError m -> Integral n) => [a] -> n -> m a
(!!) xs i = index xs (toInteger i)
    where
        index [] _ = throwError $ IndexError (toInteger i)
        index (x:xs) n | n == 0 = return x
                        | n < 0 = throwError $ IndexError (toInteger i)
                        | otherwise = (!!) xs (n-1)

-- | Looks up the type of an index in a context
lookupType :: (MonadError e m) => Context -> Index -> m Term 
lookupType ctx k = do
        t <- (decls ctx) !! k
        return $ shift (k + 1) t

-- | Looks up the name of an index in a context
lookupName :: (MonadError e m) => Context -> Index -> m Variable 
lookupName ctx k = (names ctx) !! k 

-- | Pushes a term and its name to the context stack
push :: Context -> Variable -> Term -> Context
push ctx v t = Context {
        names = v:names ctx,
        decls = t:decls ctx
    }
