module Infer where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map

import Syntax

type Subst = Map.Map Variable Expr

data Unique = Unique { count :: Integer }

data TypeError
    = UnknownIdentifier Variable
    | TypeExpected
    | FunctionExpected
    | EqualityError Expr Expr
    deriving Show

type Infer a = ExceptT TypeError (State Unique) a

data Binding 
    = Type Expr
    | Value Expr Expr
    deriving Show

type Context = Map.Map Variable Binding

runInfer :: Infer a -> Either TypeError a
runInfer m = case evalState (runExceptT m) initUnique of
    Left err  -> Left err
    Right res -> Right res

initUnique :: Unique
initUnique = Unique { count = 0 }

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

-- Creates a singleton substitution
singletonSubst :: Variable -> Expr -> Subst
singletonSubst = Map.singleton

-- Creates a singleton substitution
emptySubst :: Subst
emptySubst = Map.empty

-- Performs a substitution in 'e'
subst :: Subst -> Expr -> Infer Expr
subst s e = case e of
    Var x -> do
        case Map.lookup x s of
            Just e -> return e
            Nothing -> return $ Var x
    Universe k -> return $ Universe k
    Pi a -> do
        a' <- substAbstraction s a
        return $ Pi a'
    Lambda a -> do
        a' <- substAbstraction s a
        return $ Lambda a'
    App e1 e2 -> do
        e1' <- subst s e1
        e2' <- subst s e2
        return $ App e1' e2'

-- Performs a substitution in an abstraction
substAbstraction :: Subst -> Abstraction -> Infer Abstraction
substAbstraction s (x, t, e) = do
    x' <- fresh x 
    t' <- subst s t
    e' <- subst (Map.insert x (Var x') s) e
    return (x', t', e')


-- Returns an empty context
emptyCtx :: Context
emptyCtx = Map.empty

-- Returns the type of 'x' in the Context 'ctx'
lookupType :: Variable -> Context -> Infer Expr
lookupType x ctx = case Map.lookup x ctx of
    Just(Type(t)) -> return t
    Just(Value t _) -> return t
    Nothing -> throwError $ UnknownIdentifier x

-- Returns the value of 'x' in the Context 'ctx' if it is a value
lookupValue :: Variable -> Context -> Infer (Maybe Expr)
lookupValue x ctx = case Map.lookup x ctx of
    Just(Type(_)) -> return Nothing
    Just(Value _ v) -> return $ Just v
    Nothing -> throwError $ UnknownIdentifier x
    

-- Extends 'ctx' with a a variable bound to a type
extendType :: Variable -> Expr -> Context -> Context
extendType x t ctx = Map.insert x (Type t) ctx

-- Extends 'ctx' with a a variable bound to a value e of type t
extendValue :: Variable -> Expr -> Expr -> Context -> Context
extendValue x t e ctx = Map.insert x (Value t e) ctx

infer :: Expr -> Context -> Either TypeError Expr
infer e ctx = runInfer $ inferType e ctx

inferType :: Expr -> Context -> Infer Expr
inferType e ctx = case e of
    -- The type of a variable can be looked up in the context
    Var v -> lookupType v ctx
    -- The type of Type_k is Type_{k+1}
    Universe k -> return $ Universe (k + 1)
    -- The type of Pi_{x:T1} T2
    Pi (x, t1, t2) -> do
        k1 <- inferUniverse t1 ctx
        k2 <- inferUniverse t2 (extendType x t1 ctx)
        return $ Universe (max k1 k2)
    Lambda (x, t, e) -> do
        _ <- inferUniverse t ctx
        te <- inferType e (extendType x t ctx)
        return $ Pi (x, t, te)
    App e1 e2 -> do
        (x, s, t) <- inferPi e1 ctx
        te <- inferType e2 ctx
        eq <- equal s te ctx
        if eq
            then subst (singletonSubst x e2) t
            else throwError $ EqualityError e1 e2
       
-- Infers the universe level of a type
inferUniverse :: Expr -> Context -> Infer Integer
inferUniverse t ctx = do
    u <- inferType t ctx
    u' <- normalize u ctx
    case u' of
        Universe k -> return k
        _ -> throwError FunctionExpected

inferPi :: Expr -> Context -> Infer Abstraction
inferPi e ctx = do
    t <- inferType e ctx
    t' <- normalize t ctx
    case t' of
        Pi a -> return a
        _ -> throwError FunctionExpected

-- Reduces down the expressions as far as they can go
normalize :: Expr -> Context -> Infer Expr
normalize e ctx = case e of
    Var v -> do
        val <- lookupValue v ctx
        case val of
            Nothing -> return $ Var v
            Just val' -> normalize val' ctx
    Universe k -> return $ Universe k
    Pi a -> do
        a' <- normalizeAbstraction a ctx
        return $ Pi a'
    Lambda a -> do
        a' <- normalizeAbstraction a ctx
        return $ Lambda a'
    App e1 e2 -> do
        e1' <- normalize e1 ctx
        e2' <- normalize e2 ctx
        case e1' of
            Lambda (x, _, b) -> do
                b' <- subst (singleton x e2') b
                normalize b' ctx
            e1' -> return $ App e1' e2'

normalizeAbstraction :: Abstraction -> Context -> Infer Abstraction
normalizeAbstraction (x, t, e) ctx = do
    t' <- normalize t ctx
    e' <- normalize e (extendType x t ctx)
    return (x, t', e')

-- Determines if 2 expressions, when normalized, are equal up to bound variables
equal :: Expr -> Expr -> Context -> Infer Bool
equal e1 e2 ctx = do 
    e1' <- normalize e1 ctx
    e2' <- normalize e1 ctx
    equal' e1' e2'
    where
        equal' (Var x1) (Var x2) = return $ x1 == x2
        equal' (Universe k1) (Universe k2) = return $ k1 == k2
        equal' (Pi a1) (Pi a2) = equalAbstraction a1 a2
        equal' (Lambda a1) (Lambda a2) = equalAbstraction a1 a2
        equal' (App e11 e12) (App e21 e22) = do
            eq1 <- equal' e11 e21 
            eq2 <- equal' e12 e22
            return $ eq1 && eq2
        equal' _ _ = return False
        equalAbstraction (x1, t1, e1) (x2, t2, e2) = do
            eq1 <- equal' t1 t2 
            e2' <- subst (singletonSubst x2 (Var x1)) e2
            eq2 <- equal' e1 e2'
            return $ eq1 && eq2