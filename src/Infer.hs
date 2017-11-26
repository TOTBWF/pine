module Infer where

import Prelude hiding (lookup)
import Control.Monad.Except
import Control.Monad.Identity

import Syntax
import Context


data TypeError 
    = IndexError Index
    | UniverseExpected Term
    | PiExpected Term
    | FunctionExpected Term
    | MismatchError Term Term 
    deriving Show
type Infer = ExceptT TypeError Identity

runInfer :: Context -> Term -> Either TypeError Term
runInfer ctx t = runExcept (infer ctx t)

-- Wrap context lookup inside a monadic error handler
lookup :: Context -> Index -> Infer Term
lookup ctx i = case lookupType ctx i of
    Just t -> return t
    Nothing -> throwError $ IndexError i

-- | Throws a unification error if the types do not unify
equal :: Context -> Term -> Term -> Bool
equal ctx e1 e2 = do
    case (e1, e2) of
        (Var k1, Var k2) -> (k1 == k2)
        (Prop, Prop) -> True
        (Universe k1, Universe k2) -> (k1 == k2)
        (Pi a1, Pi a2) -> equalAbstraction ctx a1 a2
        (Lambda a1, Lambda a2) -> equalAbstraction ctx a1 a2
        (App f1 a1, App f2 a2) -> equal ctx f1 f2 && equal ctx a1 a2
        (_, _) -> False
    where equalAbstraction ctx (x1, t1, e1) (_, t2, e2) = 
            equal ctx t1 t2 && equal (extendType ctx x1 t1) e1 e2

-- | Infers the type of a term
infer :: Context -> Term -> Infer Term
infer ctx t = case t of
    Prop -> return $ Universe 1
    Universe k -> return $ Universe (k + 1)
    Var k -> lookup ctx k
    Lambda (x, a, t) -> do
        t' <- infer (extendType ctx x a) t
        return $ Pi (x, a, t')
    Pi (x, a, b) -> do
        b' <- infer (extendType ctx x a) b
        case b' of
            Universe k -> do
                a' <- whnf ctx =<< infer ctx a
                b'' <- whnf ctx b'
                if equal ctx a' b''
                    then return $ Universe k
                    else throwError $ MismatchError a' b''
            Prop -> return Prop
            t -> throwError $ UniverseExpected t
    App f a -> do
        f' <- infer ctx f
        case f' of
            Pi (x, t, e) -> do
                a' <- whnf ctx =<< infer ctx a
                t' <- whnf ctx =<< infer ctx t
                if equal ctx a' t' 
                    then return $ Subst (Dot a idShift) e
                    else throwError $ MismatchError a' t'
            t -> throwError $ PiExpected t

-- | Evaluates a term to weak head normal form
whnf :: Context -> Term -> Infer Term
whnf ctx e = case e of
    Var k -> case lookupDefinition ctx k of
        Just e -> whnf ctx e
        Nothing -> return e
    Universe _ -> return e
    Prop -> return e
    Pi a -> return $ Pi a
    Lambda a -> return $ Lambda a
    Subst s e -> whnf ctx (subst s e)
    App e1 e2 -> do
        e1' <- whnf ctx e1
        case e1' of
            Lambda(_, _, e) -> whnf ctx (Subst (Dot e2 idShift) e)
            Var _ -> return $ App e1 e2
            App _ _ -> return $ App e1 e2
            t -> throwError $ FunctionExpected t

-- | Evaluates a term to normal form
nf :: Context -> Term -> Infer Term
nf ctx e = case e of
    Var k -> case lookupDefinition ctx k of
        Just e -> whnf ctx e
        Nothing -> return e
    Universe _ -> return e
    Prop -> return e
    Pi a -> return $ Pi a
    Lambda a -> return $ Lambda a
    Subst s e -> whnf ctx (subst s e)
    App e1 e2 -> do
        e1' <- whnf ctx e1
        case e1' of
            Lambda(_, _, e) -> whnf ctx (Subst (Dot e2 idShift) e)
            Var _ -> return $ App e1 e2
            App _ _ -> return $ App e1 e2
            t -> throwError $ FunctionExpected t