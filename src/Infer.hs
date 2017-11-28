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
    | Debug String
    deriving Show

type Infer = ExceptT TypeError Identity

runInfer :: Context -> Term -> Either TypeError Term
runInfer ctx t = runExcept (whnf ctx =<< infer ctx t)

runNormalize :: Context -> Term -> Either TypeError Term
runNormalize ctx t = runExcept (nf ctx t)

-- Wrap context lookup inside a monadic error handler
lookup :: Context -> Index -> Infer Term
lookup ctx i = case lookupType ctx i of
    Just t -> return t
    Nothing -> throwError $ IndexError i

-- | Unifies two types
unify :: Context -> Term -> Term -> Infer Term
unify ctx e1 e2 = do
    e1' <- whnf ctx e1
    e2' <- whnf ctx e2
    case (e1', e2') of
        (Var k1, Var k2) -> 
            if (k1 == k2) 
            then return $ Var k1
            else throwError $ MismatchError e1' e2'
        (Prop, Prop) -> return Prop
        (Prop, Universe k) -> return $ Universe k
        (Universe k, Prop) -> return $ Universe k
        (Universe k1, Universe k2) -> 
            return $ Universe (max k1 k2)
        (Pi a1, Pi a2) -> Pi <$> unifyAbstraction ctx a1 a2
        (Lambda a1, Lambda a2) -> Lambda <$> unifyAbstraction ctx a1 a2
        (App f1 a1, App f2 a2) -> do
            f' <- unify ctx f1 f2 
            a' <- unify ctx a1 a2
            return $ App f' a'
        (_, _) -> throwError $ MismatchError e1' e2'
    where unifyAbstraction ctx (x1, t1, e1) (_, t2, e2) = do
            t' <- unify ctx t1 t2 
            e' <- unify (extendType ctx x1 t1) e1 e2
            return (x1, t', e')

-- | Infers the type of a term
infer :: Context -> Term -> Infer Term
infer ctx t = case t of
    Prop -> return $ Universe 1
    Universe k -> return $ Universe (k + 1)
    Var k -> lookup ctx k
    Subst s e -> infer ctx (subst s e)
    Lambda (x, a, t) -> do
        t' <- infer (extendType ctx x a) t
        return $ Pi (x, a, t')
    Pi (x, a, b) -> do
        a' <- infer ctx a
        b' <- whnf ctx =<< infer (extendType ctx x a) b
        case b' of
            Universe _ -> unify ctx a' b'
            Prop -> return Prop
            _ -> throwError $ UniverseExpected b'
    App f a -> do
        f' <- whnf ctx =<< infer ctx f
        case f' of
            Pi (x, t, e) -> do
                a' <- infer ctx a
                _ <- unify ctx t a'
                return $ Subst (Dot a idShift) e
            t -> throwError $ PiExpected t

-- | Evaluates a term to weak head normal form
whnf :: Context -> Term -> Infer Term
whnf = norm True

-- | Evaluates a term to weak head normal form
nf :: Context -> Term -> Infer Term
nf = norm False

-- | Evaluates a term to normal form
norm :: Bool -> Context -> Term -> Infer Term
norm weak ctx e = case e of
    Var k -> case lookupDefinition ctx k of
        Just e -> norm weak ctx e
        Nothing -> return e
    Universe _ -> return e
    Prop -> return e
    Pi a -> Pi <$> normAbstraction weak ctx a
    Lambda a -> Lambda <$> normAbstraction weak ctx a
    Subst s e -> norm weak ctx (subst s e)
    App e1 e2 -> do
        e1' <- norm weak ctx e1
        case e1' of
            Lambda(_, _, e) -> norm weak ctx (Subst (Dot e2 idShift) e)
            Var _ -> normApp weak ctx e1' e2
            App _ _ -> normApp weak ctx e1' e2
            t -> throwError $ FunctionExpected t
    where 
        normAbstraction weak ctx (x,t,e) = 
            if weak 
            then return (x, t, e)
            else do
                t' <- norm weak ctx t
                e' <- norm weak (extendType ctx x t) e
                return (x, t', e')
        normApp weak ctx e1 e2 = do
            e2' <- if weak then return e2 else norm weak ctx e2
            return $ App e1 e2'
