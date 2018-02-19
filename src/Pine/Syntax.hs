module Pine.Syntax where

import Data.Text (Text)

type Variable = Text
type Index = Int

type Abstraction = (Variable, Term, Term)-- We Retain variable names for the purpose of pretty-printing

data Term
    = Var Index 
    | Subst Substitution Term
    | Prop
    | Universe Int 
    | App Term Term
    | Lambda Abstraction
    | Pi Abstraction
    deriving Show

data Substitution
    = Shift Index -- 'Shift k' represents adding k to all indicies
    | Dot Term Substitution -- 'Dot e s' represents pushing 'e' to the context stack and use 's'
    deriving Show

-- | Creates a term that will shift all indicies by k when evaluated
shift :: Index -> Term -> Term
shift k t = Subst (Shift k) t

idShift :: Substitution
idShift = Shift 0

-- | Composes 2 substitutions
compose :: Substitution -> Substitution -> Substitution
compose s (Shift 0) = s
compose (Shift n) (Shift m) = Shift (n + m)
compose (Dot _ s) (Shift n) = compose s $ Shift (n - 1)
compose s (Dot e t)  = Dot (Subst s e) (compose s t)


-- | Applies a substitution to a term
subst :: Substitution -> Term -> Term
subst s t = case (s,t) of
    (Shift n, Var k) -> Var (n + k)
    (Dot a _, Var 0) -> a
    (Dot _ s, Var k) -> subst s $ Var (k - 1)
    (s, Subst t e) -> subst s $ subst t e
    (_, Prop) -> Prop
    (_, Universe k) -> Universe k
    (s, Lambda a) -> Lambda $ substAbstraction s a 
    (s, Pi a) -> Pi $ substAbstraction s a
    (s, App e1 e2) -> App (subst s e1) (subst s e2)

-- | Helper function for substituting in a lambda/pi
substAbstraction :: Substitution -> Abstraction -> Abstraction
substAbstraction s (x, t, e) = 
    let t' = Subst s t
        e' = Subst (Dot (Var 0) (Shift 1 `compose` s)) e
    in (x, t', e')

-- | Determines if a variable with index level 'k' is free in term 't'
free :: Index -> Term -> Bool
free k t = case t of
    Var n -> n == k
    Subst s e -> free k (subst s e)
    Prop -> False
    Universe _ -> False
    Pi a -> freeAbstraction k a
    Lambda a -> freeAbstraction k a
    App e1 e2 -> free k e1 || free k e2
    where freeAbstraction k (_, t, e) = free k t || free (k + 1) e