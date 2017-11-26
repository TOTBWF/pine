module Syntax where

type Variable = String
type Index = Int

data Term
    = Var Index 
    | Subst Substitution Term
    | Prop
    | Universe Int 
    | App Term Term
    | Lambda Variable Term Term -- We Retain variable names for the purpose of pretty-printing
    | Pi Variable Term Term
    deriving Show

data Substitution
    = Shift Index -- 'Shift k' represents adding k to all indicies
    | Dot Term Substitution -- 'Dot e s' represents pushing 't' to the context stack and use 's'
    deriving Show

-- | Creates a term that will shift all indicies by k when evaluated
shift :: Index -> Term -> Term
shift k t = Subst (Shift k) t

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
    (s, Lambda x t e) -> substAbstraction s t e (Lambda x) 
    (s, Pi x t e) -> substAbstraction s t e (Pi x) 
    (s, App e1 e2) -> App (subst s e1) (subst s e2)

-- | Helper function for substituting in a lambda/pi
substAbstraction :: Substitution -> Term -> Term -> (Term -> Term -> Term) -> Term
substAbstraction s t e c = 
    let t' = Subst s t
        e' = Subst (Dot (Var 0) (Shift 1 `compose` s)) e
    in c t' e'

-- | Determines if a variable with index level 'k' is free in term 't'
free :: Index -> Term -> Bool
free k t = case t of
    Var n -> n == k
    Subst s e -> free k (subst s e)
    Prop -> False
    Universe _ -> False
    Pi _ t e -> freeAbstraction k t e
    Lambda _ t e -> freeAbstraction k t e
    App e1 e2 -> free k e1 || free k e2
    where freeAbstraction k t e = free k t || free (k + 1) e