module Pretty where

import Syntax
import Infer
import Beautify

import Text.PrettyPrint
import qualified Data.Map as Map

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Variable where
    ppr _ (Sym s) = text s
    ppr _ (GenSym s i) = text $ s ++ show i
    ppr _ (Dummy) = text "_"

instance Pretty Expr where
    ppr p (Var x) = ppr p x
    ppr _ (Universe k) = text "Type" <+> integer k
    ppr p (Pi (Dummy, t1, t2)) = parensIf (p > 0) $ ppr (p + 1) t1 <+> text "->" <+> ppr p t2 
    ppr p (Pi (x, t1, t2)) = text "forall" <+> ppr p x <+> colon <+> ppr p t1 <> comma <+> ppr p t2
    ppr p (Lambda (x, t, e)) = text "fun" <+> ppr p x <+> colon <+> ppr p t <+> text "=>" <+> ppr p e
    ppr p (App e1 e2) = parensIf (p > 0) $ ppr p e1 <+> ppr (p + 1) e2

ppExpr :: Expr -> String
ppExpr = render . ppr 0 . beautify

ppVariable :: Variable -> String
ppVariable = render . ppr 0

ppBinding :: (Variable, Binding) -> String
ppBinding (x, (Type t)) = ppVariable x ++ " : " ++ ppExpr t
ppBinding (x, (Value t v)) = ppVariable x ++ " = " ++ ppExpr v ++ "\n    : " ++ ppExpr t

ppEnv :: Context -> [String]
ppEnv ctx = fmap ppBinding $ Map.toList ctx

