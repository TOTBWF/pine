module Pretty where

import Syntax
import Infer
import Context
import Beautify

import Text.PrettyPrint
import qualified Data.Map as Map

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

compressLambdas :: Int -> Context -> Term -> Doc
compressLambdas p ctx (Lambda (x, t, e)) = parens (text x <+> colon <+> ppr p ctx t) <+> compressLambdas p ctx e
compressLambdas p ctx e = text "=>" <+> ppr p ctx e

class Pretty p where
    ppr :: Int -> Context -> p -> Doc

instance Pretty Term where
    ppr p ctx (Var x) = case lookupName ctx x of
        Just n -> text n
        Nothing -> text "_"
    ppr _ ctx (Universe k) = text "Type" <+> integer (toInteger k)
    ppr _ ctx (Prop) = text "Prop"
    ppr p ctx (Pi ("_", t1, t2)) = parensIf (p > 0) $ ppr (p + 1) ctx t1 <+> text "->" <+> ppr (p + 1) ctx t2 
    ppr p ctx (Pi (x, t1, t2)) = text "forall" <+> text x <+> colon <+> ppr p ctx t1 <> comma <+> ppr p ctx t2
    ppr p ctx (Lambda a) = text "fun" <+> compressLambdas p ctx (Lambda a)
    ppr p ctx (App e1 e2) = parensIf (p > 0) $ ppr p ctx e1 <+> ppr (p + 1) ctx e2
    ppr p ctx (Subst (Shift k) e) = brackets (integer $ toInteger k) <+> ppr p ctx e
    ppr p ctx (Subst (Dot e1 s) e2) = brackets (ppr p ctx e1 <> text "/") <+> ppr p ctx e2

ppTerm :: Context -> Term -> String
ppTerm ctx = render . ppr 0 ctx

ppBinding :: Context -> (Variable, Declaration) -> String
ppBinding ctx (x, (Type t)) = show x ++ " : " ++ ppTerm ctx t
ppBinding ctx (x, (Definition t v)) = show x ++ " = " ++ ppTerm ctx v ++ "\n    : " ++ ppTerm ctx t

-- ppEnv :: [(Variable, Declaration)] -> [String]
-- ppEnv ctx = fmap ppBinding ctx

