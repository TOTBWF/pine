module Pine.Pretty where

import Pine.Syntax
import Pine.Infer
import Pine.Context
import Pine.Beautify

import Text.PrettyPrint
import qualified Data.Text as T
import qualified Data.Map as Map

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

compressLambdas :: Int -> Context -> Term -> Doc
compressLambdas p ctx (Lambda (x, t, e)) = parens (text (T.unpack x) <+> typedef <+> ppr p ctx t) <+> compressLambdas p ctx e
compressLambdas p ctx e = text "=>" <+> ppr p ctx e

typedef :: Doc 
typedef = colon <> colon

class Pretty p where
    ppr :: Int -> Context -> p -> Doc

instance Pretty Term where
    ppr p ctx (Var x) = case lookupName ctx x of
        Just n -> text $ T.unpack n
        Nothing -> text "_"
    ppr _ ctx (Universe k) = text "Type" <+> integer (toInteger k)
    ppr _ ctx (Prop) = text "Prop"
    ppr p ctx (Pi ("_", t1, t2)) = parensIf (p > 0) $ ppr (p + 1) ctx t1 <+> text "->" <+> ppr (p + 1) (extendName ctx "_") t2 
    ppr p ctx (Pi (x, t1, t2)) = text "∀" <> text (T.unpack x) <+> typedef <+> ppr p ctx t1 <> comma <+> ppr p (extendName ctx x) t2
    ppr p ctx (Lambda a) = text "fun" <+> compressLambdas p ctx (Lambda a)
    ppr p ctx (App e1 e2) = parensIf (p > 0) $ ppr p ctx e1 <+> ppr (p + 1) ctx e2
    ppr p ctx (Subst (Shift k) e) = text "SUBST" <+> brackets (integer $ toInteger k) <+> ppr p ctx e
    ppr p ctx (Subst (Dot e1 s) e2) = text "SUBST" <+> brackets (ppr p ctx e1 <> text "/") <+> ppr p ctx e2

ppTerm :: Context -> Term -> String
ppTerm ctx = render . ppr 0 ctx

ppBinding :: Context -> (Variable, Declaration) -> String
ppBinding ctx (x, (Type t)) = show x ++ " : " ++ ppTerm ctx t
ppBinding ctx (x, (Definition t v)) = show x ++ " = " ++ ppTerm ctx v ++ "\n    : " ++ ppTerm ctx t

-- ppEnv :: [(Variable, Declaration)] -> [String]
-- ppEnv ctx = fmap ppBinding ctx

