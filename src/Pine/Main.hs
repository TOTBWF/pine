{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Pine.Parser.Parser
import Pine.Syntax
import Pine.AbsSyntax
import Pine.Infer
import Pine.Context
import Pine.Pretty

import Control.Monad.Trans
import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Control.Monad.Except

import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Environment
import System.Console.Repline

-- data ReplState = ReplState { 
--         rctx :: Context
--     }

type Repl a = HaskelineT (StateT Context IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

exec :: Text -> Repl ()
exec line = do
    ctx <- get
    let res = parseTop line
    case res of
        Left err -> liftIO $ putStrLn err
        Right tp -> case tp of
            AbsParameter x i -> do
                -- _ <- hoistErr $ runInfer $ inferUniverse t (rctx ctx)
                t <- hoistErr $ desugar ctx i
                let ctx' = extendType (ctx) x t
                put ctx'
                return ()
            AbsDefinition x i -> do
                e <- hoistErr $ desugar ctx i
                t <- hoistErr $ runInfer ctx e 
                let ctx' = extendDefinition ctx x t e 
                put ctx'
                return ()
            AbsInductive x i c -> do
                e <- hoistErr $ desugar ctx i
                let ctx' = extendType ctx x e
                ctx'' <- foldM (\ctx (x', i') -> do 
                        e' <- hoistErr $ desugar ctx i'
                        return $ extendType ctx x' e') ctx' c
                put ctx''
                return ()


-- Commands

quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

help :: a -> Repl ()
help _ = liftIO $ do 
    putStrLn "Top Level Commands:"
    putStrLn ":quit                         Quits"
    putStrLn ":help                         Prints this message"
    putStrLn ":context                      Prints the Current Set Of Parameters and Definitions"
    putStrLn ":dump <filename>              Dumps the current set of definitions to the specified file"
    putStrLn ":load <filename>              Loads a set of definitions from a file"
    putStrLn "<var> : <expr>                Declares a variable <var> to be of type <expr>"
    putStrLn "<var> := <expr>               Defines a variable <var> to be <expr>"
    putStrLn ":type <expr>                 Checks the type of an expression"
    putStrLn ":eval <expr>                  Evaluates an <expr>"

context :: a -> Repl ()
context _ = do
    ctx <- get 
    liftIO $ putStrLn $ show ctx

typeof :: [String] -> Repl ()
typeof args = do
    ctx <- get 
    i <- hoistErr $ parseTerm $ T.pack $ unwords args
    term <- hoistErr $ desugar ctx i
    t <- hoistErr $ runNormalize ctx =<< runInfer ctx term
    liftIO $ putStrLn $ ppTerm ctx t

debug :: [String] -> Repl ()
debug args = do
    ctx <- get 
    i <- hoistErr $ parseTerm $ T.pack $ unwords args
    liftIO $ putStrLn $ "Input: " ++ show i
    term <- hoistErr $ desugar ctx i
    liftIO $ putStrLn $ "Desugared: " ++ show term
    liftIO $ putStrLn $ "Names: " ++ show (names ctx)
    t <- hoistErr $ runInfer ctx term
    liftIO $ putStrLn $ "Desugard Type: " ++ show t


eval :: [String] -> Repl ()
eval args = do
    ctx <- get
    i <- hoistErr $ parseTerm $ T.pack $ unwords args
    term <- hoistErr $ desugar ctx i
    t <- hoistErr $ runInfer ctx term
    e <- hoistErr $ runNormalize ctx term
    liftIO $ putStrLn ("    = " ++ ppTerm ctx e ++ "\n    : " ++ ppTerm ctx t)

load :: [String] -> Repl ()
load args = do
    contents <- liftIO $ T.readFile (unwords args)
    mapM_ exec $ T.lines contents
    return ()

dump :: [String] -> Repl ()
dump args = do
    ctx <- get
    let defs = reverse $ zip (names ctx) (decls ctx)  
    let path = head args
    liftIO $ writeFile path $ unlines $ fmap (writeDecl ctx) defs
    where writeDecl ctx (x, (Type t)) = (T.unpack x) ++ " : " ++ ppTerm ctx t
          writeDecl ctx (x, (Definition _ e)) = (T.unpack x) ++ " := " ++ ppTerm ctx e

cmd :: [(String, [String] -> Repl ())]
cmd = [
    ("quit", quit),
    ("help", help),
    ("context", context),
    ("type", typeof),
    ("debug", debug),
    ("eval", eval),
    -- ("load", load),
    ("dump", dump)
  ]

-- Tab Completion
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter),
    (":dump"  , fileCompleter)
  ]

comp :: (Monad m, MonadState Context m) => WordCompleter m
comp n = do
    let cmds = ((':':) . fst)<$> cmd
    defs <- gets names
    return $ filter (isPrefixOf n) (cmds ++ (T.unpack <$> defs))

completer :: CompleterStyle (StateT Context IO)
completer = Prefix (wordCompleter comp) defaultMatcher

main :: IO ()
main = 
    flip evalStateT emptyCtx
    $ evalRepl "λπ> " (exec . T.pack) cmd completer (return ())
