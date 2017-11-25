{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Parser
import Syntax
import Infer
import Pretty

import Control.Monad.Trans
import qualified Data.Text.Lazy as L
import qualified Data.Map as Map
import Control.Monad.State.Strict

import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Environment
import System.Console.Repline

type Repl a = HaskelineT (StateT Context IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

exec :: String -> Repl ()
exec line = do
    ctx <- get
    let res = parseTop $ L.pack line
    case res of
        Left err -> liftIO $ print err
        Right tp -> case tp of
            Parameter x t -> do
                _ <- hoistErr $ runInfer $ inferUniverse t ctx
                let ctx' = extendType x t ctx
                put ctx'
                return ()
            Definition x e -> do
                t <- hoistErr $ infer e ctx
                let ctx' = extendValue x t e ctx
                put ctx'
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
    putStrLn "<var> : <expr>                Declares a variable <var> to be of type <expr>"
    putStrLn "<var> := <expr>               Defines a variable <var> to be <expr>"
    putStrLn ":type <expr>                 Checks the type of an expression"
    putStrLn ":eval <expr>                  Evaluates an <expr>"

context :: a -> Repl ()
context _ = do
    ctx <- get
    liftIO $ mapM_ putStrLn $ ppEnv ctx

typeof :: [String] -> Repl ()
typeof args = do
    ctx <- get
    expr <- hoistErr $ parseExpr $ L.pack $ unwords args
    t <- hoistErr $ infer expr ctx
    liftIO $ putStrLn $ ppExpr t

eval :: [String] -> Repl ()
eval args = do
    ctx <- get
    expr <- hoistErr $ parseExpr $ L.pack $ unwords args
    t <- hoistErr $ infer expr ctx
    e <- hoistErr $ runInfer $ normalize expr ctx
    liftIO $ putStrLn ("    = " ++ ppExpr e ++ "\n    : " ++ ppExpr t)

cmd :: [(String, [String] -> Repl ())]
cmd = [
    ("quit", quit),
    ("help", help),
    ("context", context),
    ("type", typeof),
    ("eval", eval)
  ]

-- Tab Completion
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = []

comp :: (Monad m, MonadState Context m) => WordCompleter m
comp n = do
    let cmds = [":quit", ":help", ":context", ":type", ":eval", "Type"]
    ctx <- get
    let defs = fmap ppVariable $ Map.keys ctx
    return $ filter (isPrefixOf n) (cmds ++ defs)

completer :: CompleterStyle (StateT Context IO)
completer = Prefix (wordCompleter comp) defaultMatcher

main :: IO ()
main = 
    flip evalStateT emptyCtx
    $ evalRepl "λπ> " exec cmd completer (return ())

