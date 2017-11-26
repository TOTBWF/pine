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
-- import Pretty

-- import Control.Monad.Trans
-- import qualified Data.Text.Lazy as L
-- import qualified Data.Text.Lazy.IO as L
-- import qualified Data.Map as Map
-- import Control.Monad.State.Strict

-- import Data.List (isPrefixOf, foldl')

-- import System.Exit
-- import System.Environment
-- import System.Console.Repline

-- data ReplState = ReplState { 
--         rctx :: Context,
--         rdefs :: [(Variable, Binding)]
--     }

-- type Repl a = HaskelineT (StateT ReplState IO) a

-- initState :: ReplState
-- initState = ReplState emptyCtx []

-- hoistErr :: Show e => Either e a -> Repl a
-- hoistErr (Right val) = return val
-- hoistErr (Left err) = do
--   liftIO $ print err
--   abort

-- exec :: L.Text -> Repl ()
-- exec line = do
--     ctx <- get
--     let res = parseTop line
--     case res of
--         Left err -> liftIO $ print err
--         Right tp -> case tp of
--             Parameter x t -> do
--                 _ <- hoistErr $ runInfer $ inferUniverse t (rctx ctx)
--                 let rctx' = extendType x t (rctx ctx)
--                 let ctx' = ctx { rctx = rctx'
--                                , rdefs = (x, Type t):(rdefs ctx)
--                                }
--                 put ctx'
--                 return ()
--             Definition x e -> do
--                 t <- hoistErr $ infer e (rctx ctx)
--                 let rctx' = extendValue x t e (rctx ctx)
--                 let ctx' = ctx {
--                         rctx = rctx',
--                         rdefs = (x, Value t e):(rdefs ctx)
--                     }
--                 put ctx'
--                 return ()

-- -- Commands

-- quit :: a -> Repl ()
-- quit _ = liftIO $ exitSuccess

-- help :: a -> Repl ()
-- help _ = liftIO $ do 
--     putStrLn "Top Level Commands:"
--     putStrLn ":quit                         Quits"
--     putStrLn ":help                         Prints this message"
--     putStrLn ":context                      Prints the Current Set Of Parameters and Definitions"
--     putStrLn ":dump <filename>              Dumps the current set of definitions to the specified file"
--     putStrLn ":load <filename>              Loads a set of definitions from a file"
--     putStrLn "<var> : <expr>                Declares a variable <var> to be of type <expr>"
--     putStrLn "<var> := <expr>               Defines a variable <var> to be <expr>"
--     putStrLn ":type <expr>                 Checks the type of an expression"
--     putStrLn ":eval <expr>                  Evaluates an <expr>"

-- context :: a -> Repl ()
-- context _ = do
--     ctx <- gets rdefs
--     liftIO $ mapM_ putStrLn $ ppEnv $ reverse ctx

-- typeof :: [String] -> Repl ()
-- typeof args = do
--     ctx <- gets rctx
--     expr <- hoistErr $ parseExpr $ L.pack $ unwords args
--     t <- hoistErr $ infer expr ctx
--     liftIO $ putStrLn $ ppExpr t

-- eval :: [String] -> Repl ()
-- eval args = do
--     ctx <- gets rctx
--     expr <- hoistErr $ parseExpr $ L.pack $ unwords args
--     t <- hoistErr $ infer expr ctx
--     e <- hoistErr $ runInfer $ normalize expr ctx
--     liftIO $ putStrLn ("    = " ++ ppExpr e ++ "\n    : " ++ ppExpr t)

-- load :: [String] -> Repl ()
-- load args = do
--     contents <- liftIO $ L.readFile (unwords args)
--     mapM_ exec $ L.lines contents
--     return ()

-- dump :: [String] -> Repl ()
-- dump args = do
--     defs <- gets rdefs
--     let path = head args
--     liftIO $ writeFile path $ unlines $ fmap writeBinding $ reverse defs
--     where writeBinding (x, (Type t)) = ppVariable x ++ " : " ++ ppExpr t
--           writeBinding (x, (Value _ e)) = ppVariable x ++ " := " ++ ppExpr e
--     -- L.writeFile

-- cmd :: [(String, [String] -> Repl ())]
-- cmd = [
--     ("quit", quit),
--     ("help", help),
--     ("context", context),
--     ("type", typeof),
--     ("eval", eval),
--     ("load", load),
--     ("dump", dump)
--   ]

-- -- Tab Completion
-- defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
-- defaultMatcher = [
--     (":load"  , fileCompleter),
--     (":dump"  , fileCompleter)
--   ]

-- comp :: (Monad m, MonadState ReplState m) => WordCompleter m
-- comp n = do
--     let cmds = [":quit", ":help", ":context", ":type", ":eval", ":load", ":dump", "Type"]
--     ctx <- gets rdefs
--     let defs = fmap (ppVariable . fst) ctx 
--     return $ filter (isPrefixOf n) (cmds ++ defs)

-- completer :: CompleterStyle (StateT ReplState IO)
-- completer = Prefix (wordCompleter comp) defaultMatcher

-- main :: IO ()
-- main = 
--     flip evalStateT initState
--     $ evalRepl "λπ> " (exec . L.pack) cmd completer (return ())

main :: IO ()
main = putStrLn "TODO"