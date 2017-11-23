module Main where

import Parser

import Control.Monad.Trans
import System.Console.Haskeline
import qualified Data.Text.Lazy as L


process :: L.Text -> IO ()
process line = do
    let res = parseDirective line
    case res of
        Left err -> print err
        Right d -> print $ show d

main :: IO ()
main = runInputT defaultSettings loop
    where 
    loop = do
        minput <- getInputLine "Pine> "
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> (liftIO $ process $ L.pack input) >> loop

