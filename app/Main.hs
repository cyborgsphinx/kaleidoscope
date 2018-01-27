module Main where

import Parser

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = case parseToplevel line of
                 Left err -> print err
                 Right ex -> mapM_ print ex

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop = do
            minput <- getInputLine "ε> "
            case minput of
              Nothing -> outputStrLn "Done."
              Just input -> liftIO (process input) >> loop
