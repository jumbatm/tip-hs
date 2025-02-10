module Main (main) where

import Data.Maybe
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  let filename = case args of
        [x] -> Just x
        _ -> Nothing
  if isNothing filename
    then do
      putStrLn "Usage: tip <filename>"
      exitFailure
    else do
      contents <- readFile $ fromJust filename
      case parse contents of
        Just ast -> putStrLn $ show ast
        Nothing -> do
          putStrLn "Parsing failure!"
          exitFailure
  return ()
