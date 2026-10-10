module Main (main) where

import Data.Maybe
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)
import TipInterpreter

main :: IO ()
main = do
  args <- getArgs
  let filearg = case args of
        [x] -> Just x
        _ -> Nothing
  if isNothing filearg
    then do
      putStrLn "Usage: tip <filename>"
      exitFailure
    else do
      let file = fromJust filearg
      contents <- readFile file
      result <- evaluate contents
      putStrLn $ case result of
        Right v -> show v
        Left err -> err
  return ()
