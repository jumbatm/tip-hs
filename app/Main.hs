module Main (main) where

import Data.Maybe
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)

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
      case parse contents of
        ParseOk ast -> putStrLn $ show ast
        ParseError (SourceLocation (line, col)) msg -> do
          putStrLn $ "Parsing failure at " ++ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ unlines msg
          exitFailure
  return ()
