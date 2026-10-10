module ExamplesSpec where

import Control.Monad
import Data.List (stripPrefix)
import Parser.Internal
import System.Directory
import System.FilePath
import Test.Hspec
import TipInterpreter

getXfailReason :: String -> Maybe String
getXfailReason = stripPrefix "// XFAIL:" . head . lines

spec :: Spec
spec = do
  let examplesDirectory = "test/examples"
  tipFiles <- runIO $ filter (isExtensionOf "tip") <$> listDirectory examplesDirectory

  describe "can parse and run examples" $ do
    forM_ tipFiles $ \file ->
      it file $ do
        contents <- readFile $ examplesDirectory </> file
        let failReason = getXfailReason contents
        result <- evaluate contents
        case result of
          Right value -> case failReason of
            Nothing -> pure ()
            Just reason -> expectationFailure $ "unexpected pass" ++ reason
          Left err -> case getXfailReason contents of
            Just reason -> pendingWith (reason ++ ": " ++ err)
            Nothing -> expectationFailure err
