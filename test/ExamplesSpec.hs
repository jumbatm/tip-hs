module ExamplesSpec where

import Control.Monad
import Data.List (stripPrefix)
import Parser.Internal
import Parser.TipParser
import System.Directory
import System.FilePath
import Test.Hspec

getXfailReason :: String -> Maybe String
getXfailReason = stripPrefix "// XFAIL:" . head . lines

spec :: Spec
spec = do
  let examplesDirectory = "test/examples"
  tipFiles <- runIO $ filter (isExtensionOf "tip") <$> listDirectory examplesDirectory

  describe "can parse examples" $ do
    forM_ tipFiles $ \file ->
      it file $ do
        contents <- readFile $ examplesDirectory </> file
        case parse contents of
          ParseOk _ast -> pure ()
          ParseError _ loc msg ->
            let message = show loc ++ ": expected " ++ show msg
             in case getXfailReason contents of
                  Just reason -> pendingWith (reason ++ ": " ++ message)
                  Nothing -> expectationFailure message
