module ExamplesSpec where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List (stripPrefix)
import Parser.Internal
import System.Directory
import System.FilePath
import Test.Hspec
import TipInterpreter

data TestSpec = TestSpec (Maybe String) [String]

instance Semigroup TestSpec where
  (TestSpec lx li) <> (TestSpec rx ri) = TestSpec (rx <|> lx) (li ++ ri)

instance Monoid TestSpec where
  mempty = TestSpec Nothing []

parseTestSpec :: String -> TestSpec
parseTestSpec fileContents = foldMap parseLine (lines fileContents)
  where
    parseLine :: String -> TestSpec
    parseLine line
      | Just f <- stripPrefix "// XFAIL:" line = TestSpec (Just f) []
      | Just i <- stripPrefix "// INPUT:" line = TestSpec Nothing (words i)
      | otherwise = mempty

-- Very, very cool.
mockInputs :: [String] -> IO (IO String)
mockInputs xs = do
  stored <- newIORef xs
  pure $ do
    current <- readIORef stored
    case current of
      (curr : next) -> do
        writeIORef stored next
        pure curr
      [] -> pure ""

spec :: Spec
spec = do
  let examplesDirectory = "test/examples"
  tipFiles <- runIO $ filter (isExtensionOf "tip") <$> listDirectory examplesDirectory

  describe "can parse and run examples" $ do
    forM_ tipFiles $ \file ->
      it file $ do
        contents <- readFile $ examplesDirectory </> file
        let (TestSpec failReason inputs) = parseTestSpec contents
        input <- mockInputs inputs
        result <- evaluateWithInput contents input
        case result of
          Right value -> case failReason of
            Nothing -> pure ()
            Just reason -> expectationFailure $ "unexpected pass" ++ reason
          Left err -> case failReason of
            Just reason -> pendingWith (reason ++ ": " ++ err)
            Nothing -> expectationFailure err
