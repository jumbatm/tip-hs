module ExamplesSpec where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List (stripPrefix)
import Parser.Internal
import Parser.TipParser
import System.Directory
import System.FilePath
import Test.Hspec
import TipInterpreter

data TestSpec = TestSpec (Maybe String) [String] Bool

instance Semigroup TestSpec where
  (TestSpec lx li lr) <> (TestSpec rx ri rr) = TestSpec (rx <|> lx) (li ++ ri) (lr || rr)

instance Monoid TestSpec where
  mempty = TestSpec Nothing [] False

parseTestSpec :: String -> TestSpec
parseTestSpec fileContents = foldMap parseLine (lines fileContents)
  where
    parseLine :: String -> TestSpec
    parseLine line
      | Just f <- stripPrefix "// XFAIL:" line = TestSpec (Just f) [] False
      | Just i <- stripPrefix "// INPUT:" line = TestSpec Nothing (words i) False
      | Just _ <- stripPrefix "// CHECK-ONLY" line = TestSpec Nothing [] True
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
        let (TestSpec failReason inputs checkOnly) = parseTestSpec contents
        inputStream <- mockInputs inputs
        case parseProgram contents of
          ParseError _ loc e -> expectationFailure $ "Parse error at " ++ show loc ++ ": expected " ++ show e
          ParseOk _ _ | checkOnly -> pure ()
          ParseOk _ p -> do
            result <- evalProgram p inputStream
            case result of
              Right _ -> case failReason of
                Nothing -> pure ()
                Just reason -> expectationFailure $ "unexpected pass" ++ reason
              Left err -> case failReason of
                Just reason -> pendingWith (reason ++ ": " ++ err)
                Nothing -> expectationFailure err
