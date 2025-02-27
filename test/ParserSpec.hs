module ParserSpec (spec) where

import Data.Maybe (isNothing)
import Parser.Internal
import Test.Hspec
import Test.Hspec.QuickCheck

prop_char_parsed :: Char -> String -> Bool
prop_char_parsed e s@[] = isNothing $ runParser (charP e) s
prop_char_parsed e s@(c : cs) = runParser (charP e) s == if e == c then Just (c, cs) else Nothing

spec :: Spec
spec = do
  describe "charP" $ do
    prop "parses its character properly" $ do
      prop_char_parsed

  describe "stringP" $ do
    it "parses a valid sequence properly" $ do
      runParser (stringP "foo") "foobar" `shouldBe` Just ("foo", "bar")
    it "rejects an invalid sequence" $ do
      runParser (stringP "abc") "defghi" `shouldBe` Nothing

  describe "instance Alternative Parser" $ do
    it "returns result of first parser if it's successful" $ do
      runParser (stringP "foo" <|> stringP "bar") "barbaz" `shouldBe` Just ("bar", "baz")
    it "returns result of second parser if that's successful" $ do
      runParser (stringP "baz" <|> stringP "bar") "barfoo" `shouldBe` Just ("bar", "foo")
    it "returns Nothing if both parsers fail" $ do
      runParser (stringP "foo" <|> stringP "bar") "bazqux" `shouldBe` Nothing
