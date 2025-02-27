module ParserSpec (spec) where

import Control.Applicative
import Data.Char
import Data.Maybe (isNothing)
import Parser.Internal
import Test.Hspec
import Test.Hspec.QuickCheck

prop_char_parsed :: Char -> String -> Bool
prop_char_parsed e s@[] = isNothing $ runParser (charP e) s
prop_char_parsed e s@(c : cs) = runParser (charP e) s == if e == c then Just (c, cs) else Nothing

prop_valid_identifier :: String -> Bool
prop_valid_identifier s =
  let r = runParser identifierP s; (i, rest) = spanValidIdentifier s
   in case s of
        (c : _) | not $ null i -> r == Just (Identifier i, rest)
        _ -> isNothing r
  where
    spanValidIdentifier :: String -> (String, String)
    spanValidIdentifier [] = ("", "")
    spanValidIdentifier s@(c : cs)
      | isAlpha c = let i = c; (is, t) = span isAlphaOrDigit cs in (i : is, t)
      | otherwise = ("", s)
      where
        isAlphaOrDigit c = isAlpha c || isDigit c

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

  describe "spanP" $ do
    it "correctly splits string starting with chars which satisfy the predicate" $ do
      runParser (spanP (== 'a')) "aaabbb" `shouldBe` Just ("aaa", "bbb")
    it "correctly consumes nothing if no chars satisfy the predicate" $ do
      runParser (spanP (== 'a')) "bbbccc" `shouldBe` Just ("", "bbbccc")
    it "correctly consumes nothing if matching chars not at start" $ do
      runParser (spanP (== 'a')) "bbbaaa" `shouldBe` Just ("", "bbbaaa")

  describe "identifierP" $ do
    prop "only allows identifiers starting with a letter, followed by letters or numbers" $ do
      prop_valid_identifier
