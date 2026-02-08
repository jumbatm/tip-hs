module ParserSpec (spec) where

import Control.Applicative
import Data.Char
import Data.Functor.Identity
import Data.Maybe (isNothing)
import Parser.CharParser
import Parser.Internal
import Parser.TipParser
import Test.Hspec
import Test.Hspec.QuickCheck

-- Run the parser, returning the result value and the remaining string.
testRunParser :: CharParser Identity o -> String -> ParseResult (o, [Char])
testRunParser p s = runIdentity $ f <$> (unParser p) (CharParserState s (SourceLocation (1, 1)))
  where
    -- f :: ParseResult (o, CharParserState) -> ParseResult (o, [Char])
    f (ParseOk (v, st)) = ParseOk (v, getRawChars st)
    f (ParseError loc ex) = ParseError loc ex

testError :: ParseResult a -> Bool
testError (ParseError _ _) = True
testError (ParseOk _) = False

prop_char_parsed :: Char -> String -> Bool
prop_char_parsed e s@[] = testRunParser (char e) s == ParseError (Just $ SourceLocation (1, 1)) [[e]]
prop_char_parsed e s@(c : cs) = test $ testRunParser (char e) s
  where
    test p | c == e = p == ParseOk (c, dropWhile isSpace cs)
    test p = p == ParseError (Just $ SourceLocation (1, 1)) [[e]]

prop_valid_identifier :: String -> Bool
prop_valid_identifier s =
  let r = testRunParser identifierP s; (i, rest) = spanValidIdentifier s
   in case s of
        (c : _) | not $ null i -> r == ParseOk (i, rest)
        _ -> case r of
          ParseError _ _ -> True
          _ -> False
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
  describe "char" $ do
    it "matches a simple positive case" $ do
      testRunParser (char 'a') "abc" `shouldBe` ParseOk ('a', "bc")
    it "rejects a simple negative case" $ do
      testRunParser (char 'a') "cba" `shouldBe` ParseError (Just $ SourceLocation (1, 1)) ["a"]
    prop "parses its character properly" $ do
      prop_char_parsed

  describe "keyword" $ do
    it "parses a valid sequence properly" $ do
      testRunParser (keyword "foo") "foobar" `shouldBe` ParseOk ("foo", "bar")
    it "rejects an invalid sequence" $ do
      testRunParser (keyword "abc") "defghi" `shouldBe` ParseError (Just $ SourceLocation (1, 1)) ["abc"]

  describe "instance Alternative Parser" $ do
    it "returns result of first parser if it's successful" $ do
      testRunParser (keyword "foo" <|> keyword "bar") "barbaz" `shouldBe` ParseOk ("bar", "baz")
    it "returns result of second parser if that's successful" $ do
      testRunParser (keyword "baz" <|> keyword "bar") "barfoo" `shouldBe` ParseOk ("bar", "foo")
    it "returns Nothing if both parsers fail" $ do
      testRunParser (keyword "foo" <|> keyword "bar") "bazqux" `shouldBe` ParseError (Just $ SourceLocation (1, 1)) ["foo", "bar"]

  describe "satisfyWhile" $ do
    it "correctly splits string starting with chars which satisfy the predicate" $ do
      testRunParser (satisfyWhile (== 'a')) "aaabbb" `shouldBe` ParseOk ("aaa", "bbb")
    it "correctly consumes nothing if no chars satisfy the predicate" $ do
      testRunParser (satisfyWhile (== 'a')) "bbbccc" `shouldBe` ParseOk ("", "bbbccc")
    it "correctly consumes nothing if matching chars not at start" $ do
      testRunParser (satisfyWhile (== 'a')) "bbbaaa" `shouldBe` ParseOk ("", "bbbaaa")

  describe "identifierP" $ do
    prop "only allows identifiers starting with a letter, followed by letters or numbers" $ do
      prop_valid_identifier
