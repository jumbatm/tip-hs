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
testRunParser :: (Monad m) => CharParser m o -> String -> m (ParseResult o, [Char])
testRunParser p s = (\(result, state) -> (result, getRawChars state)) <$> (unParser p) (CharParserState s (SourceLocation (1, 1)))

prop_char_parsed :: Char -> String -> Bool
prop_char_parsed e s@[] = isNothing $ testRunParser (char e) s
prop_char_parsed e s@(c : cs) = testRunParser (char e) s == if e == c then Just (pure c, dropWhile isSpace cs) else Nothing

prop_valid_identifier :: String -> Bool
prop_valid_identifier s =
  let r = testRunParser identifierP s; (i, rest) = spanValidIdentifier s
   in case s of
        (c : _) | not $ null i -> r == (Identity $ (ParseOk i, rest))
        _ -> case (fst $ runIdentity $ r) of
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
    prop "parses its character properly" $ do
      prop_char_parsed

  describe "keyword" $ do
    it "parses a valid sequence properly" $ do
      testRunParser (keyword "foo") "foobar" `shouldBe` Identity (ParseOk "foo", "bar")
    it "rejects an invalid sequence" $ do
      testRunParser (keyword "abc") "defghi" `shouldBe` Nothing

  describe "instance Alternative Parser" $ do
    it "returns result of first parser if it's successful" $ do
      testRunParser (keyword "foo" <|> keyword "bar") "barbaz" `shouldBe` Identity (ParseOk "bar", "baz")
    it "returns result of second parser if that's successful" $ do
      testRunParser (keyword "baz" <|> keyword "bar") "barfoo" `shouldBe` Identity (ParseOk "bar", "foo")
    it "returns Nothing if both parsers fail" $ do
      testRunParser (keyword "foo" <|> keyword "bar") "bazqux" `shouldBe` Nothing

  describe "satisfyWhile" $ do
    it "correctly splits string starting with chars which satisfy the predicate" $ do
      testRunParser (satisfyWhile (== 'a')) "aaabbb" `shouldBe` Identity (ParseOk "aaa", "bbb")
    it "correctly consumes nothing if no chars satisfy the predicate" $ do
      testRunParser (satisfyWhile (== 'a')) "bbbccc" `shouldBe` Identity (ParseOk "", "bbbccc")
    it "correctly consumes nothing if matching chars not at start" $ do
      testRunParser (satisfyWhile (== 'a')) "bbbaaa" `shouldBe` Identity (ParseOk "", "bbbaaa")

  describe "identifierP" $ do
    prop "only allows identifiers starting with a letter, followed by letters or numbers" $ do
      prop_valid_identifier
