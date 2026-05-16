module ParserSpec (spec) where

import Control.Applicative
import Data.Char
import Data.Functor.Identity
import Data.Maybe (isNothing)
import Data.Set as S
import Parser.CharParser (CharParser, CharParserState (CharParserState), getRawChars, satisfyWhile, string)
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
    f (ParseError p loc ex) = ParseError p loc ex

testError :: ParseResult a -> Bool
testError (ParseError _ _ _) = True
testError (ParseOk _) = False

prop_char_parsed :: Char -> String -> Bool
prop_char_parsed e s@[] = testRunParser (char e) s == ParseError Empty (SourceLocation (1, 1)) (singleton [e])
prop_char_parsed e s@(c : cs) = test $ testRunParser (char e) s
  where
    test p | c == e = p == ParseOk (c, dropWhile isSpace cs)
    test p = p == ParseError Empty (SourceLocation (1, 1)) (singleton [e])

prop_valid_identifier :: String -> Bool
prop_valid_identifier s =
  let r = testRunParser identifierP s; (i, rest) = spanValidIdentifier s
   in case s of
        (c : _) | not $ Prelude.null i -> r == ParseOk (i, rest)
        _ -> case r of
          ParseError _ _ _ -> True
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
      testRunParser (char 'a') "cba" `shouldBe` ParseError Empty (SourceLocation (1, 1)) (singleton "a")
    prop "parses its character properly" $ do
      prop_char_parsed

  describe "keyword" $ do
    it "parses a valid sequence properly" $ do
      testRunParser (keyword "foo") "foobar" `shouldBe` ParseOk ("foo", "bar")
    it "rejects an invalid sequence" $ do
      testRunParser (keyword "abc") "defghi" `shouldBe` ParseError Empty (SourceLocation (1, 1)) (singleton "abc")

  describe "instance Alternative Parser" $ do
    it "returns result of first parser if it's successful" $ do
      testRunParser (keyword "foo" <|> keyword "bar") "barbaz" `shouldBe` ParseOk ("bar", "baz")
    it "returns result of second parser if that's successful" $ do
      testRunParser (keyword "baz" <|> keyword "bar") "barfoo" `shouldBe` ParseOk ("bar", "foo")
    it "returns Nothing if both parsers fail" $ do
      testRunParser (keyword "foo" <|> keyword "bar") "bazqux" `shouldBe` ParseError Empty (SourceLocation (1, 1)) (fromList ["foo", "bar"])
    it "commits to the first parser if we got a partial match" $ do
      testRunParser (string "ABC" <|> string "ABG") "ABG" `shouldBe` ParseError Consumed (SourceLocation (1, 3)) (fromList ["ABC"])
    it "allows backtracking if the try combinator is used" $ do
      testRunParser (try (string "ABC") <|> string "ABG") "ABG" `shouldBe` ParseOk ("ABG", "")

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

  describe "progress tracking" $ do
    it "marks a partial match as consumed" $ do
      testRunParser (string "AB") "AC" `shouldBe` ParseError Consumed (SourceLocation (1, 2)) (fromList ["AB"])
    it "marks a partial match as consumed with 2 matches" $ do
      testRunParser (string "ABC") "ABD" `shouldBe` ParseError Consumed (SourceLocation (1, 3)) (fromList ["ABC"])
    it "marks no match as Empty" $ do
      testRunParser (string "AB") "BC" `shouldBe` ParseError Empty (SourceLocation (1, 1)) (fromList ["AB"])
    it "marks match with gap as Empty" $ do
      testRunParser (string "AB") "ACB" `shouldBe` ParseError Consumed (SourceLocation (1, 2)) (fromList ["AB"])

  describe "error handling" $ do
    it "generates an intuitive token set for many + a parser which partially succeeded" $ do
      -- TODO: better error handling:
      -- ghci> runParser (char '{' *> many statementP <* char '}') "{ output; }"
      -- Identity (ParseError (Just (SourceLocation (1,3))) (fromList ["}"])) -- Assumes the parsing error means we actually expected no statements
      -- ghci> runParser (char '{' *> some statementP <* char '}') "{ output; }"
      -- Identity (ParseError (Just (SourceLocation (1,3))) (fromList ["(","an identifier","an integer","if","return","var"])) -- Would have been much better
      pendingWith "improved error message generation"
      testRunParser (char '{' *> (many (char 'A') <|> string "BC") <* char '}') "{ B" `shouldBe` ParseError Empty (SourceLocation (1, 3)) (fromList ["C"])
    it "generates an intuitive token set for some + a parser which partially succeeded" $ do
      -- pendingWith "improved error message generation"
      testRunParser (char '{' *> (some (char 'A') <|> string "BC") <* char '}') "{ B" `shouldBe` ParseError Empty (SourceLocation (1, 3)) (fromList ["A", "BC"])
