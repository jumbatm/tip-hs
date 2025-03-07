module Parser.Internal (Parser, sepBy, ws, lexeme, satisfy, token, keyword, satisfyWhile, runParser) where

import Control.Applicative
import Data.Char

-- Our parser type. We return pairs of (the parsed object, the remaining
-- string). We have a list as we're able to handle ambiguous grammars and
-- return all possible parse trees.
newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (v, s') <- runParser p s
    return (f v, s')

instance Applicative Parser where
  pure v = Parser $ \s -> Just (v, s)
  pf <*> pv = Parser $ \s -> do
    -- TODO: Which order should this actually be in? Unwrap f first or v? Does
    -- it matter?
    (f, s') <- runParser pf s
    (v, s'') <- runParser pv s'
    return (f v, s'')

instance Monad Parser where
  pv >>= pf = Parser $ \s -> do
    (v, s') <- runParser pv s
    runParser (pf v) s'

instance Alternative Parser where
  empty = Parser $ const Nothing
  p <|> q = Parser $ \s -> runParser p s <|> runParser q s

ws :: Parser String
ws = satisfyWhile isSpace

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f :: String -> Maybe (Char, String)
    f [] = Nothing
    f (c : cs)
      | p c = Just (c, cs)
      | otherwise = Nothing

satisfyWhile :: (Char -> Bool) -> Parser [Char]
satisfyWhile = many . satisfy


sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = (:) <$> p <*> many (q *> p) <|> pure []

-- Allow any amount of whitespace after the parser.
lexeme :: Parser a -> Parser a
lexeme p = p <* ws

token :: Char -> Parser Char
token e = lexeme $ Parser $ f
  where
    f :: String -> Maybe (Char, String)
    f (c : cs)
      | c == e = Just (c, cs)
      | otherwise = Nothing
    f [] = Nothing

keyword :: String -> Parser String
keyword = lexeme . sequenceA . fmap token

