module Parser.CharParser where

import Control.Applicative
import Data.Char
import Parser.Internal

-- Parser on Strings.
type CharParser = Parser Char

satisfy :: (Alternative m, Monad m) => (Char -> Bool) -> CharParser m Char
satisfy p = Parser f
  where
    f [] = empty
    f (c : cs)
      | p c = pure (c, cs) -- TODO: Update pos.
      | otherwise = empty

satisfyWhile :: (Alternative m, Monad m) => (Char -> Bool) -> CharParser m [Char]
satisfyWhile = many . satisfy

ws :: (Alternative m, Monad m) => CharParser m [Char]
ws = satisfyWhile isSpace

-- Allow any amount of whitespace after the parser.
lexeme :: (Alternative m, Monad m) => CharParser m a -> CharParser m a
lexeme p = p <* ws

nextChar :: (Monad m, Alternative m) => CharParser m Char
nextChar = satisfy (const True)

char :: (Alternative m, Monad m) => Char -> CharParser m Char
char e = lexeme . Parser $ f
  where
    f (c : cs)
      | c == e = pure (c, cs)
      | otherwise = empty
    f [] = empty

keyword :: (Alternative m, Monad m) => String -> CharParser m [Char]
keyword = lexeme . sequenceA . fmap char
