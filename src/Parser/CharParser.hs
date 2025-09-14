{-# LANGUAGE ScopedTypeVariables #-}

module Parser.CharParser where

import Control.Applicative
import Data.Char
import Parser.Internal

-- Parser on Strings.
type CharParser = Parser Char

satisfy :: forall i. forall m. (Alternative m, Monad m) => (i -> Bool) -> Parser i m i
satisfy p = Parser f
  where
    f :: (Alternative m) => [i] -> m (i, [i])
    f [] = empty
    f (c : cs)
      | p c = pure (c, cs)
      | otherwise = empty

satisfyWhile :: (Alternative m, Monad m) => (i -> Bool) -> Parser i m [i]
satisfyWhile = many . satisfy

ws :: (Alternative m, Monad m) => CharParser m [Char]
ws = satisfyWhile isSpace

-- Allow any amount of whitespace after the parser.
lexeme :: (Alternative m, Monad m) => CharParser m a -> CharParser m a
lexeme p = p <* ws

nextChar :: (Monad m, Alternative m) => CharParser m Char
nextChar = Parser f
  where
    -- TODO: Automatically update source location.
    f (c : cs) = pure (c, cs)
    f [] = empty

char :: (Alternative m, Monad m) => Char -> CharParser m Char
char e = lexeme . Parser $ f
  where
    f (c : cs)
      | c == e = pure (c, cs)
      | otherwise = empty
    f [] = empty

keyword :: (Alternative m, Monad m) => String -> CharParser m [Char]
keyword = lexeme . sequenceA . fmap char
