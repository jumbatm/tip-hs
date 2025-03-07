module Parser.CharParser (CharParser, ws, lexeme, token, keyword) where

import Parser.Internal
import Control.Applicative
import Data.Char

-- Parser on Strings.
type CharParser m a = Parser m Char a

ws :: (Alternative m, Monad m) => CharParser m [Char]
ws = satisfyWhile isSpace

-- Allow any amount of whitespace after the parser.
lexeme :: (Alternative m, Monad m) => CharParser m a -> CharParser m a
lexeme p = p <* ws

token :: (Alternative m, Monad m) => Char -> CharParser m Char
token e = lexeme . Parser $ f
  where
    f (c : cs)
      | c == e = pure (c, cs)
      | otherwise = empty
    f [] = empty

keyword :: (Alternative m, Monad m) => String -> CharParser m [Char]
keyword = lexeme . sequenceA . fmap token
