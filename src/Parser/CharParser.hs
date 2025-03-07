module Parser.CharParser (CharParser, ws, lexeme, token, keyword) where

import Parser.Internal
import Control.Applicative
import Data.Char

-- Parser on Chars which tracks source locations.
type CharParser m a = Parser m (Char, SourceLocation) (a, SourceLocation)

ws :: CharParser m [Char]
ws = satisfyWhile $ isSpace . fst

-- Allow any amount of whitespace after the parser.
lexeme :: (Alternative m, Monad m) => CharParser m a -> CharParser m a
lexeme p = p <* ws

token :: (Alternative m, Monad m) => Char -> CharParser m Char
token e = lexeme $ satisfy (== e)

keyword :: (Alternative m, Monad m) => String -> CharParser m [Char]
keyword = lexeme . sequenceA . fmap token
