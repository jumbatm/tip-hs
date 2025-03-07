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

newtype SourceLocation = SourceLocation (Int, Int) deriving (Show)

withSourceLocation :: String -> [(Char, SourceLocation)]
withSourceLocation [] = []
withSourceLocation (c:cs) = scanl go (c, SourceLocation (1, 1)) cs
  where
    go :: (Char, SourceLocation) -> Char -> (Char, SourceLocation)
    go (_, SourceLocation (line, col)) n = (n, case n of
                  '\n' -> SourceLocation (line+1, col)
                  _ -> SourceLocation (line, col+1))
