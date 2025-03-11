module Parser.CharParser (CharParser, ws)  where

import Parser.Internal
import Control.Applicative
import Data.Char

-- Parser on Chars which tracks source locations.
type CharParser m a = Parser m (Char, SourceLocation) (a, SourceLocation)

newtype SourceLocation = SourceLocation (Int, Int) deriving (Show)

runParserWithLocs :: Monad m => CharParser m a -> String -> m ((a, SourceLocation), [(Char, SourceLocation)])
runParserWithLocs p s = runParser p (withSourceLocation s)

withSourceLocation :: String -> [(Char, SourceLocation)]
withSourceLocation [] = []
withSourceLocation (c:cs) = scanl go (c, SourceLocation (1, 1)) cs
  where
    go :: (Char, SourceLocation) -> Char -> (Char, SourceLocation)
    go (_, SourceLocation (line, col)) n = (n, case n of
                  '\n' -> SourceLocation (line+1, col)
                  _ -> SourceLocation (line, col+1))

-- Lifts a parser producing multiple source locations into a parser producing
-- the result but with only the first source location.
lift :: Monad m => Parser m (Char, SourceLocation) [(a, SourceLocation)] -> CharParser m [a]
lift p = (\charsAndLocs -> (map fst charsAndLocs, snd . head $ charsAndLocs)) <$> p

ws :: (Alternative m, Monad m) => CharParser m [Char]
ws = lift $ satisfyWhile (isSpace . fst)

-- Allow any amount of whitespace after the parser.
lexeme :: (Alternative m, Monad m) => CharParser m a -> CharParser m a
lexeme p = p <* ws

token :: (Alternative m, Monad m) => Char -> CharParser m Char
token e = lexeme $ satisfy $ (== e) . fst

keyword :: (Alternative m, Monad m) => String -> CharParser m [Char]
keyword = lexeme . lift . sequenceA . fmap token
