module Parser.CharParser where

import Control.Applicative
import Data.Char
import Parser.Internal

data CharParserState = CharParserState {getRawChars :: [Char], getRawPos :: SourceLocation} deriving Show

-- Parser on Strings.
type CharParser = Parser CharParserState

getPos :: Monad m => CharParser m SourceLocation
getPos = updatePos id

setPos :: Monad m => SourceLocation -> CharParser m SourceLocation
setPos = updatePos . const

updatePos :: Monad m => (SourceLocation -> SourceLocation) -> CharParser m SourceLocation
updatePos f = Parser $ \(CharParserState rawChars rawPos) -> let newPos = f rawPos in pure (newPos, CharParserState rawChars newPos)

satisfy :: (Alternative m, Monad m) => (Char -> Bool) -> CharParser m Char
satisfy p = Parser f
  where
    f (CharParserState (c : cs) l) | p c = pure (c, CharParserState cs (computeNewPos c l))
    f (CharParserState _ _) = empty

    computeNewPos c (SourceLocation (line, col)) = SourceLocation $ case c of
     '\n' -> (line+1, 1)
     _    -> (line, col+1)

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
char e = lexeme $ satisfy (== e)

keyword :: (Alternative m, Monad m) => String -> CharParser m [Char]
keyword = lexeme . sequenceA . fmap char
