module Parser.CharParser where

import Control.Applicative
import Data.Char
import Data.Set as S
import Parser.Internal

data CharParserState = CharParserState {getRawChars :: [Char], getRawPos :: SourceLocation} deriving (Show)

-- Parser on Strings.
type CharParser = Parser CharParserState

getPos :: (Monad m) => CharParser m SourceLocation
getPos = updatePos id

setPos :: (Monad m) => SourceLocation -> CharParser m SourceLocation
setPos = updatePos . const

updatePos :: (Monad m) => (SourceLocation -> SourceLocation) -> CharParser m SourceLocation
updatePos f = Parser $ \(CharParserState rawChars rawPos) -> let newPos = f rawPos in pure $ ParseOk (newPos, CharParserState rawChars newPos)

satisfy :: (Monad m) => (Char -> Bool) -> CharParser m Char
satisfy p = Parser f
  where
    f (CharParserState (c : cs) l) | p c = pure $ ParseOk (c, CharParserState cs (computeNewPos c l))
    f (CharParserState _ pos) = pure $ ParseError pos S.empty

    computeNewPos c (SourceLocation (line, col)) = SourceLocation $ case c of
      '\n' -> (line + 1, 1)
      _ -> (line, col + 1)

satisfyWhile :: (Monad m) => (Char -> Bool) -> CharParser m [Char]
satisfyWhile = many . satisfy

ws :: (Monad m) => CharParser m [Char]
ws = satisfyWhile isSpace

-- Allow any amount of whitespace after the parser.
lexeme :: (Monad m) => CharParser m a -> CharParser m a
lexeme p = p <* ws

nextChar :: (Monad m) => CharParser m Char
nextChar = satisfy (const True)

label :: (Monad m) => String -> CharParser m a -> CharParser m a
label msg parser = Parser $ \s -> do
  r <- unParser parser s
  pure $ case r of
    ParseError loc _ -> ParseError loc (singleton msg)
    ParseOk v -> ParseOk v

-- Label operator.
(<?>) :: (Monad m) => CharParser m a -> String -> CharParser m a
(<?>) = flip label

infixl 2 <?>

char :: (Monad m) => Char -> CharParser m Char
char e = lexeme $ satisfy (== e) <?> [e]

string :: (Monad m) => String -> Parser CharParserState m String
string s = traverse (\c -> satisfy (== c)) s <?> s

keyword :: (Monad m) => String -> CharParser m [Char]
keyword w = lexeme $ traverse char w <?> w

-- traverse f w = sequenceA $ fmap f w
