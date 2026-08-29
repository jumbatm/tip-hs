module Parser.TipLexer where

import Control.Applicative
import Data.Char
import Data.Functor.Identity
import Data.Set (singleton)
import Parser.CharParser (CharParser, CharParserState (..), satisfy, satisfyWhile, string, (<?>))
import Parser.Internal

type TipParser = CharParser Identity

lineComment :: TipParser String
lineComment = string "//" *> (string "\\\n" <|> satisfyWhile (/= '\n'))

-- Whitespace and comments consumer.
ws :: Parser CharParserState Identity [String]
ws = many (lineComment <|> ((: []) <$> satisfy isSpace))

-- Allow any amount of whitespace after a token parser.
-- Kept internal so non-terminal grammar rules cannot mistakenly call it.
lexeme :: TipParser a -> TipParser a
lexeme p = p <* ws

-- Terminals / Lexemes

symbol :: String -> TipParser String
symbol s = lexeme (string s <?> s)

keyword :: String -> TipParser String
keyword s = lexeme (string s <?> s)

char :: Char -> TipParser Char
char c = lexeme (satisfy (== c) <?> [c])

-- Raw identifier (used for testing or as primitive)
identifierP :: TipParser String
identifierP = (:) <$> satisfy isAlpha <*> satisfyWhile (\x -> isDigit x || isAlpha x) <?> "an identifier"

-- Lexeme identifier (consumes trailing whitespace)
identifier :: TipParser String
identifier = lexeme identifierP

intLit :: TipParser Int
intLit = lexeme (read <$> ((:) <$> satisfy isDigit <*> satisfyWhile isDigit) <?> "an integer")

-- Enclosure and punctuation helpers

parens :: TipParser a -> TipParser a
parens p = char '(' *> p <* char ')'

braces :: TipParser a -> TipParser a
braces p = char '{' *> p <* char '}'

semi :: TipParser Char
semi = char ';'

comma :: TipParser Char
comma = char ','

eof :: TipParser ()
eof = Parser $ \st@(CharParserState ch pos) -> pure $ case ch of
  "" -> ParseOk Empty ((), st)
  _ -> ParseError Empty pos (singleton "end of file")
