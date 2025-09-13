module Parser.Internal (Parser, CharParser, sepBy, ws, lexeme, satisfy, char, keyword, satisfyWhile, runParser, parseError, ParseResult (..), SourceLocation (..)) where

import Control.Applicative
import Data.Char

newtype SourceLocation = SourceLocation (Int, Int) deriving (Show)

-- Error handling.

data ParseResult a = ParseError String | ParseOk a deriving (Show, Eq)

instance Functor ParseResult where
  fmap f (ParseOk v) = ParseOk $ f v
  fmap _(ParseError m) = ParseError m

instance Applicative ParseResult where
  pure = ParseOk

  ParseError m <*> _ = ParseError m
  ParseOk f <*> r = fmap f r

instance Monad ParseResult where
  ParseError m  >>= _ = ParseError m
  ParseOk v >>= f = f v

instance MonadFail ParseResult where
  fail = ParseError

instance Alternative ParseResult where
  empty = ParseError ""

  (ParseOk v) <|> _ = ParseOk v
  (ParseError _) <|> (ParseOk v) = ParseOk v
  -- Is the rightmost ParseError the right one?
  (ParseError _) <|> (ParseError rm) = ParseError rm


-- Our parser type. We return pairs of (the parsed object, the remaining
-- string). We have a list as we're able to handle ambiguous grammars and
-- return all possible parse trees.
newtype Monad m => Parser i m o = Parser
  { runParser :: [i] -> m (o, [i])
  }

instance Monad m => Functor (Parser i m) where
  fmap f p = Parser $ \s -> do
    (v, s') <- runParser p s
    return (f v, s')

instance Monad m => Applicative (Parser i m) where
  pure v = Parser $ \s -> pure (v, s)
  pf <*> pv = Parser $ \s -> do
    -- TODO: Which order should this actually be in? Unwrap f first or v? Does
    -- it matter?
    (f, s') <- runParser pf s
    (v, s'') <- runParser pv s'
    return (f v, s'')

instance Monad m => Monad (Parser i m) where
  pv >>= pf = Parser $ \s -> do
    (v, s') <- runParser pv s
    runParser (pf v) s'

instance (Alternative m, Monad m) => Alternative (Parser i m) where
  empty = Parser $ const empty
  p <|> q = Parser $ \s -> runParser p s <|> runParser q s


satisfy :: (Alternative m, Monad m) => (i -> Bool) -> Parser i m i
satisfy p = Parser f
  where
    -- FIXME: Why can't I write this signature?
    -- f :: [i] -> Maybe (i, [i])
    f [] = empty
    f (c : cs)
      | p c = pure (c, cs)
      | otherwise = empty


satisfyWhile :: (Alternative m, Monad m) => (i -> Bool) -> Parser i m [i]
satisfyWhile = many . satisfy

sepBy :: (Alternative m, Monad m) => Parser i m o -> Parser i m o' -> Parser i m [o]
sepBy p q = (:) <$> p <*> many (q *> p) <|> pure []

parseError :: MonadFail m => String -> Parser i m o
parseError msg = Parser $ pure (fail msg)

-- Parser on Strings.
type CharParser = Parser Char

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
