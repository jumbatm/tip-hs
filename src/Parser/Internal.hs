module Parser.Internal (Parser, sepBy, ws, lexeme, satisfy, token, keyword, satisfyWhile, runParser, parseError, ParseResult (..), SourceLocation (..)) where

import Control.Applicative
import Data.Char

newtype SourceLocation = SourceLocation (Int, Int) deriving (Show)

withSourceLocation :: String -> [(Char, SourceLocation)]
withSourceLocation [] = []
withSourceLocation (c : cs) = scanl go (c, SourceLocation (1, 1)) cs
  where
    go :: (Char, SourceLocation) -> Char -> (Char, SourceLocation)
    go (_, SourceLocation (line, col)) n =
      ( n,
        case n of
          '\n' -> SourceLocation (line + 1, col)
          _ -> SourceLocation (line, col + 1)
      )

-- Error handling.

data ParseResult a = ParseError String | ParseOk a deriving (Show, Eq)

instance Functor ParseResult where
  fmap f (ParseOk v) = ParseOk $ f v
  fmap _ (ParseError m) = ParseError m

instance Applicative ParseResult where
  pure = ParseOk

  ParseError m <*> _ = ParseError m
  ParseOk f <*> r = fmap f r

instance Monad ParseResult where
  ParseError m >>= _ = ParseError m
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
newtype (Monad m) => Parser m i o = Parser
  { runParser :: [i] -> m (o, [i])
  }

instance (Monad m) => Functor (Parser m i) where
  fmap f p = Parser $ \s -> do
    (v, s') <- runParser p s
    return (f v, s')

instance (Monad m) => Applicative (Parser m i) where
  pure v = Parser $ \s -> pure (v, s)
  pf <*> pv = Parser $ \s -> do
    -- TODO: Which order should this actually be in? Unwrap f first or v? Does
    -- it matter?
    (f, s') <- runParser pf s
    (v, s'') <- runParser pv s'
    return (f v, s'')

instance (Monad m) => Monad (Parser m i) where
  pv >>= pf = Parser $ \s -> do
    (v, s') <- runParser pv s
    runParser (pf v) s'

instance (Alternative m, Monad m) => Alternative (Parser m i) where
  empty = Parser $ const empty
  p <|> q = Parser $ \s -> runParser p s <|> runParser q s

satisfy :: (Alternative m, Monad m) => (i -> Bool) -> Parser m i i
satisfy p = Parser f
  where
    -- FIXME: Why can't I write this signature?
    -- f :: [i] -> Maybe (i, [i])
    f [] = empty
    f (c : cs)
      | p c = pure (c, cs)
      | otherwise = empty

satisfyWhile :: (Alternative m, Monad m) => (i -> Bool) -> Parser m i [i]
satisfyWhile = many . satisfy

sepBy :: (Alternative m, Monad m) => Parser m i o -> Parser m i o' -> Parser m i [o]
sepBy p q = (:) <$> p <*> many (q *> p) <|> pure []

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

parseError :: (MonadFail m) => String -> CharParser m a
parseError msg = Parser $ pure (fail msg)

keyword :: (Alternative m, Monad m) => String -> CharParser m [Char]
keyword = lexeme . sequenceA . fmap token
