module Parser.Internal where

import Control.Applicative

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


-- NOTE: As it turns out, a generic satisfy doesn't necessarily work out in all
-- cases. For example, we want CharParser to be able to update its own source
-- location automatically. However, we can't use nextChar here since it
-- operates only on Chars, and the parsers here want `i`. Even Parsec needs to
-- have "re-implementations" of these functions. Go figure.
-- https://hackage.haskell.org/package/parsec-3.1.18.0/docs/src/Text.Parsec.Char.html#satisfy
--
-- For our case, we'll just only implement satisfy in CharParser.
--
-- satisfy :: (Alternative m, Monad m) => (i -> Bool) -> Parser i m i
-- satisfy p = Parser f
--   where
--     -- FIXME: Why can't I write this signature?
--     -- f :: [i] -> Maybe (i, [i])
--     f [] = empty
--     f (c : cs)
--       | p c = pure (c, cs)
--       | otherwise = empty
--
-- satisfyWhile :: (Alternative m, Monad m) => (i -> Bool) -> Parser i m [i]
-- satisfyWhile = many . satisfy

sepBy :: (Alternative m, Monad m) => Parser i m o -> Parser i m o' -> Parser i m [o]
sepBy p q = (:) <$> p <*> many (q *> p) <|> pure []

parseError :: MonadFail m => String -> Parser i m o
parseError msg = Parser $ pure (fail msg)
