module Parser.Internal (Parser (..), sepBy, satisfy, satisfyWhile, runParser) where

import Control.Applicative

-- Our parser type. We return pairs of (the parsed object, the remaining
-- string). We have a list as we're able to handle ambiguous grammars and
-- return all possible parse trees.
newtype Monad m => Parser m i o = Parser
  { runParser :: [i] -> m (o, [i])
  }

instance Monad m => Functor (Parser m i) where
  fmap f p = Parser $ \s -> do
    (v, s') <- runParser p s
    return (f v, s')

instance Monad m => Applicative (Parser m i) where
  pure v = Parser $ \s -> pure (v, s)
  pf <*> pv = Parser $ \s -> do
    -- TODO: Which order should this actually be in? Unwrap f first or v? Does
    -- it matter?
    (f, s') <- runParser pf s
    (v, s'') <- runParser pv s'
    return (f v, s'')

instance Monad m => Monad (Parser m i) where
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

