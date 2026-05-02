module Parser.Internal where

import Control.Applicative
import Control.Monad
import Data.Set as S

newtype SourceLocation = SourceLocation (Int, Int) deriving (Show, Eq, Ord)

-- Error handling.

data Progress = Consumed | Empty deriving (Show, Eq)

data ParseResult a = ParseError Progress SourceLocation (Set String) | ParseOk a deriving (Show, Eq)

instance Functor ParseResult where
  fmap f (ParseOk v) = ParseOk $ f v
  fmap _ (ParseError p l s) = ParseError p l s

instance Applicative ParseResult where
  pure = ParseOk

  ParseError p l s <*> _ = ParseError p l s
  ParseOk f <*> r = fmap f r

instance Monad ParseResult where
  ParseError p l s >>= _ = ParseError p l s
  ParseOk v >>= f = f v

instance Alternative ParseResult where
  empty = ParseError Empty (SourceLocation (0, 0)) S.empty
  (<|>) = mergeErrors

mergeErrors :: ParseResult a -> ParseResult a -> ParseResult a
mergeErrors (ParseOk v) _ = ParseOk v
mergeErrors (ParseError {}) (ParseOk v) = ParseOk v
-- TODO: Do ParseResults even need to track their fail location? Multiple parse
-- Wouldn't multiple parse errors end up failing in the same spot?
--
-- NOTE: When commitment rule implemented (partial progress = parser commits), then we don't need to worry about the source location.
-- If left parser partially succeeded, we'll commit to to the left parser.
-- If left parser failed and right parser partially succeeded, we'll commit to the right parser.
-- If neither made progress, then they'll both agree on source location.
--
-- Probably a reasonable strategy is to implement Alternative first and then see whether we need this at all.
mergeErrors (ParseError lp ll ls) (ParseError rp rl rs) = ParseError (merge lp rp) (min ll rl) (S.union ls rs)
  where
    merge :: Progress -> Progress -> Progress
    merge Empty Empty = Empty
    merge l Empty = l
    merge Empty r = r
    merge Consumed Consumed = Consumed

-- Our parser type. We return pairs of (the parsed object, the remaining
-- string). We have a list as we're able to handle ambiguous grammars and
-- return all possible parse trees.
newtype (Monad m) => Parser s m o = Parser
  { unParser :: s -> m (ParseResult (o, s))
  }

instance (Monad m) => Functor (Parser i m) where
  fmap f p = Parser $ \s -> do
    r <- unParser p s
    -- TODO: There's probably a nicer way to write this which uses
    -- ParseResult's fmap instance. Apparently there's Control.Arrow.first
    -- which will apply a function to the first element of a tuple.
    pure $ case r of
      ParseOk (v, s') -> ParseOk (f v, s')
      ParseError p loc ex -> ParseError p loc ex

instance (Show i, Monad m) => Applicative (Parser i m) where
  pure v = Parser $ \s -> pure $ ParseOk (v, s)
  (<*>) = ap

instance (Monad m, Show i) => Monad (Parser i m) where
  pv >>= pf = Parser $ \s -> do
    v <- unParser pv s
    case v of
      ParseError _ loc msg -> pure $ ParseError Empty loc msg
      ParseOk (vv, vs) -> do
        result <- unParser (pf vv) vs
        pure $ case result of
          ParseError _ loc ex -> ParseError Consumed loc ex
          ok@ParseOk {} -> ok

instance (Monad m, Show i) => Alternative (Parser i m) where
  empty = Parser $ \_ -> pure Control.Applicative.empty

  -- TODO: Implement "commitment" rule? If a parser partially succeeds, don't allow backtracking -- that is, don't try running the next parser.
  -- This improves error messages. Essentially, it means if we see an error in a certain construct, then we diagnose that as an error in that particular construct rather than seeing it as a reason to try a different parser.
  --
  -- We can check for partial match by looking at the source location information and seeing if that changed.
  --
  -- Note that we occasionally might have a valid reason to still try backtracking. This is where having `try` can be helpful.
  p <|> q = Parser $ \s -> do
    pl <- unParser p s
    case pl of
      ParseOk v -> pure $ ParseOk v
      el@(ParseError {}) -> do
        pr <- unParser q s
        case pr of
          ParseOk v -> pure $ ParseOk v
          er@(ParseError {}) -> pure $ mergeErrors el er

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

sepBy :: (Show i, Monad m) => Parser i m o -> Parser i m o' -> Parser i m [o]
sepBy p q = (:) <$> p <*> many (q *> p) <|> pure []
