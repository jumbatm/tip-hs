module Parser.Internal (Parser, sepBy, ws, lexeme, satisfy, token, keyword, satisfyWhile, runParser) where

import Control.Applicative
import Data.Char

newtype SourceLocation = SourceLocation (Int, Int) deriving (Show)

withSourceLocation :: String -> [(Char, SourceLocation)]
withSourceLocation [] = []
withSourceLocation (c:cs) = scanl go (c, SourceLocation (1, 1)) cs
  where
    go :: (Char, SourceLocation) -> Char -> (Char, SourceLocation)
    go (_, SourceLocation (line, col)) n = (n, case n of
                  '\n' -> SourceLocation (line+1, col)
                  _ -> SourceLocation (line, col+1))

-- Our parser type. We return pairs of (the parsed object, the remaining
-- string). We have a list as we're able to handle ambiguous grammars and
-- return all possible parse trees.
newtype Parser i o = Parser
  { runParser :: [i] -> Maybe (o, [i])
  }

instance Functor (Parser i) where
  fmap f p = Parser $ \s -> do
    (v, s') <- runParser p s
    return (f v, s')

instance Applicative (Parser i) where
  pure v = Parser $ \s -> Just (v, s)
  pf <*> pv = Parser $ \s -> do
    -- TODO: Which order should this actually be in? Unwrap f first or v? Does
    -- it matter?
    (f, s') <- runParser pf s
    (v, s'') <- runParser pv s'
    return (f v, s'')

instance Monad (Parser i) where
  pv >>= pf = Parser $ \s -> do
    (v, s') <- runParser pv s
    runParser (pf v) s'

instance Alternative (Parser i) where
  empty = Parser $ const Nothing
  p <|> q = Parser $ \s -> runParser p s <|> runParser q s


satisfy :: (i -> Bool) -> Parser i i
satisfy p = Parser f
  where
    -- FIXME: Why can't I write this signature?
    -- f :: [i] -> Maybe (i, [i])
    f [] = Nothing
    f (c : cs)
      | p c = Just (c, cs)
      | otherwise = Nothing


satisfyWhile :: (i -> Bool) -> Parser i [i]
satisfyWhile = many . satisfy

sepBy :: Parser i o -> Parser i o' -> Parser i [o]
sepBy p q = (:) <$> p <*> many (q *> p) <|> pure []

-- Parser on Strings.
type CharParser a = Parser Char a

ws :: CharParser [Char]
ws = satisfyWhile isSpace

-- Allow any amount of whitespace after the parser.
lexeme :: CharParser a -> CharParser a
lexeme p = p <* ws

token :: Char -> CharParser Char
token e = lexeme . Parser $ f
  where
    f (c : cs)
      | c == e = Just (c, cs)
      | otherwise = Nothing
    f [] = Nothing

keyword :: String -> CharParser [Char]
keyword = lexeme . sequenceA . fmap token
