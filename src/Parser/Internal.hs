module Parser.Internal (parse, charP, stringP, spanP, identifierP, runParser, AST (..)) where

import Control.Applicative
import Data.Char
import Data.Maybe (fromMaybe)

-- Our parser type. We return pairs of (the parsed object, the remaining
-- string). We have a list as we're able to handle ambiguous grammars and
-- return all possible parse trees.
newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

data AST
  = Identifier String
  | Function String
  deriving (Show, Eq)

-- Actual function we export.
parse :: String -> Maybe AST
parse s = do
  (ast, rest) <- runParser tipProgramP s
  if null rest then Just ast else Nothing

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (v, s') <- runParser p s
    return (f v, s')

instance Applicative Parser where
  pure v = Parser $ \s -> Just (v, s)
  pf <*> pv = Parser $ \s -> do
    -- TODO: Which order should this actually be in? Unwrap f first or v? Does
    -- it matter?
    (f, s') <- runParser pf s
    (v, s'') <- runParser pv s'
    return (f v, s'')

instance Monad Parser where
  pv >>= pf = Parser $ \s -> do
    (v, s') <- runParser pv s
    runParser (pf v) s'

instance Alternative Parser where
  empty = Parser $ const Nothing
  p <|> q = Parser $ \s -> runParser p s <|> runParser q s

charP :: Char -> Parser Char
charP e = Parser $ f
  where
    f :: String -> Maybe (Char, String)
    f (c : cs)
      | c == e = Just (c, cs)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . fmap charP

predP :: (Char -> Bool) -> Parser Char
predP p = Parser f
  where
    f :: String -> Maybe (Char, String)
    f [] = Nothing
    f (c : cs)
      | p c = Just (c, cs)
      | otherwise = Nothing

spanP :: (Char -> Bool) -> Parser String
spanP = many . predP

wsP :: Parser String
wsP = spanP isSpace

-- Allow any amount of whitespace after the parser.
lexeme :: Parser a -> Parser a
lexeme p = Parser $ \s -> do
  (v, s1) <- runParser p s
  (_, s2) <- runParser wsP s1
  return (v, s2)

opt :: Parser a -> Parser (Maybe a)
opt p = Parser $ \s -> pure $
  case runParser p s of
    Nothing -> (Nothing, s)
    Just (v, s') -> (Just v, s')

-- FIXME: There's definitely a better way of writing this.
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = do
  v <- p
  vs <- ps p q
  pure $ v : vs
  where
    ps :: Parser a -> Parser b -> Parser [a]
    ps p' q' = do
      sep <- opt q'
      case sep of
        Nothing -> pure []
        Just _ -> do
          v' <- p'
          vs' <- ps p' q'
          pure $ v' : vs'

-- TIP Parsing.

tipProgramP :: Parser AST
tipProgramP = functionP

identifierP :: Parser String
identifierP = do
  i <- predP isAlpha
  is <- spanP (\x -> isDigit x || isAlpha x)
  return $ i : is

functionP :: Parser AST
functionP = do
  functionName <- lexeme identifierP
  _ <- lexeme $ charP '('
  _ <- lexeme $ charP ')'
  return $ Function functionName
