module Parser.Internal (parse, charP, stringP, spanP, identifierP, runParser, AST (..)) where

import Control.Applicative
import Control.Monad
import Data.Char

import Debug.Trace

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
    let s1 = dropWhile isSpace s
    (f, s2) <- runParser pf s1
    let s3 = dropWhile isSpace s2
    (v, s4) <- runParser pv s3
    return (f v, s4)

-- FIXME: Could we thread the string through the context? So we don't have to
-- keep passing it through ourselves?
instance Monad Parser where
  pv >>= pf = Parser $ \s -> do
    (_, s1) <- runParser wsP s
    (v, s2) <- runParser pv s1
    runParser (pf v) s2

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

guardNoTrailingWhitespace :: Parser a -> Parser a
guardNoTrailingWhitespace p = Parser $ \s -> do
  (v, s') <- runParser p s
  guard $ emptyOrNonWhitespace s'
  Just (v, s')
  where
    emptyOrNonWhitespace :: String -> Bool
    emptyOrNonWhitespace (c:_) = not $ isSpace c
    emptyOrNonWhitespace [] = True

-- TIP Parsing.

tipProgramP :: Parser AST
tipProgramP = functionP

identifierP :: Parser String
identifierP = do
  i <- guardNoTrailingWhitespace $ predP isAlpha
  is <- spanP (\x -> isDigit x || isAlpha x)
  return $ i : is

functionP :: Parser AST
functionP = do
  functionName <- identifierP
  _ <- charP '('
  _ <- charP ')'
  return $ Function functionName
