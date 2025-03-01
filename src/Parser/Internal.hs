module Parser.Internal (parse, charP, stringP, spanP, identifierP, runParser, Decl (..)) where

import Control.Applicative
import Data.Char

-- Our parser type. We return pairs of (the parsed object, the remaining
-- string). We have a list as we're able to handle ambiguous grammars and
-- return all possible parse trees.
newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

newtype TipProgram = TipProgram [Decl] deriving (Show)

data Decl
  = Identifier String
  | Function String [String] [Statement]
  deriving (Show, Eq)

data Op = Add | Subtract | Multiply | Divide | GreaterThan | Equal deriving (Show, Eq)

data Statement
  = VariableDeclaration String
  | Output Expression
  | If Expression [Statement] (Maybe [Statement])
  | Return (Maybe Expression)
  deriving (Show, Eq)

data Expression
  = Int Int
  | Id String
  | Binary Op Expression Expression
  | Call Expression [Expression]
  deriving (Show, Eq)

-- Actual function we export.
parse :: String -> Maybe TipProgram
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
lexeme p = p <* wsP

opt :: Parser a -> Parser (Maybe a)
opt p = Just <$> p <|> pure Nothing

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = (:) <$> p <*> many (q *> p) <|> pure []

-- TIP Parsing.

tipProgramP :: Parser TipProgram
tipProgramP = TipProgram <$> many functionP

statementP :: Parser Statement
statementP = (variableDeclarationStmt <|> outputStmt <|> ifStmt <|> returnStmt) <* lexeme (charP ';')
  where
    variableDeclarationStmt :: Parser Statement
    variableDeclarationStmt = VariableDeclaration <$> (lexeme (stringP "var") *> lexeme identifierP)

    outputStmt :: Parser Statement
    outputStmt = undefined

    ifStmt :: Parser Statement
    ifStmt = undefined

    returnStmt :: Parser Statement
    returnStmt = undefined

identifierP :: Parser String
identifierP = do
  i <- predP isAlpha
  is <- spanP (\x -> isDigit x || isAlpha x)
  return $ i : is

functionP :: Parser Decl
functionP = do
  functionName <- lexeme identifierP
  _ <- lexeme $ charP '('
  functionParams <- lexeme identifierP `sepBy` lexeme (charP ',')
  _ <- lexeme $ charP ')'
  _ <- lexeme $ charP '{'
  _ <- lexeme $ charP '}'
  return $ Function functionName functionParams []
