module Parser.Internal (parse, token, keyword, satisfyWhile, identifierP, runParser, Decl (..)) where

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

ws :: Parser String
ws = satisfyWhile isSpace

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f :: String -> Maybe (Char, String)
    f [] = Nothing
    f (c : cs)
      | p c = Just (c, cs)
      | otherwise = Nothing

satisfyWhile :: (Char -> Bool) -> Parser [Char]
satisfyWhile = many . satisfy


sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = (:) <$> p <*> many (q *> p) <|> pure []

-- Allow any amount of whitespace after the parser.
lexeme :: Parser a -> Parser a
lexeme p = p <* ws

token :: Char -> Parser Char
token e = lexeme $ Parser $ f
  where
    f :: String -> Maybe (Char, String)
    f (c : cs)
      | c == e = Just (c, cs)
      | otherwise = Nothing
    f [] = Nothing

keyword :: String -> Parser String
keyword = lexeme . sequenceA . fmap token

-- TIP Parsing.

tipProgramP :: Parser TipProgram
tipProgramP = TipProgram <$> (ws *> many functionP)

expressionP :: Parser Expression
expressionP = termP

termOpP :: Parser Op
termOpP = lexeme $ Add <$ token '+'
      <|> Subtract <$ token '-'
      <|> GreaterThan <$ token '>'
      <|> Equal <$ keyword "=="

factorOpP :: Parser Op
factorOpP = lexeme $ Multiply <$ token '*'
      <|> Divide <$ token '/'

termP :: Parser Expression
termP = ((\lhs op rhs -> Binary op lhs rhs) <$> factorP <*> termOpP <*> factorP) <|> factorP

factorP' :: Parser (Maybe (Op, Expression))
factorP' = optional ((\op rhs -> (op, rhs)) <$> factorOpP <*> factorP)

factorP :: Parser Expression
factorP = f <$> ( intP <|> idP <|> (token '(' *> expressionP <* token ')') ) <*> factorP'
  where
    f :: Expression -> Maybe (Op, Expression) -> Expression
    f lhs op_rhs = case op_rhs of
                        Nothing -> lhs
                        Just (op, rhs) -> Binary op lhs rhs

intP :: Parser Expression
intP = lexeme $ Int <$> read <$> ((:) <$> satisfy isDigit <*> satisfyWhile isDigit)

idP :: Parser Expression
idP = lexeme $ Id <$> identifierP

statementP :: Parser Statement
statementP = lexeme $ (variableDeclarationP <|> outputP <|> ifP <|> returnP) <* lexeme (token ';')

variableDeclarationP :: Parser Statement
variableDeclarationP = lexeme $ VariableDeclaration <$> (lexeme (keyword "var") *> identifierP)

outputP :: Parser Statement
outputP = Output <$> (keyword "output" *> expressionP)

ifP :: Parser Statement
ifP = If <$> (keyword "if" *> token '(' *> expressionP <* token ')') <*> (token '{' *> many statementP <* token '}') <*> optional (keyword "else" *> token '{' *> many statementP <* token '}')

returnP :: Parser Statement
returnP = Return <$> (keyword "return" *> optional expressionP)

identifierP :: Parser String
identifierP = do
  i <- satisfy isAlpha
  is <- satisfyWhile (\x -> isDigit x || isAlpha x)
  return $ i : is

functionP :: Parser Decl
functionP = do
  functionName <- lexeme identifierP
  _ <- token '('
  functionParams <- lexeme identifierP `sepBy` token ','
  _ <- token ')'
  _ <- token '{'
  statements <- many statementP
  _ <- token '}'
  return $ Function functionName functionParams statements
