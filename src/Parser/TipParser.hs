module Parser.TipParser where

import Parser.Internal
import Control.Applicative
import Data.Char

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


-- TIP Parsing.

tipProgramP :: Parser Char TipProgram
tipProgramP = TipProgram <$> (ws *> many functionP)

expressionP :: Parser Char Expression
expressionP = termP

termOpP :: Parser Char Op
termOpP = lexeme $ Add <$ token '+'
      <|> Subtract <$ token '-'
      <|> GreaterThan <$ token '>'
      <|> Equal <$ keyword "=="

factorOpP :: Parser Char Op
factorOpP = lexeme $ Multiply <$ token '*'
      <|> Divide <$ token '/'

termP :: Parser Char Expression
termP = ((\lhs op rhs -> Binary op lhs rhs) <$> factorP <*> termOpP <*> factorP) <|> factorP

factorP' :: Parser Char (Maybe (Op, Expression))
factorP' = optional ((\op rhs -> (op, rhs)) <$> factorOpP <*> factorP)

factorP :: Parser Char Expression
factorP = f <$> ( intP <|> idP <|> (token '(' *> expressionP <* token ')') ) <*> factorP'
  where
    f :: Expression -> Maybe (Op, Expression) -> Expression
    f lhs op_rhs = case op_rhs of
                        Nothing -> lhs
                        Just (op, rhs) -> Binary op lhs rhs

intP :: Parser Char Expression
intP = lexeme $ Int <$> read <$> ((:) <$> satisfy isDigit <*> satisfyWhile isDigit)

idP :: Parser Char Expression
idP = lexeme $ Id <$> identifierP

statementP :: Parser Char Statement
statementP = lexeme $ (variableDeclarationP <|> outputP <|> ifP <|> returnP) <* lexeme (token ';')

variableDeclarationP :: Parser Char Statement
variableDeclarationP = lexeme $ VariableDeclaration <$> (lexeme (keyword "var") *> identifierP)

outputP :: Parser Char Statement
outputP = Output <$> (keyword "output" *> expressionP)

ifP :: Parser Char Statement
ifP = If <$> (keyword "if" *> token '(' *> expressionP <* token ')') <*> (token '{' *> many statementP <* token '}') <*> optional (keyword "else" *> token '{' *> many statementP <* token '}')

returnP :: Parser Char Statement
returnP = Return <$> (keyword "return" *> optional expressionP)

identifierP :: Parser Char String
identifierP = do
  i <- satisfy isAlpha
  is <- satisfyWhile (\x -> isDigit x || isAlpha x)
  return $ i : is

functionP :: Parser Char Decl
functionP = do
  functionName <- identifierP
  _ <- token '('
  functionParams <- identifierP `sepBy` token ','
  _ <- token ')'
  _ <- token '{'
  statements <- many statementP
  _ <- token '}'
  return $ Function functionName functionParams statements

parse :: String -> Maybe TipProgram
parse s = do
  (ast, rest) <- runParser tipProgramP s
  if null rest then Just ast else Nothing

