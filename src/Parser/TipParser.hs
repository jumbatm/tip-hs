module Parser.TipParser where

import Control.Applicative
import Data.Char
import Parser.CharParser
import Parser.Internal

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

type TipParser = CharParser ParseResult

tipProgramP :: TipParser TipProgram
tipProgramP = TipProgram <$> (ws *> many functionP)

expressionP :: TipParser Expression
expressionP = termP

termOpP :: TipParser Op
termOpP =
  lexeme $
    Add <$ char '+'
      <|> Subtract <$ char '-'
      <|> GreaterThan <$ char '>'
      <|> Equal <$ keyword "=="
      <|> parseError "expected termOp"

factorOpP :: TipParser Op
factorOpP =
  lexeme $
    Multiply <$ char '*'
      <|> Divide <$ char '/'

termP :: TipParser Expression
termP = ((\lhs op rhs -> Binary op lhs rhs) <$> factorP <*> termOpP <*> factorP) <|> factorP

factorP' :: TipParser (Maybe (Op, Expression))
factorP' = optional ((\op rhs -> (op, rhs)) <$> factorOpP <*> factorP)

factorP :: TipParser Expression
factorP = f <$> (intP <|> idP <|> (char '(' *> expressionP <* char ')')) <*> factorP'
  where
    f :: Expression -> Maybe (Op, Expression) -> Expression
    f lhs op_rhs = case op_rhs of
      Nothing -> lhs
      Just (op, rhs) -> Binary op lhs rhs

intP :: TipParser Expression
intP = lexeme $ Int <$> read <$> ((:) <$> satisfy isDigit <*> satisfyWhile isDigit)

idP :: TipParser Expression
idP = lexeme $ Id <$> identifierP

statementP :: TipParser Statement
statementP = lexeme $ (variableDeclarationP <|> outputP <|> ifP <|> returnP) <* lexeme (char ';')

variableDeclarationP :: TipParser Statement
variableDeclarationP = lexeme $ VariableDeclaration <$> (lexeme (keyword "var") *> identifierP)

outputP :: TipParser Statement
outputP = Output <$> (keyword "output" *> expressionP)

ifP :: TipParser Statement
ifP = If <$> (keyword "if" *> char '(' *> expressionP <* char ')') <*> (char '{' *> many statementP <* char '}') <*> optional (keyword "else" *> char '{' *> many statementP <* char '}')

returnP :: TipParser Statement
returnP = Return <$> (keyword "return" *> optional expressionP)

identifierP :: TipParser String
identifierP = do
  i <- satisfy isAlpha
  is <- satisfyWhile (\x -> isDigit x || isAlpha x)
  return $ i : is

functionP :: TipParser Decl
functionP = do
  functionName <- identifierP
  _ <- char '('
  functionParams <- identifierP `sepBy` char ','
  _ <- char ')'
  _ <- char '{'
  statements <- many statementP
  _ <- char '}'
  return $ Function functionName functionParams statements

parse :: String -> ParseResult TipProgram
parse s = do
  (ast, _) <- runParser tipProgramP (CharParserState s (SourceLocation (1, 1)))
  pure ast
