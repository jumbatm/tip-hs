module Parser.TipParser where

import Control.Applicative
import Data.Char
import Parser.CharParser
import Parser.Internal

newtype TipProgram = TipProgram [Located Decl] deriving (Show)

data Decl
  = Identifier String
  | Function String [String] [Located Statement]
  deriving (Show, Eq)

data Op = Add | Subtract | Multiply | Divide | GreaterThan | Equal deriving (Show, Eq)

data Statement
  = VariableDeclaration String
  | Output Expression
  | If Expression [Statement] (Maybe [Statement])
  | Return (Maybe Expression)
  deriving (Show, Eq)

data Located a = Located SourceLocation a deriving (Show, Eq)

data Expression
  = Int Int
  | Id String
  | Binary Op Expression Expression
  | Call Expression [Expression]
  deriving (Show, Eq)

-- TIP Parsing.

type TipParser = CharParser ParseResult

annotateLoc :: TipParser a -> TipParser (Located a)
annotateLoc p = Located <$> getPos <*> p

tipProgramP :: TipParser TipProgram
tipProgramP = TipProgram <$> (ws *> many (annotateLoc functionP))

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
termP = (flip Binary <$> factorP <*> termOpP <*> factorP) <|> factorP

factorP' :: TipParser (Maybe (Op, Expression))
factorP' = optional ((,) <$> factorOpP <*> factorP)

factorP :: TipParser Expression
factorP = f <$> (intP <|> idP <|> (char '(' *> expressionP <* char ')')) <*> factorP'
  where
    f :: Expression -> Maybe (Op, Expression) -> Expression
    f lhs op_rhs = case op_rhs of
      Nothing -> lhs
      Just (op, rhs) -> Binary op lhs rhs

intP :: TipParser Expression
intP = lexeme (Int . read <$> ((:) <$> satisfy isDigit <*> satisfyWhile isDigit))

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
identifierP = (:) <$> satisfy isAlpha <*> satisfyWhile (\x -> isDigit x || isAlpha x)

functionP :: TipParser Decl
functionP = (Function <$> identifierP <*> (char '(' *> (identifierP `sepBy` char ',') <* char ')') <*> (char '{' *> many (annotateLoc statementP) <* char '}'))

runParser :: TipParser a -> String -> ParseResult (a, CharParserState)
runParser p s = unParser p (CharParserState s (SourceLocation (1, 1)))

parse :: String -> ParseResult TipProgram
parse s = fst <$> runParser tipProgramP s
