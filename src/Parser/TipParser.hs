{-# LANGUAGE DeriveFunctor #-}
module Parser.TipParser where

import Parser.Internal
import Parser.CharParser
import Control.Applicative
import Parser.Fix
import Data.Char

newtype TipProgram = TipProgram [Decl] deriving (Show)

data Decl = Function String [String] [Statement] deriving Show

data Op = Add | Subtract | Multiply | Divide | GreaterThan | Equal deriving (Show, Eq)

data StatementF a
  = VariableDeclaration String
  | Output Expression
  | If Expression [a] (Maybe [a])
  | Return (Maybe Expression)
  deriving (Show, Eq, Functor)

type Statement = Fix StatementF

data ExpressionF a
  = Int Int
  | Id String
  | Binary Op a a
  | Call a [a]
  deriving (Show, Eq, Functor)

type Expression = Fix ExpressionF

-- TIP Parsing.

type TipParser = CharParser ParseResult

tipProgramP :: TipParser TipProgram
tipProgramP = TipProgram <$> (ws *> many functionP)

expressionP :: TipParser Expression
expressionP = termP

termOpP :: TipParser Op
termOpP = lexeme $ Add <$ char '+'
      <|> Subtract <$ char '-'
      <|> GreaterThan <$ char '>'
      <|> Equal <$ keyword "=="
      <|> parseError "expected termOp"

factorOpP :: TipParser Op
factorOpP = lexeme $ Multiply <$ char '*'
      <|> Divide <$ char '/'

termP :: TipParser Expression
termP = ((\lhs op rhs -> Fix $ Binary op lhs rhs) <$> factorP <*> termOpP <*> factorP) <|> factorP

factorP' :: TipParser (Maybe (Op, Expression))
factorP' = optional ((\op rhs -> (op, rhs)) <$> factorOpP <*> factorP)

factorP :: TipParser Expression
factorP = f <$> ( intP <|> idP <|> (char '(' *> expressionP <* char ')') ) <*> factorP'
  where
    f :: Expression -> Maybe (Op, Expression) -> Expression
    f lhs op_rhs = case op_rhs of
                        Nothing -> lhs
                        Just (op, rhs) -> Fix $ Binary op lhs rhs

intP :: TipParser Expression
intP = lexeme $ (Fix . Int) <$> read <$> ((:) <$> satisfy isDigit <*> satisfyWhile isDigit)

idP :: TipParser Expression
idP = lexeme $ (Fix . Id) <$> identifierP

statementP :: TipParser Statement
statementP = lexeme $ (variableDeclarationP <|> outputP <|> ifP <|> returnP) <* lexeme (char ';')

variableDeclarationP :: TipParser Statement
variableDeclarationP = lexeme $ Fix . VariableDeclaration <$> (lexeme (keyword "var") *> identifierP)

outputP :: TipParser Statement
outputP = Fix . Output <$> (keyword "output" *> expressionP)

ifP :: TipParser Statement
ifP = Fix <$> (If <$> (keyword "if" *> char '(' *> expressionP <* char ')') <*> (char '{' *> many statementP <* char '}') <*> optional (keyword "else" *> char '{' *> many statementP <* char '}'))
-- NOTE: fmap needed to get the Fix inside the Parser's context. Just putting
-- "Fix . If" wraps the whole parser in Fix.
--
-- The difference for this rule vs the others is that the others only take 1
-- parameter, so composing with Fix has the same effect as fmapping Fix into
-- it. Try it -- hlint will warn about a redundant <$>.

returnP :: TipParser Statement
returnP = Fix . Return <$> (keyword "return" *> optional expressionP)

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
  (ast, _) <- unParser tipProgramP (CharParserState s (SourceLocation (1, 1)))
  pure ast

