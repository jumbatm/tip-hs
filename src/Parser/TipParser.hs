module Parser.TipParser where

import Control.Applicative
import Data.Functor.Identity
import Parser.CharParser (CharParserState (..), getPos)
import Parser.Internal
import Parser.TipLexer

newtype TipProgram = TipProgram [Located Decl] deriving (Show)

data Decl
  = Identifier String
  | Function String [String] [Located Statement]
  deriving (Show, Eq)

data Op = Add | Subtract | Multiply | Divide | GreaterThan | Equal deriving (Show, Eq)

data Statement
  = VariableDeclaration [String]
  | Output Expression
  | If Expression [Statement] (Maybe [Statement])
  | Return (Maybe Expression)
  | Assignment String Expression
  | Expression Expression
  deriving (Show, Eq)

data Located a = Located SourceLocation a deriving (Show, Eq)

data Expression
  = Int Int
  | Id String
  | Binary Op Expression Expression
  | Call Expression [Expression]
  deriving (Show, Eq)

annotateLoc :: TipParser a -> TipParser (Located a)
annotateLoc p = Located <$> getPos <*> p

tipProgramP :: TipParser TipProgram
tipProgramP = TipProgram <$> (ws *> some (annotateLoc functionP))

expressionP :: TipParser Expression
expressionP = termP

termOpP :: TipParser Op
termOpP =
  Add <$ symbol "+"
    <|> Subtract <$ symbol "-"
    <|> GreaterThan <$ symbol ">"
    <|> Equal <$ keyword "=="

factorOpP :: TipParser Op
factorOpP =
  Multiply <$ symbol "*"
    <|> Divide <$ symbol "/"

termP :: TipParser Expression
termP = buildExpression <$> factorP <*> optional ((,) <$> termOpP <*> factorP)
  where
    buildExpression expr Nothing = expr
    buildExpression lhs (Just (op, rhs)) = Binary op lhs rhs

data RestOfFactor = BinExpr Op Expression | CallArgs [Expression]

factorP' :: TipParser (Maybe RestOfFactor)
factorP' = optional (parseRestOfBinExpr <|> parseRestOfCallExpr)
  where
    parseRestOfBinExpr = BinExpr <$> factorOpP <*> factorP
    parseRestOfCallExpr = CallArgs <$> parens (expressionP `sepBy` char ',')

factorP :: TipParser Expression
factorP = build <$> (intP <|> idP <|> parens expressionP) <*> factorP'
  where
    build :: Expression -> Maybe RestOfFactor -> Expression
    build lhs op_rhs = case op_rhs of
      Nothing -> lhs
      Just (BinExpr op rhs) -> Binary op lhs rhs
      Just (CallArgs args) -> Call lhs args

intP :: TipParser Expression
intP = Int <$> intLit

idP :: TipParser Expression
idP = Id <$> identifier

statementP :: TipParser Statement
statementP = ifP <|> ((variableDeclarationP <|> outputP <|> returnP <|> assignmentP <|> (Expression <$> expressionP)) <* semi)

assignmentP :: TipParser Statement
assignmentP = Assignment <$> identifier <*> (symbol "=" *> expressionP)

variableDeclarationP :: TipParser Statement
variableDeclarationP = VariableDeclaration <$> (keyword "var" *> (identifier `sepBy` char ','))

outputP :: TipParser Statement
outputP = Output <$> (keyword "output" *> expressionP)

ifP :: TipParser Statement
ifP =
  If
    <$> (keyword "if" *> parens expressionP)
    <*> braces (many statementP)
    <*> optional (keyword "else" *> braces (many statementP))

returnP :: TipParser Statement
returnP = Return <$> (keyword "return" *> optional expressionP)

functionP :: TipParser Decl
functionP =
  Function
    <$> identifier
    <*> parens (identifier `sepBy` comma)
    <*> braces (many (annotateLoc statementP))

runParser :: TipParser a -> String -> Identity (ParseResult (a, CharParserState))
runParser p s = unParser p (CharParserState s (SourceLocation (1, 1)))

parse :: String -> ParseResult TipProgram
parse = (fst <$>) . runIdentity . runParser tipProgramP
