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

data Statement
  = VariableDeclaration [String]
  | Output Expression
  | If Expression [Statement] (Maybe [Statement])
  | Return (Maybe Expression)
  | Assignment Expression Expression
  | Expression Expression
  | While Expression [Statement]
  deriving (Show, Eq)

data Located a = Located SourceLocation a deriving (Show, Eq)

data UnOp = Dereference | AddressOf | Negate deriving (Show, Eq)

data BinOp = Add | Subtract | Multiply | Divide | GreaterThan | Equal deriving (Show, Eq)

data Expression
  = Int Int
  | Id String
  | Binary BinOp Expression Expression
  | Unary UnOp Expression
  | Call Expression [Expression]
  | Alloc Expression
  deriving (Show, Eq)

annotateLoc :: TipParser a -> TipParser (Located a)
annotateLoc p = Located <$> getPos <*> p

tipProgramP :: TipParser TipProgram
tipProgramP = TipProgram <$> (ws *> some (annotateLoc functionP))

termOpP :: TipParser BinOp
termOpP =
  Add <$ symbol "+"
    <|> Subtract <$ symbol "-"
    <|> GreaterThan <$ symbol ">"
    <|> Equal <$ keyword "=="

factorOpP :: TipParser BinOp
factorOpP =
  Multiply <$ symbol "*"
    <|> Divide <$ symbol "/"

expressionP :: TipParser Expression
expressionP = buildExpression <$> termP <*> optional ((,) <$> termOpP <*> termP)
  where
    buildExpression expr Nothing = expr
    buildExpression lhs (Just (op, rhs)) = Binary op lhs rhs

data RestOfFactor = BinExpr BinOp Expression | CallArgs [Expression]

termP' :: TipParser RestOfFactor
termP' = parseRestOfBinExpr <|> parseRestOfCallExpr
  where
    parseRestOfBinExpr = BinExpr <$> factorOpP <*> termP
    parseRestOfCallExpr = CallArgs <$> parens (expressionP `sepBy` char ',')

unOpP :: TipParser UnOp
unOpP = Negate <$ char '-' <|> AddressOf <$ char '&' <|> Dereference <$ char '*'

termP :: TipParser Expression
termP = build <$> factorP <*> optional termP'
  where
    build :: Expression -> Maybe RestOfFactor -> Expression
    build lhs op_rhs = case op_rhs of
      Nothing -> lhs
      Just (BinExpr op rhs) -> Binary op lhs rhs
      Just (CallArgs args) -> Call lhs args

factorP :: TipParser Expression
factorP = (Alloc <$> (keyword "alloc" *> factorP)) <|> (intP <|> idP <|> parens expressionP) <|> (Unary <$> unOpP <*> factorP)

intP :: TipParser Expression
intP = Int <$> intLit

idP :: TipParser Expression
idP = Id <$> identifier

-- TODO: Left factor rules starting with an expression so we don't need to backtrack assignments.
statementP :: TipParser Statement
statementP = ifP <|> whileP <|> ((variableDeclarationP <|> outputP <|> returnP <|> try assignmentP <|> (Expression <$> expressionP)) <* semi)

assignmentP :: TipParser Statement
assignmentP = Assignment <$> expressionP <*> (symbol "=" *> expressionP)

variableDeclarationP :: TipParser Statement
variableDeclarationP = VariableDeclaration <$> (keyword "var" *> (identifier `sepBy` char ','))

outputP :: TipParser Statement
outputP = Output <$> (keyword "output" *> expressionP)

whileP :: TipParser Statement
whileP = While <$> (keyword "while" *> parens expressionP) <*> braces (many statementP)

ifP :: TipParser Statement
ifP =
  If
    <$> (keyword "if" *> parens expressionP)
    <*> statements
    <*> optional (keyword "else" *> statements)
  where
    statements = (: []) <$> statementP <|> braces (many statementP)

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
