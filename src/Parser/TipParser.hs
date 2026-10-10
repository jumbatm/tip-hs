module Parser.TipParser where

import Control.Applicative
import Data.Functor.Identity
import Parser.CharParser (CharParserState (..), getPos)
import Parser.Internal
import Parser.TipLexer

newtype TipProgram = TipProgram [Located Function] deriving (Show)

data Function = Function String [String] [Located Statement]
  deriving (Show, Eq)

data Statement
  = VariableDeclaration [String]
  | Output (Located Expression)
  | If (Located Expression) [Located Statement] (Maybe [Located Statement])
  | Return (Maybe (Located Expression))
  | Assignment (Located Expression) (Located Expression)
  | Expression (Located Expression)
  | While (Located Expression) [Located Statement]
  | Block [Located Statement]
  | Error (Located Expression)
  deriving (Show, Eq)

data Located a = Located SourceLocation a deriving (Show, Eq)

data UnOp = Dereference | AddressOf | Negate deriving (Show, Eq)

data BinOp = Add | Subtract | Multiply | Divide | GreaterThan | Equal deriving (Show, Eq)

data Expression
  = Int Int
  | Id String
  | Binary BinOp (Located Expression) (Located Expression)
  | Unary UnOp (Located Expression)
  | Call (Located Expression) [Located Expression]
  | Alloc (Located Expression)
  | Record [(String, Located Expression)]
  | RecordAccess (Located Expression) (Located String)
  deriving (Show, Eq)

annotateLoc :: TipParser a -> TipParser (Located a)
annotateLoc p = Located <$> getPos <*> p

atLoc :: Located a -> b -> Located b
atLoc (Located loc _) = Located loc

tipProgramP :: TipParser TipProgram
tipProgramP = TipProgram <$> (ws *> some (annotateLoc functionP))

termOpP :: TipParser (Located BinOp)
termOpP =
  annotateLoc $
    Add <$ symbol "+"
      <|> Subtract <$ symbol "-"
      <|> GreaterThan <$ symbol ">"
      <|> Equal <$ keyword "=="

factorOpP :: TipParser (Located BinOp)
factorOpP =
  annotateLoc $
    Multiply <$ symbol "*"
      <|> Divide <$ symbol "/"

buildBinary :: Located BinOp -> Located Expression -> Located Expression -> Located Expression
buildBinary (Located loc op) lhs rhs = Located loc $ Binary op lhs rhs

expressionP :: TipParser (Located Expression)
expressionP = chainl1 termP (buildBinary <$> termOpP)

unOpP :: TipParser UnOp
unOpP = Negate <$ char '-' <|> Dereference <$ char '*'

termP :: TipParser (Located Expression)
termP = chainl1 factorP (buildBinary <$> factorOpP)

factorP :: TipParser (Located Expression)
factorP = foldl (flip ($)) <$> atomP <*> many trailing <|> (buildUnary <$> annotateLoc unOpP <*> factorP)
  where
    trailing :: TipParser (Located Expression -> Located Expression)
    trailing = buildCall <$> parens (expressionP `sepBy` char ',') <|> buildRecordAccess <$> (char '.' *> annotateLoc identifierP)

    buildUnary :: Located UnOp -> Located Expression -> Located Expression
    buildUnary (Located loc op) expr = Located loc (Unary op expr)

    buildRecordAccess :: Located String -> Located Expression -> Located Expression
    buildRecordAccess field base = atLoc field (RecordAccess base field)

    buildCall :: [Located Expression] -> Located Expression -> Located Expression
    buildCall args callee = atLoc callee (Call callee args)

atomP :: TipParser (Located Expression)
atomP =
  annotateLoc (keyword "alloc" *> (Alloc <$> factorP))
    <|> (buildAlloc <$> optional (annotateLoc (char '&')) <*> annotateLoc idP)
    <|> annotateLoc (intP <|> recordP)
    <|> parens expressionP
  where
    buildAlloc addr x = case addr of
      Just (Located loc _) -> Located loc (Unary AddressOf x)
      Nothing -> x

recordP :: TipParser Expression
recordP = braces $ Record <$> ((,) <$> identifier <*> (char ':' *> expressionP)) `sepBy` char ','

intP :: TipParser Expression
intP = Int <$> intLit

idP :: TipParser Expression
idP = Id <$> identifier

-- TODO: Left factor rules starting with an expression so we don't need to backtrack assignments.
statementP :: TipParser (Located Statement)
statementP = annotateLoc $ blockP <|> ifP <|> whileP <|> ((variableDeclarationP <|> outputP <|> errorP <|> returnP <|> try assignmentP <|> (Expression <$> expressionP)) <* semi)

assignmentP :: TipParser Statement
assignmentP = Assignment <$> expressionP <*> (symbol "=" *> expressionP)

variableDeclarationP :: TipParser Statement
variableDeclarationP = VariableDeclaration <$> (keyword "var" *> (identifier `sepBy` char ','))

outputP :: TipParser Statement
outputP = Output <$> (keyword "output" *> expressionP)

errorP :: Parser CharParserState Identity Statement
errorP = Error <$> (keyword "error" *> expressionP)

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

blockP :: TipParser Statement
blockP = Block <$> braces (many statementP)

returnP :: TipParser Statement
returnP = Return <$> (keyword "return" *> optional expressionP)

functionP :: TipParser Function
functionP =
  Function
    <$> identifier
    <*> parens (identifier `sepBy` comma)
    <*> braces (many statementP)

runParser :: TipParser a -> String -> Identity (ParseResult (a, CharParserState))
runParser p s = unParser p (CharParserState s (SourceLocation (1, 1)))

parse :: String -> ParseResult TipProgram
parse = (fst <$>) . runIdentity . runParser tipProgramP
