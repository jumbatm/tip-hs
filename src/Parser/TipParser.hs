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
  | Output Expression
  | If Expression [Statement] (Maybe [Statement])
  | Return (Maybe Expression)
  | Assignment Expression Expression
  | Expression Expression
  | While Expression [Statement]
  | Block [Statement]
  | Error Expression
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
  | Record [(String, Expression)]
  | RecordAccess Expression String
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
expressionP = chainl1 termP (Binary <$> termOpP)

unOpP :: TipParser UnOp
unOpP = Negate <$ char '-' <|> Dereference <$ char '*'

termP :: TipParser Expression
termP = chainl1 factorP (Binary <$> factorOpP)

factorP :: TipParser Expression
factorP = foldl (flip ($)) <$> atomP <*> many trailing <|> (Unary <$> unOpP <*> factorP)
  where
    trailing = flip Call <$> parens (expressionP `sepBy` char ',') <|> flip RecordAccess <$> (char '.' *> identifierP)

atomP :: TipParser Expression
atomP =
  (Alloc <$> (keyword "alloc" *> factorP))
    <|> ( ( \addr x -> case addr of
              Just _ -> Unary AddressOf x
              Nothing -> x
          )
            <$> (optional $ char '&')
            <*> idP
        )
    <|> (intP <|> parens expressionP <|> recordP)

recordP :: TipParser Expression
recordP = braces $ Record <$> ((,) <$> identifier <*> (char ':' *> expressionP)) `sepBy` char ','

intP :: TipParser Expression
intP = Int <$> intLit

idP :: TipParser Expression
idP = Id <$> identifier

-- TODO: Left factor rules starting with an expression so we don't need to backtrack assignments.
statementP :: TipParser Statement
statementP = blockP <|> ifP <|> whileP <|> ((variableDeclarationP <|> outputP <|> errorP <|> returnP <|> try assignmentP <|> (Expression <$> expressionP)) <* semi)

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
    <*> braces (many (annotateLoc statementP))

runParser :: TipParser a -> String -> Identity (ParseResult (a, CharParserState))
runParser p s = unParser p (CharParserState s (SourceLocation (1, 1)))

parse :: String -> ParseResult TipProgram
parse = (fst <$>) . runIdentity . runParser tipProgramP
