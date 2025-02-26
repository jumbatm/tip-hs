module Parser (parse) where

-- Our parser type. We return pairs of (the parsed object, the remaining
-- string). We have a list as we're able to handle ambiguous grammars and
-- return all possible parse trees.
newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

data AST = AST deriving (Show)

-- Actual function we export.
parse :: String -> Maybe AST
parse _s = Just AST

-- TODO: Fill in.
instance Functor Parser

-- TODO: Fill in.
instance Applicative Parser

instance Monad Parser
