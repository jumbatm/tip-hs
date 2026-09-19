module Interpreter where

import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Parser.TipParser as TP

data Value = Integer Int | String String | Record (Map String Value) | Cell String deriving (Show)

type Error = String

-- TODO: Automatically derive from StateT
newtype Interpreter a = Interpreter {run :: Map String Value -> Either Error (a, Map String Value)}

instance Functor Interpreter where
  fmap f (Interpreter fv) = Interpreter $ fmap (first f) <$> fv

instance Applicative Interpreter where
  pure v = Interpreter $ \st -> Right (v, st)
  (Interpreter interpf) <*> (Interpreter interpv) = Interpreter $ \s -> do
    (f, fs) <- interpf s
    (a, vs) <- interpv fs
    Right (f a, vs)

instance Monad Interpreter where
  return = pure

  (Interpreter iv) >>= f = Interpreter $ \st -> iv st >>= (\(a, st') -> run (f a) st')

get :: String -> Interpreter Value
get var = Interpreter $ \st -> case Map.lookup var st of
  Nothing -> Left $ "invalid variable " ++ var
  Just v -> Right (v, st)

put :: String -> Value -> Interpreter ()
put var value = Interpreter $ \st -> Right ((), Map.insert var value st)

panic :: String -> Interpreter a
panic = Interpreter . const . Left

-- TODO: Alas, again, the allure of using recursion scheme. Must... resist...
evalExpr :: TP.Expression -> Interpreter Value
evalExpr (TP.Int i) = pure $ Integer i
evalExpr (TP.Id name) = get name
evalExpr (TP.Binary op lhs rhs) = do
  l <- evalExpr lhs
  r <- evalExpr rhs
  evalBinOp op l r
  where
    evalBinOp bop (Integer a) (Integer b) = pure . Integer $ case bop of
      TP.Add -> a + b
      TP.Subtract -> a - b
      TP.Multiply -> a * b
      TP.Divide -> a `div` b
      TP.GreaterThan -> if a > b then 1 else 0
      TP.Equal -> if a == b then 1 else 0
    evalBinOp _ _ _ = panic $ "no " ++ show op ++ " defined on " ++ show lhs ++ " and " ++ show rhs
evalExpr (TP.Unary TP.Dereference (TP.Id name)) = get name
evalExpr (TP.Unary TP.AddressOf (TP.Id name)) = pure $ Cell name
evalExpr (TP.Unary op v) = do
  x <- evalExpr v
  evalUnOp op x
  where
    evalUnOp TP.Negate (Integer n) = pure $ Integer (-n)
    evalUnOp _ _ = panic $ "no " ++ show op ++ " defined for " ++ show v
evalExpr (TP.Call f args) = undefined
evalExpr (TP.Alloc expr) = undefined
evalExpr (TP.Record bindings) = undefined
evalExpr (TP.RecordAccess r field) = undefined
