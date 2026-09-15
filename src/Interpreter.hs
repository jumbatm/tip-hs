module Interpreter where

import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as Map
import Parser.TipParser

data Value = Integer Int | String String | Record (Map String Value)

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
