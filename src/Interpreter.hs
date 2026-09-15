module Interpreter where

import Data.Bifunctor (first)
import Data.Map
import Parser.TipParser

data Value = Integer Int | String String | Record (Map String Value)

type Error = String

-- TODO: Automatically derive from StateT
newtype Interpreter a = Interpreter {runInterpreter :: Either Error (Map String Value -> (a, Map String Value))}

instance Functor Interpreter where
  fmap f (Interpreter frun) = Interpreter $ (first f .) <$> frun

instance Applicative Interpreter

-- instance Monad Interpreter where
--  return v = Interpreter $ \st -> (v, st)
--
--  mv >>= mf = Interpreter $ \st ->
--    let (v, vs) = run mv st
--     in run (mf v) vs
