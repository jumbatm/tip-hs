module Interpreter where

import Data.Map
import Parser.TipParser

data Value = Integer Int | String String | Record (Map String Value)

type Error = String

type State = Either Error (Map String Value)

-- TODO: Automatically derive from StateT
newtype Interpreter a = Interpreter {run :: State -> (a, State)}

instance Functor Interpreter where
  fmap f interp = Interpreter $ \st -> let (v, st') = run interp st in (f v, st')

instance Applicative Interpreter

instance Monad Interpreter where
  return v = Interpreter $ \st -> (v, st)

  mv >>= mf = Interpreter $ \st ->
    let (v, vs) = run mv st
        (f, fs) = run (mf v) vs
     in (f, fs)
