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

instance Monad Interpreter where
  return = Interpreter . Right . (,)

  mv >>= mf = Interpreter $ case runInterpreter mv of
    Left err -> Left err -- If initial interpreter is already error, don't bother
    Right rf -> Right $ \st ->
      -- It's fine. Initial interpreter is good and contains the step we're doing now. We produce a new interpreter, which given a state...
      let (v, st') = rf st -- runs the action we got before with whatever the state is we have, giving us the value and new state
          (Interpreter r) = mf v -- which we can then finally run our function with, giving us a new interpreter. The new interpreter contains our next action.
       in case r of -- This interpreter may have failed, so check it...
            Left err -> _HERE -- Uh oh! We need to produce a `b`. But we can't do that!
            Right rf -> rf st' -- This is fine. Take what should happen next and run it on the new state. This yields what we need!

-- And this hole is why the Either needs to sit at the function result.
-- At _HERE, we need to produce a `(b, Map String Value)`. However, the `a`
-- which we thought we'd receive which should have come through `mv` --
-- doesn't exist! So what do we do??
--
-- Thinking about what explicitly is happening here: first we run the
-- interpreter in mv and get back an Either. If it was Left, we can short
-- circuit and error out immediately.
--
-- If it was Right, then inside it was the current step's function. We're now
-- looking to produce a new Interpreter, so we produce a new one which takes
-- the state and runs it against the current step's function, yielding the
-- LHS value and the new state.
--
-- We can now pass this to the wrapped function on the right side, giving us
-- the new interpreter. If it was Right, then we're fine -- pass the new
-- state into the new function and return that result.
--
-- But... if we're Left? Uh oh! There's no value for us to pass in!
--
-- What's happening here is once we're inside the function, the _computation
-- is not allowed to fail_. But that's not right at all!
--
-- Contrast if this to if we put the Either inside: `(a -> (Either Error (Map
-- String Value))`. Now rather than requiring that we have an `a` even after
-- the first computation fails, we can just straight up return Left if we
-- don't have what we need!
