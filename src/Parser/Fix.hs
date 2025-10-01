{-# LANGUAGE UndecidableInstances #-}
module Parser.Fix where

newtype Fix f = Fix { unfix :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  show (Fix x) = "(Fix " ++ show x ++ ")"

instance Eq (f (Fix f)) => Eq (Fix f) where
  (Fix x) == (Fix y) = x == y

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix
