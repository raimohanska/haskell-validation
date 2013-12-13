module Validation where

import Control.Applicative

data Validation a = Validation a [String]
  deriving (Eq, Show)

valid x = Validation x []
invalid x errors = Validation x errors
isValid (Validation _ []) = True
isValid _ = False

instance Monad Validation where
  return = valid
  (Validation a errors) >>= f = case f a of
    Validation b moreErrors -> Validation b (errors ++ moreErrors)

instance Functor Validation where
  fmap f (Validation a errors) = Validation (f a) errors

instance Applicative Validation where
  pure = valid
  (Validation f e1) <*> (Validation x e2) = Validation (f x) (e1 ++ e2)
