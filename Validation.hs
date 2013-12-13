{-# LANGUAGE GADTs #-}

module Validation(Validation, valid, invalid) where

import Control.Applicative
import Data.Monoid

data Validation e a where
  Validation :: (Monoid e, Eq e) => a -> e -> Validation e a

valid x = Validation x mempty
invalid x error = Validation x [error]

instance (Show a, Show e) => Show (Validation e a) where
  show (Validation a e) | e == mempty = "OK:" ++ show a
                        | otherwise = "FAIL:" ++ show e

instance (Monoid e, Eq e) => Monad (Validation e) where
  return = valid
  (Validation a errors) >>= f = case f a of
    Validation b moreErrors -> Validation b (errors `mappend` moreErrors)

instance Functor (Validation e) where
  fmap f (Validation a errors) = Validation (f a) errors

instance (Monoid e, Eq e) => Applicative (Validation e) where
  pure = valid
  (Validation f e1) <*> (Validation x e2) = Validation (f x) (e1 `mappend` e2)
